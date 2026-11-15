# Les zones se ressemblent-elles ? Classifieur discriminant (« adversarial validation ») : un XGBoost apprend à deviner
# de quelle zone vient un feu à partir de ses variables. AUC = 0,5 : les zones sont indiscernables ; AUC proche de 1 : leurs
# distributions sont très différentes, sur toutes les variables à la fois. Les contributions de Shapley du classifieur
# disent quelles variables les distinguent.
#   Comparaisons : nord contre sud (comme 06_transfert.R et 07_distances.R), puis chaque région du test « région jamais
#   vue » contre les cinq autres ; feux 2006-2018 (comme la comparaison nord / sud de 07_distances.R).
#   Variables : celles du modèle sans mémoire des lieux (dep, lon, lat et historique des feux reconnaîtraient la zone
#   trivialement), sans l'année (simple repère dans le temps) ; toutes ensemble, puis famille par famille.
#   Validation croisée croisée : 5 groupes de communes tirés au hasard x 5 blocs d'années ; chaque case est prédite par un
#   modèle appris sans ses communes ni ses années. Sinon le classifieur reconnaîtrait les communes (végétation et relief
#   identiques pour tous leurs feux) ou les journées (même météo pour les départs d'un même jour) et l'AUC serait gonflée.
#   Nombre d'arbres fixe (200) : le classifieur est un instrument de mesure ; mal réglé, il ne peut que sous-estimer
#   l'écart entre zones, jamais l'inventer.
# Entrée : jeu de données ; resultats/tables/distances_nord_sud.csv (07_distances.R) pour le contrôle de cohérence.
# Sorties : resultats/tables/zones_auc.csv, zones_shap.csv ; donnees/modeles/zones.rds (prédictions hors échantillon et
#           contributions de Shapley par feu, pour les figures et le repondérage du ticket T8)
# Lancer depuis la racine du dépôt : Rscript analyses/10_zones.R

source("analyses/commun.R")

NORD <- c("04", "05", "07", "26", "48")
NR <- 200
d <- feux[apprentissage == TRUE]
VARS <- setdiff(SANS_MEMOIRE, "annee")
FAM_VARS <- FAMILLE[match(VARS, PREDICTEURS)]
NOMS_FAM <- c("calendrier" = "Calendrier", "météo" = "Météo", "bilan hydrique" = "Bilan hydrique",
              "végétation" = "Végétation", "état de la végétation" = "État de la végétation",
              "relief et position" = "Relief et position", "présence humaine" = "Présence humaine",
              "historique" = "Autres départs récents")
ENSEMBLES <- c(list("Toutes (hors mémoire)" = VARS), setNames(lapply(names(NOMS_FAM), function(f) VARS[FAM_VARS == f]), NOMS_FAM))
COMPARAISONS <- c(list("Nord contre sud" = d$dep %in% NORD),
                  setNames(lapply(sort(unique(d$region_test)), function(r) d$region_test == r),
                           paste(sort(unique(d$region_test)), "contre le reste")))

X <- matrice(d, VARS)
col_var <- sub("=.*", "", colnames(X))                        # variable de chaque colonne
set.seed(1)
communes <- unique(d$code_insee)
gcom <- setNames(sample(rep(1:5, length.out = length(communes))), communes)[d$code_insee]
bloc <- sapply(d$annee, function(a) which(sapply(BLOCS_ANNEES, function(b) a %in% b)))
cat(sprintf("%d feux 2006-2018, %d communes, %d variables (%d colonnes) ; %d cases de validation croisée\n",
            nrow(d), length(communes), length(VARS), ncol(X), 25))

# Prédictions hors échantillon (et contributions de Shapley si demandé) d'un ensemble de variables
hors_echantillon <- function(zone, vars, shap = FALSE) {
  cols <- which(col_var %in% vars)
  p <- rep(NA_real_, nrow(d))
  S <- if (shap) matrix(NA_real_, nrow(d), length(cols) + 1, dimnames = list(NULL, c(colnames(X)[cols], "(Intercept)")))
  for (g in 1:5) for (b in 1:5) {
    test <- gcom == g & bloc == b
    app <- gcom != g & bloc != b
    m <- ajuster(X[app, cols, drop = FALSE], as.numeric(zone[app]), NR)
    p[test] <- predire(m, X[test, cols, drop = FALSE])
    if (shap) S[test, ] <- predict(m, xgb.DMatrix(X[test, cols, drop = FALSE]), predcontrib = TRUE)
  }
  list(p = p, S = S)
}

resultats <- list()
for (k in names(COMPARAISONS)) {
  zone <- COMPARAISONS[[k]]
  cat(sprintf("\n%s : %d feux dans la zone (%.0f %%)\n", k, sum(zone), 100 * mean(zone)))
  r <- lapply(names(ENSEMBLES), function(e) hors_echantillon(zone, ENSEMBLES[[e]], shap = e == "Toutes (hors mémoire)"))
  names(r) <- names(ENSEMBLES)
  aucs <- sapply(r, function(x) auc(x$p, zone))
  print(round(aucs, 3))
  S <- r[["Toutes (hors mémoire)"]]$S
  stopifnot(!anyNA(S))
  # indicatrices additionnées par variable, puis variables par famille (produits par des matrices de 0 et de 1)
  phi_var <- S[, -ncol(S)] %*% outer(sub("=.*", "", colnames(S)[-ncol(S)]), VARS, "==")
  colnames(phi_var) <- VARS
  phi_fam <- phi_var %*% outer(FAM_VARS, names(NOMS_FAM), "==")
  colnames(phi_fam) <- NOMS_FAM
  resultats[[k]] <- list(zone = zone, auc = aucs, p = sapply(r, `[[`, "p"), base = S[, ncol(S)], phi_var = phi_var,
                         phi_fam = phi_fam)
}

# Tableaux ------------------------------------------------------------------------------------------------------
zones_auc <- rbindlist(lapply(names(resultats), function(k) {
  r <- resultats[[k]]
  data.table(comparaison = k, feux_zone = sum(r$zone), feux_reste = sum(!r$zone), ensemble = names(r$auc),
             variables = lengths(ENSEMBLES)[names(r$auc)], auc = r$auc)
}))
# Importance (moyenne des |contributions| au logit du classifieur « toutes variables ») par variable et par famille
zones_shap <- rbindlist(lapply(names(resultats), function(k) {
  r <- resultats[[k]]
  rbind(data.table(comparaison = k, niveau = "variable", nom = VARS, famille = NOMS_FAM[FAM_VARS],
                   shap_moy_abs = colMeans(abs(r$phi_var))),
        data.table(comparaison = k, niveau = "famille", nom = NOMS_FAM, famille = NOMS_FAM,
                   shap_moy_abs = colMeans(abs(r$phi_fam))))
}))
zones_shap[, part := shap_moy_abs / sum(shap_moy_abs), by = .(comparaison, niveau)]
setorder(zones_shap, comparaison, niveau, -shap_moy_abs)

# Contrôle de cohérence avec 07_distances.R : les variables qui distinguent le nord du sud pour le classifieur sont-elles
# celles dont les distributions s'écartent le plus (distance de Wasserstein) ?
w1 <- fread("resultats/tables/distances_nord_sud.csv")[, .(nom = variable, w1)]
zones_shap <- merge(zones_shap, w1, by = "nom", all.x = TRUE, sort = FALSE)
zones_shap[comparaison != "Nord contre sud" | niveau != "variable", w1 := NA]
setorder(zones_shap, comparaison, niveau, -shap_moy_abs)
setcolorder(zones_shap, c("comparaison", "niveau", "nom", "famille", "shap_moy_abs", "part", "w1"))
ns <- zones_shap[comparaison == "Nord contre sud" & niveau == "variable" & !is.na(w1)]
top_shap <- ns[order(-shap_moy_abs)][1:15, nom]
top_w1 <- ns[order(-w1)][1:15, nom]
cat(sprintf("\nCohérence nord / sud avec 07 : Spearman |SHAP| et Wasserstein %.2f (%d variables numériques) ; %d variables communes aux 15 premières de chaque classement\n",
            ns[, cor(shap_moy_abs, w1, method = "spearman")], nrow(ns), length(intersect(top_shap, top_w1))))

cat("\nAUC du classifieur discriminant (validation croisée communes x années, feux 2006-2018) :\n")
print(dcast(zones_auc, ensemble ~ comparaison, value.var = "auc")[order(-`Nord contre sud`)], digits = 3)
cat("\nCe qui distingue le nord du sud (familles, puis 10 variables) :\n")
print(zones_shap[comparaison == "Nord contre sud" & niveau == "famille", .(nom, shap = round(shap_moy_abs, 3), part = round(part, 3))])
print(zones_shap[comparaison == "Nord contre sud" & niveau == "variable"][1:10, .(nom, famille, shap = round(shap_moy_abs, 3), w1 = round(w1, 2))])

fwrite(zones_auc, "resultats/tables/zones_auc.csv")
fwrite(zones_shap, "resultats/tables/zones_shap.csv")
saveRDS(list(id_feu = d$id_feu, dep = d$dep, region = d$region_test, annee = d$annee, gcom = gcom, bloc = bloc,
             resultats = resultats), "donnees/modeles/zones.rds")
