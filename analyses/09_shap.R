# Socle SHAP : contributions de Shapley des modèles complet et sans mémoire, par variable et par groupe de variables,
# et interactions de Shapley ; matière des figures SHAP de la présentation.
#   1. contributions (TreeSHAP exact d'XGBoost, en log-odds) sur les feux de test 2019-2024, modèle de la première graine
#      appris sur 2006-2018 (le même que pour p34) : base + somme des contributions = logit de la prédiction (vérifié) ;
#      indicatrices d'une même variable additionnées ; groupes : la mémoire des lieux à part, les autres variables
#      dans leur famille ;
#   2. interactions de Shapley sur 2 000 feux de test tirés au hasard : phi_ij (i != j) est la moitié de l'interaction
#      de la paire (l'autre moitié est phi_ji), phi_ii l'effet propre de i ; moyennes des |interactions| par paire de
#      variables et de groupes, et valeurs par feu pour quelques paires végétation x météo.
# Entrée : modèles en cache de 01_modeles.R (sinon recalculés, ~10 min).
# Sorties : donnees/modeles/shap_complet.rds, shap_sans_memoire.rds,
#           resultats/tables/shap_familles.csv (importance par groupe), shap_interactions.csv (paires les plus fortes)
# Lancer depuis la racine du dépôt : Rscript analyses/09_shap.R

source("analyses/commun.R")

MODELES <- list(complet = PREDICTEURS, sans_memoire = SANS_MEMOIRE)
NOMS <- c(complet = "Toutes les variables", sans_memoire = "Sans la mémoire des lieux")
GROUPE <- setNames(ifelse(PREDICTEURS %in% MEMOIRE, "mémoire des lieux", FAMILLE), PREDICTEURS)
VEG_CLES <- c("bdfh_part_pins_med", "bdfh_part_landes", "clc_interface_habitat_comb_km_km2", "clc_surf_massif_max_ha",
              "ndvi_avant")
METEO_CLES <- c("humidite_alerte_pct", "rafale_max_alerte_kmh", "vent_moyen_alerte_kmh", "fwi_dispo_a_alerte")
N_INTER <- 2000   # feux pour les interactions (tableau feux x colonnes x colonnes, calculé par paquets de 200)

test <- which(!feux$apprentissage)
set.seed(1)
ech <- sort(sample(length(test), N_INTER))   # rangs parmi les feux de test

# Matrice d'agrégation : colonnes (indicatrices) -> clés (variables ou groupes), dans l'ordre des clés
agregation <- function(cle, niveaux = unique(cle)) {
  A <- matrix(0, length(cle), length(niveaux), dimnames = list(NULL, niveaux))
  A[cbind(seq_along(cle), match(cle, niveaux))] <- 1
  A
}

shap_modele <- function(nom) {
  vars <- MODELES[[nom]]
  res <- evaluer(vars)
  m <- xgb.load.raw(res$modele)
  X <- matrice(feux, vars)[test, ]
  p <- predire(m, X)

  # 1. Contributions
  s <- predict(m, xgb.DMatrix(X), predcontrib = TRUE)
  stopifnot(identical(colnames(s), c(colnames(X), "(Intercept)")))
  stopifnot(max(abs(rowSums(s) - qlogis(p))) < 1e-3)        # additivité, à la précision des flottants d'XGBoost
  var_col <- sub("=.*", "", colnames(X))                      # variable de chaque colonne de la matrice
  A_var <- agregation(var_col, vars)
  phi_var <- s[, colnames(X)] %*% A_var
  A_grp <- agregation(GROUPE[vars])
  phi_grp <- phi_var %*% A_grp

  # 2. Interactions, par paquets de 200 feux
  A_var1 <- rbind(A_var, 0)                                   # la ligne du biais ne compte dans aucune variable
  A_grp1 <- A_var1 %*% A_grp
  inter_var <- matrix(0, length(vars), length(vars), dimnames = list(vars, vars))
  inter_grp <- matrix(0, ncol(A_grp), ncol(A_grp), dimnames = list(colnames(A_grp), colnames(A_grp)))
  paires <- CJ(vegetation = intersect(VEG_CLES, vars), meteo = intersect(METEO_CLES, vars), sorted = FALSE)
  val_paires <- matrix(NA_real_, N_INTER, nrow(paires))
  for (paquet in split(seq_len(N_INTER), ceiling(seq_len(N_INTER) / 200))) {
    a <- predict(m, xgb.DMatrix(X[ech[paquet], , drop = FALSE]), predinteraction = TRUE)
    for (k in seq_along(paquet)) {
      ak <- a[k, , ]
      if (paquet[k] == 1) stopifnot(abs(sum(ak) - qlogis(p[ech[1]])) < 1e-3)
      iv <- crossprod(A_var1, ak %*% A_var1)                 # variables x variables
      inter_var <- inter_var + abs(iv)
      inter_grp <- inter_grp + abs(crossprod(A_grp1, ak %*% A_grp1))
      val_paires[paquet[k], ] <- 2 * iv[cbind(match(paires$vegetation, vars), match(paires$meteo, vars))]
    }
    cat(".")
  }
  cat("\n")

  list(modele = nom, variables = vars, groupe = GROUPE[vars], id_feu = feux$id_feu[test], y = feux$y_1ha[test], p = p,
       base = s[, "(Intercept)"], phi_var = phi_var, phi_grp = phi_grp, valeurs = feux[test, ..vars],
       ech = ech, inter_var = inter_var / N_INTER, inter_grp = inter_grp / N_INTER,
       paires = paires, val_paires = val_paires)
}

shap <- lapply(setNames(names(MODELES), names(MODELES)), function(nom) {
  cat("SHAP :", NOMS[[nom]], "\n")
  r <- shap_modele(nom)
  saveRDS(r, sprintf("donnees/modeles/shap_%s.rds", nom))
  r
})

# Importance par groupe : moyenne sur les feux de |somme des contributions du groupe|, en log-odds et en points de
# probabilité autour du taux moyen ; part = part de la somme des importances des groupes du modèle
familles <- rbindlist(lapply(shap, function(r) {
  data.table(modele = NOMS[[r$modele]], groupe = colnames(r$phi_grp), variables = as.vector(table(r$groupe)[colnames(r$phi_grp)]),
             shap_moy_abs = colMeans(abs(r$phi_grp)), points_moy_abs = colMeans(abs(en_points(r$phi_grp))))
}))
familles[, part := shap_moy_abs / sum(shap_moy_abs), by = modele]
setorder(familles, modele, -shap_moy_abs)
cat("\nImportance par groupe (moyenne des |contributions| du groupe, feux 2019-2024) :\n")
print(familles, digits = 3)

# Interactions : les 15 paires de variables (et toutes les paires de groupes) les plus fortes, |interaction de la paire|
# moyenne = 2 x moyenne |phi_ij|
paires_fortes <- function(M, n = Inf) {
  i <- which(upper.tri(M), arr.ind = TRUE)
  d <- data.table(a = rownames(M)[i[, 1]], b = colnames(M)[i[, 2]], interaction_moy_abs = 2 * M[i])
  head(d[order(-interaction_moy_abs)], n)
}
interactions <- rbindlist(lapply(shap, function(r) rbind(
  cbind(modele = NOMS[[r$modele]], niveau = "variables", paires_fortes(r$inter_var, 15)),
  cbind(modele = NOMS[[r$modele]], niveau = "groupes", paires_fortes(r$inter_grp)))))
cat("\nInteractions les plus fortes (moyenne des |interactions|, 2 000 feux 2019-2024) :\n")
print(interactions[niveau == "variables"], digits = 3)

fwrite(familles, "resultats/tables/shap_familles.csv")
fwrite(interactions, "resultats/tables/shap_interactions.csv")
