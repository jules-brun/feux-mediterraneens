# Distributions et transfert : l'écart entre zones (10_zones.R) explique-t-il les résultats du transfert ?
#   1. Par région : séparabilité de la région (AUC du classifieur discriminant, par famille de variables) contre AUC du
#      test « région jamais vue » du modèle sans mémoire (01_modeles.R). Six points : une illustration, pas une preuve.
#   2. Sud -> nord repondéré : le modèle sans historique appris au sud (06_transfert.R) est réappris en donnant à chaque
#      feu du sud le poids w(x) = p(nord | x) / p(sud | x), rapport de densités estimé par le classifieur discriminant
#      (prédictions hors échantillon de 10_zones.R) : les feux du sud qui ressemblent à ceux du nord comptent plus
#      (correction du décalage des covariables par pondération d'importance). Poids tirés du classifieur à toutes les
#      variables, puis de celui à la météo seule ; normalisés à une moyenne de 1 ; taille d'échantillon effective de Kish
#      (somme des poids)² / somme des poids². Même nombre d'arbres et mêmes graines que 06 : sans poids, on retrouve
#      exactement l'AUC de resultats/tables/sud_nord.csv (vérifié).
# Entrées : jeu de données, modèles en cache (01), donnees/modeles/zones.rds et resultats/tables/zones_auc.csv (10),
#           resultats/tables/sud_nord.csv (06).
# Sorties : resultats/tables/transfert_regions.csv, transfert_repondere.csv
# Lancer depuis la racine du dépôt : Rscript analyses/12_transfert_zones.R

source("analyses/commun.R")
y <- feux$y_1ha
a <- feux$apprentissage

# 1. Séparabilité et transfert, région par région -----------------------------------------------------------------
res <- evaluer(SANS_MEMOIRE)                                   # en cache (01_modeles.R)
za <- fread("resultats/tables/zones_auc.csv")[comparaison != "Nord contre sud"]
za[, region := sub(" contre le reste$", "", comparaison)]
transfert <- feux[, .(feux = .N, taux = mean(y_1ha), auc_region_jamais_vue = auc(res$p_spatial[.I], y_1ha)), by = .(region = region_test)]
transfert <- merge(transfert, dcast(za, region ~ ensemble, value.var = "auc"), by = "region")
setnames(transfert, setdiff(names(transfert), c("region", "feux", "taux", "auc_region_jamais_vue")),
         paste0("separabilite_", setdiff(names(transfert), c("region", "feux", "taux", "auc_region_jamais_vue"))))
print(transfert[order(auc_region_jamais_vue)], digits = 3)
cat("\nSpearman entre séparabilité et AUC « région jamais vue » (6 régions) :\n")
print(sapply(grep("^separabilite_", names(transfert), value = TRUE), function(s)
  round(cor(transfert[[s]], transfert$auc_region_jamais_vue, method = "spearman"), 2)))

# 2. Sud -> nord repondéré ----------------------------------------------------------------------------------------
NORD <- c("04", "05", "07", "26", "48")
nord <- feux$dep %in% NORD
app <- which(a & !nord)
test_nord <- nord & !a
X <- matrice(feux, SANS_MEMOIRE)
ref <- fread("resultats/tables/sud_nord.csv")[modele == "Sans historique" & appris == "au sud"]
NR <- ref$nrounds
zones <- readRDS("donnees/modeles/zones.rds")
p_zone <- zones$resultats[["Nord contre sud"]]$p                # prédictions hors échantillon, feux 2006-2018
ligne <- match(feux$id_feu[app], zones$id_feu)
stopifnot(!anyNA(ligne))
poids <- function(ensemble) {
  p <- pmin(pmax(p_zone[ligne, ensemble], 1e-4), 1 - 1e-4)
  w <- p / (1 - p)
  w / mean(w)
}
VARIANTES <- list("Sans repondération (06)" = NULL, "Repondéré : toutes les variables" = poids("Toutes (hors mémoire)"),
                  "Repondéré : météo seule" = poids("Météo"))
predire_variante <- function(w) rowMeans(sapply(1:3, function(g) {
  if (is.null(w)) return(predire(ajuster(X[app, ], y[app], NR, g), X[test_nord, ]))
  set.seed(g)
  m <- xgb.train(params = PARAMS, data = xgb.DMatrix(X[app, ], label = y[app], weight = w), nrounds = NR, verbose = 0)
  predire(m, X[test_nord, ])
}))
p_var <- lapply(VARIANTES, predire_variante)
stopifnot(abs(auc(p_var[[1]], y[test_nord]) - ref$auc) < 1e-12)  # on retrouve exactement 06_transfert.R
repondere <- rbindlist(lapply(names(VARIANTES), function(k) {
  w <- VARIANTES[[k]]
  ess <- if (is.null(w)) length(app) else sum(w)^2 / sum(w^2)
  cbind(data.table(variante = k, feux_sud = length(app), taille_effective = ess,
                   part_1pct_plus_lourds = if (is.null(w)) 0.01 else sum(sort(w, decreasing = TRUE)[1:ceiling(length(w) / 100)]) / sum(w),
                   auc = auc(p_var[[k]], y[test_nord]), taux_predit = mean(p_var[[k]]), taux_observe = mean(y[test_nord])),
        gain_auc(p_var[[1]], p_var[[k]], y[test_nord], B = 500))
}))
cat(sprintf("\nSud -> nord, modèle sans historique, %d feux du sud 2006-2018, %d feux du nord 2019-2024 ; sur place : AUC %.3f\n",
            length(app), sum(test_nord), fread("resultats/tables/sud_nord.csv")[modele == "Sans historique" & appris != "au sud", auc]))
print(repondere, digits = 3)

fwrite(transfert, "resultats/tables/transfert_regions.csv")
fwrite(repondere, "resultats/tables/transfert_repondere.csv")
