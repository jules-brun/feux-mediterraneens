# Échelle des modèles : météo et calendrier, sans mémoire des lieux, complet ; ce que rend, seule, une variable de
# mémoire (taux d'échappement passé du département, ou de la commune) ajoutée au modèle sans mémoire ; ce que
# regardent les modèles complet et sans mémoire (contributions de Shapley).
# Entrée : jeu de données (commun.R).
# Sorties : resultats/tables/modeles.csv (AUC par modèle et par test), modeles_gains.csv (écarts d'AUC, bootstrap),
#           modeles_importance.csv (contributions de Shapley des modèles complet et sans mémoire)
# Lancer depuis la racine du dépôt : Rscript analyses/01_modeles.R

source("analyses/commun.R")

cat(sprintf("%d feux, %d communes, %.1f %% de 1 ha ou plus ; %d feux de test 2019-2024\n", nrow(feux),
            uniqueN(feux$code_insee), 100 * mean(feux$y_1ha), sum(!feux$apprentissage)))
cat(length(PREDICTEURS), "prédicteurs :", paste(names(FAMILLES), lengths(FAMILLES), collapse = ", "), "\n")
cat(length(MEMOIRE), "variables de mémoire des lieux\n\n")

MODELES <- list(
  "Météo et calendrier" = PREDICTEURS[FAMILLE %in% c("calendrier", "météo")],
  "Sans la mémoire des lieux" = SANS_MEMOIRE,
  "Toutes les variables" = PREDICTEURS,
  "Sans mémoire + taux passé du département" = c(SANS_MEMOIRE, "taux_echappement_dep_10ans"),
  "Sans mémoire + taux passé de la commune" = c(SANS_MEMOIRE, "taux_echappement_commune_10ans_lisse"))
res <- lapply(MODELES, evaluer)

y_t <- feux$y_1ha[!feux$apprentissage]
y_s <- feux$y_1ha
modeles <- data.table(modele = names(MODELES), variables = lengths(MODELES), nrounds = sapply(res, `[[`, "nrounds"),
                      auc_futur = sapply(res, function(r) auc(r$p_temporel, y_t)),
                      auc_region = sapply(res, function(r) auc(r$p_spatial, y_s)))
print(modeles, digits = 3)

# Écarts d'AUC (intervalle à 95 % par bootstrap apparié)
comparer <- function(a, b) rbind(
  cbind(reference = a, modele = b, test = "années futures", gain_auc(res[[a]]$p_temporel, res[[b]]$p_temporel, y_t)),
  cbind(reference = a, modele = b, test = "région jamais vue", gain_auc(res[[a]]$p_spatial, res[[b]]$p_spatial, y_s)))
gains <- rbind(comparer("Sans la mémoire des lieux", "Toutes les variables"),
               comparer("Sans la mémoire des lieux", "Sans mémoire + taux passé du département"),
               comparer("Sans la mémoire des lieux", "Sans mémoire + taux passé de la commune"))
cat("\nÉcarts d'AUC par rapport au modèle sans mémoire :\n")
print(gains, digits = 3)

# Ce que regardent le modèle complet et le modèle sans mémoire : contribution moyenne |SHAP| de chaque variable sur
# les feux 2019-2024 (modèle de la première graine ; indicatrices d'une même variable regroupées)
importance <- rbindlist(lapply(c("Toutes les variables", "Sans la mémoire des lieux"), function(m) {
  X <- matrice(feux, MODELES[[m]])[!feux$apprentissage, ]
  s <- predict(xgb.load.raw(res[[m]]$modele), xgb.DMatrix(X), predcontrib = TRUE)[, colnames(X)]
  s <- sapply(split(seq_len(ncol(s)), sub("=.*", "", colnames(s))), function(j) rowSums(s[, j, drop = FALSE]))
  data.table(modele = m, variable = colnames(s), shap_moy_abs = colMeans(abs(s)))[order(-shap_moy_abs)]
}))
importance[, `:=`(famille = FAMILLE[match(variable, PREDICTEURS)], memoire = variable %in% MEMOIRE)]
cat("\nVariables les plus utilisées (|SHAP| moyen, feux 2019-2024) :\n")
print(importance[, head(.SD, 10), by = modele][, .(modele, variable, shap = round(shap_moy_abs, 3))])

fwrite(modeles, "resultats/tables/modeles.csv")
fwrite(gains, "resultats/tables/modeles_gains.csv")
fwrite(importance, "resultats/tables/modeles_importance.csv")
