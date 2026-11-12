# Distances entre distributions dans le temps : la distribution d'une variable a-t-elle bougé, et le modèle le voit-il ?
#   1. chaque année 2012-2024 contre les années précédentes, variable par variable : distance de Wasserstein-1 (aire entre
#      les fonctions de répartition, en écarts-types de la référence) et de Kolmogorov-Smirnov (écart maximal) ; même
#      distance pour les prédictions du modèle glissant (06_transfert.R) contre ses prédictions hors échantillon ;
#   2. référence du hasard : journées de feu passées tirées au hasard (les départs d'un même jour partagent la météo) ;
#   3. 2023 : bruit avec des départs tirés un par un ou des journées entières ; 4. le nord contre le sud (2006-2018).
# Entrées : jeu de données, donnees/modeles/predictions_glissantes.rds. Sorties : resultats/tables/distances_annees.csv,
#           distances_variables.csv, distances_nord_sud.csv, hasard_2023.csv

source("analyses/commun.R")
glissant <- readRDS("donnees/modeles/predictions_glissantes.rds")
FAMILLE_D <- c("météo" = "météo et végétation vivante", "bilan hydrique" = "météo et végétation vivante",
               "état de la végétation" = "météo et végétation vivante", "calendrier" = "saison et heure",
               "végétation" = "lieux", "relief et position" = "lieux", "présence humaine" = "lieux",
               "historique" = "historique des feux")
NUM <- setdiff(PREDICTEURS[sapply(PREDICTEURS, function(v) is.numeric(feux[[v]]) || is.logical(feux[[v]]))], "annee")
famille_d <- function(v) unname(FAMILLE_D[FAMILLE[match(v, PREDICTEURS)]])
METEO_V <- NUM[famille_d(NUM) == "météo et végétation vivante"]
VALEURS <- lapply(setNames(NUM, NUM), function(v) as.numeric(feux[[v]]))
JOUR <- as.integer(as.Date(feux$date_alerte_utc, tz = "UTC"))   # journée de l'alerte (date UTC)

# Distances entre l'échantillon a (cible) et l'échantillon b (référence), valeurs manquantes retirées
w1_ks <- function(a, b) {
  a <- a[!is.na(a)]
  b <- b[!is.na(b)]
  if (length(a) < 30 || length(b) < 30) return(c(w1 = NA, ks = NA))
  v <- sort(unique(c(a, b)))
  ecart <- abs(ecdf(a)(v) - ecdf(b)(v))
  c(w1 = sum(ecart[-length(v)] * diff(v)) / sd(b), ks = max(ecart))
}
distances <- function(ia, ib, vars = NUM) rbindlist(lapply(vars, function(v) {
  x <- VALEURS[[v]]
  r <- w1_ks(x[ia], x[ib])
  data.table(variable = v, famille = famille_d(v), w1 = r[["w1"]], ks = r[["ks"]],
             ecart_moyenne = (mean(x[ia], na.rm = TRUE) - mean(x[ib], na.rm = TRUE)) / sd(x[ib], na.rm = TRUE))
}))
w1_meteo <- function(ia, ib) mean(sapply(METEO_V, function(v) w1_ks(VALEURS[[v]][ia], VALEURS[[v]][ib])[["w1"]]), na.rm = TRUE)

# 1-2. Année par année, avec la zone du hasard (40 tirages de journées) ------------------------------------------
variables <- rbindlist(lapply(glissant, function(g) distances(g$cible, g$reference)[, annee := g$annee]))
annees <- rbindlist(lapply(glissant, function(g) {
  iref <- g$reference
  jours <- unique(JOUR[iref])
  nj <- uniqueN(JOUR[g$cible])
  set.seed(g$annee)
  bruit <- replicate(40, {
    dans <- JOUR[iref] %in% sample(jours, nj)
    c(meteo = w1_meteo(iref[dans], iref[!dans]), predictions = w1_ks(g$p_reference[dans], g$p_reference[!dans])[["w1"]])
  })
  dv <- variables[annee == g$annee]
  y <- feux$y_1ha[g$cible]
  r <- data.table(annee = g$annee, jours = nj, erreur = mean(g$p) - mean(y), w1_variables = mean(dv$w1, na.rm = TRUE),
                  w1_predictions = w1_ks(g$p, g$p_reference)[["w1"]], decalage_predictions = mean(g$p) - mean(g$p_reference),
                  hasard_meteo = quantile(bruit["meteo", ], 0.95, names = FALSE),
                  hasard_predictions = quantile(bruit["predictions", ], 0.95, names = FALSE))
  r[, paste0("w1_", unique(FAMILLE_D)) := lapply(unique(FAMILLE_D), function(f) dv[famille == f, mean(w1, na.rm = TRUE)])]
  r
}))
print(annees[, .(annee, jours, erreur = round(100 * erreur, 1), w1_variables, w1_meteo = `w1_météo et végétation vivante`,
                 hasard_meteo, w1_predictions, hasard_predictions)], digits = 2)
cat("Années où la météo et la végétation vivante dépassent le hasard :",
    annees[`w1_météo et végétation vivante` > hasard_meteo, paste(annee, collapse = " ")], "\n")
spearman <- function(x, e) cor.test(x, e, method = "spearman")
cat(sprintf("Spearman, distance des variables et erreur de niveau : %.2f (p = %.2f) ; variables et prédictions : %.2f\n",
            spearman(annees$w1_variables, abs(annees$erreur))$estimate, spearman(annees$w1_variables, abs(annees$erreur))$p.value,
            spearman(annees$w1_variables, annees$w1_predictions)$estimate))
cat(sprintf("2024 : prédictions décalées de %.1f points ; taux d'échappement passés de la commune %.2f, du département %.2f écart-type\n",
            100 * annees[annee == 2024, decalage_predictions],
            variables[annee == 2024 & variable == "taux_echappement_commune_10ans_lisse", ecart_moyenne],
            variables[annee == 2024 & variable == "taux_echappement_dep_10ans", ecart_moyenne]))
print(variables[annee == 2022][order(-w1)][1:5, .(variable, w1, ks)], digits = 2)

# 3. 2023 : départs tirés un par un ou journées entières (200 tirages chacun) -------------------------------------
iref <- which(feux$annee < 2023)
icib <- which(feux$annee == 2023)
set.seed(1)
un_par_un <- replicate(200, {
  dans <- seq_along(iref) %in% sample(length(iref), length(icib))
  w1_meteo(iref[dans], iref[!dans])
})
jours <- unique(JOUR[iref])
journees <- replicate(200, {
  dans <- JOUR[iref] %in% sample(jours, uniqueN(JOUR[icib]))
  w1_meteo(iref[dans], iref[!dans])
})
hasard <- rbind(data.table(tirage = "un par un", distance = un_par_un), data.table(tirage = "journées", distance = journees),
                data.table(tirage = "observé", distance = w1_meteo(icib, iref)))
print(hasard[, .(moyenne = mean(distance), q95 = quantile(distance, 0.95)), by = tirage], digits = 2)
cat(sprintf("Seuil de Kolmogorov-Smirnov à 5 %% : %.3f (%d contre %d départs)\n",
            1.36 * sqrt((length(icib) + length(iref)) / (length(icib) * length(iref))), length(icib), length(iref)))

# 4. Le nord contre le sud, 2006-2018 -----------------------------------------------------------------------------
nord <- feux$dep %in% c("04", "05", "07", "26", "48")
nord_sud <- distances(which(feux$apprentissage & nord), which(feux$apprentissage & !nord))
cat(sprintf("\nNord contre sud : distance moyenne %.2f (entre années : %.2f à %.2f)\n", mean(nord_sud$w1, na.rm = TRUE),
            min(annees$w1_variables), max(annees$w1_variables)))
print(nord_sud[!variable %in% c("lon", "lat")][order(-w1)][1:5, .(variable, w1)], digits = 2)

fwrite(annees, "resultats/tables/distances_annees.csv")
fwrite(variables, "resultats/tables/distances_variables.csv")
fwrite(nord_sud, "resultats/tables/distances_nord_sud.csv")
fwrite(hasard, "resultats/tables/hasard_2023.csv")
