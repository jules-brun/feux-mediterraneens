# Transferts dans le temps et dans l'espace.
#   1. Avant/après : chaque année de 2012 à 2024 prédite par le modèle complet appris sur toutes les années précédentes
#      (une graine, nombre d'arbres du test temporel) : AUC et part prédite contre part observée. On garde aussi des
#      prédictions hors échantillon des années précédentes (4 groupes d'années), référence de la dérive des prédictions.
#   2. Sud -> nord : apprendre au sud (littoral et Corse), prédire le nord (intérieur et montagne), feux du nord 2019-2024,
#      comparé au même modèle appris sur place.
# Entrée : jeu de données. Sorties : resultats/tables/avant_apres.csv, sud_nord.csv, sud_nord_gains.csv,
#          donnees/modeles/predictions_glissantes.rds (pour 07_distances.R)

source("analyses/commun.R")
y <- feux$y_1ha
a <- feux$apprentissage

# 1. Avant/après ------------------------------------------------------------------------------------------
X <- matrice(feux, PREDICTEURS)
NR <- choisir_nrounds(X[a, ], y[a], feux$annee[a])
# Prédictions hors échantillon des feux de référence : 4 groupes d'années tirés au hasard, chacun prédit par un
# modèle appris sur les 3 autres
hors_echantillon <- function(iref) {
  annees <- sort(unique(feux$annee[iref]))
  set.seed(1)
  groupe <- setNames(sample(rep(1:4, length.out = length(annees))), annees)[as.character(feux$annee[iref])]
  p <- numeric(length(iref))
  for (g in 1:4) p[groupe == g] <- predire(ajuster(X[iref[groupe != g], ], y[iref[groupe != g]], NR), X[iref[groupe == g], ])
  p
}
glissant <- lapply(2012:2024, function(t) {
  iref <- which(feux$annee < t)
  icib <- which(feux$annee == t)
  list(annee = t, cible = icib, p = predire(ajuster(X[iref, ], y[iref], NR), X[icib, ]), reference = iref,
       p_reference = hors_echantillon(iref))
})
saveRDS(glissant, "donnees/modeles/predictions_glissantes.rds")

avant_apres <- rbindlist(lapply(glissant, function(g) {
  yt <- y[g$cible]
  data.table(annee = g$annee, feux = length(yt), observe = mean(yt), obs_bas = wilson(sum(yt), length(yt))[[1]],
             obs_haut = wilson(sum(yt), length(yt))[[2]], predit = mean(g$p), auc = auc(g$p, yt))
}))
print(avant_apres, digits = 3)
cat(sprintf("AUC de %.2f à %.2f ; écart de niveau au plus %.1f points hors 2023 (2023 : %.1f %% prédit, %.1f %% observé)\n",
            min(avant_apres$auc), max(avant_apres$auc), 100 * avant_apres[annee != 2023, max(abs(predit - observe))],
            100 * avant_apres[annee == 2023, predit], 100 * avant_apres[annee == 2023, observe]))
fwrite(avant_apres, "resultats/tables/avant_apres.csv")

# 2. Sud -> nord ------------------------------------------------------------------------------------------
NORD <- c("04", "05", "07", "26", "48")
nord <- feux$dep %in% NORD
cat(sprintf("\nNord : %d feux, %.0f %% d'échappement ; sud : %d feux, %.0f %%\n", sum(nord), 100 * mean(y[nord]),
            sum(!nord), 100 * mean(y[!nord])))
test_nord <- nord & !a
ENSEMBLES <- list("Météo et calendrier" = PREDICTEURS[FAMILLE %in% c("calendrier", "météo")],
                  "Sans historique" = SANS_MEMOIRE, "Avec historique des feux" = PREDICTEURS)
p_nord <- list()
sud_nord <- list()
for (k in names(ENSEMBLES)) {
  Xk <- matrice(feux, ENSEMBLES[[k]])
  for (lieu in c("au sud", "sur place, au nord")) {
    app <- a & nord == (lieu != "au sud")
    nr <- choisir_nrounds(Xk[app, ], y[app], feux$annee[app])
    p <- rowMeans(sapply(1:3, function(g) predire(ajuster(Xk[app, ], y[app], nr, g), Xk[test_nord, ])))
    p_nord[[paste(k, lieu)]] <- p
    sud_nord[[paste(k, lieu)]] <- data.table(modele = k, appris = lieu, nrounds = nr, auc = auc(p, y[test_nord]),
                                             taux_predit = mean(p), taux_observe = mean(y[test_nord]))
  }
}
sud_nord <- rbindlist(sud_nord)
cat(sum(test_nord), "feux du nord 2019-2024\n")
print(sud_nord, digits = 3)

comparer <- function(a, b, question) cbind(question, gain_auc(p_nord[[a]], p_nord[[b]], y[test_nord], B = 500))
gains <- rbind(comparer("Sans historique au sud", "Sans historique sur place, au nord", "apprendre au nord plutôt qu'au sud"),
               comparer("Sans historique au sud", "Avec historique des feux au sud", "ajouter l'historique (appris au sud)"),
               comparer("Météo et calendrier au sud", "Sans historique au sud",
                        "ajouter végétation, relief et présence humaine à la météo (appris au sud)"))
print(gains, digits = 3)
fwrite(sud_nord, "resultats/tables/sud_nord.csv")
fwrite(gains, "resultats/tables/sud_nord_gains.csv")
