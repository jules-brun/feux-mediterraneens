# Profils des zones : en quoi chaque zone diffère-t-elle du reste, variable par variable ? Complément univarié du
# classifieur discriminant de 10_zones.R (qui regarde toutes les variables à la fois).
#   Mêmes zones (nord contre sud, chaque région contre les cinq autres), mêmes feux (2006-2018) et mêmes variables
#   (sans mémoire des lieux ni année) que 10_zones.R ; variables numériques et booléennes seulement (une distance entre
#   fonctions de répartition n'a pas de sens pour une catégorie).
#   Distances de 07_distances.R (w1_ks de commun.R) : Wasserstein-1 en écarts-types du reste, Kolmogorov-Smirnov ;
#   sens de l'écart : différence des moyennes (zone - reste) en écarts-types du reste. Le nord contre le sud est
#   recalculé ici et doit redonner exactement resultats/tables/distances_nord_sud.csv (vérifié).
# Entrée : jeu de données. Sortie : resultats/tables/profils_zones.csv
# Lancer depuis la racine du dépôt : Rscript analyses/11_profils_zones.R

source("analyses/commun.R")

NORD <- c("04", "05", "07", "26", "48")
d <- feux[apprentissage == TRUE]
VARS <- setdiff(SANS_MEMOIRE, "annee")
NUM <- VARS[sapply(VARS, function(v) is.numeric(d[[v]]) || is.logical(d[[v]]))]
NOMS_FAM <- c("calendrier" = "Calendrier", "météo" = "Météo", "bilan hydrique" = "Bilan hydrique",
              "végétation" = "Végétation", "état de la végétation" = "État de la végétation",
              "relief et position" = "Relief et position", "présence humaine" = "Présence humaine",
              "historique" = "Autres départs récents")
regions <- sort(unique(d$region_test))
COMPARAISONS <- c(list("Nord contre sud" = d$dep %in% NORD),
                  setNames(lapply(regions, function(r) d$region_test == r), paste(regions, "contre le reste")))

profils <- rbindlist(lapply(names(COMPARAISONS), function(k) {
  zone <- COMPARAISONS[[k]]
  rbindlist(lapply(NUM, function(v) {
    x <- as.numeric(d[[v]])
    r <- w1_ks(x[zone], x[!zone])
    data.table(comparaison = k, variable = v, famille = NOMS_FAM[[FAMILLE[match(v, PREDICTEURS)]]], w1 = r[["w1"]],
               ks = r[["ks"]], ecart_moyenne = (mean(x[zone], na.rm = TRUE) - mean(x[!zone], na.rm = TRUE)) / sd(x[!zone], na.rm = TRUE),
               moyenne_zone = mean(x[zone], na.rm = TRUE), moyenne_reste = mean(x[!zone], na.rm = TRUE))
  }))
}))

# Contrôle : le nord contre le sud redonne exactement les distances de 07_distances.R
ref <- fread("resultats/tables/distances_nord_sud.csv")
ns <- merge(profils[comparaison == "Nord contre sud"], ref[, .(variable, w1_07 = w1, ks_07 = ks)], by = "variable")
stopifnot(nrow(ns) == length(NUM), isTRUE(all.equal(ns$w1, ns$w1_07)), isTRUE(all.equal(ns$ks, ns$ks_07)))
cat(sprintf("%d variables, %d comparaisons ; nord contre sud identique à 07_distances.R\n", length(NUM), length(COMPARAISONS)))

cat("\nDistance de Wasserstein moyenne par famille (en écarts-types du reste) :\n")
print(dcast(profils[, .(w1 = mean(w1, na.rm = TRUE)), by = .(comparaison, famille)], famille ~ comparaison, value.var = "w1"),
      digits = 2)
cat("\nLes 4 variables qui s'écartent le plus, par zone :\n")
print(profils[order(-w1), head(.SD, 4), by = comparaison][, .(comparaison, variable, w1 = round(w1, 2),
                                                              sens = round(ecart_moyenne, 2))])
fwrite(profils, "resultats/tables/profils_zones.csv")
