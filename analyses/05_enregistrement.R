# Pratiques d'enregistrement : le taux d'échappement d'un département mesure aussi ce qu'il enregistre.
#   1. par département, part des feux notés à moins de 100 m² contre part des feux de 1 ha ou plus (Spearman) ;
#   2. la nouvelle fiche de 2023 : micro-feux et échappement dans les Pyrénées-Orientales, taux de la zone par année.
# Description seulement : les surfaces finales ne servent jamais de prédicteur.
# Entrée : jeu de données (fiches partielles exclues). Sorties : resultats/tables/enregistrement_departements.csv,
#          enregistrement_annees.csv

source("analyses/commun.R")
NOMS_DEP <- c("04" = "Alpes-de-Haute-Provence", "05" = "Hautes-Alpes", "06" = "Alpes-Maritimes", "07" = "Ardèche",
              "11" = "Aude", "13" = "Bouches-du-Rhône", "26" = "Drôme", "2A" = "Corse-du-Sud", "2B" = "Haute-Corse",
              "30" = "Gard", "34" = "Hérault", "48" = "Lozère", "66" = "Pyrénées-Orientales", "83" = "Var", "84" = "Vaucluse")

# 1. Par département
deps <- feux[, .(feux = .N, part_moins_100m2 = mean(surface_ha < 0.01), taux_1ha = mean(y_1ha)), keyby = dep]
deps[, nom := NOMS_DEP[dep]]
cat(sprintf("Micro-feux et échappement par département : Spearman %.2f (%d départements)\n",
            deps[, cor(part_moins_100m2, taux_1ha, method = "spearman")], nrow(deps)))

# 2. La nouvelle fiche de 2023
cat("\nPyrénées-Orientales :\n")
print(feux[dep == "66" & annee >= 2019, .(feux = .N, moins_100m2 = mean(surface_ha < 0.01), taux_1ha = mean(y_1ha)),
           keyby = .(periode = fifelse(annee <= 2022, "2019-2022", as.character(annee)))], digits = 3)
annees <- feux[, .(feux = .N, moins_100m2 = mean(surface_ha < 0.01), taux_1ha = mean(y_1ha)), keyby = annee]
cat(sprintf("Taux d'échappement de la zone : %.1f %% en 2023, %.1f %% en 2024, de %.1f à %.1f %% en 2006-2022\n",
            100 * annees[annee == 2023, taux_1ha], 100 * annees[annee == 2024, taux_1ha],
            100 * annees[annee <= 2022, min(taux_1ha)], 100 * annees[annee <= 2022, max(taux_1ha)]))

fwrite(deps, "resultats/tables/enregistrement_departements.csv")
fwrite(annees, "resultats/tables/enregistrement_annees.csv")
