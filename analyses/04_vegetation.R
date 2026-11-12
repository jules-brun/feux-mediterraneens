# Végétation à météo comparable : à danger météo égal, la végétation change-t-elle la chance qu'un départ dépasse 1 ha ?
#   1. strates de météo comparable : quintiles du risque prédit par un modèle météo seule (météo à l'alerte et bilan
#      hydrique), sur les feux de test 2019-2024 ;
#   2. dans chaque strate, végétation « défavorable » / « favorable » : les 20 % de feux où la somme des contributions de
#      Shapley de la végétation (modèle complet appris sur 2006-2018) est la plus haute / la plus basse ; taux observés ;
#   3. gain d'AUC quand on ajoute la végétation à la météo seule, puis à tout le reste, sans ou avec la mémoire des lieux.
# Entrées : jeu de données, donnees/calculs/*.parquet. Sorties : resultats/tables/vegetation_taux.csv, vegetation_gains.csv

source("analyses/commun.R")
source("analyses/commun_calculs.R")

# Végétation : nature (composition et continuité du combustible, aussi « là où partent les feux » avec le calcul 1 ;
# sans l'occupation du sol humaine), état (verdure, humidité de la végétation) et brûlures passées (calcul 2)
METEO <- PREDICTEURS[FAMILLE %in% c("météo", "bilan hydrique")]
SOL_HUMAIN <- c("clc_part_artificialise", "clc_part_agricole", "clc_interface_artif_comb_km_km2",
                "clc_interface_habitat_comb_km_km2")
VEG <- c(setdiff(FAMILLES[["végétation"]], SOL_HUMAIN), "part_veg_naturelle",
         grep("^a1_(part_|log10_tache|p_tache)", CALCUL1, value = TRUE),
         setdiff(FAMILLES[["état de la végétation"]], "part_veg_naturelle"), CALCUL2)
COMPLET <- c(PREDICTEURS, CALCUL1, CALCUL2, CALCUL3)
SANS_MEM <- c(SANS_MEMOIRE, CALCUL1, CALCUL2, CALCUL3)
cat(length(METEO), "variables de météo ;", length(VEG), "de végétation\n")

MODELES <- list(meteo = METEO, meteo_veg = c(METEO, VEG), sansmem_sans_veg = setdiff(SANS_MEM, VEG), sansmem = SANS_MEM,
                complet_sans_veg = setdiff(COMPLET, VEG), complet = COMPLET)
res <- lapply(MODELES, evaluer, d = feux, d_sp = feux_sp)
test <- !feux$apprentissage
y <- feux$y_1ha[test]
print(sapply(res, function(r) c(futur = auc(r$p_temporel, y), region = auc(r$p_spatial, feux$y_1ha))), digits = 3)

# 1-2. Strates de météo comparable, contribution de la végétation, taux observés -----------------------------
p_meteo <- res$meteo$p_temporel
strate <- cut(p_meteo, quantile(p_meteo, 0:5 / 5), include.lowest = TRUE, labels = paste0("Q", 1:5))
shap <- predict(xgb.load.raw(res$complet$modele), xgb.DMatrix(matrice(feux, COMPLET)[test, ]), predcontrib = TRUE)
phi_veg <- rowSums(shap[, VEG])
cat(sprintf("Rapport de cotes entre végétation défavorable et favorable (90e / 10e centile de sa contribution) : %.2f\n",
            exp(quantile(phi_veg, 0.9) - quantile(phi_veg, 0.1))))

v <- data.table(strate, y, phi_veg)
v[, classe := {
  q <- quantile(phi_veg, c(0.2, 0.8))
  fcase(phi_veg <= q[1], "favorable", phi_veg >= q[2], "défavorable", default = "intermédiaire")
}, by = strate]
taux <- v[classe != "intermédiaire", .(feux = .N, echappes = sum(y), taux = mean(y)), keyby = .(classe, strate)]
taux[, c("bas", "haut") := wilson(echappes, feux)]
print(taux, digits = 3)
fwrite(taux, "resultats/tables/vegetation_taux.csv")

# 3. Ce que la végétation ajoute, selon ce que le modèle sait déjà ---------------------------------------------
AJOUTS <- list("à la météo seule" = c("meteo", "meteo_veg"),
               "à tout le reste, sans la mémoire" = c("sansmem_sans_veg", "sansmem"),
               "à tout le reste, avec la mémoire" = c("complet_sans_veg", "complet"))
gains <- rbindlist(lapply(names(AJOUTS), function(k) {
  a <- res[[AJOUTS[[k]][1]]]
  b <- res[[AJOUTS[[k]][2]]]
  rbind(cbind(ajout = k, test = "années futures", gain_auc(a$p_temporel, b$p_temporel, y, B = 500)),
        cbind(ajout = k, test = "région jamais vue", gain_auc(a$p_spatial, b$p_spatial, feux$y_1ha, B = 500)))
}))
print(gains, digits = 3)
fwrite(gains, "resultats/tables/vegetation_gains.csv")
