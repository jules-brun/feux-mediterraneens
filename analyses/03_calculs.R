# Calculs 1 à 3 : ce que chaque variable construite rend au modèle sans mémoire des lieux, sur une région jamais vue ;
# puis le contrôle descriptif du calcul 2 (taux d'échappement l'année qui suit une brûlure).
# Entrées : jeu de données, donnees/calculs/*.parquet (calcul 3 : 02_calcul3_cause.R).
# Sortie : resultats/tables/calculs_gains.csv
# Lancer depuis la racine du dépôt : Rscript analyses/03_calculs.R

source("analyses/commun.R")
source("analyses/commun_calculs.R")

MODELES <- list("Sans la mémoire des lieux" = SANS_MEMOIRE,
                "Calcul 1 : présence humaine" = c(SANS_MEMOIRE, CALCUL1),
                "Calcul 2 : brûlures (satellite)" = c(SANS_MEMOIRE, CALCUL2),
                "Calcul 3 : cause probable" = c(SANS_MEMOIRE, CALCUL3),
                "Les trois calculs ensemble" = c(SANS_MEMOIRE, CALCUL1, CALCUL2, CALCUL3))
res <- lapply(MODELES, evaluer, d = feux, d_sp = feux_sp)

y <- feux$y_1ha
gains <- rbindlist(lapply(names(MODELES)[-1], function(m)
  cbind(modele = m, auc_region = auc(res[[m]]$p_spatial, y), gain_auc(res[[1]]$p_spatial, res[[m]]$p_spatial, y))))
cat("Gain d'AUC sur une région jamais vue, par rapport au modèle sans mémoire :\n")
print(gains, digits = 3)
fwrite(gains, "resultats/tables/calculs_gains.csv")

# Calcul 2, contrôle descriptif : à année et danger météo égaux, taux d'échappement des feux partis dans l'année qui
# suit une brûlure (au moins 1 % de la végétation de la commune) rapporté à celui des communes sans brûlure vue depuis
# novembre 2000. Rapport de risques de Mantel-Haenszel, strates année x quintile de FWI, variance de Greenland et Robins.
x <- feux[!is.na(a2_annees_depuis_brulure_1pct) & !is.na(fwi_dispo_a_alerte)]
x[, strate := paste(annee, cut(fwi_dispo_a_alerte, quantile(fwi_dispo_a_alerte, 0:5 / 5), include.lowest = TRUE))]
x[, groupe := fcase(a2_brulure_1pct_censuree, "aucune brûlure", a2_annees_depuis_brulure_1pct < 1, "brûlée l'année passée")]
s <- merge(x[groupe == "brûlée l'année passée", .(a = as.numeric(sum(y_1ha)), n1 = as.numeric(.N)), by = strate],
           x[groupe == "aucune brûlure", .(c = as.numeric(sum(y_1ha)), n0 = as.numeric(.N)), by = strate], by = "strate")
s[, N := n1 + n0]
R <- s[, sum(a * n0 / N)]
S <- s[, sum(c * n1 / N)]
v <- s[, sum((n1 * n0 * (a + c) - a * c * N) / N^2)] / (R * S)
cat(sprintf("\nCalcul 2 : rapport de risques l'année qui suit une brûlure %.2f [%.2f ; %.2f] (%d feux)\n",
            R / S, exp(log(R / S) - 1.96 * sqrt(v)), exp(log(R / S) + 1.96 * sqrt(v)), x[groupe == "brûlée l'année passée", .N]))
