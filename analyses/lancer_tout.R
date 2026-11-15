# Toute l'analyse, dans l'ordre, chaque script dans une session R neuve (environ 1 h 45 la première fois, dont 42 min
# pour 10_zones.R ; les modèles déjà évalués sont repris de donnees/modeles/).
# Prérequis : donnees/final/feux_med_2006_2024.parquet (ou .csv avec data_dictionary.csv) (Kaggle) et les sorties des
# calculs 1 et 2 (calculs/, Python) dans donnees/calculs/. Lancer depuis la racine du dépôt : Rscript analyses/lancer_tout.R
# Les figures (08) viennent en dernier : elles lisent les sorties de tous les autres scripts.

for (s in c("01_modeles", "02_calcul3_cause", "03_calculs", "04_vegetation", "05_enregistrement", "06_transfert",
            "07_distances", "09_shap", "10_zones", "11_profils_zones", "12_transfert_zones", "08_figures")) {
  cat("\n=====", s, "=====\n")
  debut <- Sys.time()
  if (system2(file.path(R.home("bin"), "Rscript"), file.path("analyses", paste0(s, ".R"))) != 0) stop("échec de ", s)
  cat(sprintf("(%.1f min)\n", as.numeric(difftime(Sys.time(), debut, units = "mins"))))
}
