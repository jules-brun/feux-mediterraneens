# Variables construites des calculs 1 à 3, ajoutées aux feux (sourcé après commun.R par 03_calculs.R et 04_vegetation.R)
#
# Entrées : donnees/calculs/calcul1_lieux(_regions).parquet, calcul2_brulures.parquet, calcul3_cause(_regions).parquet
# Crée : CALCUL1 (14 variables : composition et accès « là où partent les feux », pondérés par l'intensité de départ λ),
#        CALCUL2 (13 : brûlures passées vues par satellite), CALCUL3 (6 : probabilités de cause, départs de la commune en 6 h) ;
#        feux_sp : copie de feux où les variables apprises par un modèle (λ du calcul 1, cause du calcul 3) sont remplacées
#        par leur version « région jamais vue », pour le test spatial (sinon il serait optimiste).

lire_calcul <- function(fichier, motif) {
  r <- as.data.table(arrow::read_parquet(file.path("donnees/calculs", fichier)))
  r[match(feux$id_feu, r$id_feu), grep(motif, names(r)), with = FALSE]
}
MOTIF1 <- "_lambda$|^a1_intensite_departs$"
MOTIF3 <- "^a3_p_|^a3_nb_feux_commune_6h$"
c1 <- lire_calcul("calcul1_lieux.parquet", MOTIF1)
c2 <- lire_calcul("calcul2_brulures.parquet", "^a2_")
c3 <- lire_calcul("calcul3_cause.parquet", MOTIF3)
CALCUL1 <- names(c1)
CALCUL2 <- names(c2)
CALCUL3 <- names(c3)
feux <- cbind(feux, c1, c2, c3)

feux_sp <- copy(feux)
feux_sp[, (CALCUL1) := lire_calcul("calcul1_lieux_regions.parquet", MOTIF1)]
feux_sp[, (grep("^a3_p_", CALCUL3, value = TRUE)) := lire_calcul("calcul3_cause_regions.parquet", "^a3_p_")]
rm(c1, c2, c3)
