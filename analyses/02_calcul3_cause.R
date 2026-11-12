# Calcul 3 : deviner la cause du feu à l'alerte, avec un modèle appris sur les causes notées après enquête
# (information privilégiée : connue pour les feux passés, jamais pour le feu qu'on prédit).
#   1. profil de la commune : part lissée de chaque cause parmi ses feux passés à cause connue (alertés avant J - 7 j),
#      et départs de la commune dans les 6 heures qui précèdent l'alerte ;
#   2. modèle de cause (XGBoost à 5 classes, sans la mémoire des lieux) : probabilités hors échantillon pour tous les feux
#      (blocs d'années pour 2006-2018, modèle appris sur 2006-2018 pour le test), puis version « région jamais vue ».
# Le jeu publié commence en 2006 : le profil y ajoute les feux BDIFF de 1997-2005 (historique/feux_1997_2005.csv).
# Entrées : jeu de données (commun.R), historique 1997-2005. Sorties : donnees/calculs/calcul3_cause.parquet,
# calcul3_cause_regions.parquet

source("analyses/commun.R")
CAUSES <- c(malveillance = "Malveillance", particulier = "Involontaire (particulier)", travaux = "Involontaire (travaux)",
            accidentelle = "Accidentelle", naturelle = "Naturelle")
K <- 9        # poids de l'a priori du lissage (en feux), estimé sur les feux 1997-2005 de la base BDIFF complète
GARDE_J <- 7  # la cause d'un feu n'est utilisée qu'à partir du 7e jour après son alerte

# 1. Profil de causes et série de départs ---------------------------------------------------------------
# Historique : feux BDIFF 1997-2005, puis tous les feux du jeu publié (fiches partielles et feux sans commune
# compris), doublons probables comptés une fois ; jour local et instant UTC de l'alerte
historique <- rbind(
  fread("historique/feux_1997_2005.csv", colClasses = list(character = c("dep", "code_insee", "cause"))),
  jeu[doublon_probable_bdiff == FALSE, .(date_alerte_utc, dep = as.character(dep),
                                         code_insee = as.character(code_insee), cause = as.character(cause))])
historique[cause == "", cause := NA]
base <- historique[, .(dep, code_insee, cause, t = as.numeric(date_alerte_utc),
                       jour = as.IDate(as.Date(date_alerte_utc, tz = "Europe/Paris")))]
cible <- feux[, .(dep, code_insee, t_fin = as.numeric(date_alerte_utc),
                  borne = as.IDate(as.Date(date_alerte_utc, tz = "Europe/Paris")) - GARDE_J)]
cible[, t_debut := t_fin - 6 * 3600]

# Feux passés de chaque cause, dans la commune ou le département, alertés avant J - 7 jours
compter <- function(passe, lieu) passe[cible, on = c(lieu, "jour<borne"), .N, by = .EACHI]$N
n_com <- sapply(CAUSES, function(k) compter(base[cause == k], "code_insee"))
n_dep <- sapply(CAUSES, function(k) compter(base[cause == k], "dep"))
# Lissage bayésien empirique vers la répartition du département : part_k = (n_k + K pi_k) / (n + K)
# (aucun feu passé à cause connue dans le département, au tout début de 2006 : profil manquant)
part <- (n_com + K * n_dep / rowSums(n_dep)) / (rowSums(n_com) + K)
part[rowSums(n_dep) == 0, ] <- NA
profil <- data.table(a3_n_causes_connues = rowSums(n_com))
profil[, paste0("a3_part_", names(CAUSES)) := as.data.table(part)]
# Autres départs de la commune dans les 6 heures précédant l'alerte (ex aequo exclus)
profil[, a3_nb_feux_commune_6h := base[!is.na(code_insee)][cible, on = .(code_insee, t >= t_debut, t < t_fin),
                                                         .N, by = .EACHI]$N]

# 2. Modèle de cause --------------------------------------------------------------------------------------
d <- cbind(feux, profil)
X <- matrice(d, c(SANS_MEMOIRE, names(profil)))
y <- match(d$cause, CAUSES) - 1L   # 0 = malveillance ... 4 = naturelle ; NA si cause inconnue
connue <- !is.na(y)
P_CAUSE <- modifyList(PARAMS, list(objective = "multi:softprob", eval_metric = "mlogloss", num_class = length(CAUSES)))

# Chaque bloc d'années est prédit par un modèle appris (et réglé) sur les autres blocs ; le test 2019-2024 par un
# modèle appris sur 2006-2018. Seuls les feux à cause connue servent à apprendre.
P <- matrix(NA_real_, nrow(d), length(CAUSES), dimnames = list(NULL, names(CAUSES)))
plis <- c(lapply(seq_along(BLOCS_ANNEES), function(b) list(predits = BLOCS_ANNEES[[b]], reglage = BLOCS_ANNEES[-b])),
          list(list(predits = 2019:2024, reglage = BLOCS_ANNEES)))
for (pl in plis) {
  app <- which(connue & d$annee %in% unlist(pl$reglage))
  nr <- choisir_nrounds(X[app, ], y[app], d$annee[app], pl$reglage, P_CAUSE)
  i <- which(d$annee %in% pl$predits)
  P[i, ] <- predire(ajuster(X[app, ], y[app], nr, params = P_CAUSE), X[i, ])
  cat(sprintf("années %d-%d prédites : %4d arbres, %5d feux appris\n", min(pl$predits), max(pl$predits), nr, length(app)))
}

# Version « région jamais vue » : chaque région prédite par un modèle appris sur les autres (toutes années),
# avec le nombre d'arbres du modèle du test
P_sp <- P
for (r in unique(d$region_test)) {
  t <- d$region_test == r
  app <- which(connue & !t)
  P_sp[t, ] <- predire(ajuster(X[app, ], y[app], nr, params = P_CAUSE), X[t, ])
}

test <- !d$apprentissage & connue
cat("\nAUC de chaque cause contre les autres (test 2019-2024, feux à cause connue) :\n")
print(round(sapply(seq_along(CAUSES), function(k) auc(P[test, k], y[test] == k - 1L)) |> setNames(names(CAUSES)), 3))

colonnes_p <- paste0("a3_p_", names(CAUSES))
arrow::write_parquet(cbind(d[, .(id_feu)], setnames(as.data.table(P), colonnes_p), profil),
                     "donnees/calculs/calcul3_cause.parquet")
arrow::write_parquet(cbind(d[, .(id_feu)], setnames(as.data.table(P_sp), colonnes_p)),
                     "donnees/calculs/calcul3_cause_regions.parquet")
