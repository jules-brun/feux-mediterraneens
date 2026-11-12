# Socle commun : données, échantillon, familles de variables, protocole XGBoost, mesures
#
# Sourcé en tête de chaque script (source("analyses/commun.R")), depuis la racine du dépôt.
# Entrée : donnees/final/feux_med_2006_2024.parquet (jeu publié sur Kaggle).
# Protocole commun à toutes les comparaisons : même échantillon, mêmes découpages (temps : apprentissage
# 2006-2018, test 2019-2024 ; espace : 6 régions tour à tour jamais vues), mêmes hyperparamètres ; seul le
# nombre d'arbres est choisi, par validation croisée sur des blocs d'années, de la même façon pour chaque modèle.

suppressPackageStartupMessages({
  library(data.table)
  library(xgboost)
})
dir.create("resultats/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("donnees/modeles", recursive = TRUE, showWarnings = FALSE)

# Données ------------------------------------------------------------------------------------------------
jeu <- as.data.table(arrow::read_parquet("donnees/final/feux_med_2006_2024.parquet"))   # 31 942 feux
# Les nombres décimaux sont ramenés à 8 chiffres significatifs, comme dans les versions .csv et .rds du jeu sur
# lesquelles ont été calculés les chiffres du diaporama (sinon les arbres d'XGBoost changent un peu)
decimaux <- names(jeu)[sapply(jeu, function(x) is.double(x) && !inherits(x, "POSIXct"))]
jeu[, (decimaux) := lapply(.SD, signif, 8), .SDcols = decimaux]
# Ordre des modalités du jeu d'origine : il fixe l'ordre des colonnes de la matrice, donc les tirages d'XGBoost
jeu[, creneau := factor(creneau, c("nuit", "matin", "apres_midi", "soir"))]
jeu[, direction_vent_alerte_secteur := factor(direction_vent_alerte_secteur, c("N", "NE", "E", "SE", "S", "SO", "O", "NO"))]
jeu[, dep := as.character(dep)]

# Échantillon étudié : sans les fiches partielles ni les feux sans commune (31 530 feux)
feux <- jeu[!donnees_partielles & !is.na(code_insee)]
feux[, apprentissage := annee <= 2018]

# Régions du test « région jamais vue » : départements voisins regroupés
REGIONS <- c("2A" = "Corse", "2B" = "Corse", "13" = "Provence", "84" = "Provence", "04" = "Provence",
             "05" = "Provence", "83" = "Var-Alpes-Maritimes", "06" = "Var-Alpes-Maritimes",
             "34" = "Languedoc", "30" = "Languedoc", "48" = "Languedoc", "66" = "Roussillon-Aude",
             "11" = "Roussillon-Aude", "07" = "Ardèche-Drôme", "26" = "Ardèche-Drôme")
feux[, region_test := REGIONS[dep]]

# Variables ----------------------------------------------------------------------------------------------
# Les 174 prédicteurs sont rangés par famille dans le jeu de données : chaque famille va d'une colonne à une autre
colonnes <- function(de, a) names(feux)[match(de, names(feux)):match(a, names(feux))]
FAMILLES <- list(
  "calendrier" = colonnes("annee", "vacances_scolaires_cl"),
  "météo" = colonnes("temperature_alerte_c", "vent_max_veille_kmh"),
  "bilan hydrique" = colonnes("swi_sim2", "etp_sim2_30j_mm"),
  "végétation" = colonnes("bdf_part_pins_med", "clc_interface_habitat_comb_km_km2"),
  "état de la végétation" = colonnes("ndvi_avant", "part_veg_naturelle"),
  "relief et position" = colonnes("dep", "part_versant_sud"),
  "présence humaine" = colonnes("population_connue_hab", "dens_routes_secondaires_locales_km_km2"),
  "historique" = colonnes("nb_feux_dep_jour_avant", "dernier_feu_10ha_censure"))
PREDICTEURS <- unlist(FAMILLES, use.names = FALSE)
FAMILLE <- rep(names(FAMILLES), lengths(FAMILLES))   # famille de chaque prédicteur (même ordre)

# « Mémoire des lieux » : ce qui identifie l'endroit ou résume son passé d'incendies, sans rien dire du
# mécanisme (17 variables : département, longitude, latitude et 14 variables d'historique)
MEMOIRE <- c("dep", "lon", "lat", grep(paste0("^(nb_feux_commune_(1an|5ans|10ans)|surface_brulee_commune|",
                                              "pct_surface_brulee|taux_echappement|annees_depuis_dernier_feu|dernier_feu_10ha)"),
                                       PREDICTEURS, value = TRUE))
SANS_MEMOIRE <- setdiff(PREDICTEURS, MEMOIRE)

# Matrice numérique pour XGBoost : une indicatrice par modalité (NA conservés), booléens en 0/1
matrice <- function(d, vars) {
  do.call(cbind, lapply(vars, function(v) {
    x <- d[[v]]
    if (is.numeric(x) || is.logical(x)) return(matrix(as.numeric(x), dimnames = list(NULL, v)))
    x <- as.factor(x)
    matrix(sapply(levels(x), function(l) as.numeric(x == l)), nrow = length(x),
           dimnames = list(NULL, paste0(v, "=", levels(x))))
  }))
}

# Modèle -------------------------------------------------------------------------------------------------
PARAMS <- xgb.params(objective = "binary:logistic", eval_metric = "logloss", learning_rate = 0.05, max_depth = 5,
                     min_child_weight = 10, subsample = 0.8, colsample_bytree = 0.6, reg_lambda = 1,
                     nthread = max(1, parallel::detectCores() - 2))
BLOCS_ANNEES <- list(2006:2008, 2009:2011, 2012:2014, 2015:2016, 2017:2018)

# Nombre d'arbres : validation croisée sur des blocs d'années, arrêt après 50 arbres sans progrès
choisir_nrounds <- function(X, y, annees, blocs = BLOCS_ANNEES, params = PARAMS) {
  set.seed(1)
  cv <- xgb.cv(params = params, data = xgb.DMatrix(X, label = y), nrounds = 3000,
               folds = lapply(blocs, function(b) which(annees %in% b)), early_stopping_rounds = 50, verbose = 0)
  cv$early_stop$best_iteration
}
ajuster <- function(X, y, nrounds, graine = 1, params = PARAMS) {
  set.seed(graine)
  xgb.train(params = params, data = xgb.DMatrix(X, label = y), nrounds = nrounds, verbose = 0)
}
predire <- function(m, X) predict(m, xgb.DMatrix(X))

# Évaluation d'un jeu de variables, prédictions moyennées sur 3 graines :
#   test temporel : appris sur 2006-2018, prédit 2019-2024 ;
#   test spatial  : chaque région prédite par un modèle appris sur les 5 autres (toutes années), avec le nombre
#                   d'arbres du test temporel ; d_sp remplace les variables apprises par un modèle auxiliaire
#                   (calculs 1 et 3) par leur version « région jamais vue ».
# Résultat mis en cache dans donnees/modeles/, sous une empreinte des données et du protocole.
evaluer <- function(vars, d = feux, d_sp = d, graines = 1:3) {
  X <- matrice(d, vars)
  X_sp <- matrice(d_sp, vars)
  y <- d$y_1ha
  a <- d$apprentissage
  f <- sprintf("donnees/modeles/%s.rds", tools::md5sum(bytes = serialize(list(X, X_sp, y, a, d$region_test, graines,
                                                                              PARAMS, BLOCS_ANNEES), NULL)))
  if (file.exists(f)) return(readRDS(f))
  nr <- choisir_nrounds(X[a, ], y[a], d$annee[a])
  modeles <- lapply(graines, function(g) ajuster(X[a, ], y[a], nr, g))
  p_spatial <- numeric(nrow(d))
  for (r in unique(d$region_test)) {
    t <- d$region_test == r
    p_spatial[t] <- rowMeans(sapply(graines, function(g) predire(ajuster(X_sp[!t, ], y[!t], nr, g), X_sp[t, ])))
  }
  res <- list(nrounds = nr, p_temporel = rowMeans(sapply(modeles, function(m) predire(m, X[!a, ]))), p_spatial = p_spatial,
              modele = xgb.save.raw(modeles[[1]]))
  saveRDS(res, f)
  res
}

# Mesures ------------------------------------------------------------------------------------------------
auc <- function(p, y) {
  r <- rank(p)
  n1 <- sum(y == 1)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * sum(y == 0))
}
# Écart d'AUC entre deux modèles (B - A) et intervalle à 95 % par bootstrap apparié sur les feux
gain_auc <- function(pA, pB, y, B = 1000) {
  set.seed(1)
  g <- replicate(B, {
    i <- sample.int(length(y), replace = TRUE)
    auc(pB[i], y[i]) - auc(pA[i], y[i])
  })
  data.table(gain = auc(pB, y) - auc(pA, y), bas = quantile(g, 0.025, names = FALSE),
             haut = quantile(g, 0.975, names = FALSE))
}
# Intervalle de Wilson à 95 % d'une proportion k / n
wilson <- function(k, n, z = 1.96) {
  p <- k / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  demi <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  list(centre - demi, centre + demi)
}
