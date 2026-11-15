# Échappement des feux de forêt méditerranéens

Projet de machine learning de M2, par Pierre Raffalli et Jules Brun.

> À conditions météorologiques comparables, la nature et l'état de la végétation permettent-ils de prédire qu'un départ de feu dépassera 1 ha ?

Le dépôt contient le code qui produit tous les chiffres et toutes les figures de notre présentation, à partir du jeu de données que nous avons construit et publié sur Kaggle.

## Ce que nous avons fait

1. **Le jeu de données.** 31 530 départs de feu de 2006 à 2024 dans 15 départements méditerranéens (base BDIFF), avec 174 variables connues au moment de l'alerte : météo, végétation, état de la végétation vu par satellite, relief, présence humaine, historique des feux. La cible vaut 1 si le feu dépasse 1 ha (22,7 % des départs).
2. **Prédire le futur.** Un modèle XGBoost appris sur 2006-2018 prédit 2019-2024 avec une AUC de 0,77. À danger météo comparable, la végétation est associée à l'échappement, mais l'historique des feux de la commune (la « mémoire des lieux ») la rend redondante.
3. **Prédire du sud vers le nord.** Appris sur le littoral et la Corse, sans aucun historique des feux, le modèle classe les départs de l'intérieur et de la montagne presque aussi bien qu'un modèle appris sur place (AUC 0,723 contre 0,728). Trois variables construites (lieu probable du départ, brûlures récentes vues par satellite, cause probable) ne remplacent pas la mémoire des lieux.
4. **Mesurer l'écart entre deux jeux de données.** Distances de Kolmogorov-Smirnov et de Wasserstein entre les distributions des variables, année par année et entre le nord et le sud, comparées à ce que donnerait le hasard (tirage de journées entières).

## Organisation

| Dossier | Contenu |
|---|---|
| `analyses/` | Code R : modèles, végétation, calcul 3 (cause probable), transfert, distances, valeurs de Shapley, classifieur discriminant entre zones, figures |
| `calculs/` | Code Python des calculs 1 (lieu probable du départ, processus ponctuel) et 2 (brûlures MODIS) |
| `historique/` | Feux BDIFF de 1997 à 2005 (date, commune, cause), utilisés par le calcul 3 |
| `resultats/` | Figures de la présentation et tableaux de chiffres, déjà calculés |
| `donnees/` | Données à télécharger, non versionnées (voir ci-dessous) |

## Données

Télécharger le jeu de données sur Kaggle, [French Mediterranean Wildfires 2006-2024](https://www.kaggle.com/datasets/pierridotite/french-mediterranean-wildfires-2006-2024), et placer le fichier `feux_med_2006_2024.parquet` dans `donnees/final/`. La description des 276 colonnes est sur la page Kaggle (`data_dictionary.csv`).

Les analyses R acceptent aussi la version `.csv` : placer `feux_med_2006_2024.csv` et `data_dictionary.csv` dans `donnees/final/`. Les types des colonnes (codes à zéros initiaux, modalités, dates) sont alors rétablis d'après le dictionnaire. Les calculs 1 et 2 (Python) demandent le `.parquet`.

Le code de construction du jeu de données (interpolation de la météo, croisement des cartes de végétation, moyennes de pixels satellite) n'est pas dans ce dépôt.

## Installation

R (testé avec R 4.6.1) :

```r
install.packages(c("data.table", "xgboost", "arrow", "ggplot2", "patchwork", "sf", "ggrepel", "systemfonts", "ragg"))
```

Python 3.12, seulement pour les calculs 1 et 2 :

```bash
python -m venv .venv
.venv/bin/pip install -r requirements.txt      # sous Windows : .venv\Scripts\pip
```

## Exécution

Toutes les commandes se lancent depuis la racine du dépôt.

**1. Calculs 1 et 2 (Python, facultatif).** Ils téléchargent environ 5 Go de données publiques (OpenStreetMap, BD Forêt de l'IGN, relief Copernicus, surfaces brûlées MODIS) puis prennent environ 30 minutes :

```bash
python -X utf8 calculs/telecharger.py
python -X utf8 calculs/calcul1_01_osm_vegetation.py
python -X utf8 calculs/calcul1_02_couches.py
python -X utf8 calculs/calcul1_03_lambda.py
python -X utf8 calculs/calcul1_04_variables.py
python -X utf8 calculs/calcul2_01_brulures.py
python -X utf8 calculs/calcul2_02_variables.py
```

**2. Analyses (R).** Environ une heure la première fois ; les modèles sont ensuite repris de `donnees/modeles/` :

```bash
Rscript analyses/lancer_tout.R
```

Les scripts peuvent aussi se lancer un par un, dans l'ordre : `01_modeles.R` (échelle des modèles, mémoire des lieux, valeurs de Shapley), `02_calcul3_cause.R`, `03_calculs.R` (apport des trois calculs), `04_vegetation.R`, `05_enregistrement.R`, `06_transfert.R` (année par année, sud vers nord), `07_distances.R`, `09_shap.R` (valeurs et interactions de Shapley, environ 11 min), `10_zones.R` (classifieur discriminant entre zones, environ 42 min), `11_profils_zones.R` (distances de chaque zone au reste), `12_transfert_zones.R` (séparabilité et transfert, repondérage sud vers nord), puis `08_figures.R` en dernier, qui lit les sorties de tous les autres. Les scripts 03 et 04 ont besoin des sorties des calculs 1 et 2.

## Reproductibilité

- **Arrondi des décimaux.** Les chiffres de la présentation ont été calculés sur les versions `.csv` et `.rds` du jeu, dont les décimaux ont 8 chiffres significatifs. `analyses/commun.R` applique le même arrondi au fichier `.parquet` pour retrouver exactement les mêmes arbres XGBoost.
- **Données qui évoluent.** OpenStreetMap et la BD Forêt sont mises à jour : en les retéléchargeant aujourd'hui, le calcul 1 peut différer très légèrement de nos résultats.
- **Journées entières.** Les journées tirées pour la référence du hasard sont des dates UTC de l'alerte.

## Sources et licences

Le jeu de données est distribué sous licence ODbL 1.0, parce qu'il contient des données dérivées d'OpenStreetMap. Il combine la BDIFF (ministère de l'Agriculture et IGN), Météo-France, l'IGN, Corine Land Cover, MODIS (NASA), ESA CCI Land Cover, Copernicus DEM, l'INSEE et OpenStreetMap : le détail est sur la page Kaggle. Le fichier `historique/feux_1997_2005.csv` est extrait de la BDIFF (bdiff.agriculture.gouv.fr, Licence Ouverte / Etalab 2.0).
