"""Chemins, départements et communes, communs aux calculs 1 et 2.

Données brutes : donnees/brutes/ (ou le dossier donné par la variable d'environnement FEUX_BRUTES),
remplies par calculs/telecharger.py. Feux : donnees/final/feux_med_2006_2024.parquet (jeu Kaggle).
Sorties : donnees/calculs/, fichiers intermédiaires dans donnees/calculs/intermediaires/.
"""
import os
from pathlib import Path
import numpy as np
import geopandas as gpd
import rasterio
from rasterio.transform import from_origin
from rasterio.windows import Window

RACINE = Path(__file__).resolve().parents[1]
BRUTES = Path(os.environ.get("FEUX_BRUTES", RACINE / "donnees/brutes"))
SORTIES = RACINE / "donnees/calculs"
INTER = SORTIES / "intermediaires"
INTER.mkdir(parents=True, exist_ok=True)
FEUX = RACINE / "donnees/final/feux_med_2006_2024.parquet"

DEPS = ["04", "05", "06", "07", "11", "13", "2A", "2B", "26", "30", "34", "48", "66", "83", "84"]
# régions du test « région jamais vue » des analyses
REGIONS = {"Corse": ["2A", "2B"], "Provence": ["13", "84", "04", "05"], "Var-Alpes-Maritimes": ["83", "06"],
           "Languedoc": ["34", "30", "48"], "Roussillon-Aude": ["66", "11"], "Ardèche-Drôme": ["07", "26"]}


def communes():
    """Communes des 15 départements (COG 2024, contours Etalab simplifiés à 50 m) en Lambert-93, sans
    les arrondissements de Marseille (qui doublent 13055) ; icom = rang dans l'ordre des codes INSEE."""
    com = gpd.read_file("/vsigzip/" + str(BRUTES / "communes/communes-50m-2024.geojson.gz"))
    com = com.rename(columns={"code": "code_insee", "nom": "nom_commune", "departement": "dep"})
    com = com[com["dep"].isin(DEPS) & ~com["code_insee"].str.match(r"^132\d\d$")].to_crs(2154)
    com = com.sort_values("code_insee").reset_index(drop=True)
    com["surface_km2"] = com.area / 1e6
    point = com.representative_point()
    com["x_l93"], com["y_l93"] = point.x, point.y
    com["icom"] = np.arange(len(com))
    return com[["code_insee", "nom_commune", "dep", "icom", "surface_km2", "x_l93", "y_l93", "geometry"]]


# Calcul 1 : classes des cellules de 25 m dans le modèle d'intensité λ. Végétation (BD Forêt harmonisée) :
# 0 hors BD Forêt, 1 pins méditerranéens, 2 autres conifères, 3 chênes sempervirents, 4 autres feuillus,
# 5 mélange fermé, 6 forêt ouverte, 7 landes, 8 formations herbacées, 255 inconnue (hors des 15 départements)
VARIABLES = ["route", "bati", "sentier", "piste", "pente", "veg"]
TAILLES = [7, 7, 7, 7, 6, 9]
BORNES_DIST = np.array([25, 50, 100, 200, 500, 1000], dtype="float32")  # m, bornes supérieures incluses
BORNES_PENTE = np.array([5, 10, 15, 20, 30], dtype="float32")         # degrés, bornes supérieures exclues
COMBUSTIBLE = np.arange(1, 8)                                          # forêt et landes, sans les herbacées
NCOMBO = int(np.prod(TAILLES))


def code_combinaison(classes):
    """Entier unique pour une combinaison de classes (route, bâti, sentier, piste, pente, végétation)."""
    code = np.zeros(classes[0].shape, dtype="int32")
    for c, t in zip(reversed(classes), reversed(TAILLES)):
        code = code * t + c
    return code


def decoder_combinaison(code):
    classes = []
    for t in TAILLES:
        classes.append(code % t)
        code = code // t
    return classes


# Calcul 1 : grilles Lambert-93 de 25 m calées sur des multiples de 50 m (grille du temps de marche) ;
# deux mosaïques (continent, Corse) pour la végétation et les voies, une emprise par département
RES, MARGE = 25.0, 5000.0
CELL_HA = RES * RES / 1e4
OSM = INTER / "calcul1_osm"
RASTERS = INTER / "calcul1_rasters"
AGREGATS = INTER / "calcul1_agregats"


def emprise(bornes, marge=MARGE, pas=50.0):
    """Emprise (x0, y0, x1, y1) élargie de la marge et calée sur des multiples de `pas`."""
    x0, y0, x1, y1 = bornes
    return (np.floor((x0 - marge) / pas) * pas, np.floor((y0 - marge) / pas) * pas,
            np.ceil((x1 + marge) / pas) * pas, np.ceil((y1 + marge) / pas) * pas)


def zone(dep):
    return "corse" if dep in ("2A", "2B") else "continent"


def emprise_zone(com, z):
    return emprise(com.loc[com["dep"].map(zone) == z].total_bounds, marge=MARGE + 1000)


def grille(emp, res=RES):
    """Transformation affine, nombre de lignes et de colonnes d'une emprise."""
    x0, y0, x1, y1 = emp
    return from_origin(x0, y1, res, res), int(round((y1 - y0) / res)), int(round((x1 - x0) / res))


def fenetre(grande, petite):
    """Fenêtre d'une emprise incluse dans une autre (même calage)."""
    return Window(int(round((petite[0] - grande[0]) / RES)), int(round((grande[3] - petite[3]) / RES)),
                  int(round((petite[2] - petite[0]) / RES)), int(round((petite[3] - petite[1]) / RES)))


def ecrire_tif(chemin, a, transform):
    chemin.parent.mkdir(parents=True, exist_ok=True)
    with rasterio.open(chemin, "w", driver="GTiff", height=a.shape[0], width=a.shape[1], count=1, dtype=a.dtype,
                       crs="EPSG:2154", transform=transform, compress="deflate", tiled=True, BIGTIFF="IF_SAFER") as f:
        f.write(a, 1)


def lire_tif(chemin, fen=None):
    with rasterio.open(chemin) as f:
        return f.read(1, window=fen)
