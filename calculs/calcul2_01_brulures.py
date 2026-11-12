"""Calcul 2 (brûlures récentes), étape 1 : végétation brûlée vue par MODIS MCD64A1, par commune et par jour.

Pour chaque commune et chaque disque de 5 km autour de son point représentatif : surface de végétation naturelle
(ESA CCI 2000) et part de cette végétation brûlée chaque jour (pixels brûlés retenus après contrôle qualité).
Entrées : brutes/mcd64a1/mcd64a1_<aaaamm>.tif, brutes/esa_cci_lc/cci_lc_2000_fenetre.tif, communes.
Sorties : intermediaires/calcul2_brulures_jour.parquet, intermediaires/calcul2_surfaces_vegetation.parquet.
"""
import calendar
import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio
from rasterio.warp import reproject, Resampling
from scipy import sparse
from exactextract import exact_extract
from commun import BRUTES, INTER, communes

PIX_HA = 463.31271653 ** 2 / 1e4      # surface d'un pixel MODIS (projection sinusoïdale, équivalente)
# végétation naturelle (classes LCCS) : forêts, arbustes, prairies ; mosaïques à dominante naturelle ou cultivée.
# Millésime 2000, antérieur à toutes les brûlures (un millésime plus récent classerait déjà en végétation
# clairsemée une partie des surfaces brûlées)
POIDS_VEG = {**dict.fromkeys([50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90, 100, 110, 120, 121, 122, 130], 1.0),
             40: 0.75, 30: 0.25}

fichiers = sorted((BRUTES / "mcd64a1").glob("mcd64a1_*.tif"))
assert len(fichiers) == 290, "mois MCD64A1 manquants (2000-11 à 2024-12) : lancer calculs/telecharger.py"
with rasterio.open(fichiers[0]) as ds:
    profil, crs, tr, (H, W) = ds.profile, ds.crs, ds.transform, ds.shape


def pixels_brules(fichier):
    """Pixels brûlés retenus d'un mois (guide utilisateur MCD64A1 C6.1) : terre (bit QA 0), données valides (bit 1),
    date de brûlure dans le mois du produit et dans la période de détection fiable [First Day ; Last Day]."""
    mois = pd.Period(fichier.stem[8:12] + "-" + fichier.stem[12:], freq="M")
    with rasterio.open(fichier) as ds:
        date, qa = ds.read(1), ds.read(3)
        premier, dernier = ds.read(4).astype("int32"), ds.read(5).astype("int32")
    # First Day et Last Day sont des jours de l'année : en janvier, First Day peut appartenir à l'année
    # précédente, en décembre Last Day à l'année suivante (on a alors First Day > Last Day)
    inverse = (premier > 0) & (dernier > 0) & (premier > dernier)
    if mois.month == 1:
        premier = np.where(inverse, premier - 365 - calendar.isleap(mois.year - 1), premier)
    if mois.month == 12:
        dernier = np.where(inverse, dernier + 365 + calendar.isleap(mois.year), dernier)
    retenu = ((qa & 3) == 3) & (date >= mois.start_time.dayofyear) & (date <= mois.end_time.dayofyear) \
        & (date >= premier) & (date <= dernier)
    r, c = np.nonzero(retenu)
    return pd.DataFrame({"pixel": r.astype("int64") * W + c,
                         "date_brulure": pd.Timestamp(mois.year, 1, 1) + pd.to_timedelta(date[r, c] - 1, unit="D")})


pix = pd.concat(map(pixels_brules, fichiers)).sort_values(["date_brulure", "pixel"], ignore_index=True)

# part de végétation naturelle sur la grille MODIS (moyenne des cellules CCI de 300 m)
with rasterio.open(BRUTES / "esa_cci_lc/cci_lc_2000_fenetre.tif") as ds:
    lc = ds.read(1)
    poids = np.zeros(256, dtype="float32")
    poids[list(POIDS_VEG)] = list(POIDS_VEG.values())
    veg300 = np.where(lc == 0, np.nan, poids[lc]).astype("float32")
    veg = np.full((H, W), np.nan, dtype="float32")
    reproject(veg300, veg, src_transform=ds.transform, src_crs=ds.crs, src_nodata=np.nan, dst_transform=tr,
              dst_crs=crs, dst_nodata=np.nan, resampling=Resampling.average)
fichier_veg = INTER / "calcul2_part_vegetation.tif"
with rasterio.open(fichier_veg, "w", **dict(profil, count=1, dtype="float32", nodata=None)) as dst:
    dst.write(veg, 1)
veg = np.nan_to_num(veg, nan=0.0).ravel().astype("float64")

com = communes()
disques = gpd.GeoDataFrame(geometry=gpd.points_from_xy(com["x_l93"], com["y_l93"]).buffer(5000, resolution=64),
                           crs=com.crs)
surfaces = pd.DataFrame({"code_insee": com["code_insee"]})
brulures = []
for support, zones in [("commune", com), ("disque5km", disques)]:
    # poids zone x pixel : fraction du pixel couverte par la zone
    ee = exact_extract(str(fichier_veg), zones.to_crs(crs), ["cell_id", "coverage"], output="pandas")
    ligne = np.repeat(np.arange(len(ee)), ee["cell_id"].map(len))
    poids = sparse.csr_matrix((np.concatenate(ee["coverage"]), (ligne, np.concatenate(ee["cell_id"]))),
                              shape=(len(com), H * W))
    surface_veg = (poids @ veg) * PIX_HA
    surfaces[f"surface_veg_{support}_ha"] = surface_veg
    M = poids[:, pix["pixel"].to_numpy()].tocoo()          # zones x pixels brûlés (un par mois)
    garde = M.data > 0
    z, e = M.row[garde], M.col[garde]
    b = pd.DataFrame({"support": support, "zone": z, "date_brulure": pix["date_brulure"].to_numpy()[e],
                      "brule_veg_ha": M.data[garde] * veg[pix["pixel"].to_numpy()[e]] * PIX_HA})
    b = b.groupby(["support", "zone", "date_brulure"], as_index=False)["brule_veg_ha"].sum()
    b["part_veg_brulee"] = b["brule_veg_ha"] / surface_veg[b["zone"]]
    b["code_insee"] = com["code_insee"].to_numpy()[b["zone"]]
    brulures.append(b[["support", "code_insee", "date_brulure", "part_veg_brulee"]])
pd.concat(brulures).to_parquet(INTER / "calcul2_brulures_jour.parquet", index=False)
surfaces.to_parquet(INTER / "calcul2_surfaces_vegetation.parquet", index=False)
print(len(pix), "pixels brûlés retenus ;", {s: len(b) for s, b in zip(["commune", "disque5km"], brulures)},
      "lignes zone x jour")
