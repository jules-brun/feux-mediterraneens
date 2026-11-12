"""Calcul 1 (lieu du départ), étape 2 : couches à 25 m par département, agrégées par commune.

Distances à la route, au bâti, au sentier et à la piste OSM, pente (Copernicus DEM), temps de marche des pompiers,
végétation et tache (étape 1). Les cellules de chaque commune sont regroupées par combinaison de classes : nombre
de cellules et sommes utiles à l'étape 4. Entrées : étape 1, brutes/copdem.
Sorties : intermediaires/calcul1_agregats/agg_<dep>_<v1|v2>.parquet (BD Forêt v1 et v2).
"""
from concurrent.futures import ProcessPoolExecutor
import warnings
import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import rasterio
import shapely
from rasterio.features import rasterize
from rasterio.merge import merge
from rasterio.warp import reproject, Resampling, transform_bounds
from scipy import ndimage
from scipy.sparse import csr_matrix
from scipy.sparse.csgraph import dijkstra
from commun import (BRUTES, DEPS, VARIABLES, RES, COMBUSTIBLE, BORNES_DIST, BORNES_PENTE, OSM, RASTERS, AGREGATS,
                    communes, emprise, emprise_zone, zone, grille, fenetre, lire_tif, code_combinaison)

MARCHE_MAX = 240.0   # min, plafond du temps de marche (cellules inaccessibles comprises)
CLE = 1 << 17        # > nombre de combinaisons de classes
warnings.filterwarnings("ignore", message="Mean of empty slice")    # blocs 2 x 2 sans altitude (mer)


def decale(a, dr, dc, remplissage):
    """b[r, c] = a[r + dr, c + dc], `remplissage` hors de la grille."""
    H, W = a.shape
    b = np.full_like(a, remplissage)
    h0, h1, w0, w1 = max(0, dr), max(0, -dr), max(0, dc), max(0, -dc)
    b[h1:H - h0, w1:W - w0] = a[h0:H - h1, w0:W - w1]
    return b


def temps_marche(z, source, sentier, franchissable, res=50.0):
    """Temps de marche (min) depuis les cellules `source` : plus court chemin sur le graphe des 8 voisins. Loi
    de Tobler sur la pente signée de chaque arc, de la route vers le feu : 6 exp(-3,5 |dz/dx + 0,05|) km/h,
    x 0,6 hors sentier (x 1 entre deux cellules de sentier ou de voie). Grille de 50 m : à 25 m, le graphe
    d'un département dépasserait 3 Go."""
    H, W = z.shape
    N = H * W
    ids = np.arange(N, dtype=np.int32).reshape(H, W)
    zf = np.where(franchissable, z, np.nan).astype("float32")
    voisins, temps = np.empty((N, 8), dtype=np.int32), np.empty((N, 8))
    for k, (dr, dc) in enumerate([(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]):
        d = res * (np.sqrt(2.0) if dr and dc else 1.0)
        vitesse = 6000.0 / 60.0 * np.exp(-3.5 * np.abs((decale(zf, dr, dc, np.nan) - zf) / d + 0.05))   # m/min
        vitesse *= np.where(sentier & decale(sentier, dr, dc, False), 1.0, 0.6)
        t = d / vitesse
        ok = np.isfinite(t)               # NaN : cellule infranchissable ou hors grille -> boucle sur soi
        voisins[:, k] = np.where(ok, decale(ids, dr, dc, 0), ids).ravel()
        temps[:, k] = np.where(ok, t, 1e9).ravel()
    graphe = csr_matrix((temps.ravel(), voisins.ravel(), np.arange(0, 8 * N + 1, 8, dtype=np.int32)), shape=(N, N))
    del voisins, temps
    t = dijkstra(graphe, indices=np.flatnonzero((source & franchissable).ravel()), min_only=True, limit=MARCHE_MAX)
    return np.minimum(t, MARCHE_MAX).reshape(H, W).astype("float32")


def bloc(a, fonction):
    """Agrégation 2 x 2 (25 m -> 50 m)."""
    H, W = a.shape
    return fonction(a.reshape(H // 2, 2, W // 2, 2), axis=(1, 3))


def mnt(emp, transform, forme):
    """Copernicus DEM (modèle de surface) reprojeté en bilinéaire sur la grille."""
    lo0, la0, lo1, la1 = transform_bounds(2154, 4326, *emp)
    tuiles = [rasterio.open(t) for t in sorted((BRUTES / "copdem").glob("*.tif"))]
    tuiles = [t for t in tuiles if not (t.bounds.right < lo0 or t.bounds.left > lo1 or t.bounds.top < la0
                                        or t.bounds.bottom > la1)]
    mos, tr = merge(tuiles, bounds=(lo0 - 0.01, la0 - 0.01, lo1 + 0.01, la1 + 0.01), nodata=np.nan, dtype="float32")
    z = np.full(forme, np.nan, dtype="float32")
    reproject(mos[0], z, src_transform=tr, src_crs="EPSG:4326", src_nodata=np.nan, dst_transform=transform,
              dst_crs="EPSG:2154", dst_nodata=np.nan, resampling=Resampling.bilinear)
    return z


def pente_horn(z):
    p = np.pad(z, 1, constant_values=np.nan)
    a, b, c, d = p[:-2, :-2], p[:-2, 1:-1], p[:-2, 2:], p[1:-1, :-2]
    f, g, h, i = p[1:-1, 2:], p[2:, :-2], p[2:, 1:-1], p[2:, 2:]
    dzdx = ((c + 2 * f + i) - (a + 2 * d + g)) / (8 * RES)
    dzdy = ((a + 2 * b + c) - (g + 2 * h + i)) / (8 * RES)
    return np.degrees(np.arctan(np.hypot(dzdx, dzdy))).astype("float32")


def masque_bati(emp, tr, H, W):
    """Bâtiments : cellules de leur boîte englobante si elle tient dans 3 x 3 cellules (presque tous), sinon
    rastérisés (toute cellule touchée)."""
    bati = np.zeros((H, W), dtype="uint8")
    for f in sorted(OSM.glob("*_bati.parquet")):
        for lot in pq.ParquetFile(f).iter_batches(batch_size=500_000):
            g = shapely.from_wkb(lot.column("geometry").to_numpy(zero_copy_only=False))
            b = shapely.bounds(g)
            dedans = (b[:, 2] >= emp[0]) & (b[:, 0] <= emp[2]) & (b[:, 3] >= emp[1]) & (b[:, 1] <= emp[3])
            g, b = g[dedans], b[dedans]
            c0, c1 = np.floor((b[:, 0] - tr.c) / RES).astype(int), np.floor((b[:, 2] - tr.c) / RES).astype(int)
            r0, r1 = np.floor((tr.f - b[:, 3]) / RES).astype(int), np.floor((tr.f - b[:, 1]) / RES).astype(int)
            petit = (c1 - c0 <= 2) & (r1 - r0 <= 2)
            for dr in range(3):
                for dc in range(3):
                    s = petit & (r0 + dr <= r1) & (c0 + dc <= c1)
                    rr, cc = r0[s] + dr, c0[s] + dc
                    ok = (rr >= 0) & (rr < H) & (cc >= 0) & (cc < W)
                    bati[rr[ok], cc[ok]] = 1
            if (~petit).any():
                rasterize(((x, 1) for x in g[~petit]), out=bati, transform=tr, all_touched=True)
    return bati > 0


def distance(masque):
    """Distance euclidienne (m) à la plus proche cellule marquée, plafonnée à 65 535."""
    if not masque.any():
        return np.full(masque.shape, 65535, dtype="uint16")
    return np.minimum(np.round(ndimage.distance_transform_edt(~masque, sampling=RES)), 65535).astype("uint16")


def traiter(dep):
    com = communes()
    emp = emprise(com.loc[com["dep"] == dep].total_bounds)
    tr, H, W = grille(emp)
    sous = com[com.intersects(shapely.box(*emp))]
    icom = rasterize(zip(sous.geometry, sous["icom"].tolist()), out_shape=(H, W), transform=tr, fill=-1,
                     dtype="int32")                             # rang de la commune, -1 hors des 15 départements
    fen = fenetre(emprise_zone(com, zone(dep)), emp)
    voies = lire_tif(RASTERS / f"voies_{zone(dep)}.tif", fen)
    route, piste, sentier = (voies & 1) > 0, (voies & 2) > 0, (voies & 4) > 0

    z = mnt(emp, tr, (H, W))
    pente = pente_horn(z)
    z50 = bloc(z, np.nanmean) if np.isnan(z).any() else bloc(z, np.mean)
    del z
    # temps de marche depuis les routes et pistes (où passent les camions-citernes) ; la mer (altitude 0
    # hors des communes) et les cellules sans altitude sont infranchissables
    carrossable50 = bloc(route | piste, np.any)
    franchissable50 = np.isfinite(z50) & ~(bloc(icom < 0, np.all) & (z50 <= 0.0))
    marche50 = temps_marche(z50, carrossable50, bloc(sentier, np.any) | carrossable50, franchissable50)
    marche = np.repeat(np.repeat(marche50, 2, axis=0), 2, axis=1)

    dist = {"route": distance(route), "bati": distance(masque_bati(emp, tr, H, W)),
            "sentier": distance(sentier), "piste": distance(piste)}

    # agrégats sur les cellules des communes du département
    propre = icom >= 0
    propre[propre] = com["dep"].to_numpy()[icom[propre]] == dep
    ic, p, m = icom[propre].astype("int64"), pente[propre], marche[propre]
    assert np.isfinite(p).all(), "pente manquante dans une commune"
    classes = [np.searchsorted(BORNES_DIST, dist[k][propre], side="left").astype("uint8")
               for k in VARIABLES[:4]]
    classes.append(np.searchsorted(BORNES_PENTE, p, side="right").astype("uint8"))
    for v in ["v1", "v2"]:
        veg = lire_tif(RASTERS / f"veg_{v}_{zone(dep)}.tif", fen)[propre]
        veg[veg == 255] = 0
        tache = lire_tif(RASTERS / f"tache_{v}_{zone(dep)}.tif", fen)[propre]
        comb = np.isin(veg, COMBUSTIBLE)
        u, inv = np.unique(ic * CLE + code_combinaison(classes + [veg]), return_inverse=True)
        somme = lambda w: np.bincount(inv, weights=w)
        pd.DataFrame({"icom": (u // CLE).astype("int32"), "combo": (u % CLE).astype("int32"),
                      "n": np.bincount(inv).astype("int32"), "s_pente": somme(p), "s_marche": somme(m),
                      "s_marche15": somme(m > 15.0),
                      "s_log10_tache": somme(np.where(comb, np.log10(np.where(comb, tache, 1.0)), 0.0)),
                      "s_tache100": somme(comb & (tache > 100.0))}
                     ).to_parquet(AGREGATS / f"agg_{dep}_{v}.parquet", index=False)
    return dep


if __name__ == "__main__":
    AGREGATS.mkdir(parents=True, exist_ok=True)
    with ProcessPoolExecutor(3) as ex:
        for dep in ex.map(traiter, DEPS):
            print(dep, flush=True)
