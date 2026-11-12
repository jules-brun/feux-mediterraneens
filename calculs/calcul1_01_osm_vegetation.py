"""Calcul 1 (lieu du départ), étape 1 : voies et bâtiments OpenStreetMap, végétation, taches de combustible.

Voies (routes publiques, pistes, sentiers) et bâtiments OSM en Lambert-93 ; sur deux mosaïques à 25 m (continent,
Corse) : voies, végétation BD Forêt v1 et v2 en 8 groupes harmonisés, surface de la tache de combustible.
Entrées : brutes/osm, brutes/bdforet, brutes/bdforet_v1, communes. Sorties : intermediaires/calcul1_osm/ et
intermediaires/calcul1_rasters/ (voies_<zone>, veg_<v1|v2>_<zone>, tache_<v1|v2>_<zone>).
"""
from concurrent.futures import ProcessPoolExecutor
import re
import unicodedata
import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import pyogrio
import shapely
from pyproj import Transformer
from rasterio.features import rasterize
from scipy import ndimage
from commun import (BRUTES, DEPS, COMBUSTIBLE, CELL_HA, OSM, RASTERS, communes, emprise, emprise_zone, zone,
                    grille, fenetre, ecrire_tif)

ROUTES = ["motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "living_street",
          "service", "road", "motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link"]
CLASSE = {**dict.fromkeys(ROUTES, "route"), "track": "piste",
          **dict.fromkeys(["path", "footway", "bridleway", "steps"], "sentier")}
BITS = {"route": 1, "piste": 2, "sentier": 4}
SCHEMA = pa.schema([("classe", pa.string()), ("geometry", pa.binary())])
# BD Forêt v2 : code TFV -> groupe harmonisé (forêt ouverte : FO1-3 et « sans couvert » FF0, FO0) ; les autres
# codes FF2* sont d'autres conifères, les autres FF1* d'autres feuillus (dont chênes décidus)
TFV = {"FF2-51-51": 1, "FF2-57-57": 1, "FF2-81-81": 1, "FF1G06-06": 3, "FF31": 5, "FF32": 5, "FO1": 6, "FO2": 6,
       "FO3": 6, "FF0": 6, "FO0": 6, "FP": 4, "LA4": 7, "LA6": 8}


def code_v2(tfv):
    return TFV[tfv] if tfv in TFV else {"FF2": 2, "FF1": 4}[tfv[:3]]


def code_v1(typn, libelle):
    """BD Forêt v1 : type de peuplement IFN (TYPN) et libellé départemental -> groupe harmonisé."""
    lib = "".join(c for c in unicodedata.normalize("NFD", libelle.lower().replace("’", "'"))
                  if unicodedata.category(c) != "Mn")
    typn = int(typn)
    if "espace vert urbain" in lib:
        return 0
    if typn == 2:
        return 1 if re.search(r"pin d'alep|pin maritime|pin pignon|pin parasol", lib) else 2
    if typn in (1, 4, 6, 50):
        return 3 if re.search(r"chene vert|chene-liege|chene liege", lib) else 4
    if typn in (3, 5):
        return 5
    if typn == 30:
        return 6
    if typn == 40:
        return 8 if re.search(r"pelouse|formation pastorale", lib) else 7
    raise ValueError((typn, libelle))


def extraire_osm(region, emprises):
    """Voies et bâtiments d'un extrait régional, lus en flux (paquets de 200 000 objets)."""
    pbf = sorted((BRUTES / "osm").glob(f"{region}-*.osm.pbf"))[-1]
    vers_l93 = Transformer.from_crs(4326, 2154, always_xy=True)
    couches = {"voies": dict(layer="lines", columns=["highway", "other_tags"],
                             where="highway IN (" + ",".join(f"'{h}'" for h in CLASSE) + ")"),
               "bati": dict(layer="multipolygons", columns=["building"],
                            where="building IS NOT NULL AND building <> 'no'")}
    for nom, options in couches.items():
        with pyogrio.open_arrow(pbf, batch_size=200_000, use_pyarrow=True, **options) as (_, lots), \
                pq.ParquetWriter(OSM / f"{region}_{nom}.parquet", SCHEMA) as ecrivain:
            for lot in lots:
                g = shapely.from_wkb(lot.column("wkb_geometry").to_numpy(zero_copy_only=False))
                if nom == "voies":
                    classe = np.array([CLASSE[h] for h in lot.column("highway").to_pylist()])
                    tags = lot.column("other_tags").to_pylist()
                    garde = np.array([t is None or '"tunnel"=>"yes"' not in t for t in tags])     # sans les tunnels
                else:
                    classe, garde = np.full(len(g), "bati"), np.ones(len(g), dtype=bool)
                garde &= ~shapely.is_missing(g)
                g, classe = g[garde], classe[garde]
                g = shapely.transform(g, lambda xy: np.column_stack(vers_l93.transform(xy[:, 0], xy[:, 1])))
                b = shapely.bounds(g)[:, None, :]
                garde = ((b[..., 2] >= emprises[:, 0]) & (b[..., 0] <= emprises[:, 2]) & (b[..., 3] >= emprises[:, 1])
                         & (b[..., 1] <= emprises[:, 3])).any(axis=1)          # recoupe un département + 5 km
                table = pa.table({"classe": classe[garde], "geometry": shapely.to_wkb(g[garde])}, schema=SCHEMA)
                ecrivain.write_table(table)


def raster_voies(emp):
    """Voies (toute cellule touchée) codées par bits : 1 route publique, 2 piste, 4 sentier."""
    tr, H, W = grille(emp)
    marques = {k: np.zeros((H, W), dtype="uint8") for k in BITS}
    for f in sorted(OSM.glob("*_voies.parquet")):
        for lot in pq.ParquetFile(f).iter_batches(batch_size=200_000):
            g = shapely.from_wkb(lot.column("geometry").to_numpy(zero_copy_only=False))
            b = shapely.bounds(g)
            dedans = (b[:, 2] >= emp[0]) & (b[:, 0] <= emp[2]) & (b[:, 3] >= emp[1]) & (b[:, 1] <= emp[3])
            classe = np.asarray(lot.column("classe"))
            for k in BITS:
                if (dedans & (classe == k)).any():
                    rasterize(((x, 1) for x in g[dedans & (classe == k)]), out=marques[k], transform=tr,
                              all_touched=True)
    return marques["route"] * np.uint8(1) | marques["piste"] * np.uint8(2) | marques["sentier"] * np.uint8(4)


def rasteriser_bdforet(version, dep, transform, forme):
    """Groupes harmonisés (0-8) de la BD Forêt d'un département, au centre des cellules."""
    dossier = BRUTES / ("bdforet" if version == "v2" else "bdforet_v1")
    edition = pd.read_csv(dossier / "editions.csv", dtype=str).set_index("dep")["edition"][dep]
    shp = next((dossier / edition).rglob("FORMATION_VEGETALE" + (".shp" if version == "v2" else "_*.shp")))
    if version == "v2":
        bd = pyogrio.read_dataframe(shp, columns=["CODE_TFV"])
        codes = [code_v2(c) for c in bd["CODE_TFV"]]
    else:   # le .cpg annonce UTF-8, mais les attributs sont en Windows-1252
        bd = pyogrio.read_dataframe(shp, columns=["TYPN", "LIBELLE"], encoding="cp1252")
        codes = [code_v1(t, l) for t, l in zip(bd["TYPN"], bd["LIBELLE"])]
    formes = [(g, c) for g, c in zip(bd.geometry.values, codes) if g is not None and c > 0]
    return rasterize(formes, out_shape=forme, transform=transform, dtype="uint8")


def mosaique(com, z, version):
    """Végétation de la zone ; 255 = inconnue (hors des 15 départements)."""
    emp = emprise_zone(com, z)
    _, H, W = grille(emp)
    mos = np.full((H, W), 255, dtype="uint8")
    for dep in [d for d in DEPS if zone(d) == z]:
        sous = com[com["dep"] == dep]
        e = emprise(sous.total_bounds, marge=2000.0)            # la BD Forêt déborde un peu du département
        tr_e, h, w = grille(e)
        dans = rasterize(((g, 1) for g in sous.geometry), out_shape=(h, w), transform=tr_e, dtype="uint8") > 0
        veg = rasteriser_bdforet(version, dep, tr_e, (h, w))
        sub = mos[fenetre(emp, e).toslices()]                   # vue sur la mosaïque
        sub[dans] = veg[dans]                                   # chaque cellule prend la BD Forêt de son département
        comble = ~dans & (sub == 255) & (veg > 0)               # interstices entre communes : BD Forêt qui les couvre
        sub[comble] = veg[comble]
    return mos


def taches(veg, voies):
    """Surface (ha) de la tache de combustible de chaque cellule (0 hors combustible). Sans découpage, le
    combustible percole (une tache couvrirait la moitié du continent) : on le coupe par les routes et pistes,
    à la fois coupures de combustible et lignes d'appui des pompiers. Une cellule de combustible traversée par
    une voie prend la plus grande tache à moins de 50 m (fenêtre 5 x 5), au moins une cellule."""
    comb = np.isin(veg, COMBUSTIBLE)
    route = (voies & 3) > 0
    coupe = comb & route
    etiq, n = ndimage.label(comb & ~route, structure=np.ones((3, 3), dtype=bool), output=np.int32)
    del comb, route
    surface = (np.bincount(etiq.ravel(), minlength=n + 1) * CELL_HA).astype("float32")
    surface[0] = 0.0
    tache = surface[etiq]
    del etiq
    voisin = ndimage.maximum_filter(tache, size=5, mode="constant", cval=0.0)
    tache[coupe] = np.maximum(voisin[coupe], CELL_HA)
    return tache


if __name__ == "__main__":
    com = communes()
    OSM.mkdir(parents=True, exist_ok=True)
    emprises = np.array([emprise(com.loc[com["dep"] == d].total_bounds) for d in DEPS])
    regions = ["provence-alpes-cote-d-azur", "languedoc-roussillon", "rhone-alpes", "corse"]
    with ProcessPoolExecutor(4) as ex:
        list(ex.map(extraire_osm, regions, [emprises] * 4))
    print("OSM extrait", flush=True)
    for z in ["corse", "continent"]:
        emp = emprise_zone(com, z)
        tr = grille(emp)[0]
        voies = raster_voies(emp)
        ecrire_tif(RASTERS / f"voies_{z}.tif", voies, tr)
        for version in ["v1", "v2"]:
            veg = mosaique(com, z, version)
            ecrire_tif(RASTERS / f"veg_{version}_{z}.tif", veg, tr)
            ecrire_tif(RASTERS / f"tache_{version}_{z}.tif", taches(veg, voies), tr)
            print(z, version, flush=True)
