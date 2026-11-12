"""Téléchargement des données brutes des calculs 1 et 2 dans donnees/brutes/ (ou $FEUX_BRUTES).

Sources publiques, accès anonyme ; un fichier déjà présent n'est pas retéléchargé :
- contours des communes 2024 simplifiés à 50 m (Etalab) ;
- extraits OpenStreetMap de Geofabrik (4 régions) ;
- BD Forêt v1 et v2 de l'IGN (Géoplateforme), édition la plus récente de chaque département ;
- Copernicus DEM GLO-30, MODIS MCD64A1 (2000-11 à 2024-12) et ESA CCI Land Cover 2000 (Planetary Computer) ;
  deux mois de MCD64A1 absents de Planetary Computer viennent du serveur SFTP public de l'université du Maryland.
"""
from concurrent.futures import ThreadPoolExecutor
import hashlib
import os
import re
import numpy as np
import pandas as pd
import geopandas as gpd
import planetary_computer
import pystac_client
import py7zr
import rasterio
import requests
import shapely
from rasterio.merge import merge
from rasterio.warp import transform_bounds
from rasterio.windows import Window, from_bounds
from commun import BRUTES, DEPS, communes

STAC = "https://planetarycomputer.microsoft.com/api/stac/v1"
ACTIFS = ["Burn_Date", "Burn_Date_Uncertainty", "QA", "First_Day", "Last_Day"]   # bandes de MCD64A1
os.environ.setdefault("GDAL_HTTP_MAX_RETRY", "5")
os.environ.setdefault("GDAL_HTTP_RETRY_DELAY", "2")


def telecharger(url, dest):
    if not dest.exists():
        dest.parent.mkdir(parents=True, exist_ok=True)
        with requests.get(url, stream=True, timeout=600) as r:
            r.raise_for_status()
            with open(dest.with_name(dest.name + ".part"), "wb") as f:
                for bloc in r.iter_content(1 << 20):
                    f.write(bloc)
        dest.with_name(dest.name + ".part").rename(dest)
    return dest


def bdforet(version, dossier):
    """BD Forêt (archives .7z par département, somme MD5 du flux Atom vérifiée), décompressée."""
    if (dossier / "editions.csv").exists():
        return
    api = "https://data.geopf.fr/telechargement/resource/BDFORET"
    titres, page = [], 1
    while True:
        xml = requests.get(api, params={"page": page}, timeout=60).text
        titres += re.findall(rf"<title>(BDFORET_{version}__SHP_LAMB93_D0\w\w_[\d-]+)</title>", xml)
        if f'gpf_dl:pagecount="{page}"' in xml:
            break
        page += 1
    editions = []
    for dep in DEPS:
        nom = sorted(t for t in titres if f"_D0{dep}_" in t)[-1]
        xml = requests.get(f"{api}/{nom}", timeout=60).text
        archive = telecharger(re.search(r'<link href="([^"]+\.7z)"', xml).group(1), dossier / f"{nom}.7z")
        md5 = re.search(r"<content>([0-9a-f]{32})</content>", xml).group(1)
        assert hashlib.md5(archive.read_bytes()).hexdigest() == md5, f"archive corrompue : {archive}"
        if not (dossier / nom).exists():
            with py7zr.SevenZipFile(archive) as z:
                z.extractall(dossier / f"{nom}.tmp")
            (dossier / f"{nom}.tmp").rename(dossier / nom)
        editions.append({"dep": dep, "edition": nom})
    pd.DataFrame(editions).to_csv(dossier / "editions.csv", index=False)


def mois_umd(m, fen):
    """Mois absent de Planetary Computer : fichier HDF4 du serveur SFTP public de l'université du Maryland
    (identifiants publiés dans le guide utilisateur MCD64A1 C6.1)."""
    import paramiko
    from pyhdf.SD import SD, SDC
    transport = paramiko.Transport(("fuoco.geog.umd.edu", 22))
    transport.connect(username="fire", password="burnt")
    sftp = paramiko.SFTPClient.from_transport(transport)
    distant = "data/MODIS/C61/MCD64A1/HDF/h18v04"
    debut = f"MCD64A1.A{m.year}{m.start_time.dayofyear:03d}."
    nom = sorted(n for n in sftp.listdir(distant) if n.startswith(debut))[-1]      # production la plus récente
    local = BRUTES / "mcd64a1/hdf" / nom
    local.parent.mkdir(parents=True, exist_ok=True)
    sftp.get(f"{distant}/{nom}", str(local))
    transport.close()
    hdf = SD(str(local), SDC.READ)
    bandes = [hdf.select(a.replace("_", " ")).get()[fen.toslices()] for a in ACTIFS]
    hdf.end()
    return bandes


def mcd64a1(com, catalogue):
    """MCD64A1 (tuile h18v04) sur l'emprise des communes + 25 km : une GeoTIFF à 5 bandes par mois."""
    fichier = lambda m: BRUTES / f"mcd64a1/mcd64a1_{m.strftime('%Y%m')}.tif"
    manquants = [m for m in pd.period_range("2000-11", "2024-12", freq="M") if not fichier(m).exists()]
    if not manquants:
        return
    items = catalogue.search(collections=["modis-64A1-061"], datetime="2000-11-01/2024-12-31T23:59:59Z",
                             query={"modis:horizontal-tile": {"eq": 18}, "modis:vertical-tile": {"eq": 4}}).items()
    par_mois = {it.properties["start_datetime"][:7]: it for it in sorted(items, key=lambda it: it.id)}
    with rasterio.open(next(iter(par_mois.values())).assets["Burn_Date"].href) as ds:
        crs, tr, H, W = ds.crs, ds.transform, ds.height, ds.width
    x0, y0, x1, y1 = com.to_crs(crs).total_bounds
    f = from_bounds(x0 - 25_000, y0 - 25_000, x1 + 25_000, y1 + 25_000, tr)
    c0, r0 = max(int(np.floor(f.col_off)), 0), max(int(np.floor(f.row_off)), 0)
    c1, r1 = min(int(np.ceil(f.col_off + f.width)), W), min(int(np.ceil(f.row_off + f.height)), H)
    fen = Window(c0, r0, c1 - c0, r1 - r0)

    def un_mois(m):
        if str(m) in par_mois:
            bandes = []
            for a in ACTIFS:
                with rasterio.open(par_mois[str(m)].assets[a].href) as ds:
                    bandes.append(ds.read(1, window=fen))
        else:
            bandes = mois_umd(m, fen)
        with rasterio.open(fichier(m), "w", driver="GTiff", width=fen.width, height=fen.height, count=5, dtype="int16",
                           crs=crs, transform=rasterio.windows.transform(fen, tr), compress="deflate") as dst:
            dst.write(np.stack(bandes).astype("int16"))

    fichier(manquants[0]).parent.mkdir(parents=True, exist_ok=True)
    with ThreadPoolExecutor(8) as ex:
        list(ex.map(un_mois, manquants))


def cci_lc_2000(catalogue):
    """ESA CCI Land Cover 2000 sur l'emprise de la fenêtre MCD64A1 + 0,1°."""
    dest = BRUTES / "esa_cci_lc/cci_lc_2000_fenetre.tif"
    if dest.exists():
        return
    with rasterio.open(next((BRUTES / "mcd64a1").glob("*.tif"))) as ds:
        lo0, la0, lo1, la1 = transform_bounds(ds.crs, 4326, *ds.bounds)
    bornes = (lo0 - 0.1, la0 - 0.1, lo1 + 0.1, la1 + 0.1)
    items = catalogue.search(collections=["esa-cci-lc"], bbox=list(bornes), datetime="2000-01-01/2000-12-31").items()
    sources = [rasterio.open(it.assets["lccs_class"].href) for it in items]
    mos, tr = merge(sources, bounds=bornes)
    dest.parent.mkdir(parents=True, exist_ok=True)
    with rasterio.open(dest, "w", driver="GTiff", width=mos.shape[2], height=mos.shape[1], count=1, dtype="uint8",
                       crs=sources[0].crs, transform=tr, nodata=0, compress="deflate") as dst:
        dst.write(mos[0].astype("uint8"), 1)


if __name__ == "__main__":
    telecharger("https://etalab-datasets.geo.data.gouv.fr/contours-administratifs/2024/geojson/communes-50m.geojson.gz",
                BRUTES / "communes/communes-50m-2024.geojson.gz")
    com = communes()
    for region in ["provence-alpes-cote-d-azur", "languedoc-roussillon", "rhone-alpes", "corse"]:
        if not list((BRUTES / "osm").glob(f"{region}-*.osm.pbf")):     # nom daté après redirection
            url = requests.head(f"https://download.geofabrik.de/europe/france/{region}-latest.osm.pbf",
                                allow_redirects=True, timeout=60).url
            telecharger(url, BRUTES / "osm" / url.rsplit("/", 1)[1])
    bdforet("2-0", BRUTES / "bdforet")
    bdforet("1-0", BRUTES / "bdforet_v1")
    catalogue = pystac_client.Client.open(STAC, modifier=planetary_computer.sign_inplace)
    zone = gpd.GeoSeries([com.buffer(3000).union_all()], crs=2154).to_crs(4326).iloc[0]   # communes + 3 km
    for it in catalogue.search(collections=["cop-dem-glo-30"], bbox=zone.bounds).items():
        if shapely.box(*it.bbox).intersects(zone):
            telecharger(it.assets["data"].href, BRUTES / f"copdem/{it.id}.tif")
    mcd64a1(com, catalogue)
    cci_lc_2000(catalogue)
