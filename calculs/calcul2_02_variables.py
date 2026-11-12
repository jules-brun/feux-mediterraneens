"""Calcul 2 (brûlures récentes), étape 2 : végétation brûlée avant chaque feu, dans sa commune (a2_) et dans le
disque de 5 km (a2_r5km_) : parts brûlées 0-1 an, 1-5 ans et 5-10 ans avant l'alerte (sommes des parts
journalières), années depuis le dernier épisode de brûlure couvrant au moins 1 % de la végétation naturelle
(sans épisode depuis le 1/11/2000, début de MCD64A1 : durée depuis cette date et a2_brulure_1pct_censuree vrai).
Entrées : étape 1, feux. Sortie : calcul2_brulures.parquet (NA pour les communes sans végétation naturelle).
"""
import numpy as np
import pandas as pd
from commun import INTER, SORTIES, FEUX

# Garde de 10 jours : MCD64A1 date les pixels brûlés à quelques jours près, et environ 5 % des pixels d'un grand
# feu sont datés avant son alerte (jusqu'à J-8). On n'utilise que les brûlures datées strictement avant
# C = J - 10 jours (J : jour civil de l'alerte) : la garde ne dépend que de la date, jamais de l'issue du feu.
GARDE_J, SEUIL, ECART_EPISODE_J = 10, 0.01, 10      # épisode : jours de brûlure séparés de 10 jours au plus
EPOQUE = pd.Timestamp("2000-01-01")
DEBUT = (pd.Timestamp("2000-11-01") - EPOQUE).days      # début de MCD64A1, en jours depuis EPOQUE

feux = pd.read_parquet(FEUX, columns=["id_feu", "code_insee", "date_alerte"])
J = feux["date_alerte"].dt.normalize()
jours = lambda t: (t - EPOQUE).dt.days.to_numpy()
bornes = np.column_stack([jours(J), jours(J - pd.Timedelta(days=GARDE_J)), jours(J - pd.DateOffset(years=1)),
                          jours(J - pd.DateOffset(years=5)), jours(J - pd.DateOffset(years=10))])
incomplete = bornes[:, 4] < DEBUT
brulures = pd.read_parquet(INTER / "calcul2_brulures_jour.parquet")
surfaces = pd.read_parquet(INTER / "calcul2_surfaces_vegetation.parquet").set_index("code_insee")


def variables(support):
    br = brulures[brulures["support"] == support].sort_values(["code_insee", "date_brulure"])
    historique = {c: (jours(g["date_brulure"]), g["part_veg_brulee"].fillna(0.0).to_numpy())
                  for c, g in br.groupby("code_insee")}
    sans_veg = set(surfaces.index[surfaces[f"surface_veg_{support}_ha"] <= 0])
    res = np.full((len(feux), 5), np.nan)
    for i, code in enumerate(feux["code_insee"]):
        if pd.isna(code) or code in sans_veg:
            continue
        j, c, an1, an5, an10 = bornes[i]
        d, s = historique.get(code, (np.zeros(0), np.zeros(0)))
        d, s = d[d < c], s[d < c]                            # garde de 10 jours
        part = lambda debut, fin: s[(d >= debut) & (d < fin)].sum()
        episodes = np.split(np.arange(len(d)), np.flatnonzero(np.diff(d) > ECART_EPISODE_J) + 1)
        fins = [d[e[-1]] for e in episodes if len(e) and s[e].sum() >= SEUIL]
        res[i] = [part(an1, c), part(an5, an1), part(an10, an5), (j - (fins[-1] if fins else DEBUT)) / 365.25, not fins]
    res[incomplete, 2] = np.nan            # fenêtre 5-10 ans commencée avant MCD64A1
    prefixe = "a2_" if support == "commune" else "a2_r5km_"
    censuree = pd.array(np.where(np.isnan(res[:, 4]), None, res[:, 4] == 1), dtype="boolean")
    return pd.DataFrame({
        f"{prefixe}part_veg_brulee_0_1an": res[:, 0],
        f"{prefixe}part_veg_brulee_1_5ans": res[:, 1],
        f"{prefixe}part_veg_brulee_5_10ans": res[:, 2],
        f"{prefixe}annees_depuis_brulure_1pct": res[:, 3],
        f"{prefixe}brulure_1pct_censuree": censuree})


out = pd.concat([feux[["id_feu"]], variables("commune"), variables("disque5km")], axis=1)
out.insert(4, "a2_fenetre_5_10ans_incomplete", incomplete)
out["a2_surface_veg_commune_ha"] = feux["code_insee"].map(surfaces["surface_veg_commune_ha"])
out["a2_surface_veg_r5km_ha"] = feux["code_insee"].map(surfaces["surface_veg_disque5km_ha"])
out = out.astype({c: "float32" for c in out.columns if out[c].dtype == "float64"})
out.to_parquet(SORTIES / "calcul2_brulures.parquet", index=False)
print(len(out), "feux ;", out.shape[1] - 1, "variables ; part > 0 dans l'année :",
      f"{(out['a2_part_veg_brulee_0_1an'] > 0).mean():.2%}")
