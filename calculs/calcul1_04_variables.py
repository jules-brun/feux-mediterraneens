"""Calcul 1 (lieu du départ), étape 4 : variables par feu, moyennes sur sa commune pondérées par λ (_lambda) ou
uniformes (_uniforme). Une cellule x pèse λ(x), probabilité qu'un départ de la commune soit en x ; chaque feu prend
la version de la BD Forêt de son champ source_bdforet (β estimés avec la v1, appliqués aux classes v2).
Entrées : agrégats (étape 2), β (étape 3), feux. Sorties : calcul1_lieux.parquet ; calcul1_lieux_regions.parquet,
où les variables _lambda viennent du λ appris sans la région de la commune (test « région jamais vue »).
"""
import numpy as np
import pandas as pd
from commun import (DEPS, REGIONS, VARIABLES, COMBUSTIBLE, INTER, AGREGATS, SORTIES, FEUX, communes,
                    decoder_combinaison)

# parts de végétation au départ (les six premières somment à 1 ; pins_med et foret_ouverte sont des « dont »)
CLASSES = {"landes": [6, 7], "resineux": [1, 2], "feuillus": [3, 4], "melange": [5], "herbace": [8],
           "non_combustible": [0], "pins_med": [1], "foret_ouverte": [6]}

com = communes()
C = len(com)
coef = pd.read_csv(INTER / "calcul1_lambda.csv")
aggs = {v: pd.concat([pd.read_parquet(AGREGATS / f"agg_{d}_{v}.parquet") for d in DEPS],
                     ignore_index=True) for v in ["v1", "v2"]}


def variables(agg, region):
    """Variables par commune (ligne = icom) pour une version de la BD Forêt et le λ appris sans `region`."""
    beta = {v: g.sort_values("classe")["beta"].to_numpy()
            for v, g in coef[coef["region"] == region].groupby("variable")}
    classes = decoder_combinaison(agg["combo"].to_numpy())
    veg, n = classes[5], agg["n"].to_numpy()
    lam = np.exp(sum(beta[v][c] for v, c in zip(VARIABLES, classes)))
    somme = lambda w: np.bincount(agg["icom"], weights=w, minlength=C)
    out = {}
    for suffixe, poids in [("lambda", lam), ("uniforme", np.ones(len(agg)))]:
        tot = somme(poids * n)
        for nom, cl in CLASSES.items():
            out[f"a1_part_{nom}_{suffixe}"] = somme(poids * n * np.isin(veg, cl)) / tot
        # surface de la tache de combustible (découpée par les voies carrossables), parmi les départs
        # situés dans le combustible : NA pour une commune sans combustible
        comb = somme(poids * n * np.isin(veg, COMBUSTIBLE))
        with np.errstate(invalid="ignore", divide="ignore"):
            out[f"a1_log10_tache_ha_{suffixe}"] = somme(poids * agg["s_log10_tache"]) / comb
            out[f"a1_p_tache_100ha_{suffixe}"] = somme(poids * agg["s_tache100"]) / comb
        out[f"a1_marche_min_{suffixe}"] = somme(poids * agg["s_marche"]) / tot      # temps de marche des pompiers
        out[f"a1_p_marche_15min_{suffixe}"] = somme(poids * agg["s_marche15"]) / tot
        out[f"a1_pente_deg_{suffixe}"] = somme(poids * agg["s_pente"]) / tot
        if suffixe == "lambda":   # λ moyen de la commune rapporté à celui de toute la zone
            out["a1_intensite_departs"] = (tot / somme(n)) / (tot.sum() / n.sum())
    return pd.DataFrame(out)


def table(region):
    """Variables par (commune, version de la BD Forêt)."""
    t = [variables(agg, region).assign(code_insee=com["code_insee"].to_numpy(), source_bdforet=v)
         for v, agg in aggs.items()]
    return pd.concat(t, ignore_index=True)


base = table("toutes")
a1 = [c for c in base.columns if c.startswith("a1_")]
cles = ["code_insee", "source_bdforet"]
pondere = [c for c in a1 if not c.endswith("_uniforme")]
morceaux = []
for region, deps_r in REGIONS.items():
    t = table(region)
    morceaux.append(t[t["code_insee"].isin(com.loc[com["dep"].isin(deps_r), "code_insee"])][cles + pondere])
regions = pd.concat(morceaux).merge(base.drop(columns=pondere), on=cles)
assert len(regions) == len(base)

feux = pd.read_parquet(FEUX, columns=["id_feu", "code_insee", "source_bdforet"])
for t, nom in [(base, "calcul1_lieux"), (regions, "calcul1_lieux_regions")]:
    f = feux.merge(t, on=cles, how="left")
    assert len(f) == len(feux)
    f[["id_feu"] + a1].to_parquet(SORTIES / f"{nom}.parquet", index=False)
    print(f"{nom} : {len(f)} feux, {len(a1)} variables, {f[a1[0]].isna().sum()} feux sans valeur")
