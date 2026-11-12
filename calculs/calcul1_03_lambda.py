"""Calcul 1 (lieu du départ), étape 3 : intensité de départ des feux λ(x), estimée sur les comptes par commune.

Vraisemblance pénalisée (κ par validation croisée spatiale sur 2006-2018), puis validation sur 2019-2024.
Entrées : agrégats v1 (étape 2), communes, feux. Sorties : intermediaires/calcul1_lambda.csv (β du modèle
complet et des modèles appris sans chaque région), calcul1_validation.csv (part de déviance expliquée).
"""
import numpy as np
import pandas as pd
from scipy.linalg import block_diag
from scipy.optimize import minimize, minimize_scalar, root
from scipy.sparse import csr_matrix
from scipy.special import xlogy
from commun import (DEPS, REGIONS, VARIABLES, TAILLES, NCOMBO, INTER, AGREGATS, SORTIES, FEUX, communes,
                    decoder_combinaison)

REFERENCE = {"route": 6, "bati": 6, "sentier": 6, "piste": 6, "pente": 0, "veg": 0}  # > 1 km, 0-5°, hors BD Forêt
KAPPAS = [100.0, 30.0, 10.0, 3.0, 1.0, 0.3]   # du plus au moins lissé

com = communes()
C, D = len(com), len(DEPS)
dep = np.array([DEPS.index(d) for d in com["dep"]])
feux = pd.read_parquet(FEUX, columns=["code_insee", "annee"])
feux = feux[feux["code_insee"].isin(com["code_insee"])]
rang = pd.Series(com["icom"].to_numpy(), index=com["code_insee"])
N_app, N_test = [np.bincount(rang[feux.loc[feux["annee"].isin(a), "code_insee"]], minlength=C).astype(float)
                 for a in (range(2006, 2019), range(2019, 2025))]

# On ne voit pas les points de départ, seulement N_c, le nombre de feux 2006-2018 de la commune c. Les départs
# forment un processus de Poisson d'intensité exp(γ_dep) λ(x) sur les cellules de 25 m, avec
# log λ(x) = Σ_k β_k[classe_k(x)] (distances à la route, au bâti, au sentier, à la piste ; pente ; végétation v1).
# En agrégeant par commune : N_c ~ Poisson(exp(γ_dep) Σ_j S[c, j] λ_j), S[c, j] = nombre de cellules de c dans
# la combinaison de classes j.
agg = pd.concat([pd.read_parquet(AGREGATS / f"agg_{d}_v1.parquet", columns=["icom", "combo", "n"])
                 for d in DEPS])
S = csr_matrix((agg["n"].to_numpy(float), (agg["icom"], agg["combo"])), shape=(C, NCOMBO))

# X[j] : indicatrices des classes (hors référence) de la combinaison j
classes = decoder_combinaison(np.arange(NCOMBO))
libres = {v: [l for l in range(t) if l != REFERENCE[v]] for v, t in zip(VARIABLES, TAILLES)}
X = np.column_stack([classes[k] == l for k, v in enumerate(VARIABLES) for l in libres[v]]).astype(float)
# Sans pénalité, le maximum de vraisemblance part à l'infini (bandes de distance voisines presque colinéaires).
# Pénalité κ/2 β'Rβ : différences premières entre classes voisines (distances, pente), écarts à la moyenne
# pour les 9 classes de végétation.
morceaux = []
for v, t in zip(VARIABLES, TAILLES):
    Dv = ((np.eye(t) - 1 / t) if v == "veg" else np.diff(np.eye(t), axis=0))[:, libres[v]]
    morceaux.append(Dv.T @ Dv)
R = block_diag(*morceaux)


def ll_profil(L, N, dep):
    """Log-vraisemblance de Poisson (à une constante près) de μ_c = exp(γ_dep) Λ_c, les γ_dep étant pris à
    leur maximum exp(γ_d) = N_d / Λ_d : seule compte la répartition des feux entre communes d'un département."""
    Ld, Nd = np.bincount(dep, L, D), np.bincount(dep, N, D)
    return N @ np.log(L) - Nd[Nd > 0] @ np.log(Ld[Nd > 0])


def objectif(beta, S, N, dep, kappa):
    """Opposé de la log-vraisemblance pénalisée et son gradient."""
    lam = np.exp(X @ beta)
    L = S @ lam                                            # Λ_c = Σ_j S[c, j] λ_j
    Ld, Nd = np.bincount(dep, L, D), np.bincount(dep, N, D)
    w = N / L - np.divide(Nd, Ld, out=np.zeros(D), where=Ld > 0)[dep]      # ∂ll / ∂Λ_c
    ll = ll_profil(L, N, dep) - kappa / 2 * beta @ R @ beta
    return -ll, -(X.T @ (lam * (S.T @ w)) - kappa * R @ beta)


def ajuster(garde, kappa, beta0, precis=False):
    """β maximisant la vraisemblance pénalisée sur les communes `garde` (comptes 2006-2018)."""
    args = (S[garde], N_app[garde], dep[garde], kappa)
    beta = minimize(objectif, beta0, args=args, jac=True, method="BFGS").x
    if precis:   # BFGS s'arrête quand la vraisemblance (~1e5) ne baisse plus en double précision ;
        # on finit en annulant le gradient
        beta = root(lambda b: objectif(b, *args)[1], beta, method="hybr", options={"xtol": 1e-13}).x
    return beta


def deviance(L, N, dep):
    """Déviance de Poisson des comptes N, μ ∝ Λ dans chaque département, niveau recalé sur le total observé."""
    tot, obs = np.bincount(dep, L, D), np.bincount(dep, N, D)
    mu = L * np.divide(obs, tot, out=np.zeros(D), where=tot > 0)[dep]
    return 2 * np.sum(xlogy(N, N) - xlogy(N, mu) - (N - mu))


# κ par validation croisée spatiale sur 2006-2018 : 5 plis de blocs de 30 km
bx = ((com["x_l93"] - com["x_l93"].min()) // 30_000).astype(int)
by = ((com["y_l93"] - com["y_l93"].min()) // 30_000).astype(int)
blocs = pd.factorize(bx * 1000 + by)[0]
plis = np.random.default_rng(11).permutation(np.arange(blocs.max() + 1) % 5)[blocs]
cv = dict.fromkeys(KAPPAS, 0.0)
for pli in range(5):
    beta = np.zeros(X.shape[1])
    for kappa in KAPPAS:                                    # chaque ajustement part du précédent
        beta = ajuster(plis != pli, kappa, beta)
        out = plis == pli
        cv[kappa] += deviance(S[out] @ np.exp(X @ beta), N_app[out], dep[out])
KAPPA = min(cv, key=cv.get)
print("déviance de validation croisée :", {k: round(v, 1) for k, v in cv.items()}, "; κ retenu :", KAPPA)

# modèle complet, puis β appris sans chaque région (test « région jamais vue »)
betas = {"toutes": ajuster(np.ones(C, dtype=bool), KAPPA, np.zeros(X.shape[1]), precis=True)}
for region, deps_r in REGIONS.items():
    betas[region] = ajuster(~com["dep"].isin(deps_r).to_numpy(), KAPPA, betas["toutes"], precis=True)
lignes = []
for region, beta in betas.items():
    k = 0
    for v, t in zip(VARIABLES, TAILLES):
        b = np.zeros(t)
        b[libres[v]] = beta[k:k + t - 1]
        k += t - 1
        lignes += [{"region": region, "variable": v, "classe": l, "beta": b[l]} for l in range(t)]
coef = pd.DataFrame(lignes)
coef.to_csv(INTER / "calcul1_lambda.csv", index=False)
print(coef[coef["region"] == "toutes"].assign(intensite_relative=lambda d: np.exp(d["beta"])).round(3).to_string())

# Validation sur les comptes 2019-2024, jamais vus : part de la déviance du modèle « département seul »
# expliquée par λ, et par la seule surface (log μ = γ_dep + b log surface, b estimé sur 2006-2018)
surface = com["surface_km2"].to_numpy()
b = minimize_scalar(lambda b: -ll_profil(surface ** b, N_app, dep)).x
dev = {"département seul": deviance(np.ones(C), N_test, dep),
       "surface + département": deviance(surface ** b, N_test, dep),
       "λ complet": deviance(S @ np.exp(X @ betas["toutes"]), N_test, dep)}
val = pd.DataFrame({"modele": list(dev), "deviance_2019_2024": list(dev.values())})
val["part_deviance_expliquee"] = 1 - val["deviance_2019_2024"] / dev["département seul"]
val.to_csv(SORTIES / "calcul1_validation.csv", index=False)
print(f"feux 2006-2018 : {N_app.sum():.0f} ; 2019-2024 : {N_test.sum():.0f} ; pente sur log(surface) : {b:.3f}")
print(val.round(3).to_string(index=False))
