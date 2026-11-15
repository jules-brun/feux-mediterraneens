# Figures du diaporama. Une figure = un message : pas de titre dans l'image (il est sur la diapositive), étiquettes
# directes plutôt que légendes, deux couleurs au plus ; chaque figure est dessinée à sa taille d'affichage sur une
# diapositive de 1920 x 1080 px, à résolution double (charte : theme.R).
# Entrées : jeu de données (cartes, exemple de distance), tables de resultats/tables (scripts 01 à 07 et 09) et
#           contributions de Shapley de donnees/modeles/shap_*.rds (09_shap.R).
# Sorties : resultats/figures/p*.png, shap_*.png

source("analyses/commun.R")
source("analyses/theme.R")
suppressPackageStartupMessages(library(ggrepel))
tab <- function(nom) fread(file.path("resultats/tables", paste0(nom, ".csv")), encoding = "UTF-8")
TESTS <- c("Années futures (2019-2024)", "Région jamais vue")
COUL_TESTS <- setNames(c(BLEU, ORANGE), TESTS)
# Position verticale de deux séries (les deux tests) autour de chaque ligne d'un graphique en barres horizontales
position <- function(nom, test, niveaux, ecart) length(niveaux) + 1 - match(nom, niveaux) + ifelse(test == TESTS[1], ecart, -ecart)
cat("Figures :\n")

# p01. Zone d'étude et les 6 régions du test « région jamais vue » ----------------------------------------------
dep <- charger_departements()
zone <- dep[dep$dep %in% names(REGIONS), ]
zone$region <- sub("Var-Alpes-Maritimes", "Var-Alpes-Mar.", REGIONS[zone$dep])
reg <- aggregate(zone["region"], by = list(nom = zone$region), FUN = function(x) x[1])
pos <- st_coordinates(st_point_on_surface(st_geometry(reg)))
reg_lab <- data.table(nom = reg$nom, x = pos[, 1], y = pos[, 2])
reg_lab[nom == "Var-Alpes-Mar.", `:=`(x = x + 25000, y = y - 5000)]
reg_lab[nom == "Provence", y := y + 20000]
g <- ggplot() +
  geom_sf(data = dep, fill = "#ECEAE3", colour = "#FFFFFF", linewidth = 0.4) +
  geom_sf(data = zone, aes(fill = region), colour = "#FFFFFF", linewidth = 0.5) +
  geom_label(data = reg_lab, aes(x, y, label = nom), family = POLICE, size = TXT * 0.95, colour = TEXTE,
             fill = alpha(FOND, 0.88), linewidth = 0, label.padding = unit(0.18, "lines")) +
  scale_fill_manual(values = c("Roussillon-Aude" = "#C4652B", "Languedoc" = "#E7A65A", "Ardèche-Drôme" = "#8E6BAF",
                               "Provence" = "#2A6F97", "Var-Alpes-Mar." = "#86B6D9", "Corse" = "#5E8C61")) +
  coord_sf(xlim = CADRE_SUD[c("xmin", "xmax")], ylim = CADRE_SUD[c("ymin", "ymax")], expand = FALSE, datum = NA) +
  theme_carte
sauver(g, "p01_zone_regions.png", 820, 600)

# p33. Échelle des modèles : AUC sur les années futures et sur une région jamais vue ----------------------------
mo <- tab("modeles")[1:3]
mo[, nom := c(sprintf("Météo et calendrier\n(%d variables)", variables[1]),
              sprintf("Sans la mémoire des lieux\n(%d variables)", variables[2]), sprintf("Toutes les variables\n(%d)", variables[3]))]
d <- melt(mo, id.vars = "nom", measure.vars = c("auc_futur", "auc_region"), variable.name = "test", value.name = "auc")
d[, test := factor(fifelse(test == "auc_futur", TESTS[1], TESTS[2]), TESTS)]
d[, y := position(nom, test, mo$nom, 0.19)]
g <- ggplot(d, aes(auc, y, colour = test)) +
  geom_segment(aes(x = 0.5, xend = auc, yend = y), linewidth = 1.3) +
  geom_point(size = 5) +
  geom_text(aes(label = virgule(auc, 3)), hjust = -0.35, family = POLICE, size = TXT, show.legend = FALSE) +
  scale_colour_manual(values = COUL_TESTS, name = NULL) +
  scale_x_continuous(limits = c(0.5, 0.82), breaks = seq(0.5, 0.8, 0.1), labels = virgule(seq(0.5, 0.8, 0.1), 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(breaks = 3:1, labels = mo$nom, limits = c(0.5, 3.5)) +
  labs(x = "AUC (0,5 = hasard ; 1 = classement parfait)", y = NULL) +
  theme_diapo("x") + theme(legend.position = "top", legend.justification = "left")
sauver(g, "p33_echelle_modeles.png", 960, 600)

# p34. Ce que regarde le modèle, avec et sans la mémoire des lieux : les 10 variables les plus utilisées -----------
COURTS_IMP <- c(taux_echappement_commune_10ans_lisse = "Taux passé de la commune (lissé)",
                taux_echappement_commune_10ans = "Taux passé de la commune (brut)",
                taux_echappement_dep_10ans = "Taux passé du département", lon = "Longitude",
                humidite_alerte_pct = "Humidité de l’air à l’alerte", heure_alerte = "Heure de l’alerte",
                rafale_max_alerte_kmh = "Rafales à l’alerte", vent_moyen_alerte_kmh = "Vent moyen à l’alerte",
                jour_annee_cos = "Saison", annee = "Année", clc_interface_habitat_comb_km_km2 = "Interface habitat-végétation",
                bdfh_part_pins_med = "Pins méditerranéens", bdfh_part_landes = "Landes et garrigues",
                clc_surf_massif_max_ha = "Taille du plus grand massif", ndvi_avant = "Verdure avant le feu (NDVI)")
COUL_IMP <- c("Mémoire des lieux" = ORANGE, "Météo et calendrier" = BLEU, "Végétation" = "#5E8C61")
imp <- tab("modeles_importance")
panneau_imp <- function(m, titre) {
  d <- imp[modele == m][1:10]
  d[, groupe := fcase(memoire, "Mémoire des lieux", famille %in% c("végétation", "état de la végétation"), "Végétation",
                      default = "Météo et calendrier")]
  d[, nom := ifelse(variable %in% names(COURTS_IMP), COURTS_IMP[variable], variable)]
  ggplot(d, aes(shap_moy_abs, factor(nom, levels = rev(nom)), fill = groupe)) +
    geom_col(width = 0.7, show.legend = TRUE) +
    scale_fill_manual(values = COUL_IMP, limits = names(COUL_IMP), name = NULL) +
    scale_x_continuous(limits = c(0, 0.27), breaks = c(0, 0.1, 0.2), labels = function(x) virgule(x, 1),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(subtitle = titre, x = "Contribution moyenne (|SHAP|)", y = NULL) +
    theme_diapo("x") + theme(plot.subtitle = element_text(size = PT, colour = TEXTE, face = "bold", hjust = 0),
                             plot.title.position = "plot")
}
g <- (panneau_imp("Toutes les variables", "Avec la mémoire des lieux (174 variables)") |
        panneau_imp("Sans la mémoire des lieux", "Sans la mémoire des lieux (157 variables)")) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top", legend.justification = "left", plot.background = element_rect(fill = FOND, colour = NA))
sauver(g, "p34_ce_que_regarde.png", 1664, 560)

# p12. Végétation à météo comparable : taux observés --------------------------------------------------------------
DEFAV <-"Végétation la plus défavorable\n(1 départ sur 5)"
FAV <- "Végétation la plus favorable\n(1 départ sur 5)"
v <- tab("vegetation_taux")
v[, classe := fifelse(classe == "défavorable", DEFAV, FAV)]
v[, q := as.integer(sub("Q", "", strate))]
g <- ggplot(v, aes(q, taux, colour = classe)) +
  geom_errorbar(aes(ymin = bas, ymax = haut), width = 0.08, linewidth = 0.8) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 5) +
  geom_text(data = v[q %in% c(1, 5)], aes(label = pct(taux), hjust = ifelse(q == 1, 1.45, -0.45)), family = POLICE,
            size = TXT, show.legend = FALSE) +
  geom_text(data = v[q == 5], aes(x = ifelse(classe == DEFAV, 2.6, 4.3), y = ifelse(classe == DEFAV, 0.49, 0.045), label = classe),
            family = POLICE, size = TXT, fontface = "bold", lineheight = 0.95, show.legend = FALSE) +
  scale_colour_manual(values = setNames(c(ORANGE, BLEU), c(DEFAV, FAV))) +
  scale_x_continuous(breaks = 1:5, labels = c("Q1\nfaible", "Q2", "Q3", "Q4", "Q5\nfort"), limits = c(0.55, 5.6)) +
  scale_y_continuous(labels = pct, limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1)) +
  labs(x = "Danger météo à l’alerte (quintiles)", y = "Part des départs\nqui dépassent 1 ha") +
  theme_diapo("y")
sauver(g, "p12_vegetation_meteo.png", 1060, 620)

# p13. Ce que la végétation ajoute, selon ce que le modèle sait déjà ---------------------------------------------
gv <- tab("vegetation_gains")
gv[, test := factor(fifelse(test == "années futures", TESTS[1], TESTS[2]), TESTS)]
gv[, y := position(ajout, test, unique(ajout), 0.18)]
g <- ggplot(gv, aes(y = y)) +
  geom_vline(xintercept = 0, colour = TEXTE2, linewidth = 0.7) +
  geom_rect(aes(xmin = 0, xmax = gain, ymin = y - 0.15, ymax = y + 0.15, fill = test)) +
  geom_errorbar(aes(xmin = bas, xmax = haut), width = 0.12, linewidth = 0.7, colour = TEXTE2, orientation = "y") +
  geom_text(aes(x = pmax(haut, 0), label = signe(gain)), hjust = -0.25, family = POLICE, size = TXT, colour = TEXTE) +
  scale_fill_manual(values = COUL_TESTS, name = NULL) +
  scale_x_continuous(limits = c(-0.004, 0.112), breaks = seq(0, 0.1, 0.02), labels = virgule(seq(0, 0.1, 0.02), 2)) +
  scale_y_continuous(breaks = 3:1, labels = unique(gv$ajout), limits = c(0.5, 3.5)) +
  labs(x = "Gain d’AUC quand on ajoute la végétation…", y = NULL) +
  theme_diapo("x") + theme(legend.position = "top", legend.justification = "left")
sauver(g, "p13_vegetation_apport.png", 1060, 560)

# p11. Pratiques d'enregistrement par département -----------------------------------------------------------------
dd <- tab("enregistrement_departements")[order(taux_1ha)]
g <- ggplot(dd, aes(part_moins_100m2, taux_1ha)) +
  geom_smooth(method = "lm", se = FALSE, colour = GRIS, linewidth = 1, linetype = "dashed", formula = y ~ x) +
  geom_point(size = 5, colour = BLEU) +
  geom_text_repel(aes(label = nom), family = POLICE, size = TXT * 0.92, colour = TEXTE, seed = 2, box.padding = 0.45,
                  point.padding = 0.3, min.segment.length = 0.3, segment.colour = GRIS, max.overlaps = 30) +
  scale_x_continuous(labels = pct, limits = c(0, 0.37)) +
  scale_y_continuous(labels = pct, limits = c(0.05, 0.58)) +
  labs(x = "Part des départs enregistrés à moins de 100 m²", y = "Part des départs\nqui dépassent 1 ha") +
  theme_diapo("xy")
sauver(g, "p11_enregistrement.png", 1060, 620)

# p30. Avant/après : chaque année prédite par le modèle appris sur les années précédentes -------------------------
an <- tab("avant_apres")
nouvelle_fiche <- function(y) list(annotate("rect", xmin = 2022.6, xmax = 2024.4, ymin = -Inf, ymax = Inf, fill = "#F0EEE7"),
                                   annotate("text", x = 2023.5, y = y, label = "nouvelle\nfiche", family = POLICE,
                                            size = TXT, colour = TEXTE2, lineheight = 0.9))
classement <- ggplot(an, aes(annee, auc)) +
  geom_line(colour = BLEU, linewidth = 1.1) +
  geom_point(colour = BLEU, size = 3) +
  scale_x_continuous(breaks = seq(2012, 2024, 4), limits = c(2011.6, 2026.6)) +
  scale_y_continuous(labels = function(x) virgule(x, 2), limits = c(0.65, 0.85), breaks = seq(0.65, 0.85, 0.05)) +
  labs(x = NULL, y = "Classement :\nAUC de l’année") +
  theme_diapo("y")
niveau <- ggplot(an, aes(annee)) +
  nouvelle_fiche(0.31) +
  geom_linerange(aes(ymin = obs_bas, ymax = obs_haut), colour = GRIS, linewidth = 1.2) +
  geom_point(aes(y = observe), colour = TEXTE, size = 3.2) +
  geom_line(aes(y = predit), colour = ORANGE, linewidth = 1.1) +
  geom_point(aes(y = predit), colour = ORANGE, shape = 18, size = 4.4) +
  annotate("text", x = 2024.4, y = an[annee == 2024, predit] + 0.018, label = "prédit", hjust = 0, family = POLICE,
           size = TXT, colour = ORANGE) +
  annotate("text", x = 2024.4, y = an[annee == 2024, observe] - 0.018, label = "observé", hjust = 0, family = POLICE,
           size = TXT, colour = TEXTE) +
  scale_x_continuous(breaks = seq(2012, 2024, 4), limits = c(2011.6, 2026.6)) +
  scale_y_continuous(labels = pct, limits = c(0.12, 0.34), breaks = seq(0.15, 0.3, 0.05)) +
  labs(x = NULL, y = "Niveau : départs\nqui dépassent 1 ha") +
  theme_diapo("y")
sauver(classement / niveau + plot_layout(heights = c(1, 1.25)) + fond_patch, "p30_avant_apres.png", 880, 560)

# p19. Le découpage sud / nord ------------------------------------------------------------------------------------
NORD <- c("04", "05", "07", "26", "48")
eff <- feux[, .(feux = .N, taux = mean(y_1ha), deps = uniqueN(dep)), keyby = .(zone = ifelse(dep %in% NORD, "Nord", "Sud"))]
z <- dep[dep$dep %in% names(REGIONS), ]
z$zone <- ifelse(z$dep %in% NORD, "Nord", "Sud")
etiquette <- function(zz, titre) sprintf("%s\n%d départements · %s départs\n%s d’échappement", titre, eff[zz, deps],
                                         format(eff[zz, feux], big.mark = " "), pct(eff[zz, taux]))
etiq <- data.table(x = c(798000, 935000), y = c(6545000, 6140000),   # position des étiquettes (Lambert-93)
                   lab = c(etiquette("Nord", "Nord : intérieur et montagne"), etiquette("Sud", "Sud : littoral et Corse")))
g <- ggplot() +
  geom_sf(data = dep, fill = "#ECEAE3", colour = "#FFFFFF", linewidth = 0.4) +
  geom_sf(data = z, aes(fill = zone), colour = "#FFFFFF", linewidth = 0.5) +
  geom_label(data = etiq, aes(x, y, label = lab), family = POLICE, size = TXT, colour = TEXTE, fill = alpha(FOND, 0.92),
             linewidth = 0, lineheight = 0.95, label.padding = unit(0.3, "lines")) +
  scale_fill_manual(values = c(Nord = "#5E8FB8", Sud = "#D2A86A")) +
  coord_sf(xlim = CADRE_SUD[c("xmin", "xmax")], ylim = CADRE_SUD[c("ymin", "ymax")] + c(-40000, 110000),
           expand = FALSE, datum = NA) +
  theme_carte
sauver(g, "p19_sud_nord_carte.png", 760, 600)

# p20. Classement des feux du nord, modèles appris au sud ou sur place ---------------------------------------------
sn <- tab("sud_nord")
sn[, nom := factor(modele, levels = rev(unique(modele)))]
sn[, src := factor(fifelse(appris == "au sud", "appris au sud", "appris sur place, au nord"),
                   levels = c("appris au sud", "appris sur place, au nord"))]
g <- ggplot(sn, aes(auc, nom)) +
  geom_line(aes(group = nom), colour = "#D5D2C8", linewidth = 2.4) +
  geom_point(aes(colour = src), size = 5.5) +
  geom_text(aes(label = virgule(auc, 3), colour = src, vjust = ifelse(src == "appris au sud", -1.1, 2.1)), family = POLICE,
            size = TXT, show.legend = FALSE) +
  scale_colour_manual(values = c("appris au sud" = ORANGE, "appris sur place, au nord" = BLEU), name = NULL) +
  scale_x_continuous(limits = c(0.64, 0.76), breaks = seq(0.64, 0.76, 0.04), labels = virgule(seq(0.64, 0.76, 0.04), 2)) +
  labs(x = "AUC sur les feux du nord 2019-2024", y = NULL) +
  theme_diapo("x") + legende_haut
sauver(g, "p20_sud_nord_classement.png", 900, 500)

# p27. Principe des deux distances sur un exemple : anomalie d'humidité du sol, 2022 contre 2006-2021 ---------------
ref <- na.omit(feux[annee < 2022, swi_sim2_anom_z])
cib <- na.omit(feux[annee == 2022, swi_sim2_anom_z])
grille <- sort(unique(c(ref, cib)))
Fr <- ecdf(ref)(grille)
Fc <- ecdf(cib)(grille)
ecart <- abs(Fr - Fc)
k <- which.max(ecart)
w1 <- sum(ecart[-length(grille)] * diff(grille)) / sd(ref)
cat(sprintf("     exemple : Kolmogorov-Smirnov %.2f en %.2f, Wasserstein %.2f écart-type\n", ecart[k], grille[k], w1))
bornes <- quantile(c(ref, cib), c(0.005, 0.995))
garde <- grille >= bornes[1] & grille <= bornes[2]
courbes <- rbind(data.table(x = grille, F = Fr, serie = "2006-2021"), data.table(x = grille, F = Fc, serie = "2022"))[c(garde, garde)]
x_lab <- bornes[1] + 0.6 * diff(bornes)
g <- ggplot() +
  geom_ribbon(data = data.table(x = grille, bas = pmin(Fr, Fc), haut = pmax(Fr, Fc))[garde], aes(x, ymin = bas, ymax = haut),
              fill = BLEU, alpha = 0.16) +
  geom_step(data = courbes, aes(x, F, colour = serie), linewidth = 1.2) +
  annotate("segment", x = grille[k], xend = grille[k], y = Fr[k], yend = Fc[k], colour = ORANGE, linewidth = 1.4,
           arrow = arrow(ends = "both", length = unit(7, "pt"))) +
  annotate("segment", x = grille[k], xend = x_lab, y = (Fr[k] + Fc[k]) / 2, yend = 0.3, colour = ORANGE, linewidth = 0.5) +
  annotate("label", x = x_lab, y = 0.3, label = paste0("Kolmogorov-Smirnov :\nécart maximal ", virgule(ecart[k], 2)),
           hjust = 0, vjust = 1, family = POLICE, size = TXT * 0.9, colour = ORANGE, fill = FOND, linewidth = 0, lineheight = 0.95) +
  annotate("label", x = bornes[1] + 0.02 * diff(bornes), y = 0.93, label = paste0("Wasserstein : aire colorée\n", virgule(w1, 2),
           " écart-type"), hjust = 0, family = POLICE, size = TXT * 0.9, colour = BLEU, fill = FOND, linewidth = 0, lineheight = 0.95) +
  scale_colour_manual(values = c("2006-2021" = "#8A929B", "2022" = BLEU), name = NULL) +
  scale_y_continuous(labels = pct, limits = c(0, 1)) +
  scale_x_continuous(labels = function(x) gsub("-", "−", virgule(x, 1))) +
  labs(x = "Anomalie d’humidité du sol", y = "Proportion cumulée des départs") +
  theme_diapo("y") + legende_haut
sauver(g, "p27_distance_principe.png", 820, 520)

# p31. Cours : deux lois normales d'écart-type 1 décalées de δ ; Kolmogorov-Smirnov plafonne à 1, Wasserstein vaut δ --
xs <- seq(-3.5, 9, by = 0.02)
CAS <- c("δ = 1 : les lois se recouvrent", "δ = 4 : elles se séparent")
dens <- rbindlist(lapply(1:2, function(i) {
  dl <- c(1, 4)[i]
  data.table(cas = factor(CAS[i], CAS), x = xs, y = c(dnorm(xs), dnorm(xs, dl)), loi = rep(c("référence", "décalée"), each = length(xs)))
}))
etiq_cas <- data.table(cas = factor(CAS, CAS), lab = paste0("KS ", virgule(2 * pnorm(c(1, 4) / 2) - 1, 2), "\nW ", c(1, 4)))
g_dens <- ggplot(dens, aes(x, y, colour = loi)) +
  geom_line(linewidth = 1.2) +
  geom_text(data = etiq_cas, aes(x = 9, y = 0.36, label = lab), inherit.aes = FALSE, hjust = 1, vjust = 1, family = POLICE,
            size = TXT, colour = TEXTE, lineheight = 0.95) +
  facet_wrap(~cas, ncol = 1) +
  scale_colour_manual(values = c(référence = "#8A929B", décalée = BLEU), guide = "none") +
  scale_y_continuous(limits = c(0, 0.42), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = NULL) +
  theme_diapo("aucune") + theme(axis.text = element_blank(), strip.text = element_text(size = PT, face = "plain"))
dl <- seq(0, 5, by = 0.05)
g_courbes <- ggplot(rbind(data.table(dl, v = 2 * pnorm(dl / 2) - 1, m = "ks"), data.table(dl, v = dl, m = "w")), aes(dl, v, colour = m)) +
  geom_line(linewidth = 1.5) +
  annotate("text", x = 5, y = 1.25, label = "Kolmogorov-Smirnov\n= variation totale :\nplafond à 1", hjust = 1, vjust = 0,
           family = POLICE, size = TXT, colour = ORANGE, lineheight = 0.95) +
  annotate("text", x = 2.7, y = 3.9, label = "Wasserstein = δ", hjust = 1, family = POLICE, size = TXT, colour = BLEU) +
  scale_colour_manual(values = c(ks = ORANGE, w = BLEU), guide = "none") +
  scale_x_continuous(breaks = 0:5) +
  scale_y_continuous(breaks = 0:5, limits = c(0, 5.2)) +
  labs(x = "Écart δ entre les lois (écarts-types)", y = "Distance") +
  theme_diapo("xy")
sauver(g_dens + g_courbes + plot_layout(widths = c(1, 1.25)) + fond_patch, "p31_recouvrement_deplacement.png", 900, 520)

# p32. Cours : distance due au seul hasard en 2023, départs tirés un par un ou journées entières ----------------------
hz <- tab("hasard_2023")
observe <- hz[tirage == "observé", distance]
nul <- hz[tirage != "observé"]
pic <- nul[, {
  dd <- density(distance, adjust = 1.2)
  .(x = dd$x[which.max(dd$y)], y = max(dd$y))
}, by = tirage]
pic[, lab := fifelse(tirage == "un par un", "départs tirés\nun par un", "journées entières\ntirées")]
g <- ggplot(nul, aes(distance, fill = tirage)) +
  geom_density(alpha = 0.65, colour = NA, adjust = 1.2) +
  geom_text(data = pic, aes(x = x, y = y, label = lab, colour = tirage), inherit.aes = FALSE, vjust = -0.25, family = POLICE,
            size = TXT, lineheight = 0.9) +
  geom_vline(xintercept = observe, colour = ORANGE, linewidth = 1.3) +
  annotate("text", x = observe + 0.004, y = max(pic$y) * 0.55, label = paste0("2023 observé :\n", virgule(observe, 2)),
           hjust = 0, family = POLICE, size = TXT, colour = ORANGE, lineheight = 0.9) +
  scale_fill_manual(values = c("un par un" = "#8A929B", "journées" = BLEU), guide = "none") +
  scale_colour_manual(values = c("un par un" = "#5E666E", "journées" = BLEU), guide = "none") +
  scale_x_continuous(labels = function(x) virgule(x, 2), limits = c(0, observe * 1.45)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = "Distance moyenne, météo et végétation vivante (écarts-types)", y = "Tirages") +
  theme_diapo("x") + theme(axis.text.y = element_blank())
sauver(g, "p32_hasard_journees.png", 880, 470)

# p28. Distance de chaque année aux précédentes (variables, prédictions, hasard) et erreur de niveau -------------------
da <- merge(tab("distances_annees"), an, by = "annee")
COUL_S <- c("météo et\nvégétation vivante" = BLEU, "prédictions\ndu modèle" = ORANGE)
long <- rbind(da[, .(annee, distance = `w1_météo et végétation vivante`, serie = names(COUL_S)[1])],
              da[, .(annee, distance = w1_predictions, serie = names(COUL_S)[2])])
meteo <- da[annee %in% c(2013, 2018, 2022), .(annee, y = `w1_météo et végétation vivante`, lab = c("humide", "humide", "sèche"))]
haut <- ggplot(long, aes(annee, distance, colour = serie)) +
  geom_ribbon(data = da, aes(annee, ymin = 0, ymax = pmax(hasard_meteo, hasard_predictions)), inherit.aes = FALSE,
              fill = "#E6E3DA") +
  annotate("text", x = 2024.3, y = da[annee == 2024, max(hasard_meteo, hasard_predictions)] / 2, label = "hasard seul",
           hjust = 0, family = POLICE, size = TXT, colour = TEXTE2) +
  geom_text(data = meteo, aes(annee, y, label = lab), inherit.aes = FALSE, vjust = -0.9, family = POLICE, size = TXT,
            colour = BLEU) +
  geom_line(aes(linewidth = serie)) +
  geom_point(size = 2.8) +
  geom_text(data = long[annee == 2024], aes(label = serie), hjust = 0, nudge_x = 0.3, family = POLICE, size = TXT,
            lineheight = 0.9) +
  scale_colour_manual(values = COUL_S, guide = "none") +
  scale_linewidth_manual(values = c(1, 1.6), guide = "none") +
  scale_x_continuous(breaks = seq(2012, 2024, 4), limits = c(2011.6, 2029.2)) +
  scale_y_continuous(labels = function(x) virgule(x, 1), breaks = seq(0, 0.3, 0.1), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.14))) +
  labs(x = NULL, y = "Distance aux\nannées précédentes") +
  theme_diapo("y")
signe_pts <- function(x) ifelse(x > 0, paste0("+", x), ifelse(x < 0, paste0("−", abs(x)), "0"))
erreur <- ggplot(da, aes(annee, 100 * (predit - observe))) +   # barres : intervalle à 95 % de la part observée
  nouvelle_fiche(9.4) +
  geom_hline(yintercept = 0, colour = TEXTE2, linewidth = 0.6) +
  geom_linerange(aes(ymin = 100 * (predit - obs_haut), ymax = 100 * (predit - obs_bas)), colour = GRIS, linewidth = 1.2) +
  geom_point(colour = ORANGE, size = 3.4) +
  scale_x_continuous(breaks = seq(2012, 2024, 4), limits = c(2011.6, 2029.2)) +
  scale_y_continuous(labels = signe_pts, breaks = seq(-6, 6, 3), limits = c(-8, 11.5)) +
  labs(x = NULL, y = "Prédit − observé\n(points)") +
  theme_diapo("y")
sauver(haut / erreur + plot_layout(heights = c(1.15, 1)) + fond_patch, "p28_distances_annees.png", 900, 600)

# p29. Variables qui ont le plus bougé : 2022 contre les années précédentes, puis le nord contre le sud --------------
COURTS <- c(swi_sim2_anom_z = "Anomalie d’humidité du sol", swi_sim2_anom = "Anomalie d’humidité du sol",
            wg_racine_sim2_anom = "Anomalie d’eau dans le sol", indice_humidite_anom_z = "Anomalie d’humidité de la végétation",
            indice_humidite_anom = "Anomalie d’humidité de la végétation", ndvi_anom_z = "Anomalie de verdure (NDVI)",
            ndvi_anom = "Anomalie de verdure (NDVI)", bdfh_part_ouverte_dans_foret = "Forêt ouverte (part de la forêt)",
            jour_annee = "Jour de l’année", mois = "Mois de l’alerte", dist_cote_km = "Distance à la côte",
            bdf_part_pin_sylvestre = "Pin sylvestre (part de la commune)", bdf_surf_pin_sylvestre_ha = "Pin sylvestre (surface)",
            alt_min_m = "Altitude minimale", bdf_part_autres_coniferes = "Autres conifères (BD Forêt v2)",
            bdfh_part_autres_coniferes = "Autres conifères", clc_surf_massif_max_ha = "Taille du plus grand massif")
COUL_FAM <- c("météo et végétation vivante" = BLEU, "lieux" = "#8C6D1F", "saison et heure" = "#52606D",
              "historique des feux" = GRIS)
premieres <- function(t) {   # 7 premières variables, une seule version (brute ou centrée-réduite) d'une même grandeur
  t <- t[!is.na(w1)][order(-w1)][, nom := ifelse(variable %in% names(COURTS), COURTS[variable], variable)]
  t[!duplicated(nom)][1:7]
}
t1 <- premieres(tab("distances_variables")[annee == 2022])
t2 <- premieres(tab("distances_nord_sud")[!variable %in% c("lon", "lat")])
familles <- intersect(names(COUL_FAM), c(t1$famille, t2$famille))
panneau <- function(t, titre) {
  ggplot(t, aes(w1, factor(nom, levels = rev(nom)), fill = factor(famille, levels = familles))) +
    geom_col(width = 0.66) +
    scale_fill_manual(values = COUL_FAM[familles], name = NULL, drop = FALSE) +
    scale_x_continuous(labels = function(x) gsub(".", ",", format(x, trim = TRUE, drop0trailing = TRUE), fixed = TRUE),
                       expand = expansion(mult = c(0, 0.06))) +
    labs(title = titre, x = NULL, y = NULL) +
    theme_diapo("x") + theme(plot.title = element_text(size = PT, colour = TEXTE, face = "bold"), plot.title.position = "plot")
}
g1 <- panneau(t1, "2022 contre 2006-2021") + legende_haut
g2 <- panneau(t2, "Nord contre sud, 2006-2018 (hors coordonnées)") + labs(x = "Distance de Wasserstein (en écarts-types)")
sauver(g1 / g2 + fond_patch, "p29_distances_variables.png", 900, 660)

# Figures SHAP (contributions de 09_shap.R) ---------------------------------------------------------------------
GROUPES_SHAP <- c("mémoire des lieux" = "Mémoire des lieux", "végétation" = "Végétation", "météo" = "Météo",
                  "calendrier" = "Calendrier", "état de la végétation" = "État de la végétation",
                  "bilan hydrique" = "Bilan hydrique", "relief et position" = "Relief et position",
                  "présence humaine" = "Présence humaine", "historique" = "Autres départs récents")
SANS_MEM <- "Sans la mémoire des lieux"
AVEC_MEM <- "Avec la mémoire des lieux"
COUL_MEM <- setNames(c(BLEU, ORANGE), c(SANS_MEM, AVEC_MEM))

# shap_familles. Poids de chaque groupe de variables, sans puis avec la mémoire des lieux : effet moyen du groupe sur
# la probabilité prédite (moyenne sur les feux 2019-2024 de |contribution du groupe|, en points autour de 22,7 %)
sf <- tab("shap_familles")
sf[, modele := fifelse(modele == "Toutes les variables", AVEC_MEM, SANS_MEM)]
sf[, nom := sprintf("%s (%d)", GROUPES_SHAP[groupe], variables)]
sf[, y := length(GROUPES_SHAP) + 1 - match(groupe, names(GROUPES_SHAP))]
sf[, points := 100 * points_moy_abs]
lien <- dcast(sf, y ~ modele, value.var = "points")[!is.na(get(SANS_MEM))]
cles <- sf[groupe %in% c("mémoire des lieux", "végétation")]
g <- ggplot(sf, aes(points, y)) +
  geom_segment(data = lien, aes(x = get(SANS_MEM), xend = get(AVEC_MEM), y = y, yend = y), colour = GRIS, linewidth = 1.2) +
  geom_point(aes(colour = modele), size = 5) +
  geom_text(data = cles, aes(label = virgule(points, 1), colour = modele), vjust = -1.1, family = POLICE, size = TXT,
            show.legend = FALSE) +
  scale_colour_manual(values = COUL_MEM, limits = names(COUL_MEM), name = NULL) +
  scale_x_continuous(limits = c(0, 9.6), breaks = 0:9, labels = function(x) virgule(x, 0), expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(breaks = unique(sf[, .(y, nom)])$y, labels = unique(sf[, .(y, nom)])$nom,
                     limits = c(0.5, length(GROUPES_SHAP) + 0.7)) +
  labs(x = "Effet moyen du groupe sur la probabilité prédite (points de %)", y = NULL) +
  theme_diapo("x") + legende_haut
sauver(g, "shap_familles.png", 1060, 620)

# shap_dependance. Courbes de dépendance : effet de quatre variables de végétation sur la probabilité prédite, selon
# leur valeur, sans puis avec la mémoire des lieux (points : un feu 2019-2024 ; courbe : lissage GAM ; feux hors des 1er
# et 99e centiles de la variable hors du cadre, où le lissage n'aurait presque plus de feux ; histogramme : répartition)
shp <- setNames(lapply(c("sans_memoire", "complet"), function(m) readRDS(sprintf("donnees/modeles/shap_%s.rds", m))),
                c(SANS_MEM, AVEC_MEM))
VARS_DEP <- c(bdfh_part_landes = "Landes et garrigues", bdfh_part_pins_med = "Pins méditerranéens",
              clc_interface_habitat_comb_km_km2 = "Interface habitat-végétation", ndvi_avant = "Verdure avant le feu (NDVI)")
AXES_DEP <- c(bdfh_part_landes = "Part de la commune (%)", bdfh_part_pins_med = "Part de la commune (%)",
              clc_interface_habitat_comb_km_km2 = "Longueur de lisière habitat-combustible (km par km²)",
              ndvi_avant = "NDVI avant le feu (0 = sol nu, 1 = très vert)")
ECHELLE_DEP <- c(bdfh_part_landes = 100, bdfh_part_pins_med = 100, clc_interface_habitat_comb_km_km2 = 1, ndvi_avant = 1)
dl <- rbindlist(lapply(names(shp), function(m) rbindlist(lapply(names(VARS_DEP), function(v)
  data.table(modele = m, variable = v, x = shp[[m]]$valeurs[[v]] * ECHELLE_DEP[[v]], points = 100 * en_points(shp[[m]]$phi_var[, v]))))))
dl <- dl[!is.na(x)]
dl[, dans := x >= quantile(x, 0.01) & x <= quantile(x, 0.99), by = .(modele, variable)]
dl[, modele := factor(modele, names(COUL_MEM))]
LIM_Y <- c(-9, 6)
signe_entier <- function(y) ifelse(y > 0, paste0("+", y), ifelse(y < 0, paste0("−", abs(y)), "0"))
# deux graphiques par variable : la courbe (haut) et l'histogramme (bas)
panneau_dep <- function(v, gauche) {
  d <- dl[variable == v & dans == TRUE]
  haut <- ggplot(d, aes(x, points, colour = modele)) +
    geom_hline(yintercept = 0, colour = TEXTE2, linewidth = 0.6) +
    geom_point(size = 0.5, alpha = 0.05, shape = 16) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, linewidth = 1.6) +
    scale_colour_manual(values = COUL_MEM, limits = names(COUL_MEM), name = NULL) +
    scale_x_continuous(labels = function(x) virgule(x, if (ECHELLE_DEP[[v]] == 1) 1 else 0)) +
    scale_y_continuous(labels = signe_entier, breaks = seq(-8, 6, 2)) +
    coord_cartesian(ylim = LIM_Y, expand = FALSE) +
    # étiquettes directes des deux courbes, dans le panneau des pins où il y a de la place sous la courbe bleue
    (if (v == "bdfh_part_pins_med") annotate("text", x = 26, y = c(0.9, -5.9), label = names(COUL_MEM)[2:1],
                                              colour = COUL_MEM[2:1], family = POLICE, size = TXT, fontface = "bold")) +
    labs(subtitle = VARS_DEP[[v]], x = NULL, y = if (gauche) "Effet sur la probabilité\nprédite (points de %)" else NULL) +
    theme_diapo("y") +
    theme(plot.subtitle = element_text(size = PT, colour = TEXTE, face = "bold", hjust = 0), plot.title.position = "plot",
          axis.text.x = element_blank(), plot.margin = margin(6, 24, 0, 12))
  bas <- ggplot(d[modele == SANS_MEM], aes(x)) +
    geom_histogram(bins = 40, fill = GRIS, colour = FOND, linewidth = 0.2) +
    scale_x_continuous(labels = function(x) virgule(x, if (ECHELLE_DEP[[v]] == 1) 1 else 0)) +
    coord_cartesian(xlim = range(d$x), expand = FALSE) +
    labs(x = AXES_DEP[[v]], y = if (gauche) "Feux" else NULL) +
    theme_diapo("aucune") + theme(axis.text.y = element_blank(), plot.margin = margin(0, 24, 12, 12))
  list(haut = haut, bas = bas)
}
p <- lapply(seq_along(VARS_DEP), function(i) panneau_dep(names(VARS_DEP)[i], i %% 2 == 1))
# grille à plat : courbes et histogrammes alternés, les deux colonnes alignées
g <- wrap_plots(p[[1]]$haut, p[[2]]$haut, p[[1]]$bas, p[[2]]$bas, p[[3]]$haut, p[[4]]$haut, p[[3]]$bas, p[[4]]$bas,
                ncol = 2, heights = c(4, 1, 4, 1)) &
  theme(legend.position = "none", plot.background = element_rect(fill = FOND, colour = NA))
sauver(g, "shap_dependance.png", 1500, 980)

# zones_auc. Classifieur discriminant (10_zones.R) : à quel point chaque famille de variables suffit à reconnaître la
# zone d'un feu (AUC hors échantillon ; 0,5 = zones indiscernables, 1 = parfaitement séparées)
ZONES_COURTES <- c("Nord contre sud" = "Nord\n(contre sud)", "Corse contre le reste" = "Corse",
                   "Provence contre le reste" = "Provence", "Var-Alpes-Maritimes contre le reste" = "Var-Alpes-\nMaritimes",
                   "Languedoc contre le reste" = "Languedoc", "Roussillon-Aude contre le reste" = "Roussillon-\nAude",
                   "Ardèche-Drôme contre le reste" = "Ardèche-\nDrôme")
TOUTES <- "Toutes (hors mémoire)"
za <- tab("zones_auc")
za[, zone := factor(ZONES_COURTES[comparaison], ZONES_COURTES)]
za[, ens := ifelse(ensemble == TOUTES, "Toutes les variables\n(hors mémoire des lieux)", sprintf("%s (%d)", ensemble, variables))]
familles_triees <- za[ensemble != TOUTES, .(m = mean(auc)), by = ens][order(-m), ens]   # de la plus discriminante à la moins
za[, ens := factor(ens, rev(c(za[ensemble == TOUTES, ens[1]], familles_triees)))]
g <- ggplot(za, aes(zone, ens, fill = auc)) +
  geom_tile(colour = FOND, linewidth = 1.2) +
  geom_text(aes(label = virgule(auc, 2), colour = auc > 0.76), family = POLICE, size = TXT * 0.85, show.legend = FALSE) +
  geom_hline(yintercept = length(levels(za$ens)) - 0.5, colour = TEXTE, linewidth = 0.8) +
  scale_colour_manual(values = c("FALSE" = TEXTE, "TRUE" = "#FFFFFF")) +
  scale_fill_gradient(low = "#EEEBE3", high = "#173F5A", limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1),
                      labels = function(x) virgule(x, 1),
                      name = "AUC du classifieur (0,5 = zones indiscernables, 1 = parfaitement séparées)") +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_diapo("aucune") +
  theme(legend.position = "bottom", legend.justification = "left", legend.title.position = "top",
        legend.title = element_text(size = PT - 2, colour = TEXTE2), legend.key.width = unit(60, "pt"),
        legend.key.height = unit(10, "pt"), axis.text.x.top = element_text(colour = TEXTE, lineheight = 0.9))
sauver(g, "zones_auc.png", 1400, 720)

# zones_profils. Carte de chaleur : en quoi chaque zone diffère du reste (11_profils_zones.R). Lignes : pour chaque zone,
# ses 3 variables les plus éloignées du reste (distance de Wasserstein), sans doublon : une seule version d'une même
# essence (bdfh_ / bdf_) ni deux variables corrélées à plus de 0,8 ; parts de la commune seulement (les surfaces en
# hectares mesurent surtout la taille des communes). Couleur : distance signée par le sens de l'écart des moyennes.
NOMS_PROFILS <- c(dist_cote_km = "Distance à la côte", alt_min_m = "Altitude minimale",
                  alt_et_m = "Relief contrasté (écart-type d’altitude)", pente_moy_deg = "Pente moyenne",
                  bdf_part_pin_sylvestre = "Pin sylvestre", bdf_part_pin_alep = "Pin d’Alep",
                  bdf_part_pin_maritime = "Pin maritime", bdf_part_autres_feuillus = "Hêtre, châtaignier, autres feuillus",
                  bdf_part_melange_ferme = "Forêt mixte feuillus-conifères", clc_part_foret_melangee = "Forêt mélangée (Corine)",
                  bdf_part_foret_totale = "Forêt, toutes formes", bdfh_part_ouverte_dans_foret = "Forêt ouverte (part de la forêt)",
                  bdfh_part_pins_med_dans_foret = "Pins méditerranéens (part de la forêt)",
                  bdfh_part_pins_med = "Pins méditerranéens (Alep, maritime, pignon)",
                  clc_part_veg_clairsemee = "Végétation clairsemée", clc_part_agricole = "Terres agricoles",
                  clc_surf_massif_max_ha = "Taille du plus grand massif",
                  clc_interface_habitat_comb_km_km2 = "Interface habitat-végétation",
                  population_connue_hab = "Population de la commune")
pz <- tab("profils_zones")
d_ref <- feux[apprentissage == TRUE]
meme_essence <- function(v) sub("^bdfh_", "bdf_", v)
retenues <- rbindlist(lapply(names(ZONES_COURTES), function(k) {
  pris <- character()
  for (v in pz[comparaison == k & !grepl("^bdf_surf_", variable)][order(-w1), variable]) {
    if (length(pris) == 3) break
    if (meme_essence(v) %in% meme_essence(pris)) next
    if (length(pris) && max(abs(cor(as.numeric(d_ref[[v]]), sapply(pris, function(u) as.numeric(d_ref[[u]])),
                                    use = "pairwise"))) > 0.8) next
    pris <- c(pris, v)
  }
  data.table(comparaison = k, variable = pris, retenue = TRUE)
}))
hm <- merge(pz[variable %in% retenues$variable], retenues, by = c("comparaison", "variable"), all.x = TRUE)
hm[is.na(retenue), retenue := FALSE]
hm[, `:=`(zone = factor(ZONES_COURTES[comparaison], ZONES_COURTES), signe = sign(ecart_moyenne) * w1,
          nom = ifelse(variable %in% names(NOMS_PROFILS), NOMS_PROFILS[variable], variable),
          famille = factor(c("Relief et position" = "Relief", "Végétation" = "Végétation",
                             "Présence humaine" = "Habitants")[famille], c("Relief", "Végétation", "Habitants")))]
ordre <- hm[, .(m = max(w1)), by = .(nom, famille)][order(famille, -m), nom]
hm[, nom := factor(nom, rev(ordre))]
g <- ggplot(hm, aes(zone, nom)) +
  geom_tile(aes(fill = signe), colour = FOND, linewidth = 1.2) +
  geom_tile(data = hm[retenue == TRUE], fill = NA, colour = TEXTE, linewidth = 0.9, width = 0.94, height = 0.9) +
  geom_text(data = hm[w1 >= 0.5], aes(label = signe(signe, 1), colour = abs(signe) > 1.9), family = POLICE,
            size = TXT * 0.85, show.legend = FALSE) +
  scale_colour_manual(values = c("FALSE" = TEXTE, "TRUE" = "#FFFFFF")) +
  scale_fill_gradient2(low = BLEU, mid = "#EEEBE3", high = ORANGE, midpoint = 0, limits = c(-3, 3),
                       oob = scales::oob_squish, breaks = seq(-3, 3, 1.5), labels = c("−3", "−1,5", "0", "+1,5", "+3"),
                       name = "Écart de la zone au reste (écarts-types) : orange = plus élevé dans la zone, bleu = plus bas") +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_grid(rows = vars(famille), scales = "free_y", space = "free_y", switch = "y") +
  labs(x = NULL, y = NULL) +
  theme_diapo("aucune") +
  theme(legend.position = "bottom", legend.justification = "left", legend.title.position = "top",
        legend.title = element_text(size = PT - 2, colour = TEXTE2), legend.key.width = unit(60, "pt"),
        legend.key.height = unit(10, "pt"), axis.text.x.top = element_text(colour = TEXTE, lineheight = 0.9),
        strip.placement = "outside", strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 0, vjust = 1),
        panel.spacing.y = unit(8, "pt"))
sauver(g, "zones_profils.png", 1500, 900)

# zones_densites. Distributions par région (feux 2006-2018) de deux variables de lieu, parmi celles qui distinguent le
# plus les zones pour le classifieur de 10_zones.R (la taille du plus grand massif, variable de lieu déguisée à quelques
# valeurs, est laissée de côté), et de deux variables de météo : les lieux se séparent, la météo des départs se recouvre.
# Densités à noyau (estimées sur l'échelle de l'axe, entre les 1er et 99e centiles), ramenées à la même hauteur maximale.
REGIONS_ORDRE <- c("Corse", "Provence", "Var-Alpes-Maritimes", "Languedoc", "Roussillon-Aude", "Ardèche-Drôme")
VARS_DENS <- list(
  dist_cote_km = list(titre = "Lieu : distance à la côte", axe = "km (échelle logarithmique)", f = log10,
                      breaks = log10(c(1, 3, 10, 30, 100)), labels = c("1", "3", "10", "30", "100")),
  alt_min_m = list(titre = "Lieu : altitude minimale", axe = "m", f = identity,
                   breaks = seq(0, 800, 200), labels = c("0", "200", "400", "600", "800")),
  humidite_alerte_pct = list(titre = "Météo : humidité de l’air", axe = "%", f = identity,
                             breaks = seq(20, 100, 20), labels = seq(20, 100, 20)),
  rafale_max_alerte_kmh = list(titre = "Météo : rafales de vent", axe = "km/h", f = identity,
                               breaks = seq(0, 100, 25), labels = seq(0, 100, 25)))
dens_feux <- feux[apprentissage == TRUE]
panneau_dens <- function(v, gauche) {
  o <- VARS_DENS[[v]]
  x <- o$f(pmax(dens_feux[[v]], if (identical(o$f, log10)) 0.1 else -Inf))
  lim <- quantile(x, c(0.01, 0.99), na.rm = TRUE)
  d <- rbindlist(lapply(seq_along(REGIONS_ORDRE), function(i) {
    xi <- x[dens_feux$region_test == REGIONS_ORDRE[i]]
    k <- density(xi[!is.na(xi)], from = lim[1], to = lim[2], n = 256)
    data.table(region = REGIONS_ORDRE[i], rang = length(REGIONS_ORDRE) - i + 1, x = k$x, y = k$y / max(k$y) * 0.9)
  }))
  ggplot(d, aes(x, group = region)) +
    geom_ribbon(aes(ymin = rang, ymax = rang + y), fill = alpha(GRIS, 0.75), colour = TEXTE2, linewidth = 0.5) +
    scale_x_continuous(breaks = o$breaks[o$breaks >= lim[1] & o$breaks <= lim[2]],
                       labels = o$labels[o$breaks >= lim[1] & o$breaks <= lim[2]], expand = c(0, 0)) +
    scale_y_continuous(breaks = seq_along(REGIONS_ORDRE), labels = if (gauche) rev(REGIONS_ORDRE) else NULL,
                       limits = c(0.9, length(REGIONS_ORDRE) + 1), expand = c(0, 0)) +
    labs(subtitle = o$titre, x = o$axe, y = NULL) +
    theme_diapo("y") +
    theme(plot.subtitle = element_text(size = PT, colour = TEXTE, face = "bold", hjust = 0), plot.title.position = "plot",
          axis.text.y = element_text(vjust = 0, colour = TEXTE))
}
g <- wrap_plots(lapply(seq_along(VARS_DENS), function(i) panneau_dens(names(VARS_DENS)[i], i == 1)), nrow = 1) + fond_patch
sauver(g, "zones_densites.png", 1700, 640)

# shap_cascade. Deux feux de l'été 2021, l'un échappé, l'autre non, que le modèle complet juge aussi dangereux côté
# météo : de la prédiction moyenne à la prédiction du feu, groupe de variables par groupe (contributions de Shapley de
# 09_shap.R). Choix parmi les feux 2019-2024 : bien prédits (échappé p > 0,6, non échappé p < 0,15), contribution de la
# météo forte (> 0,3) et égale à 0,03 près, calendrier égal à 0,1 près, écart porté par la végétation et la mémoire
# (> 0,3 chacune) ; paire d'écart de prédiction maximal, hors un feu de 1,000 ha tout juste (seuil). Les contributions
# s'additionnent sur l'échelle logit : l'axe est en logit, gradué en probabilités.
CASCADE <- c("2021-7081", "2021-7971")
ETAPES <- c("météo" = "Météo", "calendrier" = "Calendrier", "végétation" = "Végétation",
            "état de la végétation" = "État de la végétation", "relief et position" = "Relief et position",
            "autres" = "Autres variables", "mémoire des lieux" = "Mémoire des lieux")
AUTRES <- c("bilan hydrique", "présence humaine", "historique")   # groupes de faible poids, réunis
MOIS <- c("janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre", "octobre", "novembre",
          "décembre")
sc <- readRDS("donnees/modeles/shap_complet.rds")
cas <- rbindlist(lapply(CASCADE, function(id) {
  i <- match(id, sc$id_feu)
  f <- feux[id_feu == id]
  phi <- sc$phi_grp[i, ]
  contrib <- c(phi[setdiff(names(ETAPES), "autres")], autres = sum(phi[AUTRES]))[names(ETAPES)]
  issue <- if (sc$y[i] == 1) sprintf("s’est échappé : %s ha", virgule(f$surface_ha, 1)) else
    sprintf("est resté sous 1 ha : %d m²", round(f$surface_ha * 1e4))
  titre <- sprintf("%s (%s), %d %s %s à %s h\n%s", f$nom_commune, f$dep, mday(f$date_alerte), MOIS[month(f$date_alerte)],
                   year(f$date_alerte), format(f$date_alerte, "%H"), issue)
  fin <- sc$base[i] + cumsum(contrib)
  stopifnot(abs(fin[length(fin)] - qlogis(sc$p[i])) < 1e-3)        # la cascade arrive bien à la prédiction
  data.table(feu = titre, k = seq_along(contrib), etape = ETAPES, debut = fin - contrib, fin = fin, contrib = contrib,
             base = sc$base[i], p = sc$p[i])
}))
cas[, feu := factor(feu, unique(feu))]
cas[, sens := factor(ifelse(contrib > 0, "pousse vers l’échappement", "retient le feu"),
                     c("pousse vers l’échappement", "retient le feu"))]
bornes <- cas[, .(base = base[1], p = p[1]), by = feu]
NIVEAUX <- c(0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9)
lim <- range(c(cas$debut, cas$fin)) + c(-0.75, 0.75)
g <- ggplot(cas) +
  geom_vline(data = bornes, aes(xintercept = base), colour = GRILLE, linewidth = 0.8) +
  geom_segment(aes(x = fin, xend = fin, y = k + 0.3, yend = k + 0.7), colour = TEXTE2, linewidth = 0.4, linetype = "22") +
  geom_segment(data = bornes, aes(x = base, xend = base, y = 0.3, yend = 0.7), colour = TEXTE2, linewidth = 0.4,
               linetype = "22") +
  geom_rect(aes(xmin = pmin(debut, fin), xmax = pmax(debut, fin), ymin = k - 0.3, ymax = k + 0.3, fill = sens)) +
  geom_text(aes(x = fin, y = k, label = pct(plogis(fin)), hjust = ifelse(contrib > 0, -0.2, 1.2)), family = POLICE,
            size = TXT * 0.85, colour = TEXTE2) +
  geom_point(data = bornes, aes(x = base, y = 0), size = 4, colour = TEXTE) +
  geom_text(data = bornes, aes(x = base, y = 0, label = paste0("feu moyen : ", pct(plogis(base)))), hjust = -0.15,
            family = POLICE, size = TXT * 0.85, colour = TEXTE) +
  geom_point(data = bornes, aes(x = qlogis(p), y = length(ETAPES) + 1), size = 4, colour = TEXTE) +
  geom_text(data = bornes, aes(x = qlogis(p), y = length(ETAPES) + 1, label = paste0("prédiction : ", pct(p)),
                               hjust = ifelse(p > 0.5, 1.15, -0.15)), family = POLICE, size = TXT * 0.85, fontface = "bold",
            colour = TEXTE) +
  scale_fill_manual(values = c("pousse vers l’échappement" = ORANGE, "retient le feu" = BLEU), name = NULL) +
  scale_x_continuous(breaks = qlogis(NIVEAUX[-1]), labels = pct(NIVEAUX[-1]), limits = lim, expand = c(0, 0)) +
  scale_y_reverse(breaks = 0:(length(ETAPES) + 1), labels = c("Départ", ETAPES, "Arrivée")) +
  facet_wrap(~feu, nrow = 1) +
  labs(x = "Probabilité prédite de dépasser 1 ha (échelle logit)", y = NULL) +
  theme_diapo("x") + legende_haut + theme(panel.spacing.x = unit(30, "pt"))
sauver(g, "shap_cascade.png", 1700, 720)

# shap_interaction. L'effet des pins méditerranéens dépend-il du vent ? Modèle sans mémoire (où la végétation compte),
# feux 2019-2024 (contributions et interactions de Shapley de 09_shap.R).
#   À gauche : contribution totale des pins selon leur part dans la commune, par classe de rafales à l'alerte (lissage
#   GAM, 9 185 feux, au-delà du 99e centile des pins hors cadre) ;
#   à droite : valeur d'interaction de Shapley pins x rafales (interaction de la paire = 2 phi_ij), moyenne par classe,
#   sur les 2 000 feux de l'échantillon des interactions.
# Tout en points de probabilité autour du taux moyen (en_points).
CL_RAF <- c("rafales < 30 km/h", "30 à 50 km/h", "rafales > 50 km/h")
CL_PINS <- c("pas de pins\n(< 0,5 %)", "0,5 à 5 %", "plus de 5 %")
COUL_RAF <- setNames(c("#E3AE8C", "#CC7444", ORANGE), CL_RAF)          # du vent faible (clair) au vent fort (foncé)
classe_raf <- function(x) factor(CL_RAF[findInterval(x, c(30, 50)) + 1], CL_RAF)
classe_pins <- function(x) factor(CL_PINS[findInterval(x, c(0.005, 0.05)) + 1], CL_PINS)
sm <- readRDS("donnees/modeles/shap_sans_memoire.rds")
di <- data.table(pins = 100 * sm$valeurs$bdfh_part_pins_med, raf = classe_raf(sm$valeurs$rafale_max_alerte_kmh),
                 points = 100 * en_points(sm$phi_var[, "bdfh_part_pins_med"]))
di <- di[pins <= quantile(pins, 0.99)]
gauche <- ggplot(di, aes(pins, points, colour = raf)) +
  geom_hline(yintercept = 0, colour = TEXTE2, linewidth = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, linewidth = 1.6) +
  scale_colour_manual(values = COUL_RAF, name = NULL) +
  scale_x_continuous(labels = function(x) virgule(x, 0), expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(labels = signe_entier, breaks = seq(-6, 4, 2)) +
  coord_cartesian(ylim = c(-6, 4)) +
  labs(subtitle = "Effet des pins sur la probabilité prédite, selon le vent", x = "Part de pins méditerranéens dans la commune (%)",
       y = "Effet des pins (points de %)") +
  theme_diapo("y") + legende_haut +
  theme(plot.subtitle = element_text(size = PT, colour = TEXTE, face = "bold", hjust = 0), plot.title.position = "plot")
k_pr <- which(sm$paires$vegetation == "bdfh_part_pins_med" & sm$paires$meteo == "rafale_max_alerte_kmh")
de <- data.table(pins = classe_pins(sm$valeurs$bdfh_part_pins_med[sm$ech]),
                 raf = classe_raf(sm$valeurs$rafale_max_alerte_kmh[sm$ech]), inter = sm$val_paires[, k_pr])
de <- de[, .(points = 100 * en_points(mean(inter)), feux = .N), by = .(pins, raf)]
droite <- ggplot(de, aes(raf, pins, fill = points)) +
  geom_tile(colour = FOND, linewidth = 1.5) +
  geom_text(aes(label = signe(points, 1)), family = POLICE, size = TXT, colour = TEXTE) +
  scale_fill_gradient2(low = BLEU, mid = "#EEEBE3", high = ORANGE, midpoint = 0, limits = c(-1.5, 1.5),
                       oob = scales::oob_squish, guide = "none") +
  scale_x_discrete(labels = c("< 30 km/h", "30 à 50", "> 50 km/h"), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(subtitle = "Interaction pins × rafales seule (points de %)", x = "Rafales à l’alerte", y = NULL) +
  theme_diapo("aucune") +
  theme(plot.subtitle = element_text(size = PT, colour = TEXTE, face = "bold", hjust = 0), plot.title.position = "plot")
sauver(gauche + droite + plot_layout(widths = c(1.5, 1)) + fond_patch, "shap_interaction.png", 1700, 640)

# zones_transfert. Distributions et transfert (12_transfert_zones.R).
#   À gauche : par région, séparabilité météo (AUC du classifieur discriminant, météo seule ; famille choisie avant de
#   voir le résultat, l'AUC à toutes les variables valant presque 1 partout) contre AUC du test « région jamais vue ».
#   À droite : écart d'AUC sur les feux du nord 2019-2024 par rapport au modèle appris au sud sans poids (bootstrap
#   apparié, 95 %) : apprendre sur place (06), ou au sud en repondérant les feux par le rapport de densités du
#   classifieur discriminant ; taille d'échantillon effective dans l'étiquette.
tr <- tab("transfert_regions")
sp <- cor(tr$separabilite_Météo, tr$auc_region_jamais_vue, method = "spearman")
gauche <- ggplot(tr, aes(separabilite_Météo, auc_region_jamais_vue)) +
  geom_point(size = 5, colour = BLEU) +
  geom_text_repel(aes(label = region), family = POLICE, size = TXT * 0.9, colour = TEXTE, point.padding = 8,
                  min.segment.length = Inf, seed = 1) +
  annotate("text", x = 0.712, y = 0.654, label = sprintf("Spearman %s (6 régions)", signe(sp, 2)), hjust = 0,
           family = POLICE, size = TXT * 0.9, colour = TEXTE2) +
  scale_x_continuous(limits = c(0.71, 0.85), breaks = seq(0.72, 0.84, 0.04), labels = function(x) virgule(x, 2)) +
  scale_y_continuous(limits = c(0.65, 0.74), breaks = seq(0.66, 0.74, 0.02), labels = function(x) virgule(x, 2)) +
  labs(subtitle = "Plus une région se distingue par sa météo,\nmoins bien elle est prédite depuis les autres",
       x = "Séparabilité météo de la région (AUC du classifieur)", y = "AUC « région jamais vue »") +
  theme_diapo("xy") +
  theme(plot.subtitle = element_text(size = PT, colour = TEXTE, face = "bold", hjust = 0), plot.title.position = "plot")
rp <- tab("transfert_repondere")[variante != "Sans repondération (06)"]
gn <- tab("sud_nord_gains")[question == "apprendre au nord plutôt qu'au sud"]
ec <- rbind(data.table(nom = "Apprendre au nord, sur place", gain = gn$gain, bas = gn$bas, haut = gn$haut),
            rp[, .(nom = sprintf("%s\n(taille effective : %s feux)",
                                 fifelse(grepl("météo", variante), "Repondérer par la météo seule",
                                         "Repondérer par toutes les variables"),
                                 formatC(round(taille_effective), big.mark = " ", format = "d")),
                   gain, bas, haut)])
ec[, y := rev(seq_len(.N))]
droite <- ggplot(ec, aes(y = y)) +
  geom_vline(xintercept = 0, colour = TEXTE2, linewidth = 0.7) +
  geom_rect(aes(xmin = 0, xmax = gain, ymin = y - 0.18, ymax = y + 0.18), fill = BLEU) +
  geom_errorbar(aes(xmin = bas, xmax = haut), width = 0.14, linewidth = 0.7, colour = TEXTE2, orientation = "y") +
  geom_text(aes(x = ifelse(gain > 0, haut, bas), label = signe(gain), hjust = ifelse(gain > 0, -0.25, 1.25)),
            family = POLICE, size = TXT, colour = TEXTE) +
  scale_x_continuous(limits = c(-0.085, 0.045), breaks = seq(-0.08, 0.04, 0.04), labels = function(x) signe(x, 2)) +
  scale_y_continuous(breaks = ec$y, labels = ec$nom, limits = c(0.5, nrow(ec) + 0.5)) +
  labs(subtitle = "Corriger le décalage vers le nord n’aide pas", y = NULL,
       x = "Écart d’AUC sur le nord 2019-2024, par rapport\nau modèle appris au sud sans poids (0,723)") +
  theme_diapo("x") +
  theme(plot.subtitle = element_text(size = PT, colour = TEXTE, face = "bold", hjust = 0), plot.title.position = "plot")
sauver(gauche + droite + plot_layout(widths = c(1, 1.15)) + fond_patch, "zones_transfert.png", 1700, 640)
