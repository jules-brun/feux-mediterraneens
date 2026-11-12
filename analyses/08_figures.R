# Figures du diaporama. Une figure = un message : pas de titre dans l'image (il est sur la diapositive), étiquettes
# directes plutôt que légendes, deux couleurs au plus ; chaque figure est dessinée à sa taille d'affichage sur une
# diapositive de 1920 x 1080 px, à résolution double (charte : theme.R).
# Entrées : jeu de données (cartes, exemple de distance) et tables de resultats/tables (scripts 01 à 07).
# Sorties : resultats/figures/p*.png

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
