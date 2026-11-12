# Charte graphique des figures du diaporama : police, couleurs, thème ggplot2, sauvegarde, fond de carte
#
# Sourcé par 08_figures.R. Police STIX Two Text (celle du texte des diapositives, licence OFL), téléchargée une fois
# depuis Google Fonts dans donnees/polices ; contours des départements (IGN Admin Express 2024 simplifié par Etalab),
# téléchargés une fois dans donnees/carte.

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(sf)
  library(systemfonts)
})
SORTIE <- "resultats/figures"
dir.create(SORTIE, recursive = TRUE, showWarnings = FALSE)

# Police ---------------------------------------------------------------------------------------------------
POLICES <- "donnees/polices"
fichiers <- file.path(POLICES, c(regular = "STIXTwoText-Regular.ttf", semibold = "STIXTwoText-SemiBold.ttf",
                                 italic = "STIXTwoText-Italic.ttf"))
if (!all(file.exists(fichiers))) {
  dir.create(POLICES, recursive = TRUE, showWarnings = FALSE)
  css <- readLines(url("https://fonts.googleapis.com/css2?family=STIX+Two+Text:ital,wght@0,400;0,600;1,400"), warn = FALSE)
  for (b in split(css, cumsum(grepl("@font-face", css)))) {
    src <- regmatches(b, regexpr("https://[^)]+\\.ttf", b))
    if (!length(src)) next
    i <- if (any(grepl("italic", b))) 3 else if (any(grepl("font-weight: 600", b))) 2 else 1
    download.file(src, fichiers[i], mode = "wb", quiet = TRUE)
  }
}
register_font("Stix", plain = fichiers[1], bold = fichiers[2], italic = fichiers[3], bolditalic = fichiers[2])
POLICE <- "Stix"

# Couleurs et tailles --------------------------------------------------------------------------------------
FOND <- "#FAF9F6"
TEXTE <- "#1F2933"
TEXTE2 <- "#52606D"
GRILLE <- "#E4E2DA"
BLEU <- "#2A6F97"
ORANGE <- "#B4440E"
GRIS <- "#A9AFB6"
PT <- 18          # taille de base en points (environ 24 px sur une diapositive de 1920 x 1080)
TXT <- PT / .pt   # même taille pour geom_text (en mm)

virgule <- function(x, n = 2) formatC(x, format = "f", digits = n, decimal.mark = ",")
pct <- function(x, n = 0) paste0(virgule(100 * x, n), " %")
# Écart signé à n décimales, sans « −0,000 » quand l'écart arrondi est nul
signe <- function(g, n = 3) {
  r <- round(g, n)
  ifelse(r == 0, virgule(0, n), paste0(ifelse(r > 0, "+", "−"), virgule(abs(r), n)))
}

theme_diapo <- function(grille = "x") {
  t <- theme_minimal(base_size = PT, base_family = POLICE) +
    theme(plot.background = element_rect(fill = FOND, colour = NA),
          panel.background = element_rect(fill = FOND, colour = NA),
          text = element_text(colour = TEXTE),
          axis.text = element_text(colour = TEXTE2, size = PT),
          axis.title = element_text(colour = TEXTE2, size = PT),
          strip.text = element_text(colour = TEXTE, size = PT + 1, face = "bold", hjust = 0),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = GRILLE, linewidth = 0.6),
          legend.position = "none",
          legend.text = element_text(size = PT, colour = TEXTE),
          plot.margin = margin(12, 24, 12, 12))
  if (grille == "x") t <- t + theme(panel.grid.major.y = element_blank())
  if (grille == "y") t <- t + theme(panel.grid.major.x = element_blank())
  if (grille == "aucune") t <- t + theme(panel.grid.major = element_blank())
  t
}
legende_haut <- theme(legend.position = "top", legend.justification = "left", legend.location = "plot")
theme_carte <- theme_void(base_family = POLICE) +
  theme(legend.position = "none", plot.background = element_rect(fill = FOND, colour = NA))
fond_patch <- plot_annotation(theme = theme(plot.background = element_rect(fill = FOND, colour = NA)))

# Sauvegarde à la taille d'affichage sur la diapositive (en px), résolution double
sauver <- function(g, nom, l_px, h_px) {
  ggsave(file.path(SORTIE, nom), g, width = l_px / 96, height = h_px / 96, dpi = 192, device = ragg::agg_png, bg = FOND)
  cat("  ", nom, "\n")
}

# Fond de carte : départements de France métropolitaine, en Lambert-93 -----------------------------------------
charger_departements <- function() {
  f <- "donnees/carte/departements-50m.geojson.gz"
  if (!file.exists(f)) {
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    download.file("https://etalab-datasets.geo.data.gouv.fr/contours-administratifs/2024/geojson/departements-50m.geojson.gz",
                  f, mode = "wb", quiet = TRUE)
  }
  dep <- st_read(paste0("/vsigzip/", f), quiet = TRUE)
  dep <- st_transform(dep[!grepl("^97", dep$code), c("code")], 2154)
  names(dep)[1] <- "dep"
  st_simplify(st_make_valid(dep), dTolerance = 300, preserveTopology = TRUE)
}
CADRE_SUD <- c(xmin = 560000, xmax = 1250000, ymin = 6020000, ymax = 6520000)   # arc méditerranéen (Lambert-93)
