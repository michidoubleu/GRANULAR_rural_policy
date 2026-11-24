# ============================================================================
# Shared colour scheme for all paper figures (sourced by every figure script).
#
# Every figure must read in colour AND when printed in black and white, so each
# encoding is carried by lightness (and, where marks overlap, by line type or
# hatching), not by hue alone.
#
#   Categorical (identity)  urban-rural typology, Figs 1, 2, 5
#                           dark navy / mid orange / light green: CIELAB
#                           lightness 27 / 62 / 84, so the three classes print
#                           as clearly different greys; colour-blind safe for all
#                           pairs (worst CVD dE 21, deutan)
#                           + line types in Fig 2 (solid / dashed / dotted)
#   Sequential (magnitude)  policy funding, Fig 4: one hue (blue), light -> dark
#   Diverging (polarity)    population growth, Fig 3: dark red (decline) -
#                           mid grey (no change) - light blue (growth); lightness
#                           rises monotonically, so in grey darker = decline
#   Excluded regions        white with grey hatching (hatch_lines())
#   Single series           trace plots: navy
# ============================================================================

TYPO_LEVELS <- c("Urban", "Intermediate", "Rural")
TYPO_COLS <- c(Urban = "#1d3f6e", Intermediate = "#e07b39", Rural = "#b7dd8c")
# darker versions of the same hues for lines and outlines (visible on white)
TYPO_LINE <- c(Urban = "#1d3f6e", Intermediate = "#b3561a", Rural = "#4f7f22")
TYPO_LTY  <- c(Urban = "solid", Intermediate = "dashed", Rural = "dotted")

# Map raw typology labels ("predominantly urban", "intermediate", ...) to the
# fixed display order used in every figure.
typo_factor <- function(typ) {
  typ <- as.character(typ)
  out <- ifelse(grepl("urban", typ, ignore.case = TRUE), "Urban",
         ifelse(grepl("rural", typ, ignore.case = TRUE), "Rural",
         ifelse(grepl("intermediate", typ, ignore.case = TRUE), "Intermediate", typ)))
  factor(out, levels = TYPO_LEVELS)
}

# Sequential blue ramp (light = low, dark = high); the lightest step is kept
# clearly darker than the background countries (BG_FILL) so that low values
# and areas outside the study region stay distinct in grey.
SEQ_RAMP <- c("#b4d2f6", "#8dbbf0", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")

# Diverging ramp, monotone in lightness: decline (dark) -> no change -> growth (light)
DIV_RAMP <- c("#3d0606", "#7d1b1b", "#a8403a", "#8a8a8a", "#8fc3e8", "#c6e2f6", "#eef7fd")
# break points of DIV_RAMP (most regions lie within +-0.1, so the steps are
# concentrated there; lightness 10, 27, 42, 57, 76, 88, 97)
DIV_BREAKS <- c(-0.10, -0.04, 0, 0.04, 0.10)

# Countries outside the study area, and region borders on maps
BG_FILL     <- "grey96"
BG_BORDER   <- "grey70"
BORDER_GREY <- "grey55"

SERIES_COL <- "#1d3f6e"

# Diagonal hatching for excluded regions: returns the hatch lines clipped to
# the union of the given polygons (projected CRS; spacing in map units).
hatch_lines <- function(polys, spacing = 50000) {
  u  <- sf::st_union(sf::st_make_valid(sf::st_geometry(polys)))
  bb <- sf::st_bbox(u)
  w  <- unname(bb["xmax"] - bb["xmin"]); h <- unname(bb["ymax"] - bb["ymin"])
  x0 <- unname(bb["xmin"]); y0 <- unname(bb["ymin"])
  lines <- lapply(seq(-h, w, by = spacing), function(o)
    sf::st_linestring(rbind(c(x0 + o, y0), c(x0 + o + h, y0 + h))))
  sf::st_intersection(sf::st_sfc(lines, crs = sf::st_crs(u)), u)
}
