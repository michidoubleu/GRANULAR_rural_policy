# ============================================================================
# Shared colour scheme for all paper figures (sourced by every figure script).
#
#   Categorical (identity)  urban-rural typology, Figs 1, 2, 5
#                           validated colour-blind safe for all pairs
#                           (worst CVD dE 9.2, normal-vision dE 24.0)
#   Sequential (magnitude)  policy funding, Fig 4: one hue (blue), light -> dark
#   Diverging (polarity)    population growth, Fig 3: red (decline) - grey - blue (growth)
#   Single series           trace plots: slot-1 blue
# ============================================================================

TYPO_LEVELS <- c("Urban", "Intermediate", "Rural")
TYPO_COLS <- c(Urban = "#2a78d6", Intermediate = "#eb6834", Rural = "#1baf7a")

# Map raw typology labels ("predominantly urban", "intermediate", ...) to the
# fixed display order used in every figure.
typo_factor <- function(typ) {
  typ <- as.character(typ)
  out <- ifelse(grepl("urban", typ, ignore.case = TRUE), "Urban",
         ifelse(grepl("rural", typ, ignore.case = TRUE), "Rural",
         ifelse(grepl("intermediate", typ, ignore.case = TRUE), "Intermediate", typ)))
  factor(out, levels = TYPO_LEVELS)
}

# Sequential blue ramp (light = low, dark = high)
SEQ_RAMP <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")

# Diverging: red arm / neutral grey midpoint / blue arm
DIV_LOW  <- "#b3312c"
DIV_MID  <- "#f0efec"
DIV_HIGH <- "#184f95"

# Excluded / missing regions and region borders on maps
NA_GREY     <- "grey70"
BORDER_GREY <- "grey55"

SERIES_COL <- "#2a78d6"
