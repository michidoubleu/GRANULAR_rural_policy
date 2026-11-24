# ============================================================
# Figure 3: Spatial distribution of population growth
#
# This script reproduces Figure 3 of the manuscript.
# It generates a three-panel map showing:
#   (A) All regions
#   (B) Excluding predominantly urban regions
#   (C) Only predominantly rural regions
#
# The colour scale is shared by all panels and rises monotonically in
# lightness (dark red = decline, mid grey = no change, light blue = growth),
# so it also reads correctly when printed in black and white. Regions
# excluded from a panel are white with grey hatching.
#
# Input:
#   - inputs/popdata_prepared.rds
#   - inputs/final_NUTS.rds
#
# Output:
#   - plots/Figure3_growth_corr_maps.png
#
# Requirements:
#   R >= 4.2.0
#   Packages: ggplot2, sf, dplyr, patchwork
# ============================================================

# ---- Load required libraries ----
library(ggplot2)   # plotting
library(sf)        # spatial data
library(dplyr)     # data manipulation
library(patchwork) # plot composition
source("Code/figure_palette.R")

# ---- Load prepared population data ----
pop <- readRDS("inputs/popdata_prepared.rds")

# ---- Load and filter NUTS3 shapefile ----
nuts <- readRDS("inputs/final_NUTS.rds") %>%
  filter(NUTS_ID %in% unique(pop$NUTS_ID))

# ---- Select variable to be plotted ----
plot.var <- "growth_corr"

# ---- Merge spatial and attribute data ----
map_data <- nuts %>%
  left_join(pop, by = "NUTS_ID") %>%
  filter(variable == plot.var)

# ---- Common colour scale (zero fixed at the mid-grey step) ----
lim <- range(map_data$value, na.rm = TRUE)
div_values <- scales::rescale(c(lim[1], DIV_BREAKS, lim[2]), from = lim)
fill_scale <- scale_fill_gradientn(
  colours = DIV_RAMP,
  values = div_values,
  limits = lim,
  na.value = "white",
  name = NULL,
  guide = guide_colorbar(
    barwidth = 16,
    barheight = 0.6,
    ticks = FALSE,
    frame.colour = "black",
    frame.linewidth = 0.5
  )
)

# ---- One panel: regions not kept are blanked and hatched ----
growth_panel <- function(keep, subtitle) {
  d <- map_data %>% mutate(value = ifelse(keep, value, NA))
  p <- ggplot(d) +
    geom_sf(aes(fill = value), color = BORDER_GREY, linewidth = 0.1)
  if (any(is.na(d$value))) {
    p <- p + geom_sf(data = hatch_lines(d[is.na(d$value), ], spacing = 40000),
                     colour = "grey45", linewidth = 0.15)
  }
  p + fill_scale +
    labs(subtitle = subtitle) +
    theme_minimal()
}

A <- growth_panel(rep(TRUE, nrow(map_data)), "(A) All regions")
B <- growth_panel(map_data$typ != "predominantly urban", "(B) Without predominantly urban")
C <- growth_panel(map_data$typ == "predominantly rural", "(C) Only predominantly rural")

# ---- Combine panels into one figure, one shared legend ----
combined_plot <- (A + B + C) +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

# ---- Save figure to disk ----
ggsave(
  filename = "plots/Figure3_growth_corr_maps.png",
  plot = combined_plot,
  width = 30,
  height = 15,
  units = "cm",
  dpi = 300,
  bg = "white"
)
