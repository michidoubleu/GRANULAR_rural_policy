# ============================================================
# Figure 3: Spatial distribution of population growth
#
# This script reproduces Figure 3 of the manuscript.
# It generates a three-panel map showing:
#   (A) All regions
#   (B) Excluding predominantly urban regions
#   (C) Only predominantly rural regions
#
# Input:
#   - output/popdata_prepared.rds
#   - input/NUTS_RG_20M_2016_3035.shp
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

# ---- Load prepared population data ----
pop <- readRDS("output/popdata_prepared.rds")

# ---- Load and filter NUTS3 shapefile ----
nuts <- st_read("input/NUTS_RG_20M_2016_3035.shp", quiet = TRUE) %>%
  filter(NUTS_ID %in% unique(pop$NUTS_ID))

# ---- Select variable to be plotted ----
plot.var <- "growth_corr"

# ---- Merge spatial and attribute data ----
map_data <- nuts %>%
  left_join(pop, by = "NUTS_ID")

# ---- Define common color scale ----
fill_scale <- scale_fill_gradient2(
  low = "#D73027",
  mid = "#FDFDFD",
  high = "#1A9850",
  midpoint = 0,
  na.value = "grey",
  name = "Growth"
)

# ---- Define common legend formatting ----
legend_guide <- guides(
  fill = guide_colorbar(
    title = NULL,
    barwidth = 10,
    barheight = 0.5,
    ticks = FALSE,
    frame.colour = "black",
    frame.linewidth = 0.5
  )
)

# ---- Panel A: All regions ----
A <- ggplot(map_data %>% filter(variable == plot.var)) +
  geom_sf(aes(fill = value), color = "darkgrey", size = 0.2) +
  fill_scale +
  labs(subtitle = "(A) All regions") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  legend_guide

# ---- Panel B: Excluding predominantly urban regions ----
B <- ggplot(
  map_data %>%
    filter(variable == plot.var) %>%
    mutate(value = ifelse(typ == "predominantly urban", NA, value))
) +
  geom_sf(aes(fill = value), color = "darkgrey", size = 0.2) +
  fill_scale +
  labs(subtitle = "(B) Without predominantly urban") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  legend_guide

# ---- Panel C: Only predominantly rural regions ----
C <- ggplot(
  map_data %>%
    filter(variable == plot.var) %>%
    mutate(value = ifelse(typ != "predominantly rural", NA, value))
) +
  geom_sf(aes(fill = value), color = "darkgrey", size = 0.2) +
  fill_scale +
  labs(subtitle = "(C) Only predominantly rural") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  legend_guide

# ---- Combine panels into one figure ----
combined_plot <- (A + B + C) +
  plot_layout(ncol = 3)

# ---- Save figure to disk ----
ggsave(
  filename = "plots/Figure3_growth_corr_maps.png",
  plot = combined_plot,
  width = 30,
  height = 18,
  units = "cm",
  dpi = 300
)
