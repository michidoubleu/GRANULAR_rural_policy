# ============================================================
# Figure 1: NUTS3 regions classified by urban–rural typology
#
# This script reproduces Figure 1 of the manuscript.
# It generates a spatial map of NUTS3 regions classified
# by urban–rural typology for the selected variable.
#
# Input:
#   - output/popdata_prepared.rds
#   - input/NUTS_RG_20M_2016_3035.shp
#
# Output:
#   - plots/Figure1_typology.png
#
# Requirements:
#   R >= 4.2.0
#   Packages: ggplot2, sf, dplyr
# ============================================================

# ---- Load required libraries ----
library(ggplot2)  # plotting
library(sf)       # spatial data handling
library(dplyr)    # data manipulation
source("Code/figure_palette.R")


# ---- Load prepared population data ----
pop <- readRDS("inputs/popdata_prepared.rds")

# ---- Load NUTS3 shapefile and keep relevant regions only ----
nuts <- readRDS("inputs/final_NUTS.rds") %>%
  filter(NUTS_ID %in% unique(pop$NUTS_ID))

# ---- Select variable to be plotted ----
plot.var <- "growth_corr"

# ---- Merge spatial and attribute data ----
map_data <- nuts %>%
  left_join(pop, by = "NUTS_ID") %>%
  mutate(Typology = typo_factor(typ))

# ---- Create the map ----
p <- ggplot(map_data %>% filter(variable == plot.var)) +
  geom_sf(aes(fill = Typology), color = BORDER_GREY, size = 0.2) +
  scale_fill_manual(values = TYPO_COLS, name = "Urban–rural typology") +
  labs(caption = "Source: European Commission") +
  theme_minimal() +
  theme(legend.position = "right")

# ---- Save figure ----
ggsave(
  filename = "plots/Figure1_typology.png",
  plot = p,
  width = 14,
  height = 14,
  units = "cm",
  dpi = 300
)
