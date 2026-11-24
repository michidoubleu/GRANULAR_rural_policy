# ============================================================
# Figure 2: Density of population change by regional typology
#
# This script reproduces Figure 2 of the manuscript.
# It visualizes the distribution of population change
# (births & deaths corrected) by urban–rural typology.
#
# Input:
#   - output/popdata_prepared.rds
#
# Output:
#   - plots/Figure2_relative_density_corr.png
#
# Requirements:
#   R >= 4.2.0
#   Packages: ggplot2, dplyr
# ============================================================

# ---- Load required libraries ----
library(ggplot2)  # plotting
library(dplyr)    # data manipulation

# ---- Load prepared population data ----
source("Code/figure_palette.R")
pop <- readRDS("inputs/popdata_prepared.rds") %>%
  mutate(Typology = typo_factor(typ))


# ---- Compute mean population change per typology ----
means <- pop %>%
  filter(variable == "growth_corr") %>%
  group_by(Typology) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))

# ---- Create density plot ----
p <- ggplot(
  pop %>% filter(variable == "growth_corr"),
  aes(x = value, fill = Typology, color = Typology, linetype = Typology)
) +
  # typology is encoded by fill lightness, outline colour AND line type, so the
  # distributions and their means stay distinguishable in black and white
  geom_density(alpha = 0.45, linewidth = 0.8) +
  geom_vline(
    data = means,
    aes(xintercept = mean_value, color = Typology, linetype = Typology),
    linewidth = 1,
    show.legend = FALSE
  ) +
  geom_vline(
    xintercept = 0,
    color = "black",
    linewidth = 0.5
  ) +
  scale_x_continuous(
    "Population change (births & deaths corrected), 2011–2021 (share of 2011 population)",
    limits = c(-0.3, 0.3)
  ) +
  scale_y_continuous("Density") +
  scale_fill_manual(values = TYPO_COLS) +
  scale_color_manual(values = TYPO_LINE) +
  scale_linetype_manual(values = TYPO_LTY) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(1.4, "cm")
  )

# ---- Save figure to disk ----
ggsave(
  filename = "plots/Figure2_relative_density_corr.png",
  plot = p,
  width = 20,
  height = 12,
  units = "cm",
  dpi = 300
)
