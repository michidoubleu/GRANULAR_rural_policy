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
pop <- readRDS("output/popdata_prepared.rds")

# ---- Compute mean population change per typology ----
means <- pop %>%
  filter(variable == "growth_corr") %>%
  group_by(typ) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))

# ---- Create density plot ----
p <- ggplot(
  pop %>% filter(variable == "growth_corr"),
  aes(x = value, fill = typ, color = typ)
) +
  geom_density(alpha = 0.5) +
  geom_vline(
    data = means,
    aes(xintercept = mean_value, color = typ),
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = 0,
    color = "black",
    linewidth = 0.7
  ) +
  scale_x_continuous(
    "Population change (births & deaths corrected), 2011–2021 (%)",
    limits = c(-0.3, 0.3)
  ) +
  scale_y_continuous("Density") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
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
