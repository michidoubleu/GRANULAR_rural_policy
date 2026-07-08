# codes/generate_descriptives.R

library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(knitr)
library(kableExtra)
library(patchwork) # For combining plots
library(viridis)

# -------------------------------------------------------------------------
# 1. Define Policy Variables of Interest
# -------------------------------------------------------------------------
# Adjust these names to match exact column names in your 'df' if different
policy_vars <- c("Pillar I", "Pillar II env", "Pillar II dev",
                 "ESIF", "ESIF_env") # Or "ESIF_infra", "ESIF_env" check your col names

# Readable labels for plots
policy_labels <- c(
  "Pillar I" = "CAP Pillar I (Direct Pay)",
  "Pillar II env" = "CAP Pillar II (Env)",
  "Pillar II dev" = "CAP Pillar II (Dev)",
  "ESIF" = "ESIF (Infrastructure)",
  "ESIF_env" = "ESIF (Environment)"
)

# -------------------------------------------------------------------------
# 2. Generate Descriptive Table by Typology
# -------------------------------------------------------------------------
desc_table <- df %>%
  st_drop_geometry() %>% # 1. Drop the geometry column first
  dplyr::select(typ, all_of(policy_vars), growth_corr) %>%
  pivot_longer(cols = -typ, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, typ) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = typ,
    values_from = c(Mean, SD),
    names_glue = "{typ}_{.value}"
  )

# Print LaTeX code for the paper
kbl(desc_table, format = "latex", booktabs = T, digits = 3,
    caption = "Descriptive Statistics of Policy Intensities by Regional Typology") %>%
  kable_styling(latex_options = "scale_down") %>%
  cat(., file = "tables/descriptive_stats.tex")

print("Table saved to tables/descriptive_stats.tex")

# -------------------------------------------------------------------------
# 3. Generate Spatial Map of Policy Intensities
# -------------------------------------------------------------------------
# Load spatial data if not already joined (assuming 'final_NUTS.rds' has geometry)
# If 'df' is not an sf object, join it with the shapefile
if(!"sf" %in% class(df)){
  nuts_sf <- readRDS("inputs/final_NUTS.rds") # Check path
  plot_df <- left_join(nuts_sf %>% dplyr::select(NUTS_ID, geometry), df, by = "NUTS_ID")
} else {
  plot_df <- df
}

# Transform to Long format for Faceted Mapping
map_data <- plot_df %>%
  dplyr::select(NUTS_ID, geometry, all_of(policy_vars)) %>%
  pivot_longer(cols = all_of(policy_vars), names_to = "Policy", values_to = "Intensity") %>%
  mutate(Policy = recode(Policy, !!!policy_labels))

# Create the Map
p_map <- ggplot(map_data) +
  geom_sf(aes(fill = Intensity), color = NA, size = 0.01) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Log Intensity\n(Eur/GDP)") +
  facet_wrap(~Policy, ncol = 3) +
  theme_void() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(title = "Spatial Distribution of EU Policy Funding Intensities")

# Save
ggsave("figures/policy_distribution_map.png", plot = p_map, width = 12, height = 8, dpi = 300)
print("Map saved to figures/policy_distribution_map.png")

# -------------------------------------------------------------------------
# 4. Boxplot for Distribution Heterogeneity (Optional but good)
# -------------------------------------------------------------------------
p_box <- df %>%
  sf::st_drop_geometry() %>% # 1. Drop the geometry column first
  dplyr::select(typ, all_of(policy_vars)) %>%
  pivot_longer(cols = -typ, names_to = "Policy", values_to = "Intensity") %>%
  mutate(Policy = recode(Policy, !!!policy_labels)) %>%
  ggplot(aes(x = typ, y = Intensity, fill = typ)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  facet_wrap(~Policy, scales = "free_y", ncol = 5) +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  labs(y = "Log Funding Intensity", x = NULL, title = "Policy Intensity by Rural-Urban Typology")

ggsave("figures/policy_boxplot.png", plot = p_box, width = 12, height = 4)
