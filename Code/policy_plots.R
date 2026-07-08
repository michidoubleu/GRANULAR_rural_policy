# ==============================================================================
# Policy Funding Intensity Plots
# Modeled on income_plots.R — produces publication-quality maps and boxplots
# of EU policy funding intensities (CAP Pillar I/II, ESIF) across NUTS 3 regions
# ==============================================================================


# ==============================================================================
# 1. USER SETTINGS
# ==============================================================================

# -- File Paths --
path_nuts0_shp   <- "./data/geo/NUTS_RG_20M_2016_3035/NUTS_RG_20M_2016_3035.shp"
path_world_shp   <- "./data/geo/ref-countries-2020-20M/CNTR_RG_20M_2020_4326.shp"
path_nuts_custom <- "./inputs/final_NUTS.rds"
path_predictors  <- "./inputs/predictors.rds"
path_popdata     <- "./inputs/popdata_prepared.rds"
output_dir       <- "./GRANULAR_rural_paper_MW/figures/"

# -- Map Bounding Box (European extent, EPSG:3035) --
map_extent <- c(xmin = 2700000, ymin = 1500000, xmax = 5900000, ymax = 5280000)

# -- Plot Output Settings --
plot_dpi    <- 300
map_height  <- 8
map_width   <- 14
box_height  <- 5
box_width   <- 14


# ==============================================================================
# 2. LIBRARIES & DATA LOADING
# ==============================================================================

library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(scales)
library(viridis)

# Load Base Shapefiles
nuts0_shp <- st_read(path_nuts0_shp, quiet = TRUE) %>%
  filter(LEVL_CODE == 0)

world <- st_read(path_world_shp, quiet = TRUE) %>%
  st_transform(crs = st_crs(nuts0_shp))

# Load NUTS 3 shapefile (project-specific, already filtered)
nuts_shp <- readRDS(path_nuts_custom)

# Load predictor data (raw, before log-transformation)
predictors <- readRDS(path_predictors)

# Load population data (for GDP per capita normalization and typology)
pop <- readRDS(path_popdata)
pop_wide <- pop %>%
  filter(variable == "pop2011") %>%
  dplyr::select(NUTS_ID, typ, value) %>%
  rename(pop2011 = value)


# ==============================================================================
# 3. DATA PREPARATION
# ==============================================================================

prepare_policy_data <- function(predictors, pop_info, shape_data) {

  # Construct policy aggregates (mirroring prepare_data.R)
  df <- predictors %>%
    left_join(pop_info, by = "NUTS_ID") %>%
    mutate(
      # CAP Pillar I: coupled + decoupled + market
      `CAP Pillar I` = COUP + DECOUP + MARKET,

      # CAP Pillar II Environmental: agri-env + Natura 2000 + LFA
      `CAP Pillar II (Env)` = pmax(AEP + N2K + LFA, 0),

      # CAP Pillar II Development: human capital + cooperation + diversification etc.
      `CAP Pillar II (Dev)` = pmax(HC + COOP + PC + GRD + FOR + EARLY + TA + DIV, 0),

      # ESIF Environment: Natura 2000 + risk prevention + brownfield
      `ESIF (Environment)` = EnvN2K + RiskPrev + Brownfield,

      # ESIF Construction/Infrastructure: transport + building + energy
      `ESIF (Infrastructure)` = TransConstr + BuildConstr + EnergyConstr
    ) %>%
    # Normalize by GDP per capita and log-transform (matching paper methodology)
    mutate(
      across(
        c(`CAP Pillar I`, `CAP Pillar II (Env)`, `CAP Pillar II (Dev)`,
          `ESIF (Environment)`, `ESIF (Infrastructure)`),
        ~ log(.x + 1),
        .names = "{.col}"
      )
    )

  # Policy variable columns
  policy_vars <- c("CAP Pillar I", "CAP Pillar II (Env)", "CAP Pillar II (Dev)",
                   "ESIF (Environment)", "ESIF (Infrastructure)")

  # Pivot to long format for faceted plotting
  policy_long <- df %>%
    dplyr::select(NUTS_ID, typ, all_of(policy_vars)) %>%
    pivot_longer(
      cols = all_of(policy_vars),
      names_to = "Policy",
      values_to = "Intensity"
    )

  # Fix factor ordering for display
  policy_long$Policy <- factor(policy_long$Policy, levels = policy_vars)

  # Join with geometry
  map_data <- shape_data %>%
    dplyr::select(NUTS_ID, geometry) %>%
    left_join(policy_long, by = "NUTS_ID") %>%
    filter(!is.na(Policy))

  list(
    map_data = map_data,
    box_data = policy_long,
    policy_vars = policy_vars
  )
}


# ==============================================================================
# 4. PLOT FUNCTIONS
# ==============================================================================

plot_policy_map <- function(spatial_data, world_bg, nuts0, extent) {

  ggplot() +
    # World background
    geom_sf(data = world_bg, fill = "lightgrey", colour = NA) +

    # NUTS 3 policy intensity
    geom_sf(
      data = spatial_data,
      aes(fill = Intensity),
      colour = "grey40",
      lwd = 0.02
    ) +

    # Country borders
    geom_sf(
      data = nuts0,
      fill = NA,
      colour = "black",
      lwd = 0.4
    ) +

    # Bounding box
    coord_sf(
      xlim = c(extent["xmin"], extent["xmax"]),
      ylim = c(extent["ymin"], extent["ymax"])
    ) +

    # Facet by policy category
    facet_wrap(~Policy, ncol = 3) +

    # Color scale: sequential (all values are positive intensities)
    scale_fill_viridis_c(
      option = "magma",
      direction = -1,
      name = expression(atop("Log Funding", "Intensity")),
      labels = scales::label_number(accuracy = 0.1)
    ) +

    # Theme
    theme_bw() +
    ggthemes::theme_map() +

    guides(
      fill = guide_colourbar(
        barwidth = 20,
        barheight = 1
      )
    ) +

    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10, face = "bold"),
      strip.text = element_text(face = "bold", size = 12),
      strip.background = element_rect(fill = "white", colour = NA),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white", colour = NA)
    ) +

    labs(title = "Spatial Distribution of EU Policy Funding Intensities")
}


plot_policy_boxplot <- function(box_data) {

  # Clean typology labels
  box_data <- box_data %>%
    mutate(
      Typology = case_when(
        grepl("urban", typ, ignore.case = TRUE) ~ "Urban",
        grepl("intermediate", typ, ignore.case = TRUE) ~ "Intermediate",
        grepl("rural", typ, ignore.case = TRUE) ~ "Rural",
        TRUE ~ as.character(typ)
      ),
      Typology = factor(Typology, levels = c("Urban", "Intermediate", "Rural"))
    )

  ggplot(box_data, aes(x = Typology, y = Intensity, fill = Typology)) +
    geom_boxplot(
      outlier.size = 0.5,
      outlier.alpha = 0.4,
      alpha = 0.85,
      width = 0.7
    ) +
    facet_wrap(~Policy, scales = "free_y", ncol = 5) +
    scale_fill_manual(
      values = c("Urban" = "#440154", "Intermediate" = "#31688e", "Rural" = "#35b779")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 10),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.minor = element_blank()
    ) +
    labs(
      y = "Log Funding Intensity",
      x = NULL,
      title = "EU Policy Funding Intensity by Regional Typology"
    )
}


# ==============================================================================
# 5. EXECUTION
# ==============================================================================

# Prepare data
prep <- prepare_policy_data(predictors, pop_wide, nuts_shp)

# Generate the map
policy_map <- plot_policy_map(
  spatial_data = prep$map_data,
  world_bg = world,
  nuts0 = nuts0_shp,
  extent = map_extent
)

# Generate the boxplot
policy_box <- plot_policy_boxplot(prep$box_data)

# Save plots
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  filename = paste0(output_dir, "policy_intensity_maps.png"),
  plot     = policy_map,
  dpi      = plot_dpi,
  width    = map_width,
  height   = map_height,
  bg       = "white"
)
message("Saved: ", output_dir, "policy_intensity_maps.png")

ggsave(
  filename = paste0(output_dir, "policy_boxplot_typology.png"),
  plot     = policy_box,
  dpi      = plot_dpi,
  width    = box_width,
  height   = box_height,
  bg       = "white"
)
message("Saved: ", output_dir, "policy_boxplot_typology.png")
