# ==============================================================================
# 1. USER SETTINGS
# ==============================================================================

# -- File Paths --
path_nuts0_shp   <- "./data/geo/NUTS_RG_20M_2016_3035/NUTS_RG_20M_2016_3035.shp"
path_world_shp   <- "./data/geo/ref-countries-2020-20M/CNTR_RG_20M_2020_4326.shp"
path_nuts_custom <- "./data/NUTS_LAMASUS/shp_nuts.shp"
output_dir       <- "./plots/"

# -- Map Bounding Box --
map_extent <- c(xmin = 2700000, ymin = 1500000, xmax = 5900000, ymax = 5280000)

# -- Data Filtering & Scenarios --
target_year      <- 2050
target_scenarios <- c("BASE", "EA", "FP") # Maps will display side-by-side in this order
target_crops     <- "Corn"                  # NULL for all, or subset e.g., c("Barl", "Whea")
calc_per_area    <- TRUE                  # TRUE for EUR/ha, FALSE for Millions of EUR
user_length_bins <- 11

# -- Plot Output Settings --
plot_dpi                 <- 300
plot_height              <- 8
plot_width_per_scenario  <- 6             # Automatically scales the image width


# ==============================================================================
# 2. LIBRARIES & DATA LOADING
# ==============================================================================

# Load required plotting and spatial libraries
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(scales)

# Load Base Shapefiles using the paths defined in User Settings
nuts0_shp <- st_read(path_nuts0_shp, quiet = TRUE) %>%
  filter(LEVL_CODE == 0)

world <- st_read(path_world_shp, quiet = TRUE) %>%
  st_transform(crs = st_crs(nuts0_shp))

nuts_shp <- st_read(path_nuts_custom, quiet = TRUE)


# ==============================================================================
# 3. HELPER FUNCTIONS
# ==============================================================================

prepare_income_data <- function(
    res_data,
    shape_data,
    year_sel,
    scenarios,
    crop_sel,
    per_area
) {

  df <- res_data %>%
    filter(
      year == year_sel,
      scenario %in% scenarios
    )

  if (!is.null(crop_sel)) {
    df <- df %>% filter(crop %in% crop_sel)
  }

  map_data <- df %>%
    group_by(scenario, region) %>%
    summarise(
      income  = sum(income, na.rm = TRUE),
      area    = sum(area, na.rm = TRUE),
      .groups = "drop"
    )

  if (per_area) {
    map_data <- map_data %>%
      mutate(income = if_else(area > 0, income / area, NA_real_))
    legend_title <- "EUR / ha"

  } else {
    map_data <- map_data %>%
      mutate(income = income / 1e6)
    legend_title <- "Mio EUR"
  }

  # Ensure scenarios stay in the order provided by the user
  map_data$scenario <- factor(map_data$scenario, levels = scenarios)

  to_plot <- shape_data %>%
    left_join(map_data, by = c("NUTS_ID" = "region")) %>%
    filter(!is.na(scenario))

  list(
    data = to_plot,
    legend_title = legend_title
  )
}

cube_root_trans <- scales::trans_new(
  name = "cuberoot",
  transform = function(x) sign(x) * abs(x)^(1/3),
  inverse   = function(x) sign(x) * abs(x)^3
)

plot_income_map <- function(
    spatial_data,
    legend_title,
    max_abs
) {

  p_trans <- cube_root_trans
  # Create symmetric class boundaries in transformed space
  t_max <- p_trans$transform(max_abs)

  breaks_div <- p_trans$inverse(
    seq(-t_max, t_max, length.out = user_length_bins)
  )

  breaks_div <- signif(breaks_div, 2)

  mid_idx <- ceiling(length(breaks_div) / 2)
  breaks_div[mid_idx] <- 0

  ggplot() +
    geom_sf(data = world, fill = "lightgrey", colour = NA) +

    geom_sf(
      data = spatial_data,
      aes(fill = income),
      colour = "grey20",
      lwd = .05
    ) +

    geom_sf(
      data = nuts0_shp,
      fill = NA,
      colour = "black",
      lwd = .5
    ) +

    coord_sf(
      xlim = c(map_extent["xmin"], map_extent["xmax"]),
      ylim = c(map_extent["ymin"], map_extent["ymax"])
    ) +

    facet_wrap(~scenario, nrow = 1) +

    theme_bw() +
    ggthemes::theme_map() +

    scale_fill_steps2(
      low = "#A50026",
      mid = "white",
      high = "#006837",
      midpoint = 0,
      limits = c(-max_abs, max_abs),
      transform = p_trans,
      breaks = breaks_div,
      labels = scales::label_number(big.mark = ","),
      name = legend_title
    ) +

    guides(
      fill = guide_coloursteps(
        show.limits = TRUE,
        even.steps = TRUE,
        barwidth = 30,
        barheight = 1
      )
    ) +

    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      strip.text = element_text(face = "bold", size = 14),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
}


# ==============================================================================
# 4. EXECUTION
# ==============================================================================

# Prepare the data
prep <- prepare_income_data(
  res_data   = res,
  shape_data = nuts_shp,
  year_sel   = target_year,
  scenarios  = target_scenarios,
  crop_sel   = target_crops,
  per_area   = calc_per_area
)

# Calculate the maximum absolute value to center the diverging color scale at 0
max_income_abs <- max(abs(prep$data$income), na.rm = TRUE)
if(max_income_abs == 0 || is.na(max_income_abs)) { max_income_abs <- 1 }

crop_text <- if (is.null(target_crops)) {
  ""
} else {
  paste0(" - ", paste(target_crops, collapse = ", "))
}

# Generate the plot
final_income_plot <- plot_income_map(
  spatial_data = prep$data,
  legend_title = prep$legend_title,
  max_abs      = max_income_abs
) +
  labs(
    title = paste0(
      "Regional Income Comparison (",
      target_year,
      ")",
      crop_text
    )
  )

# Save the plot
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

unit_name <- ifelse(calc_per_area, "per_ha", "mio_eur")

# Dynamic width: scales based on the number of scenarios to prevent squishing
total_width <- plot_width_per_scenario * length(unique(prep$data$scenario))

ggsave(
  filename = paste0(output_dir, "income_comparison_", unit_name, ".png"),
  plot     = final_income_plot,
  dpi      = plot_dpi,
  width    = total_width,
  height   = plot_height,
  bg = "white"
)
