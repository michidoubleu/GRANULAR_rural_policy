# ==============================================================================
# Download the Eurostat GISCO boundary files used for the NUTS 3 geometry,
# the spatial weight matrices and the maps (public, (c) EuroGeographics).
#   data/geo/NUTS_RG_20M_2016_3035/   NUTS 2016, 1:20 million, EPSG:3035
#   data/geo/CNTR_RG_20M_2020_4326/   countries 2020, 1:20 million (map background)
# ==============================================================================

gisco <- "https://gisco-services.ec.europa.eu/distribution/v2"
layers <- c(NUTS_RG_20M_2016_3035 = paste0(gisco, "/nuts/shp/NUTS_RG_20M_2016_3035.shp.zip"),
            CNTR_RG_20M_2020_4326 = paste0(gisco, "/countries/shp/CNTR_RG_20M_2020_4326.shp.zip"))

for (layer in names(layers)) {
  dir <- file.path("data/geo", layer)
  if (file.exists(file.path(dir, paste0(layer, ".shp")))) next
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  zip <- tempfile(fileext = ".zip")
  download.file(layers[[layer]], zip, mode = "wb", quiet = TRUE)
  unzip(zip, exdir = dir)
  unlink(zip)
  message("Downloaded ", layer)
}
