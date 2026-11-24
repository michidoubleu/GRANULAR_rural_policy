# ==============================================================================
# Build the prepared estimation data in inputs/ from the raw data files.
#
# Reads  <raw.path>/*.csv, *.xlsx   (see README: "Raw data")
#        data/geo/NUTS_RG_20M_2016_3035/   (Code/get_geo_data.R)
# Writes inputs/popdata_prepared.rds  population, natural change, net migration
#        inputs/predictors.rds        economic, accessibility and policy variables
#        inputs/final_NUTS.rds        NUTS 3 geometry with metro-region flags
# ==============================================================================

library(dplyr)
library(tidyr)
library(sf)
library(openxlsx)

if (!exists("raw.path")) raw.path <- Sys.getenv("GRANULAR_RAW_DATA", unset = "raw_data")
raw <- function(f) {
  p <- file.path(raw.path, f)
  if (!file.exists(p)) stop("Raw data file not found: ", p, " (see README, section 'Raw data')")
  p
}
dir.create("inputs", showWarnings = FALSE)


# ---- dependent variable: population change net of natural change ----

pop <- read.csv(raw("pop_NUTS3.csv"))
colnames(pop) <- c("NUTS_ID", "c", "d", "country", "typ", "a", "b", "pop2011", "pop2021")

# countries outside the study area
problems <- c("TR", "AL", "UK", "IS", "RS", "MK", "ME", "LI", "CH", "NO")
pop <- pop %>% filter(!country %in% problems) %>% dplyr::select(NUTS_ID, typ, pop2011, pop2021)

births <- read.csv(raw("Total_live_births_NUTS3.csv"))
births <- births %>% group_by(geo) %>% summarise(births = mean(OBS_VALUE, na.rm = TRUE))
deaths <- read.csv(raw("Total_deaths_NUTS3.csv"))
deaths <- deaths %>% group_by(geo) %>% summarise(deaths = mean(OBS_VALUE, na.rm = TRUE))

# natural change over the decade = 10 x average annual (births - deaths)
tot.change <- births %>% left_join(deaths) %>% mutate(change = 10 * (births - deaths)) %>% rename("NUTS_ID" = "geo")
pop <- pop %>% left_join(tot.change) %>% mutate(pop2021.corr = pop2021 - change)

pop$growth      <- (pop$pop2021 - pop$pop2011) / pop$pop2011
pop$change      <- pop$pop2021 - pop$pop2011
pop$growth_corr <- (pop$pop2021.corr - pop$pop2011) / pop$pop2011   # dependent variable
pop$change_corr <- pop$pop2021.corr - pop$pop2011

# reported net migration (descriptive comparison only, not used in the models)
mig <- read.csv(raw("nuts3_migration_2016.csv"))
mig <- mig %>% pivot_longer(cols = -NUTS_ID, names_to = "year", values_to = "value")
mig <- mig %>% group_by(NUTS_ID) %>% summarise(avg.mig = mean(value, na.rm = TRUE))
pop <- pop %>% left_join(mig)
pop$mig.rate <- pop$avg.mig / pop$pop2011

pop <- pop %>% pivot_longer(cols = -c(NUTS_ID, typ), names_to = "variable", values_to = "value")
represented.nuts <- unique(pop$NUTS_ID)
saveRDS(pop, file = "inputs/popdata_prepared.rds")


# ---- explanatory variables (averaged over the years in each file) ----

# GDP and gross value added by sector (ARDECO)
econ <- read.csv(raw("economic_data.csv"))[, -1]
colnames(econ)[1] <- "NUTS_ID"
econ <- econ %>% filter(NUTS_ID %in% represented.nuts) %>%
  group_by(NUTS_ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  dplyr::select(-year)

# employment per capita (ARDECO), 2011
emp <- read.csv(raw("Employment_per_capita_NUTS3.csv"))
colnames(emp)[1] <- "NUTS_ID"
emp <- emp %>% filter(NUTS_ID %in% represented.nuts) %>%
  group_by(NUTS_ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  dplyr::select(-LEVEL_ID, -X2021)
colnames(emp)[2] <- "emp_pc"

# drive times to health and education services, 2020
access <- read.csv(raw("Health_and_educ_accessibility_NUTS3.csv"))[, -1]
access <- access[, c(TRUE, grepl("2020", colnames(access))[-1])]
access <- access %>% filter(NUTS_ID %in% represented.nuts) %>%
  group_by(NUTS_ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Cohesion Policy (ESIF) payments by category
esif <- read.csv(raw("NUTS3_harm_ESIF_2024-06-10.csv"))
esif <- esif %>% filter(geo %in% represented.nuts) %>%
  group_by(geo) %>%
  pivot_wider(id_cols = c(geo, year), names_from = "Category", values_from = "values", values_fill = 0) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  dplyr::select(-year) %>% rename("NUTS_ID" = "geo")
colnames(esif) <- c("NUTS_ID", "TransConstr", "EnvN2K", "BuildConstr", "RiskPrev", "Brownfield", "EnergyConstr")

# CAP payments by measure (CATS; access on request, see README)
CAP <- read.csv(raw("CAPpayments_processed.csv"))
CAP <- CAP %>% filter(geo %in% represented.nuts) %>%
  group_by(geo) %>%
  pivot_wider(id_cols = c(geo), names_from = "type", values_from = "values", values_fill = 0) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>% rename("NUTS_ID" = "geo")

final <- econ %>% left_join(access) %>% left_join(emp) %>% left_join(esif) %>% left_join(CAP)
final[is.na(final)] <- 0
saveRDS(final, file = "inputs/predictors.rds")


# ---- geometry and metropolitan regions ----

metro <- read.xlsx(raw("Metro-regions-NUTS-2016.xlsx"), sheet = 2)
metro <- metro %>%
  mutate(class = ifelse(substr(MREG_CODE, 7, 7) == "C", "capital", "STmetro"), value = 1) %>%
  dplyr::select(NUTS_ID, class, value) %>%
  pivot_wider(id_cols = NUTS_ID, names_from = "class", values_fill = 0)

NUTS_shape <- st_read("data/geo/NUTS_RG_20M_2016_3035/NUTS_RG_20M_2016_3035.shp", quiet = TRUE)
NUTS_shape <- NUTS_shape %>% filter(NUTS_ID %in% unique(pop$NUTS_ID)) %>% left_join(metro)
NUTS_shape[is.na(NUTS_shape)] <- 0
saveRDS(NUTS_shape, file = "inputs/final_NUTS.rds")
