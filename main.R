rm(list=ls())


##############################################################################################################################
################################################## LOAD LIBRARIES ############################################################
##############################################################################################################################
library(sf)
library(dplyr)
library(tidyr)
library(sf)
library(spdep)
library(openxlsx)
##############################################################################################################################

##############################################################################################################################
############################################# LOAD RAW DATA (run once) #######################################################
##############################################################################################################################

###### load data.. to be updated to include more policy variables and maybe play around with other variables, check also literature to have most important drivers covered. (accessibility)

input.path <- "P:/globiom/Projects/GRANULAR/rural_estim"

if(!dir.exists("renv")){
  source("Code/S2_setup_renv.R")
}
if(length(list.files("inputs", all.files = TRUE, no.. = TRUE)) == 0){
  source("Code/load_data.R")
}

##############################################################################################################################




##############################################################################################################################
################################################# USER SETTINGS PART #########################################################
##############################################################################################################################

CLUSTER <- FALSE
test.scen <- 3
save.draws <- 8000
burn.draws <- 2000

### change to run different variables
### clim: "tas_perc", "tasmax_perc", "tasmin_perc", "pr",
### CAP: "AEP", "COOP","COUP", "DECOUP", "DIV", "FOR", "GRD", "HC","LFA", "MARKET", "MISC", "PC", "N2K", "TA","EARLY"
### ESIF: "TransConstr", "EnvN2K", "BuildConstr", "RiskPrev", "Brownfield", "EnergyConstr"
### access: "health_2020_n1", "health_2020_n3", "educ_2020_n1"

all.vars.considered <- list(
  base=c("pc_gdp", "emp_pc", "gva_A", "gva_B.E", "accessibility", "initial_pop_log", "pop_dens", "pop_dens_sq"),
 # policy.small=c("pc_gdp", "emp_pc", "gva_A" , "gva_B.E", "accessibility", "initial_pop_log", "pop_dens", "pop_dens_sq",
  #       "Pillar I", "Pillar II", "ESIF"),
  policy.big=c("pc_gdp", "emp_pc", "gva_A", "gva_B.E", "accessibility", "initial_pop_log", "pop_dens", "pop_dens_sq",
         "Pillar I","Pillar II env", "Pillar II dev", "ESIF_env", "ESIF_cons"))

#### east, nordic, MOUNT_TYPE, COAST_TYPE, intermediate, rural, urban
all.dummies.considered <- list(dummy.set1=c("COAST_TYPE", "predominantly urban","predominantly rural", "capital", "STmetro"))

### currently only option growth_corr (2021-2011 population minus natural change)
param.grid <- expand.grid(
  Y.spec = c("growth_corr"),
  region.spec = c("full"),
  add.interaction = c("none", "policy"),
  add.c.dummies = c(FALSE),
  var.set = names(all.vars.considered),
  dummy.set = names(all.dummies.considered),
  model = c("SAR", "SDM"),
  W.spec = c("knn10", "knn5", "knn15", "contiguity", "dist_decay"),
  burn.draws = burn.draws,
  save.draws = save.draws,
  stringsAsFactors = FALSE
)

### Filtering rules to keep grid manageable:
# 1. No policy interactions for the base variable set
param.grid <- param.grid %>% filter(!(add.interaction=="policy" & var.set=="base"))
# 2. Alternative W-specs only with policy.big + no-interaction + no country dummies (robustness)
param.grid <- param.grid %>% filter(W.spec == "knn10" | (var.set == "policy.big" & add.interaction == "policy" & add.c.dummies == FALSE))
# 3. Country dummies only with knn10 baseline W (robustness)
param.grid <- param.grid %>% filter(!add.c.dummies | W.spec == "knn10")
source("Code/update_config_estim.R")

##############################################################################################################################


##############################################################################################################################
##################################################### processing #############################################################
##############################################################################################################################

if(CLUSTER){
  system("run_estim.bat")
} else {
  source("Code/estimateW_estim.R")
}

##############################################################################################################################



##############################################################################################################################
################################################## RESULT PROCESSING #########################################################
##############################################################################################################################

source("Code/summarize_cluster_res.R")
# source("Code/write_resPDF.R")

source("Code/write_res_latex.R")
source("Code/policy_plots.R")
source("Code/income_plots.R")
source("Code/Figure1_typology_map.R")
source("Code/Figure2_density_growth_corr.R")
source("Code/Figure3_growth_maps.R")

##############################################################################################################################






