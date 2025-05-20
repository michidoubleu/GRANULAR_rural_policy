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
# input.path <- "P:/globiom/Projects/GRANULAR/rural_estim"
# source("codes/load_data.R")

##############################################################################################################################




##############################################################################################################################
################################################# USER SETTINGS PART #########################################################
##############################################################################################################################

CLUSTER <- FALSE
test.scen <- 7

### change to run different variables
### clim: "tas_perc", "tasmax_perc", "tasmin_perc", "pr",
### CAP: "AEP", "COOP","COUP", "DECOUP", "DIV", "FOR", "GRD", "HC","LFA", "MARKET", "MISC", "PC", "N2K", "TA","EARLY"
### ESIF: "TransConstr", "EnvN2K", "BuildConstr", "RiskPrev", "Brownfield", "EnergyConstr"
### access: "health_2020_n1", "health_2020_n3", "educ_2020_n1"

all.vars.considered <- list(
  set1=c("pc_gdp", "emp_pc", "gva_B.E", "health_2020_n1", "educ_2020_n1", "initial_pop_log", "pop_dens", "pop_dens_sq"),
  set2=c("pc_gdp", "emp_pc", "gva_B.E", "health_2020_n1", "educ_2020_n1", "initial_pop_log", "pop_dens", "pop_dens_sq",
         "Pillar II", "ESIF"))

#### east, nordic, MOUNT_TYPE, COAST_TYPE, intermediate, rural, urban
all.dummies.considered <- list(dummy.set1=c("COAST_TYPE", "predominantly urban","predominantly rural", "capital", "STmetro"))

### currently only option growth_corr (2021-2011 population minus natural change)
param.grid <- expand.grid(
  Y.spec = c("growth_corr", "change_corr"),
  region.spec = c("full"),
  model = c("SAR", "SDM"),
  add.interaction = c(TRUE),
  add.c.dummies = c(FALSE),
  var.set = names(all.vars.considered),
  dummy.set = names(all.dummies.considered),
  stringsAsFactors = FALSE
)
source("codes/update_config_estim.R")

##############################################################################################################################


##############################################################################################################################
##################################################### processing #############################################################
##############################################################################################################################

if(CLUSTER){
  system("run_estim.bat")
} else {
  source("codes/estimateW_estim.R")
}

##############################################################################################################################



##############################################################################################################################
################################################## RESULT PROCESSING #########################################################
##############################################################################################################################

#source("codes/write_res_latex.R")

##############################################################################################################################






