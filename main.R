rm(list=ls())

library(sf)
library(dplyr)
library(tidyr)
library(sf)
library(spdep)

input.path <- "P:/globiom/Projects/GRANULAR/rural_estim"

#### run once after update of data
###### load data.. to be updated to include more policy variables and maybe play around with other variables, check also literature to have most important drivers covered. (accessibility)
source("codes/load_data.R")



### clim: "tas_perc", "tasmax_perc", "tasmin_perc", "pr",
### CAP: "AEP", "COOP","COUP", "DECOUP", "DIV", "FOR", "GRD", "HC","LFA", "MARKET", "MISC", "PC", "N2K", "TA","EARLY"
### ESIF: "TransConstr", "EnvN2K", "BuildConstr", "RiskPrev", "Brownfield", "EnergyConstr"
### access: "health_2020_n1", "health_2020_n3", "educ_2020_n1"

### change to run different specification of econometric equation
vars.considered <- c("gva_A", "gva_B.E", "gva_F", "gva_G.J", "gva_K.N", "gva_O.U", "health_2020_n1", "educ_2020_n1",
                     "nr_farms", "rent", "emp_pc","BuildConstr", "RiskPrev", "COUP", "DECOUP","LFA")

#support functions
source('codes/knn.R')
source("codes/lndetPaceBarry.R")
source("codes/SDM_SSVS.R")

# ##### run the econometrics
# source("codes/non_spatial_estim.R")

source("codes/spatial_estim.R")
source("codes/write_res_latex.R")


