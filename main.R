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



### change to run different specification of econometric equation
vars.considered <- c("gva_A")


#support functions
source('codes/knn.R')
source("codes/lndetPaceBarry.R")
source("codes/SDM_SSVS.R")


# ##### run the econometrics
# source("codes/non_spatial_estim.R")


source("codes/spatial_estim.R")
source("codes/write_res_latex.R")


