environment(.libPaths)$.lib.loc = c("renv/library/R-4.1/x86_64-w64-mingw32")

args <- commandArgs(trailingOnly = TRUE)
JOB <- ifelse(.Platform$GUI == "RStudio",test.scen, as.integer(args[[1]]))
dir.create("output")

param.grid <- readRDS("output/temp_grid.rds")

require(estimateW)
library(dplyr)
library(tidyr)
library(FNN)
library(spam)
library(Matrix)
library(pracma)
library(MASS)

# Use the parameters
job.params <- param.grid[JOB,]
Y.spec <- job.params$Y.spec
model <- job.params$model
region.spec <- job.params$region.spec
add.c.dummies <- job.params$add.c.dummies
# Resolve sets to actual variables
vars.considered <- all.vars.considered[[job.params$var.set]]
dummies.considered <- all.dummies.considered[[job.params$dummy.set]]

source("codes/prepare_data.R")
source("codes/knn.R")


shape <- readRDS("output/final_NUTS.rds")
centroids <- st_centroid(shape %>% filter(NUTS_ID %in% Y.info$NUTS_ID))
centroids <- centroids[match(Y.info$NUTS_ID, centroids$NUTS_ID), ]
coords <- st_coordinates(centroids)
W<-getWknn(coords,5)

if(model=="SDM"){
res1 = sdm(Y,tt=1,X = X,Z = Z,W = W)
}

# if(model=="SLX"){
#   res1 = slx(Y,tt=1,X = X,Z = Z,W = W)
# }

if(model=="SAR"){
  Z <- as.matrix(bind_cols(X,Z))
  res1 = sar(Y,tt=1,Z = Z,W = W)
}

res1[["setting"]] <- list(
  Y.spec = job.params$Y.spec,
  model = job.params$model,
  region.spec = job.params$region.spec,
  add.c.dummies = job.params$add.c.dummies,
  vars.considered = all.vars.considered[[job.params$var.set]],
  dummies.considered = all.dummies.considered[[job.params$dummy.set]]
)

save(res1, file="./output/GRANULAR_estim.RData")

