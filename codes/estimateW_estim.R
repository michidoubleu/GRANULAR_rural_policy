environment(.libPaths)$.lib.loc = c("renv/library/R-4.1/x86_64-w64-mingw32")

args <- commandArgs(trailingOnly = TRUE)
JOB <- ifelse(.Platform$GUI == "RStudio",test.scen, as.integer(args[[1]]))
dir.create("output")
dir.create("economic_res")

load("input/temp_grid.RData")

require(estimateW)
library(dplyr)
library(tidyr)
library(FNN)
library(spam)
library(Matrix)
library(pracma)
library(MASS)
library(sf)
library(spdep)
library(scales)

# Use the parameters
job.params <- param.grid[JOB,]
Y.spec <- job.params$Y.spec
model <- job.params$model
add.interaction <- job.params$add.interaction
region.spec <- job.params$region.spec
add.c.dummies <- job.params$add.c.dummies
burn.draws <- job.params$burn.draws
save.draws <- job.params$save.draws
# Resolve sets to actual variables
vars.considered <- all.vars.considered[[job.params$var.set]]
dummies.considered <- all.dummies.considered[[job.params$dummy.set]]

source("codes/prepare_data.R")
source("codes/knn.R")


shape <- readRDS("input/final_NUTS.rds")
centroids <- st_centroid(shape %>% filter(NUTS_ID %in% Y.info$NUTS_ID))
centroids <- centroids[match(Y.info$NUTS_ID, centroids$NUTS_ID), ]
coords <- st_coordinates(centroids)
W<-getWknn(coords,10)

if(model=="SDM"){
res1 = sdm(Y,tt=1,X = X,Z = Z,W = W, niter = save.draws+burn.draws, nretain = save.draws)
}

if(model=="SLX"){
  res1 = slx(Y,tt=1,X = X,Z = Z,W = W, niter = save.draws+burn.draws, nretain = save.draws)
}

if(model=="SAR"){
  Z <- as.matrix(bind_cols(X,Z))
  res1 = sar(Y,tt=1,Z = Z,W = W, niter = save.draws+burn.draws, nretain = save.draws)
}

res1[["setting"]] <- list(
  Y.spec = job.params$Y.spec,
  model = job.params$model,
  region.spec = job.params$region.spec,
  add.interaction = job.params$add.interaction,
  add.c.dummies = job.params$add.c.dummies,
  vars.considered = all.vars.considered[[job.params$var.set]],
  dummies.considered = all.dummies.considered[[job.params$dummy.set]]
)

save(res1, file="./output/GRANULAR_estim.RData")

