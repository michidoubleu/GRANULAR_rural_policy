environment(.libPaths)$.lib.loc = c("renv/library/R-4.1/x86_64-w64-mingw32")

args <- commandArgs(trailingOnly = TRUE)
JOB <- ifelse(.Platform$GUI == "RStudio",test.scen, as.integer(args[[1]]))
dir.create("output")
dir.create("economic_res")

load("inputs/temp_grid.RData")

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
W.spec <- job.params$W.spec
# Resolve sets to actual variables
vars.considered <- all.vars.considered[[job.params$var.set]]
dummies.considered <- all.dummies.considered[[job.params$dummy.set]]

source("Code/prepare_data.R")
source("Code/knn.R")

# Generate descriptive stats and maps before running the model

# Combine predictors (explain) with the dependent variable (Y.info)
# We rename 'Y' back to 'growth_corr' (or whatever Y.spec is) for clarity in the table
df <- explain %>%
  left_join(Y.info %>% dplyr::select(NUTS_ID, Y), by = "NUTS_ID") %>%
  rename(growth_corr = Y)

# Ensure geometry is present for mapping (if not already in 'explain')
if(!"sf" %in% class(df)){
  # Load shapefile if it wasn't kept in explain
  nuts_sf <- readRDS("inputs/final_NUTS.rds")
  df <- nuts_sf %>%
    dplyr::select(NUTS_ID, geometry) %>%
    right_join(df, by = "NUTS_ID")
}

# message("Generating descriptive statistics and maps...")
# source("Code/generate_descriptives.R")


shape <- readRDS("inputs/final_NUTS.rds")
shape_filtered <- shape %>% filter(NUTS_ID %in% Y.info$NUTS_ID)
shape_filtered <- shape_filtered[match(Y.info$NUTS_ID, shape_filtered$NUTS_ID), ]
centroids <- st_centroid(shape_filtered)
coords <- st_coordinates(centroids)

# Construct W-matrix based on specification
W <- switch(W.spec,
  "knn5"       = getWknn(coords, 5),
  "knn10"      = getWknn(coords, 10),
  "knn15"      = getWknn(coords, 15),
  "contiguity" = getWcontiguity(shape_filtered),
  "dist_decay" = getWdistdecay(coords, bandwidth_km = 50, cutoff_km=100),
  stop(paste0("Unknown W.spec: ", W.spec))
)
message(paste0("W-matrix constructed: ", W.spec, " (dim: ", nrow(W), "x", ncol(W), ")"))

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
  W.spec = job.params$W.spec,
  vars.considered = all.vars.considered[[job.params$var.set]],
  dummies.considered = all.dummies.considered[[job.params$dummy.set]]
)

save(res1, file="./output/GRANULAR_estim.RData")

