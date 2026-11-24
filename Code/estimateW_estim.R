# ==============================================================================
# Estimate ONE model of the specification grid (one row of param.grid).
#
#   Rscript Code/estimateW_estim.R <job>      (called by main.R, one process per job)
#
# Reads  inputs/*.rds and inputs/model_grid.RData (written by main.R)
# Writes output/GRANULAR_estim_local.<job>.RData  (object res1: posterior draws)
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
JOB  <- if (length(args) >= 1) as.integer(args[[1]]) else get0("JOB", ifnotfound = 1L)

suppressPackageStartupMessages({
  library(estimateW)
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
})

dir.create("output", showWarnings = FALSE)
load("inputs/model_grid.RData")   # param.grid, all.vars.considered, all.dummies.considered

# ---- settings of this job ----
job.params      <- param.grid[JOB, ]
Y.spec          <- job.params$Y.spec
model           <- job.params$model
add.interaction <- job.params$add.interaction
region.spec     <- job.params$region.spec
add.c.dummies   <- job.params$add.c.dummies
burn.draws      <- job.params$burn.draws
save.draws      <- job.params$save.draws
W.spec          <- job.params$W.spec
vars.considered    <- all.vars.considered[[job.params$var.set]]
dummies.considered <- all.dummies.considered[[job.params$dummy.set]]

# ---- data: Y, X (continuous regressors), Z (dummies / policy interactions) ----
source("Code/prepare_data.R")
source("Code/knn.R")

# ---- spatial weight matrix ----
shape <- readRDS("inputs/final_NUTS.rds")
shape_filtered <- shape %>% filter(NUTS_ID %in% Y.info$NUTS_ID)
shape_filtered <- shape_filtered[match(Y.info$NUTS_ID, shape_filtered$NUTS_ID), ]
coords <- st_coordinates(st_centroid(shape_filtered))

W <- switch(W.spec,
  "knn5"       = getWknn(coords, 5),
  "knn10"      = getWknn(coords, 10),
  "knn15"      = getWknn(coords, 15),
  "contiguity" = getWcontiguity(shape_filtered),
  "dist_decay" = getWdistdecay(coords, bandwidth_km = 50, cutoff_km = 100),
  stop("Unknown W.spec: ", W.spec)
)
message("Job ", JOB, ": ", model, ", W = ", W.spec, " (", nrow(W), " regions), ",
        burn.draws + save.draws, " iterations")

# ---- MCMC (estimateW defaults: rho ~ Beta(1,1) on (0,1), griddy Gibbs) ----
# The draws reported in the paper were produced without a fixed seed; a re-run
# therefore reproduces them up to Monte Carlo error. The seed makes re-runs of
# this code reproducible.
set.seed(20250800 + JOB)
if (model == "SDM") {
  res1 <- sdm(Y, tt = 1, X = X, Z = Z, W = W, niter = save.draws + burn.draws, nretain = save.draws)
} else if (model == "SAR") {
  Z <- as.matrix(bind_cols(X, Z))
  res1 <- sar(Y, tt = 1, Z = Z, W = W, niter = save.draws + burn.draws, nretain = save.draws)
} else {
  stop("Unknown model: ", model)
}

res1[["setting"]] <- list(
  Y.spec = Y.spec, model = model, region.spec = region.spec,
  add.interaction = add.interaction, add.c.dummies = add.c.dummies, W.spec = W.spec,
  vars.considered = vars.considered, dummies.considered = dummies.considered
)

save(res1, file = sprintf("output/GRANULAR_estim_local.%06d.RData", JOB))
