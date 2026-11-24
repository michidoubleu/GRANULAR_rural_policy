# ==============================================================================
# Rural Population Change in Europe: The Role of Policy, Economic Structure,
# and Space -- replication code (Journal of Agricultural Economics submission)
#
# Run from the project root:   Rscript main.R
# Each step can be switched off below. See README.md for data and runtimes.
# ==============================================================================

# ---- settings ----------------------------------------------------------------
raw.path   <- Sys.getenv("GRANULAR_RAW_DATA", unset = "raw_data")  # folder with the raw data files
BUILD_DATA <- TRUE   # raw data -> inputs/*.rds (needs raw.path, incl. the CATS data)
ESTIMATE   <- TRUE   # MCMC estimation of all models -> output/ (long, see README)
N_CORES    <- max(1, parallel::detectCores() - 1)  # models are estimated in parallel
save.draws <- 8000   # retained draws per model (paper: 8,000)
burn.draws <- 2000   # burn-in draws per model  (paper: 2,000)

library(dplyr)
library(tidyr)
library(sf)
for (d in c("inputs", "output", "economic_res", "tables", "plots")) dir.create(d, showWarnings = FALSE)


# ---- 1. data -------------------------------------------------------------------
source("Code/get_geo_data.R")                 # Eurostat GISCO boundaries -> data/geo/
if (BUILD_DATA) source("Code/load_data.R")    # raw data -> inputs/


# ---- 2. model specifications (14 models) ---------------------------------------
all.vars.considered <- list(
  base       = c("pc_gdp", "emp_pc", "gva_A", "gva_B.E", "accessibility", "initial_pop_log",
                 "pop_dens", "pop_dens_sq"),
  policy.big = c("pc_gdp", "emp_pc", "gva_A", "gva_B.E", "accessibility", "initial_pop_log",
                 "pop_dens", "pop_dens_sq",
                 "Pillar I", "Pillar II env", "Pillar II dev", "ESIF_env", "ESIF_cons"))
all.dummies.considered <- list(
  dummy.set1 = c("COAST_TYPE", "predominantly urban", "predominantly rural", "capital", "STmetro"))

param.grid <- expand.grid(
  Y.spec          = "growth_corr",             # 2011-2021 population change net of natural change
  region.spec     = "full",
  add.interaction = c("none", "policy"),       # "policy": policy x urban-rural typology
  add.c.dummies   = FALSE,
  var.set         = names(all.vars.considered),
  dummy.set       = names(all.dummies.considered),
  model           = c("SAR", "SDM"),
  W.spec          = c("knn10", "knn5", "knn15", "contiguity", "dist_decay"),
  burn.draws      = burn.draws,
  save.draws      = save.draws,
  stringsAsFactors = FALSE
)
param.grid <- param.grid %>%
  filter(!(add.interaction == "policy" & var.set == "base")) %>%      # interactions only with policy variables
  filter(W.spec == "knn10" | (var.set == "policy.big" & add.interaction == "policy"))  # alternative W: robustness
save(param.grid, all.vars.considered, all.dummies.considered, file = "inputs/model_grid.RData")


# ---- 3. estimation (one R process per model) -------------------------------------
if (ESTIMATE) {
  rscript <- file.path(R.home("bin"), "Rscript")
  dir.create("output/logs", showWarnings = FALSE)
  cl <- parallel::makeCluster(min(N_CORES, nrow(param.grid)))
  status <- parallel::parLapply(cl, seq_len(nrow(param.grid)), function(job, rscript) {
    system2(rscript, c("Code/estimateW_estim.R", job),
            stdout = sprintf("output/logs/job_%02d.log", job), stderr = sprintf("output/logs/job_%02d.log", job))
  }, rscript = rscript)
  parallel::stopCluster(cl)
  if (any(unlist(status) != 0)) stop("Estimation failed for job(s) ", paste(which(unlist(status) != 0), collapse = ", "),
                                     " - see output/logs/")
  source("Code/summarize_results.R")          # draws -> economic_res/model_res_local_includingSD.rds
  source("Code/convergence_diagnostics.R")    # Table A3, Figure 6
}


# ---- 4. tables and figures -------------------------------------------------------
source("Code/write_res_latex.R")              # Tables 2, A1, A2 (from economic_res/)
source("Code/Figure1_typology_map.R")         # Figure 1
source("Code/Figure2_density_growth_corr.R")  # Figure 2
source("Code/Figure3_growth_maps.R")          # Figure 3
source("Code/policy_plots.R")                 # Figures 4 and 5
message("Done: tables/ and plots/")
