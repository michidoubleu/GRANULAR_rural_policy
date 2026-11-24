library(dplyr)
library(tidyr)
library(xtable)


# Use the most recently produced summary: economic_res/model_res_300_includingSD.rds
# (paper) unless main.R has been re-run locally (model_res_local_includingSD.rds).
res_files <- list.files("./economic_res", pattern = "^model_res_.+_includingSD\\.rds$", full.names = TRUE)
results <- readRDS(res_files[which.max(file.mtime(res_files))])
digits_in_res <- 4
source("Code/results_processing.R")
source("Code/write_latextable.R")



variable_mapping <- c(
  "pc_gdp"                            = "GDP per capita",
  "emp_pc"                            = "Employment per capita",
  "gva_A"                             = "Agriculture GVA",
  "gva_B.E"                           = "Industry GVA",
  "accessibility"                     = "Accessibility Index",
  "initial_pop_log"                   = "log(Initial population)",
  "pop_dens"                          = "Population density",
  "pop_dens_sq"                       = "Population density squared",
  "COAST_TYPE"                        = "Coastal type",
  "predominantly urban"               = "Urban",
  "predominantly rural"               = "Rural",
  "capital"                           = "Capital Region",
  "STmetro"                           = "Metro Region",
  "Pillar I"                          = "CAP Pillar I",
  "Pillar II env"                     = "CAP Pillar II Environment",
  "Pillar II dev"                     = "CAP Pillar II Development",
  "ESIF_env"                          = "ESIF Environment",
  "ESIF_cons"                         = "ESIF Infrastructure",
  "intermediate:Pillar I"             = "IM : Pillar I",
  "intermediate:Pillar II env"        = "IM : Pillar II Environment",
  "intermediate:Pillar II dev"        = "IM : Pillar II Development",
  "intermediate:ESIF_env"             = "IM : ESIF Environment",
  "intermediate:ESIF_cons"            = "IM : ESIF Infrastructure",
  "predominantly rural:Pillar I"      = "Rural : Pillar I",
  "predominantly rural:Pillar II env" = "Rural : Pillar II Environment",
  "predominantly rural:Pillar II dev" = "Rural : Pillar II Development",
  "predominantly rural:ESIF_env"      = "Rural : ESIF Environment",
  "predominantly rural:ESIF_cons"     = "Rural : ESIF Infrastructure",
  "predominantly urban:Pillar I"      = "Urban : Pillar I",
  "predominantly urban:Pillar II env" = "Urban : Pillar II Environment",
  "predominantly urban:Pillar II dev" = "Urban : Pillar II Development",
  "predominantly urban:ESIF_env"      = "Urban : ESIF Environment",
  "predominantly urban:ESIF_cons"     = "Urban : ESIF Infrastructure",
  "rho"    = "$\\rho$",      # LaTeX rho
  "sigma2" = "$\\sigma^2$",
  "Observations" = "Observations"
)


### NOTE: indexing a named vector with a factor (variable_mapping[some_factor])
### uses the factor's internal integer codes, not its labels. Always coerce
### to character first, or rows silently get the wrong label.
big.sar$coef <- variable_mapping[as.character(big.sar$coef)]

# Example usage
plot.tex(big.sar, "tables/bigsar.tex", caption="Bayesian Spatial Autoregressive (SAR) Model Results: Drivers of Population Change (2011--2021). Entries are posterior medians with posterior standard deviations in parentheses; ***, ** and * indicate that the 98\\%, 90\\% and 80\\% equal-tailed credible intervals exclude zero.", label="tab:sar_main", placement="t")

sdm.effects.policyBIG$coef <- variable_mapping[as.character(sdm.effects.policyBIG$coef)]

plot.tex.effects(
  sdm.effects.policyBIG,
  "tables/bigsar_sdm.tex",
  caption = "Spatial Durbin Model (SDM): Direct and Indirect Effects (Policy-Interaction Specification). Entries are posterior medians with posterior standard deviations in parentheses; ***, ** and * indicate that the 98\\%, 90\\% and 80\\% equal-tailed credible intervals exclude zero.",
  label   = "tab:sdm_robustness"
)

plot.tex.wspec(
  wspec.robustness.table,
  "tables/bigsar_wspec.tex",
  caption = "Sensitivity of Headline Policy-Interaction Coefficients to Alternative Spatial Weight Matrices. Entries are posterior medians with posterior standard deviations in parentheses; ***, ** and * indicate that the 98\\%, 90\\% and 80\\% equal-tailed credible intervals exclude zero.",
  label   = "tab:wspec_robustness"
)













