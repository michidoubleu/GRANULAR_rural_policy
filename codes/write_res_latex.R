library(dplyr)
library(tidyr)
library(xtable)


results <- readRDS("./economic_res/model_res_3292_includingSD.rds")
digits_in_res <- 4
source("codes/results_processing.R")
source("codes/write_latextable.R")



variable_mapping <- c(
  "pc_gdp"                            = "GDP per capita",
  "emp_pc"                            = "Employment per capita",
  "gva_A"                             = "Agriculture GVA",
  "gva_B.E"                           = "Mining and Utilities GVA",
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
  "ESIF_cons"                         = "ESIF Construction",
  "intermediate:Pillar I"             = "IM : Pillar I",
  "intermediate:Pillar II env"        = "IM : Pillar II Environment",
  "intermediate:Pillar II dev"        = "IM : Pillar II Development",
  "intermediate:ESIF_env"             = "IM : ESIF Environment",
  "intermediate:ESIF_cons"            = "IM : ESIF Construction",
  "predominantly rural:Pillar I"      = "Rural : Pillar I",
  "predominantly rural:Pillar II env" = "Rural : Pillar II Environment",
  "predominantly rural:Pillar II dev" = "Rural : Pillar II Development",
  "predominantly rural:ESIF_env"      = "Rural : ESIF Environment",
  "predominantly rural:ESIF_cons"     = "Rural : ESIF Construction",
  "predominantly urban:Pillar I"      = "Urban : Pillar I",
  "predominantly urban:Pillar II env" = "Urban : Pillar II Environment",
  "predominantly urban:Pillar II dev" = "Urban : Pillar II Development",
  "predominantly urban:ESIF_env"      = "Urban : ESIF Environment",
  "predominantly urban:ESIF_cons"     = "Urban : ESIF Construction",
  "rho"    = "$\\rho$",      # LaTeX rho
  "sigma2" = "$\\sigma^2$"
)


big.sar$coef <- variable_mapping[big.sar$coef]

# Example usage
plot.tex(big.sar, "bigsar.tex", caption="Table1", label="tab:sar_main")













