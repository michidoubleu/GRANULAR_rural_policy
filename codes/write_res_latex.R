library(dplyr)
library(tidyr)
library(xtable)

# Load results
finalres <- readRDS("output/finalres.rds")

# Reshaping to wide format
df_wide <- finalres %>% 
  mutate(spec = recode(spec, "full" = "A", "no.urb" = "B", "rural" = "C"), 
         PIP = as.numeric(PIP), beta = as.numeric(beta)) %>%
  distinct() %>%  # Remove duplicates if any
  pivot_wider(names_from = spec, values_from = c(PIP, beta), names_glue = "{spec}_{.value}") 


variable_mapping <- c(
  "Intercept"      = "Intercept",
  "gva_A"          = "Agriculture_GVA",
  "gva_K.N"        = "KnowledgeIntensive_GVA",
  "tas_perc"       = "Temp_change",
  "W_gva_G.J"      = "W_GVA_TradeTransport",
  "W_tas_perc"     = "W_Temp_change",
  "gva_O.U"        = "OtherServices_GVA",
  "EnergyConstr"   = "ESIF_EnergyConstruction",
  "W_gva_O.U"      = "W_GVA_OtherServices",
  "COAST_TYPE"     = "Coastal_Type",
  "W_gva_F"        = "W_GVA_Manufacturing",
  "gva_B.E"        = "MiningUtilities_GVA",
  "BuildConstr"    = "ESIF_BuildingConstruction",
  "pr"             = "Precipitation",
  "gva_G.J"        = "TradeTransport_GVA",
  "W_pr"           = "W_Precipitation",
  "nr_farms"       = "Number_Farms",
  "W_gva_A"        = "W_GVA_Agriculture",
  "RiskPrev"       = "ESIF_Risk_Prevention",
  "W_EnergyConstr" = "W_EnergyConstruction",
  "W_RiskPrev"     = "W_RiskPrevention",
  "EnvN2K"         = "ESIF_Natura2000",
  "TransConstr"    = "ESIF_TransportConstruction",
  "MOUNT_TYPE"     = "Mountain_Type",
  "W_gva_B.E"      = "W_GVA_MiningUtilities",
  "nordic"         = "Nordic_Region_dummy",
  "W_tasmax_perc"  = "W_TempMax_change",
  "Brownfield"     = "ESIF_Brownfield_Area",
  "east"           = "Eastern_Region_dummy",
  "W_Brownfield"   = "W_Brownfield",
  "W_BuildConstr"  = "W_BuildingConstruction",
  "W_gva_K.N"      = "W_GVA_KnowledgeIntensive",
  "tasmax_perc"    = "TempMax_change",
  "W_EnvN2K"       = "W_Environmental_Natura2000",
  "rent"           = "Land_Rent",
  "W_TransConstr"  = "W_TransportConstruction",
  "gva_F"          = "Manufacturing_GVA",
  "W_nr_farms"     = "W_Number_Farms",
  "W_rent"         = "W_Land_Rent",
  "W_tasmin_perc"  = "W_TempMin_change",
  "tasmin_perc"    = "TempMin_change",
  "rho"            = "rho"
)

df_wide$names <- variable_mapping[df_wide$names]

colnames(df_wide)[1] <- "Covariate name"


# Rounding values
df_wide[,-1] <- round(df_wide[,-1], 2)

# Reordering columns
df_wide <- df_wide[, c(1, 2, 5, 3, 6, 4, 7)]

# Number of rows
n_rows <- nrow(df_wide)

# Insert a horizontal line before the last row
add_line <- list()
add_line$pos <- list(n_rows - 1)  # Position before the last row
add_line$command <- "\\hline \n"

# Specify column alignment with vertical lines
align_str <- "ll|cc|cc|cc"


print(xtable(df_wide, align = align_str,
      caption = "Results of 10,000 posterior draws per model specification",
      label = "tab:res1"),  add.to.row = add_line, include.rownames = F,  file = "./overleaf/reg_1_res.tex")


df_wide.short <- df_wide %>% filter(A_PIP>0.5|B_PIP>0.5|C_PIP>0.5)

# Number of rows
n_rows <- nrow(df_wide.short)

# Insert a horizontal line before the last row
add_line <- list()
add_line$pos <- list(n_rows - 1)  # Position before the last row
add_line$command <- "\\hline \n"

print(xtable(df_wide.short, align = align_str,
             caption = "Results of 10,000 posterior draws per model specification with PIP>0.5 in any specification",
             label = "tab:res1_short"),  add.to.row = add_line, include.rownames = F,  file = "./overleaf/reg_1_res_short.tex")













