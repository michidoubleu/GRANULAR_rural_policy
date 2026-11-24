options(scipen = 999)

#----------------------------
# Formatting function
#----------------------------
format_num <- function(x, digits = 4) {
  sapply(x, function(val) {
    if (is.na(val)) return(NA)
    if (abs(val) < 5e-05) {
      "<1e-04"
    } else {
      formatC(val, format = "f", digits = digits)
    }
  })
}

#----------------------------
# Significance stars
#----------------------------
get_stars <- function(mat) {
  stars <- apply(mat, 1, function(row) {
    med <- row["50%"]

    if (med > 0) {
      if (row["1%"] > 0 & row["99%"] > 0) return("***")
      if (row["5%"] > 0 & row["95%"] > 0) return("**")
      if (row["10%"] > 0 & row["90%"] > 0) return("*")
    } else if (med < 0) {
      if (row["1%"] < 0 & row["99%"] < 0) return("***")
      if (row["5%"] < 0 & row["95%"] < 0) return("**")
      if (row["10%"] < 0 & row["90%"] < 0) return("*")
    }
    return("")
  })

  return(stars)
}

#----------------------------
# Function to combine results
#----------------------------
combine_results <- function(base, policy, inter) {
  base.res <- data.frame(
    coef = rownames(base$Direkt),
    Mean1 = format_num(base$Direkt[,"50%"]),
    stars1 = get_stars(base$Direkt),
    SD1 = paste0("(", format_num(t(base$DirektSD)), ")")
  )

  poli.res <- data.frame(
    coef = rownames(policy$Direkt),
    Mean2 = format_num(policy$Direkt[,"50%"]),
    stars2 = get_stars(policy$Direkt),
    SD2 = paste0("(", format_num(t(policy$DirektSD)), ")")
  )

  inter.res <- data.frame(
    coef = rownames(inter$Direkt),
    Mean3 = format_num(inter$Direkt[,"50%"]),
    stars3 = get_stars(inter$Direkt),
    SD3 = paste0("(", format_num(t(inter$DirektSD)), ")")
  )

  combined <- full_join(full_join(base.res, poli.res, by = "coef"), inter.res, by = "coef")

  # Ensure rho and sigma2 are last
  combined$coef <- factor(
    combined$coef,
    levels = c(
      setdiff(combined$coef, c("rho", "sigma2")),
      "rho", "sigma2"
    )
  )

  # Reorder dataframe
  combined <- combined[order(combined$coef), ]
  combined$coef <- as.character(combined$coef)

  obs.row <- data.frame(
    coef = "Observations",
    Mean1 = as.character(base$Observations), stars1 = "", SD1 = "",
    Mean2 = as.character(policy$Observations), stars2 = "", SD2 = "",
    Mean3 = as.character(inter$Observations), stars3 = "", SD3 = ""
  )
  combined <- rbind(combined, obs.row)
  return(combined)
}

#----------------------------
# SAR results
#----------------------------

# The main tables always report the knn10/no-country-FE baseline. Now that
# alternative W-specs and country-FE runs can share the results list, keys
# must be matched exactly -- `results$\`SAR-none-Base\`` used to rely on
# partial ($) matching, which returns NULL as soon as it's ambiguous (e.g.
# once knn5/knn15/contiguity/dist_decay variants of the same spec exist).
baseline <- function(key) results[[paste0(key, "-knn10-noCFE")]]

# "policy.small" is currently commented out of all.vars.considered in main.R,
# so a fresh cluster run will not produce policySMALL keys. small.sar/small.sdm
# are kept for backward compatibility with older result files but are skipped
# (rather than erroring) once those keys are absent.
has.policy.small <- any(grepl("policySMALL", names(results)))

if (has.policy.small) {
  small.sar <- combine_results(
    baseline("SAR-none-Base"),
    baseline("SAR-none-policySMALL"),
    baseline("SAR-policy-policySMALL")
  )
}

big.sar <- combine_results(
  baseline("SAR-none-Base"),
  baseline("SAR-none-policyBIG"),
  baseline("SAR-policy-policyBIG")
)

#----------------------------
# SDM results
#----------------------------
if (has.policy.small) {
  small.sdm <- combine_results(
    baseline("SDM-none-Base"),
    baseline("SDM-none-policySMALL"),
    baseline("SDM-policy-policySMALL")
  )
}

big.sdm <- combine_results(
  baseline("SDM-none-Base"),
  baseline("SDM-none-policyBIG"),
  baseline("SDM-policy-policyBIG")
)

#----------------------------
# SDM direct vs. indirect (spillover) effects
# for the main policy-interaction specification (model iii),
# used as a robustness check against the baseline SAR results.
#----------------------------
combine_direct_indirect <- function(res) {
  direct.res <- data.frame(
    coef = rownames(res$Direkt),
    MeanD = format_num(res$Direkt[,"50%"]),
    starsD = get_stars(res$Direkt),
    SDD = paste0("(", format_num(t(res$DirektSD)), ")")
  )

  indirect.res <- data.frame(
    coef = rownames(res$Indirekt),
    MeanI = format_num(res$Indirekt[,"50%"]),
    starsI = get_stars(res$Indirekt),
    SDI = paste0("(", format_num(t(res$IndirektSD)), ")")
  )

  combined <- full_join(direct.res, indirect.res, by = "coef")

  # Ensure rho and sigma2 are last (no indirect effect exists for these)
  combined$coef <- factor(
    combined$coef,
    levels = c(
      setdiff(combined$coef, c("rho", "sigma2")),
      "rho", "sigma2"
    )
  )
  combined <- combined[order(combined$coef), ]
  combined$coef <- as.character(combined$coef)

  obs.row <- data.frame(
    coef = "Observations",
    MeanD = as.character(res$Observations), starsD = "", SDD = "",
    MeanI = "", starsI = "", SDI = ""
  )
  combined <- rbind(combined, obs.row)
  return(combined)
}

sdm.effects.policyBIG <- combine_direct_indirect(baseline("SDM-policy-policyBIG"))

#----------------------------
# Alternative weight-matrix robustness (SAR, policy-interaction spec):
# compares the headline rural/intermediate policy-interaction coefficients
# and rho across all estimated W-specs.
#----------------------------
wspec.robustness <- function() {
  wspecs <- c("knn10", "knn5", "knn15", "contiguity", "dist_decay")
  wspec.labels <- c(knn10 = "KNN (k=10)", knn5 = "KNN (k=5)", knn15 = "KNN (k=15)",
                     contiguity = "Contiguity", dist_decay = "Distance decay")
  headline.vars <- c("rho", "predominantly rural:Pillar I", "predominantly rural:Pillar II env",
                      "predominantly rural:ESIF_cons", "intermediate:ESIF_env")

  rows <- lapply(wspecs, function(w) {
    res <- results[[paste0("SAR-policy-policyBIG-", w, "-noCFE")]]
    d <- res$Direkt[headline.vars, , drop = FALSE]
    sd <- t(res$DirektSD)[headline.vars, , drop = FALSE]
    vals <- sapply(seq_along(headline.vars), function(i) {
      paste0(format_num(d[i, "50%"]), get_stars(d)[i], " (", format_num(sd[i, 1]), ")")
    })
    c(`W-spec` = wspec.labels[[w]], setNames(vals, headline.vars))
  })

  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(out) <- c("W-spec", "$\\rho$", "Rural : Pillar I", "Rural : Pillar II Env.",
                      "Rural : ESIF Infra.", "Interm. : ESIF Env.")
  out
}

wspec.robustness.table <- wspec.robustness()
