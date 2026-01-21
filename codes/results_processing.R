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
  return(combined)
}

#----------------------------
# SAR results
#----------------------------
small.sar <- combine_results(
  results$`SAR-none-Base`,
  results$`SAR-none-policySMALL`,
  results$`SAR-policy-policySMALL`
)

big.sar <- combine_results(
  results$`SAR-none-Base`,
  results$`SAR-none-policyBIG`,
  results$`SAR-policy-policyBIG`
)

#----------------------------
# SDM results
#----------------------------
small.sdm <- combine_results(
  results$`SDM-none-Base`,
  results$`SDM-none-policySMALL`,
  results$`SDM-policy-policySMALL`
)

big.sdm <- combine_results(
  results$`SDM-none-Base`,
  results$`SDM-none-policyBIG`,
  results$`SDM-policy-policyBIG`
)
