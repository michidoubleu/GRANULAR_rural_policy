# Function to bold coefficients with stars
bold_if_star <- function(x, y) {
  ifelse(y != "" & !is.na(y),
         paste0("\\textbf{", x, "}"),
         x)
}





plot.tex <- function(results, outputname, caption="Table1", label="tab:sdm_results", placement="ht") {
  # Apply bolding
  results$Mean1 <- bold_if_star(results$Mean1, results$stars1)
  # results$SD1   <- bold_if_star(results$SD1,   results$stars1)
  results$Mean2 <- bold_if_star(results$Mean2, results$stars2)
  # results$SD2   <- bold_if_star(results$SD2,   results$stars2)
  results$Mean3 <- bold_if_star(results$Mean3, results$stars3)
  #  results$SD3   <- bold_if_star(results$SD3,   results$stars3)

  library(xtable)

  latex_tab <- xtable(
    results,
    caption = caption,
    label   = label,
    align   = c("c", "l", "r", "l@{~}", "r@{~~~~~}",  "r", "l@{~}", "r@{~~~~~}", "r", "l@{~}", "r")
  )

  addtorow <- list()
  # The last 3 rows (rho, sigma2, Observations) are set off from the
  # coefficients by a midrule.
  addtorow$pos <- list(0,nrow(results)-3, nrow(results))
  addtorow$command <- c(
    paste(
      "\\toprule\n",
      "\\multicolumn{1}{c}{} &",
      "\\multicolumn{3}{c}{i)} &",
      "\\multicolumn{3}{c}{ii)} &",
      "\\multicolumn{3}{c}{iii)} \\\\ \n",
      "Variable & Median && Std. dev. & Median && Std. dev. & Median && Std. dev. \\\\ \n",
      "\\midrule\n"
    ),
    "\\midrule\n",
    "\\bottomrule\n"
  )

  # Capture xtable output into a string
  tab_str <- print(
    latex_tab,
    include.rownames = FALSE,
    include.colnames = FALSE,
    sanitize.text.function = identity,
    add.to.row = addtorow,
    hline.after = NULL,
    floating = TRUE,
    table.placement = placement,
    print.results = FALSE
  )

  # Now manually prepend things before \begin{tabular}
  tab_str <- sub(
    "(?=\\\\begin\\{tabular\\})",
    "\\\\addtolength{\\\\tabcolsep}{-0.6em}\n",
    tab_str,
    perl = TRUE
  )

  writeLines(tab_str, outputname)
}


plot.tex.effects <- function(results, outputname, caption="Table1", label="tab:sdm_effects") {
  # Apply bolding
  results$MeanD <- bold_if_star(results$MeanD, results$starsD)
  results$MeanI <- bold_if_star(results$MeanI, results$starsI)

  library(xtable)

  latex_tab <- xtable(
    results,
    caption = caption,
    label   = label,
    align   = c("c", "l@{~~}", "r", "l@{~}", "r@{~~~~~}", "r", "l@{~}", "r")
  )

  addtorow <- list()
  # The last 3 rows (rho, sigma2, Observations) are set off from the
  # coefficients by a midrule.
  addtorow$pos <- list(0, nrow(results)-3, nrow(results))
  addtorow$command <- c(
    paste(
      "\\toprule\n",
      "\\multicolumn{1}{c}{} &",
      "\\multicolumn{3}{c}{Direct} &",
      "\\multicolumn{3}{c}{Indirect} \\\\ \n",
      "Variable & Median && Std. dev. & Median && Std. dev. \\\\ \n",
      "\\midrule\n"
    ),
    "\\midrule\n",
    "\\bottomrule\n"
  )

  tab_str <- print(
    latex_tab,
    include.rownames = FALSE,
    include.colnames = FALSE,
    sanitize.text.function = identity,
    add.to.row = addtorow,
    hline.after = NULL,
    floating = TRUE,
    print.results = FALSE
  )

  tab_str <- sub(
    "(?=\\\\begin\\{tabular\\})",
    "\\\\addtolength{\\\\tabcolsep}{-0.6em}\n",
    tab_str,
    perl = TRUE
  )

  writeLines(tab_str, outputname)
}


plot.tex.wspec <- function(results, outputname, caption="Table1", label="tab:wspec") {
  library(xtable)

  latex_tab <- xtable(results, caption = caption, label = label,
                       align = c("l", "l", "r", "r", "r", "r", "r"))

  tab_str <- print(
    latex_tab,
    include.rownames = FALSE,
    sanitize.text.function = identity,
    booktabs = TRUE,
    floating = TRUE,
    print.results = FALSE
  )

  # This table has enough columns that it can overflow the page width at
  # normal size; scale it to fit rather than let it run off the margin.
  tab_str <- sub("(?=\\\\begin\\{tabular\\})", "\\\\resizebox{\\\\linewidth}{!}{\n", tab_str, perl = TRUE)
  tab_str <- sub("(\\\\end\\{tabular\\})", "\\1\n}", tab_str, perl = TRUE)

  writeLines(tab_str, outputname)
}
