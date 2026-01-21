# Function to bold coefficients with stars
bold_if_star <- function(x, y) {
  ifelse(y != "" & !is.na(y),
         paste0("\\textbf{", x, "}"),
         x)
}






plot.tex <- function(results, outputname, caption="Table1", label="tab:sdm_results") {
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
  addtorow$pos <- list(0,nrow(results)-2, nrow(results))
  addtorow$command <- c(
    paste(
      "\\toprule\n",
      "\\multicolumn{1}{c}{} &",
      "\\multicolumn{3}{c}{i)} &",
      "\\multicolumn{3}{c}{ii)} &",
      "\\multicolumn{3}{c}{iii)} \\\\ \n",
      "Variable & Mean && Std. dev. & Mean && Std. dev. & Mean && Std. dev. \\\\ \n",
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

