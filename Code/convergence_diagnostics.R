# ============================================================
# MCMC convergence diagnostics: Geweke (1991) Z-statistics and
# trace plots, computed directly from the raw posterior draws in
# output/ (the summarized economic_res/*.rds files only keep
# posterior quantiles/SD, not the full chains).
#
# Output:
#   - economic_res/geweke_diagnostics.rds  (per-model, per-parameter Z stats)
#   - tables/geweke_summary.tex            (pass-rate summary for the
#                                            models reported in the paper)
#   - plots/trace_plots.png                (representative trace plots
#                                            for the headline SAR model)
#
# Requirements: coda
# ============================================================

library(coda)

# ---- Identify the latest cluster run, same logic as summarize_cluster_res.R ----
files <- list.files("./output/", full.names = TRUE)
files <- files[grepl("GRANULAR", files)]
cluster.ids <- sub(".*GRANULAR_estim_([A-Za-z0-9]+)\\..*", "\\1", basename(files))
mtime.by.cluster <- tapply(file.mtime(files), cluster.ids, max)
cluster.run <- names(mtime.by.cluster)[which.max(mtime.by.cluster)]
files <- files[cluster.ids == cluster.run]
files <- files[file.size(files) > 0]

# ---- Compute Geweke Z-statistics for every parameter in every job ----
geweke_results <- list()

for (f in files) {
  load(f)  # loads res1

  if (res1$setting$model == "SLX") next

  set.name <- if (sum(grepl("ESIF", res1$setting$vars.considered)) == 2) {
    "policyBIG"
  } else if (sum(grepl("ESIF", res1$setting$vars.considered)) == 1) {
    "policySMALL"
  } else {
    "Base"
  }
  W.label <- ifelse(is.null(res1$setting$W.spec), "knn10", res1$setting$W.spec)
  cd.label <- ifelse(!is.null(res1$setting$add.c.dummies) && res1$setting$add.c.dummies, "cFE", "noCFE")
  scen.name <- paste0(res1$setting$model, "-", res1$setting$add.interaction, "-",
                      set.name, "-", W.label, "-", cd.label)

  # Combine all chains (betas, rho, sigma2) into one coda mcmc object
  draws <- rbind(res1$post.direct, rho = as.numeric(res1$postr), sigma2 = as.numeric(res1$posts))
  chain <- coda::mcmc(t(draws))

  gz <- coda::geweke.diag(chain)$z
  geweke_results[[scen.name]] <- gz
}

saveRDS(geweke_results, "economic_res/geweke_diagnostics.rds")

# ---- Summary table for the models actually reported in the paper ----
headline.specs <- c(
  "SAR-none-Base-knn10-noCFE"       = "SAR (i)",
  "SAR-none-policyBIG-knn10-noCFE"  = "SAR (ii)",
  "SAR-policy-policyBIG-knn10-noCFE" = "SAR (iii)",
  "SDM-policy-policyBIG-knn10-noCFE" = "SDM (iii)"
)

summary.rows <- lapply(names(headline.specs), function(key) {
  z <- geweke_results[[key]]
  n.total <- length(z)
  n.fail <- sum(abs(z) > 1.96, na.rm = TRUE)
  data.frame(
    Model = headline.specs[[key]],
    Parameters = n.total,
    `Max abs. Z` = sprintf("%.2f", max(abs(z), na.rm = TRUE)),
    `Exceeding 1.96` = n.fail,
    check.names = FALSE
  )
})
geweke.summary <- do.call(rbind, summary.rows)

library(xtable)
tab <- xtable(geweke.summary,
               caption = "Geweke (1991) Convergence Diagnostics for the Reported Specifications",
               label = "tab:geweke",
               align = c("l", "l", "r", "r", "r"))
tab_str <- print(tab, include.rownames = FALSE, booktabs = TRUE, floating = TRUE, print.results = FALSE)
writeLines(tab_str, "tables/geweke_summary.tex")

source("Code/figure_palette.R")
# ---- Trace plots for the headline SAR policy-interaction model ----
key.trace <- "SAR-policy-policyBIG-knn10-noCFE"
for (f in files) {
  load(f)
  if (res1$setting$model == "SLX") next
  set.name <- if (sum(grepl("ESIF", res1$setting$vars.considered)) == 2) "policyBIG" else "other"
  W.label <- ifelse(is.null(res1$setting$W.spec), "knn10", res1$setting$W.spec)
  if (paste0(res1$setting$model, "-", res1$setting$add.interaction, "-", set.name, "-", W.label, "-noCFE") == key.trace) {
    trace.res <- res1
    break
  }
}

png("plots/trace_plots.png", width = 900, height = 700, res = 120)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(as.numeric(trace.res$postr), type = "l", col = SERIES_COL,
     xlab = "Draw", ylab = expression(rho), main = expression(paste("Trace: ", rho)))
plot(as.numeric(trace.res$posts), type = "l", col = SERIES_COL,
     xlab = "Draw", ylab = expression(sigma^2), main = expression(paste("Trace: ", sigma^2)))
plot(trace.res$post.direct["predominantly rural:Pillar I", ], type = "l", col = SERIES_COL,
     xlab = "Draw", ylab = "Coefficient", main = "Trace: Rural : Pillar I")
plot(trace.res$post.direct["predominantly rural:Pillar II env", ], type = "l", col = SERIES_COL,
     xlab = "Draw", ylab = "Coefficient", main = "Trace: Rural : Pillar II Env.")
dev.off()

cat("Convergence diagnostics written for cluster", cluster.run, "\n")
cat("Models checked:", length(geweke_results), "\n")
print(geweke.summary)
