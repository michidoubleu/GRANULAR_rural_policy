results <- readRDS("./economic_res/model_res_3292_includingSD.rds")

#
# #Calculating summary statistics
# # # Select only numeric columns
# # X_numeric <- X.info[, sapply(X.info, is.numeric)]
# #
# # # Apply quantiles: min, 25th, median, 75th, max
# # # quantile_summary <- t(apply(X_numeric, 2, quantile, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
# # quantile_summary <- t(apply(X_numeric, 2, quantile, probs = c(0, 0.5, 1), na.rm = TRUE))
# #
# #
# # # Save to CSV
# # write.csv(quantile_summary, "output/summary_X_info.csv", row.names = T)
#
# # For aggregated policies,
# # agg <- c("SAR-none-Base", "SAR-none-policySMALL", "SAR-policy-policySMALL")
# agg <- names(results)
#
# # Convert each element into a data frame and store in a new list
# df_list <- lapply(agg, function(k) as.data.frame(results[[k]]))
# names(df_list) <- agg
# df <- bind_rows(df_list, .id = "source")
# write.csv(df, "output/res_agg.csv", row.names = T)
#



# complete results for SAR------------------------------------------------------

base.res <- data.frame(coef=rownames(results$`SAR-none-Base`$Direkt),results$`SAR-none-Base`$Direkt)
colnames(base.res)[-1] <- paste0("(1) - ",colnames(base.res)[-1])
poli.res <- data.frame(coef=rownames(results$`SAR-none-policySMALL`$Direkt), results$`SAR-none-policySMALL`$Direkt)
colnames(poli.res)[-1] <- paste0("(2) - ",colnames(poli.res)[-1])
inter.res <- data.frame(coef=rownames(results$`SAR-policy-policySMALL`$Direkt), results$`SAR-policy-policySMALL`$Direkt)
colnames(inter.res)[-1] <- paste0("(3) - ",colnames(inter.res)[-1])

full.res.small.sar <- full_join(full_join(base.res, poli.res), inter.res)


base.res <- data.frame(coef=rownames(results$`SAR-none-Base`$Direkt),results$`SAR-none-Base`$Direkt)
colnames(base.res)[-1] <- paste0("(1) - ",colnames(base.res)[-1])
poli.res <- data.frame(coef=rownames(results$`SAR-none-policyBIG`$Direkt), results$`SAR-none-policyBIG`$Direkt)
colnames(poli.res)[-1] <- paste0("(2) - ",colnames(poli.res)[-1])
inter.res <- data.frame(coef=rownames(results$`SAR-policy-policyBIG`$Direkt), results$`SAR-policy-policyBIG`$Direkt)
colnames(inter.res)[-1] <- paste0("(3) - ",colnames(inter.res)[-1])

full.res.big.sar <- full_join(full_join(base.res, poli.res), inter.res)

### addition of standard deviation SAR

base.res_sd <- data.frame(coef=rownames(results$`SAR-none-Base`$DirektSD),results$`SAR-none-Base`$DirektSD)
colnames(base.res_sd)[-1] <- paste0("(1) - ",colnames(base.res_sd)[-1])
t(base.res_sd)
poli.res_sd <- data.frame(coef=rownames(results$`SAR-none-policyBIG`$DirektSD), results$`SAR-none-policyBIG`$DirektSD)
colnames(poli.res_sd)[-1] <- paste0("(2) - ",colnames(poli.res_sd)[-1])
t(poli.res_sd)
inter.res_sd <- data.frame(coef=rownames(results$`SAR-policy-policyBIG`$DirektSD), results$`SAR-policy-policyBIG`$DirektSD)
colnames(inter.res_sd)[-1] <- paste0("(3) - ",colnames(inter.res_sd)[-1])
t(inter.res_sd)

full.res.big.sar_sd <- full_join(full_join(base.res_sd, poli.res_sd), inter.res_sd)
full.res.big.sar_sd <- t(full.res.big.sar_sd)
# Complete results for SDM (Robustness check)------------------------------------

base.res <- data.frame(coef=rownames(results$`SDM-none-Base`$Direkt),results$`SDM-none-Base`$Direkt)
colnames(base.res)[-1] <- paste0("(1) - ",colnames(base.res)[-1])
poli.res <- data.frame(coef=rownames(results$`SDM-none-policySMALL`$Direkt), results$`SDM-none-policySMALL`$Direkt)
colnames(poli.res)[-1] <- paste0("(2) - ",colnames(poli.res)[-1])
inter.res <- data.frame(coef=rownames(results$`SDM-policy-policySMALL`$Direkt), results$`SDM-policy-policySMALL`$Direkt)
colnames(inter.res)[-1] <- paste0("(3) - ",colnames(inter.res)[-1])

full.res.small.sdm <- full_join(full_join(base.res, poli.res), inter.res)

base.res <- data.frame(coef=rownames(results$`SDM-none-Base`$Direkt),results$`SDM-none-Base`$Direkt)
colnames(base.res)[-1] <- paste0("(1) - ",colnames(base.res)[-1])
poli.res <- data.frame(coef=rownames(results$`SDM-none-policyBIG`$Direkt), results$`SDM-none-policyBIG`$Direkt)
colnames(poli.res)[-1] <- paste0("(2) - ",colnames(poli.res)[-1])
inter.res <- data.frame(coef=rownames(results$`SDM-policy-policyBIG`$Direkt), results$`SDM-policy-policyBIG`$Direkt)
colnames(inter.res)[-1] <- paste0("(3) - ",colnames(inter.res)[-1])

full.res.big.sdm <- full_join(full_join(base.res, poli.res), inter.res)

# addition of standard deviation SDM

base.res_sd <- data.frame(coef=rownames(results$`SDM-none-Base`$DirektSD),results$`SDM-none-Base`$DirektSD)
colnames(base.res_sd)[-1] <- paste0("(1) - ",colnames(base.res_sd)[-1])
t(base.res_sd)
poli.res_sd <- data.frame(coef=rownames(results$`SDM-none-policyBIG`$DirektSD), results$`SDM-none-policyBIG`$DirektSD)
colnames(poli.res_sd)[-1] <- paste0("(2) - ",colnames(poli.res_sd)[-1])
t(poli.res_sd)
inter.res_sd <- data.frame(coef=rownames(results$`SDM-policy-policyBIG`$DirektSD), results$`SDM-policy-policyBIG`$DirektSD)
colnames(inter.res_sd)[-1] <- paste0("(3) - ",colnames(inter.res_sd)[-1])
t(inter.res_sd)

full.res.big.sdm_sd <- full_join(full_join(base.res_sd, poli.res_sd), inter.res_sd)
full.res.big.sdm_sd <- t(full.res.big.sdm_sd)

