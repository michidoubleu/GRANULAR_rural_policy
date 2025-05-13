rm(list = ls())

library(sf)
library(dplyr)
library(tidyr)
library(stargazer)
library(BMS)
library(htmlTable)


source("SSVS_OLS.R")

###### prepare Y
pop <- readRDS("output/popdata_prepared.rds")
Y <- pop %>% filter(variable=="growth") %>% arrange(NUTS_ID) %>% dplyr::select(NUTS_ID,typ,value) %>% rename("Y"="value")
Y$typ <- as.factor(Y$typ)



###### prepare X
explan <- readRDS("output/predictors.rds")

# vars.considered <- colnames(explan)[c(-1,-4,-6,-7,-9,-10)]
vars.considered <- colnames(explan)[-1]

X <- explan %>% dplyr::select(NUTS_ID, all_of(vars.considered)) %>% arrange(NUTS_ID)


for.OLS <- Y %>% left_join(X)
OLS1 <- lm(paste0("Y~",paste(vars.considered,collapse = "+")), data = for.OLS)
OLS2 <- lm(paste0("Y~",paste(vars.considered,collapse = "+")), data = for.OLS %>% filter(typ!="predominantly urban"))
OLS3 <- lm(paste0("Y~",paste(vars.considered,collapse = "+")), data = for.OLS %>% filter(typ=="predominantly rural"))
stargazer(OLS1,OLS2,OLS3, type="html")

html_output <- capture.output(
  stargazer(OLS1, OLS2,OLS3, type = "html",
            omit.stat = c("rsq", "f", "ser"),  # Omit R-squared, F-stat, and Std. Error
            report = "vc*",                    # Coefficients only
            title = "")
)

# Save to an HTML file
output_file <- "stargazer_output.html"
writeLines(html_output, output_file)


X1 <- model.matrix(OLS1)
Y1 <- dependent_var <- model.response(model.frame(OLS1))
X2 <- model.matrix(OLS2)
Y2 <- dependent_var <- model.response(model.frame(OLS2))
X3 <- model.matrix(OLS3)
Y3 <- dependent_var <- model.response(model.frame(OLS3))


res <- NonSpatial_SSVS(Y1,X1, ssvs_constant = 10)
res1 <- data.frame(var=colnames(X1), S_PIP1=rowMeans(res$postg), S_par1=round(rowMeans(res$postb),4)) %>% arrange(desc(S_PIP1))

res <- NonSpatial_SSVS(Y2,X2, ssvs_constant = 10)
res2 <- data.frame(var=colnames(X2), S_PIP2=rowMeans(res$postg), S_par2=round(rowMeans(res$postb),4)) %>% arrange(desc(S_PIP2))

res <- NonSpatial_SSVS(Y3,X3, ssvs_constant = 10)
res3 <- data.frame(var=colnames(X3), S_PIP3=rowMeans(res$postg), S_par3=round(rowMeans(res$postb),4)) %>% arrange(desc(S_PIP3))

table.SSVS <- res1 %>% left_join(res2) %>% left_join(res3)

# Generate an HTML table with borders
html_table <- htmlTable(
  table.SSVS,
  css.table = "border-collapse: collapse; width: 30%; border: 1px solid black;",
  css.thead = "background-color: #f2f2f2; font-weight: bold; border: 1px solid black;",
  css.cell = "border: 1px solid black; padding: 5px;"
)

# Save to an HTML file
writeLines(html_table, "table_SSVS.html")







# 
# 
# 
# 
# res.BMA = bms(X.data=cbind(Y1,X1[,-1]),burn=1000,iter=10000,nmodel=100, mprior="fixed", g="UIP",  mprior.size=5, user.int=T)
# res.BMA1 <- data.frame(coef(res.BMA,exact=TRUE))[,c(1,2)]
# colnames(res.BMA1) <- c("B_PIP1", "B_par1")
# res.BMA1 <- data.frame(var=row.names(res.BMA1),res.BMA1)
# res.BMA1 <- res.BMA1 %>% arrange(desc(B_PIP1))
# 
# res.BMA <- bms(X.data=cbind(Y2,X2[,-1]),burn=1000,iter=10000,nmodel=100, mprior="fixed", g="UIP",  mprior.size=5, user.int=T)
# res.BMA2 <- data.frame(coef(res.BMA,exact=TRUE))[,c(1,2)]
# colnames(res.BMA2) <- c("B_PIP2", "B_par2")
# res.BMA2 <- data.frame(var=row.names(res.BMA2),res.BMA2)
# res.BMA2 <- res.BMA2 %>% arrange(desc(B_PIP2))
# 
# res.BMA <- bms(X.data=cbind(Y3,X3[,-1]),burn=1000,iter=10000,nmodel=100, mprior="fixed", g="UIP",  mprior.size=5, user.int=T)
# res.BMA3 <- data.frame(coef(res.BMA,exact=TRUE))[,c(1,2)]
# colnames(res.BMA3) <- c("B_PIP3", "B_par3")
# res.BMA3 <- data.frame(var=row.names(res.BMA3),res.BMA3)
# res.BMA3 <- res.BMA3 %>% arrange(desc(B_PIP3))
# 
# table.BMA <- res.BMA1 %>% left_join(res.BMA2) %>% left_join(res.BMA3)
# table.BMA[,-1] <- round(table.BMA[,-1],4)
# 
# # Generate an HTML table with borders
# html_table <- htmlTable(
#   table.BMA,
#   css.table = "border-collapse: collapse; width: 30%; border: 1px solid black;",
#   css.thead = "background-color: #f2f2f2; font-weight: bold; border: 1px solid black;",
#   css.cell = "border: 1px solid black; padding: 5px;"
# )
# 
# # Save to an HTML file
# writeLines(html_table, "table_BMA.html")
# 
# 
# 
# 
# res.all <- res1 %>% left_join(res.BMA1) %>% left_join(res2) %>% left_join(res.BMA2) %>% left_join(res3) %>% left_join(res.BMA3)
# # Generate an HTML table with borders
# res.all[,-1] <- round(res.all[,-1],4)
# html_table <- htmlTable(
#   res.all,
#   css.table = "border-collapse: collapse; width: 60%; border: 1px solid black;",
#   css.thead = "background-color: #f2f2f2; font-weight: bold; border: 1px solid black;",
#   css.cell = "border: 1px solid black; padding: 5px;"
# )
# 
# # Save to an HTML file
# writeLines(html_table, "table_all.html")
# 
# 
# 
# 
# A <- data.frame(value=rnorm(10000, 0, 2), distribution="slab")
# B <- data.frame(value=rnorm(10000, 0, 1/2), distribution="spike")
# 
# C <- rbind(A,B)
# 
# #### plot number 1
# ggplot(C, aes(x = value, fill = distribution, color = distribution)) +
#   geom_density(alpha = 0.5) +
#   geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 1) +
#   geom_vline(xintercept = 1.3, color = "darkgreen", linetype = "dashed", size = 0.7) +
#   geom_vline(xintercept = -0.25, color = "brown", linetype = "dashed", size = 0.7) +
#   scale_x_continuous(name = "parameter", limits = c(-5,5)) +
#   scale_y_continuous(name = "Density") +
#   labs(title = "SSVS in a nutshell")+
#   theme_minimal() +
#   theme(legend.title = element_blank(), legend.position = "bottom")
# 
# ggsave("./plots/SSVS.png", last_plot() ,width = 12, height = 8, units = "cm")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# lambda <- 1000  # Small regularization term
# XtX_regularized <- t(X1) %*% X1 + diag(lambda, ncol(X1))
# XtX_inv <- solve(XtX_regularized)


