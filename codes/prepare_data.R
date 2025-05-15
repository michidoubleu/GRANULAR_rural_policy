###### prepare Y
pop <- readRDS("output/popdata_prepared.rds")

Y.info <- pop %>% filter(variable==Y.spec) %>% arrange(NUTS_ID) %>% dplyr::select(NUTS_ID,typ,value) %>% rename("Y"="value")
Y.info$typ <- as.factor(Y.info$typ)

if(region.spec!="full"){
  if(region.spec=="no.urb"){
    Y.info <- Y.info %>% filter(typ!="predominantly urban")
  } else if(region.spec=="rural") {
    Y.info <- Y.info %>% filter(typ=="predominantly rural")
  } else {
    warnings("region.spec not known, running full region model")
  }
}
Y <- as.matrix(Y.info$Y)

for.X <- pop %>% filter(variable=="pop2011", NUTS_ID %in% Y.info$NUTS_ID) %>% arrange(NUTS_ID) %>% dplyr::select(NUTS_ID,typ,value) %>% rename("initial_pop"="value") %>% mutate(initial_pop_log=log(initial_pop))


###### prepare X & Z
explain <- readRDS("output/predictors.rds")

explain <- for.X %>% left_join(explain) %>% arrange(NUTS_ID)

explain = explain %>%
  # Transform CAP payments data by creating Pillar I and II variables
  mutate(
    `Pillar I` = COUP + DECOUP + MARKET,
    `Pillar II` = HC + COOP + PC + GRD + FOR + AEP +
      EARLY + N2K + LFA + TA + DIV,
    CAP = `Pillar I` + `Pillar II`,
    `Pillar II - area` = HC + COOP + PC + GRD + FOR + AEP +
      EARLY + N2K + LFA + TA + DIV,
    `Pillar II - project` = HC + COOP + PC + GRD + FOR + AEP +
      EARLY + N2K + LFA + TA + DIV,
  ) %>%
  mutate(across(
    c(gdp, starts_with("gva")),
    ~ .x / initial_pop * 10^6,
    .names = "pc_{.col}"
  )) %>%
  mutate(ESIF=TransConstr + EnvN2K + BuildConstr + RiskPrev + Brownfield + EnergyConstr
  ) %>%
  mutate(across(c("CAP",starts_with("Pillar"),starts_with("garrone_"),"HC",
                  "COOP","PC","GRD","FOR","AEP",
                  "EARLY","N2K","LFA","TA","DIV","MARKET",
                  "DECOUP","COUP","MISC"),
                ~.x/ 10^6/ gva_A   )
  ) %>% mutate(ESIF = ESIF / 10^6 / gdp) %>%
  mutate(across(c("gdp", "pc_gdp",starts_with("gva")),
                ~log(.x))
  )




X.info <- explain %>% arrange(NUTS_ID)
X <- X.info %>% dplyr::select(all_of(vars.considered)) %>% as.matrix()



### prep Z

###### prepare W
shape <- readRDS("output/final_NUTS.rds")

dummy.OLS <- as.data.frame(shape) %>% left_join(Y.info) %>% mutate(MOUNT_TYPE=ifelse(MOUNT_TYPE==4,0,1), COAST_TYPE=ifelse(COAST_TYPE==3,0,1), east=ifelse(substr(NUTS_ID,1,2)%in%c("BG", "CZ", "HU", "PL", "RO", "SK"),1,0), nordic=ifelse(substr(NUTS_ID,1,2)%in%c("NO","SE", "FI"),1,0), typ=as.factor(typ), country=as.factor(substr(NUTS_ID,1,2))) %>%
mutate(
  EU15 = ifelse(substr(NUTS_ID,1,2) %in% c("AT","BE","DK","DE","FI","FR","EL",
                                               "IE","IT","LU","NL","PT","ES","UK","SE"),1,0)
) %>% arrange(NUTS_ID)

### additional dummies
dummyOLS <- lm(paste0("Y~east+nordic+MOUNT_TYPE+COAST_TYPE+EU15+typ-1"), data = dummy.OLS)
Z.info <- model.matrix(dummyOLS)
colnames(Z.info) <- gsub("typ","",colnames(Z.info))
Z.info <- bind_cols(NUTS_ID=Y.info$NUTS_ID,Z.info)

Z <- Z.info %>% arrange(NUTS_ID) %>% dplyr::select(any_of(dummies.considered)) %>% as.matrix()

country.dummies <- lm(paste0("Y~CNTR_CODE"), data = dummy.OLS)
country.dummies <- model.matrix(country.dummies)
colnames(country.dummies) <- gsub("CNTR_CODE","",colnames(country.dummies))
country.dummies <- as.matrix(country.dummies[,-1])

if(add.c.dummies){
  Z <- Z %>% bind_cols(country.dummies) %>% as.matrix()
}

