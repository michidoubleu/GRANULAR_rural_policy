###### prepare Y
pop <- readRDS("input/popdata_prepared.rds")

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
explain <- readRDS("input/predictors.rds")

explain <- for.X %>% left_join(explain) %>% arrange(NUTS_ID)

explain <- explain %>%

  # ---- CAP PAYMENTS: Construct aggregate CAP variables ----
mutate(
  # Pillar I includes coupled, decoupled, and market support
  `Pillar I` = COUP + DECOUP + MARKET,

  # Pillar II includes various rural development payments
  `Pillar II` = HC + COOP + PC + GRD + FOR + AEP +
    EARLY + N2K + LFA + TA + DIV,
  `Pillar II env` =  AEP + N2K + LFA ,
  `Pillar II dev` = HC + COOP + PC + GRD + FOR +
    EARLY  + TA + DIV,

  `Pillar II env` = ifelse(`Pillar II env`<0,0,`Pillar II env`),
  `Pillar II dev` = ifelse(`Pillar II dev`<0,0,`Pillar II dev`),

  # Total CAP = Pillar I + Pillar II
  CAP = `Pillar I` + `Pillar II`,

  # These two variables seem redundant but might be used differently later:
  `Pillar II - area` = HC + COOP + PC + GRD + FOR + AEP +
    EARLY + N2K + LFA + TA + DIV,
  `Pillar II - project` = HC + COOP + PC + GRD + FOR + AEP +
    EARLY + N2K + LFA + TA + DIV
) %>%

  # ---- PER CAPITA TRANSFORMATION: GDP and GVA per capita ----
mutate(
  # Normalize GDP and GVA variables to per capita values (per million people)
  across(
    c(gdp, starts_with("gva")),
    ~ .x / initial_pop * 1e6,
    .names = "pc_{.col}"
  )
) %>%

  # ---- ESIF FUNDS: Aggregate various ESIF-related payments ----
mutate(
  ESIF = TransConstr + EnvN2K + BuildConstr + RiskPrev +
    Brownfield + EnergyConstr,
  ESIF_env = EnvN2K  + RiskPrev + Brownfield ,
  ESIF_cons = TransConstr + BuildConstr + EnergyConstr
) %>%

  # ---- LOG TRANSFORMATION: CAP, Pillars, and other variables scaled by per capita GDP ----
mutate(
  across(
    c("CAP", starts_with("Pillar"), starts_with("garrone_"), "HC", "COOP", "PC",
      "GRD", "FOR", "AEP", "EARLY", "N2K", "LFA", "TA", "DIV",
      "MARKET", "DECOUP", "COUP", "MISC"),
    ~ log((.x + 1) / pc_gdp)
  )
)%>%

  # ---- LOG TRANSFORMATION: CAP, Pillars, and other variables scaled by per capita GDP ----
mutate(
  across(
    c(starts_with("ESIF")),
    ~ log((.x + 1) / pc_gdp)
  )
) %>%

mutate(
  across(
    starts_with("gva"),
    ~ .x / gdp
  )
) %>%

  # ---- BOUNDED TRANSFORMATION: Avoiding extreme values for inverse variables ----
mutate(
  across(
    ends_with("n1"),
    ~ rescale(pmin(1 / ., 1), to = c(0, 1))
  )
) %>%
  rowwise() %>%
  mutate(
    accessibility = mean(c_across(ends_with("n1")), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # ---- LOG TRANSFORMATION: GDP and per capita GDP ----
mutate(
  across(
    c("gdp", "pc_gdp"),
    ~ log(.x)
  )
)



###### prepare W
shape <- readRDS("input/final_NUTS.rds")
area.vars <- data.frame(NUTS_ID=shape$NUTS_ID, area=as.numeric(st_area(shape))/10^6)
explain <- explain %>% left_join(area.vars) %>% mutate(pop_dens=log(initial_pop/area), pop_dens_sq=pop_dens*pop_dens)

X.info <- explain %>% arrange(NUTS_ID)
X <- X.info %>% dplyr::select(any_of(vars.considered)) %>% as.matrix()



### prep Z




dummy.OLS <- as.data.frame(shape) %>% left_join(Y.info) %>% mutate(MOUNT_TYPE=ifelse(MOUNT_TYPE==4,0,1), COAST_TYPE=ifelse(COAST_TYPE==3,0,1), east=ifelse(substr(NUTS_ID,1,2)%in%c("BG", "CZ", "HU", "PL", "RO", "SK"),1,0), nordic=ifelse(substr(NUTS_ID,1,2)%in%c("NO","SE", "FI"),1,0), typ=as.factor(typ), country=as.factor(substr(NUTS_ID,1,2))) %>%
mutate(
  EU15 = ifelse(substr(NUTS_ID,1,2) %in% c("AT","BE","DK","DE","FI","FR","EL",
                                               "IE","IT","LU","NL","PT","ES","UK","SE"),1,0)
) %>% arrange(NUTS_ID)

### additional dummies






if(region.spec=="rural"){
  dummyOLS <- lm(paste0("Y~east+nordic+MOUNT_TYPE+COAST_TYPE+EU15+capital+STmetro-1"), data = dummy.OLS)
} else {
  dummyOLS <- lm(paste0("Y~east+nordic+MOUNT_TYPE+COAST_TYPE+EU15+typ+capital+STmetro-1"), data = dummy.OLS)
}
Z.info <- model.matrix(dummyOLS)
type.cols <- grepl("typ", colnames(Z.info))
colnames(Z.info) <- gsub("typ","",colnames(Z.info))
type.cols <- colnames(Z.info)[type.cols]
Z.info <- bind_cols(NUTS_ID=Y.info$NUTS_ID,Z.info)

Z <- Z.info %>% arrange(NUTS_ID) %>% dplyr::select(any_of(dummies.considered)) %>% as.matrix()

# if(add.interaction=="simple"){
#   jj <- type.cols[1]
#   if(length(type.cols)>1){
#     for(jj in type.cols){
#       temp.cols <- X * pull(Z.info[, jj])
#       colnames(temp.cols) <- paste0(jj,":",colnames(X))
#       Z <- as.matrix(bind_cols(Z,temp.cols))
#       X <- NULL
#     }
#   }
# }w


if(add.interaction=="policy"){
  jj <- type.cols[1]
  if(length(type.cols)>1){
    for(jj in type.cols[-1]){
      Pillar <- X[,grepl("Pillar", colnames(X))]
      ESIF <- X[,grepl("ESIF", colnames(X))]
      policy.X <- cbind(Pillar, ESIF)
      if(ncol(policy.X)!=0){
      temp.cols <- policy.X * pull(Z.info[, jj])
      colnames(temp.cols) <- paste0(jj,":",colnames(temp.cols))
      Z <- as.matrix(bind_cols(Z,temp.cols))
      }
    }
    # X <- X[,!grepl("Pillar", colnames(X))]
    # X <- X[,!grepl("ESIF", colnames(X))]
  }
}

if(add.interaction=="full"){
  jj <- type.cols[1]
  to.add <- c()
  if(length(type.cols)>1){
    for(jj in type.cols){
      temp.cols <- X * pull(Z.info[, jj])
      colnames(temp.cols) <- paste0(jj,":",colnames(X))
      to.add <- as.matrix(bind_cols(to.add,temp.cols))
    }
    X <- to.add
  }
}




country.dummies <- lm(paste0("Y~CNTR_CODE"), data = dummy.OLS)
country.dummies <- model.matrix(country.dummies)
colnames(country.dummies) <- gsub("CNTR_CODE","",colnames(country.dummies))
country.dummies <- as.matrix(country.dummies[,-1])

if(add.c.dummies){
  Z <- Z %>% bind_cols(country.dummies) %>% as.matrix()
}

