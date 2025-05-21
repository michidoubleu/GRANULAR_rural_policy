dir.create("output")
dir.create("input")
dir.create("overleaf")

###### dependent variable

### pop change and natural change

pop <- read.csv(paste0(input.path,"/input/pop_NUTS3.csv"))
colnames(pop) <- c("NUTS_ID", "c", "d", "country", "typ", "a", "b","pop2011","pop2021")

problems <- c("TR", "AL", "UK", "IS", "RS", "MK", "ME", "LI", "CH", "NO")

pop <- pop %>% filter(!country %in% problems) %>% dplyr::select(NUTS_ID, typ, pop2011, pop2021)


births <- read.csv(paste0(input.path,"/input/Total_live_births_NUTS3.csv"))
births <- births %>% group_by(geo) %>% summarise(births=mean(OBS_VALUE, na.rm=T))
deaths <- read.csv(paste0(input.path,"/input/Total_deaths_NUTS3.csv"))
deaths <- deaths %>% group_by(geo) %>% summarise(deaths=mean(OBS_VALUE, na.rm=T))

tot.change <- births %>% left_join(deaths) %>% mutate(change=10*(births-deaths)) %>% rename("NUTS_ID"="geo")
pop <- pop %>% left_join(tot.change) %>% mutate(pop2021.corr=pop2021-change)


pop$growth <- (pop$pop2021-pop$pop2011)/pop$pop2011
pop$change <- pop$pop2021-pop$pop2011

pop$growth_corr <- (pop$pop2021.corr-pop$pop2011)/pop$pop2011
pop$change_corr <- pop$pop2021.corr-pop$pop2011



### migration

mig <- read.csv(paste0(input.path,"./input/nuts3_migration_2016.csv"))
mig <- mig %>% pivot_longer(cols = -NUTS_ID, names_to = "year", values_to = "value")
mig <- mig %>% group_by(NUTS_ID) %>% summarise(avg.mig=mean(value, na.rm = T))

pop <- pop %>% left_join(mig)

pop$mig.rate <- pop$avg.mig/pop$pop2011

pop <- pop %>% pivot_longer(cols = -c(NUTS_ID, typ), names_to = "variable", values_to = "value")

represented.nuts <- unique(pop$NUTS_ID)

saveRDS(pop, file="input/popdata_prepared.rds")





####### independent variables

##### economic vars

#gva

econ <- read.csv(paste0(input.path,"/input/economic_data.csv"))[,-1]
colnames(econ)[1] <- 'NUTS_ID'

econ <- econ %>% filter(NUTS_ID %in%represented.nuts) %>%
  group_by(NUTS_ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

econ <- econ %>% dplyr::select(-year)


#emp

emp <- read.csv(paste0(input.path,"/input/Employment_per_capita_NUTS3.csv"))
colnames(emp)[1] <- 'NUTS_ID'

emp <- emp %>% filter(NUTS_ID %in%represented.nuts) %>%
  group_by(NUTS_ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

emp <- emp %>% dplyr::select(-LEVEL_ID, -X2021)
colnames(emp)[2] <- "emp_pc"



##### accessibility
access <- read.csv(paste0(input.path,"/input/Health_and_educ_accessibility_NUTS3.csv"))[,-1]
access <- access[,c(TRUE,grepl("2020",colnames(access))[-1])]

access <- access %>% filter(NUTS_ID %in%represented.nuts) %>%
  group_by(NUTS_ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE))


##### climate vars


clim <- read.csv(paste0(input.path,"/input/EU_climate_NUTS0123_new.csv"))
colnames(clim)[1] <- 'NUTS_ID'

clim.old <- clim %>% filter(NUTS_ID %in%represented.nuts, year %in% seq(1979,1983,1)) %>%
  group_by(NUTS_ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>% dplyr::select(-year)

clim.new <- clim %>% filter(NUTS_ID %in%represented.nuts, year %in% seq(2009,2013,1)) %>%
  group_by(NUTS_ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>% dplyr::select(-year)

clim.level <- clim.new
clim.level[,-1] <- clim.new[,-1] - clim.old[,-1]
colnames(clim.level)[-1] <- paste0(colnames(clim.level)[-1], "_level")

clim.perc <- clim.new
clim.perc[,-1] <- (clim.new[,-1] - clim.old[,-1])/abs(clim.old[,-1])*100
colnames(clim.perc)[-1] <- paste0(colnames(clim.perc)[-1], "_perc")



clim.fin <- clim.perc[,c("NUTS_ID","tas_perc", "tasmax_perc", "tasmin_perc")]

clim.fin <- clim.fin %>% left_join(clim.new %>% dplyr::select(NUTS_ID,pr, sfcWindmax) %>% mutate(pr=pr*1000))

##### farm number/rents

rents2 <- read.csv(paste0(input.path,"/input/fadn_rents_nuts2_2024-08-31.csv"))
rents <- data.frame(NUTS_ID=represented.nuts, NUTS2=substr(represented.nuts,1,4))

rents <- rents %>%left_join(rents2) %>% filter(year %in% seq(2009,2013,1)) %>%
  dplyr::select(-NUTS2) %>%
  group_by(NUTS_ID) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>% dplyr::select(-year) %>%
  dplyr::select(NUTS_ID,nr_farms, rent)


##### policy vars


esif <- read.csv(paste0(input.path,"/input/NUTS3_harm_ESIF_2024-06-10.csv"))

esif <- esif %>% filter(geo %in%represented.nuts) %>%
  group_by(geo) %>% pivot_wider(id_cols = c(geo,year), names_from = "Category", values_from = "values", values_fill = 0) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>% dplyr::select(-year) %>% rename("NUTS_ID"="geo")
colnames(esif) <- colnames(esif) <- c(
  "NUTS_ID",
  "TransConstr",
  "EnvN2K",
  "BuildConstr",
  "RiskPrev",
  "Brownfield",
  "EnergyConstr"
)


CAP <- read.csv(paste0(input.path,"/input/CAPpayments_processed.csv"))

CAP <- CAP %>% filter(geo %in%represented.nuts) %>%
  group_by(geo) %>% pivot_wider(id_cols = c(geo), names_from = "type", values_from = "values", values_fill = 0) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>% rename("NUTS_ID"="geo")





##### final processing


final <- econ %>% left_join(clim.fin) %>% left_join(rents)  %>% left_join(access) %>% left_join(emp) %>% left_join(esif) %>% left_join(CAP)
final[is.na(final)] <- 0

saveRDS(final, file="input/predictors.rds")



metro <- read.xlsx(paste0(input.path,"/input/Metro-regions-NUTS-2016.xlsx"), sheet = 2)

metro <- metro %>% mutate(class=ifelse(substr(MREG_CODE,7,7)=="C","capital","STmetro"), value=1) %>% dplyr::select(NUTS_ID, class, value) %>% pivot_wider(id_cols = NUTS_ID, names_from = "class", values_fill = 0)



NUTS_shape <- st_read(paste0(input.path,"/input/NUTS/NUTS_RG_20M_2016_3035.shp"))
NUTS_shape <- NUTS_shape %>% filter(NUTS_ID%in%unique(pop$NUTS_ID)) %>% left_join(metro)
NUTS_shape[is.na(NUTS_shape)] <- 0

saveRDS(NUTS_shape, file="input/final_NUTS.rds")


