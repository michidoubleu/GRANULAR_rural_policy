

to.filter <- c()#

east <- read.csv(paste0(input.path,"./input/Eastern_Europe.csv"))
countries.east <- unique(east$NUTS2016_0)

###### prepare Y
pop <- readRDS("output/popdata_prepared.rds")
Y <- pop %>% filter(variable=="growth_corr") %>% arrange(NUTS_ID) %>% dplyr::select(NUTS_ID,typ,value) %>% rename("Y"="value")
Y$typ <- as.factor(Y$typ)

###### prepare W
shape <- readRDS("output/final_NUTS.rds")

###### prepare X
explan <- readRDS("output/predictors.rds")

X <- explan %>% dplyr::select(NUTS_ID, all_of(vars.considered)) %>% arrange(NUTS_ID)

specs <- list(full="",no.urb="predominantly urban", rural=c("predominantly urban", "intermediate"))

finalres <- NULL
iii <- 1
for(iii in 1:length(specs)){

  to.filter <- specs[[iii]]

dummy.OLS <- as.data.frame(shape) %>% left_join(Y) %>% filter(!typ%in%to.filter) %>% mutate(MOUNT_TYPE=ifelse(MOUNT_TYPE==4,0,1), COAST_TYPE=ifelse(COAST_TYPE==3,0,1), east=ifelse(substr(NUTS_ID,1,2)%in%c(countries.east),1,0), nordic=ifelse(substr(NUTS_ID,1,2)%in%c("NO","SE", "FI"),1,0), typ=as.factor(typ), country=as.factor(substr(NUTS_ID,1,2))) %>% arrange(NUTS_ID)

### additional dummies
dummyOLS <- lm(paste0("Y~east+nordic+MOUNT_TYPE+COAST_TYPE"), data = dummy.OLS)
curr.Z <- model.matrix(dummyOLS)[,-1]



for.OLS <- Y %>% left_join(X) %>% filter(!typ%in%to.filter) %>% arrange(NUTS_ID)
OLS1 <- lm(paste0("Y~",paste(vars.considered,collapse = "+","-1")), data = for.OLS)

centroids <- st_centroid(shape %>% filter(NUTS_ID %in% for.OLS$NUTS_ID))
centroids <- centroids[match(for.OLS$NUTS_ID, centroids$NUTS_ID), ]
coords <- st_coordinates(centroids)
W<-getWknn(coords,5)


curr.X <- model.matrix(OLS1)
Y1 <- model.response(model.frame(OLS1))
# curr.X = scale(curr.X,scale = FALSE,center = TRUE)
curr.X <- as.matrix(curr.X)

test <- SDM_SSVS(Y1,curr.X,W, curr.Z, niter = 2000, nretain = 1000)
res1 <- data.frame(names=c("Intercept",colnames(curr.X),paste0("W_",colnames(curr.X)),colnames(curr.Z)), PIP=rowMeans(test$postg), beta=round(rowMeans(test$postb),5))
res1 <- res1 %>% arrange(desc(PIP))
res1[nrow(res1)+1,] <- c("rho",1,round(rowMeans(test$postr),5))
res <- res1[!grepl("count", res1$names),]
res$spec <- names(specs)[iii]

finalres <- finalres %>% bind_rows(res)
}

saveRDS(finalres, file = "output/finalres.rds")

