files <- list.files("./output/", full.names = T)
files <- files[grepl("GRANULAR", files)]
files <- files[grepl(max(as.numeric(substr(files,25,28))), files)]

model_results_nopolicy <- list()
model_results_policy <- list()
scen <- files[1]
A.files <- files[1:(length(files)/2)]
B.files <- files[((length(files)/2)+1):length(files)]


for(scen in A.files){

  load(paste0(scen))

  scen.name <- paste0(res1$setting$model,"-",
                      res1$setting$region.spec,"-",
                      res1$setting$Y.spec,"-",
                      res1$setting$add.interaction,"-C.dummies:",
                      res1$setting$add.c.dummies)

  temp.res <- list()
  if(res1$model_type!="SLX"){
    temp.res[["Direkt"]] <- as.data.frame(t(round(apply(res1$post.direct,1,quantile,c(0.1,0.5,0.9)),6)))
    temp.res[["Indirekt"]] <- as.data.frame(t(round(apply(res1$post.indirect,1,quantile,c(0.1,0.5,0.9)),6)))

  } else {
    next
  }
  model_results_nopolicy[[scen.name]] <- temp.res
}




for(scen in B.files){

  load(paste0(scen))

  scen.name <- paste0(res1$setting$model,"-",
                      res1$setting$region.spec,"-",
                      res1$setting$Y.spec,"-",
                      res1$setting$add.interaction,"-C.dummies:",
                      res1$setting$add.c.dummies)

  temp.res <- list()
  if(res1$model_type!="SLX"){
    temp.res[["Direkt"]] <- as.data.frame(t(round(apply(res1$post.direct,1,quantile,c(0.1,0.5,0.9)),6)))
    temp.res[["Indirekt"]] <- as.data.frame(t(round(apply(res1$post.indirect,1,quantile,c(0.1,0.5,0.9)),6)))

  } else {
    next
  }
  model_results_policy[[scen.name]] <- temp.res
}
