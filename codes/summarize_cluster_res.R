files <- list.files("./output/", full.names = T)
files <- files[grepl("GRANULAR", files)]
files <- files[grepl(max(as.numeric(substr(files,25,28))), files)]

cluster.run <- max(as.numeric(substr(files,25,28)))

model_results <- list()
scen <- files[1]
files <- files[1:(length(files))]


scen <- files[22]
for(scen in files){

  load(paste0(scen))
  if(sum(grepl("ESIF",res1$setting$vars.considered))==2){
    set.name <- "policyBIG"
  }else if(sum(grepl("ESIF",res1$setting$vars.considered))==1){
    set.name <- "policySMALL"
  }else { set.name <- "Base" }

  scen.name <- paste0(res1$setting$model,"-",
                      res1$setting$add.interaction,"-",
                      set.name)

  temp.res <- list()
  if(res1$setting$model!="SLX"){
    temp.res[["Direkt"]] <- rbind(as.data.frame(t(round(apply(res1$post.direct,1,quantile,c(0.01,0.05,0.1,0.5,0.9,0.95,0.99)),6))),rho=quantile(as.numeric(res1$postr,6),c(0.01,0.05,0.1,0.5,0.9,0.95,0.99)),sigma2=quantile(as.numeric(res1$posts,6),c(0.01,0.05,0.1,0.5,0.9,0.95,0.99)))

    temp.res[["Indirekt"]] <- as.data.frame(t(round(apply(res1$post.indirect,1,quantile,c(0.01,0.05,0.1,0.5,0.9,0.95,0.99)),6)))

    temp.res[["DirektSD"]] <- cbind(as.data.frame(t(round(apply(res1$post.direct,1,sd),6))),rho=sd(as.numeric(t(round(res1$postr,6)))),
                                    sigma2=sd(as.numeric(t(round(res1$posts,6)))))
    temp.res[["IndirektSD"]] <- as.data.frame(t(round(apply(res1$post.indirect,1,sd),6)))

    temp.res[["Observations"]] <- nrow(res1$Y)


  } else {
    next
  }
  model_results[[scen.name]] <- temp.res
}

saveRDS(model_results, paste0("./economic_res/model_res_",cluster.run,"_includingSD.rds"))
