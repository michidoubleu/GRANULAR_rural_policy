files <- list.files("./output/", full.names = T)
files <- files[grepl("GRANULAR", files)]

# Run ID is parsed from the filename (GRANULAR_estim_<runID>.<jobID>.RData):
# "local" for runs of main.R, a number for runs on the IIASA HTCondor cluster
# (run 300 = results in the paper). The most recently written run is used.
cluster.ids <- sub(".*GRANULAR_estim_([A-Za-z0-9]+)\\..*", "\\1", basename(files))
mtime.by.cluster <- tapply(file.mtime(files), cluster.ids, max)
cluster.run <- names(mtime.by.cluster)[which.max(mtime.by.cluster)]
files <- files[cluster.ids == cluster.run]

# Failed jobs leave behind a 0-byte .RData file. Skip them
# here (with a warning) instead of erroring inside load() below.
is.empty <- file.size(files) == 0
if (any(is.empty)) {
  warning(sum(is.empty), " of ", length(files), " job output file(s) for run ",
          cluster.run, " are empty (failed jobs) and will be skipped: ",
          paste(basename(files[is.empty]), collapse = ", "))
  files <- files[!is.empty]
}

model_results <- list()
for(scen in files){

  load(paste0(scen))
  if(sum(grepl("ESIF",res1$setting$vars.considered))==2){
    set.name <- "policyBIG"
  }else if(sum(grepl("ESIF",res1$setting$vars.considered))==1){
    set.name <- "policySMALL"
  }else { set.name <- "Base" }

  W.label <- ifelse(is.null(res1$setting$W.spec), "knn10", res1$setting$W.spec)
  cd.label <- ifelse(!is.null(res1$setting$add.c.dummies) && res1$setting$add.c.dummies, "cFE", "noCFE")

  scen.name <- paste0(res1$setting$model,"-",
                      res1$setting$add.interaction,"-",
                      set.name,"-",
                      W.label,"-",
                      cd.label)

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
