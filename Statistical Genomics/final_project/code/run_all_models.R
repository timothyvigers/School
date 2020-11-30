library(parallel)
library(nlme)
setwd("/Users/timvigers/GitHub/School/Statistical Genomics/final_project")
load("./data/final_data.RData")
# Cluster
cl = makeCluster(8,type = "FORK")
# All models in parallel
l = parLapply(cl,probes,function(p){
  mod = lme(as.formula(paste0(p,"~ clinage + SEX")),random = ~1|ID,df,na.action = na.omit)
  return(mod)
})
stopCluster(cl)