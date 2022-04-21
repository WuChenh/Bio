# exam iteration number
# trait 4, Bayes B, k=10
setwd("~/bglr/iter")
load("~/rice.origin.RData")
library(BGLR)
library(mltools)
library(Metrics)
source("~/bglr/iter/bglr.iter.func.R")
source("~/useful_funcs.R")
source("~/KFold_CV.R")
#
#mkdir save
rslt.t4.bI49 <- KFoldCV.BGLR.iter(rice.compl, burnIn=49)
save(rslt.t4.bI49, file = 'nIter.t4.bI49.RData')
#save.image('r.bglr.nIter.RData')
#save_multiThreads('rslt', 'r.bglr.nIter0', 16)
#bayes <- BGLR(y=rice.compl$phen[,1], ETA=list(list(X=rice.compl$geno, model='BayesB')), 
#              nIter=1000, burnIn=900, saveAt='')
#cor(rice.compl$phen[,1], (rice.compl$geno %*% (bayes[["ETA"]][[1]][["b"]]) + bayes$mu))