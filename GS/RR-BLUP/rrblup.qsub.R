# rrblup qsub
argv <- commandArgs(trailingOnly = TRUE)
setwd("~/wch/rrblup")
load("../rice_origin.RData")
source("../useful_funcs.R")
source("../RandomRep+KFoldCV.R")
source("rrblup.pipe.R")
print(noquote('------------------------------------'))
print(paste0('Trait=', argv[1], ',  TrnPerc=', argv[2]))
print(noquote('------------------------------------'))

time_begin = Sys.time()
tmp_list <- list()
#tmp_list[[1]] <- KFoldCV.slow(dataO=rice.compl, trait=as.numeric(argv[1]),
#                              algo.fn='rrblup.pipe',
#                              k=as.numeric(argv[2]))
tmp_list[[1]] <- Rand_Rep(rice.compl.p11, as.numeric(argv[1]), ####### 11 traits
                          algo.fn='rrblup.pipe', as.numeric(argv[2]))
time_end = Sys.time()
print(noquote('----------------------------------------'))
print(time_end - time_begin)
print(noquote('----------------------------------------'))

#if (as.numeric(argv[1]) < 10) { tnum <- paste0('0', argv[1])}
variable_name <- paste0('r.rrblup.tra', argv[1], '.tp', argv[2])
save_name <- variable_name
names(tmp_list) <- c(variable_name)
list2env(tmp_list, .GlobalEnv)
save(list=c(variable_name), file=paste0(save_name, '.RData'), compress='xz', compression_level=9)
#save_multiThreads(variable_name, save_name, 10)
