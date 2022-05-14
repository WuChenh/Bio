# bglr qsub
argv <- commandArgs(trailingOnly = TRUE)
setwd("~/wch/bglr")
load("../rice_origin.RData")
source("../useful_funcs.R")
source("../RandomRep+KFoldCV.R")
source("bglr.pipe.R")
print(noquote('---------------------------------------'))
print(paste0('Trait=', argv[1], ',  TrnPerc=', argv[2]))
print(noquote('---------------------------------------'))

time_begin = Sys.time()
tmp_list <- list()
#tmp_list[[1]] <- KFoldCV(dataO=rice.compl, trait=as.numeric(argv[1]),
#                         algo.fn='bglr.pipe',
#                         k=as.numeric(argv[2]))
tmp_list[[1]] <- Rand_Rep(rice.compl.p11, as.numeric(argv[1]), ####### 11 traits
                          algo.fn='bglr.pipe', as.numeric(argv[2]))
time_end = Sys.time()
print(noquote('------------------------------------------'))
print(time_end - time_begin)
print(noquote('------------------------------------------'))

variable_name <- paste0('r.bglr.tra', argv[1], '.tp', argv[2])
save_name <- variable_name
names(tmp_list) <- c(variable_name)
list2env(tmp_list, .GlobalEnv)
save(list=c(variable_name), file=paste0(save_name, '.RData'), compress='xz', compression_level=9)
#save_multiThreads(variable_name, save_name, 16)