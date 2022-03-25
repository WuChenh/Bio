# rrblup qsub
argv <- commandArgs(trailingOnly = TRUE)
load("~/rice.origin.RData")
setwd("~/rrblup")
source("~/useful_funcs.R")
source("~/KFold_CV.R")
source("~/rrblup/rrblup.pipe.R")
print(noquote('______________________________________'))
print(paste0('trait=', argv[1], ',  k=', argv[2]))
print(noquote('______________________________________'))

time_begin = Sys.time()
tmp_list <- list()
tmp_list[[1]] <- KFoldCV(dataO=rice.compl, trait=as.numeric(argv[1]),
                         algo.fn='rrblup.pipe',
                         k=as.numeric(argv[2]))
time_end = Sys.time()
print(noquote('__________________________________________'))
print(time_end - time_begin)
print(noquote('__________________________________________'))

variable_name <- paste0('r.rrblup.tra', argv[1], '.k', argv[2])
save_name <- paste0(variable_name, ".RData")
names(tmp_list) <- c(variable_name)
list2env(tmp_list, .GlobalEnv)
#save(list=c(variable_name), file = save_name)
save_multiThreads(variable_name, save_name, 12)
