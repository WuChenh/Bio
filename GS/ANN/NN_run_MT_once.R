#NN_run.R
argv <- commandArgs(trailingOnly = TRUE)
setwd("~/GS/ANN")
#setwd("G:/GS/ANN")
library(keras)
library(Metrics)
library(foreach)
library(dplyr)
load("../rice_origin.RData")
source("../useful_funcs.R")
source("../RandomRep+KFoldCV.R")
source("NN_SMT.R")
load("wt_traits.RData")
trait_list <- colnames(rice.compl.p11$pheno)

isWt  <- as.logical(argv[1])
isEnv <- as.logical(argv[2])
tagN  <- as.numeric(argv[3])
cobN <- as.numeric(argv[4])
repN <- as.numeric(argv[5])
traN <- as.numeric(argv[6])
#traits <- strsplit(argv[3], '.', fixed = TRUE)[[1]] |> as.numeric()
batch_size <- 32 #as.numeric(argv[4])
if (isWt) { isWtO <- '_Wt'} else { isWtO <- 'uWt'}
if (isEnv) { isEnvO <- '_Env'} else { isEnvO <- 'nEnv'}
variable_name <- paste0('r.NN.', isWtO, '.', isEnvO, '.tag.', argv[3], '+', argv[4], '+', argv[5]) #####
print(variable_name)

#=================================== 1-trait ===================================
if (traN==1) {combn_list <- as.matrix(seq(1,11))}

#=================================2-trait combn=================================
if (traN==2) {combn_list <- t(combn(11, 2))}

#=================================3-trait combn=================================
#SW, SV, SSA
#AC, FLL, PH
#FLW, FP, SN
#PF, FLL, FP
if (traN==3) {
  combn_list <- foreach (traNam = c('SW', 'SV', 'SSA', 'FLL', 'PH', 'AC',
                                      'FLW', 'SN', 'FP', 'FLL', 'FP', 'PF'),
                         .combine = 'c') %do% {
    which(trait_list==traNam)
  }
  combn_list <- matrix(combn_list, ncol = 3, byrow = TRUE)
}

#=================================== 1-trait ===================================
if (traN==11) {combn_list <- matrix(seq(1,11), 1)}


#=====================================START====================================#
tmp_list <- list()
tmp_list[[1]] <- Rand_Rep(rice.compl.p11, combn_list[cobN,], 'NN_SMT', 0.8,
                         isWt=isWt, isEnv=isEnv, batch_size=batch_size,
                         once = T, onceN = repN)
#tmp_list[[1]] <- best_combn(tmp_list[[1]], combn_list, tagN)

out_line <- matrix(c(tmp_list[[1]], tagN, cobN, repN), nrow=1, byrow=TRUE)
print(out_line)
nam_csv <- paste0('r.NN.', '.tag.', argv[3], '.tn.', traN, '.', isWtO, '.', isEnvO, '.csv')
write.table(out_line, nam_csv, TRUE, FALSE, ',',
            row.names = F, col.names = F)
#names(tmp_list) <- c(variable_name)
#list2env(tmp_list, .GlobalEnv)
#save_name <- variable_name
#save(list=c(variable_name), file=paste0(save_name, '.RData'))
#save(list=c(variable_name), file=paste0(save_name, '.RData'), compress='xz', compression_level=9)