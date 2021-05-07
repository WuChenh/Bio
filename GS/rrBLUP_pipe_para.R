rrBLUP_pipe_para <- function(dataSplited, isCluster=TRUE) {
  library(parallel)
  library(foreach)
  library(doParallel)
  cores = detectCores()
  result = list()
  
  if (isCluster) {
    nam_cls = names(dataSplited)
    num_cls = length(dataSplited)
    num_TrT = length(dataSplited[[1]])
    num_phe = ncol(as.matrix(dataSplited[[1]][[1]][[1]][[1]]$train$phen_train))
    nam_phe = colnames(as.matrix(dataSplited[[1]][[1]][[1]][[1]]$train$phen_train))
    if (cores > num_TrT) { cores = num_TrT }
    
    for (cl in 1:num_cls) {
      result_ph = list()
      for (ph in 1:num_phe) {
        result_cv = list()
        result_cv_mx = matrix(NA, num_TrT, 1,
                              dimnames = list(seq(1:num_TrT), c("mse")))
        # Parallel begin
        clt = makeCluster(cores)
        registerDoParallel(clt)
        result_cv = foreach(cv = 1:num_TrT) %dopar% {
          library(dplyr)
          library(magrittr)
          source('~/gs/rice/rrBLUP_train2test.R')
          trainvalid = dataSplited[[cl]][[cv]][[1]]
          geno = rbind(trainvalid[[1]][[1]][[1]], trainvalid[[1]][[2]][[1]])
          phen = as.matrix((rbind(trainvalid[[1]][[1]][[2]], trainvalid[[1]][[2]][[2]]))[ ,ph])
          #splm = rbind(trainvalid[[1]][[1]][[3]], trainvalid[[1]][[2]][[3]])
          #envi = rbind(trainvalid[[1]][[1]][[4]], trainvalid[[1]][[2]][[4]])
          train = list(geno, phen)
          test = list(dataSplited[[cl]][[cv]][[2]][[1]],
                      as.matrix(dataSplited[[cl]][[cv]][[2]][[2]][ ,ph]))
          result_cv[[cv]] = rrBLUP_train2test(train, test)
        }
        stopCluster(clt)
        # Parallel end
        
        for (c in 1:num_TrT) {
          result_cv_mx[c, 1] = result_cv[[c]][[1]]
        }
        result_ph[[ph]] = list(result=result_cv, resultMSE=result_cv_mx)
      }
      names(result_ph) = nam_phe
      result[[cl]] = result_ph
    }
    names(result) = nam_cls
    
  } else {
    num_TrT = length(dataSplited)
    num_phe = ncol(as.matrix(dataSplited[[1]][[1]][[1]]$train$phen_train))
    nam_phe = colnames(as.matrix(dataSplited[[1]][[1]][[1]]$train$phen_train))
    result_ph = list()
    if (cores > num_TrT) { cores = num_TrT }
    
    for (ph in 1:num_phe) {
      result_cv = list()
      result_cv_mx = matrix(NA, num_TrT, 1,
                            dimnames = list(seq(1:num_TrT), c("mse")))
      # Parallel begin
      clt = makeCluster(cores)
      registerDoParallel(clt)
      result_cv = foreach(cv = 1:num_TrT) %dopar% {
        library(dplyr)
        library(magrittr)
        source('~/gs/rice/rrBLUP_train2test.R')
        trainvalid = dataSplited[[cv]][[1]]
        geno = rbind(trainvalid[[1]][[1]][[1]], trainvalid[[1]][[2]][[1]])
        phen = as.matrix((rbind(trainvalid[[1]][[1]][[2]], trainvalid[[1]][[2]][[2]]))[ ,ph])
        #splm = rbind(trainvalid[[1]][[1]][[3]], trainvalid[[1]][[2]][[3]])
        #envi = rbind(trainvalid[[1]][[1]][[4]], trainvalid[[1]][[2]][[4]])
        train = list(geno, phen)
        test = list(dataSplited[[cv]][[2]][[1]],
                    as.matrix(dataSplited[[cv]][[2]][[2]][ ,ph]))
        result_cv[[cv]] = rrBLUP_train2test(train, test)
      }
      stopCluster(clt)
      # Parallel end
      for (c in 1:num_TrT) {
        result_cv_mx[c, 1] = result_cv[[c]][[1]]
      }
      result_ph[[ph]] = list(result=result_cv, resultMSE=result_cv_mx)
    }
    names(result_ph) = nam_phe
    result = result_ph
  }
  return(result)
}