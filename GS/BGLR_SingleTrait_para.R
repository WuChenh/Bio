BGLR_SingleTrait_para <- function(dataSet,
                                  nIter=12000,
                                  burnIn=2000, 
                                  bayesModel=NULL,
                                  isCluster=TRUE) {
  library(dplyr)
  library(magrittr)
  library(parallel)
  library(foreach)
  library(doParallel)
  
  if (is.null(bayesModel)) {
    model_list <- c("FIXED","BRR","BayesA","BL","BayesB","BayesC")
  } else { model_list <- c(bayesModel) }
  num_md <- length(model_list)
  
  if (isCluster) {
    num_cl = length(dataSet)
    num_tt = length(dataSet[[1]])
    num_ph = ncol(dataSet[[1]][[1]][[2]][[2]])
    nam_cl = names(dataSet)
    nam_ph = colnames(dataSet[[1]][[1]][[2]][[2]])
    out = list()
    for (ph in 1:num_ph) {
      tmp_cls = list()
      
      # Parallel begin
      cores = detectCores() - 1
      if (num_tt < cores) { cores = num_tt }
      cl <- makeCluster(cores) #type = "SOCK"|"FORK"
      registerDoParallel(cl)
      tmp_cls <- foreach(cls = 1:num_cl) %dopar% {
        source('~/gs/rice/BGLR_pipe.R')
        library(dplyr)
        library(magrittr)
        library(parallel)
        library(foreach)
        library(doParallel)
        
        tmp_md = list()
        for (md in 1:num_md) {
          tmp_tt = list()
          for (t in 1:num_tt) {
            x_train = rbind(dataSet[[cls]][[t]][[1]][[1]][[1]][[1]],
                            dataSet[[cls]][[t]][[1]][[1]][[2]][[1]])
            y_train = as.matrix(rbind(dataSet[[cls]][[t]][[1]][[1]][[1]][[2]],
                                      dataSet[[cls]][[t]][[1]][[1]][[2]][[2]])[ , ph])
            x__test = dataSet[[cls]][[t]][[2]][[1]]
            y__test = as.matrix((dataSet[[cls]][[t]][[2]][[2]])[ , ph])
            saveAt <- paste(c(model_list[md], nam_cl[cls], nam_ph[ph], t, "_"), collapse = "_")
            print(saveAt)
            BGLR_out = BGLR_pipe(x_train, y_train,
                                 model_list[md], nIter, burnIn, saveAt,
                                 x__test, y__test)
            tmp_tt[[t]] = BGLR_out
          }
          tmp_md[[md]] = tmp_tt
        }
        names(tmp_md) = model_list
        tmp_md
      }
      stopCluster(cl)
      # Parallel end
      
      names(tmp_cls) = nam_cl
      out[[ph]] = tmp_cls
    }
    names(out) <- nam_ph
    return(out)
    
  } else {
    num_tt <- length(dataSet)
    num_ph <- ncol(dataSet[[1]][[2]][[2]])
    nam_ph <- colnames(dataSet[[1]][[2]][[2]])
    # Parallel begin
    cores = detectCores() - 1
    if (num_ph < cores) { cores = num_ph }
    cl <- makeCluster(cores) #type = "SOCK"|"FORK"
    registerDoParallel(cl)
    para_ph <- foreach(ph = 1:num_ph) %dopar% {
      source('~/gs/rice/BGLR_pipe.R')
      library(dplyr)
      library(magrittr)
      library(parallel)
      library(foreach)
      library(doParallel)
      tmp_md = list()
      for (md in 1:num_md) {
        tmp_tt = list()
        for (t in 1:num_tt) {
          x_train = rbind(dataSet[[t]][[1]][[1]][[1]][[1]],
                          dataSet[[t]][[1]][[1]][[2]][[1]])
          y_train = as.matrix(rbind(dataSet[[t]][[1]][[1]][[1]][[2]],
                                    dataSet[[t]][[1]][[1]][[2]][[2]])[ , ph])
          x__test = dataSet[[t]][[2]][[1]]
          y__test = as.matrix((dataSet[[t]][[2]][[2]])[ , ph])
          saveAt <- paste(c(model_list[md], nam_ph[ph], t, "_"), collapse = "_")
          print(saveAt)
          BGLR_out = BGLR_pipe(x_train, y_train,
                               model_list[md], nIter, burnIn, saveAt,
                               x__test, y__test)
          tmp_tt[[t]] = BGLR_out
        }
        tmp_md[[md]] = tmp_tt
      }
      names(tmp_md) = model_list
      tmp_md
    }
    stopCluster(cl)
    # Parallel end
    names(para_ph) = nam_ph
    return(para_ph)
  }
}
