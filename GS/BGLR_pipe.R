BGLR_pipe <- function(x, y, model, nIter, burnIn, saveAt=NULL,
                      x__test, y__test) {
  library(BGLR)
  library(mltools)
  library(Metrics)
  bayes <- BGLR(y=y, ETA=list(list(X=x, model=model)), 
                      nIter=nIter, burnIn=burnIn, saveAt=saveAt)
  nam_bayes <- names(bayes)
  bayes[[length(bayes)+1]] <- cor(bayes$yHat, bayes$y)
  bayes[[length(bayes)+1]] <- mse(bayes$yHat, bayes$y)
  bayes[[length(bayes)+1]] <- mae(bayes$yHat, bayes$y)
  # test # no random effects
  pred_test <- x__test %*% (bayes[["ETA"]][[1]][["b"]]) + bayes$mu
  bayes[[length(bayes)+1]] <- cor(pred_test, y__test)
  bayes[[length(bayes)+1]] <- mse(pred_test, y__test)
  bayes[[length(bayes)+1]] <- mae(pred_test, y__test)
  bayes[[length(bayes)+1]] <- t.test(pred_test, y__test)$p.value
  bayes[[length(bayes)+1]] <- y__test
  bayes[[length(bayes)+1]] <- pred_test
  names(bayes) <- c(nam_bayes,
                    "cor_train", "mse_train", "mae_train",
                    "cor_test", "mse_test", "mae_test", "p_value_test",
                    "y_test", "pred_test")
  return(bayes)
}

BGLR_SingleTrait_para <- function(dataSet,
                                  nIter=10000,
                                  burnIn=2000, 
                                  bayesModel=NULL,
                                  isCluster=TRUE,
                                  nam_data=NULL) {
  library(parallel)
  library(foreach)
  library(doParallel)
  if (is.null(bayesModel)) {
    model_list <- c("BRR","BayesA","BL","BayesB","BayesC")
  } else { model_list <- c(bayesModel) }
  num_md <- length(model_list)
  
  # saveAt name pre
  if (isCluster) {
    isCls <- "subp"
  } else {
    isCls <- "nosubp"
  }
  if (is.null(nam_data)) {
    saveAt_name_pre <- paste(c("saved_", isCls), collapse = "")
  } else {
    reg_tk <- regexec("t0.[1-9]+k[0-9][0-9]", nam_data)
    tk <- substring(nam_data, reg_tk[[1]][1], (reg_tk[[1]][1] + attributes(reg_tk[[1]])[[1]] - 1))
    saveAt_name_pre <- paste(c("saved_", isCls, "_", tk), collapse = "")
  }
  
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
      cores = detectCores() - 2
      if (num_tt < cores) { cores = num_tt }
      cl <- makeCluster(cores) #type = "SOCK"|"FORK"
      registerDoParallel(cl)
      tmp_cls <- foreach(cls = 1:num_cl) %dopar% {
        source('BGLR_pipe.R')
        # Skip cluster which has less than 10 training samples.
        if (nrow(as.matrix(dataSet[[cls]][[1]][[1]][[1]]$train$phen_train)) < 10) {
          tmp_md <- "This cluster has less than 10 training samples."
        } else {
          tmp_md <- BGLR_for_md_model(dataSet[[cls]], isCluster, model_list,
                                      num_md, num_tt, nam_ph,
                                      ph, nIter, burnIn, saveAt_name_pre,
                                      cls, nam_cl)
        }
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
      source('BGLR_pipe.R')
      BGLR_for_md_model(dataSet, isCluster, model_list,
                        num_md, num_tt, nam_ph,
                        ph, nIter, burnIn, saveAt_name_pre)
    }
    stopCluster(cl)
    # Parallel end
    names(para_ph) = nam_ph
    return(para_ph)
  }
}

BGLR_for_md_model <- function(data_pre, isCluster, model_list,
                              num_md, num_tt, nam_ph,
                              ph, nIter, burnIn, saveAt_name_pre,
                              cls=NA, nam_cl=NA) {
  tmp_md = list()
  for (md in 1:num_md) {
    tmp_tt = list()
    result_t_mx <- matrix(NA, num_tt, 4,
                          dimnames = list(seq(1:num_tt), c("p_value","MSE","MAE","Cor")))
    for (t in 1:num_tt) {
      x_train = rbind(data_pre[[t]][[1]][[1]][[1]][[1]],
                      data_pre[[t]][[1]][[1]][[2]][[1]])
      y_train = as.matrix(rbind(data_pre[[t]][[1]][[1]][[1]][[2]],
                                data_pre[[t]][[1]][[1]][[2]][[2]])[ , ph])
      x__test = data_pre[[t]][[2]][[1]]
      y__test = as.matrix((data_pre[[t]][[2]][[2]])[ , ph])
      # Name saveAt
      if (isCluster) {
        saveAt <- paste(c(saveAt_name_pre, "_", model_list[md], "_", nam_cl[cls], "_", nam_ph[ph], "_", t, "_"), collapse = "")
      } else {
        saveAt <- paste(c(saveAt_name_pre, "_", model_list[md], "_", nam_ph[ph], "_", t, "_"), collapse = "")
      }
      print(saveAt)
      # BGLR
      BGLR_out = BGLR_pipe(x_train, y_train,
                           model_list[md], nIter, burnIn, saveAt,
                           x__test, y__test)
      tmp_tt[[t]] = BGLR_out
      result_t_mx[t, 1] <- BGLR_out[["p_value_test"]]
      result_t_mx[t, 2] <- BGLR_out[["mse_test"]]
      result_t_mx[t, 3] <- BGLR_out[["mae_test"]]
      result_t_mx[t, 4] <- BGLR_out[["cor_test"]]
    }
    mean_mse <- mean(result_t_mx[, 2])
    mean_mae <- mean(result_t_mx[, 3])
    mean_cor <- mean(result_t_mx[, 4])
    sd_mse <- sd(result_t_mx[, 2])
    sd_mae <- sd(result_t_mx[, 3])
    sd_cor <- sd(result_t_mx[, 4])
    p_value_mse <- t.test(result_t_mx[, 2])$p.value
    p_value_mae <- t.test(result_t_mx[, 3])$p.value
    p_value_cor <- t.test(result_t_mx[, 4])$p.value
    tmp_md[[md]] = list(result=tmp_tt, resultMMC=result_t_mx,
                        mean_mse=mean_mse, mean_mae=mean_mae, mean_cor=mean_cor,
                        sd_mse=sd_mse, sd_mae=sd_mae, sd_cor=sd_cor,
                        p_value_mse=p_value_mse, p_value_mae=p_value_mae, p_value_cor=p_value_cor)
  }
  names(tmp_md) = model_list
  return(tmp_md)
}
