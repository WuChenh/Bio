library(rrBLUP)
library(mltools)
library(Metrics)
library(parallel)
library(foreach)
library(doParallel)

rrBLUP_pipe <- function(dataSet, isCluster=FALSE, para=FALSE) {
  if (para) {
    rrBLUP_for_pheno <- rrBLUP_for_ph_para
  } else {
    rrBLUP_for_pheno <- rrBLUP_for_ph
  }
  result <- list()
  if (isCluster) {
    nam_cls <- names(dataSet)
    num_cls <- length(dataSet)
    for (cls in 1:num_cls) {
      # Skip the cluster which has less than 10 training samples.
      if (nrow(as.matrix(dataSet[[cls]][[1]][[1]][[1]]$train$phen_train)) < 10) {
        result[[cls]] <- "This cluster has less than 10 training samples."
      } else {
        result[[cls]] <- rrBLUP_for_pheno(dataSet, isCluster, cls)
      }
    }
    names(result) <- nam_cls
  } else {
    result <- rrBLUP_for_pheno(dataSet, isCluster)
  }
  return(result)
}

rrBLUP_train2test <- function(train, test) {
  pred_mod <- mixed.solve(train[[2]], train[[1]], SE=TRUE)
  pred <- (test[[1]] %*% as.matrix(pred_mod$u))[, 1] + as.numeric(pred_mod$beta)
  pCor <- cor(pred, test[[2]])
  pMSE <- mse(pred, test[[2]])
  pMAE <- mae(pred, test[[2]])
  p_value <- t.test(pred, test[[2]], var.equal = TRUE)$p.value
  return(list(p_value=p_value, MSE=pMSE, MAE=pMAE, Cor=pCor,
              y_pred=pred, y=test[[2]], x_test=test[[1]], mod=pred_mod))
}

rrBLUP_for_ph_para <- function(dataSet, isCluster=TRUE, cls=NA) {
  data_pre <- rrBLUP_data_pre(dataSet, isCluster, cls)
  num_tt = data_pre[[1]]
  num_ph = data_pre[[2]]
  nam_ph = data_pre[[3]]
  trainValidTest_pre=data_pre[[4]]
  cores = detectCores()
  if (cores > (num_ph * 2)) { cores <- (num_ph * 2) }
  # Parallel begin
  clt = makeCluster(cores)
  registerDoParallel(clt)
  result_ph <- foreach(ph = 1:num_ph) %dopar% {
    source('rrBLUP_pipe.R')
    result_ph[[ph]] <- rrBLUP_for_t_num_tt(trainValidTest_pre, num_tt, ph)
  }
  stopCluster(clt)
  # Parallel end
  names(result_ph) <- nam_ph
  return(result_ph)
}

rrBLUP_for_ph <- function(dataSet, isCluster=TRUE, cls=NA) {
  source('rrBLUP_pipe.R')
  data_pre <- rrBLUP_data_pre(dataSet, isCluster, cls)
  num_tt = data_pre[[1]]
  num_ph = data_pre[[2]]
  nam_ph = data_pre[[3]]
  trainValidTest_pre=data_pre[[4]]
  result_ph <- list()
  for(ph in 1:num_ph) {
    result_ph[[ph]] <- rrBLUP_for_t_num_tt(trainValidTest_pre, num_tt, ph)
  }
  names(result_ph) <- nam_ph
  return(result_ph)
}

rrBLUP_data_pre <- function(dataSet, isCluster=TRUE, cls=NA) {
  if (isCluster) {
    num_tt <- length(dataSet[[1]])
    num_ph <- ncol(as.matrix(dataSet[[1]][[1]][[1]][[1]]$train$phen_train))
    nam_ph <- colnames(as.matrix(dataSet[[1]][[1]][[1]][[1]]$train$phen_train))
    trainValidTest_pre <- dataSet[[cls]]
  } else {
    num_tt <- length(dataSet)
    num_ph <- ncol(as.matrix(dataSet[[1]][[1]][[1]]$train$phen_train))
    nam_ph <- colnames(as.matrix(dataSet[[1]][[1]][[1]]$train$phen_train))
    trainValidTest_pre <- dataSet
  }
  return(list(num_tt=num_tt, num_ph=num_ph, nam_ph=nam_ph, trainValidTest_pre=trainValidTest_pre))
}

rrBLUP_for_t_num_tt <- function(trainValidTest_pre, num_tt, ph) {
  result_t <- list()
  result_t_mx <- matrix(NA, num_tt, 4,
                        dimnames = list(seq(1:num_tt), c("p_value","MSE","MAE","Cor")))
  for (t in 1:num_tt) {
    trainvalid <- trainValidTest_pre[[t]][[1]]
    geno <- rbind(trainvalid[[1]][[1]][[1]], trainvalid[[1]][[2]][[1]])
    phen <- as.matrix((rbind(trainvalid[[1]][[1]][[2]], trainvalid[[1]][[2]][[2]]))[, ph])
    #splm <- rbind(trainvalid[[1]][[1]][[3]], trainvalid[[1]][[2]][[3]])
    #envi <- rbind(trainvalid[[1]][[1]][[4]], trainvalid[[1]][[2]][[4]])
    train <- list(geno, phen)
    test <- list(trainValidTest_pre[[t]][[2]][[1]],
                 as.matrix(trainValidTest_pre[[t]][[2]][[2]][, ph]))
    result_t[[t]] <- rrBLUP_train2test(train, test)
    result_t_mx[t, 1] <- result_t[[t]][[1]]
    result_t_mx[t, 2] <- result_t[[t]][[2]]
    result_t_mx[t, 3] <- result_t[[t]][[3]]
    result_t_mx[t, 4] <- result_t[[t]][[4]]
  }
  mean_p_value <- mean(result_t_mx[, 1])
  mean_mse <- mean(result_t_mx[, 2])
  mean_mae <- mean(result_t_mx[, 3])
  mean_cor <- mean(result_t_mx[, 4])
  sd_mse <- sd(result_t_mx[, 2])
  sd_mae <- sd(result_t_mx[, 3])
  sd_cor <- sd(result_t_mx[, 4])
  p_value_mse <- t.test(result_t_mx[, 2])$p.value
  p_value_mae <- t.test(result_t_mx[, 3])$p.value
  p_value_cor <- t.test(result_t_mx[, 4])$p.value
  result_ph_ph <- list(result=result_t, resultMMC=result_t_mx,
                          mean_p_value=mean_p_value, mean_mse=mean_mse, mean_mae=mean_mae, mean_cor=mean_cor,
                          sd_mse=sd_mse, sd_mae=sd_mae, sd_cor=sd_cor,
                          p_value_mse=p_value_mse, p_value_mae=p_value_mae, p_value_cor=p_value_cor)
  return(result_ph_ph)
}
