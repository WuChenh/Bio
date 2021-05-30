rrBLUP_pipe <- function(dataSet, isCluster=TRUE) {
  result <- list()
  if (isCluster) {
    nam_cls <- names(dataSet)
    num_cls <- length(dataSet)
    num_TrT <- length(dataSet[[1]])
    num_phe <- ncol(as.matrix(dataSet[[1]][[1]][[1]][[1]]$train$phen_train))
    nam_phe <- colnames(as.matrix(dataSet[[1]][[1]][[1]][[1]]$train$phen_train))
    for (cl in 1:num_cls) {
      result_ph <- list()
      for (ph in 1:num_phe) {
        result_cv <- list()
        result_cv_mx <- matrix(NA, num_TrT, 3,
                              dimnames = list(seq(1:num_TrT), c("MSE","MAE","Cor")))
        for (cv in 1:num_TrT) {
          trainvalid <- dataSet[[cl]][[cv]][[1]]
          geno <- rbind(trainvalid[[1]][[1]][[1]], trainvalid[[1]][[2]][[1]])
          phen <- as.matrix((rbind(trainvalid[[1]][[1]][[2]], trainvalid[[1]][[2]][[2]]))[, ph])
          #splm <- rbind(trainvalid[[1]][[1]][[3]], trainvalid[[1]][[2]][[3]])
          #envi <- rbind(trainvalid[[1]][[1]][[4]], trainvalid[[1]][[2]][[4]])
          train <- list(geno, phen)
          test <- list(dataSet[[cl]][[cv]][[2]][[1]],
                      as.matrix(dataSet[[cl]][[cv]][[2]][[2]][, ph]))
          result_cv[[cv]] <- rrBLUP_train2test(train, test)
          result_cv_mx[cv, 1] <- result_cv[[cv]][[1]]
          result_cv_mx[cv, 2] <- result_cv[[cv]][[3]]
          result_cv_mx[cv, 3] <- result_cv[[cv]][[2]]
        }
        mean_mse <- mean(result_cv_mx[, 1])
        mean_mae <- mean(result_cv_mx[, 2])
        mean_cor <- mean(result_cv_mx[, 3])
        sd_mse <- sd(result_cv_mx[, 1])
        sd_mae <- sd(result_cv_mx[, 2])
        sd_cor <- sd(result_cv_mx[, 3])
        #print(result_cv_mx[, 1])
        #p_value_mse <- (t.test(result_cv_mx[, 1], alternative = "two.sided"))$p.value
        #p_value_mae <- (t.test(result_cv_mx[, 2], alternative = "two.sided"))$p.value
        #p_value_cor <- (t.test(result_cv_mx[, 3], alternative = "two.sided"))$p.value
        result_ph[[ph]] <- list(result=result_cv, resultMMC=result_cv_mx,
                                mean_mse=mean_mse, mean_mae=mean_mae, mean_cor=mean_cor,
                                sd_mse=sd_mse, sd_mae=sd_mae, sd_cor=sd_cor)#,
                                #p_value_mse=p_value_mse, p_value_mae=p_value_mae, p_value_cor=p_value_cor)
      }
      names(result_ph) <- nam_phe
      result[[cl]] <- result_ph
    }
    names(result) <- nam_cls
  } else {
    num_TrT <- length(dataSet)
    num_phe <- ncol(as.matrix(dataSet[[1]][[1]][[1]]$train$phen_train))
    nam_phe <- colnames(as.matrix(dataSet[[1]][[1]][[1]]$train$phen_train))
    result_ph <- list()
    for (ph in 1:num_phe) {
      result_cv <- list()
      result_cv_mx <- matrix(NA, num_TrT, 3,
                            dimnames = list(seq(1:num_TrT), c("MSE","MAE","Cor")))
      for (cv in 1:num_TrT) {
        trainvalid <- dataSet[[cv]][[1]]
        geno <- rbind(trainvalid[[1]][[1]][[1]], trainvalid[[1]][[2]][[1]])
        phen <- as.matrix((rbind(trainvalid[[1]][[1]][[2]], trainvalid[[1]][[2]][[2]]))[ ,ph])
        #splm <- rbind(trainvalid[[1]][[1]][[3]], trainvalid[[1]][[2]][[3]])
        #envi <- rbind(trainvalid[[1]][[1]][[4]], trainvalid[[1]][[2]][[4]])
        train <- list(geno, phen)
        test <- list(dataSet[[cv]][[2]][[1]],
                    as.matrix(dataSet[[cv]][[2]][[2]][, ph]))
        result_cv[[cv]] <- rrBLUP_train2test(train, test)
        result_cv_mx[cv, 1] <- result_cv[[cv]][[1]]
        result_cv_mx[cv, 2] <- result_cv[[cv]][[3]]
        result_cv_mx[cv, 3] <- result_cv[[cv]][[2]]
      }
      mean_mse <- mean(result_cv_mx[, 1])
      mean_mae <- mean(result_cv_mx[, 2])
      mean_cor <- mean(result_cv_mx[, 3])
      sd_mse <- sd(result_cv_mx[, 1])
      sd_mae <- sd(result_cv_mx[, 2])
      sd_cor <- sd(result_cv_mx[, 3])
      #p_value_mse <- (t.test(result_cv_mx[, 1], alternative = "two.sided"))$p.value
      #p_value_mae <- (t.test(result_cv_mx[, 2], alternative = "two.sided"))$p.value
      #p_value_cor <- (t.test(result_cv_mx[, 3], alternative = "two.sided"))$p.value
      result_ph[[ph]] <- list(result=result_cv, resultMMC=result_cv_mx,
                              mean_mse=mean_mse, mean_mae=mean_mae, mean_cor=mean_cor,
                              sd_mse=sd_mse, sd_mae=sd_mae, sd_cor=sd_cor)#,
                              #p_value_mse=p_value_mse, p_value_mae=p_value_mae, p_value_cor=p_value_cor)
    }
    names(result_ph) <- nam_phe
    result <- result_ph
  }
  return(result)
}
