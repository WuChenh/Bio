rrBLUP_train2test <- function(train, test) {
  library(rrBLUP)
  library(mltools)
  library(Metrics)
  #accuracy = matrix(nrow=CV, ncol=1)
  pred_mod <- mixed.solve(train[[2]], train[[1]], SE=TRUE)
  pred <- (test[[1]] %*% as.matrix(pred_mod$u))[, 1] + as.numeric(pred_mod$beta)
  pCor <- cor(pred, test[[2]])
  pMSE <- mse(pred, test[[2]])
  pMAE <- mae(pred, test[[2]])
  #mean.a <- mean(accuracy)
  #sd.a <- sd(accuracy)
  #p_value <- (t.test(accuracy, alternative = "two.sided"))$p.value
  #return(list(mean=mean.a, sd=sd.a, p_value=p_value, CV=CV, mod=pred_mod))
  return(list(MSE=pMSE, Cor=pCor, MAE=pMAE, y_pred=pred, y=test[[2]], mod=pred_mod))
}
