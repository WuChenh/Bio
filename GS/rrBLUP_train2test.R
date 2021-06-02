rrBLUP_train2test <- function(train, test) {
  library(rrBLUP)
  library(mltools)
  library(Metrics)
  pred_mod <- mixed.solve(train[[2]], train[[1]], SE=TRUE)
  pred <- (test[[1]] %*% as.matrix(pred_mod$u))[, 1] + as.numeric(pred_mod$beta)
  pCor <- cor(pred, test[[2]])
  pMSE <- mse(pred, test[[2]])
  pMAE <- mae(pred, test[[2]])
  p_value <- t.test(pred, test[[2]], var.equal = TRUE)$p.value
  return(list(p_value=p_value, MSE=pMSE, MAE=pMAE, Cor=pCor,
              y_pred=pred, y=test[[2]], x_test=test[[1]], mod=pred_mod))
}
