library(rrBLUP)
library(Metrics)
rrblup.pipe <- function(x_trn, y_trn, x_tst, y_tst) {
  pred_mod <- mixed.solve(y_trn, x_trn, SE=TRUE)
  #write(x='---MSol Done.', file='mark.txt', append=TRUE)############
  pred <- (x_tst %*% as.matrix(pred_mod$u))[, 1] + as.numeric(pred_mod$beta)
  #write(x='---Pred Done.', file='mark.txt', append=TRUE)############
  pCor <- cor(pred, y_tst)
  pMSE <- mse(pred, y_tst)
  pMAE <- mae(pred, y_tst)
  p_value <- t.test(pred, y_tst, var.equal = TRUE)$p.value
  return(list(corr=pCor, mse=pMSE, mae=pMAE, p_value=p_value,
              mod=pred_mod, y_pred=pred, y_test=y_tst)) #x_tst=x_tst
}
