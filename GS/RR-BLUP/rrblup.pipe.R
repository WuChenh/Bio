library(rrBLUP)
library(Metrics)
# R2
R2.calc <- function(y_tst, y_pred) {
  n <- length(y_tst)
  ssr <- sum(se(y_pred, y_tst))
  sst <- sum(se(y_tst, rep(mean(y_tst), n)))
  r2 <- 1-(ssr/sst)
  return(r2)
}
R2.adj.calc <- function(r2, n, p) {
  return(1-((1-r2)*(n-1)/(n-p)))
}
#
rrblup.pipe <- function(x_trn, y_trn, x_tst, y_tst) {
  source("rrblup.pipe.R")
  pred_mod <- mixed.solve(y_trn, x_trn, SE=TRUE)
  #write(x='---MSol Done.', file='mark.txt', append=TRUE)############
  pred <- (x_tst %*% as.matrix(pred_mod$u))[, 1] + as.numeric(pred_mod$beta)
  #write(x='---Pred Done.', file='mark.txt', append=TRUE)############
  r2 <- R2.calc(y_tst, pred)
  r2.adj <- R2.adj.calc(r2, nrow(x_tst), ncol(x_tst))
  pCor <- cor(pred, y_tst)
  pMSE <- mse(pred, y_tst)
  pMAE <- mae(pred, y_tst)
  p_value <- t.test(pred, y_tst, var.equal = TRUE)$p.value
  return(list(R2=r2, R2.adj=r2.adj,
              corr=pCor, mse=pMSE, mae=pMAE, p_value=p_value,
              mod=pred_mod, y_pred=pred, y_test=y_tst)) #x_tst=x_tst
}
