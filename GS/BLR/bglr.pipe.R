library(BGLR)
library(mltools)
library(Metrics)
#
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

#BGLR_per_md
bglr.pipe <- function(x_trn, y_trn, x_tst, y_tst, saveAt='z',
                      md_lst=c("BRR","BayesA","BL","BayesB","BayesC"),
                      nIter=11000, burnIn=2000) {
  source('bglr.pipe.R') ######
  num_md <- length(md_lst)
  tmp_md <- list()
  result_mx <- matrix(NA, num_md, 5,
                      dimnames = list(md_lst, c('MSE', 'MAE', 'Corr', 'R2', 'R2_adj')))
  for (md in 1:num_md) {
    saveAt0 <- paste0("~/bglr/rslt/", saveAt, '_md', md, '_')
    BGLR_out <- BGLR_pipe(x_trn, y_trn,
                          md_lst[md], nIter, burnIn, saveAt0,
                          x_tst, y_tst)
    result_mx[md, 1] <- BGLR_out[["mse_tst"]]
    result_mx[md, 2] <- BGLR_out[["mae_tst"]]
    result_mx[md, 3] <- BGLR_out[["cor_tst"]]
    result_mx[md, 4] <- BGLR_out[["R2"]]
    result_mx[md, 5] <- BGLR_out[["R2_adj"]]
    tmp_md[[md]] <- BGLR_out
  }
  names(tmp_md) <- md_lst
  best_wh <- which(result_mx[,1]==min(result_mx[,1])) # Best: Min MSE
  best_md <- list(md_best=md_lst[best_wh], MSE=result_mx[best_wh, 1], MAE=result_mx[best_wh, 2],
                  Corr=result_mx[best_wh, 3], R2=result_mx[best_wh, 4])
  return(list(best_md=best_md, rslt_mx=result_mx, bglr=tmp_md))
}

BGLR_pipe <- function(x, y, model, nIter, burnIn, saveAt='z_',
                      x_tst, y_tst) {
  bayes <- BGLR(y=y, ETA=list(list(X=x, model=model)), 
                      nIter=nIter, burnIn=burnIn, saveAt=saveAt)
  nam_bayes <- names(bayes)
  # test # no random effects
  pred_test <- x_tst %*% (bayes[["ETA"]][[1]][["b"]]) + bayes$mu
  r2 <- R2.calc(y_tst, pred_test)
  bayes[[length(bayes)+1]] <- r2
  bayes[[length(bayes)+1]] <- R2.adj.calc(r2, nrow(x_tst), ncol(x_tst))
  bayes[[length(bayes)+1]] <- cor(pred_test, y_tst)
  bayes[[length(bayes)+1]] <- mse(pred_test, y_tst)
  bayes[[length(bayes)+1]] <- mae(pred_test, y_tst)
  #bayes[[length(bayes)+1]] <- t.test(pred_test, y_tst)$p.value
  bayes[[length(bayes)+1]] <- y_tst
  bayes[[length(bayes)+1]] <- pred_test
  #bayes[[length(bayes)+1]] <- cor(bayes$yHat, bayes$y)
  #bayes[[length(bayes)+1]] <- mse(bayes$yHat, bayes$y)
  #bayes[[length(bayes)+1]] <- mae(bayes$yHat, bayes$y)
  names(bayes) <- c(nam_bayes,
                    #"cor_trn", "mse_trn", "mae_trn",
                    'R2', 'R2_adj', "cor_tst", "mse_tst", "mae_tst", #"p_value_tst",
                    "y_tst", "pred_tst")
  return(bayes)
}
