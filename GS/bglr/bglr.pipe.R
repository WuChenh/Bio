library(BGLR)
library(mltools)
library(Metrics)

#BGLR_per_md
bglr.pipe <- function(x_trn, y_trn, x_tst, y_tst, saveAt='z',
                      md_lst=c("BRR","BayesA","BL","BayesB","BayesC"),
                      nIter=12000, burnIn=2000) {
  source('bglr.pipe.R') ######
  num_md <- length(md_lst)
  tmp_md <- list()
  result_mx <- matrix(NA, num_md, 4,
                      dimnames = list(md_lst, c("cor","mse","mae","p_value")))
  for (md in 1:num_md) {
    saveAt0 <- paste0("~/bglr/rslt/", saveAt, '_md', md, '_')
    BGLR_out <- BGLR_pipe(x_trn, y_trn,
                          md_lst[md], nIter, burnIn, saveAt0,
                          x_tst, y_tst)
    result_mx[md, 1] <- BGLR_out[["cor_tst"]]
    result_mx[md, 2] <- BGLR_out[["mse_tst"]]
    result_mx[md, 3] <- BGLR_out[["mae_tst"]]
    result_mx[md, 4] <- BGLR_out[["p_value_tst"]]
    tmp_md[[md]] <- BGLR_out
  }
  names(tmp_md) <- md_lst
  best_wh <- which(result_mx[,1]==max(result_mx[,1]))
  best_md <- list(md_best=md_lst[best_wh], corr=result_mx[best_wh, 1],
                  mse=result_mx[best_wh, 2], mae=result_mx[best_wh, 3])
  return(list(best_md=best_md, rslt_mx=result_mx, bglr=tmp_md))
}

BGLR_pipe <- function(x, y, model, nIter, burnIn, saveAt='z_',
                      x_tst, y_tst) {
  bayes <- BGLR(y=y, ETA=list(list(X=x, model=model)), 
                      nIter=nIter, burnIn=burnIn, saveAt=saveAt)
  nam_bayes <- names(bayes)
  bayes[[length(bayes)+1]] <- cor(bayes$yHat, bayes$y)
  bayes[[length(bayes)+1]] <- mse(bayes$yHat, bayes$y)
  bayes[[length(bayes)+1]] <- mae(bayes$yHat, bayes$y)
  # test # no random effects
  pred_test <- x_tst %*% (bayes[["ETA"]][[1]][["b"]]) + bayes$mu
  bayes[[length(bayes)+1]] <- cor(pred_test, y_tst)
  bayes[[length(bayes)+1]] <- mse(pred_test, y_tst)
  bayes[[length(bayes)+1]] <- mae(pred_test, y_tst)
  bayes[[length(bayes)+1]] <- t.test(pred_test, y_tst)$p.value
  bayes[[length(bayes)+1]] <- y_tst
  bayes[[length(bayes)+1]] <- pred_test
  names(bayes) <- c(nam_bayes,
                    "cor_trn", "mse_trn", "mae_trn",
                    "cor_tst", "mse_tst", "mae_tst", "p_value_tst",
                    "y_tst", "pred_tst")
  return(bayes)
}
