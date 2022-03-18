library(BGLR)
library(mltools)
library(Metrics)

#BGLR_per_md
bglr.pipe <- function(x_trn, y_trn, x_val, y_val,
                        model_list=c("BRR","BayesA","BL","BayesB","BayesC"),
                        nIter=10000, burnIn=2000) {
  source('bglr.pipe.R')
  num_md <- length(model_list)
  tmp_md <- list()
  result_mx <- matrix(NA, num_md, 4,
                      dimnames = list(model_list, c("Cor","MSE","MAE","p_value")))
  for (md in 1:num_md) {
    BGLR_out <- BGLR_pipe(x_trn, y_trn,
                          model_list[md], nIter, burnIn,
                          x_val, y_val)
    result_mx[md, 1] <- BGLR_out[["cor_tst"]]
    result_mx[md, 2] <- BGLR_out[["mse_tst"]]
    result_mx[md, 3] <- BGLR_out[["mae_tst"]]
    result_mx[md, 4] <- BGLR_out[["p_value_tst"]]
    tmp_md[[md]] <- list(result=BGLR_out)
  }
  names(tmp_md) <- model_list
  return(list(result_mx=result_mx, bglr.r=tmp_md))
}

BGLR_pipe <- function(x, y, model, nIter, burnIn, #saveAt=NULL,
                      x__test, y__test) {
  bayes <- BGLR(y=y, ETA=list(list(X=x, model=model)), 
                      nIter=nIter, burnIn=burnIn, saveAt='z_')
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
                    "cor_trn", "mse_trn", "mae_trn",
                    "cor_tst", "mse_tst", "mae_tst", "p_value_tst",
                    "y_tst", "pred_tst")
  return(bayes)
}
