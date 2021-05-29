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
  test_pred <- x__test %*% (bayes[["ETA"]][[1]][["b"]]) + bayes$mu
  bayes[[length(bayes)+1]] <- cor(test_pred, y__test)
  bayes[[length(bayes)+1]] <- mse(test_pred, y__test)
  bayes[[length(bayes)+1]] <- mae(test_pred, y__test)
  names(bayes) <- c(nam_bayes,
                    "cor_train", "mse_train", "mae_train",
                    "cor_test", "mse_test", "mae_test")
  return(bayes)
}
