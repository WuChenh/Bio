BGLR_pipe.iter <- function(x, y, x_tst, y_tst, model, nIter, burnIn, saveAt='z_') {
  bayes <- BGLR(y=y, ETA=list(list(X=x, model=model)), 
                nIter=nIter, burnIn=burnIn, saveAt=saveAt)
  # test # no random effects
  pred_test <- x_tst %*% (bayes[["ETA"]][[1]][["b"]]) + bayes$mu
  out <- c(mse(pred_test, y_tst), mae(pred_test, y_tst), cor(pred_test, y_tst),
           R2.calc(y_tst, pred_test)) #, t.test(pred_test, y_tst)$p.value)
  return(out)
}
#
KFoldCV.BGLR.iter <- function(dataO, trait=4,
                              md="BayesB", iterL=seq(50,12000,50), burnIn=2000,
                              rpn=1, k=10, rep.k=30, seed.rep=seq(101,130)) {
  library(foreach)
  library(doParallel)
  num.sam <- nrow(dataO[[1]])
  kcv <- KFoldCV.id(num.sam, k, rep.k, seed.rep)
  len.iterL <- length(iterL)
  # Parallel begin
  cores <- detectCores() - 2
  if (len.iterL < cores) { cores <- len.iterL }
  print(paste0('cores: ', cores)) ##-----------------------
  cl <- makeCluster(cores) #type = "SOCK"|"FORK"
  registerDoParallel(cl)
  nIter.all <- foreach(nIter = iterL, .combine = 'rbind', .verbose = T) %dopar% {
    #
    source("~/bglr/iter/bglr.iter.func.R")
    library(BGLR)
    library(mltools)
    library(Metrics)
    #
    kn <- 1
    kn.all <- list()
    mse.sum <- 0
    mae.sum <- 0
    corr.sum <- 0
    R2.sum <- 0
    while (kn <= k) {
      trn <- kcv[[rpn]][[kn]][["trn"]]
      tst <- kcv[[rpn]][[kn]][["tst"]]
      g.trn <- dataO[[1]][trn,]
      g.tst <- dataO[[1]][tst,]
      p.trn <- dataO[[2]][trn, trait]
      p.tst <- dataO[[2]][tst, trait]
      #e.trn <- dataO[[4]][trn, ]
      #e.tst <- dataO[[4]][tst, ]
      saveAt <- paste0("~/bglr/iter/save/", 'z_md', md, '_nIter', nIter, '_tra', trait, '_k', k, '_rn', rpn, '_kn', kn)
      kn.all[[kn]] <- BGLR_pipe.iter(g.trn, p.trn, g.tst, p.tst, md, nIter, burnIn, saveAt)
      mse.sum <- mse.sum + kn.all[[kn]][1]
      mae.sum <- mae.sum + kn.all[[kn]][2]
      corr.sum <- corr.sum + kn.all[[kn]][3]
      R2.sum <- R2.sum + kn.all[[kn]][4]
      kn <- kn+1
    }
    c(nIter, mse.sum/k, mae.sum/k, corr.sum/k, R2.sum/k)
  }
  stopCluster(cl)
  # Parallel end
  rownames(nIter.all) <- NULL
  colnames(nIter.all) <- c('nIter', 'MSE_mean', 'MAE_mean', 'Corr_mean', 'R2_mean')
  return(nIter.all)
}

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