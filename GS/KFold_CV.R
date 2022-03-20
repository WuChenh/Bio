#KFoldCV
library(foreach)
library(doParallel)

KFoldCV <- function(dataO, trait=1, algo.fn="bglr.pipe",
                    k=10, rep.k=10, seed.rep=seq(101,110)) {
  source(paste0(algo.fn, '.R'))
  func <- .GlobalEnv[[algo.fn]]
  num.sam <- nrow(dataO[[1]])
  print(paste0('number of samples: ', num.sam))#################
  kcv <- KFoldCV.id(num.sam, k, rep.k, seed.rep)
  # Parallel begin
  cores <- detectCores() - 2
  if (rep.k < cores) { cores <- rep.k }
  print(paste0('cores: ', cores))##############
  cl <- makeCluster(cores) #type = "SOCK"|"FORK"
  registerDoParallel(cl)
  #rep.all <- foreach(rpn = 1:rep.k, .export='func', .verbose=F) %dopar% {
  rep.all <- foreach(rpn = 1:rep.k) %dopar% {
    #source('~/KFold_CV.R')
    kn <- 1
    kn.all <- list()
    while (kn <= k) {
      trn <- kcv[[rpn]][[kn]][["trn"]]
      val <- kcv[[rpn]][[kn]][["val"]]
      g.trn <- dataO[[1]][trn,]
      g.val <- dataO[[1]][val,]
      p.trn <- dataO[[2]][trn, trait]
      p.val <- dataO[[2]][val, trait]
      #e.trn <- dataO[[4]][trn, ]
      #e.val <- dataO[[4]][val, ]
      if (algo.fn=='bglr.pipe') {
        saveAt <- paste0('z_tra', trait, '_k', k, '_rn', rpn, '_kn', kn)
        kn.all[[kn]] <- func(g.trn, p.trn, g.val, p.val, saveAt)
      } else {
        kn.all[[kn]] <- func(g.trn, p.trn, g.val, p.val, trait)
      }
      kn <- kn+1
    }
    kn.all
  }
  stopCluster(cl)
  # Parallel end
  return(rep.all)
}

KFoldCV.id <- function(num.samp, k=10, rep.k=10, seed.rep=seq(101,110)) {
  rn <- 1
  out <- list()
  while (rn <= rep.k) {
    set.seed(seed.rep[rn])
    trnVal.chaos <- sample(1:num.samp, num.samp)
    trnVal <- list()
    len <- round(num.samp/k)
    begin <- 1
    for (kn in 1:k) {
      endn <- begin + len -1
      val <- trnVal.chaos[begin:endn]
      if (kn==k) {
        val <- trnVal.chaos[begin:num.samp]
      }
      trn <- as.matrix(setdiff(1:num.samp, val))
      trnVal[[kn]] <- list(trn=trn, val=as.matrix(val))
      begin <- endn +1
    }
    out[[rn]] <- trnVal
    rn <- rn+1
  }
  return(out)
}

if (F) {
  KFoldCV.id <- function(num.samp, k=10, tst=.2, rep.k=10, seed.rep=seq(101,110)) {
    num.tst <- round(num.samp * tst)
    rn <- 1
    out <- list()
    while (rn <= rep.k) {
      set.seed(seed.rep[rn])
      tst.m <- sample(1:num.samp, num.tst)
      set.seed(seed.rep[rn])
      trnVal.chaos <- sample(setdiff(1:num.samp, tst.m), num.samp-num.tst)
      ###-------------- trn and val --------------###
      trnVal <- list()
      len <- round((num.samp-num.tst)/k)
      begin <- 1
      for (kn in 1:k) {
        endn <- begin + len -1
        val <- trnVal.chaos[begin:endn]
        if (kn==k) {
          val <- trnVal.chaos[begin:(num.samp-num.tst)]
        }
        trn <- as.matrix(setdiff(1:(num.samp-num.tst), val))
        trnVal[[kn]] <- list(trn=trn, val=as.matrix(val))
        #print(paste(c('-----rn: ', rn, 'kn: ', kn)))
        #print(paste(c(begin, endn, length(val))))
        begin <- endn +1
      }
      ###
      out[[rn]] <- list(tst=as.matrix(tst.m), trnVal=trnVal)
      rn <- rn+1
    }
    return(out)
  }
}
