#KFoldCV

KFoldCV <- function(dataO, trait=1, algo.fn="bglr.pipe",
                    k=10, rep.k=30, seed.rep=seq(101,130)) {
  library(foreach)
  library(doParallel)
  source(paste0(algo.fn, '.R'))
  func <- .GlobalEnv[[algo.fn]]
  num.sam <- nrow(dataO[[1]])
  print(paste0('number of samples: ', num.sam)) ##-------------------
  kcv <- KFoldCV.id(num.sam, k, rep.k, seed.rep)
  # Parallel begin
  cores <- detectCores() - 2
  if (rep.k < cores) { cores <- rep.k }
  print(paste0('cores: ', cores)) ##-----------------------
  cl <- makeCluster(cores) #type = "SOCK"|"FORK"
  registerDoParallel(cl)
  #rep.all <- foreach(rpn = 1:rep.k, .export='func', .verbose=F) %dopar% {
  rep.all <- foreach(rpn = 1:rep.k) %dopar% {
    kn <- 1
    kn.all <- list()
    while (kn <= k) {
      trn <- kcv[[rpn]][[kn]][["trn"]]
      tst <- kcv[[rpn]][[kn]][["tst"]]
      g.trn <- dataO[[1]][trn,]
      g.tst <- dataO[[1]][tst,]
      p.trn <- dataO[[2]][trn, trait]
      p.tst <- dataO[[2]][tst, trait]
      #e.trn <- dataO[[4]][trn, ]
      #e.tst <- dataO[[4]][tst, ]
      if (algo.fn=='bglr.pipe') {
        saveAt <- paste0('z_tra', trait, '_k', k, '_rn', rpn, '_kn', kn)
        kn.all[[kn]] <- func(g.trn, p.trn, g.tst, p.tst, saveAt)
      } else {
        kn.all[[kn]] <- func(g.trn, p.trn, g.tst, p.tst)
      }
      kn <- kn+1
    }
    kn.all
  }
  stopCluster(cl)
  # Parallel end
  return(rep.all)
}

KFoldCV.slow <- function(dataO, trait=1, algo.fn="bglr.pipe",
                         k=10, rep.k=30, seed.rep=seq(101,130)) {
  source(paste0(algo.fn, '.R'))
  func <- .GlobalEnv[[algo.fn]]
  num.sam <- nrow(dataO[[1]])
  print(paste0('number of samples: ', num.sam)) ##-------------------
  kcv <- KFoldCV.id(num.sam, k, rep.k, seed.rep)
  rep.all <- list()
  for (rpn in 1:rep.k) {
    kn <- 1
    kn.all <- list()
    while (kn <= k) {
      trn <- kcv[[rpn]][[kn]][["trn"]]
      tst <- kcv[[rpn]][[kn]][["tst"]]
      g.trn <- dataO[[1]][trn,]
      g.tst <- dataO[[1]][tst,]
      p.trn <- dataO[[2]][trn, trait]
      p.tst <- dataO[[2]][tst, trait]
      #e.trn <- dataO[[4]][trn, ]
      #e.tst <- dataO[[4]][tst, ]
      if (algo.fn=='bglr.pipe') {
        saveAt <- paste0('z_tra', trait, '_k', k, '_rn', rpn, '_kn', kn)
        kn.all[[kn]] <- func(g.trn, p.trn, g.tst, p.tst, saveAt)
      } else {
        kn.all[[kn]] <- func(g.trn, p.trn, g.tst, p.tst)
      }
      kn <- kn+1
    }
    rep.all[[rpn]] <- kn.all
  }
  return(rep.all)
}

KFoldCV.id <- function(num.samp, k=10, rep.k=30, seed.rep=seq(101,130)) {
  rn <- 1
  out <- list()
  while (rn <= rep.k) {
    set.seed(seed.rep[rn])
    trntst.chaos <- sample(1:num.samp, num.samp)
    trntst <- list()
    len <- round(num.samp/k)
    begin <- 1
    for (kn in 1:k) {
      endn <- begin + len -1
      tst <- trntst.chaos[begin:endn]
      if (kn==k) {
        tst <- trntst.chaos[begin:num.samp]
      }
      trn <- as.matrix(setdiff(1:num.samp, tst))
      trntst[[kn]] <- list(trn=trn, tst=as.matrix(tst))
      begin <- endn +1
    }
    out[[rn]] <- trntst
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
      trntst.chaos <- sample(setdiff(1:num.samp, tst.m), num.samp-num.tst)
      ###-------------- trn and tst --------------###
      trntst <- list()
      len <- round((num.samp-num.tst)/k)
      begin <- 1
      for (kn in 1:k) {
        endn <- begin + len -1
        tst <- trntst.chaos[begin:endn]
        if (kn==k) {
          tst <- trntst.chaos[begin:(num.samp-num.tst)]
        }
        trn <- as.matrix(setdiff(1:(num.samp-num.tst), tst))
        trntst[[kn]] <- list(trn=trn, tst=as.matrix(tst))
        #print(paste(c('-----rn: ', rn, 'kn: ', kn)))
        #print(paste(c(begin, endn, length(tst))))
        begin <- endn +1
      }
      ###
      out[[rn]] <- list(tst=as.matrix(tst.m), trntst=trntst)
      rn <- rn+1
    }
    return(out)
  }
}
