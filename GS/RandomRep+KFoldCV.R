# Random repeats
Rand_Rep <- function(dataO, trait=c(1), algo.fn="bglr.pipe",
                     trnPerc=0.8, repN=10, seed.rep=seq(101,120), isScale=FALSE,
                     isWt=TRUE, isEnv=FALSE, batch_size=32,
                     once=FALSE, onceN=NA) {
  source(paste0(algo.fn, '.R'))
  func <- .GlobalEnv[[algo.fn]]
  data.g <- dataO[[1]]
  data.p <- dataO[[2]]
  data.e <- dataO[[3]]
  num.sam <- nrow(data.p)
  nam.trt <- colnames(data.p)
  num.trt <- length(nam.trt)
  trait <- sort(trait)
  print(paste0('=========== Number of samples: ', num.sam)) ##------------------
  if (isScale) {
    #data.g <- scale(data.g)
    data.p <- scale(data.p)
    data.e <- scale(data.e)
  }
  rep.all <- list()
  rpn <- 1
  if (once) {
    rpn <- onceN
    repN <- onceN
  }
  while (rpn <= repN) {
    print(paste0('=========== Rep ', rpn)) ##------------------
    set.seed(seed.rep[rpn])
    trn.rd <- sort(sample(num.sam, round(num.sam * trnPerc)))
    trn.g <- data.g[trn.rd,]
    trn.p <- data.p[trn.rd, trait]
    trn.e <- data.e[trn.rd,]
    #trn.s <- dataO[[4]][trn.rd,]
    tst.g <- data.g[-trn.rd,]
    tst.p <- data.p[-trn.rd, trait]
    tst.e <- data.e[-trn.rd,]
    #tst.s <- dataO[[4]][-trn.rd,]
    if (algo.fn=='bglr.pipe') {
      saveAt <- paste0('z_tra', trait, '_trnPc', trnPerc, '_rn', rpn)
      rep.rs <- func(trn.g, trn.p, tst.g, tst.p, saveAt)
    }
    if (algo.fn=='NN_SMT') {
      if (isWt) {
        marker_eff <- wt_combn_mean(wt_traits, rpn, trait, nam.trt)
      } else {
        marker_eff <- rep(1, ncol(tst.g))
      }
      if (isEnv) {
        rep.rs <- NN_SMT(trn.g, tst.g, trn.p, tst.p, trn.e, tst.e,
                         trait, marker_eff,
                         batch_size=batch_size,
                         activation = 'sigmoid')
      } else {
        rep.rs <- NN_SMT_Sequential(trn.g, tst.g, trn.p, tst.p, trn.e, tst.e,
                         trait, marker_eff,
                         batch_size=batch_size,
                         activation = 'sigmoid',
                         include_env = isEnv)
      }
      rep.rs <- format_output_NN(rep.rs, num.trt)
    } else {
      rep.rs <- func(trn.g, trn.p, tst.g, tst.p)
    }
    if (once) {return(rep.rs)}
    rep.all[[rpn]] <- rep.rs
    rpn <- rpn + 1
  }
  return(rep.all)
}


Rand_Rep_para <- function(dataO, trait=c(1), algo.fn="bglr.pipe",
                          trnPerc=0.8, repN=10, seed.rep=seq(101,120), isScale=FALSE,
                          isWt=TRUE, isEnv=FALSE, batch_size=32) {
  library(foreach)
  library(doParallel)
  data.g <- dataO[[1]]
  data.p <- dataO[[2]]
  data.e <- dataO[[3]]
  num.sam <- nrow(data.p)
  nam.trt <- colnames(data.p)
  num.trt <- length(nam.trt)
  trait <- sort(trait)
  print(paste0('=========== Number of samples: ', num.sam)) ##------------------
  if (isScale) {
    #data.g <- scale(data.g)
    data.p <- scale(data.p)
    data.e <- scale(data.e)
  }
  #### Parallel begin ####
  cores <- 20 #detectCores() - 2
  if (repN < cores) { cores <- repN }
  #cores <- 2#######################################################
  print(paste0('cores: ', cores)) ##-----------------------
  cl <- makeCluster(cores) #type = "SOCK"|"FORK"
  registerDoParallel(cl)
  #rep.all <- foreach(rpn = 1:repN, .export='func', .verbose=F) %dopar% {
  rep.all <- foreach(rpn = 1:repN) %dopar% {
    print(paste0('=========== Rep ', rpn)) ##------------------
    source(paste0(algo.fn, '.R'))
    func <- .GlobalEnv[[algo.fn]]
    
    set.seed(seed.rep[rpn])
    trn.rd <- sort(sample(num.sam, round(num.sam * trnPerc)))
    trn.g <- data.g[trn.rd,]
    trn.p <- data.p[trn.rd, trait]
    trn.e <- data.e[trn.rd,]
    #trn.s <- dataO[[4]][trn.rd,]
    tst.g <- data.g[-trn.rd,]
    tst.p <- data.p[-trn.rd, trait]
    tst.e <- data.e[-trn.rd,]
    #tst.s <- dataO[[4]][-trn.rd,]
    if (algo.fn=='bglr.pipe') {
      saveAt <- paste0('z_tra', trait, '_trnPc', trnPerc, '_rn', rpn)
      rep.rs <- func(trn.g, trn.p, tst.g, tst.p, saveAt)
    }
    if (algo.fn=='NN_SMT') {
      if (isWt) {
        load("wt_traits.RData")
        marker_eff <- wt_combn_mean(wt_traits, rpn, trait, nam.trt)
      } else {
        marker_eff <- rep(1, ncol(tst.g))
      }
      if (isEnv) {
        rep.rs <- NN_SMT(trn.g, tst.g, trn.p, tst.p, trn.e, tst.e,
                         trait, marker_eff,
                         batch_size=batch_size,
                         activation = 'sigmoid')
      } else {
        rep.rs <- NN_SMT(trn.g, tst.g, trn.p, tst.p, trn.e, tst.e,
                         trait, marker_eff,
                         batch_size=batch_size,
                         activation = 'sigmoid',
                         include_env = isEnv)
      }
      rep.rs <- format_output_NN(rep.rs, num.trt)
    } else {
      rep.rs <- func(trn.g, trn.p, tst.g, tst.p)
    }
    rep.rs
  }
  stopCluster(cl)
  #### Parallel end ####
  return(rep.all)
}



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