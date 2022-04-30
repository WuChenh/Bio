#plot_data_pre old func

count_subp <- function() {
  # no NA ----------------------
  num_comp <- length(rice.compl[["splm"]][["Sub.population"]])
  nam_subp_compl <- names(rice.subp.compl)
  out_compl <- as.data.frame(matrix(NA, nrow = length(nam_subp_compl), ncol = 3))
  for (sp in 1:length(nam_subp_compl)) {
    out_compl[sp, 1] <- nam_subp_compl[sp]
    out_compl[sp, 2] <- nrow(rice.subp.compl[[sp]][[1]])
    out_compl[sp, 3] <- out_compl[sp, 2]/num_comp
  }
  out_compl[, 2] <- as.numeric(out_compl[, 2])
  out_compl[, 3] <- as.numeric(out_compl[, 3])
  # original --------------------
  num_orig <- length(rice.origin[["SD1"]][["Sub.population"]])
  nam_subp <- names(rice.subp)
  out <- as.data.frame(matrix(NA, nrow = length(nam_subp), ncol = 3))
  for (sp in 1:length(nam_subp)) {
    out[sp, 1] <- nam_subp[sp]
    out[sp, 2] <- nrow(rice.subp[[sp]][[1]])
    out[sp, 3] <- out[sp, 2]/num_orig
  }
  out[, 2] <- as.numeric(out[, 2])
  out[, 3] <- as.numeric(out[, 3])
  return(list(origin=out, no_NA=out_compl))
}

rslt.collect.bglr <- function(dir_rslt='~/bglr/', grepW='r.bg', trait_list) {
  rslt.co <- foreach (n = dir(dir_rslt)[grep(grepW, dir(dir_rslt))], .combine = 'rbind') %do% {
    load(paste0(dir_rslt, n), envir = .GlobalEnv)
    varName <- names(globalenv())[grep(grepW, names(globalenv()))]
    rslt <- .GlobalEnv[[varName]]
    rm(list=varName, envir = .GlobalEnv)
    traitN <- reg_trait(varName)
    randN <- length(rslt)
    kN <- length(rslt[[1]])
    ##
    out.rn <- foreach (rn = 1:randN, .combine = 'rbind') %do% {
      rslt.f1 <- rslt[[rn]][[1]][[2]][, -5] #[[2]]=[['rslt_mx']]
      # Mean matrix of r2, corr... for K-fold
      for (fdn in 2:kN) {
        rslt.f1 <- rslt.f1 + rslt[[rn]][[fdn]][[2]][, -5]
      }
      m.rslt <- rslt.f1/kN
      m.rslt <- m.rslt %>% cbind(row.names(m.rslt)) %>% cbind(rn) %>% cbind(trait_list[traitN]) %>% cbind(kN)
      colnames(m.rslt)[5:8] <- c('Bayes', 'RandomN', 'Trait', 'K')
      rownames(m.rslt) <- NULL
      m.rslt
    }
    ##
    out.rn <- as.data.frame(out.rn)
    for (colu in 1:4) {out.rn[[colu]] <- as.numeric(out.rn[[colu]])}
    for (colu in 5:8) {out.rn[[colu]] <- as.factor(out.rn[[colu]])}
    out.rn
  }
  return(rslt.co)
}
rslt.rep30.bglr <- rslt.collect.bglr(trait_list = trait_list)
save_mT_grep('rslt.rep30.bglr')


rslt.collect.rrblup <- function(dir_rslt='~/rrblup/', grepW='r.rrb', trait_list) {
  rslt.co <- foreach (n = dir(dir_rslt)[grep(grepW, dir(dir_rslt))], .combine = 'rbind') %do% {
    load(paste0(dir_rslt, n), envir = .GlobalEnv)
    varName <- names(globalenv())[grep(grepW, names(globalenv()))]
    rslt <- .GlobalEnv[[varName]]
    rm(list=varName, envir = .GlobalEnv)
    traitN <- reg_trait(varName)
    randN <- length(rslt)
    kN <- length(rslt[[1]])
    ##
    out.rn <- foreach (rn = 1:randN, .combine = 'rbind') %do% {
      # MSE, MAE, Cor, R2
      rslt.f1 <- c(rslt[[rn]][[1]][[1]], rslt[[rn]][[1]][[2]], rslt[[rn]][[1]][[3]], rslt[[rn]][[1]][[4]])
      for (fdn in 2:kN) {
        rslt.f1 <- rslt.f1 + c(rslt[[rn]][[fdn]][[1]], rslt[[rn]][[fdn]][[2]], rslt[[rn]][[fdn]][[3]], rslt[[rn]][[fdn]][[4]])
      }
      m.rslt <- rslt.f1/kN
      m.rslt <- c(m.rslt, rn, trait_list[traitN], kN)
      m.rslt
    }
    colnames(out.rn) <- c('MSE', 'MAE', 'Corr', 'R2', 'RandomN', 'Trait', 'K')
    rownames(out.rn) <- NULL
    ##
    out.rn <- as.data.frame(out.rn)
    for (colu in 1:4) {out.rn[[colu]] <- as.numeric(out.rn[[colu]])}
    for (colu in 5:7) {out.rn[[colu]] <- as.factor(out.rn[[colu]])}
    out.rn
  }
  return(rslt.co)
}
rslt.rep30.rrblup <- rslt.collect.rrblup(trait_list = trait_list)
save_mT_grep('rslt.rep30.rrblup')


rslt.best.X <- function(rsltX, indicator=1, mean_OR_median='mean') {
  ################ Indicator: 1=MSE, 2=MAE, 3=Cor, 4=R2 #################
  out <- foreach (tr = trait_list, .combine = 'rbind') %do% {
    pkl <- rsltX[which(rsltX$Trait==tr), ]
    pkl.k5 <- pkl[which(pkl$K==5), 1:4]
    pkl.k10 <- pkl[which(pkl$K==10), 1:4]
    if (indicator < 2.1) {
      if (mean_OR_median=='mean') {
        if (sum(pkl.k5[,indicator]) <= sum(pkl.k10[,indicator])) { bstK <- 5 } else { bstK <- 10 }
        outl <- c(tr, bstK, 
                  mean(pkl[which(pkl$K==bstK), 1]), mean(pkl[which(pkl$K==bstK), 2]),
                  mean(pkl[which(pkl$K==bstK), 3]), mean(pkl[which(pkl$K==bstK), 4]))
      } else {
        if (median(pkl.k5[,indicator]) <= median(pkl.k10[,indicator])) { bstK <- 5 } else { bstK <- 10 }
        outl <- c(tr, bstK, 
                  median(pkl[which(pkl$K==bstK), 1]), median(pkl[which(pkl$K==bstK), 2]),
                  median(pkl[which(pkl$K==bstK), 3]), median(pkl[which(pkl$K==bstK), 4]))
      }
    } else {
      if (mean_OR_median=='mean') {
        if (sum(pkl.k5[,indicator]) >= sum(pkl.k10[,indicator])) { bstK <- 5 } else { bstK <- 10 }
        outl <- c(tr, bstK, 
                  mean(pkl[which(pkl$K==bstK), 1]), mean(pkl[which(pkl$K==bstK), 2]),
                  mean(pkl[which(pkl$K==bstK), 3]), mean(pkl[which(pkl$K==bstK), 4]))
      } else {
        if (median(pkl.k5[,indicator]) >= median(pkl.k10[,indicator])) { bstK <- 5 } else { bstK <- 10 }
        outl <- c(tr, bstK, 
                  median(pkl[which(pkl$K==bstK), 1]), median(pkl[which(pkl$K==bstK), 2]),
                  median(pkl[which(pkl$K==bstK), 3]), median(pkl[which(pkl$K==bstK), 4]))
      }
    }
    outl
  }
  colnames(out) <- c('Trait', 'K', 'MSE', 'MAE', 'Corr', 'R2')
  rownames(out) <- NULL
  out <- as.data.frame(out)
  for (colu in 3:6) {out[[colu]] <- as.numeric(out[[colu]])}
  for (colu in 1:2) {out[[colu]] <- as.factor(out[[colu]])}
  return(out)
}

bst.round <- function(rslt, digits=6) {
  return(cbind(rslt[,1:2], round(rslt[,3:6], digits)))
}

rslt.bst.rrblup.mse <- rslt.best.X(rslt.rep30.rrblup)
rslt.bst.rrblup.cor <- rslt.best.X(rslt.rep30.rrblup, 3)
rslt.bst.bglr.mse <- rslt.best.X(rslt.rep30.bglr)
rslt.bst.bglr.cor <- rslt.best.X(rslt.rep30.bglr, 3)
write.table(bst.round(rslt.bst.rrblup.mse), 'rslt.best.rrblup.mse.txt', quote=F, row.names=F)
write.table(bst.round(rslt.bst.rrblup.cor), 'rslt.best.rrblup.cor.txt', quote=F, row.names=F)
write.table(bst.round(rslt.bst.bglr.mse), 'rslt.best.bglr.mse.txt', quote=F, row.names=F)
write.table(bst.round(rslt.bst.bglr.cor), 'rslt.best.bglr.cor.txt', quote=F, row.names=F)
