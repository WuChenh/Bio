#data preparation for plot
library(foreach)
library(dplyr)
setwd("~/")
load("~/rice_origin.RData") #load("~/rice.origin.RData")
source("~/useful_funcs.R")
trait_list <- colnames(rice.compl.p11$pheno)
#trait_list <- sort(trait_list)

################################## COUNT SUBP ##################################
count_subp <- function(splmd_compl, splmd_origin) {
  num_compl_sampl <- nrow(splmd_compl)
  nam_compl_subp <- unique(as.character(splmd_compl[["Sub.population"]]))
  num_compl_subp <- length(nam_compl_subp)
  out_compl <- as.data.frame(matrix(NA, nrow=num_compl_subp, ncol=3))
  for (sp in 1:num_compl_subp) {
    out_compl[sp, 1] <- nam_compl_subp[sp] # Name
    out_compl[sp, 2] <- length(which(splmd_compl[["Sub.population"]]==nam_compl_subp[sp])) # Number
    out_compl[sp, 3] <- out_compl[sp, 2]/num_compl_sampl # %Percent
  }
  out_compl[, 2] <- as.numeric(out_compl[, 2])
  out_compl[, 3] <- as.numeric(out_compl[, 3])
  # original --------------------
  num_orig_sampl <- nrow(splmd_origin)
  nam_orig_subp <- unique(as.character(splmd_origin[["Sub.population"]]))
  num_orig_subp <- length(nam_orig_subp)
  out <- as.data.frame(matrix(NA, nrow = num_orig_subp, ncol = 3))
  for (sp in 1:num_orig_subp) {
    out[sp, 1] <- nam_orig_subp[sp]
    out[sp, 2] <- length(which(splmd_origin[["Sub.population"]]==nam_orig_subp[sp]))
    out[sp, 3] <- out[sp, 2]/num_orig_sampl
  }
  out[, 2] <- as.numeric(out[, 2])
  out[, 3] <- as.numeric(out[, 3])
  return(list(origin=out, compl=out_compl))
}


###################################### BGLR ####################################

rslt.collect.bglr <- function(dir_rslt='~/bglr/', grepW='r.bg', trait_list) {
  rslt.co <- foreach (n = dir(dir_rslt)[grep(grepW, dir(dir_rslt))], .combine = 'rbind') %do% {
    load(paste0(dir_rslt, n), envir = .GlobalEnv)
    varName <- names(globalenv())[grep(grepW, names(globalenv()))]
    rslt <- .GlobalEnv[[varName]]
    rm(list=varName, envir = .GlobalEnv)
    traitN <- reg_trait(varName)
    trnPerc <- reg_trnPerc(varName)
    randN <- length(rslt)
    ##
    out.rn <- foreach (rn = 1:randN, .combine = 'rbind') %do% {
      m.rslt <- rslt[[rn]][[2]][, 1:3]
      m.rslt <- m.rslt %>% cbind(row.names(m.rslt)) %>% cbind(rn) %>% cbind(trait_list[traitN]) %>% cbind(trnPerc)
      colnames(m.rslt)[4:7] <- c('Bayes', 'RandomN', 'Trait', 'TrnPerc')
      rownames(m.rslt) <- NULL
      m.rslt
    }
    ##
    out.rn <- as.data.frame(out.rn)
    for (colu in 1:3) {out.rn[[colu]] <- as.numeric(out.rn[[colu]])}
    for (colu in 4:7) {out.rn[[colu]] <- as.factor(out.rn[[colu]])}
    out.rn
  }
  return(rslt.co)
}
rslt.randrep10.bglr <- rslt.collect.bglr(trait_list = trait_list)
rslt.randrep10.bglr <- rslt.randrep10.bglr[order(as.character(rslt.randrep10.bglr$Trait)), ]
save_mT_grep('rslt.randrep10.bglr')


#################################### RR-BLUP ###################################
library(foreach)
rslt.collect.rrblup <- function(dir_rslt='~/rrblup/', grepW='r.rrb', trait_list) {
  rslt.co <- foreach (n = dir(dir_rslt)[grep(grepW, dir(dir_rslt))], .combine = 'rbind') %do% {
    load(paste0(dir_rslt, n), envir = .GlobalEnv)
    varName <- names(globalenv())[grep(grepW, names(globalenv()))]
    rslt <- .GlobalEnv[[varName]]
    rm(list=varName, envir = .GlobalEnv)
    traitN <- reg_trait(varName)
    trnPerc <- reg_trnPerc(varName)
    randN <- length(rslt)
    out.rn <- foreach (rn = 1:randN, .combine = 'rbind') %do% {
      # MSE, MAE, Cor
      m.rslt <- c(rslt[[rn]]$mse, rslt[[rn]]$mae, rslt[[rn]]$corr, rn, trait_list[traitN], trnPerc)
      m.rslt
    }
    colnames(out.rn) <- c('MSE', 'MAE', 'Corr', 'RandomN', 'Trait', 'TrnPerc')
    rownames(out.rn) <- NULL
    ##
    out.rn <- as.data.frame(out.rn)
    for (colu in c(1,2,3)) {out.rn[[colu]] <- as.numeric(out.rn[[colu]])}
    for (colu in 4:6) {out.rn[[colu]] <- as.factor(out.rn[[colu]])}
    out.rn
  }
  return(rslt.co)
}
rslt.randrep10.rrblup <- rslt.collect.rrblup(trait_list = trait_list)
rslt.randrep10.rrblup <- rslt.randrep10.rrblup[order(as.character(rslt.randrep10.rrblup$Trait)), ]
save_mT_grep('rslt.randrep10.rrblup')


##################################### BEST #####################################
# Compare mean/median corr of 30 repeats
library(foreach)
load("~/rice_origin.RData")
trait_list <- colnames(rice.compl.p11$pheno)
#
rslt.best.X <- function(rsltX, indicator=1, mean_OR_median='mean') {
  ################ Indicator: 1=MSE, 2=MAE, 3=Cor #################
  out <- foreach (tr = trait_list, .combine = 'rbind') %do% {
    pkl <- rsltX[which(rsltX$Trait==tr), ]
    pkl.tp8 <- pkl[which(pkl$TrnPerc==0.8), 1:3]
    pkl.tp9 <- pkl[which(pkl$TrnPerc==0.9), 1:3]
    if (indicator < 2.1) {
      if (mean_OR_median=='mean') {
        if (sum(pkl.tp8[,indicator]) <= sum(pkl.tp9[,indicator])) { bstP <- 0.8 } else { bstP <- 0.9 }
        whRow <- which(pkl$TrnPerc==bstP)
        outl <- c(tr, bstP, 
                  mean(pkl[whRow, 1]), mean(pkl[whRow, 2]), mean(pkl[whRow, 3]))
      } else {
        if (median(pkl.tp8[,indicator]) <= median(pkl.tp9[,indicator])) { bstP <- 0.8 } else { bstP <- 0.9 }
        whRow <- which(pkl$TrnPerc==bstP)
        outl <- c(tr, bstP, 
                  median(pkl[whRow, 1]), median(pkl[whRow, 2]), median(pkl[whRow, 3]))
      }
    } else {
      if (mean_OR_median=='mean') {
        if (sum(pkl.tp8[,indicator]) >= sum(pkl.tp9[,indicator])) { bstP <- 0.8 } else { bstP <- 0.9 }
        whRow <- which(pkl$TrnPerc==bstP)
        outl <- c(tr, bstP, 
                  mean(pkl[whRow, 1]), mean(pkl[whRow, 2]), mean(pkl[whRow, 3]))
      } else {
        if (median(pkl.tp8[,indicator]) >= median(pkl.tp9[,indicator])) { bstP <- 0.8 } else { bstP <- 0.9 }
        whRow <- which(pkl$TrnPerc==bstP)
        outl <- c(tr, bstP, 
                  median(pkl[whRow, 1]), median(pkl[whRow, 2]), median(pkl[whRow, 3]))
      }
    }
    outl
  }
  colnames(out) <- c('Trait', 'TrnPerc', 'MSE', 'MAE', 'Corr')
  rownames(out) <- NULL
  out <- as.data.frame(out)
  for (colu in 3:5) {out[[colu]] <- as.numeric(out[[colu]])}
  for (colu in 1:2) {out[[colu]] <- as.factor(out[[colu]])}
  return(out)
}
#
bst.round <- function(rslt, digits=7) {
  return(cbind(rslt[,1:2], round(rslt[,3:5], digits)))
}

rslt.bst.rrblup.mse <- rslt.best.X(rslt.randrep10.rrblup)
rslt.bst.rrblup.cor <- rslt.best.X(rslt.randrep10.rrblup, 3)
rslt.bst.bglr.mse <- rslt.best.X(rslt.randrep10.bglr)
rslt.bst.bglr.cor <- rslt.best.X(rslt.randrep10.bglr, 3)
write.table(bst.round(rslt.bst.rrblup.mse), 'rslt.best.rrblup.mse.txt', quote=F, row.names=F)
write.table(bst.round(rslt.bst.rrblup.cor), 'rslt.best.rrblup.cor.txt', quote=F, row.names=F)
write.table(bst.round(rslt.bst.bglr.mse), 'rslt.best.bglr.mse.txt', quote=F, row.names=F)
write.table(bst.round(rslt.bst.bglr.cor), 'rslt.best.bglr.cor.txt', quote=F, row.names=F)


################################### Best combn #################################
library(foreach)
best_combn <- function(bigmx, combn_list, #combn_list <- combn(11, 2)
                       tag,
                       nam_trait_all=c('FLL', 'FLW', 'PH', 'SN', 'FP', 'PF', 'SL', 'SW', 'SV', 'SSA', 'AC'),
                       return_best_combn=TRUE) { 
  combn_list <- t(combn_list)
  num_trait_all  <- (ncol(bigmx)-1)/3
  num_trait_used <- ncol(combn_list)
  tmp <- foreach(nr = 1:nrow(combn_list), .combine='rbind') %do% {
    c.corr <- bigmx[nr, combn_list[nr,]]
    c.mse  <- bigmx[nr, num_trait_all + combn_list[nr,]]
    corr_mse <- cbind(cbind(c.corr, c.mse), combn_list[nr,])
    cbind(corr_mse, nr)
  }
  colnames(tmp) <- c('Corr', "MSE", 'Trait', 'Combn')
  rownames(tmp) <- NULL
  #
  tmp <- data.frame(Corr= as.numeric(tmp[,1]), 
                    MSE=  as.numeric(tmp[,2]),
                    Trait=as.factor(nam_trait_all[tmp[,3]]),
                    Combn=as.factor(tmp[,4]) )
  #
  if(!return_best_combn) {return(tmp)}
  #
  tmp <- foreach (trt = sort(nam_trait_all), .combine='rbind') %do% {
    lines_trt <- tmp[which(tmp$Trait == trt), -4]
    lines_trt[which(lines_trt[,2] == min(lines_trt[,2])), ]
  }
  rownames(tmp) <- NULL
  tmp <- cbind(tmp, tag) ######tag#######
  colnames(tmp)[4] <- "allMethods"
  tmp$allMethods <- as.factor(tmp$allMethods)
  return(tmp)
}
