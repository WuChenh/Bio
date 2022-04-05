#data preparation for plot

########################## COUNT SUBP ########################
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

################################## BGLR #################################
library(foreach)
library(dplyr)
setwd("~/")
load("~/rice.origin.RData")
source("~/useful_funcs.R")
trait_list <- colnames(rice.compl$phen)

#method_name <- 'BGLR'
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
      rslt.f1 <- rslt[[rn]][[1]][[2]] #[[2]]=[['rslt_mx']]
      for (fdn in 2:kN) {
        rslt.f1 <- rslt.f1 + rslt[[rn]][[fdn]][[2]]
      }
      m.rslt <- rslt.f1/kN
      m.rslt <- m.rslt %>% cbind(row.names(m.rslt)) %>% cbind(rn) %>% cbind(trait_list[traitN]) %>% cbind(kN)
      colnames(m.rslt)[5:8] <- c('bayes', 'randomN', 'trait', 'k')
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


################################### RR-BLUP ###################################
library(foreach)
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
      rslt.f1 <- c(rslt[[rn]][[1]][[1]], rslt[[rn]][[1]][[2]], rslt[[rn]][[1]][[3]])
      for (fdn in 2:kN) {
        rslt.f1 <- rslt.f1 + c(rslt[[rn]][[fdn]][[1]], rslt[[rn]][[fdn]][[2]], rslt[[rn]][[fdn]][[3]])
      }
      m.rslt <- rslt.f1/kN
      m.rslt <- c(m.rslt, rn, trait_list[traitN], kN)
      m.rslt
    }
    colnames(out.rn) <- c('corr', 'mse', 'mae', 'randomN', 'trait', 'k')
    rownames(out.rn) <- NULL
    ##
    out.rn <- as.data.frame(out.rn)
    for (colu in 1:3) {out.rn[[colu]] <- as.numeric(out.rn[[colu]])}
    for (colu in 4:6) {out.rn[[colu]] <- as.factor(out.rn[[colu]])}
    out.rn
  }
  return(rslt.co)
}
rslt.rep30.rrblup <- rslt.collect.rrblup(trait_list = trait_list)
save_mT_grep('rslt.rep30.rrblup')
