#Calc weights based on BGLR

softmax1 <- function(vec, baseN=2) {
  vect <- baseN ** vec
  return(vect/sum(vect))
}
softmax2 <- function(vec) {
  absed <- abs(vec)
  return(absed/sum(absed))
}
std01 <- function(vec) { # abs then std to 0-1
  absed <- abs(vec)
  return((absed-min(absed))/(max(absed)-min(absed)))
}

find_best_bayesian <- function(dataIn, Nresult=seq(1,10)) {
  traits <- names(dataIn)
  tra <- 1
  out <- rep(NA, length(traits))
  while (tra <= length(traits)) {
    bayesian <- names(dataIn[[tra]])
    bs <- 1
    r.mean <- rep(0, length(bayesian))
    while (bs <= length(bayesian)) {
      r.mean[bs] <- mean(dataIn[[tra]][[bs]][["resultMMC"]][Nresult,3])
      bs <- bs+1
    }
    out[tra] <- bayesian[which(r.mean==max(r.mean))[1]]
    tra <- tra+1
  }
  names(out) <- traits
  return(out)
}

mean_weight_bglr <- function(datain, Ntt=seq(1,10)) {
  nn <- 2
  w_sum <- datain[[Ntt[1]]][["ETA"]][[1]][["b"]]
  num_tt <- length(Ntt)
  while (nn <= num_tt) {
    w_sum <- w_sum + datain[[Ntt[nn]]][["ETA"]][[1]][["b"]]
    nn <- nn + 1
  }
  return(w_sum/num_tt)
}

#strsplit('.9,.8', ',')[[1]] %>% as.numeric()
if (F) {
  set_weight <- function(wt_vec, wt_steps=c(0, .05, .1), # wt_vec can includes negative wt
                         step_quantile=c(.45, .55, .35, .65, .05, .95)) {
    qn <- c()
    for (sq in step_quantile) {
      qn <- c(qn, quantile(wt_vec, sq))
    }
    ceiling(which(step_quantile == sort(step_quantile)[length(step_quantile)/2])/2)
  }
}

set_weight <- function(wt_vec, wt=.2, quantile=.7) {
  wt_vec <- abs(wt_vec) # abs then reweight
  nn <- 1
  num_snp <- length(wt_vec)
  L1 <- quantile(wt_vec, quantile)
  while (nn <= num_snp) {
    if (wt_vec[nn] < L1) {
      wt_vec[nn] <- wt * wt_vec[nn]
    }
    nn <- nn + 1
  }
  return(wt_vec)
}
set_weight_2L <- function(wt_vec, wt1=.2, wt2=0,
                          quantile_1=.9, quantile_2=.3) {
  wt_vec <- abs(wt_vec) # abs then reweight
  nn <- 1
  num_snp <- length(wt_vec)
  L1 <- quantile(wt_vec, quantile_1)
  L2 <- quantile(wt_vec, quantile_2)
  while (nn <= num_snp) {
    if (wt_vec[nn] < L1) {
      if (wt_vec[nn] < L2) {
        wt_vec[nn] <- wt2 * wt_vec[nn]
      } else {
        wt_vec[nn] <- wt1 * wt_vec[nn]
      }
    }
    nn <- nn + 1
  }
  return(wt_vec)
}

get_weight_std <- function(datain, set_wt_step=2, # or 1
                           wt1=.2, quantile_1=.9,
                           wt2=0, quantile_2=.3, Nresult=seq(1,10)) {
  if (set_wt_step>1) {
    out <- std01(set_weight_2L(mean_weight_bglr(datain, Nresult), 
                               wt1, wt2, quantile_1, quantile_2))
  } else {
    out <- std01(set_weight(mean_weight_bglr(datain, Nresult),
                            wt1, quantile_1))
  }
  return(out)
}

best_mean_wt <- function(dataIn, Nresult=seq(1,10), wt_step=2,
                         wt1=.2, quantile_1=.9, wt2=0, quantile_2=.3) {
  best <- find_best_bayesian(dataIn, Nresult)
  traits <- names(dataIn)
  tra <- 1
  while (tra <= length(traits)) {
    if (tra==1) {
      out <- get_weight_std(dataIn[[tra]][[best[tra]]][['result']], wt_step,
                            wt1, quantile_1, wt2, quantile_2, Nresult)
    } else {
      out <- rbind(out, get_weight_std(dataIn[[tra]][[best[tra]]][['result']], wt_step,
                                       wt1, quantile_1, wt2, quantile_2, Nresult))
    }
    tra <- tra+1
  }
  rownames(out) <- traits
  return(list(weights=out, best.bayesian=best))
}

#################################################################
if (F) {
  set_weight <- function(wt_vec, wt=.2, quantile_Low=.1, quantile_High=.9) {
    nn <- 1
    num_snp <- length(wt_vec)
    Llow <- quantile(wt_vec, quantile_Low)
    Lhigh <- quantile(wt_vec, quantile_High)
    while (nn <= num_snp) {
      if (wt_vec[nn] > Llow && wt_vec[nn] < Lhigh) {
        wt_vec[nn] <- wt * wt_vec[nn]
      }
      nn <- nn + 1
    }
    return(wt_vec)
  }
  
  set_weight_2L <- function(wt_vec, wt1=.2, wt2=0,
                            quantile_L1=.1, quantile_H1=.9,
                            quantile_L2=.35, quantile_H2=.65) {
    nn <- 1
    num_snp <- length(wt_vec)
    Llow1 <- quantile(wt_vec, quantile_L1)
    Lhigh1 <- quantile(wt_vec, quantile_H1)
    Llow2 <- quantile(wt_vec, quantile_L2)
    Lhigh2 <- quantile(wt_vec, quantile_H2)
    while (nn <= num_snp) {
      if (wt_vec[nn] > Llow1 && wt_vec[nn] < Lhigh1) {
        if (wt_vec[nn] > Llow2 && wt_vec[nn] < Lhigh2) {
          wt_vec[nn] <- wt2 * wt_vec[nn]
        } else {
          wt_vec[nn] <- wt1 * wt_vec[nn]
        }
      }
      nn <- nn + 1
    }
    return(wt_vec)
  }
  
  get_weight_std <- function(datain, set_wt_step=2, # or 1
                             wt1=.2, quantile_L1=.1, quantile_H1=.9,
                             wt2=0, quantile_L2=.35, quantile_H2=.65) {
    if (set_wt_step>1) {
      out <- std01(set_weight_2L(mean_weight_bglr(datain), 
                                 wt1, wt2, quantile_L1, quantile_H1,
                                 quantile_L2, quantile_H2))
    } else {
      out <- std01(set_weight(mean_weight_bglr(datain),
                              wt1, quantile_L1, quantile_H1))
    }
    return(out)
  }
}
