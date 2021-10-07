#plot funcs
library(ggplot2)
library(latex2exp)

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

Collect_result <- function(is_BGLR=FALSE, is_NN=FALSE, is_rrBLUP=FALSE) {
  q1 <- NULL
  q2 <- NULL
  q3 <- NULL
  if (is_BGLR) { q1 <- 'BGLR_singleTrait_t' }
  if (is_NN) { q2 <- 'noEnv' }
  if (is_rrBLUP) { q3 <- 'rrBLUP_nosubp' }
  que <- c(q1, q2, q3)
  if (is.null(que)) { return('Input is NULL!') }
  for (r in que) {
    r_x <- names(globalenv())[grep(r, names(globalenv()))]
    
  }
}

A_random_produced_prediction <- function(subset_random, y_name, y_pred_name) {
  orig <- c()
  pred <- c()
  for (i in 1:length(subset_random)) {
    orig <- c(orig, as.numeric(as.vector(subset_random[[i]][[y_name]])))
    pred <- c(pred, as.numeric(as.vector(subset_random[[i]][[y_pred_name]])))
    if (length(orig) != length(pred)) {
      print("Unequal length!")
      return(NULL)
    }
  }
  return(list(y=orig, y_hat=pred))
}

Substring_stkb <- function(vname, is_batch=FALSE, is_k=TRUE) {
  # Auto recognize subp/nosubp
  reg_stk <- regexec("_subp_+t0.[1-9]+k[0-9][0-9]", vname)
  if (reg_stk[[1]][1] > 0) {
    tk <- substring(vname, reg_stk[[1]][1] + 1, (reg_stk[[1]][1] + attributes(reg_stk[[1]])[[1]] - 1))
    if (!is_k) {
      reg_k <- regexec("k[0-9][0-9]", tk)
      tk <- substring(tk, 1, reg_k[[1]][1]-1)
    }
  } else {
    if (is_batch) {
      reg_tk <- regexec("t0.[1-9]+k[0-9][0-9]+_+b[0-9][0-9][0-9]", vname)
    } else {
      if (!is_k) {
        reg_tk <- regexec("t0.[1-9]", vname)
      } else {
        reg_tk <- regexec("t0.[1-9]+k[0-9][0-9]", vname)
      }
    }
    tk <- substring(vname, reg_tk[[1]][1], (reg_tk[[1]][1] + attributes(reg_tk[[1]])[[1]] - 1))
  }
  return(tk)
}


# ------------------------------- N N --------------------------------- #
# The best NN for each phenotype: ##Unavailable! 
if (F) {
  best_NN_foreach_pheno <- function(activation, env=FALSE, env_and_noEnv=FALSE, is_lg_MAE=TRUE) {
    NN_plot <- Collect_NN(activation, env, env_and_noEnv, is_lg_MAE)
    tmp <- c()
    for (p in levels(NN_plot[,2])) {
      tmp <- c(tmp, which(NN_plot[,3]==max(NN_plot[,3][which(NN_plot[,2]==p)])))
    }
    mx <- NN_plot[tmp,]
    colnames(mx) <- c("arg", "phenotype", "-lg(MAE)")
    return(mx)
  }
}
##----------------------------- Useful!!!!!!!!!: ----------------------------##
Collect_NN <- function(activation, env=FALSE, env_and_noEnv=FALSE, is_lg_MAE=TRUE) {
  grepword <- paste("NN_", activation, sep = '')
  r_NN <- names(globalenv())[grep(grepword, names(globalenv()))]
  if (env_and_noEnv) {
    r_x <- r_NN
  } else {
    if (!env) {
      r_x <- r_NN[grep("noEnv", r_NN)]
    } else {
      r_x <- r_NN[grep("env", r_NN)]
    }
  }
  out <- matrix(NA, 1, 4)
  for (r in r_x) {
    tkb <- Substring_stkb(r, TRUE)
    out <- rbind(out, result_NN(.GlobalEnv[[r]], tkb))
  }
  out <- as.data.frame(out[-1, ])
  #out[, 1] <- factor(out[, 1])
  #out[, 2] <- factor(out[, 2])
  colnames(out) <- c("arg", "phenotype", "MAE", "R2")
  rownames(out) <- seq(1:nrow(out))
  out[, 3] <- as.numeric(as.character(out[, 3]))
  out[, 4] <- as.numeric(as.character(out[, 4]))
  if (is_lg_MAE) {
    out[, 3] <- -log10(out[, 3])
  }
  return(out)
}

result_NN <- function(result, tkb) {
  mx <- matrix(NA, 1, 4)
  for (ph in names(result)) {
    mx <- rbind(mx, cbind(tkb, ph, result[[ph]][["resultMLC"]][, 1],
                          (result[[ph]][["resultMLC"]][, 5])**2)) ## R-squared
  }
  colnames(mx) <- c("arg", "phenotype", "MAE", "R2")
  return(mx[-1, ])
}

NN_plot_pre <- function(activation, env, is_lg_MAE=TRUE) {
  if (env) {
    envStr <- "G×E"
  } else {
    envStr <- "G"
  }
  out <- Collect_NN(activation, env, FALSE, is_lg_MAE)
  out <- cbind(out, paste(c(activation, ', ', envStr), collapse = ""))
  colnames(out) <- c("arg", "Phenotype", "MAE", "R2", "Activation_and_Env")
  return(out)
}

# ------------------------------- BGLR --------------------------------- #
# Compare subp and not
Collect_BGLR <- function(is_subp=FALSE, is_lg_MAE=TRUE) {
  #grepword <- paste("BGLR_", sep = '')
  r_bglr <- names(globalenv())[grep('BGLR_single', names(globalenv()))]
  if (!is_subp) {
    r_x <- r_bglr[grep("t_t", r_bglr)]
    out <- matrix(NA, 1, 4)
    for (r in r_x) {
      stk <- Substring_stkb(r, FALSE, FALSE)
      out <- rbind(out, result_BGLR(.GlobalEnv[[r]], stk))
    }
  } else {
    r_x <- r_bglr[grep("_subp", r_bglr)]
    out <- matrix(NA, 1, 5)
    for (r in r_x) {
      stk <- Substring_stkb(r, FALSE, FALSE)
      out <- rbind(out, result_BGLR(.GlobalEnv[[r]], stk, TRUE))
    }
  }
  out <- as.data.frame(out[-1, ])
  if (is_subp) {
    colnames(out) <- c("arg", "subp", "model", "phenotype", "MAE")
  } else {
    colnames(out) <- c("arg", "model", "phenotype", "MAE")
  }
  rownames(out) <- seq(1:nrow(out))
  out[, ncol(out)] <- as.numeric(as.character(out[, ncol(out)]))
  if (is_lg_MAE) {
    out[, ncol(out)] <- -log10(out[, ncol(out)])
  }
  return(out)
}

result_BGLR <- function(result, stk, is_subp=FALSE) {
  if (is_subp) {
    mx <- matrix(NA, 1, 5)
    for (ph in names(result)) {
      for (sp in names(result[[ph]])) {
        # Skip blank subp
        if (class(result[[ph]][[sp]]) != "list") { next }
        for (md in names(result[[ph]][[sp]])) {
          # Skip FIXED
          if (md == 'FIXED') { next }
          mx <- rbind(mx, c(stk, sp, md, ph, result[[ph]][[sp]][[md]][["mean_mae"]]))
        }
      }
    }
    colnames(mx) <- c("arg", "subp", "model", "phenotype", "MAE")
    return(mx[-1, ])
  } else {
    mx <- matrix(NA, 1, 4)
    for (ph in names(result)) {
      for (md in names(result[[ph]])) {
        # Skip FIXED
        if (md == 'FIXED') { next }
        mx <- rbind(mx, c(stk, md, ph, result[[ph]][[md]][["mean_mae"]]))
      }
    }
    colnames(mx) <- c("arg", "model", "phenotype", "MAE")
    return(mx[-1, ])
  }
}


# ------------------------------- rrBLUP --------------------------------#
# Compare subp and not
Collect_rrBLUP <- function(is_subp=FALSE, is_lg_MAE=TRUE) {
  r_results <- names(globalenv())[grep('rrBLUP_', names(globalenv()))]
  if (!is_subp) {
    r_x <- r_results[grep("nosubp", r_results)]
    out <- matrix(NA, 1, 3)
    for (r in r_x) {
      st <- Substring_stkb(r, FALSE, FALSE)
      out <- rbind(out, result_rrBLUP(.GlobalEnv[[r]], st))
    }
  } else {
    r_x <- r_results[grep("_subp", r_results)]
    out <- matrix(NA, 1, 4)
    for (r in r_x) {
      st <- Substring_stkb(r, FALSE, FALSE)
      out <- rbind(out, result_rrBLUP(.GlobalEnv[[r]], st, TRUE))
    }
  }
  out <- as.data.frame(out[-1, ])
  if (is_subp) {
    colnames(out) <- c("arg", "Subpopulation", "Phenotype", "MAE")
  } else {
    colnames(out) <- c("arg", "Phenotype", "MAE")
  }
  rownames(out) <- seq(1:nrow(out))
  out[, ncol(out)] <- as.numeric(as.character(out[, ncol(out)]))
  if (is_lg_MAE) {
    out[, ncol(out)] <- -log10(out[, ncol(out)])
  }
  return(out)
}

result_rrBLUP <- function(result, st, is_subp=FALSE) {
  if (is_subp) {
    mx <- matrix(NA, 1, 4)
    for (sp in names(result)) {
      # Skip blank subpopulation
      if (class(result[[sp]]) != "list") { next }
      for (ph in names(result[[sp]])) {
        mx <- rbind(mx, c(st, sp, ph, result[[sp]][[ph]][["mean_mae"]]))
      }
    }
    colnames(mx) <- c("arg", "Subpopulation", "Phenotype", "MAE")
    return(mx[-1, ])
  } else {
    mx <- matrix(NA, 1, 3)
    for (ph in names(result)) {
      mx <- rbind(mx, c(st, ph, result[[ph]][["mean_mae"]]))
    }
    colnames(mx) <- c("arg", "Phenotype", "MAE")
    return(mx[-1, ])
  }
}
