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
  out <- matrix(NA, 1, 3)
  for (r in r_x) {
    tkb <- Substring_stkb(r, TRUE)
    out <- rbind(out, result_NN(.GlobalEnv[[r]], tkb))
  }
  out <- as.data.frame(out[-1, ])
  #out[, 1] <- factor(out[, 1])
  #out[, 2] <- factor(out[, 2])
  colnames(out) <- c("arg", "phenotype", "MAE")
  rownames(out) <- seq(1:nrow(out))
  out[, 3] <- as.numeric(as.character(out[, 3]))
  if (is_lg_MAE) {
    out[, 3] <- -log10(out[, 3])
  }
  return(out)
}
result_NN <- function(result, tkb) {
  mx <- matrix(NA, 1, 3)
  for (ph in names(result)) {
    mx <- rbind(mx, cbind(tkb, ph, result[[ph]][["resultMLC"]][, 1]))
  }
  colnames(mx) <- c("arg", "phenotype", "MAE")
  return(mx[-1, ])
}
#--------------------------- plot_NN -----------------------#
if (FALSE) {
  library(ggplot2)
  #
  NN_plot_elu_noEnv <- Collect_NN("elu", env = FALSE, env_and_noEnv = FALSE, is_lg_MAE = TRUE)
  NN_plot_elu_noEnv <- cbind(NN_plot_elu_noEnv, "elu, G") #×
  colnames(NN_plot_elu_noEnv) <- c("arg", "Phenotype", "MAE", "Activation_and_E")
  #
  NN_plot_elu_env <- Collect_NN("elu", env = TRUE, env_and_noEnv = FALSE, is_lg_MAE = TRUE)
  NN_plot_elu_env <- cbind(NN_plot_elu_env, "elu, G×E") #×
  colnames(NN_plot_elu_env) <- c("arg", "Phenotype", "MAE", "Activation_and_E")
  #
  NN_plot_relu_noEnv <- Collect_NN("relu", env = FALSE, env_and_noEnv = FALSE, is_lg_MAE = TRUE)
  NN_plot_relu_noEnv <- cbind(NN_plot_relu_noEnv, "relu, G") #×
  colnames(NN_plot_relu_noEnv) <- c("arg", "Phenotype", "MAE", "Activation_and_E")
  #
  NN_plot_relu_env <- Collect_NN("relu", env = TRUE, env_and_noEnv = FALSE, is_lg_MAE = TRUE)
  NN_plot_relu_env <- cbind(NN_plot_relu_env, "relu, G×E") #×
  colnames(NN_plot_relu_env) <- c("arg", "Phenotype", "MAE", "Activation_and_E")
  #
  NN_plot <- rbind(NN_plot_elu_noEnv, NN_plot_elu_env, NN_plot_relu_noEnv, NN_plot_relu_env)
  # 1 better
  ggplot(NN_plot, aes(x=Phenotype, y=MAE, fill=Phenotype, color=Activation_and_E)) + 
    xlab("Phenotype") + 
    ylab("-lg(MAE)") +
    #geom_rect(aes(xmin=0, xmax=1.5, ymin=-Inf, ymax=Inf), fill='#C0C0C0', alpha = .01) +
    geom_violin() +
    labs(title='NN Prediction') +
    facet_wrap(~arg, nrow=4) + # or nrow=2
    theme(legend.position = "bottom")
  # 2
  ggplot(NN_plot, aes(x=Phenotype, y=MAE, fill=Phenotype, color=Activation_and_E)) + 
    #theme_bw() +
    xlab("Phenotype") + 
    ylab("-lg(MAE)") +
    #geom_rect(aes(xmin=0, xmax=1.5, ymin=-Inf, ymax=Inf), fill='#C0C0C0', alpha = .01) +
    geom_violin() +
    #stat_summary() +
    #geom_boxplot() +
    labs(title='NN Prediction') +
    facet_grid(arg~.) +
    theme(legend.position = "bottom")
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
#--------------------------- plot_BGLR ------------------------#
if (FALSE) {
  rm(BGLR_singleTrait_subp_t0.7k05,
     BGLR_singleTrait_subp_t0.8k05,
     BGLR_singleTrait_t0.7k05, 
     BGLR_singleTrait_t0.8k05)
  BGLR_subp_plot <- Collect_BGLR(TRUE, TRUE)
  BGLR_nosubp_plot <- Collect_BGLR(FALSE, TRUE)
  BGLR_nosubp_plot5col <- cbind(BGLR_nosubp_plot[,1], "N/A", BGLR_nosubp_plot[,2:4])
  colnames(BGLR_nosubp_plot5col) <- c("arg", "subp", "model", "phenotype", "MAE")
  BGLR_all_plot <- rbind(BGLR_subp_plot, BGLR_nosubp_plot5col)
  colnames(BGLR_all_plot) <- c("arg", "Subpopulation", "Model", "Phenotype", "MAE")
  rm(BGLR_nosubp_plot5col)
  ggplot(BGLR_all_plot, aes(x=Phenotype, y=MAE, shape=Subpopulation, color=Model)) + #, group=subp)) +
    geom_point(alpha=0.5, size=1.5) +
    #geom_line(aes(color=model)) +
    xlab("Phenotype") + 
    ylab("-lg(MAE)") +
    labs(title='BGLR Prediction') +
    #facet_grid(arg~.)
    facet_wrap(~arg, nrow = 2)
}


# ------------------------------- rrBLUP --------------------------------#



if (FALSE) {
#################### Comparing methods ####################
library(ggplot2)
xMethod_yMSE <- matrix(NA, 1, 3)
# with env

# no env
# t0.7k05

dimnames(xMethod_yMSE) <- list(seq(1:nrow(xMethod_yMSE)),
                               c("-lgMAE", "Method", "Phenotype"))
#dimnames(xMethod_yMSE) <- list(seq(1:nrow(xMethod_yMSE)),
#                               c("-lgMAE", "Method", "Subpopulation"))
}

if (FALSE) {
ggplot(data = economics_long, aes(x = date, y = value01, color= variable)) +
  geom_line()

ggplot(singer, aes(x=voice.part, y=height)) + 
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="steelblue", width=.2)

plot(x=BGLR_subpSingleTrait_t0.7k10[["PF"]][["IND"]][["FIXED"]][[1]][["y"]],
     y=BGLR_subpSingleTrait_t0.7k10[["PF"]][["IND"]][["FIXED"]][[1]][["yHat"]],
     xlab = "y", ylab = "yHat")
}