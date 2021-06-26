#plot_subpopulation
if (FALSE) {
  rice.climate <- sampPosi
  rm(list=names(globalenv())[grep('rice_', names(globalenv()))])
  save(list=names(globalenv())[grep('rice.', names(globalenv()))], file="rice.origin.RData")
  #xz -9vk --threads=20 rice.origin.RData
  # -------------------------- plot subp conut --------------------------#
  library(ggplot2)
  # This method is worse.
  num_subp_all <- rbind(cbind('NA_was_eliminated', as.character(rice.compl[["splm"]][["Sub.population"]])),
                        cbind('Complete', as.character(rice.origin[["SD1"]][["Sub.population"]])))
  colnames(num_subp_all) <- c('Data', 'Subpopulation')
  num_subp_all <- as.data.frame(num_subp_all)
  ggplot(num_subp_all, aes(x=Subpopulation, fill=Data)) +
    geom_bar(position="dodge", width = .5) +
    labs(title='Count Subpopulations') +
    theme(legend.position = c(.15, .85),
          legend.background = element_blank())
  # The following method is better!
  if (FALSE) {
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
    
    num_ratio_subp <- count_subp()
    p1 <- cbind(num_ratio_subp$origin, 'Complete_Data')
    p2 <- cbind(num_ratio_subp$no_NA, 'NA_was_eliminated')
    colnames(p1) <- c('Subpopulation', 'Count', 'Ratio', 'Data')
    colnames(p2) <- c('Subpopulation', 'Count', 'Ratio', 'Data')
    num_ratio_subp <- rbind(p1, p2)
    rm(p1, p2)
    #
    ggplot(num_ratio_subp, aes(x=Subpopulation, y=Count, fill=Data, group=Data)) +
      ylim(c(0,110)) +
      geom_bar(position="dodge", width = .5, stat="identity") +
      labs(title='Count Subpopulations') +
      theme(legend.position = c(.15, .85),
            legend.background = element_blank()) +
      geom_text(show.legend = FALSE, 
                aes(alpha=.7, label=paste(format(round(Ratio*100, 2), nsmall=2), "% \n(", Count, ')', "\n\n", sep="")), 
                position=position_dodge2(.6), size=2.6, hjust=.4)
  }
}
# -------------------------------------------------------------- #

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

# ------------------------------ plot rrBLUP ----------------------------#
if (FALSE) {
  rm(rrBLUP_subp_t0.7k05, rrBLUP_subp_t0.8k05, rrBLUP_nosubp_t0.7k05, rrBLUP_nosubp_t0.8k05)
  #
  rrBLUP_subp_plot <- Collect_rrBLUP(TRUE, TRUE)
  rrBLUP_nosubp_plot <- Collect_rrBLUP(FALSE, TRUE)
  rrBLUP_nosubp_plot4col <- cbind(rrBLUP_nosubp_plot[,1], "N/A", rrBLUP_nosubp_plot[,2:3])
  colnames(rrBLUP_nosubp_plot4col) <- c("arg", "Subpopulation", "Phenotype", "MAE")
  rrBLUP_all_plot <- rbind(rrBLUP_subp_plot, rrBLUP_nosubp_plot4col)
  colnames(rrBLUP_all_plot) <- c("arg", "Subpopulation", "Phenotype", "MAE")
  rm(rrBLUP_nosubp_plot4col)
  ggplot(rrBLUP_all_plot, aes(x=Phenotype, y=MAE, color=Subpopulation)) +
    geom_point(alpha=0.5, size=1.5) +
    #geom_line(aes(color=model)) +
    xlab("Phenotype") + 
    ylab("-lg(MAE)") +
    labs(title='rrBLUP Prediction') +
    #facet_grid(arg~.)
    facet_wrap(~arg, nrow = 2)
}


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
