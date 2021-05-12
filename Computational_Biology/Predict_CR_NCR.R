Predict_CR_NCR <- function(nt_seq, transition_prob, emission_prob) {
  library(dplyr)
  len_seq <- nchar(nt_seq)
  nt_seq <- nt_seq %>% toupper() %>% substring(1:len_seq, 1:len_seq)
  transition_prob_lg <- log10(transition_prob)
  emission_prob_lg <- log10(emission_prob)
  probability <- matrix(NA, 2, len_seq, dimnames = list(c("n","c"), nt_seq))
  # Save selection history to isNC using rbind(path_n, path_c)
  isNC <- list()
  path_n <- c("N")
  path_c <- c("C")
  isNC[[1]] <- rbind(path_n, path_c)
  probability[1, 1] <- emission_prob_lg[1, nt_seq[1]] + transition_prob_lg[1, 1]
  probability[2, 1] <- emission_prob_lg[2, nt_seq[1]] + transition_prob_lg[1, 2]
  for (n in 2:(len_seq + 1)) {
    if (n > 2) {
      tmp_n_is_prev <- tmp_n_is
      tmp_c_is_prev <- tmp_c_is
    }
    # Non-Coding
    tmp_nn <- probability[1, n-1] + transition_prob_lg[1, 1]
    tmp_cn <- probability[2, n-1] + transition_prob_lg[2, 1]
    if (n < (len_seq + 1)) {
      probability[1, n] <- emission_prob_lg[1, nt_seq[n]] + max(tmp_nn, tmp_cn)
    }
    if (tmp_nn > tmp_cn) {
      tmp_n_is <- "N"
    } else {
      tmp_n_is <- "C"
    }
    if (n > 2) {
      if (length(which(isNC[[n-2]][ , ncol(isNC[[n-2]])] == tmp_n_is_prev)) > 1) {
        path_n <- c(isNC[[n-2]][which(probability[ , n-2] == max(probability[ , n-2]))[1], ], tmp_n_is) %>% as.character()
      } else {
        path_n <- c(isNC[[n-2]][which(isNC[[n-2]][ , ncol(isNC[[n-2]])] == tmp_n_is_prev), ], tmp_n_is) %>% as.character()
      }
    }
    # Coding
    tmp_nc <- probability[1, n-1] + transition_prob_lg[1, 2]
    tmp_cc <- probability[2, n-1] + transition_prob_lg[2, 2]
    if (n < (len_seq + 1)) {
      probability[2, n] <- emission_prob_lg[2, nt_seq[n]] + max(tmp_nc, tmp_cc)
    }
    if (tmp_nc > tmp_cc) {
      tmp_c_is <- "N"
    } else {
      tmp_c_is <- "C"
    }
    if (n > 2) {
      if (length(which(isNC[[n-2]][ , ncol(isNC[[n-2]])] == tmp_c_is_prev)) > 1) {
        path_c <- c(isNC[[n-2]][which(probability[ , n-2] == max(probability[ , n-2]))[1], ], tmp_c_is) %>% as.character()
      } else {
        path_c <- c(isNC[[n-2]][which(isNC[[n-2]][ , ncol(isNC[[n-2]])] == tmp_c_is_prev), ], tmp_c_is) %>% as.character()
      }
    }
    if (n > 2) {
      isNC[[n-1]] <- rbind(path_n, path_c)
    }
  }
  if (probability[1, len_seq] > probability[2, len_seq]) {
    result <- isNC[[len_seq]][1, ]
  } else {
    result <- isNC[[len_seq]][2, ]
  }
  result <- noquote(as.character(result))
  print(probability)
  print(result)
  return(list(prediction=result, probability=probability, steps=isNC))
}

transition_prob <- matrix(c(.8, .2, .4, .6), nrow=2, byrow=TRUE,
                          dimnames=list(c("n","c"), c("n","c")))
emission_prob <- matrix(c(.2, .3, .3, .2, .4, .2, .2, .2), nrow=2, byrow=TRUE,
                        dimnames=list(c("n","c"), c("A","C","G","T")))
nt_seq <- "CGAAAAAATCG"