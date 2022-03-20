#--------------------------- SVR RBF -------------------------#
setwd("~/gs/rice/SVR_tune")
library(e1071)
library(Metrics)
library(parallel)
library(foreach)
library(doParallel)

svr_cv_tune <- function(dataN, tt, ph, 
                        gamma_range = 2^(-4:3),
                        cost_range = 2^(-5:4)) {
  library(e1071)
  library(Metrics)
  test_x <- dataN[[tt]][["test"]][["geno"]]
  test_y <- dataN[[tt]][["test"]][["phen"]][, ph]
  tmp_kf <- list()
  cor_test <- c()
  mae_test <- c()
  for (kf in seq(1, length(dataN[[tt]][[1]]))) {
    valid_x <- dataN[[tt]][["trainValid"]][[kf]][["valid"]][["geno"]]
    valid_y <- dataN[[tt]][["trainValid"]][[kf]][["valid"]][["phen"]][, ph]
    train_x <- dataN[[tt]][["trainValid"]][[kf]][["train"]][["geno"]]
    train_y <- dataN[[tt]][["trainValid"]][[kf]][["train"]][["phen"]][, ph]
    best_model <- tune(svm,
                       train.x = train_x,
                       train.y = train_y,
                       validation.x = valid_x,
                       validation.y = valid_y,
                       #--------------------------- Grid --------------------------#
                       ranges = list(gamma = gamma_range, cost = cost_range) # ,
                       # tunecontrol = tune.control(sampling = "fix")
                       )[["best.model"]]
    tmp_kf[[kf]] <- best_model
    cor_test <- c(cor_test, cor(predict(best_model, test_x), test_y))
    mae_test <- c(mae_test, mae(predict(best_model, test_x), test_y))
  }
  which_max_cor <- which(cor_test == max(cor_test))
  which_min_mae <- which(mae_test == min(mae_test))
  return(list(
    cor_test = cor_test,
    cor_max = max(cor_test),
    which_max_cor = which_max_cor,
    mae_test = mae_test,
    mae_min = min(mae_test),
    which_min_mae = which_min_mae,
    best_model_by_cor = tmp_kf[[which_max_cor]],
    best_model_by_mae = tmp_kf[[which_min_mae]]#,
    #models = tmp_kf
  ))
}

datalist <- c(
  "rice_compl_tt10t0.6k05", "rice_compl_tt10t0.6k10",
  "rice_compl_tt10t0.7k05", "rice_compl_tt10t0.7k10",
  "rice_compl_tt10t0.8k05", "rice_compl_tt10t0.8k10"
)
num_data <- length(datalist)
p1 <- 1
pn <- NA
#preload for name and number of pheno
load(paste(c('~/gs/rice/_data/', datalist[1], ".RData"), collapse = ""))
nam_ph <- colnames(.GlobalEnv[[datalist[1]]][[1]][[1]][[1]][[1]][[2]])
num_ph <- length(nam_ph)
if (is.na(pn)) {
  pn <- as.numeric(num_ph)
}

out <- list()
for (ph in p1:pn) {
  cores <- num_data
  print(paste(c("Use ", cores, " cores."), collapse = ""))
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  tmp_in <- foreach(d = seq(1:num_data), .errorhandling = "pass", .verbose = TRUE) %dopar% {
    load(paste(c('~/gs/rice/_data/', datalist[d], ".RData"), collapse = ""), .GlobalEnv)
    num_tt <- length(.GlobalEnv[[datalist[d]]])
    tmp_tt <- list()
    for (tt in 1:num_tt) {
      #print(paste(c("Tune: ", datalist[d], "tt", tt, "ph", ph), collapse = " "))
      tmp_tt[[tt]] <- svr_cv_tune(.GlobalEnv[[datalist[d]]], tt, ph)
      #print("Done.")
    }
    #rm(list=names(globalenv())[grep(datalist[d], names(globalenv()))])
    save(list = c("tmp_tt"), 
         file = paste(c("~/gs/rice/SVR_tune/SVR_RBF_tune_tt_", as.character(d), ".RData"), collapse = ""), 
         compress = "xz")
    #tmp_in[[d]] <- tmp_tt
    tmp_tt
  }
  stopCluster(cl)
  print(paste("ph", ph, sep = " "))
  out[[ph]] <- tmp_in
}
names(out) <- nam_ph[p1:pn]
save(list = c("out"), file = "~/gs/rice/SVR_tune/_SVR_RBF_tune_o.RData", compress = "xz")
