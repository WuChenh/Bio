# Also working for only train and test
Split_into_TrainValidTest_kFold <- function(sample_set, train_perc=0.8, k=10,
                                            num_TT=10, is_Cluster=TRUE,
                                            genoMx_pure=FALSE, genoMx_transpose=FALSE,
                                            seed=NA) {
  #if (train_perc==1) { pureTT = TRUE } # No validation
  data_set <- list()
  if (is_Cluster) {
    nam_subp <- names(sample_set)
    for (sp in 1:length(sample_set)) {
      subp_tt_sets <- list()
      geno = sample_set[[sp]][[1]]
      if (genoMx_pure) { geno <- geno[ , 10:ncol(geno)] }
      if (genoMx_transpose) { geno <- t(geno) }
      for (smpl in 1:num_TT) {
        tmp_tt_sets <- Split_into_train_test(train_perc, geno, sample_set[[sp]][[2]],
                                             sample_set[[sp]][[3]], sample_set[[sp]][[4]],
                                             seed)
        if (train_perc==1) {
          subp_tt_sets[[smpl]] <- Split_into_kFold(tmp_tt_sets, k, TRUE, seed)
        } else {
          subp_tt_sets[[smpl]] <- list(trainValid=Split_into_kFold(tmp_tt_sets[[1]], k, FALSE, seed),
                                       test=tmp_tt_sets[[2]])
        }
      }
      data_set[[sp]] <- subp_tt_sets
    }
    names(data_set) <- nam_subp
  } else {
    tt_sets <- list()
    geno = sample_set[[1]]
    if (genoMx_pure) { geno <- geno[ , 10:ncol(geno)] }
    if (genoMx_transpose) { geno <- t(geno) }
    for (smpl in 1:num_TT) {
      tmp_tt_sets <- Split_into_train_test(train_perc, geno, sample_set[[2]],
                                           sample_set[[3]], sample_set[[4]], seed)
      if (train_perc==1) {
        tt_sets[[smpl]] <- Split_into_kFold(tmp_tt_sets, k, TRUE, seed)
      } else {
        tt_sets[[smpl]] <- list(trainValid=Split_into_kFold(tmp_tt_sets[[1]], k, FALSE, seed),
                              test=tmp_tt_sets[[2]])
      }
    }
    data_set = tt_sets
  }
  return(data_set)
}

Split_into_train_test <- function(train_perc, geno, pheno, splmd, envir, seed=NA) {
  if (train_perc > 1) {
    train_perc = 1/train_perc
  }
  geno <- as.matrix(geno)
  pheno <- as.matrix(pheno)
  num_sample <- nrow(geno)
  if (!is.na(seed)) {
    set.seed(seed)
  }
  train = as.matrix(sample(1:num_sample, round(num_sample * train_perc)))
  geno_train <- geno[train, ]
  pheno_train <- as.matrix(pheno[train, ])
  splmd_train <- as.matrix(splmd[train, ])
  envir_train <- as.matrix(envir[train, ])
  train_set <- list(geno_train, pheno_train, splmd_train, envir_train)
  if (train_perc == 1) {
    names(train_set) <- c("geno", "pheno", "splmd", "envir")
    return(train_set)
  } else {
    test = setdiff(1:num_sample, train)
    geno__test <- geno[test, ]
    pheno__test <- as.matrix(pheno[test, ])
    splmd__test <- as.matrix(splmd[test, ])
    envir__test <- as.matrix(envir[test, ])
    test__set <- list(geno__test, pheno__test, splmd__test, envir__test)
    names(train_set) <- c("geno", "pheno", "splmd", "envir")
    names(test__set) <- c("geno", "pheno", "splmd", "envir")
    return(list(train=train_set, test=test__set))
  }
}

Split_into_kFold <- function(tdata, k, pureTT=FALSE, seed=NA) {
  perc = 1/k
  nr_train = nrow(tdata[[1]])
  cv_set = list()
  if (!is.na(seed)) {
    set.seed(seed)
  }
  rSerial = sample(seq(1:nr_train), nr_train, replace = FALSE)
  len = round(nr_train * perc)
  begin = 1
  for (c in 1:k) {
    endr = begin + len - 1
    if (c==k) {
      valid_r = as.matrix(rSerial[begin:nr_train])
    } else {
      valid_r = as.matrix(rSerial[begin:endr])
    }
    train_r = setdiff(1:nr_train, valid_r)
    begin = endr + 1
    train = list(geno_train = tdata[[1]][train_r, ],
                 phen_train = tdata[[2]][train_r, ],
                 splm_train = tdata[[3]][train_r, ],
                 envi_train = tdata[[4]][train_r, ])
    valid = list(geno_valid = tdata[[1]][valid_r, ],
                 phen_valid = tdata[[2]][valid_r, ],
                 splm_valid = tdata[[3]][valid_r, ],
                 envi_valid = tdata[[4]][valid_r, ])
    if (pureTT) {
      cv_set[[c]] = list(train=train, test=valid)
    } else {
      cv_set[[c]] = list(train=train, valid=valid)
    }
  }
  return(cv_set)
}
