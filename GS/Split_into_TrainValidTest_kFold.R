# Also working for only train and test
Split_into_TrainValidTest_kFold <- function(sample_set, train_perc=0.8, k=10,
                                            num_TT=10, is_Cluster=TRUE,
                                            genoMx_pure=FALSE, genoMx_transpose=FALSE,
                                            isSeed=FALSE, seed_tt=NA, seed_kf=NA) {
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
        # set different seed for smpl
        if (!isSeed) {
          seed_tt <- smpl*100
          seed_kf <- smpl*100
        }
        tmp_tt_sets <- Split_into_train_test(train_perc, geno, sample_set[[sp]][[2]],
                                             sample_set[[sp]][[3]], sample_set[[sp]][[4]],
                                             seed_tt[smpl])
        if (train_perc==1) {
          subp_tt_sets[[smpl]] <- Split_into_kFold(tmp_tt_sets, k, TRUE, seed_kf)
        } else {
          subp_tt_sets[[smpl]] <- list(trainValid=Split_into_kFold(tmp_tt_sets[[1]], k, FALSE, seed_kf),
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
      # set different seed for smpl
      if (!isSeed) {
        seed_tt <- smpl*100
        seed_kf <- smpl*100
      }
      tmp_tt_sets <- Split_into_train_test(train_perc, geno, sample_set[[2]],
                                           sample_set[[3]], sample_set[[4]], seed_tt[smpl])
      if (train_perc==1) {
        tt_sets[[smpl]] <- Split_into_kFold(tmp_tt_sets, k, TRUE, seed_kf)
      } else {
        tt_sets[[smpl]] <- list(trainValid=Split_into_kFold(tmp_tt_sets[[1]], k, FALSE, seed_kf),
                              test=tmp_tt_sets[[2]])
      }
    }
    data_set = tt_sets
  }
  return(data_set)
}

Split_into_train_test <- function(train_perc, geno, phen, splm, envi, seed=NA) {
  if (train_perc > 1) {
    train_perc = 1/train_perc
  }
  geno <- as.matrix(geno)
  phen <- as.matrix(phen)
  num_sample <- nrow(geno)
  if (!is.na(seed)) {
    set.seed(seed)
  }
  train = as.matrix(sample(1:num_sample, round(num_sample * train_perc)))
  geno_train <- geno[train, ]
  phen_train <- as.matrix(phen[train, ])
  splm_train <- as.matrix(splm[train, ])
  envi_train <- as.matrix(envi[train, ])
  train_set <- list(geno_train, phen_train, splm_train, envi_train)
  if (train_perc == 1) {
    names(train_set) <- c("geno", "phen", "splm", "envi")
    return(train_set)
  } else {
    test = setdiff(1:num_sample, train)
    geno__test <- geno[test, ]
    phen__test <- as.matrix(phen[test, ])
    splm__test <- as.matrix(splm[test, ])
    envi__test <- as.matrix(envi[test, ])
    test__set <- list(geno__test, phen__test, splm__test, envi__test)
    names(train_set) <- c("geno", "phen", "splm", "envi")
    names(test__set) <- c("geno", "phen", "splm", "envi")
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
    train = list(geno = tdata[[1]][train_r, ],
                 phen = tdata[[2]][train_r, ],
                 splm = tdata[[3]][train_r, ],
                 envi = tdata[[4]][train_r, ])
    valid = list(geno = tdata[[1]][valid_r, ],
                 phen = tdata[[2]][valid_r, ],
                 splm = tdata[[3]][valid_r, ],
                 envi = tdata[[4]][valid_r, ])
    if (pureTT) {
      cv_set[[c]] = list(train=train, test=valid)
    } else {
      cv_set[[c]] = list(train=train, valid=valid)
    }
  }
  return(cv_set)
}
