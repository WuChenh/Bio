# Also working for only train and test
Split_into_TrainValidTest_kFold <- function(sample_set, train_perc=0.8, k=10,
                                            num_TT=10, is_Cluster=TRUE, seed=1234,
                                            genoMx_pure=FALSE, genoMx_transpose=FALSE) {
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
