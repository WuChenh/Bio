Split_into_TrainValidTest_kFold <- function(sample_set, train_perc=0.85, k=10, num_TT=20, is_Cluster=TRUE, genoMx_pure=FALSE, genoMx_transpose=FALSE) {
  data_set <- list()
  if (is_Cluster) {
    nam_subp <- names(sample_set)
    for (sp in 1:length(sample_set)) {
      subp_tt_sets <- list()
      geno = sample_set[[sp]][[1]]
      if (genoMx_pure==TRUE) { geno <- geno[ , 10:ncol(geno)] }
      if (genoMx_transpose==TRUE) { geno <- t(geno) }
      for (smpl in 1:num_TT) {
        tmp_tt_sets <- Split_into_train_test(train_perc, geno, sample_set[[sp]][[2]],
                                             sample_set[[sp]][[3]], sample_set[[sp]][[4]])
        subp_tt_sets[[smpl]] <- list(trainValid=Split_into_kFold(tmp_tt_sets[[1]], k),
                                     test=tmp_tt_sets[[2]])
      }
      data_set[[sp]] <- subp_tt_sets
    }
    names(data_set) <- nam_subp
  } else {
    tt_sets <- list()
    geno = sample_set[[1]]
    if (genoMx_pure==TRUE) { geno <- geno[ , 10:ncol(geno)] }
    if (genoMx_transpose==TRUE) { geno <- t(geno) }
    for (smpl in 1:num_TT) {
      tmp_tt_sets <- Split_into_train_test(train_perc, geno, sample_set[[2]],
                                           sample_set[[3]], sample_set[[4]])
      tt_sets[[smpl]] <- list(trainValid=Split_into_kFold(tmp_tt_sets[[1]], k),
                              test=tmp_tt_sets[[2]])
    }
    data_set = tt_sets
  }
  return(data_set)
}
