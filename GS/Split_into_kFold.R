Split_into_kFold <- function(tdata, k, pureTT=FALSE, seed=1234) {
  perc = 1/k
  nr_train = nrow(tdata[[1]])
  cv_set = list()
  set.seed(seed)
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