Split_into_train_test <- function(train_perc, geno, pheno, splmd, envir,
                                  seed=1234) {
  if (train_perc > 1) {
    train_perc = 1/train_perc
  }
  geno <- as.matrix(geno)
  pheno <- as.matrix(pheno)
  num_sample <- nrow(geno)
  set.seed(seed)
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