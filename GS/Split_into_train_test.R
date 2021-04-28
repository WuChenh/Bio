Split_into_train_test <- function(train_perc, geno, pheno, splmd, envir) {
  pheno <- as.matrix(pheno)
  num_sample <- nrow(envir)
  train = as.matrix(sample(1:num_sample, round(num_sample * train_perc)))
  test = setdiff(1:num_sample, train)
  geno_train <- geno[train, ]
  geno__test <- geno[test, ]
  pheno_train <- as.matrix(pheno[train, ])
  pheno__test <- as.matrix(pheno[test, ])
  splmd_train <- as.matrix(splmd[train, ])
  splmd__test <- as.matrix(splmd[test, ])
  envir_train <- as.matrix(envir[train, ])
  envir__test <- as.matrix(envir[test, ])
  train_set <- list(geno_train, pheno_train, splmd_train, envir_train)
  test__set <- list(geno__test, pheno__test, splmd__test, envir__test)
  names(train_set) <- c("geno", "pheno", "splmd", "envir")
  names(test__set) <- c("geno", "pheno", "splmd", "envir")
  return(list(train=train_set, test=test__set))
}