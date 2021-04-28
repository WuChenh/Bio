Divide_sample_into_subpopulation <- function(tab_subp, geno, pheno, envir=NULL, genoMx_pure=TRUE, genoMx_transpose=TRUE) {
  if (genoMx_pure==TRUE) { geno <- (geno[ , 10:ncol(geno)]) }
  if (genoMx_transpose==TRUE) { geno <- t(geno) }
  if (class(pheno) != "matrix") { pheno = as.matrix(pheno) }
  which.subp.col <- which(colnames(tab_subp)=="Sub.population")
  subp.list.fac <- unique(tab_subp[ , which.subp.col])
  subp.list.vec <- as.vector(subp.list.fac)
  num.subp <- length(subp.list.fac)
  list_subp <- list()
  for (s in 1:num.subp) {
    which.subp.row <- which(tab_subp[ , which.subp.col] == subp.list.vec[s])
    genotype <- geno[which.subp.row, ]
    phenotype <- pheno[which.subp.row, ]
    splm1 <- tab_subp[which.subp.row, ]
    environ <- envir[which.subp.row, ]
    list_subp[[s]] <- list(geno=genotype, pheno=phenotype, splmd=splm1, wc_env=environ)
    #write.csv(genotype, gsub("X",subp.list.vec[s],"$HOME/gs/rice/sub_populations/rice_X_geno.csv"))
    #write.csv(phenotype, gsub("X",subp.list.vec[s],"$HOME/gs/rice/sub_populations/rice_X_pheno.csv"))
    #write.csv(splm1, gsub("X",subp.list.vec[s],"$HOME/gs/rice/sub_populations/rice_X_splm1.csv"))
  }
  names(list_subp) <- subp.list.vec
  return(list_subp)
}