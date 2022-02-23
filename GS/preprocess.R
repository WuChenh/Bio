Del_sample_with_NA_phenotype <- function(geno, pheno, splm=NULL, genoMx_pure=TRUE, genoMx_transpose=TRUE) {
  if (genoMx_pure==TRUE) { geno <- (geno[ , 10:ncol(geno)]) }
  if (genoMx_transpose==TRUE) { geno <- t(geno) }
  pheno <- as.matrix(pheno)
  #sample.to.del <- c()
  pheno.r.to.del <- c()
  nr.pheno <- nrow(pheno)
  for (r in 1:nr.pheno) {
    num.NA <- sum(is.na(pheno[r, ]))
    if (num.NA > 0) {
      #if (genoMx_pure==FALSE) {
      # Edit colnames' pattern below. 
      #sample.to.del <- c(sample.to.del, paste("X0_sample_", r, sep = ""))
      #}
      pheno.r.to.del <- c(pheno.r.to.del, r)
    }
  }
  pheno.del <- pheno[-pheno.r.to.del, ]
  geno.del <- geno[-pheno.r.to.del, ]
  splm.del <- splm[-pheno.r.to.del, ]
  #if (is.na(envir)==FALSE) {envir.del <- envir[-pheno.r.to.del, ]}
  return(list(geno=geno.del, pheno=pheno.del, splmd=splm.del)) #wc_env=envir.del
}

Del_sample_with_non_phenotype <- function(geno, pheno, splmd=NULL, envir=NULL, genoMx_pure=TRUE, genoMx_transpose=TRUE) {
  if (genoMx_pure==TRUE) { geno <- (geno[ , 10:ncol(geno)]) }
  if (genoMx_transpose==TRUE) { geno <- t(geno) }
  phenotype <- as.matrix(pheno)
  nr.pheno <- nrow(pheno)
  nc.pheno <- ncol(pheno)
  #sample.to.del <- c()
  pheno.r.to.del <- c()
  for (r in 1:nr.pheno) {
    if (sum(is.na(pheno[r, ])) == nc.pheno) {
      #if (transp_pure==TRUE) {
      # Edit colnames' pattern below. 
      #sample.to.del <- c(sample.to.del, paste("X0_sample_", r, sep = ""))
      #}
      pheno.r.to.del <- c(pheno.r.to.del, r)
    }
  }
  pheno.leastOne.pheno <- pheno[-pheno.r.to.del, ]
  geno.del <- geno[-pheno.r.to.del, ]
  splm.del <- splmd[-pheno.r.to.del, ]
  envir.del <- envir[-pheno.r.to.del, ]
  return(list(geno=geno.del, pheno=pheno.leastOne.pheno, splmd=splm.del, wc_env=envir.del))
}

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
