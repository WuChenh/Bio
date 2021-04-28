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