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