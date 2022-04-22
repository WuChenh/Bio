Del_sample_with_NA_phenotype <- function(geno, pheno, envir, splm, to_del='allNA',
                                         genoMx_pure=T, genoMx_transpose=T) {
  if (genoMx_pure==T) { geno <- (geno[ , 10:ncol(geno)]) }
  if (genoMx_transpose==T) { geno <- t(geno) }
  #pheno <- as.matrix(pheno)
  row_to_del <- c()
  nr <- nrow(pheno)
  for (r in 1:nr) {
    if (to_del=='allNA') {num.NA <- sum(is.na(pheno[r, ])) + sum(is.na(envir[r, ]))}
    else {
      if (to_del=='pheno') {num.NA <- sum(is.na(pheno[r, ]))}
      if (to_del=='envir') {num.NA <- sum(is.na(envir[r, ]))}
    }
    if (num.NA > 0) {
      #if (genoMx_pure==FALSE) {
      # Edit colnames' pattern below. 
      #sample.to.del <- c(sample.to.del, paste("X0_sample_", r, sep = ""))
      #}
      row_to_del <- c(row_to_del, r)
    }
  }
  pheno.del <- pheno[-row_to_del, ]
  geno.del <- geno[-row_to_del, ]
  envir.del <- envir[-row_to_del, ]
  splm.del <- splm[-row_to_del, ]
  rownames(geno.del) <- NULL
  rownames(pheno.del) <- NULL
  rownames(envir.del) <- NULL
  rownames(splm.del) <- NULL
  return(list(geno=geno.del, pheno=pheno.del, wc_bio=envir.del, splmd=splm.del))
}

Del_sample_with_non_phenotype <- function(geno, pheno, splmd=NULL, envir=NULL, genoMx_pure=TRUE, genoMx_transpose=TRUE) {
  if (genoMx_pure==TRUE) { geno <- (geno[ , 10:ncol(geno)]) }
  if (genoMx_transpose==TRUE) { geno <- t(geno) }
  phenotype <- as.matrix(pheno)
  nr.pheno <- nrow(pheno)
  nc.pheno <- ncol(pheno)
  #sample.to.del <- c()
  row_to_del <- c()
  for (r in 1:nr.pheno) {
    if (sum(is.na(pheno[r, ])) == nc.pheno) {
      #if (transp_pure==TRUE) {
      # Edit colnames' pattern below. 
      #sample.to.del <- c(sample.to.del, paste("X0_sample_", r, sep = ""))
      #}
      row_to_del <- c(row_to_del, r)
    }
  }
  pheno.leastOne.pheno <- pheno[-row_to_del, ]
  geno.del <- geno[-row_to_del, ]
  splm.del <- splmd[-row_to_del, ]
  envir.del <- envir[-row_to_del, ]
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

############################ select traits #############################
if (F) {
  load("rice.origin.RData")
  rice.origin$wc_bio <- as.data.frame(rice.origin$wc_bio)
  rice.pheno.all <- read.table('44kgwas/RiceDiversity_44K_Phenotypes_34traits_PLINK.txt',
                               header = T, sep = "\t")
  library(mice)
  md.pattern(rice.pheno.all[, -c(1,2)], rotate.names = T)
  # 1600x2400
  library(VIM)
  aggr(rice.pheno.all[, -c(1,2)], prop=TRUE, numbers=TRUE)
  rice.pheno15 <- rice.pheno.all[, c(10,11,13,14,16,17,18,19,20,21,22,23,33,34,36)] #No SHS, +PNP
  rice.pheno13 <- rice.pheno.all[, c(10,11,   14,   17,18,19,20,21,22,23,33,34,36)]
  rice.pheno12 <- rice.pheno.all[, c(10,11,   14,   17,18,19,20,21,22,23,   34,36)]
  rice.pheno11 <- rice.pheno.all[, c(10,11,   14,   17,18,19,20,21,22,23,   34   )]
  rice.pheno10 <- rice.pheno.all[, c(10,11,   14,   17,18,19,20,21,22,23         )]
  md.pattern(rice.pheno15, rotate.names = T) #353
  md.pattern(rice.pheno13, rotate.names = T) #357
  md.pattern(rice.pheno12, rotate.names = T) #365
  md.pattern(rice.pheno11, rotate.names = T) #372
  #
  colnames(rice.pheno15) <- c('FLL', 'FLW', 'PNP', 'PH', 'PBN', 'SN', 'FP', 'PF',
                              'SL', 'SW', 'SV', 'SSA', 'BR', 'AC', 'PC')
  colnames(rice.pheno13) <- c('FLL', 'FLW', 'PH', 'SN', 'FP', 'PF',
                              'SL', 'SW', 'SV', 'SSA', 'BR', 'AC', 'PC')
  colnames(rice.pheno12) <- c('FLL', 'FLW', 'PH', 'SN', 'FP', 'PF',
                              'SL', 'SW', 'SV', 'SSA', 'AC', 'PC')
  colnames(rice.pheno11) <- c('FLL', 'FLW', 'PH', 'SN', 'FP', 'PF',
                              'SL', 'SW', 'SV', 'SSA', 'AC')
  colnames(rice.pheno10) <- c('FLL', 'FLW', 'PH', 'SN', 'FP', 'PF',
                              'SL', 'SW', 'SV', 'SSA')
  rice.origin[[length(rice.origin)+1]] <- rice.pheno.all
  rice.origin[[length(rice.origin)+1]] <- rice.pheno15
  rice.origin[[length(rice.origin)+1]] <- rice.pheno13
  rice.origin[[length(rice.origin)+1]] <- rice.pheno12
  rice.origin[[length(rice.origin)+1]] <- rice.pheno11
  rice.origin[[length(rice.origin)+1]] <- rice.pheno10
  names(rice.origin) <- c("geno", "pheno", "SD1", "wc_bio", 
                          'pheno_all', 'pheno15', 'pheno13', 'pheno12', 'pheno11', 'pheno10')
  #
  rice.compl.p15 <- Del_sample_with_NA_phenotype(rice.origin$geno, rice.origin$pheno15, rice.origin$wc_bio, rice.origin$SD1)
  rice.compl.p13 <- Del_sample_with_NA_phenotype(rice.origin$geno, rice.origin$pheno13, rice.origin$wc_bio, rice.origin$SD1)
  rice.compl.p12 <- Del_sample_with_NA_phenotype(rice.origin$geno, rice.origin$pheno12, rice.origin$wc_bio, rice.origin$SD1)
  rice.compl.p11 <- Del_sample_with_NA_phenotype(rice.origin$geno, rice.origin$pheno11, rice.origin$wc_bio, rice.origin$SD1)
  rice.compl.p10 <- Del_sample_with_NA_phenotype(rice.origin$geno, rice.origin$pheno10, rice.origin$wc_bio, rice.origin$SD1)
  rice.compl.pZ6 <- Del_sample_with_NA_phenotype(rice.origin$geno, rice.origin$pheno, rice.origin$wc_bio, rice.origin$SD1)
  #save(list = c('rice.origin'), file = 'rice_origin.RData', compress = 'xz', compression_level = 9)
  save.image('rice_origin.RData', compress = 'xz')
}
