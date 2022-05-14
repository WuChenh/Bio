# little functions

reg_trait <- function(varName) {
  reg_tr <- regexpr('tra', varName)
  reg_tp <- regexpr('.tp', varName)
  trait <- substring(varName, reg_tr[[1]][1], (reg_tp[[1]][1] - 1))
  traitN <- as.numeric(gsub('tra', '', trait))
  return(traitN)
}
reg_trnPerc <- function(varName) {
  reg_tp <- regexec("tp0.[0-9]", varName)
  out <- substring(varName, reg_tp[[1]][1], (reg_tp[[1]][1] + attributes(reg_tp[[1]])[[1]] - 1))
  out <- as.numeric(gsub('tp0.', '', out))
  return(out/10)
}

rename_var <- function(varName) {
  out <- .GlobalEnv[[varName]]
  rm(list=names(globalenv())[which(names(globalenv())==varName)],
     envir = .GlobalEnv)
  return(out)
}

load_grep <- function(grepW) {
  for (n in dir()[grep(grepW, dir())]){
    load(n, envir = .GlobalEnv)
  }
}

keep_onlyGrepVar <- function(grepW) {
  rm(list=names(globalenv())[-grep(grepW, names(globalenv()))],
     envir = .GlobalEnv)
}
keep_byNames <- function(vnames) {
  rm(list=names(globalenv())[-which(names(globalenv()) %in% vnames)],
     envir = .GlobalEnv)
}

save_multiThreads <- function(vnames, fname, threads=20) {
  vnames <- as.vector(vnames)
  con <- pipe(paste(c("xz -T", threads, " -9 -e > ", fname, ".RData"), collapse = ""), "wb")
  save(list=names(globalenv())[which(names(globalenv()) %in% vnames)], file = con); close(con)
}
save.image_multiThreads <- function(fname, threads=20) {
  con <- pipe(paste(c("xz -T", threads, " -9 -e > ", fname, ".RData"), collapse = ""), "wb")
  save(list=names(globalenv()), file = con); close(con)
}
save_mT_grep <- function(grepW, threads=20) {
  for (vn in names(globalenv())[grep(grepW, names(globalenv()))]) {
    save_multiThreads(vn, vn, threads)
  }
}

# tar.xz
# tar cvf rice_compl_splited2stk.tar *_s/
# xz -9vk --threads=20 rice_compl_splited2stk.tar
# rm rice_compl_splited2stk.tar
