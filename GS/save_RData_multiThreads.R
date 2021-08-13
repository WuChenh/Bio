save_multiThreads <- function(vname, threads=20) {
  con <- pipe(paste(c("xz -T", threads, " -9 -e > ", vname, ".RData"), collapse = ""), "wb")
  save(list=names(globalenv())[which(names(globalenv())==vname)], file = con); close(con)
}
