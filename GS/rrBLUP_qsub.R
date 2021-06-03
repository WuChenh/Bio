# rrBLUP_qsub
argv <- commandArgs(trailingOnly = TRUE)
setwd("~/gs/rice/rrBLUP")
if (as.logical(argv[2])) {
  rdata_path <- "~/gs/_rice_compl_subp.RData"
} else {
  rdata_path <- "~/gs/_rice_compl_noSubp.RData"
}
load(rdata_path)
source('~/gs/rice/rrBLUP_pipe.R')
source('~/gs/rice/rrBLUP_train2test.R')
print(argv)

#e.g. rrBLUP_subp_t0.7k05.RData
if (as.logical(argv[2])) {
  isCls <- "subp"
} else {
  isCls <- "nosubp"
}
reg_tk <- regexec("t0.[1-9]+k[0-9][0-9]", argv[1])
tk <- substring(argv[1], reg_tk[[1]][1], (reg_tk[[1]][1] + attributes(reg_tk[[1]])[[1]] - 1))
variable_name <- paste(c("rrBLUP_", isCls, "_", tk), collapse = "")
save_name <- paste(variable_name, ".RData", sep = "")

time_begin = Sys.time()
tmp_list <- list()
tmp_list[[1]] <- rrBLUP_pipe(dataSet = .GlobalEnv[[argv[1]]],
                             isCluster = as.logical(argv[2]),
                             para = as.logical(argv[3]))
time_end = Sys.time()
print(time_end - time_begin)

names(tmp_list) <- c(variable_name)
list2env(tmp_list, .GlobalEnv)
save(list=c(variable_name), file = save_name)