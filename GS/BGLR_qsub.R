# BGLR_qsub
argv <- commandArgs(trailingOnly = TRUE)
setwd("~/gs/rice/BGLR_subp")
if (as.logical(argv[2])) {
  rdata_path <- "~/gs/_rice_compl_subp.RData"
} else {
  rdata_path <- "~/gs/_rice_compl_noSubp.RData"
}
load(rdata_path)
source('~/gs/rice/BGLR_pipe.R')
source('~/gs/rice/BGLR_SingleTrait_para.R')

#e.g. BGLR_singleTrait_subp_t0.7k05.RData
if (as.logical(argv[2])) {
  isCls <- "subp"
} else {
  isCls <- "nosubp"
}
reg_tk <- regexec("t0.[1-9]+k[0-9][0-9]", argv[1])
tk <- substring(argv[1], reg_tk[[1]][1], (reg_tk[[1]][1] + attributes(reg_tk[[1]])[[1]] - 1))
variable_name <- paste(c("BGLR_singleTrait_", isCls, "_", tk), collapse = "")
save_name <- paste(variable_name, ".RData", sep = "")
print(variable_name)

time_begin = Sys.time()

#bayesModel = c("FIXED","BRR","BayesA","BL","BayesB","BayesC")
tmp_list <- list()
tmp_list[[1]] <- BGLR_SingleTrait_para(dataSet = .GlobalEnv[[argv[1]]],
                                       isCluster = as.logical(argv[2]),
                                       nam_data = argv[1])
time_end = Sys.time()
print(time_end - time_begin)

names(tmp_list) <- c(variable_name)
list2env(tmp_list, .GlobalEnv)
save(list=c(variable_name), file = save_name)