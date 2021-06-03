# NN Keras Regressor
# activations : relu | elu | selu | hard_sigmoid | sigmoid | linear | softmax | softplus | softsign | tanh | exponential
# optimizer: adam | adamax | adadelta | adagrad | nadam | rmsprop 
argv <- commandArgs(trailingOnly = TRUE)
rdata_read <- "~/gs/_rice_compl_noSubp.RData"
setwd("~/gs/rice/NN_noEnv")
load(rdata_read)
source('~/gs/rice/Keras_singleTrait_para.R')
tmp_list <- list()
print(argv)

#e.g. NN_relu_noEnv_t0.7k05_b128
if (as.logical(argv[6])) {
  isenv <- "env"
} else {
  isenv <- "noEnv"
}
# b
if (as.numeric(argv[2]) < 100) {
  if (as.numeric(argv[2]) < 10) {
    b <- paste("00", argv[2], sep = "")
  } else {
    b <- paste("0", argv[2], sep = "")
  }
} else {
  b <- argv[2]
}
reg_tk <- regexec("t0.[1-9]+k[0-9][0-9]", argv[1])
tk <- substring(argv[1], reg_tk[[1]][1], (reg_tk[[1]][1] + attributes(reg_tk[[1]])[[1]] - 1))
variable_name <- paste(c("NN_", argv[5], "_", isenv, "_", tk, "_b", b), collapse = "")
save_name <- paste(variable_name, ".RData", sep = "")

patience <- argv[4]
if (patience=="NULL") {
  patience <- NULL
} else { patience <- as.numeric(patience)}

time_begin = Sys.time()

tmp_list[[1]] <- Keras_singleTrait_para(feed_set = .GlobalEnv[[argv[1]]],
                                        batch_size = as.numeric(argv[2]),
                                        epochs = as.numeric(argv[3]),
                                        patience = patience,
                                        activation = argv[5],
                                        #optimizer = ,
                                        include_env = as.logical(argv[6]),
                                        usePCinsteadSNP = as.logical(argv[7]),
                                        isScale = as.logical(argv[8]),
                                        isScaleSNP = as.logical(argv[9]))
time_end = Sys.time()
print(time_end - time_begin)

names(tmp_list) <- c(variable_name)
list2env(tmp_list, .GlobalEnv)
save(list=c(variable_name), file = save_name)
