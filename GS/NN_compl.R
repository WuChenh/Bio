# NN Keras compl normal
# activations : relu | elu | selu | hard_sigmoid | sigmoid | linear | softmax | softplus | softsign | tanh | exponential
# optimizer: adam | adamax | adadelta | adagrad | nadam | rmsprop 
argv <- commandArgs(trailingOnly = TRUE)

rdata_read <- "$HOME/gs/rice_backup_202105072137.RData"
setwd("$HOME/gs/rice/NN_normal")
load(rdata_read)
source('~/gs/rice/Keras_complSingleTrait_paraTT.R')
tmp_list <- list()

print(argv)
variable_name <- argv[1]
#e.g. NN_relu_rice_compl_t0.7k05_b128e200

rdata_save <- argv[2]
patience <- argv[6]
if (patience=="NULL") {
  patience <- NULL
} else { patience <- as.numeric(patience)}

time_begin = Sys.time()

tmp_list[[1]] <- Keras_complSingleTrait_paraTT(feed_set = .GlobalEnv[[argv[3]]],
                                               batch_size = as.numeric(argv[4]),
                                               epochs = as.numeric(argv[5]),
                                               patience = patience,
                                               activation = argv[7],
                                               #optimizer = ,
                                               include_env = as.logical(argv[8]),
                                               usePCinsteadSNP = as.logical(argv[9]),
                                               isScale = as.logical(argv[10]),
                                               isScaleSNP = as.logical(argv[11]))

time_end = Sys.time()
print(time_end - time_begin)

names(tmp_list) <- c(variable_name)
list2env(tmp_list, .GlobalEnv)
rm(list=names(globalenv())[-which(names(globalenv())==variable_name)])
save.image(rdata_save)
