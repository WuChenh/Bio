library(keras)
library(Metrics)
library(foreach)
library(dplyr)

wt_BLR_bstMd <- function(dir_rslt='~/GS/BLR/', grepW='r.bglr', trait_list, SelectTrnPerc=0.8) {
  num_trt <- length(trait_list)
  #====== Remove *grepW* in globalenv
  rm(list=names(globalenv())[grep(grepW, names(globalenv()))], envir = .GlobalEnv)
  #====== Order trait read list
  read_list <- dir(dir_rslt)[grep(grepW, dir(dir_rslt))]
  queue_trp <- c()
  queue_trt <- c()
  for (ntr in read_list) {
    if (reg_trnPerc(ntr) != SelectTrnPerc) {next}
    queue_trp <- c(queue_trp, ntr)
    queue_trt <- c(queue_trt, reg_trait(ntr))
  }
  read_list <- queue_trp[order(queue_trt)]
  #====== Read BLR rslt
  wt_traits <- foreach (n = read_list) %do% {
    print(n)
    traitN <- reg_trait(n)
    trnPerc <- reg_trnPerc(n)
    load(paste0(dir_rslt, n), envir = .GlobalEnv)
    varName <- names(globalenv())[grep(grepW, names(globalenv()))]
    print(varName)
    rslt <- .GlobalEnv[[varName]]
    rm(list=varName, envir = .GlobalEnv)
    repN <- length(rslt)
    wt_repN <- foreach(rn = 1:repN, .combine = 'rbind') %do% {
      wt <- rslt[[rn]] [['bglr']] [[ rslt[[rn]][["best_md"]][["md_best"]] ]] [["ETA"]][[1]][["b"]]
    }
    rownames(wt_repN) <- NULL
    wt_repN
  }
  names(wt_traits) <- trait_list
  return(wt_traits)
}

# wt_traits <- wt_BLR_bstMd(trait_list=trait_list)
# save(list=c('wt_traits'), file=paste0('wt_traits', '.RData'))
#hist(wt_traits[[1]][1,])
# load("~/GS/ANN/wt_traits.RData")

wt_combn_mean <- function(wt_list, Nth_rep, trait_combn, trait_list) {
  wt_sum <- foreach(trt = trait_combn, .combine='+') %do% {
    wt_list[[trait_list[trt]]][Nth_rep,] }
  return(wt_sum/length(trait_combn))
}

assign_wt_nontrainable <- function(mrk_mx, WTs) {
  for (colw in 1:ncol(mrk_mx)) {
    mrk_mx[,colw] <- mrk_mx[,colw] * WTs[colw] }
  return(mrk_mx)
}

Cor_ExP <- function(trn.p, trn.e) {
  trn.p.scale <- scale(as.matrix(trn.p))
  trn.e.scale <- scale(as.matrix(trn.e))
  return(cor(trn.e.scale, trn.p.scale)) #19x11
  #ExP <- t(scale(as.matrix(trn.e))) %*% scale(as.matrix(trn.p)) # 19x11
  #PxE <- t(scale(as.matrix(trn.p))) %*% scale(as.matrix(trn.e))
}


# activations : relu | elu | selu | hard_sigmoid | sigmoid | linear | softmax | softplus | softsign | tanh | exponential
# optimizer: adam | adamax | adadelta | adagrad | nadam | rmsprop 

# NN BEGIN
NN_SMT <- function(trn.g, tst.g, trn.p, tst.p, trn.e, tst.e,
                   trait, mrk_eff,
                   batch_size=32, epochs=300, patience=25,
                   activation='sigmoid', optimizer='adam',
                   include_CorPhe=FALSE, #include_env=TRUE,
                   isScaleSNP=TRUE, # DEFAULT: p&e are scaled.
                   dropout=0.2,
                   modelIn=NULL) {
  source("NN_SMT.R")
  # data load
  trn.p <- scale(trn.p) %>% as.matrix()
  tst.p <- scale(tst.p) %>% as.matrix()
  trn.e <- scale(trn.e)
  tst.e <- scale(tst.e)
  if(isScaleSNP) {
    trn.g <- scale(trn.g, center=FALSE)
    tst.g <- scale(tst.g, center=FALSE) }
  trn.g <- assign_wt_nontrainable(trn.g, mrk_eff)
  tst.g <- assign_wt_nontrainable(tst.g, mrk_eff)
  
  num_trait <- ncol(trn.p)
  #num_sampl <- nrow(trn.p)
  
  if(include_CorPhe & (num_trait<length(trait))) {
    return('Developing...')}
  
  input_shape <- ncol(tst.g)
  
  # Define and Refresh NN
  if (is.null(modelIn)) {
    #============================== Main input ================================#
    G_input <- layer_input(shape = input_shape, name = 'G_input')
    G_out <- G_input %>%
      layer_dense(units = 1024, activation = activation) %>% #input_shape = input_shape
      layer_dense(units = 256, activation = activation, name = 'G_out') # Should it be smaller than 256?
    #============================= Auxiliary_input ============================#
    E_input <- layer_input(shape = ncol(trn.e), name = 'E_input')
    E_out <- E_input %>%
      layer_dense(units = 16, activation = 'linear') %>%
      layer_dense(units = 12, activation = 'linear', name = 'E_out') # Larger than trait number?
    #================================ Main ====================================#
    main_output <- layer_concatenate(c(G_out, E_out)) %>%  
      layer_dense(units = 64, activation = 'linear') %>% #layer_dropout(rate = dropout) %>%
      layer_dense(units = 64, activation = activation) %>% 
      #layer_dense(units = 64, activation = activation) %>% 
      layer_dense(units = num_trait, activation = 'linear', name = 'main_output')
    #
    modelIn <- keras_model(
      inputs = c(G_input, E_input), 
      outputs = c(main_output)
    )
  }
  model = modelIn
  model %>% summary()
  model %>% compile(
    loss = "mse",
    optimizer = optimizer,
    metrics = list("mean_absolute_error"))
  
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = patience,
                                        restore_best_weights = TRUE)
  history_fit <- model %>% fit(
    list(trn.g, trn.e), trn.p,
    batch_size = batch_size,
    epochs = epochs,
    verbose = 0,
    validation_data = list(list(tst.g, tst.e), tst.p),
    callbacks = list(early_stop),
    shuffle = TRUE)
  
  c(loss, mae) %<-% (model %>% evaluate(list(tst.g, tst.e), tst.p))
  tst_pred <- model %>% predict(list(tst.g, tst.e)) %>% as.matrix()
  
  if (FALSE) {
    py <- function(x){
      reticulate::py_run_string(x)
    }
    py("from keras import backend as K")
    py("from keras.backend import set_session;")
    py("from keras.backend import clear_session;")
    py("from keras.backend import get_session;")
    
    clear_gpu <- function(){
      py("cfg = K.config_pb2.ConfigProto()")
      py("cfg.gpu_options.allow_growth = True")
      py("K.set_session(K.tf.compat.v1.Session(config=cfg))")
      k_clear_session()
    }
    #now clear with this command
    clear_gpu()
  }
  
  tst_corr <- foreach (tn = 1:num_trait, .combine='c') %do% {
    cor(tst_pred[,tn], tst.p[,tn]) }
  tst_mae <- foreach (tn = 1:num_trait, .combine='c') %do% {
    mae(tst_pred[,tn], tst.p[,tn]) }
  tst_mse <- foreach (tn = 1:num_trait, .combine='c') %do% {
    mse(tst_pred[,tn], tst.p[,tn]) }
  print(tst_corr)
  return(list(corr=tst_corr, mse_tst=tst_mse, mae_tst=tst_mae,
              trait=trait, mark_eff=mrk_eff,
              history_fit=history_fit, mae=mae, loss=loss))
}


# NN BEGIN
NN_SMT_Sequential <- function(trn.g, tst.g, trn.p, tst.p, trn.e, tst.e,
                   trait, mrk_eff,
                   batch_size=32, epochs=300, patience=25,
                   activation='sigmoid', optimizer='adam',
                   include_env=FALSE, include_CorPhe=FALSE,
                   isScaleSNP=TRUE, # DEFAULT: p&e are scaled.
                   dropout=0,
                   modelIn=NULL) {
  source("NN_SMT.R")
  # data load
  trn.p <- scale(trn.p) %>% as.matrix()
  tst.p <- scale(tst.p) %>% as.matrix()
  trn.e <- scale(trn.e)
  tst.e <- scale(tst.e)
  if(isScaleSNP) {
    trn.g <- scale(trn.g, center=FALSE)
    tst.g <- scale(tst.g, center=FALSE) }
  trn.g <- assign_wt_nontrainable(trn.g, mrk_eff)
  tst.g <- assign_wt_nontrainable(tst.g, mrk_eff)
  
  num_trait <- ncol(trn.p)
  #num_sampl <- nrow(trn.p)
  
  if(include_env) {
    cor_EP <- Cor_ExP(trn.p, trn.e)
    trn.g <- cbind(trn.g, trn.e %*% cor_EP)
    tst.g <- cbind(tst.g, tst.e %*% cor_EP)}
  if(include_CorPhe & (num_trait<length(trait))) {
    return('Developing...')}
  
  input_shape <- ncol(tst.g)
  
  # Define and Refresh NN
  if (is.null(modelIn)) {
    modelIn = keras_model_sequential() %>%
      layer_dense(units = 1024, activation = 'linear', input_shape = input_shape) %>%
      layer_dense(units = 512, activation = activation) %>%
      layer_dropout(rate = dropout) %>% 
      #layer_dense(units = 256, activation = activation) %>%
      layer_dense(units = 128, activation = activation) %>%
      #layer_dense(units = 32, activation = activation) %>%
      layer_dense(units = num_trait)
  }
  model = modelIn
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer,
    metrics = list("mean_absolute_error"))
  model %>% summary()
  
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = patience)
  history_fit <- model %>% fit(
    trn.g, trn.p,
    batch_size = batch_size,
    epochs = epochs,
    verbose = 0,
    validation_data = list(tst.g, tst.p),
    callbacks = list(early_stop))
  
  c(loss, mae) %<-% (model %>% evaluate(tst.g, tst.p))
  tst_pred <- model %>% predict(tst.g) %>% as.matrix()
  tst_corr <- foreach (tn = 1:num_trait, .combine='c') %do% {
    cor(tst_pred[,tn], tst.p[,tn]) }
  tst_mae <- foreach (tn = 1:num_trait, .combine='c') %do% {
    mae(tst_pred[,tn], tst.p[,tn]) }
  tst_mse <- foreach (tn = 1:num_trait, .combine='c') %do% {
    mse(tst_pred[,tn], tst.p[,tn]) }
  print(tst_corr)
  return(list(corr=tst_corr, mse_tst=tst_mse, mae_tst=tst_mae,
              trait=trait, mark_eff=mrk_eff,
              history_fit=history_fit, mae=mae, loss=loss))
}



format_output_NN <- function(NN_output, num_trait_all=11) {
  traits <- NN_output$trait
  cor_zero <- rep(0, num_trait_all)
  mse_zero <- rep(0, num_trait_all)
  trt_zero <- rep(0, num_trait_all)
  for (trt in 1:length(traits)) {
    cor_zero[traits[trt]] <- (NN_output$corr)[trt]
    mse_zero[traits[trt]] <- (NN_output$mse_tst)[trt]
    trt_zero[traits[trt]] <- 1
  }
  return(c(cor_zero, mse_zero, trt_zero))
}


################################### Best combn #################################
library(foreach)
best_combn <- function(mx_combns, combn_list, #combn_list <- combn(11, 2)
                       tag,
                       nam_trait_all=c('FLL', 'FLW', 'PH', 'SN', 'FP', 'PF', 'SL', 'SW', 'SV', 'SSA', 'AC'),
                       return_best_combn=TRUE) { 
  combn_list <- t(combn_list)
  num_trait_all  <- length(nam_trait_all)
  num_trait_used <- ncol(combn_list)
  tmp <- foreach(nr = 1:nrow(combn_list), .combine='rbind') %do% {
    c.corr <- mx_combns[nr, combn_list[nr,]]
    c.mse  <- mx_combns[nr, num_trait_all + combn_list[nr,]]
    corr_and_mse <- cbind(cbind(c.corr, c.mse), combn_list[nr,])
    cbind(corr_and_mse, nr)
  }
  colnames(tmp) <- c('Corr', "MSE", 'Trait', 'Combn')
  rownames(tmp) <- NULL
  #
  tmp <- data.frame(Corr= as.numeric(tmp[,1]), 
                    MSE=  as.numeric(tmp[,2]),
                    Trait=as.factor(nam_trait_all[tmp[,3]]),
                    Combn=as.factor(tmp[,4]) )
  #
  if(!return_best_combn) {return(tmp)}
  return2 <- tmp
  #
  tmp <- foreach (trt = sort(nam_trait_all), .combine='rbind') %do% {
    lines_trt <- tmp[which(tmp$Trait == trt), ]
    lines_cob <- foreach(cobs = lines_trt$Combn, .combine = 'rbind') %do% { #Mean for the same combn
      which_combn <- which(lines_trt$Combn == cobs)
      c(mean(lines_trt[which_combn, 1]), mean(lines_trt[which_combn, 2]), trt)
    }
    lines_cob[which(lines_cob[,2] == min(lines_cob[,2])), ]
  }
  rownames(tmp) <- NULL
  tmp <- cbind(tmp, tag) ######tag#######
  tmp <- data.frame(Corr=tmp[,1], MSE=tmp[,2], Trait=as.factor(tmp[,3]),
                    allMethods=as.factor(tmp[,4]))
  return(list(combn_best=tmp, combn_all=return2))
}



if (F) {
  data_debug <- function(dataO, trait=seq(1,11), trnPerc=0.8, seed=101) {
    num.sam <- nrow(dataO[[2]])
    trnPerc <- trnPerc
    set.seed(seed)
    trn.rd <- sort(sample(num.sam, round(num.sam*trnPerc)))
    trn.g <- dataO[[1]][trn.rd,]
    trn.p <- dataO[[2]][trn.rd, trait]
    trn.e <- dataO[[3]][trn.rd,]
    tst.g <- dataO[[1]][-trn.rd,]
    tst.p <- dataO[[2]][-trn.rd, trait]
    tst.e <- dataO[[3]][-trn.rd,]
    return(list(trn.g=trn.g, trn.p=trn.p, trn.e=trn.e,
                tst.g=tst.g, tst.p=tst.p, tst.e=tst.e))
  }
}


