# Build NN regressor, parallel. 
# activations : relu | elu | selu | hard_sigmoid | sigmoid | linear | softmax | softplus | softsign | tanh | exponential
# optimizer: adam | adamax | adadelta | adagrad | nadam | rmsprop 
Keras_NN_singleTrait <- function(feed_set,
                                 batch_size=256, epochs=200,
                                 patience=NULL,
                                 modelIn=NULL,
                                 activation='relu',
                                 optimizer='rmsprop', #optimizer_rmsprop(),
                                 include_env=TRUE,
                                 usePCinsteadSNP=FALSE,
                                 isScale=TRUE,
                                 isScaleSNP=TRUE,
                                 p1=1, pn=NA,
                                 dropout=.3,
                                 nlayer=5,
                                 layer1_units=512,
                                 layer2_units=256,
                                 layer3_units=128,
                                 layer4_units=64,
                                 layer5_units=32,
                                 weight_snp=NA) {
  library(dplyr)
  library(parallel)
  library(foreach)
  library(doParallel)
  
  #num_cg <- ncol(feed_set$train[[1]]$pheno)
  nam_ph <- colnames(feed_set[[1]][[1]][[1]][[1]][[2]])
  num_tt <- length(feed_set)
  num_cv <- length(feed_set[[1]][[1]])
  num_ph <- length(nam_ph)
  if (is.na(pn)) { pn <- num_ph}
  result <- list()
  p <- p1
  while (p <= pn) {
    if (!is.na(weight_snp)) {
      wt_snp <- weight_snp[p,]
    }
    #cores <- detectCores() - 2
    #if (num_tt < cores) { cores = num_tt }
    cl <- makeCluster(10)
    registerDoParallel(cl)
    tmp_tt <- foreach(t = 1:num_tt) %dopar% {
      #library(magrittr)
      library(keras)
      library(dplyr)
      library(parallel)
      library(foreach)
      library(doParallel)
      weighted <- function(dataMx, weight_snp) {
        for (colw in 1:ncol(dataMx)) {
          dataMx[,colw] <- dataMx[,colw] * weight_snp[colw]
        }
        return(dataMx)
      }
      # Test set
      if(isScale) {
        test__envi = scale(feed_set[[t]][[2]][[4]])
        test__phen = scale(as.matrix(feed_set[[t]][[2]][[2]][, p]))
      } else {
        test__envi = feed_set[[t]][[2]][[4]]
        test__phen = as.matrix(feed_set[[t]][[2]][[2]][, p])
      }
      if (!usePCinsteadSNP) {
        if (isScaleSNP) {
          test__geno = scale(feed_set[[t]][[2]][[1]], center = FALSE)
        } else {
          test__geno = feed_set[[t]][[2]][[1]]
        }
        ##############
        if (!is.na(weight_snp)) {
          test__geno <- weighted(test__geno, wt_snp)
        }
        ##############
      } else {
        test__geno = feed_set[[t]][[2]][[3]][,grep("PC", colnames(feed_set[[t]][[2]][[3]]))] %>% as.numeric(as.character())
      }
      if (include_env) {
        x_test  = as.matrix(cbind(test__envi, test__geno))
      } else {
        x_test  = as.matrix(test__geno)
      }
      
      y_test  = as.matrix(test__phen)
      input_shape = ncol(x_test)
      
      # CV
      history_list <- list()
      #train_loss <- c()
      #train_mean_absolute_error <- c()
      test_loss <- c()
      test_mae <- c()
      test_cor <- c()
      test_prediction <- list()
      #loss_and_metrics_list <- list()
      #scores_list <- c()
      cc <- 1
      while (cc <= num_cv) {
        if(isScale) {
          train_envi = scale(feed_set[[t]][[1]][[cc]][[1]][[4]])
          train_phen = scale(as.matrix(feed_set[[t]][[1]][[cc]][[1]][[2]][, p]))
          valid_envi = scale(feed_set[[t]][[1]][[cc]][[2]][[4]])
          valid_phen = scale(as.matrix(feed_set[[t]][[1]][[cc]][[2]][[2]][, p]))
        } else {
          train_envi = feed_set[[t]][[1]][[cc]][[1]][[4]]
          train_phen = as.matrix(feed_set[[t]][[1]][[cc]][[1]][[2]][, p])
          valid_envi = feed_set[[t]][[1]][[cc]][[2]][[4]]
          valid_phen = as.matrix(feed_set[[t]][[1]][[cc]][[2]][[2]][, p])
        }
        
        if (!usePCinsteadSNP) {
          if (isScaleSNP) {
            train_geno = scale(feed_set[[t]][[1]][[cc]][[1]][[1]], center = FALSE)
            valid_geno = scale(feed_set[[t]][[1]][[cc]][[2]][[1]], center = FALSE)
          } else {
            train_geno = feed_set[[t]][[1]][[cc]][[1]][[1]]
            valid_geno = feed_set[[t]][[1]][[cc]][[2]][[1]]
          }
          ##############
          if (!is.na(weight_snp)) {
            train_geno <- weighted(train_geno, wt_snp)
            valid_geno <- weighted(valid_geno, wt_snp)
          }
          ##############
        } else {
          train_geno = feed_set[[t]][[1]][[cc]][[1]][[3]][,grep("PC", colnames(feed_set[[t]][[1]][[cc]][[1]][[3]]))] %>% as.numeric(as.character())
          valid_geno = feed_set[[t]][[1]][[cc]][[2]][[3]][,grep("PC", colnames(feed_set[[t]][[1]][[cc]][[2]][[3]]))] %>% as.numeric(as.character())
        }
        
        if (include_env) {
          x_train = as.matrix(cbind(train_envi, train_geno))
          x_valid = as.matrix(cbind(valid_envi, valid_geno))
        } else {
          x_train = as.matrix(train_geno)
          x_valid = as.matrix(valid_geno)
        }
        y_train = as.matrix(train_phen)
        y_valid = as.matrix(valid_phen)
        
        # Define and Refresh NN for CV
        if (is.null(modelIn)) {
          if (usePCinsteadSNP) {
            model = keras_model_sequential() %>%
              layer_dense(units = 8, activation = activation, input_shape = input_shape) %>% 
              #layer_dropout(rate = 0.6) %>% 
              layer_dense(units = 4, activation = activation) %>%
              #layer_dropout(rate = 0.5) %>%
              ##layer_dense(units = 256, activation = activation) %>%
              #layer_dropout(rate = 0.4) %>%
              layer_dense(units = 2, activation = activation) %>%
              #layer_dropout(rate = 0.3) %>%
              layer_dense(units = 1)
          } else {
            model = keras_model_sequential() %>%
              layer_dense(units = 256, activation = activation, input_shape = input_shape) %>% 
              #layer_dropout(rate = 0.6) %>% 
              layer_dense(units = 128, activation = activation) %>%
              #layer_dropout(rate = 0.5) %>%
              ##layer_dense(units = 256, activation = activation) %>%
              #layer_dropout(rate = 0.4) %>%
              layer_dense(units = 32, activation = activation) %>%
              #layer_dropout(rate = 0.3) %>%
              layer_dense(units = 1)
          }
        }
        if (!is.null(modelIn)) {
          # Model
          if (nlayer == 3) {
            modelIn = keras_model_sequential() %>%
              layer_dense(units = layer1_units, activation = activation, input_shape = input_shape) %>% 
              layer_dense(units = layer2_units, activation = activation) %>%
              layer_dropout(rate = dropout) %>%
              layer_dense(units = 1)
          }
          if (nlayer == 4) {
            modelIn = keras_model_sequential() %>%
              layer_dense(units = layer1_units, activation = activation, input_shape = input_shape) %>% 
              layer_dense(units = layer2_units, activation = activation) %>%
              layer_dropout(rate = dropout) %>%
              layer_dense(units = layer3_units, activation = activation) %>%
              layer_dense(units = 1)
          }
          if (nlayer == 5) {
            modelIn = keras_model_sequential() %>%
              layer_dense(units = layer1_units, activation = activation, input_shape = input_shape) %>% 
              layer_dense(units = layer2_units, activation = activation) %>%
              layer_dropout(rate = dropout) %>%
              layer_dense(units = layer3_units, activation = activation) %>%
              layer_dense(units = layer4_units, activation = activation) %>%
              layer_dense(units = 1)
          }
          if (nlayer == 6) {
            modelIn = keras_model_sequential() %>%
              layer_dense(units = layer1_units, activation = activation, input_shape = input_shape) %>% 
              layer_dense(units = layer2_units, activation = activation) %>%
              layer_dropout(rate = dropout) %>%
              layer_dense(units = layer3_units, activation = activation) %>%
              layer_dense(units = layer4_units, activation = activation) %>%
              layer_dense(units = layer5_units, activation = activation) %>%
              layer_dense(units = 1)
          }
          model = modelIn
        }
        
        model %>% compile(
          loss = "mse",
          #loss = "categorical_crossentropy",
          optimizer = optimizer,
          metrics = list("mean_absolute_error")
        )
        
        model %>% summary()
        
        if (!is.null(patience)) {
          early_stop <- callback_early_stopping(monitor = "val_loss", patience = patience)
          history_list[[cc]] <- model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose = 0,
            validation_data = list(x_valid, y_valid),
            callbacks = list(early_stop)
          )
        } else {
          history_list[[cc]] <- model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose = 0,
            validation_data = list(x_valid, y_valid),
          )
        }
        #scores = model %>% evaluate(x_train, y_train, verbose = 0)
        #print(scores)
        #train_loss <- c(train_loss, scores[[1]])
        #train_mean_absolute_error <- c(train_mean_absolute_error, scores[[2]])
        c(loss, mae) %<-% (model %>% evaluate(x_test, y_test))
        test_loss <- c(test_loss, loss)
        test_mae <- c(test_mae, mae)
        test_prediction[[cc]] <- model %>% predict(x_test)
        test_cor <- c(test_cor, cor(test_prediction[[cc]], y_test))
        print(paste(c("loss: ", loss, ";  ", "MAE: ", mae, "Cor: ", test_cor[cc]), collapse = ""))
        cc <- cc + 1
      }
      list(history=history_list,
           loss=test_loss,
           MAE=test_mae,
           Cor=test_cor,
           test_prediction=list(y_pred=test_prediction, y_test=y_test))
    }
    stopCluster(cl)
    #save(list=c("tmp_tt"), file = paste(c("tmp_tt_p", p, ".RData"), collapse = ""))
    result_t_mx <- matrix(NA, num_tt, 5,
                          dimnames = list(seq(1:num_tt), c("MAE_min","MAE_mean","loss_min","loss_mean","Cor_max")))
    nam_tt <- c()
    for (rtt in 1:num_tt) {
      result_t_mx[rtt, ] <- c(min(tmp_tt[[rtt]][[3]]), mean(tmp_tt[[rtt]][[3]]),
                              min(tmp_tt[[rtt]][[2]]), mean(tmp_tt[[rtt]][[2]]),
                              max(tmp_tt[[rtt]][[4]]))
      nam_tt <- c(nam_tt, paste("random_tt_", rtt, sep = ""))
    }
    #nam_tt <- names(tmp_tt)
    tmp_tt[[length(tmp_tt)+1]] <- result_t_mx
    tmp_tt[[length(tmp_tt)+1]] <- list(MAE_min=(t.test(result_t_mx[, 1], alternative = "two.sided"))$p.value,
                                       MAE_mean=(t.test(result_t_mx[, 2], alternative = "two.sided"))$p.value,
                                       loss_min=(t.test(result_t_mx[, 3], alternative = "two.sided"))$p.value,
                                       loss_mean=(t.test(result_t_mx[, 4], alternative = "two.sided"))$p.value,
                                       Cor_max=(t.test(result_t_mx[, 5], alternative = "two.sided"))$p.value)
    names(tmp_tt) <- c(nam_tt, "resultMLC", "p_value")
    result[[p-p1+1]] <- tmp_tt
    p <- p + 1
  }
  names(result) = nam_ph[p1:pn]
  return(result)
}
