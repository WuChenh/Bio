# Build NN regressor. 
# activations : relu | leaky_relu | softmax | sigmoid | softplus | elu | tanh
# optimizer: sgd | adam | adadelta | adagrad | nadam | rmsprop 
Keras_complSingleTrait_paraTT <- function(feed_set,
                                        batch_size=256, epochs=200,
                                        patience=NULL,
                                        activation='relu',
                                        optimizer=optimizer_rmsprop(),
                                        include_env=TRUE,
                                        isScale=TRUE) {
  library(dplyr)
  library(parallel)
  library(foreach)
  library(doParallel)
  
  #num_cg <- ncol(feed_set$train[[1]]$pheno)
  nam_ph <- colnames(feed_set[[1]][[1]][[1]][[1]][[2]])
  num_tt <- length(feed_set)
  num_cv <- length(feed_set[[1]][[1]])
  num_ph <- length(nam_ph)
  result <- list()
  
  for (p in 1:num_ph) {
    cores <- detectCores() - 2
    if (num_tt < cores) { cores = num_tt }
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    tmp_tt <- foreach(t = 1:num_tt) %dopar% {
      library(magrittr)
      library(keras)
      library(dplyr)
      library(parallel)
      library(foreach)
      library(doParallel)
      
      # Test set
      if(isScale) {
        test__envi = scale(feed_set[[t]][[2]][[4]], center = FALSE)
        test__geno = scale(feed_set[[t]][[2]][[1]], center = FALSE)
        test__phen = scale(as.matrix(feed_set[[t]][[2]][[2]][ , p]), center = FALSE)
      } else {
        test__envi = feed_set[[t]][[2]][[4]]
        test__geno = feed_set[[t]][[2]][[1]]
        test__phen = as.matrix(feed_set[[t]][[2]][[2]][ , p])
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
      test_prediction <- list()
      #loss_and_metrics_list <- list()
      #scores_list <- c()
      for (c in 1:num_cv) {
        if(isScale) {
          train_envi = scale(feed_set[[t]][[1]][[c]][[1]][[4]], center = FALSE)
          train_geno = scale(feed_set[[t]][[1]][[c]][[1]][[1]], center = FALSE)
          train_phen = scale(as.matrix(feed_set[[t]][[1]][[c]][[1]][[2]][ , p]), center = FALSE)
          valid_envi = scale(feed_set[[t]][[1]][[c]][[2]][[4]], center = FALSE)
          valid_geno = scale(feed_set[[t]][[1]][[c]][[2]][[1]], center = FALSE)
          valid_phen = scale(as.matrix(feed_set[[t]][[1]][[c]][[2]][[2]][ , p]), center = FALSE)
        } else {
          train_envi = feed_set[[t]][[1]][[c]][[1]][[4]]
          train_geno = feed_set[[t]][[1]][[c]][[1]][[1]]
          train_phen = as.matrix(feed_set[[t]][[1]][[c]][[1]][[2]][ ,p])
          valid_envi = feed_set[[t]][[1]][[c]][[2]][[4]]
          valid_geno = feed_set[[t]][[1]][[c]][[2]][[1]]
          valid_phen = as.matrix(feed_set[[t]][[1]][[c]][[2]][[2]][ ,p])
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
        model %>% compile(
          loss = "mse",
          #loss = "categorical_crossentropy",
          optimizer = optimizer_rmsprop(),
          metrics = list("mean_absolute_error")
        )
        
        model %>% summary()
        
        if (!is.null(patience)) {
          early_stop <- callback_early_stopping(monitor = "val_loss", patience = patience)
          history_list[[c]] <- model %>% fit(
            x_train, y_train,
            batch_size = batch_size,
            epochs = epochs,
            verbose = 0,
            validation_data = list(x_valid, y_valid),
            callbacks = list(early_stop)
          )
        } else {
          history_list[[c]] <- model %>% fit(
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
        print(paste(c("loss: ", loss, ";  ", "MAE: ", mae), collapse = ""))
        test_loss <- c(test_loss, loss)
        test_mae <- c(test_mae, mae)
        test_prediction[[c]] <- model %>% predict(x_test)
      }
      list(history=history_list,
           loss=test_loss,
           mae=test_mae,
           test_prediction=list(y_pred=test_prediction, y_test=y_test))
    }
    stopCluster(cl)
    result[[p]] <- tmp_tt
  }
  names(result) = nam_ph[1:num_ph]
  return(result)
}