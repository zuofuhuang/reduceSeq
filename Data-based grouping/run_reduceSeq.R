# dataset (predictors, with the last column being the outcome called CA)
# initial lasso results
# alpha (default = 1, means LASSO)
# lambda grid (default = seq(0, 0.05, length.out = 101))


extract_time <- function(string){
  string <- string[!string == "(Intercept)"]
  as.numeric(substring(string, 2, 3)) * 60 + as.numeric(substring(string, 5, 6)) + 1
}


get_positions_after_threshold <- function(lasso_results, threshold){
  n_clusters <- length(lasso_results)
  
  vector <- c()
  for (j in 1:n_clusters){
    new <- extract_time((lasso_results[[j]]@Dimnames[[1]])[lasso_results[[j]]@i[which(abs((lasso_results[[j]]@x)) > threshold)]])
    vector <- c(vector, new)
  }
  
  
  return(sort(unique(vector)))
}


get_positions_after_modifiedLASSO <- function(lasso_step2_coefs){
  n_clusters <- length(lasso_step2_coefs)
  
  vector <- c()
  
  for (j in 1:n_clusters){
    new <- extract_time((lasso_step2_coefs[[j]]@Dimnames[[1]])[lasso_step2_coefs[[j]]@i[which(abs((lasso_step2_coefs[[j]]@x)) > 0)]])
    vector <- c(vector, na.omit(new)) # intercepts can be picked up
  }
  
  return(sort(unique(vector)))
}



# n = repeat times (with different seeds to average over)
# output: seed, accuracy
modifiedLASSO <- function(data, lasso_results, threshold, alpha = 1, lambda_grid = seq(0, 0.05, length.out = 101)){
  seed <- sample(1:50000, 1)
  set.seed(seed)
  
  cluster_assignment <- as.factor(as.vector(data[,ncol(data)]))
  
  positions_left <- get_positions_after_threshold(lasso_results, threshold)
  data_reduced <- data[,c(positions_left, ncol(data))]
  
  lasso_step2 <- train(CA ~ .,
                       data = data_reduced,
                       method = "glmnet",
                       family = "multinomial",
                       trControl = trainControl(method = "cv", number = 10),
                       tuneGrid = expand.grid(alpha = alpha, lambda = lambda_grid))
  
  accuracy <- sum(predict(lasso_step2, type = "raw") == cluster_assignment)
  
  lasso_step2_coefs <- coef(lasso_step2$finalModel, lasso_step2$finalModel$lambdaOpt)
  positions_left_step2 <- get_positions_after_modifiedLASSO(lasso_step2_coefs)
  
  n_positions_left <- length(positions_left)
  n_positions_left_step2 <- length(positions_left_step2)
  
  return(list(threshold, seed, accuracy, n_positions_left, positions_left, n_positions_left_step2, positions_left_step2))
}



