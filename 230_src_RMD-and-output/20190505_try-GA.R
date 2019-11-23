#https://stackoverflow.com/questions/32026436/how-to-optimize-parameters-using-genetic-algorithms


p_load(GA)

### PART I

opt_fn <- function(x) {
    
    xgb_model <- xgb.train(
        params = list(
            booster = "gbtree"
            ,objective = "reg:linear"
            ,eval_metric = "rmse"
            ,eta = x[2])
        ,data = my_train
        ,watchlist = watchlist
        ,nrounds = x[1]
        ,early_stopping_rounds = 3
        ,verbose = TRUE
        ,prediction = TRUE) 
    
    Score = -xgb_model$evaluation_log[xgb_model$best_iteration]$test_rmse
    
}


# Range of the parameter values to be tested
lower <- c(nrounds = 5, eta = 0.1)
upper <- c(nrounds = 5, eta = 0.3)


# Run the genetic algorithm
results <- ga(type = "real-valued",
              fitness = opt_fn,
              names = names(lower), 
              lower = lower,
              upper =  upper,
              popSize = 10, 
              maxiter = 10,
              monitor = monitor)


p_load(xgboost)
### PART II

opt_fn <- function(x) {
    
    xgb_model <- xgb.train(
        params = list(
            booster = "gbtree"
            ,objective = "reg:linear"
            ,eval_metric = "rmse"
            ,eta = 0.06
            
            ,max_depth = round(x[1],0)
            ,min_child_weight = x[2] 
            ,gamma = x[3]
            ,subsample = x[4]
            ,colsample_bytree = x[5]   
                
            ,alpha = x[6]
            ,lambda = x[7]
            
            )
        ,data = my_train
        ,watchlist = watchlist
        ,nrounds = 300
        ,early_stopping_rounds = 3
        ,verbose = 0
        ,prediction = TRUE) 
    
    Score = -xgb_model$evaluation_log[xgb_model$best_iteration]$test_rmse
    
}


# Range of the parameter values to be tested
lower <- c(max_depth = 2L,min_child_weight = 1L,gamma = 0L,subsample = 0.5,colsample_bytree = 0.5
           ,alpha = 0,lambda = 0)
upper <- c(max_depth = 8L,min_child_weight = 5L,gamma = 50L,subsample = 1,colsample_bytree = 1
           ,alpha = 1,lambda = 1)

library(pacman)
p_load(GA)
# Run the genetic algorithm
results <- ga(type = "real-valued",
              fitness = opt_fn,
              names = names(lower), 
              lower = lower,
              upper =  upper,
              popSize = 10, 
              maxiter = 2,
              bestSol = TRUE)


plot(results)
