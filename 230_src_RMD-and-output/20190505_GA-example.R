library(e1071)
library(GA)
p_load(mlbench)
data(Ozone, package="mlbench")
Data <- na.omit(Ozone)

# Setup the data for cross-validation
K = 5 # 5-fold cross-validation
fold_inds <- sample(1:K, nrow(Data), replace = TRUE)
lst_CV_data <- lapply(1:K, function(i) list(
    train_data = Data[fold_inds != i, , drop = FALSE], 
    test_data = Data[fold_inds == i, , drop = FALSE]))

lst_CV_data
# Given the values of parameters 'cost', 'gamma' and 'epsilon', return the rmse of the model over the test data
evalParams <- function(train_data, test_data, cost, gamma, epsilon) {
    # Train
    model <- svm(V4 ~ ., data = train_data, cost = cost, gamma = gamma, epsilon = epsilon, type = "eps-regression", kernel = "radial")
    # Test
    rmse <- mean((predict(model, newdata = test_data) - test_data$V4) ^ 2)
    return (rmse)
}

# Fitness function (to be maximized)
# Parameter vector x is: (cost, gamma, epsilon)
fitnessFunc <- function(x, Lst_CV_Data) {
    # Retrieve the SVM parameters
    cost_val <- x[1]
    gamma_val <- x[2]
    epsilon_val <- x[3]
    
    # Use cross-validation to estimate the RMSE for each split of the dataset
    rmse_vals <- sapply(Lst_CV_Data, 
                        function(in_data) with(in_data,
                                               evalParams(train_data, test_data, cost_val, gamma_val, epsilon_val)))
    
    # As fitness measure, return minus the average rmse (over the cross-validation folds), 
    # so that by maximizing fitness we are minimizing the rmse
    return (-mean(rmse_vals))
}

# Range of the parameter values to be tested
# Parameters are: (cost, gamma, epsilon)
theta_min <- c(cost = 1e-4, gamma = 1e-3, epsilon = 1e-2)
theta_max <- c(cost = 10, gamma = 2, epsilon = 2)

# Run the genetic algorithm
results <- ga(type = "real-valued", fitness = fitnessFunc, lst_CV_data, 
              names = names(theta_min), 
              min = theta_min, max = theta_max,
              popSize = 50, maxiter = 10)

summary(results)