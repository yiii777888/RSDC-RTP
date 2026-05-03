find_best_neurons <- function(data, response_var = "X", max_neurons = 10) {
    best_neurons <- 1
    best_rmse <- Inf    
  
    neurons_seq <- unique(round(seq(2, max_neurons, length.out = 5))) 
    folds <- cut(seq_along(data[[response_var]]), 
                 breaks = quantile(data[[response_var]], probs = seq(0, 1, 0.2)),
                 labels = FALSE)    
    cat("开始神经元数量调优...\n")
    for(neurons in neurons_seq) {
        cv_errors <- numeric(5)        
        for(fold in 1:5) {
            train_idx <- which(folds != fold)
            valid_idx <- which(folds == fold)            
            model <- tryCatch({
                brnn(as.formula(paste(response_var, "~ Y")),
                     data = data[train_idx, ],
                     normalize = TRUE,  
                     neurons = neurons,
                     verbose = FALSE,
                     mu = 0.01,  
                     mu_dec = 0.1,
                     mu_inc = 10,
                     mu_max = 1e10)
            }, error = function(e) NULL)            
            if(!is.null(model)) {
                pred <- predict(model, data[valid_idx, ])
                cv_errors[fold] <- sqrt(mean((data[valid_idx, response_var] - pred)^2))
            } else {
                cv_errors[fold] <- Inf
            }
        }        
        current_rmse <- mean(cv_errors)
        cat("测试神经元数:", neurons, "| CV RMSE:", current_rmse, "\n")
        
        if(current_rmse < best_rmse) {
            best_rmse <- current_rmse
            best_neurons <- neurons
        }
    }    
    cat("\n最佳神经元数:", best_neurons, "| 最佳CV RMSE:", best_rmse, "\n")
    return(best_neurons)
}
optimal_neurons <- find_best_neurons(train_data, "X", max_neurons = 10)

brnn_model <- brnn(X ~ Y,
                   data = train_data,
                   normalize = TRUE,
                   neurons = optimal_neurons,
                   verbose = TRUE,
                   mu = 0.01,      
                   mu_dec = 0.1,
                   mu_inc = 10,
                   mu_max = 1e10)
predictions <- predict(brnn_model, test_data)
