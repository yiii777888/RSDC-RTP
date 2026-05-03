tune_mars <- function(train_data, 
                      nprune_range = 2:10,     
                      degree_range = 1:3) {       
    best_rmse <- Inf
    best_params <- list(nprune = NULL, degree = NULL)
    folds <- createFolds(train_data$X, k = 5)         
    for (nprune in nprune_range) {
        for (degree in degree_range) {
            cv_rmse <- numeric(5)             
            for (i in 1:5) {
                train_idx <- unlist(folds[-i])
                val_idx <- folds[[i]]                
                mars_model <- earth(
                    X ~ Y,  
                    data = train_data[train_idx, ],
                    nprune = nprune,
                    degree = degree,
                    Scale.y = FALSE  
                )                
                pred <- predict(mars_model, train_data[val_idx, ])
                cv_rmse[i] <- sqrt(mean((pred - train_data$X[val_idx])^2))
            }                        
            avg_rmse <- mean(cv_rmse)
            cat("nprune:", nprune, "| degree:", degree, "| CV RMSE:", avg_rmse, "\n")                         
            if (avg_rmse < best_rmse) {
                best_rmse = avg_rmse
                best_params$nprune = nprune
                best_params$degree = degree
            }
        }
    }        
    return(list(
        best_params = best_params,
        best_rmse = best_rmse
    ))
}
 
final_model <- earth(
    X ~ Y,  
    data = train_data,
    nprune = tune_result$best_params$nprune,
    degree = tune_result$best_params$degree,
    Scale.y = FALSE
)
predictions <- predict(final_model, test_data1)
