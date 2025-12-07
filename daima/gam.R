library(caret) 
library(mgcv)

set.seed(123)  
train_index <- createDataPartition(train_data1$X, p = 0.7, list = FALSE)
new_train <- train_data1[train_index, ]  new_test <- train_data1[-train_index, ]  
tune_gam <- function(train_data, 
                     smooth_range = c(TRUE, FALSE),
                     k_range = 5:10) {
    
    best_rmse <- Inf
    best_params <- list(select = NULL, k = NULL)
    folds <- createFolds(train_data$X, k = 5)    
    for (select_smooth in smooth_range) {
        for (k in k_range) {
            cv_rmse <- numeric(5)            
            for (i in 1:5) {
                train_idx <- unlist(folds[-i])
                val_idx <- folds[[i]]                
                gam_model <- gam(
                    X ~ s(Y, k = k),
                    data = train_data[train_idx, ],
                    select = select_smooth
                )                
                pred <- predict(gam_model, train_data[val_idx, ])
                cv_rmse[i] <- sqrt(mean((pred - train_data$X[val_idx])^2))
            }            
            avg_rmse <- mean(cv_rmse)
            cat("平滑选择:", select_smooth, "| k:", k, "| CV RMSE:", avg_rmse, "\n")            
            if (avg_rmse < best_rmse) {
                best_rmse <- avg_rmse
                best_params$select <- select_smooth
                best_params$k <- k
            }
        }
    }    
    return(list(
        best_params = best_params,
        best_rmse = best_rmse
    ))
}
final_model <- gam(
    X ~ s(Y, k = tune_result$best_params$k),
    data = new_train,
    select = tune_result$best_params$select
)
final_model <- gam(
    X ~ s(Y_adj12, k = tune_result$best_params$k),
    data = new_train,
    select = tune_result$best_params$select
)
test_predictions <- predict(final_model, test_data13)
