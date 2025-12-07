library(stats)
set.seed(123)  
poly_reg <- function(x, y, degree) {
    model <- lm(y ~ poly(x, degree, raw = TRUE))
    return(model)
}
find_best_degree <- function(x, y, max_degree = 5) {
    best_degree <- 1
    best_rmse <- Inf    
    
    degree_seq <- seq(0.5, max_degree, by = 0.5)    
    # 5折交叉验证
    folds <- sample(rep(1:5, length.out = length(x)))     
    for(degree in degree_seq) {
        cv_errors <- numeric(5)        
        for(fold in 1:5) {
            train_idx <- which(folds != fold)
            test_idx <- which(folds == fold)            
            model <- tryCatch({
                poly_reg(x[train_idx], y[train_idx], degree)
            }, error = function(e) NULL)            
            if(!is.null(model)) {
                pred <- predict(model, data.frame(x = x[test_idx]))
                cv_errors[fold] <- sqrt(mean((y[test_idx] - pred)^2))
            } else {
                cv_errors[fold] <- Inf
            }
        }        
        current_rmse <- mean(cv_errors)        
        if(current_rmse < best_rmse) {
            best_rmse <- current_rmse
            best_degree <- degree
        }
    }    
    return(best_degree)
}
optimal_degree <- find_best_degree(train_set$Y, train_set$X, max_degree = 5)
final_model <- poly_reg(train_set$Y, train_set$X, optimal_degree)
valid_predictions <- predict(final_model, data.frame(x = valid_set$Y))