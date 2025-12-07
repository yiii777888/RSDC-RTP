require(stats, quietly = TRUE)
set.seed(123)  # 确保可重复性
find_best_nterms <- function(data, response_var = "X", max_terms = 5) {
    best_nterms <- 1
    best_mse <- Inf     
    for(nterms in 1:max_terms) {
        model <- tryCatch({
            ppr(as.formula(paste(response_var, "~ Y_adj6")), 
                data = data,
                nterms = nterms,
                max.terms = max_terms)  
        }, error = function(e) NULL)                 
        if(!is.null(model)) {
            pred <- predict(model, data)
            current_mse <- mean((data[[response_var]] - pred)^2)
            cat("测试项数:", nterms, "| MSE:", current_mse, "\n")            
            if(current_mse < best_mse) {
                best_mse <- current_mse
                best_nterms <- nterms
            }
        }
    }        
    return(best_nterms)
}
optimal_nterms <- find_best_nterms(train_data1, "X", max_terms = 5)
cat("\n最佳投影寻踪项数:", optimal_nterms, "\n")
ppr_model <- ppr(X ~ Y_adj6,
                 data = train_data1,
                 nterms = optimal_nterms,
                 max.terms = optimal_nterms + 2)
predictions <- predict(ppr_model, test_data1)