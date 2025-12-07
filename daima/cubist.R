require(Cubist, quietly = TRUE)    

train_x <- data.frame(Y = train_data1$Y)
train_y <- train_data1$X
test_x <- data.frame(Y = test_data1$Y)
test_y <- test_data1$X
results <- list()
for (committees in committees_values) {
    for (neighbors in neighbors_values) {
        cubist.model <- cubist(x = train_x, 
                               y = train_y,
                               committees = committees)
        
        predictions <- predict(cubist.model, 
                               newdata = test_x,
                               neighbors = neighbors)
        
        # 跳过常数预测
        if (sd(predictions) == 0) {
            warning(paste("常数预测: committees =", committees,
                          "neighbors =", neighbors))
            next
        }
       
        mse <- mean((predictions - test_y)^2)
        r_squared <- cor(predictions, test_y)^2
        
        results[[paste("committees", committees, "neighbors", neighbors)]] <- list(
            model = cubist.model,
            predictions = predictions,
            mse = mse,
            r_squared = r_squared,
            params = list(committees = committees, neighbors = neighbors)
        )
    }
}
mse_values <- sapply(results, function(x) x$mse)
best_index <- which.min(mse_values)
best_params <- results[[best_index]]$params
best_mse <- results[[best_index]]$mse
best_r2 <- results[[best_index]]$r_squared
 
final_model <- cubist(x = train_x,
                      y = train_y,
                      committees = best_params$committees)

predictions <- predict(final_model, 
                      newdata = test_x,
                      neighbors = best_params$neighbors)