
svr_model <- train(
    Y ~ X,                
    data = train_scaled,  
    method = "svmRadial",
    tuneGrid = tuneGrid )

pred_scaled <- predict(svr_model, newdata = test_scaled)
predictions <- pred_scaled * preProc$std[["X"]] + preProc$mean[["X"]]
