tuneGrid <- expand.grid(
    sigma = 2^seq(-15, 3, length.out = 5),  
    C = 2^seq(-5, 15, length.out = 5)       
)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

svr_model <- train(
    X ~ Y,                
    data = train_scaled,  
    method = "svmRadial",
    tuneGrid = tuneGrid,
    trControl = ctrl,
    metric = "RMSE"       
)

pred_scaled <- predict(svr_model, newdata = test_scaled)
predictions <- pred_scaled * preProc$std[["X"]] + preProc$mean[["X"]]
