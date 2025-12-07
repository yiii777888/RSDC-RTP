library(caret)
preProc <- preProcess(train_data1, method = c("center", "scale"))
train_scaled <- predict(preProc, train_data1)
test_scaled <- predict(preProc, test_data1)
tuneGrid <- expand.grid(
    sigma = 2^seq(-15, 3, length.out = 5),  
    C = 2^seq(-5, 15, length.out = 5)       
)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)
set.seed(123)
svr_model <- train(
    X ~ Y_adj7,                
    data = train_scaled,  
    method = "svmRadial",
    tuneGrid = tuneGrid,
    trControl = ctrl,
    metric = "RMSE"       
)

pred_scaled <- predict(svr_model, newdata = test_scaled)
predictions <- pred_scaled * preProc$std[["X"]] + preProc$mean[["X"]]