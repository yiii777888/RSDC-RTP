library(caret)
ctrl <- trainControl(
    method = "cv",      
    number = 5,        
    verboseIter = TRUE  
) 
set.seed(123)
nnet.model <- train(
    X ~ Y_adj7,              
    data = train_data1,
    method = "nnet",    
    tuneGrid = tuneGrid,
    trControl = ctrl,
    trace = FALSE,      
    MaxNWts = 10000,    
    linout = TRUE,     
    metric = "RMSE"     )

predictions <- predict(nnet.model, newdata = test_data1)
