ctrl <- trainControl(
    method = "cv",      
    number = 5,        
    verboseIter = TRUE  
) 

nnet.model <- train(
    X ~ Y,              
    data = train_data,
    method = "nnet",    
    tuneGrid = tuneGrid,
    trControl = ctrl,
    trace = FALSE,      
    MaxNWts = 10000,    
    linout = TRUE,     
    metric = "RMSE"     )

predictions <- predict(nnet.model, newdata = test_data)
