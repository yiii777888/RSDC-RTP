
ctrl <- trainControl(
    method = "cv",       
    number = 5,           
    verboseIter = TRUE,   
    returnResamp = "all" 
)


avNNet_model <- caret::train(
    X ~ Y,                
    data = train_data,  
    method = "avNNet",    
    linout = TRUE,        
    repeats = 5,          
    trace = FALSE,        
    MaxNWts = 5000,       
    trControl = ctrl,     
    tuneGrid = param_grid,
    metric = "RMSE"       
)
predictions <- predict(avNNet_model, newdata = test_data)
