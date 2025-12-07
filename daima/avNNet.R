
ctrl <- trainControl(
    method = "cv",       
    number = 5,           
    verboseIter = TRUE,   
    returnResamp = "all" 
)

set.seed(123)
avNNet_model <- caret::train(
    X ~ Y_adj7,                
    data = train_data1,  
    method = "avNNet",    
    linout = TRUE,        
    repeats = 5,          
    trace = FALSE,        
    MaxNWts = 5000,       
    trControl = ctrl,     
    tuneGrid = param_grid,
    metric = "RMSE"       
)
predictions <- predict(avNNet_model, newdata = test_data1)
