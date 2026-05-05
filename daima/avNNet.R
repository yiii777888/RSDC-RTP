
avNNet_model <- caret::train(
    Y ~ X,                
    data = train_data,  
    method = "avNNet",    
    linout = TRUE,        
    repeats = 5,          
    trace = FALSE,        
    MaxNWts = 5000,            
    tuneGrid = param_grid)
predictions <- predict(avNNet_model, newdata = test_data)
