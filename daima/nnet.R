

nnet.model <- train(
    X ~ Y,              
    data = train_data,
    method = "nnet",    
    tuneGrid = tuneGrid,
    trace = FALSE,      
    MaxNWts = 10000,    
    linout = TRUE    )

predictions <- predict(nnet.model, newdata = test_data)
