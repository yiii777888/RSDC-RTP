
brnn_model <- brnn(Y ~ X,
                   data = train_data,
                   normalize = TRUE,
                   neurons = val_par1,
                   verbose = TRUE,
                   mu = 0.01,      
                   mu_dec = 0.1,
                   mu_inc = 10,
                   mu_max = 1e10)
predictions <- predict(brnn_model, test_data)
