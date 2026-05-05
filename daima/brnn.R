
brnn_model <- brnn(Y ~ X,
                   data = train_data,
                   normalize = TRUE,
                   neurons = val_par1,
                   verbose = TRUE,)
predictions <- predict(brnn_model, test_data)
