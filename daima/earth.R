 
final_model <- earth(
    Y ~ X,  
    data = train_data,
    nprune = val_par2,
    degree = val_par1,
    Scale.y = FALSE
)
predictions <- predict(final_model, test_data)
