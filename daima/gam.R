
final_model <- gam(a
    Y ~ s(X, k = val_par2),
    data = new_train,
    select = val_par1
)
test_predictions <- predict(final_model, test_data)
