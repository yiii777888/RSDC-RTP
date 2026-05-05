
ppr_model <- ppr(Y ~ X,
                 data = train_data,
                 nterms = val_par1,
                 max.terms = val_par1 + 2)
predictions <- predict(ppr_model, test_data)
