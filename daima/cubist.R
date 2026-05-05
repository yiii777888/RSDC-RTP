  
 final_model <- cubist(x = train_x,
                      y = train_y,
                      committees = val_par1)

predictions <- predict(final_model, 
                      newdata = test_x,
                      neighbors = val_par2)
