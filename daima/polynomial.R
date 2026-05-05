 poly_reg <- function(x, y, degree) {
    model <- lm(y ~ poly(x, degree, raw = TRUE))
    return(model)
}
final_model <- poly_reg(train_set$X, train_set$Y, val_par1)
valid_predictions <- predict(final_model, data.frame(x = valid_set$X))
