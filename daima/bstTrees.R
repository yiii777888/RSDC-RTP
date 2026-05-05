
final_model <- bst(
  x = train_x,
  y = train_y,
  ctrl = bst_control(mstop = val_par1),
  control.tree = list(maxdepth = val_par2),
  learner = "tree"
)

train_pred <- predict(final_model, newx = train_x)
train_rmse <- sqrt(mean((train_pred - train_y)^2))

test_pred <- predict(final_model, newx = test_x)
