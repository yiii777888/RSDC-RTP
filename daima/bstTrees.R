library(bst)        
library(caret)      
library(doParallel) 

train_x <- as.matrix(train_data1["Y"]) 
train_y <- train_data1$X 
test_x <- as.matrix(test_data1["Y"])
test_y <- test_data1$X

cl <- makePSOCKcluster(detectCores() - 1)
registerDoParallel(cl)


# 4. 参数调优
best_params <- NULL
best_rmse <- Inf
cv_folds <- 5

for(i in 1:nrow(param_grid)) {
    set.seed(123) 
    cv_rmse <- numeric(cv_folds)
    folds <- createFolds(train_y, k = cv_folds)    
    
    for (fold in 1:cv_folds) {
        train_idx <- unlist(folds[-fold])
        valid_idx <- folds[[fold]]        
        train_fold_x <- as.matrix(train_data1[train_idx, "Y", drop = FALSE])
        train_fold_y <- train_data1[train_idx, "X"]
        valid_fold_x <- as.matrix(train_data1[valid_idx, "Y", drop = FALSE])
        valid_fold_y <- train_data1[valid_idx, "X"]
        
        model <- bst(
            x = train_fold_x,
            y = train_fold_y,
            ctrl = bst_control(mstop = param_grid$mstop[i]),
            control.tree = list(maxdepth = param_grid$maxdepth[i]),
            learner = "tree"
        )
        pred <- predict(model, newx = valid_fold_x)
        cv_rmse[fold] <- sqrt(mean((pred - valid_fold_y)^2))
    }
    
    current_rmse <- mean(cv_rmse)     
    cat("参数组合", i, "/", nrow(param_grid), ":
      mstop=", param_grid$mstop[i],
        "maxdepth=", param_grid$maxdepth[i], 
        "| CV RMSE =", current_rmse, "\n")
        
    if (current_rmse < best_rmse) {
        best_rmse = current_rmse
        best_params <- param_grid[i, ]
    }
}

final_model <- bst(
  x = train_x,
  y = train_y,
  ctrl = bst_control(mstop = best_params$mstop),
  control.tree = list(maxdepth = best_params$maxdepth),
  learner = "tree"
)

train_pred <- predict(final_model, newx = train_x)
train_rmse <- sqrt(mean((train_pred - train_y)^2))

test_pred <- predict(final_model, newx = test_x)