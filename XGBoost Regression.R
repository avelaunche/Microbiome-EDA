library(xgboost)

train_index <- createDataPartition(a_select$Clostridioides.difficile, p = 0.8, list = FALSE)
train_index
a_select = as.data.frame(a_select)

X_train <- a_select[train_index,]
y_train <- a_select$Clostridioides.difficile[train_index]
X_test  <- a_select[-train_index,]
y_test  <- a_select$Clostridioides.difficile[-train_index]

X_train = dplyr::select(X_train, -Clostridioides.difficile)
X_test = dplyr::select(X_test, -Clostridioides.difficile)

class(X_train)

X_train <- as.matrix(X_train)
X_test  <- as.matrix(X_test)

xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
xgb_test  <- xgb.DMatrix(data = X_test, label = y_test)

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,          # learning rate
  max_depth = 6,      # tree depth
  subsample = 0.8,    # row sampling
  colsample_bytree = 0.8 # feature sampling
)

xgb_model <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 200,
  watchlist = list(train = xgb_train, test = xgb_test),
  early_stopping_rounds = 10,
  verbose = 0
)

# Predict on test data
preds <- predict(xgb_model, X_test)

hi = data.frame(preds, y_test)

ggplot(hi, aes(preds, y_test)) + 
  geom_point() + 
  geom_abline(col = "red", size = 1) +
  theme_bw() + 
  labs(
    title = "XGBoost Regressor Predicted vs Actual Results",
    x = "Predicted",
    y = "Actual"
  )

# Performance metrics
postResample(pred = preds, obs = y_test)

# Define parameter grid
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(4, 6, 8),
  eta = c(0.01, 0.05, 0.1),
  gamma = c(0, 0.1),
  colsample_bytree = c(0.7, 0.8),
  min_child_weight = c(1, 3),
  subsample = c(0.7, 0.8)
)

# Set up cross-validation
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

xgb_model_regressor <- train(
  x = X_train,
  y = y_train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = xgb_grid
)

xgb_model_regressor$finalModel

xgb_model_regressor$results |>
  arrange(desc(Rsquared))

preds = predict(xgb_model_regressor, X_train)
postResample(pred = preds, obs = y_train)
results.xgb = data.frame(y = y_train, preds = preds, class = "Test")
ggplot(results.xgb, aes(y, preds)) + 
  geom_point() + 
  geom_abline()

preds = predict(xgb_model_regressor, X_test)
postResample(pred = preds, obs = y_test)
results.xgb = data.frame(y = y_test, preds = preds, class = "Test")
ggplot(results.xgb, aes(y, preds)) + 
  geom_point() + 
  geom_abline()


best <- xgb_model_regressor$bestTune
params <- list(
  objective = "reg:squarederror",
  eta = best$eta,
  max_depth = best$max_depth,
  gamma = best$gamma,
  subsample = best$subsample,
  colsample_bytree = best$colsample_bytree,
  min_child_weight = best$min_child_weight
)

final_booster <- xgb.train(params = params, data = xgb_train,
                           nrounds = best$nrounds)

importance_matrix <- xgb.importance(model = final_booster)

importance_matrix

best_importance = importance_matrix |>
  arrange(desc(Importance)) |>
  head(30)

ggplot(best_importance, aes(Importance, fct_reorder(as.factor(Feature), Importance), fill = Feature)) + 
  geom_col() + 
  labs(
    title = "XGBoost Regression Feature Importance",
    subtitle = "C. Difficile with various gut microbiota",
    y = "Species",
    x = "Feature Importance"
  ) + 
  theme_bw() + 
  theme(legend.position = "off")

