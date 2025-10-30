train_index <- createDataPartition(a_select$Clostridioides.difficile, p = 0.8, list = FALSE)

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

params <- list(
  objective = "reg:squarederror", # continuous outcome
  eval_metric = "rmse",
  eta = 0.10,
  max_depth = 4,
  gamma = 0.1,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.7
)

X <- as.matrix(x)


set.seed(123)

final_xgb <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 200,
  watchlist = list(train = xgb_train, test = xgb_test),
  early_stopping_rounds = 10
)

final_xgb

preds <- predict(final_xgb, X_test)
postResample(pred = preds, obs = y_test)
results.xgb = data.frame(y = y_test, preds = preds, class = "Test")
ggplot(results.xgb, aes(y, preds)) + 
  geom_point(alpha = 0.2) + 
  geom_abline()

final_xgb <- xgb.cv(
  params = params,
  data = xgb_train,
  nrounds = 200,
  watchlist = list(train = xgb_train, test = xgb_test),
  verbose = 1,
  nfold=5,
  early_stopping_rounds = 10
)

final_xgb

preds <- predict(xgb_model, X_train)
# Performance metrics
postResample(pred = preds, obs = y_train)

preds <- predict(xgb_model, X_test)
# Performance metrics
postResample(pred = preds, obs = y_test)

str(final_xgb, 1)
head(final_xgb$evaluation_log)

df <- final_xgb$evaluation_log

ggplot(df, aes(x = iter)) +
  geom_line(aes(y = train_rmse_mean, color = "train")) +
  geom_line(aes(y = test_rmse_mean, color = "test")) +
  labs(y = "RMSE", title = "xgboost cross‑validation performance") +
  theme_minimal()

length(y_test)

length(preds)

preds.test <- predict(xgb_model, X_test)

results.xgb.test = data.frame(y = y_test, preds = preds.test, class = "Test")

ggplot(results.xgb.test, aes(y, preds)) + 
  geom_point() + 
  geom_abline()


preds.train <- predict(xgb_model, X_train)

results.xgb.train = data.frame(y = y_train, preds = preds.train, class = "Train")

ggplot(results.xgb.train, aes(y, preds)) + 
  geom_point() + 
  geom_abline()

results.xgb = rbind(results.xgb.train, results.xgb.test)

ggplot(results.xgb, aes(y, preds, color = class)) + 
  geom_point() + 
  geom_abline()

class(final_xgb)
names(final_xgb)



final_xgb$pred

#BEST ITERATION
best_iteration = final_xgb$best_iteration

final_model <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = best_iteration,
  verbose = 0, 
  early_stopping_rounds = 10,
  watchlist = list(train = xgb_train, test = xgb_test)
)

final_model

best_iterationimportance_matrix <- xgb.importance(model = final_model)
head(importance_matrix)
xgb.plot.importance(importance_matrix, top_n = 20)

preds <- predict(final_model, X_test)
postResample(pred = preds, obs = y_test)


results.xgb = data.frame(y = y_test, preds = preds, class = "Test")

ggplot(results.xgb, aes(y, preds)) + 
  geom_point() + 
  geom_abline()

preds <- predict(final_model, X_train)
postResample(pred = preds, obs = y_train)
results.xgb = data.frame(y = y_train, preds = preds, class = "Test")
ggplot(results.xgb, aes(y, preds)) + 
  geom_point() + 
  geom_abline()
