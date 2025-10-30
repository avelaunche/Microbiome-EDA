library(shapviz)
library(caret)
library(xgboost)
library(compositions)
library(dplyr)
microbiomedata_t_count = microbiomedata_t > 0
row_sums <- apply(microbiomedata_t_count, 2, sum)
over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0+1/100]
over_10 = data.frame(over_10)
over_10 = rownames(over_10)
a_select = dplyr::select(microbiomedata_t, all_of(over_10))

CDIFF_PRESENCE = a_select$Clostridioides.difficile > 0
a_select <- a_select + 1e-6
a_select  <- t(apply(a_select, 1, clr))
a_select = as.data.frame(a_select)
CDIFF_PRESENCE = as.factor(CDIFF_PRESENCE)
a2$CDIFF_PRESENCE = CDIFF_PRESENCE
a2 = dplyr::select(a2, -Clostridioides.difficile)

train_index <- createDataPartition(a2$CDIFF_PRESENCE, p = 0.8, list = FALSE)
train_index
a2 = as.data.frame(a2)

X_train <- a2[train_index,]
y_train <- a2$CDIFF_PRESENCE[train_index]
X_test  <- a2[-train_index,]
y_test  <- a2$CDIFF_PRESENCE[-train_index]

X_train = dplyr::select(X_train, -CDIFF_PRESENCE)
X_test = dplyr::select(X_test, -CDIFF_PRESENCE)

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

#Basic xgb_model

xgb_model <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 200,
  watchlist = list(train = xgb_train, test = xgb_test),
  early_stopping_rounds = 10,
  verbose = 0
)

xgb_model
paste("Accuracy on test dataset:", sum(round(predict(xgb_model, X_test)) == as.numeric(y_test))/length(y_test))


#Cross validation
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

xgb_model_classification <- train(
  x = X_train,
  y = y_train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = xgb_grid
)

xgb_model_classification$results |>
  arrange(desc(Accuracy))

print("Accuracy on test dataset:")
paste("Accuracy on test dataset:", sum((predict(xgb_model_classification, X_test)) == y_test)/length(y_test))

best <- xgb_model_classification$bestTune
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

paste("Accuracy on test dataset:", sum((round(predict(final_booster, X_test))) == as.numeric(y_test))/length(y_test))

round(predict(final_booster, X_test))

final_booster

importance_matrix <- xgb.importance(model = final_booster)

shp <- shapviz(
  final_booster,
  X_pred = data.matrix(X_train),
  X = X_train
)

sv_importance(shp, kind = "beeswarm")

best_importance = importance_matrix |>
  arrange(desc(Gain)) |>
  head(30)

ggplot(best_importance, aes(Gain, fct_reorder(as.factor(Feature), Gain), fill = Feature)) + 
  geom_col() + 
  labs(
    title = "XGBoost Classification Feature Importance",
    subtitle = "C. Difficile with various gut microbiota",
    y = "Species",
    x = "Feature Importance"
  ) + 
  theme_bw() + 
  theme(legend.position = "off")

pred_prob <- predict(final_booster, X_test, type = "prob")

pred_prob

pred_rocr <- prediction(pred_prob, y_test)

# Calculate TPR/FPR for ROC
perf_roc <- performance(pred_rocr, measure = "tpr", x.measure = "fpr")

# Plot the ROC curve
plot(perf_roc, col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
title("ROC Curve for XGBoost Model")

# Calculate AUC
auc_perf <- performance(pred_rocr, measure = "auc")
auc_value <- auc_perf@y.values[[1]]
auc_value
