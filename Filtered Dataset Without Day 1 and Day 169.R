colnames(df)

df_f = filter(df, visit_name != "Day 1" | visit_name != "Day 168")

head(colnames(df_f),20)

df_f = df_f[, -c(1:9)]

df_f

row_sums <- apply(df_f, 2, sum)
over_10 = row_sums[row_sums/nrow(df_f) > 0+1/100]
over_10 = data.frame(over_10)
over_10 = rownames(over_10)
a_select = dplyr::select(df_f, all_of(over_10))

a_select <- a_select + 1e-6
a_select  <- t(apply(a_select, 1, clr))
a_select = as.data.frame(a_select)


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
set.seed(123)

xgb_model <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 200,
  watchlist = list(train = xgb_train, test = xgb_test),
  early_stopping_rounds = 10,
  verbose = 0
)

paste("Accuracy on test dataset:", sum(round(predict(xgb_model, X_test)) == as.numeric(y_test))/length(y_test))

importance_matrix <- xgb.importance(model = xgb_model)

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

shp <- shapviz(
  xgb_model,
  X_pred = data.matrix(X_train),
  X = X_train
)

sv_importance(shp, kind = "beeswarm")

#prevotella histicola prodyces butyrate
#lactobacilius is a probiotic proven to affect it 

pred_prob <- predict(xgb_model, X_test, type = "prob")

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
