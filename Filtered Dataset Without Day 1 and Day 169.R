library(tidyverse)

set.seed(125)
sam_subset$sample = rownames(sam_subset)
microbiomedata_t$sample = rownames(microbiomedata_t)

df_f = right_join(sam_subset, microbiomedata_t)

microbiomedata_t$sample = NULL

colnames(df_f)[1:30]
nrow(df_f)
nrow(filter(df_f, visit_name != "Day 1" & visit_name != "Day 168"))
df_f = filter(df_f, visit_name != "Day 1" & visit_name != "Day 168")
df_f = df_f[, -c(1:9)]
colnames(df_f)[1:30]

ncol(df_f)
nrow(df_f)

microbiomedata_t_count = df_f > 0
row_sums <- apply(microbiomedata_t_count, 2, sum)
over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0+1/100]
over_10 = data.frame(over_10)
over_10 = rownames(over_10)
a_select = dplyr::select(df_f, all_of(over_10))

ncol(a_select)

a2 <- a_select + 1e-6
a2  <- t(apply(a2, 1, clr))
a2 = as.data.frame(a2)

a2

a2$CDIFF_PRESENCE = a2$Clostridioides.difficile > 0
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

pred_rocr <- prediction(pred_prob, y_test)

perf_roc <- performance(pred_rocr, measure = "tpr", x.measure = "fpr")

plot(perf_roc, col = "blue", lwd = 2)

plot(perf_roc_NULL, col = "red", lwd = 2, add = TRUE)

legend("bottomright",
       legend = c("Model", "Null model"),
       col = c("blue", "red"),
       lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
title("ROC Curve for Null vs Control XGBoost Model")

# Calculate AUC
auc_perf <- performance(pred_rocr, measure = "auc")
auc_value <- auc_perf@y.values[[1]]
auc_value
