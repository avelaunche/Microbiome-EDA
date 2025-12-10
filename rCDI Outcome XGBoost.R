library(tidyverse)
library(compositions)
library(xgboost)
library(caret)
library(shapviz)
library(ROCR)

set.seed(125)

#0 is recurrent 1 is nonrecurrent 

outcome = dplyr::select(read_csv("data/species_df.csv"), "Patient ID", "Outcome", "Sample Time")
colnames(outcome) = c("Subject.Number", "outcome", "Visit.Name")
sam_subset = dplyr::select(sam_table, Subject.Number, Visit.Name, Visit.Name.Norm, TRT, Population, Abx.group_joined_M, CMINDC, rec.diagnosis)
sam_subset = filter(sam_subset, Visit.Name != "UNSCHED" & Visit.Name != "Screening")
nrow(sam_subset)
nrow(outcome)

sam_subset = sam_subset |>
  mutate(
      Visit.Name = case_when(
      Visit.Name == "Day 28" ~ 28,
      Visit.Name == "Day 7" ~ 7,
      Visit.Name == "Screening" ~ 1,
      Visit.Name == "Day 168" ~ 168,
      Visit.Name == "Day 115" ~ 115,
      Visit.Name == "Day 56" ~ 56,
      Visit.Name == "Day 1" ~ 1,
      Visit.Name == "Day 14" ~ 14
    )
  )

outcome <- outcome %>%
  distinct(Subject.Number, Visit.Name, .keep_all = TRUE)

sam_subset <- sam_subset %>%
  distinct(Subject.Number, Visit.Name, .keep_all = TRUE)

sam_subset$rowname = rownames(sam_subset)
sam_subset = right_join(sam_subset, outcome, relationship = "one-to-one")
sam_subset = na.omit(sam_subset)

rownames(sam_subset) = sam_subset$rowname
sam_subset$rowname = NULL

sam_subset$sample = rownames(sam_subset)
microbiomedata_t$sample = rownames(microbiomedata_t)

df_f = right_join(sam_subset, microbiomedata_t)

microbiomedata_t$sample = NULL

colnames(df_f)[1:30]
nrow(df_f)
nrow(filter(df_f, Visit.Name != 1))
df_f = filter(df_f, Visit.Name != 1)

df_f$CDIFF_PRESENCE = df_f$Clostridioides.difficile > 0

ggplot(filter(df_f, Visit.Name == 168), aes(x = outcome, fill = CDIFF_PRESENCE)) + 
  geom_bar(position = "fill") + 
  theme_minimal() + 
  labs(
    title = "C. difficile presence vs outcome",
    subtitle = "Day 168",
    fill = "C. difficile presence"
  )

outcome = df_f$outcome
df_f = dplyr::select(df_f, -c("Subject.Number", "Visit.Name", "Visit.Name.Norm", "TRT", "Population", "Abx.group_joined_M", "CMINDC", "rec.diagnosis", "sample", "outcome"))

microbiomedata_t_count = df_f > 0
row_sums <- apply(microbiomedata_t_count, 2, sum)
over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0+1/100]
over_10 = data.frame(over_10)
over_10 = rownames(over_10)
a_select = dplyr::select(df_f, all_of(over_10))

a2 <- a_select + 1e-6
a2  <- t(apply(a2, 1, clr))
a2 = as.data.frame(a2)

a2 = mutate(a2, ifelse(outcome > 5, 1, 0))
  
a2 = dplyr::select(a2, -Clostridioides.difficile)

a2$outcome = outcome

train_index <- createDataPartition(a2$outcome, p = 0.8, list = FALSE)
train_index
a2 = as.data.frame(a2)

X_train <- a2[train_index,]
y_train <- a2$outcome[train_index]
X_test  <- a2[-train_index,]
y_test  <- a2$outcome[-train_index]

X_train = dplyr::select(X_train, -outcome)
X_test = dplyr::select(X_test, -outcome)

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

feat = as.data.frame(shp$X)
res = as.data.frame(shp$S)

a = c()
b = c()
c = c()

for (x in colnames(feat)){
  print(x)
  if (!is.null(feat[,x])){
    print(cor(feat[,x], res[,x]))
    a = c(a, x)
    b = c(b, cor(feat[,x], res[,x]))
    c = c(c, cor.test(feat[,x], res[,x])$p.value)
  }
}
d = na.omit(data.frame(a, b, c))
d

filter(d, c < 0.05) |>
  arrange()


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

