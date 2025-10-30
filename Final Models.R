library(caret)
library(compositions)
library(tidyverse)
library(xgboost)

#Spearman Correlation

Y = microbiomedata_t |>
  dplyr::select(Clostridioides.difficile)

X = microbiomedata_t |>
  dplyr::select(-Clostridioides.difficile)

for (x in colnames(X)){
  cors = cor.test(as.numeric(X[1:495, x]), y = as.numeric(cdiff[1:495]), method = "spearman")
  species = c(species, x)
  corrs = c(corrs, cors$estimate)
  pvalue = c(pvalue, cors$p.value)
}

pvalue = p.adjust(pvalue)

cor_df = data.frame("species" = species, "correlation" = corrs, "p_value" = pvalue)

cor_df |>
  arrange(p_value)

cor_relevant = cor_df |>
  filter(pvalue < 0.05) |>
  arrange(p_value)

#sPLS Regression

fit <- spls(X ,cdiff, ncomp = 3, keepX = c(30,30,30))

#keeps the results
sPLS_res = selectVar(fit, comp = 1)
sPLS_res.X = sPLS_res$X
vip_scores <- mixOmics::vip(fit)

sPLS_res.X
vip_scores = as.data.frame(vip_scores)
vip_scores = vip_scores |>
  arrange(desc(comp1), desc(comp2), desc(comp3)) |>
  filter(!comp1 == 0 & !comp2 == 0 & !comp3 == 0)

vip_scores$species = rownames(vip_scores)

vip_scores

vip_scores = mutate(vip_scores, tot = comp1 + comp2 + comp3)

spls_select_species = vip_scores

#Data generation
microbiomedata_t_count = microbiomedata_t > 0
row_sums <- apply(microbiomedata_t_count, 2, sum)

over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0+1/100]

over_10 = data.frame(over_10)

over_10 = rownames(over_10)

a_select = dplyr::select(microbiomedata_t, all_of(over_10))

RF_data = clr(a_select)

#Random Forest Regressor

rf_model_hypertuned = randomForest(
  data = RF_data, 
  Clostridioides.difficile ~ ., 
  mtry = 271,
  ntree = 2000,
  importance = TRUE,
  nodesize = 5
)

best_model = rf_model_hypertuned 

best_importance = randomForest::importance(best_model)
best_importance = as.data.frame(best_importance)
best_importance$species = rownames(best_importance)
best_importance = arrange(best_importance, desc(`%IncMSE`))
best_importance = head(best_importance, 30)
best_importance

#Random Forest Classifier
RF_data_classifier = as.data.frame(RF_data)
RF_data_classifier = mutate(RF_data_classifier, CDIFF_PRESENCE = Clostridioides.difficile > 0)
RF_data_classifier = dplyr::select(-Clostridioides.difficile)
RF_data_classifier$CDIFF_PRESENCE = as.factor(RF_data_classifier$CDIFF_PRESENCE)

rf_model_hypertuned = randomForest(
  data = RF_data_classifier, 
  CDIFF_PRESENCE ~ ., 
  mtry = 345,
  ntree = 2000,
  importance = TRUE,
  nodesize = 3
)

best_importance = randomForest::importance(rf_model_hypertuned)
best_importance
best_importance = as.data.frame(best_importance)
best_importance$species = rownames(best_importance)
best_importance = arrange(best_importance, desc(MeanDecreaseAccuracy))
best_importance = head(best_importance, 30)
best_importance

#XGBoost Regressor
train_index <- createDataPartition(RF_data$Clostridioides.difficile, p = 0.8, list = FALSE)
train_index
XG_data = as.data.frame(RF_data)

X_train <- XG_data[train_index,]
y_train <- XG_data$Clostridioides.difficile[train_index]
X_test  <- XG_data[-train_index,]
y_test  <- XG_data$Clostridioides.difficile[-train_index]

X_train = dplyr::select(X_train, -Clostridioides.difficile)
X_test = dplyr::select(X_test, -Clostridioides.difficile)

class(X_train)

X_train <- as.matrix(X_train)
X_test  <- as.matrix(X_test)

xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
xgb_test  <- xgb.DMatrix(data = X_test, label = y_test)

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

final_xg_regressor <- xgb.train(params = params, data = xgb_train,
                           nrounds = best$nrounds)

pred_regress = predict(final_xg_regressor, X_test)
postResample(pred = pred_regress, obs = y_test)


#XGBoost Classifier
train_index <- createDataPartition(RF_data_classifier$CDIFF_PRESENCE, p = 0.8, list = FALSE)

XG_data_classifier = as.data.frame(RF_data_classifier)

X_train <- XG_data_classifier[train_index,]
y_train <- XG_data_classifier$CDIFF_PRESENCE[train_index]
X_test  <- XG_data_classifier[-train_index,]
y_test  <- XG_data_classifier$CDIFF_PRESENCE[-train_index]

X_train = dplyr::select(X_train, -CDIFF_PRESENCE)
X_test = dplyr::select(X_test, -CDIFF_PRESENCE)

class(X_train)

X_train <- as.matrix(X_train)
X_test  <- as.matrix(X_test)

xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
xgb_test  <- xgb.DMatrix(data = X_test, label = y_test)

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

final_xgb_classify <- xgb.train(params = params, data = xgb_train,
                           nrounds = best$nrounds)

paste("Accuracy on test dataset:", sum((predict(final_xgb_classify, X_test)) == y_test)/length(y_test))
