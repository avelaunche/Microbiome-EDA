library(janitor)
library(randomForest)
library(ranger)
library(iml)
library(ggplot2)
library(dplyr)
library(ggbeeswarm)
library(tidyr)
library(tibble)
library(compositions)
library(forcats)
library(ROCR)

#Classify by outcome
table(sam_subset$Visit.Name)
length(unique(sam_subset$Subject.Number))

colnames(sam_subset) == rownames(sam_subset)

sam_subset = clean_names(sam_subset)

colnames(sam_subset)

ncol(microbiomedata_t)

a = rownames_to_column(microbiomedata_t) |>
  left_join(rownames_to_column(sam_sub_sub))

ggplot(a, aes(Clostridioides.difficile, fill = rec_diagnosis)) +
  geom_histogram()

a = dplyr::select(a, -rowname)
a = dplyr::select(a, -Clostridioides.difficile)


str(microbiomedata_t)

a$rec_diagnosis = as.factor(a$rec_diagnosis)

b = microbiomedata_t$Clostridioides.difficile > 0

g = data.frame(microbiomedata_t, CDIFF_PRESENCE = b)

g$CDIFF_PRESENCE = as.factor(g$CDIFF_PRESENCE)

g = dplyr::select(g, -Clostridioides.difficile)


rf_model = randomForest(
  data = g,
  CDIFF_PRESENCE ~ .,
  ntree = 500,
  mtry = 2,
  importance = TRUE,
)

rf_model

#classification with normalization
a_select <- a_select + 1e-6
a_select  <- t(apply(a_select, 1, clr))
a_select = as.data.frame(a_select)


a2$CDIFF_PRESENCE = as.factor(a2$CDIFF_PRESENCE)

rf_model_classify_normalize = randomForest(
  data = a2,
  CDIFF_PRESENCE ~ .,
  ntree = 500,
  mtry = 2,
  importance = TRUE,
)

rf_model_classify_normalize

#classification with rowsums

row_sums <- apply(microbiomedata_t_count, 2, sum)

over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0.01]
over_10
over_10 = data.frame(over_10)
over_10 = rownames(over_10)

a_select = dplyr::select(microbiomedata_t, all_of(over_10))
a2 = data.frame(a_select, CDIFF_PRESENCE = a_select$Clostridioides.difficile > 0)
a2 = dplyr::select(a2, -Clostridioides.difficile)
a2$CDIFF_PRESENCE = as.factor(a2$CDIFF_PRESENCE)

rf_model_classify_normalize = randomForest(
  data = a2,
  CDIFF_PRESENCE ~ .,
  ntree = 500,
  mtry = 2,
  importance = TRUE,
)

rf_model_classify_normalize

#classification with rowsums and clr
row_sums <- apply(microbiomedata_t_count, 2, sum)

over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0.01]
over_10
over_10 = data.frame(over_10)
over_10 = rownames(over_10)

a_select = dplyr::select(microbiomedata_t, over_10)

a_select <- a_select + 1e-6
a_select  <- t(apply(a_select, 1, clr))
a_select = as.data.frame(a_select)

a2 = data.frame(a_select, CDIFF_PRESENCE = a_select$Clostridioides.difficile > 0)
a2 = dplyr::select(a2, -Clostridioides.difficile)
a2$CDIFF_PRESENCE = as.factor(a2$CDIFF_PRESENCE)

rf_model_classify_normalize = randomForest(
  data = a2,
  CDIFF_PRESENCE ~ .,
  ntree = 500,
  mtry = 2,
  importance = TRUE,
)

rf_model_classify_normalize





#hyperparameter tuning

n_features = ncol(a2)

ames_rf1 <- ranger(
  CDIFF_PRESENCE ~ ., 
  data = a2,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123
)

(default_rmse <- sqrt(ames_rf1$prediction.error))


hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)

# execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = CDIFF_PRESENCE ~ ., 
    data            = a2, 
    num.trees       = 200,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order'
  )
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}



# assess top 10 models
top_ten_models = hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)

top_ten_models

rf_model_hypertuned = randomForest(
  data = a2, 
  CDIFF_PRESENCE ~ ., 
  mtry = 345,
  ntree = 500,
  importance = TRUE,
  nodesize = 3
)



rf_model_hypertuned

best_importance_rf = randomForest::importance(rf_model_hypertuned)
best_importance_rf
best_importance_rf = as.data.frame(best_importance_rf)
best_importance_rf$species = rownames(best_importance_rf)
best_importance_rf = arrange(best_importance_rf, desc(MeanDecreaseAccuracy))
best_importance_rf = head(best_importance_rf, 30)
best_importance_rf


ggplot(best_importance_rf, aes(MeanDecreaseAccuracy, fct_reorder(as.factor(species), MeanDecreaseAccuracy), fill = species)) + 
  geom_col() + 
  labs(
    title = "Random Forest Feature Importance",
    subtitle = "C. Difficile with various gut microbiota",
    y = "Species",
    x = "Variable Weight"
  ) + 
  theme_bw() + 
  theme(legend.position = "off")

rf_model_hypertuned


a3 = dplyr::select(a2, -CDIFF_PRESENCE)

stopifnot(all(names(a3) == setdiff(names(a2), "CDIFF_PRESENCE")))

class(rf_model_hypertuned)

a3 = dplyr::select(a2, -CDIFF_PRESENCE)

rf_model_hypertuned = randomForest(
  data = a2, 
  CDIFF_PRESENCE ~ ., 
  mtry = 345,
  ntree = 500,
  importance = TRUE,
  nodesize = 3
)

rf_pred_prob <- predict(rf_model, type = "prob")[, 2]

rf_labels <- a2$CDIFF_PRESENCE


perf_rocr <- performance(pred_rocr, measure = "tpr", x.measure = "fpr")

plot(perf_rocr, col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
title("ROC Curve for Random Forest")

auc <- performance(pred_rocr, measure = "auc")
auc_value <- auc@y.values[[1]]
auc_value



predictor <- Predictor$new(
  model = rf_model_hypertuned,
  data  = a3,
  y     = a2$CDIFF_PRESENCE
)


hi = data.frame(a3[1,])

for (x in 1:30){
  print(x)
  shapley <- Shapley$new(predictor, x.interest = a3[x, ])
  results_shap = shapley$results
  phivar = results_shap$phi.var
  hi = rbind(hi, phivar)
}

shapley$plot()

shapley$results

dim(hi)

best_importance_rf$species

short_res_shap = hi[,colnames(hi) %in% head(best_importance_rf, 10)$species]

dim(short_res_shap)

short_res_shap$sample = 1:nrow(short_res_shap)

short_res_shap_long = short_res_shap |>
  pivot_longer(!sample, names_to = "species", values_to = "feature_importance")


g = filter(short_res_shap_long, species != "X.Clostridium..clostridioforme")

ggplot(g, aes(x = feature_importance, y = species, color = feature_importance)) +
  geom_beeswarm(alpha = 0.3) +
  geom_quasirandom(method = "pseudorandom") + 
  scale_color_gradient(low = "purple", high = "orange")

typeof(short_res_shap$feature.value)
