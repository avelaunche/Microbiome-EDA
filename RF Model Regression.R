library(tidyverse)
library(janitor)
library(tibble)
library(randomForest)
library(compositions)
library(pROC)
library(ranger)

#Regression with reduced number of features

var_importance = randomForest::importance(rf_model)
var_importance
var_impact = varImpPlot(rf_model)

z = c()
y = c()

for (x in 1:ncol(microbiomedata_t)){
  z = c(z, print(max(microbiomedata_t[,x])))
  y = c(y, (colnames(microbiomedata_t)[x]))
}

sum(is.na(z > 0.05))

length(z)

length(y)

b = y[z > 0.05]



reduced_feature_data = dplyr::select(microbiomedata_t, b)

dim(reduced_feature_data)

rf_model_reduced = randomForest(
  data = reduced_feature_data,
  Clostridioides.difficile ~ .,
  ntree = 2000,
  mtry = 2,
  importance = TRUE,
)

rf_model_reduced

#Regression after normalization

clr_transformed_rf_data = clr(reduced_feature_data)
clr_transformed_data = clr(microbiomedata_t)

rf_model_reduced_transformed = randomForest(
  data = clr_transformed_rf_data,
  Clostridioides.difficile ~ .,
  ntree = 200,
  mtry = 2,
  importance = TRUE,
)



microbiomedata_t_count = microbiomedata_t > 0

microbiomedata_t_count

nrow(microbiomedata_t_count)

row_sums <- apply(microbiomedata_t_count, 2, sum)

over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0.01]

over_10 = data.frame(over_10)
over_10
over_10 = rownames(over_10)

a_select = dplyr::select(microbiomedata_t, over_10)

rf_model_10_feature = randomForest(
  data = a_select,
  Clostridioides.difficile ~ .,
  ntree = 2000,
  importance = TRUE,
)

clr_a = clr(a_select)

rf_model_10feature = randomForest(
  data = clr_a,
  Clostridioides.difficile ~ .,
  ntree = 2000,
  importance = TRUE,
)

rf_model

rf_model_classify_normalize

rf_model_reduced

rf_model_reduced_transformed

rf_model_10_feature

rf_model_10feature

#tuning abundance metric

rf_model_abundance

for (x in 0:30){
  print(0+x/100)
  over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0+x/100]
  
  over_10 = data.frame(over_10)
  
  over_10 = rownames(over_10)
  
  a_select = dplyr::select(microbiomedata_t, over_10)
  rf_model_abundance = randomForest(
    data = a_select, 
    Clostridioides.difficile ~ .,
    ntree = 200,
    importance = TRUE,
    nodesize = 5
  )
  print(rf_model_abundance)
}


over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0+1/100]

over_10 = data.frame(over_10)

over_10 = rownames(over_10)

a_select = dplyr::select(microbiomedata_t, over_10)

a_select = clr(a_select)

rf_model_abundance = randomForest(
  data = a_select, 
  Clostridioides.difficile ~ .,
  ntree = 2000,
  importance = TRUE,
  nodesize = 5
)

rf_model_abundance

rf_model_hypertuned$rsq
rf_model_hypertuned$rsq 

#hyperparameter tuning




n_features = ncol(clr_a)

ames_rf1 <- ranger(
  Clostridioides.difficile ~ ., 
  data = clr_a,
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
    formula         = Clostridioides.difficile ~ ., 
    data            = clr_a, 
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
  data = a_select, 
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

ggplot(best_importance, aes(`%IncMSE`, fct_reorder(as.factor(species), `%IncMSE`), fill = species)) + 
  geom_col() + 
  labs(
    title = "Random Forest Feature Importance",
    subtitle = "C. Difficile with various gut microbiota",
    y = "Species",
    x = "Variable Weight"
  ) + 
  theme_bw() + 
  theme(legend.position = "off")

#paraputrificm is involved with stickland
#eggerthella lenta https://pmc.ncbi.nlm.nih.gov/articles/PMC9607062/
#3 https://pmc.ncbi.nlm.nih.gov/articles/PMC6297934/
#VE800 immunostimulating
#selimonas intestinalis and anaerotruncus are both in VE303



#try GLV, MiRNN, LSTM, neural ODE, 

#predicted vs measured
#look at ROC curve analysis
#SHAP analysis
#LIME

#plot heldout data vs predicted vs measured
#glv data in 
#miRNN
#standard rnn
#LSTM
