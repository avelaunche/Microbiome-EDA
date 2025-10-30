library(caret)
library(randomForest)

set.seed(123)

# 5-fold cross-validation setup
ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final" )

dim(a_select)

a_select = clr(a_select)

# Fit model with CV
rf_cv_regression <- train(
  Clostridioides.difficile ~ ., data = a_select,
  method = "rf",         # "rf" calls randomForest
  trControl = ctrl,
  tuneLength = 3         # try a few mtry values automatically
)

rf_cv_regression

ggplot(rf_cv_regression$pred, aes(pred, obs)) + 
  geom_point(alpha = 0.1) + 
  geom_abline(col = "red", size = 1) +
  labs(
    title = "Predicted vs Observed C. difficile Abundance",
    subtitle = "C. Difficile with various gut microbiota",
    y = "Observed",
    x = "Predicted"
  ) + 
  theme_bw()

postResample(pred = rf_cv_regression$pred$pred, obs = rf_cv_regression$pred$obs)

# Fit model with CV
rf_cv_classification <- train(
  CDIFF_PRESENCE ~ ., data = a2,
  method = "rf",         # "rf" calls randomForest
  trControl = ctrl,
  tuneLength = 3         # try a few mtry values automatically
)

rf_cv_classification

postResample(pred = rf_cv_classification$pred$pred, obs = rf_cv_classification$pred$obs)

