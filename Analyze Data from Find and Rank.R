spec = "Eggerthella.lenta"
filter(x, species == spec)

ggplot(data = NULL, aes(x = shp_res[, spec], y = shp_feat[, spec])) + 
  geom_point(aes(color = as.factor(shp_res[, spec] > 0 & shp_feat[, spec] > 0)), alpha = 0.6) + 
  labs(
    title = "SHAP result vs. original feature",
    subtitle = spec,
    x = "Shap result (Negative means prediction for false)", 
    y = "Feature value", 
    color = "Legend"
  ) + 
  theme_bw() 

spec = "Klebsiella.oxytoca"
filter(x, species == spec)

ggplot(data = NULL, aes(x = shp_res[, spec], y = shp_feat[, spec])) + 
  geom_point(aes(color = as.factor(shp_res[, spec] < 0 & shp_feat[, spec] > 0)), alpha = 0.6) + 
  labs(
    title = "SHAP result vs. original feature",
    subtitle = spec,
    x = "Shap result (Negative means prediction for false)", 
    y = "Feature value", 
    color = "Legend"
  ) + 
  theme_bw() 

spec = "X.Clostridium..aldenense"
filter(x, species == spec)
ggplot(data = NULL, aes(x = shp_res[, spec], y = shp_feat[, spec])) + 
  geom_point(aes(color = as.factor(shp_res[, spec] < 0 & shp_feat[, spec] > 0)), alpha = 0.6) + 
  labs(
    title = "SHAP result vs. original feature",
    subtitle = spec,
    x = "Shap result (Negative means prediction for false)", 
    y = "Feature value", 
    color = "Legend"
  ) + 
  theme_bw() 
#  geom_smooth(method = "lm", se = FALSE)

cor.test(microbiomedata_t$Klebsiella.oxytoca, microbiomedata_t$Clostridioides.difficile, method = "spearman")
cor.test(microbiomedata_t$X.Clostridium..aldenense, microbiomedata_t$Clostridioides.difficile, method = "spearman")
cor.test(microbiomedata_t$X.Clostridium..aldenense, microbiomedata_t$Klebsiella.oxytoca, method = "spearman")

spec = "Actinomyces.naeslundii"

g = cbind(microbiomedata_t, CDIFF_PRESENCE)
g = mutate(g, hi = ifelse(Actinomyces.naeslundii > 0, TRUE, FALSE))
ggplot(g, aes(CDIFF_PRESENCE, fill = hi)) + 
  geom_bar(position = "fill")

g = cbind(X_train, as.logical(y_train))
g = as.data.frame(g)
g = mutate(g, hi = ifelse(Actinomyces.naeslundii > 0, TRUE, FALSE))

ggplot(g, aes(y_train, fill = hi)) + 
  geom_bar(position = "fill")

g = cbind(X_test, as.logical(y_test))
g = as.data.frame(g)
g = mutate(g, hi = ifelse(Actinomyces.naeslundii > 0, TRUE, FALSE))
Actinomyces.naeslundii
ggplot(g, aes(y_test, fill = hi)) + 
  geom_bar(position = "fill")
x3


shp_res[, "Dialister.invisus"]
