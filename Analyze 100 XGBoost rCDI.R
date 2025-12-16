x4 = read_csv("100-runs-XGB-rCDI.csv")

filter(x4, species == "X.Clostridium..aldenense")

x5 <- x4 |>
  group_by(species) |>
  summarise(
    cor = mean(cor, na.rm = TRUE),
    tot = abs(median(shp_res_neg, na.rm = TRUE) - median(shp_feat_pos, na.rm = TRUE)) /
      median(shp_res_neg, na.rm = TRUE), 
    n = n(),
    .groups = "drop"
  )

ggplot(x4, aes(cor)) + 
  geom_histogram()

ggplot(x4, aes((shp_res_neg - shp_feat_pos)/shp_feat_pos)) + 
  geom_histogram()

ggplot(x5, aes(cor)) + 
  geom_histogram()

ggplot(x5, aes(tot)) + 
  geom_histogram()


#it is so weird that is bimodal
spec = "X.Clostridium..aldenense"

ggplot(filter(x4, species == spec), aes(`abs(shp_res_neg/shp_feat_pos)`)) + 
  geom_histogram() + 
  labs(
    title = "Distribution of shap results divided by feature value",
    subtitle =  spec,
    x = "Shap results/feature value"
  )

ggplot(filter(x4, species == "Enterobacter.cloacae"), aes(`abs(shp_res_neg/shp_feat_pos)`)) + 
  geom_histogram() + 
  labs(
    title = "Distribution of shap results divided by feature value",
    subtitle = "Klebsiella Oxytoca",
    x = "Shap results/feature value"
  )

ggplot(filter(x4, species == "Streptococcus.infantis"), aes(`abs(shp_res_neg/shp_feat_pos)`)) + 
  geom_histogram()

arrange(x5, desc(n))

ggplot(x5, aes(tot)) + 
  geom_histogram()

filter(x4, species == "Lactobacillus.paragasseri")
filter(x5, species == "Lactobacillus.paragasseri")

final = filter(x5, tot < 0.3 & n > 40)

as.data.frame(filter(x5, cor < -0.4 & n > 40))

final
other


as.data.frame(final)
cor.test(microbiomedata_t$X.Clostridium..aldenense, microbiomedata_t$Clostridioides.difficile, method = "spearman")

final
other
filter(cor_df, p_value < 0.05 & correlation < 0) 
xfilter(cor_df, p_value < 0.05 & correlation > 0.27)

#So a lot of these highly correlated species are actually super duper sparse and thats why they are so much less present
sum(microbiomedata_t$Lactobacillus.plantarum > 0)
sum(microbiomedata_t$Klebsiella.oxytoca > 0)

cor.test(microbiomedata_t$Enterobacter.cloacae, microbiomedata_t$Clostridioides.difficile, method = "spearman")
