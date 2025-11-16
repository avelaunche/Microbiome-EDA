x4 = read_csv("100-runs-XGB.csv")

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

filter(x5, species == "Klebsiella.oxytoca")

final = filter(x5, tot < 0.3 & n > 400)

as.data.frame(filter(x5, cor < -0.4 & n > 400))

final
other


for (x in other$species){
  cors = cor.test(as.numeric(microbiomedata_t[1:495, x]), y = as.numeric(cdiff[1:495]), method = "spearman")
  print(x)
  print(cors)
  species = c(species, x)
  corrs = c(corrs, cors$estimate)
  pvalue = c(pvalue, cors$p.value)
}

cor(microbiomedata_t$X.Clostridium..aldenense, microbiomedata_t$Clostridioides.difficile, method = "spearman")

for (x in final$species){
  print(x)
  cor = cor.test(microbiomedata_t[, x], microbiomedata_t$Clostridioides.difficile, method = "spearman")
  print(cors)
  species = c(species, x)
  corrs = c(corrs, cors$estimate)
  pvalue = c(pvalue, cors$p.value)
}

as.data.frame(final)
cor.test(microbiomedata_t$Bilophila.wadsworthia, microbiomedata_t$Clostridioides.difficile, method = "spearman")

final
other
filter(cor_df, p_value < 0.05 & correlation < 0)
#So a lot of these highly correlated species are actually super duper sparse and thats why they are so much less present
sum(microbiomedata_t$Lactobacillus.plantarum > 0)
sum(microbiomedata_t$Klebsiella.oxytoca > 0)

cor.test(microbiomedata_t$Enterobacter.cloacae, microbiomedata_t$Clostridioides.difficile, method = "spearman")
