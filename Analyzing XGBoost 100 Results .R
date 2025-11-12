x4 = read_csv("100-runs-XGB.csv")

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
ggplot(filter(x4, species == "Klebsiella.oxytoca"), aes(`abs(shp_res_neg/shp_feat_pos)`)) + 
  geom_histogram()

ggplot(filter(x4, species == "Streptococcus.infantis"), aes(`abs(shp_res_neg/shp_feat_pos)`)) + 
  geom_histogram()

arrange(x5, desc(n))

ggplot(x5, aes(tot)) + 
  geom_histogram()

filter(x5, species == "Klebsiella.oxytoca")

final = filter(x5, tot < 0.3 & n > 300)
as.data.frame(filter(x5, cor < -0.4 & n > 300))

as.data.frame(final)
cor.test(microbiomedata_t$Bilophila.wadsworthia, microbiomedata_t$Clostridioides.difficile, method = "spearman")

final
filter(cor_df, p_value < 0.05 & correlation < 0)
#So a lot of these highly correlated species are actually super duper sparse and thats why they are so much less present
sum(microbiomedata_t$Lactobacillus.plantarum > 0)
sum(microbiomedata_t$Klebsiella.oxytoca > 0)

cor.test(microbiomedata_t$Enterobacter.cloacae, microbiomedata_t$Clostridioides.difficile, method = "spearman")
