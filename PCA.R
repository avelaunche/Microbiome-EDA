negative_species = c(
  "Enterobacteriaceae.bacterium.ENNIH2",
  "Coprobacillus.sp..MP77E8", 
  "Enterobacter.cloacae", 
  "Campylobacter.curvus", 
  "Streptococcus.infantis",
  "Blautia.wexlerae",
  "Lactobacillus.paracasei",
  "Lactobacillus.delbrueckii",
  "Streptomyces.flaveus",
  "Klebsiella.oxytoca",
  "X.Clostridium..aldenense",
  "Actinomyces.sp..HPA0247",
  "Erysipelatoclostridium.sp..MP572A05",
  "Tyzzerella.sp..MP10F1re",
  "Enterococcus.gallinarum",
  "Erysipelotrichaceae.bacterium.6_1_45",
  "Bilophila.wadsworthia",
  "Streptococcus.thermophilus",
  "Streptomyces.californicus"
)

correlated_species = x3$species

negative_species_df = dplyr::select(shp_feat, all_of(correlated_species))

cor_matrix <- round(cor(negative_species_df),2)
melted_cormat <- melt(cor_matrix)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)

ggplot(melted_cormat, aes(value)) + 
  geom_histogram()

pca = prcomp(negative_species_df, scale = TRUE)

summary(pca)
pca$rotation

fviz_eig(pca)

fviz_pca_ind(pca,
             geom.ind = "point",
             col.ind = y_train, # a factor
             addEllipses = TRUE,
             legend.title = "Treatment",
             palette = "Dark2")

