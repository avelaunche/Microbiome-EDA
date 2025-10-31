shp <- shapviz(
  xgb_model,
  X_pred = data.matrix(X_train),
  X = X_train
)

shp_res = shp$S

shp_res = as.data.frame(shp_res)
dim(shp_res)

sum(shp_res$Eggerthella.lenta)
sum(shp_res$Actinomyces.naeslundii)

abs_mean_shap <- apply(abs(shp_res), 2, mean)
mean_shap <- colMeans(shp_res)

shap_summary <- data.frame(
  feature = names(abs_mean_shap),
  mean_abs = abs_mean_shap,
  mean_signed = mean_shap
)

shp_feat = shp$X

cor.test(shp_res$Eggerthella.lenta, shp_feat$Eggerthella.lenta)
cor.test(shp_res$Enterobacter.cloacae, shp_feat$Enterobacter.cloacae)
o = cor.test(shp_res$Enterobacter.cloacae, shp_feat$Enterobacter.cloacae)
o$p.value

cor = c()
p = c()

for (x in colnames(shp_res)){ 
  r = cor.test(shp_res[,x], shp_feat[,x])
  cor = c(r$estimate, cor)
  p = c(r$p.value, p)
  if (x == "Eggerthella.lenta"){
    print(r)
    print(cor)
  }
}


significant_species = rev(colnames(shp_res))[!is.na(cor)]
significant_cor = cor[!is.na(cor)]
significant_p = p.adjust(p[!is.na(cor)])

significant_species = significant_species[significant_p < 0.05]
significant_cor = significant_cor[significant_p < 0.05]
significant_p = significant_p[significant_p < 0.05]

significant = data.frame(
  species = significant_species,
  cor = significant_cor,
  p = significant_p
)

significant = arrange(significant, desc(cor))

filter(significant, species == "Eggerthella.lenta")
