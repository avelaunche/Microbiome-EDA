library(reshape2)
library(shapviz)
library(factoextra)

shp <- shapviz(
  xgb_model,
  X_pred = data.matrix(X_train),
  X = X_train
)

shp_res = shp$S

sv_importance(shp, kind = "beeswarm")

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
}

ggplot(data = NULL, aes(x = shp_res[,"Klebsiella.pneumoniae"], y = shp_feat[,"Klebsiella.pneumoniae"])) + 
  geom_point()

sum_rows_pos = function(x){
  sum(x > 0)
}

sum_rows_neg = function(x){
  sum(x < 0)
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

neg_significant = filter(significant, cor < 0)

significant

arrange(significant, p)

shp_res_sum = data.frame(shp_res_neg = apply(shp_res, 2, sum_rows_neg))
shp_feat_sum = data.frame(shp_feat_pos = apply(shp_feat, 2, sum_rows_pos))
x = left_join(rownames_to_column(shp_res_sum, var = "rowname"),
              rownames_to_column(shp_feat_sum, var = "rowname"),
              by = "rowname")

colnames(x) = c("species","shp_res_neg", "shp_feat_pos")

x2 = filter(x, shp_res_neg > 0 & shp_feat_pos > 0)
arrange(x2, desc(shp_res_neg))

x3 = inner_join(x, neg_significant) |>
  mutate(abs(shp_res_neg/ shp_feat_pos))

arrange(x3, cor)



#I first tried this with means and with other methods but it just doesnt work because im left dealing with the negatives and stuff.
#IE even if there is a correlation i cant find a mean because its gonna average out to 0 if its a good fit which obviously doesnt work. 
#i also tried sd also didnt wokr for similar reasons ad its just a weird weird way of looking at it rather than spread or something 
#so in the end correlation was the method i used this took way too long. 
