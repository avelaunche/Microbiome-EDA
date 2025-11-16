library(compositions)
library(xgboost)
library(caret)
library(shapviz)

sam_subset$sample = rownames(sam_subset)
microbiomedata_t$sample = rownames(microbiomedata_t)

df_f = right_join(sam_subset, microbiomedata_t)

microbiomedata_t$sample = NULL

colnames(df_f)[1:30]
nrow(df_f)
nrow(filter(df_f, Visit.Name != "Day 1" & Visit.Name != "Day 168"))
df_f = filter(df_f, Visit.Name != "Day 1" & Visit.Name != "Day 168")
df_f = df_f[, -c(1:9)]
colnames(df_f)[1:30]

ncol(df_f)
nrow(df_f)

microbiomedata_t_count = df_f > 0
row_sums <- apply(microbiomedata_t_count, 2, sum)
over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0+1/100]
over_10 = data.frame(over_10)
over_10 = rownames(over_10)
a_select = dplyr::select(df_f, all_of(over_10))

ncol(a_select)

a2 <- a_select + 1e-6
a2  <- t(apply(a2, 1, clr))
a2 = as.data.frame(a2)

a2
a2$CDIFF_PRESENCE = a2$Clostridioides.difficile > 0
a2 = dplyr::select(a2, -Clostridioides.difficile)

x4 = mutate(x3, d = shp_res_neg + 1)
x4$d = NULL

nrow(x4)

for (x in 1:500){
  print(x)
  
  set.seed(x)
  
  a2 = as.data.frame(a2)
  
  X_train <- a2
  y_train <- a2$CDIFF_PRESENCE

  
  X_train = dplyr::select(X_train, -CDIFF_PRESENCE)

  class(X_train)
  
  X_train <- as.matrix(X_train)

  xgb_train <- xgb.DMatrix(data = X_train, label = y_train)

  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.1,          # learning rate
    max_depth = 6,      # tree depth
    subsample = 0.8,    # row sampling
    colsample_bytree = 0.8 # feature sampling
  )
  
  #Basic xgb_model
  
  xgb_model <- xgb.train(
    params = params,
    data = xgb_train,
    nrounds = 100,
    watchlist = list(train = xgb_train),
    verbose = 0
  )
  
  shp <- shapviz(
    xgb_model,
    X_pred = data.matrix(X_train),
    X = X_train
  )
  
  cor = c()
  p = c()
  
  shp_res = shp$S
  shp_feat = shp$X
  
  
  for (x in colnames(shp_res)){ 
    r = cor.test(shp_res[,x], shp_feat[,x])
    cor = c(r$estimate, cor)
    p = c(r$p.value, p)
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
  
  x4 = rbind(x4, x3)
  
}

nrow(x4)

write.csv(x4, "100-runs-XGB.csv")
