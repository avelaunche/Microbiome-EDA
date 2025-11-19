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

x4 = mutate(x3, d = shp_res_pos + 1)
x4$d = NULL

nrow(x4)

final_res = c()
final_feat = c()

for (x in 1:50){
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
  
  target_list = final$species
#  target_list = c("X.Clostridium..citroniae", "Eggerthella.lenta", "Eggerthella.sp..MP163B1", "Bacteroides.fragilis", "Blautia.coccoides", "Veillonella.atypica", "Lactobacillus.paragasseri", 
#                  "Roseburia.inulinivorans", "Ruthenibacterium.lactatiformans", "Lachnoclostridium.sp..VE303.01", 
#                  "X.Ruminococcus..sp..MP14G5", "Robinsoniella.peoriensis", "Roseburia.inulinivorans.CAG.15", "Streptomyces.californicus")

  
  shp_res = shp$S
  shp_feat = shp$X
  
  sel_res = dplyr::select(as.data.frame(shp_res), target_list)
  sel_feat = dplyr::select(as.data.frame(shp_feat), target_list)
  
  sel_res$sample = 1:nrow(sel_res)
  sel_feat$sample = 1:nrow(sel_feat)
  
  sel_res$group = x
  sel_feat$group = x
  
  final_res = rbind(final_res, sel_res)
  final_feat = rbind(final_feat, sel_feat)
}

nrow(final_res)
nrow(final_feat)

colnames(final_feat)

long_res = pivot_longer(final_res, cols = !c(sample, group))
long_feat = pivot_longer(final_feat, cols = !c(sample, group))

unique(long_res$group)

long_res_summary = long_res |>
  group_by(sample, name) |>
  summarise(median(value))

long_feat_summary = long_feat |>
  group_by(sample, name) |>
  summarise(median(value))

write.csv(long_feat_summary, "summary-50-xgboost-feat-pos.csv")
write.csv(long_res_summary, "summary-50-xgboost-res-pos.csv")


