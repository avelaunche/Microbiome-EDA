library(ggplot2)
library(ggbeeswarm)
library(dplyr)

res = read_csv("summary-50-xgboost-res-main.csv")
feat = read_csv("summary-50-xgboost-feat-main.csv")

feat$name = fct_reorder(feat$name, desc(res$`median(value)`))

ggplot(data = NULL, aes(x = res$`median(value)`, y = feat$name, color = feat$`median(value)`)) +
  geom_quasirandom(groupOnX = FALSE, alpha = 0.7, size = 0.8) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    x = "SHAP value",
    color = "Feature value",
    title = "Beeswarm-style SHAP summary plot",
    subtitle = "SHAP is median of 50 XGBoost models"
  )

dplyr::filter(feat, name == "Enterobacter.cloacae")

