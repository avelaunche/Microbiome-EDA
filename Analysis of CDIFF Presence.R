microbiomedata_t$Clostridioides.difficile

sam_subset$sample = rownames(sam_subset)
microbiomedata_t$sample = rownames(microbiomedata_t)

df = right_join(sam_subset, microbiomedata_t)

microbiomedata_t$sample = NULL

df$CDIFF_PRESENCE = as.factor(df$Clostridioides.difficile > 0)
df$population = df$Population
df$visit_name = as.factor(df$Visit.Name)
df$trt = df$TRT

df$visit_name = factor(df$visit_name, levels = c("Screening","Day 1", "Day 7", "Day 14", "Day 28", "Day 56", "Day 168", "UNSCHED"))


ggplot(data.frame(df), aes(CDIFF_PRESENCE, fill = population)) + 
  geom_bar()

ggplot(data.frame(df), aes(visit_name, fill = CDIFF_PRESENCE)) + 
  geom_bar(position = "fill") + 
  labs(
    title = "C. Difficile Presence by Visit",
    y = "Proportion",
    x = "Visit name",
    fill = "C. Difficile Presence"
  ) + 
  theme_bw()

ggplot(data.frame(df), aes(visit_name, fill = Anaerotruncus.colihominis > 0)) + 
  geom_bar(position = "fill") + 
  labs(
    title = "A. Colihominis Presence by Visit",
    y = "Proportion",
    x = "Visit name",
    fill = "A. colihiminis Presence"
  ) + 
  theme_bw()

ggplot(data.frame(df), aes(visit_name, fill = Lachnoclostridium.sp..VE303.01 > 0)) + 
  geom_bar(position = "fill")+ 
  labs(
    title = "VE303.01 Presence by Visit",
    subtitle = "Lachnoclostridium sp.",
    y = "Proportion",
    x = "Visit name",
    fill = "Lachnoclostridium sp. Presence"
  ) + 
  theme_bw()

ggplot(data.frame(df), aes(trt, fill = Anaerotruncus.colihominis > 0)) + 
  geom_bar(position = "fill")

ggplot(data.frame(df), aes(trt, fill = Lachnoclostridium.sp..VE303.01 > 0)) + 
  geom_bar(position = "fill")

ggplot(data.frame(df), aes(trt, fill = Clostridioides.difficile > 0)) + 
  geom_bar(position = "fill")

#HMMM, this VE303.01 is highly correlated with C. diff presence, but overall C. diff presence is significantly higher and it doesn't come up often. Questions to ask. 

ggplot(filter(df), aes(trt, fill = Lachnoclostridium.sp..VE303.01 > 0)) + 
  geom_bar(position = "fill") + 
  facet_wrap(vars(visit_name)) + 
  theme(legend.position = "none") + 
  labs(
    title = "Presence of Lachnoclostridium by Treatment Group Across Visits"
  )

ggplot(filter(df), aes(trt, fill = Clostridioides.difficile > 0)) + 
  geom_bar(position = "fill") + 
  facet_wrap(vars(visit_name)) + 
  theme(legend.position = "none") + 
  labs(
    title = "Presence of C. Difficile by Treatment Group Across Visits"
  )

ggplot(filter(df), aes(trt, fill = Anaerotruncus.colihominis > 0)) + 
  geom_bar(position = "fill") + 
  facet_wrap(vars(visit_name)) + 
  theme(legend.position = "none") + 
  labs(
    title = "Presence of A. Colihominis by Treatment Group Across Visits"
  )

ggplot(filter(df), aes(trt, fill = Eggerthella.lenta > 0)) + 
  geom_bar(position = "fill") + 
  facet_wrap(vars(visit_name)) + 
  theme(legend.position = "none") + 
  labs(
    title = "Presence of E. Lenta by Treatment Group Across Visits"
  )

ggplot(filter(df, visit_name == "Day 56"), aes(x = Clostridioides.difficile)) +
  geom_histogram() +
  facet_wrap(~ trt) + 
  labs(
    title = "Comparing C. Difficile Distributions on Day 56 Across Treament Groups",
    y = "Count",
    x = "C. Difficle"
  )

ggplot(filter(df, visit_name == "Day 168"), aes(x = Clostridioides.difficile)) +
  geom_histogram() +
  facet_wrap(~ trt) + 
  labs(
    title = "Comparing C. Difficile Distributions on Day 168 Across Treament Groups",
    y = "Count",
    x = "C. Difficle"
  )
