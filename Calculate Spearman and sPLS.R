library(BiocManager)
library(mixOmics)
library(randomForest)
library(dplyr)
library(forcats)

#initialize dataframe
microbiomedata = as.data.frame(microbiomedata)

#glance at structure
str(microbiomedata)
glimpse(microbiomedata)

df <- as.data.frame(do.call(cbind, microbiomedata))

#transform it into a more tidy form
microbiomedata_t = t(microbiomedata)
microbiomedata_t = as.data.frame(microbiomedata_t)

#look at CDiff distribution
ggplot(microbiomedata_t, aes(Clostridioides.difficile)) +
  geom_histogram()

cdiff = microbiomedata_t$Clostridioides.difficile

#correlation
corrs = c()
pvalue = c()
species = c()

Y = microbiomedata_t |>
  dplyr::select(Clostridioides.difficile)

X = microbiomedata_t |>
  dplyr::select(-Clostridioides.difficile)

#run the for loop. Maybe I should use an apply function instead
for (x in colnames(X)){
  cors = cor.test(as.numeric(X[1:495, x]), y = as.numeric(cdiff[1:495]), method = "spearman")
  species = c(species, x)
  corrs = c(corrs, cors$estimate)
  pvalue = c(pvalue, cors$p.value)
}

#adjust pvalue
pvalue = p.adjust(pvalue)

#aggregate correlation data
cor_df = data.frame("species" = species, "correlation" = corrs, "p_value" = pvalue)

#visualize data
cor_df |>
  arrange(p_value)

#look at relevant data only
cor_relevant = cor_df |>
  filter(pvalue < 0.05) |>
  arrange(p_value)

#store 30 most relevant species
corr_select_species = head(cor_relevant, 30)

#6 and 27 are in VE303

#sPLS
fit <- spls(X ,cdiff, ncomp = 3, keepX = c(30,30,30))

#keeps the results
sPLS_res = selectVar(fit, comp = 1)
sPLS_res.X = sPLS_res$X
vip_scores <- mixOmics::vip(fit)

sPLS_res.X
vip_scores = as.data.frame(vip_scores)
vip_scores = vip_scores |>
  arrange(desc(comp1), desc(comp2), desc(comp3)) |>
  filter(!comp1 == 0 & !comp2 == 0 & !comp3 == 0)

vip_scores$species = rownames(vip_scores)

vip_scores

vip_scores = mutate(vip_scores, tot = comp1 + comp2 + comp3)

spls_select_species = vip_scores

#plots

corr_select_species


ggplot(corr_select_species, aes(correlation, fct_reorder(as.factor(species), correlation), fill = species)) + 
  geom_col() + 
  labs(
    title = "Spearman Correlation",
    subtitle = "C. Difficile with various gut microbiota",
    y = "Species",
    x = "Correlation"
  ) + 
  theme_bw() + 
  theme(legend.position = "off")

ggplot(spls_select_species, aes(tot, fct_reorder(as.factor(species), tot), fill = species)) + 
  geom_col() + 
  labs(
    title = "sPLS Correlation (Sparse Least Partial Squares)",
    subtitle = "C. Difficile with various gut microbiota",
    y = "Species",
    x = "Variable Weight"
  ) + 
  theme_bw() + 
  theme(legend.position = "off")

