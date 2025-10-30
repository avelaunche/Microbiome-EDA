library(dplyr)

nat_med_cdiff = read.csv("nat-medicine-cdiff-01-species.csv")
nat_med_cdiff2 = read.csv("microbiome data c diff.csv")
colnames(nat_med_cdiff2)

colnames(nat_med_cdiff)

nat_med_samples = nat_med_cdiff |>
  distinct(samples)
nat_med_cdiff |>
  filter(genus == "Clostridioides")

nat_med_cdiff |>
  filter(genus == "Dorea")

unique(nat_med_cdiff$genus)

sum(nat_med_cdiff2[4,2:ncol(nat_med_cdiff2)])

head(nat_med_cdiff, 1)[,10:20]
head(nat_med_cdiff, 1)[,10:20]


colnames(nat_med_cdiff)
