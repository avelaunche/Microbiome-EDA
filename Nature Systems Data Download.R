library(phyloseq)
library(dplyr)
library(tidyr)
library(ggplot2)

a = readRDS("phy_mic.rds")

sample_data(a)
slotNames(a)

#load the otu table in as df
microbiomedata = as.data.frame(a@otu_table)
microbiomedata = data.frame(microbiomedata)

dim(microbiomedata)

#microbiome data is present as keys
colnames(microbiomedata)

#longer_microbiome = pivot_longer(microbiomedata, cols = !speciesname, names_to = "Sample", values_to = "Values")

#load in taxonomic table
tax_table = data.frame(a@tax_table)
dim(tax_table)

head(tax_table)

cdfiff = filter(tax_table, Genus == "Clostridioides")$Species

sam_table = data.frame(a@sam_data)
head(sam_table)


dim(microbiomedata)
dim(tax_table)
dim(sam_table)

head(sam_table)
colnames(sam_table)

summary(sam_table)
str(sam_table)
length(unique(sam_table$Subject.Number))

dim(distinct(sam_table, Subject.Number, Visit.Name))

sam_table |>
  group_by(Subject.Number, Visit.Name) |>
  summarise(n = n())

distinct(sam_table, Visit.Name)
