dim(sam_table)

str(sam_table)

colnames(sam_table)

head(sam_table)

sam_subset = dplyr::select(sam_table, Subject.Number, Visit.Name, Visit.Name.Norm, TRT, Population, Abx.group_joined_M, CMINDC, rec.diagnosis)

ggplot(sam_table, aes(Population)) + 
  geom_bar()

sam_subset

sam_subset |>
  count(Abx.group_joined_M)

nrow(distinct(sam_table, Subject.Number))

nrow(distinct(sam_table, Visit.Name, Subject.Number))

y = unique(sam_table$Visit.Name)[1:5]
head(filter(sam_table, Visit.Name == y))
