outcome = dplyr::select(read_csv("data/species_df.csv"), "Patient ID", "Outcome", "Sample Time")
ncol(outcome)
colnames(outcome) = c("Subject.Number", "outcome", "Visit.Name")

dim(sam_table)
str(sam_table)

colnames(sam_table)

head(sam_table)

outcome = group_by(outcome, "Subject.Number") 

outcome

sam_subset = dplyr::select(sam_table, Subject.Number, Visit.Name, Visit.Name.Norm, TRT, Population, Abx.group_joined_M, CMINDC, rec.diagnosis)

nrow(sam_subset)
nrow(outcome)

table(sam_subset$outcome)

ggplot(sam_table, aes(Population)) + 
  geom_bar()

sam_subset

sam_subset |>
  count(Abx.group_joined_M)

nrow(distinct(sam_table, Subject.Number))

nrow(distinct(sam_table, Visit.Name, Subject.Number))

y = unique(sam_table$Visit.Name)[1:5]
head(filter(sam_table, Visit.Name == y))
rownames(sam_subset) = rownames(sam_table)
nrow(sam_subset)
