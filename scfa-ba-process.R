ba = read_csv("data/BA.csv")
scfa = read_csv("data/SCFA.csv")

ba = dplyr::select(ba, `Sample.ID.metabolite`, `Analyte`, Visit, concentration_corrected, `Subject.Number`, `TRT.norm`)

ba = pivot_wider(ba, names_from = Analyte, values_from = concentration_corrected)
ba

scfa = dplyr::select(scfa, `Sample.ID.metabolite`, `Analyte`, Visit.Name, concentration_corrected, `Subject.Number`)
scfa = pivot_wider(scfa, names_from = Analyte, values_from = concentration_corrected)

metabolites = right_join(
  ba, 
  scfa, 
  by = join_by(
    `Sample.ID.metabolite` == `Sample.ID.metabolite`, 
    Visit == `Visit.Name`
  )
)

ncol(final)


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

microbiomedata_t_count = df_f > 0
row_sums <- apply(microbiomedata_t_count, 2, sum)
over_10 = row_sums[row_sums/nrow(microbiomedata_t_count) > 0+10/100]
over_10 = data.frame(over_10)
over_10 = rownames(over_10)
a_select = dplyr::select(df_f, all_of(over_10))

ncol(a_select)
sample = a_select$sample
a_select = dplyr::select(a_select, -sample)

a2 <- a_select + 1e-6
a2  <- t(apply(a2, 1, clr))
a2 = as.data.frame(a2)
colnames(a2)

a2$sample = sample
colnames(a2)
colnames(final)

final = right_join(a2, metabolites, by = join_by(sample == `Sample.ID.metabolite`))

final = filter(final, Visit != "UNSCHED" & Visit != "Screening")

final = final |>
  mutate(
    Visit = case_when(
      Visit == "Day 28" ~ 28,
      Visit == "Day 7" ~ 7,
      Visit == "Screening" ~ 1,
      Visit == "Day 168" ~ 168,
      Visit == "Day 115" ~ 115,
      Visit == "Day 56" ~ 56,
      Visit == "Day 1" ~ 1,
      Visit == "Day 14" ~ 14
    )
  )

final = dplyr::select(final, -c(sample, `Subject.Number.y`))

k = paste("M", 1:(ncol(metabolites)-5))
k <- gsub("\\s+", "", k)
l = paste("S", 1:(ncol(a2)-1))
l <- gsub("\\s+", "", l)

final = final |>
  relocate("Visit", "TRT.norm", "Subject.Number.x")

colnames(final) = c("Time", "Treatment", "Experiments", k, l)
final$Replicates = 0

colnames(a2)
colnames(metabolites)

final = na.omit(final)
head(final)
write_csv(final, "result-data/processed-metabolite.csv")
