sam_table

table(sam_table$Subject.Number, sam_table$rec.diagnosis)

sam_table_sub = dplyr::select(sam_table, c(rec.diagnosis, Day.in.treatment))
sam_table_sub$group = rownames(sam_table_sub)

VE303_consortium = colnames(a2)[grepl("VE303", colnames(a2))]

#anaerotruncus in RF
#sellimonas intestinalis in RF

sum(a2$Anaerotruncus.colihominis == a2$Anaerotruncus.sp..VE303.02)
sum(a2$Anaerotruncus.colihominis == 0)
sum(0 == a2$Anaerotruncus.sp..VE303.02)
sum(a2$Anaerotruncus.colihominis == a2$Eggerthella.lenta)
sum(a2$Anaerotruncus.colihominis == a2$X.Clostridium..citroniae)

#so anaerotruncus colihiminis isn't that much more similar with its VE303 counterpart than with a random species 

final_group = c(
  VE303_consortium, 
  "Escherichia.coli", 
  "Clostridioides.difficile", 
  "Clostridium.clostridioforme.CAG.132", 
  "X.Clostridium..aldenense", 
  "Veillonella.parvula"
)

glv_dataset = select(microbiomedata_t, all_of(final_group))

dim(glv_dataset)

microbiome_t_temp = data.frame(glv_dataset, group = rownames(microbiomedata_t))

glv_table = left_join(sam_table_sub, microbiome_t_temp)

glv_table = dplyr::select(glv_table, -group)

write.csv(glv_table, "glv-table.csv")





