library("writexl")

demographics <- read.csv("oxford_hgshsa_260820_demographics.csv", sep = ";")

demographics$missing <- data.export$has.na

write_xlsx(demographics,"hab_taxon_demographics_2020_final_exported.xlsx")

data <- read.csv("oxford_hgshsa_260820_modded.csv", sep = ";")

data <- na.omit(data)

demographics_filtered <- demographics[demographics$id %in% data$id,]

#Removing problematic entries
demographics_filtered <- demographics_filtered[demographics_filtered$age > 0,]
demographics_filtered <- demographics_filtered[!is.na(demographics_filtered$age),]

mean(demographics_filtered$age)

sd(demographics_filtered$age)
