library("RTaxometrics")
library("dplyr")
library("writexl")

#Read the data
data <- read.csv("oxford_hgshsa_260820_modded.csv", sep = ";")


#Prepare a table for exporting for OSF
data.export <- data

#Generate the xlsx file for OSF
data.export$has.na <- 0
data.export$has.na[rowSums(is.na(data.export)) > 0] <- 1
write_xlsx(data.export,"hab_taxon_2020_final_exported.xlsx")

#Get the filtered data
data <- data.export[data.export$has.na == 0,]

#Subset the dataset for involuntariness scale
invol <- select(data, INV.1:INV.12)

#Classify data with base rates .15 and .10
invol15 <- ClassifyCases(invol, p = 0.15)

invol10 <- ClassifyCases(invol, p = 0.10)

#Remove problematic items
invol_purified <- select(data, INV.3:INV.10)

#Classify data with base rates .15 and .10
invol_pur15 <- ClassifyCases(invol_purified, p = 0.15)

invol_pur10 <- ClassifyCases(invol_purified, p = 0.10)

## Involuntariness data: subscales

#Create subscales:
invol$ideomotor <- invol$INV.1 + invol$INV.2 + invol$INV.3 + invol$INV.7
invol$challenge <- invol$INV.4 + invol$INV.5 + invol$INV.6 + invol$INV.8 + invol$INV.10
invol$cogper <- invol$INV.9 + invol$INV.11
invol$amnesia <- invol$INV.12

#Subset subscales columns and classify
invol_subscales15 <- select(invol, ideomotor, challenge, cogper, amnesia)
invol_subscales15 <- ClassifyCases(invol_subscales15, p = 0.15)


invol_subscales10 <- select(invol, ideomotor, challenge, cogper, amnesia)
invol_subscales10 <- ClassifyCases(invol_subscales10, p = 0.10)

## Involuntariness data: subscales ("bad" items removed)

#Create subscales

invol_purified$ideomotor <- invol_purified$INV.3 + invol_purified$INV.7
invol_purified$challenge <- invol_purified$INV.4 + invol_purified$INV.5 + invol_purified$INV.6 + invol_purified$INV.8 + invol_purified$INV.10
invol_purified$cogper <- invol_purified$INV.9

#Subset subscales columns and classify
invol_subscales_pur15 <- select(invol_purified, ideomotor, challenge, cogper)
invol_subscales_pur15 <- ClassifyCases(invol_subscales_pur15, p = 0.15)


invol_subscales_pur10 <- select(invol_purified, ideomotor, challenge, cogper)
invol_subscales_pur10 <- ClassifyCases(invol_subscales_pur10, p = 0.10)



## Hypnotic suggestibility data: separate items

#Create a H. suggestibility subset
hab <- select(data, HGSHS.A1:HGSHS.A11, HGSHS.A12)

#Classify data with base rates .15 and .10
hab15 <- ClassifyCases(hab, p = 0.15)

hab10 <- ClassifyCases(hab, p = 0.10)


## Hypnotic suggestibility data: separate items ("bad" items removed)

#Create a H. suggestibility subset without "bad" items
hab_purified <- select(data,  HGSHS.A3:HGSHS.A10)

#Classify data with base rates .15 and .10
hab_pur15 <- ClassifyCases(hab_purified, p = 0.15)

hab_pur10 <- ClassifyCases(hab_purified, p = 0.10)



## Hypnotic suggestibility data: subscales

#Create subscales
hab$ideomotor <- hab$HGSHS.A1 + hab$HGSHS.A2 + hab$HGSHS.A3 + hab$HGSHS.A7
hab$challenge <- hab$HGSHS.A4 + hab$HGSHS.A5 + hab$HGSHS.A6 + hab$HGSHS.A8 + hab$HGSHS.A10
hab$cogper <- hab$HGSHS.A9 + hab$HGSHS.A11
hab$amnesia <- hab$HGSHS.A12

#Subset subscales columns and classify
hab_subsc15 <- select(hab, ideomotor, challenge, cogper, amnesia)
hab_subsc15 <- ClassifyCases(hab_subsc15, p = 0.15)


hab_subsc10 <- select(hab, ideomotor, challenge, cogper, amnesia)
hab_subsc10 <- ClassifyCases(hab_subsc10, p = 0.10)


## Hypnotic suggestibility data: subscales ("bad" items removed)

#Create subscales
hab_purified$ideomotor <- hab_purified$HGSHS.A3 + hab_purified$HGSHS.A7
hab_purified$challenge <- hab_purified$HGSHS.A4 + hab_purified$HGSHS.A5 + hab_purified$HGSHS.A6 + hab_purified$HGSHS.A8 + hab_purified$HGSHS.A10
hab_purified$cogper <- hab_purified$HGSHS.A9

#Subset subscales columns and classify
hab_subsc_pur15 <- select(hab, ideomotor, challenge, cogper)
hab_subsc_pur15 <- ClassifyCases(hab_subsc_pur15, p = 0.15)


hab_subsc_pur10 <- select(hab, ideomotor, challenge, cogper)
hab_subsc_pur10 <- ClassifyCases(hab_subsc_pur10, p = 0.10)


# Perform the analysis

cat("invol15")
RunTaxometrics(invol15)

cat("invol10")
RunTaxometrics(invol10)

cat("invol_pur15")
RunTaxometrics(invol_pur15)

cat("invol_pur10")
RunTaxometrics(invol_pur10)

cat("invol_subscales15")
RunTaxometrics(invol_subscales15)

cat("invol_subscales10")
RunTaxometrics(invol_subscales10)

cat("invol_subscales_pur15")
RunTaxometrics(invol_subscales_pur15)

cat("invol_subscales_pur10")
RunTaxometrics(invol_subscales_pur10)

cat("hab15")
RunTaxometrics(hab15)

cat("hab10")
RunTaxometrics(hab10)

cat("hab_pur15")
RunTaxometrics(hab_pur15)

cat("hab_pur10")
RunTaxometrics(hab_pur10)

cat("hab_subsc15")
RunTaxometrics(hab_subsc15)

cat("hab_subsc10")
RunTaxometrics(hab_subsc10)

cat("hab_subsc_pur15")
RunTaxometrics(hab_subsc_pur15)

cat("hab_subsc_pur10")
RunTaxometrics(hab_subsc_pur10)

#CCFI Profile for selected models:

RunCCFIProfile(select(invol, ideomotor, challenge, cogper, amnesia))

RunCCFIProfile(select(hab, ideomotor, challenge, cogper, amnesia))


