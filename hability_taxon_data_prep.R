#load libraries
library("RTaxometrics")
library("dplyr")
library("corrplot")

#====Dataset preparation====
#Load the data
data <- read.csv("oxford_hgshsa_260820_modded.csv", sep = ";")

#Check for N/A
table(is.na(data))

#Get rid of N/A
data <- na.omit(data)
#We've lost 602 - 584 = 18 participants

#====Involuntariness data: separate items====
#Create a subset with involuntariness ratings:
invol <- select(data, id, INV.1:INV.12)

#Check correlation within involuntariness items
#Should be less than .3, ideally
select(invol, INV.1:INV.12) %>%
  cor(method = "pearson") %>%
  round(digits = 2)

#Visualize the correlation matrix
select(invol, INV.1:INV.12) %>%
  cor() %>%
  corrplot.mixed(tl.col="black")

#Check the data
select(invol, INV.1:INV.12) %>%
  CheckData()

#RESULTS:
#Taxon base rate:  P = 0.08390411 
#* This is smaller than the recommended minimum of P = 0.10.
#Taxon size:  n = 49 
#* It may be difficult to differentiate categories when one is this small.
#Complement size:  n = 31 
#* It may be difficult to differentiate categories when one is this small.
#Number of variables:  k = 11
#Mean Cohen's d = 0.11
#* One or more values below the recommended minimum of d = 1.25
#Within-group correlations (taxon) mean = 0.17
#* One or more values above the recommended maximum of r = 0.30
#Within-group correlations (complement) mean = 0.26
#* One or more values above the recommended maximum of r = 0.30
#/RESULTS

#====Involuntariness data: separate items ("bad" items removed)====
#Create a subset with involuntariness ratings without "bad" items:
invol_purified <- select(data, id, INV.3:INV.10)

#Check correlation within involuntariness items
#Should be less than 0.3, ideally
select(invol_purified, INV.3:INV.10) %>%
  cor(method = "pearson") %>%
  round(digits = 2)

#Visualize the correlation matrix
select(invol_purified, INV.3:INV.10) %>%
  cor() %>%
  corrplot.mixed(tl.col="black")

#Check the data
select(invol_purified, INV.3:INV.10) %>%
  CheckData()

#RESULTS:
# Taxon base rate:  P = 0.1113014 
# Taxon size:  n = 65 
# Complement size:  n = 34 
# * It may be difficult to differentiate categories when one is this small.
# Number of variables:  k = 7 
# Cohen's d mean = 0.6
# * One or more values below the recommended minimum of d = 1.25
# Within-group correlations (taxon) mean = 0.14
# * One or more values above the recommended maximum of r = 0.30
# Within-group correlations (complement) mean = 0.25
# * One or more values above the recommended maximum of r = 0.30
#/RESULTS

#====Involuntariness data: categories====
#Create items groups
invol$ideomotor <- invol$INV.1 + invol$INV.2 + invol$INV.3 + invol$INV.7
invol$challenge <- invol$INV.4 + invol$INV.5 + invol$INV.6 + invol$INV.8 + invol$INV.10
invol$cogper <- invol$INV.9 + invol$INV.11 + invol$INV.12

#Check the correlation between item groups
select(invol, ideomotor, challenge, cogper) %>%
  cor(method = "pearson") %>%
  round(digits = 2)
#We don't need the correlation plot here

#Check the data
select(invol, ideomotor, challenge, cogper) %>%
  CheckData()
#RESULTS
# Taxon base rate:  P = 0.08047945 
# * This is smaller than the recommended minimum of P = 0.10
# Taxon size:  n = 47 
# * It may be difficult to differentiate categories when one is this small.
# Complement size:  n = 51 
# Number of variables:  k = 2 
# * Only enough variables to perform MAMBAC and MAXSLOPE.
# Cohen's d mean = 0.12
# * One or more values below the recommended minimum of d = 1.25
# Within-group correlations (taxon) mean = 0.54
# * One or more values above the recommended maximum of r = 0.30
# Within-group correlations (complement) mean = 0.46
# * One or more values above the recommended maximum of r = 0.30
#/RESULTS

#====Involuntariness data: categories ("bad" items removed)====
#Create items groups
invol_purified$ideomotor <- invol_purified$INV.3 + invol_purified$INV.7
invol_purified$challenge <- invol_purified$INV.4 + invol_purified$INV.5 + invol_purified$INV.6 + invol_purified$INV.8 + invol_purified$INV.10
invol_purified$cogper <- invol_purified$INV.9

#Check the correlation between item groups
select(invol_purified, ideomotor, challenge, cogper) %>%
  cor(method = "pearson") %>%
  round(digits = 2)
#We don't need the correlation plot here

#Check the data
select(invol_purified, ideomotor, challenge, cogper) %>%
  CheckData()
#RESULTS
# Taxon base rate:  P = 0.09417808 
# * This is smaller than the recommended minimum of P = 0.10.
# Taxon size:  n = 55 
# Complement size:  n = 36 
# * It may be difficult to differentiate categories when one is this small.
# Number of variables:  k = 2 
# * Only enough variables to perform MAMBAC and MAXSLOPE.
# Cohen's d mean = 0.59
# * One or more values below the recommended minimum of d = 1.25
# Within-group correlations (taxon) mean = 0.4
# * One or more values above the recommended maximum of r = 0.30
# Within-group correlations (complement) mean = 0.5
# * One or more values above the recommended maximum of r = 0.30
#/RESULTS

#====Hypnotic suggestibility data: separate items====
#Create a H. suggestibility subset
hab <- select(data, id, HGSHS.A1:HGSHS.A11, HGSHS.A12)

#Check correlation within HGSHS:A items
select(hab, HGSHS.A1:HGSHS.A11, HGSHS.A12) %>%
  cor(method = "pearson") %>%
  round(digits = 2)
  
#Visualize the correlation matrix
select(hab, HGSHS.A1:HGSHS.A11, HGSHS.A12) %>%
  cor() %>%
  corrplot.mixed(tl.col = "black")

#Check the data
select(hab, HGSHS.A1:HGSHS.A11, HGSHS.A12) %>%
  CheckData()
#ERROR 1
#Error in if (min(validities) < 1.25): missing value, while TRUE/FALSE expected
#Some inner function seems to fail finding min and max values somewhere





#====Hypnotic suggestibility data: separate items ("bad" items removed)====
#Create a H. suggestibility subset without "bad" items
hab_purified <- select(data, id, HGSHS.A3:HGSHS.A10)

#Check correlation within HGSHS:A items
select(hab_purified, HGSHS.A3:HGSHS.A10) %>%
  cor(method = "pearson") %>%
  round(digits = 2)

#Visualize the correlation matrix
select(hab_purified, HGSHS.A3:HGSHS.A10) %>%
  cor() %>%
  corrplot.mixed(tl.col = "black")

#Check the data
select(hab_purified, HGSHS.A3:HGSHS.A10) %>%
  CheckData()
#ERROR 2 (same as ERROR 1)
#Error in if (min(validities) < 1.25): missing value, while TRUE/FALSE expected
#Some inner function seems to fail finding min and max values somewhere


#====Hypnotic suggestibility data: categories====

#Create items groups
hab$ideomotor <- hab$HGSHS.A1 + hab$HGSHS.A2 + hab$HGSHS.A3 + hab$HGSHS.A7
hab$challenge <- hab$HGSHS.A4 + hab$HGSHS.A5 + hab$HGSHS.A6 + hab$HGSHS.A8 + hab$HGSHS.A10
hab$cogper <- hab$HGSHS.A9 + hab$HGSHS.A11 + hab$HGSHS.A12

#Check the correlation between item groups
select(hab, ideomotor, challenge, cogper) %>%
  cor(method = "pearson") %>%
  round(digits = 2)
#We don't need the correlation plot here

#Check the data
select(hab, ideomotor, challenge, cogper) %>%
  CheckData()

#RESULTS
#Taxon base rate:  P = 0.07876712 
#* This is smaller than the recommended minimum of P = 0.10.
#Taxon size:  n = 46 
#* It may be difficult to differentiate categories when one is this small.
#Complement size:  n = 177 
#Number of variables:  k = 2 
#* Only enough variables to perform MAMBAC and MAXSLOPE.
#Mean cohen's d = 0.38
#* One or more values below the recommended minimum of d = 1.25.
#Within-group correlations (taxon) mean = 0.38
#* One or more values above the recommended maximum of r = 0.30.
#Within-group correlations (complement) mean = 0.2
#/RESULTS
#====Hypnotic suggestibility data: categories ("bad" items removed)====

#Create items groups
hab_purified$ideomotor <- hab_purified$HGSHS.A3 + hab_purified$HGSHS.A7
hab_purified$challenge <- hab_purified$HGSHS.A4 + hab_purified$HGSHS.A5 + hab_purified$HGSHS.A6 + hab_purified$HGSHS.A8 + hab_purified$HGSHS.A10
hab_purified$cogper <- hab_purified$HGSHS.A9

#Check the correlation between item groups
select(hab_purified, ideomotor, challenge, cogper) %>%
  cor(method = "pearson") %>%
  round(digits = 2)
#We don't need the correlation plot here

#Check the data
select(hab_purified, ideomotor, challenge, cogper) %>%
  CheckData()
#ERROR 3 (same as ERROR 1)
#Error in if (min(validities) < 1.25): missing value, while TRUE/FALSE expected
#Some inner function seems to fail finding min and max values somewhere
