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

#Check the data
select(invol, INV.1:INV.12) %>%
  CheckData()

#====Involuntariness data: separate items ("bad" items removed)====
#Create a subset with involuntariness ratings without "bad" items:
invol_purified <- select(data, id, INV.3:INV.10)

#Check the data
select(invol_purified, INV.3:INV.10) %>%
  CheckData()

#====Involuntariness data: categories====
#Create items groups
invol$ideomotor <- invol$INV.1 + invol$INV.2 + invol$INV.3 + invol$INV.7
invol$challenge <- invol$INV.4 + invol$INV.5 + invol$INV.6 + invol$INV.8 + invol$INV.10
invol$cogper <- invol$INV.9 + invol$INV.11 + invol$INV.12

#Check the data
select(invol, ideomotor, challenge, cogper) %>%
  CheckData()

#====Involuntariness data: categories ("bad" items removed)====
#Create items groups
invol_purified$ideomotor <- invol_purified$INV.3 + invol_purified$INV.7
invol_purified$challenge <- invol_purified$INV.4 + invol_purified$INV.5 + invol_purified$INV.6 + invol_purified$INV.8 + invol_purified$INV.10
invol_purified$cogper <- invol_purified$INV.9

#Check the data
select(invol_purified, ideomotor, challenge, cogper) %>%
  CheckData()

#====Hypnotic suggestibility data: separate items====
#Create a H. suggestibility subset
hab <- select(data, id, HGSHS.A1:HGSHS.A11, HGSHS.A12)

#Check the data
select(hab, HGSHS.A1:HGSHS.A11, HGSHS.A12) %>%
  CheckData()

#====Hypnotic suggestibility data: separate items ("bad" items removed)====
#Create a H. suggestibility subset without "bad" items
hab_purified <- select(data, id, HGSHS.A3:HGSHS.A10)

#Check the data
select(hab_purified, HGSHS.A3:HGSHS.A10) %>%
  CheckData()

#====Hypnotic suggestibility data: categories====

#Create items groups
hab$ideomotor <- hab$HGSHS.A1 + hab$HGSHS.A2 + hab$HGSHS.A3 + hab$HGSHS.A7
hab$challenge <- hab$HGSHS.A4 + hab$HGSHS.A5 + hab$HGSHS.A6 + hab$HGSHS.A8 + hab$HGSHS.A10
hab$cogper <- hab$HGSHS.A9 + hab$HGSHS.A11 + hab$HGSHS.A12

#Check the data
select(hab, ideomotor, challenge, cogper) %>%
  CheckData()

#====Hypnotic suggestibility data: categories ("bad" items removed)====

#Create items groups
hab_purified$ideomotor <- hab_purified$HGSHS.A3 + hab_purified$HGSHS.A7
hab_purified$challenge <- hab_purified$HGSHS.A4 + hab_purified$HGSHS.A5 + hab_purified$HGSHS.A6 + hab_purified$HGSHS.A8 + hab_purified$HGSHS.A10
hab_purified$cogper <- hab_purified$HGSHS.A9

#Check the data
select(hab_purified, ideomotor, challenge, cogper) %>%
  CheckData()