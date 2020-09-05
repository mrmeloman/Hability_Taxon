#load RTaxometrics library
library("RTaxometrics")
library("dplyr")

#Load the data
data <- read.csv("oxford_hgshsa_260820_modded.csv", sep = ";")

#Check for N/A
table(is.na(data))

#Get rid of N/A
data <- na.omit(data)
#We've lost 602 - 584 = 18 participants

#Create subset with only involuntariness ratings:
invol <- select(data, id, INV.1:INV.12)

#Check correlation within HGSHS:A items and involuntariness items
#Should be less than .3, ideally
select(invol, INV.1:INV.12) %>%
  cor(method = "pearson") %>%
  round(digits = 2)

#Calculate mean correlation

