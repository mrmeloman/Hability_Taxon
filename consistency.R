library("DescTools")

data <- read.csv("oxford_hgshsa_260820_modded.csv", sep = ";")

data <- na.omit(data)

invol <- select(data, INV.1:INV.12)

hab <- select(data, HGSHS.A1:HGSHS.A11, HGSHS.A12)

cron_invol <- CronbachAlpha(invol)

cron_hab <- CronbachAlpha(hab)

cron_invol

cron_hab