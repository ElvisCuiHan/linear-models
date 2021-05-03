setwd("~/Desktop/UCLA_Study/Bio250B/HW4")
rm(list=ls())

library(haven)
library("PMCMRplus")
library(lawstat)
data <- read_dta("Data for HW4 Q2.dta")

hartleyTest(response~group, data=data)
levene.test(data$response, data$group, location="mean")
levene.test(data$response, data$group, location="median")
bartlett.test(response~group, data=data)

