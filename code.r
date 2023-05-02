data <- read.csv("marketing_campaign.csv", sep = "\t", header=TRUE)

#Cek tipe data
str(data)

#Hapus atribut kategorik
data$Education <- NULL
data$Marital_Status <- NULL
data$Dt_Customer <- NULL
str(data)

#Cek missing value
library(mice)
md.pattern(data)

#Mengganti nilai missing value
data$Income[is.na(data$Income)] <- mean(data$Income, na.rm= TRUE)
md.pattern(data)