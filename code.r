#Baca data
data <- read.csv("marketing_campaign.csv", sep = "\t", header=TRUE)

#Membuat atribut age untuk mengetahui usia customer
data$Age <- NULL
data$Age <- 2023-data$Year_Birth

#Membuat atribut spending untuk mengetahui total produk yang sudah dibeli
data$Spending <- NULL
data$Spending <- data$MntWines+data$MntFruits+data$MntMeatProducts+
  data$MntFishProducts+data$MntSweetProducts+data$MntGoldProds

#Membuat atribut Children untuk mengetahui jumlah anak yang dimiliki
data$Children <- NULL
data$Children <- data$Kidhome+data$Teenhome

#Membuat atribut Seniority untuk mengetahui lama waktu seseorang menjadi customer di perusahaan ini
data$Seniority <- NULL
now <- as.Date('1-5-2023',format='%d-%m-%Y')
data$Dt_Customer <- as.Date(data$Dt_Customer, format='%d-%m-%Y')
data$Seniority <- as.integer(now - data$Dt_Customer)

#Memberi kode pada atribut Education
#1=undergraduated; 2=postgraduated
data$Education[which(data$Education=="Basic")] <- 1
data$Education[which(data$Education=="2n Cycle")] <- 1
data$Education[which(data$Education=="Graduation")] <- 2
data$Education[which(data$Education=="Master")] <- 2
data$Education[which(data$Education=="PhD")] <- 2
data$Education <- as.integer(data$Education)

#Memberi kode pada atribut Marital Status
#1=alone; 2=couple
data$Marital_Status[which(data$Marital_Status=="Divorced")] <- 1
data$Marital_Status[which(data$Marital_Status=="Single")] <- 1
data$Marital_Status[which(data$Marital_Status=="Absurd")] <- 1
data$Marital_Status[which(data$Marital_Status=="Widow")] <- 1
data$Marital_Status[which(data$Marital_Status=="YOLO")] <- 1
data$Marital_Status[which(data$Marital_Status=="Married")] <- 2
data$Marital_Status[which(data$Marital_Status=="Together")] <- 2
data$Marital_Status <- as.integer(data$Marital_Status)

#Memilih atribut yang diperlukan
dataCust <- data.frame(data$Age, data$Education, data$Marital_Status, data$Income, data$Spending)

#Cek missing value
library(mice)
md.pattern(dataCust)

#Menghapus record dengan missing value
library("tidyr")
dataCust <- dataCust %>% drop_na()

#Cek missing value
md.pattern(dataCust)

#Hapus outliers
dataCust <- subset(dataCust, dataCust$data.Income < 600000)
