#Baca data
data <- read.csv("marketing_campaign.csv", sep = "\t", header=TRUE)

###PRAPROSES DATA###

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

#Memilih atribut yang diperlukan
dataCust <- data.frame(data$Age, data$Marital_Status, data$Children, data$Education, data$Seniority,
                       data$Income, data$Spending, data$MntWines, data$MntFruits, data$MntMeatProducts,
                       data$MntFishProducts, data$MntSweetProducts, data$MntGoldProds)

#Memilih Marital_Status yang bernilai Single dan Married
dataCust <- subset.data.frame(dataCust, data.Marital_Status=="Single" | data.Marital_Status=="Married")

#Pembersihan Data
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

#Isi nilai missing value
dataCust$data.Income[is.na(dataCust$data.Income)] <- mean(dataCust$data.Income, na.rm= TRUE)
md.pattern(dataCust)


str(dataCust)

#Inisialisasi Data Kategorik
#Atribut Education
#1=undergraduated; 2=postgraduated
dataCust$data.Education[which(dataCust$data.Education=="Basic")] <- 1
dataCust$data.Education[which(dataCust$data.Education=="2n Cycle")] <- 1
dataCust$data.Education[which(dataCust$data.Education=="Graduation")] <- 2
dataCust$data.Education[which(dataCust$data.Education=="Master")] <- 2
dataCust$data.Education[which(dataCust$data.Education=="PhD")] <- 2
dataCust$data.Education <- as.integer(dataCust$data.Education)

#Atribut Marital Status
#1=Single; 2=Married
dataCust$data.Marital_Status[which(dataCust$data.Marital_Status=="Single")] <- 1
dataCust$data.Marital_Status[which(dataCust$data.Marital_Status=="Married")] <- 2
dataCust$data.Marital_Status <- as.integer(dataCust$data.Marital_Status)


summary(dataCust)

#Normalisasi
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dataCust$data.Age <- normalize(dataCust$data.Age)
dataCust$data.Marital_Status <- normalize(dataCust$data.Marital_Status)
dataCust$data.Children <- normalize(dataCust$data.Children)
dataCust$data.Education <- normalize(dataCust$data.Education)
dataCust$data.Seniority <- normalize(dataCust$data.Seniority)
dataCust$data.Income <- normalize(dataCust$data.Income)
dataCust$data.Spending <- normalize(dataCust$data.Spending)
dataCust$data.MntWines <- normalize(dataCust$data.MntWines)
dataCust$data.MntFruits <- normalize(dataCust$data.MntFruits)
dataCust$data.MntMeatProducts <- normalize(dataCust$data.MntMeatProducts)
dataCust$data.MntFishProducts <- normalize(dataCust$data.MntFishProducts)
dataCust$data.MntSweetProducts <- normalize(dataCust$data.MntSweetProducts)
dataCust$data.MntGoldProds <- normalize(dataCust$data.MntGoldProds)


### MENENTUKAN JUMLAH CLUSTER ###
library(cluster)

# Menyiapkan data
data_norm <- data.frame(
  Age = dataCust$data.Age,
  Marital_Status = dataCust$data.Marital_Status,
  Children = dataCust$data.Children,
  Education = dataCust$data.Education,
  Seniority = dataCust$data.Seniority,
  Income = dataCust$data.Income,
  Spending = dataCust$data.Spending,
  MntWines = dataCust$data.MntWines,
  MntFruits = dataCust$data.MntFruits,
  MntMeatProducts = dataCust$data.MntMeatProducts,
  MntFishProducts = dataCust$data.MntFishProducts,
  MntSweetProducts = dataCust$data.MntSweetProducts,
  MntGoldProds = dataCust$data.MntGoldProds
)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(data_norm, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# K-means Clustering
(result<- kmeans(data_norm, 4))

