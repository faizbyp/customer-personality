# Baca data
data <- read.csv("marketing_campaign.csv", sep = "\t", header=TRUE)

### PRAPROSES DATA ###

#Membuat atribut age untuk mengetahui usia customer
data$Age <- NULL
data$Age <- 2023-data$Year_Birth

# Membuat atribut spending untuk mengetahui total produk yang sudah dibeli
data$Spending <- NULL
data$Spending <- data$MntWines+data$MntFruits+data$MntMeatProducts+
  data$MntFishProducts+data$MntSweetProducts+data$MntGoldProds

# Membuat atribut Children untuk mengetahui jumlah anak yang dimiliki
data$Children <- NULL
data$Children <- data$Kidhome+data$Teenhome

# Membuat atribut Seniority untuk mengetahui lama waktu seseorang menjadi customer di perusahaan ini
data$Seniority <- NULL
now <- as.Date('1-5-2023',format='%d-%m-%Y')
data$Dt_Customer <- as.Date(data$Dt_Customer, format='%d-%m-%Y')
data$Seniority <- as.integer(now - data$Dt_Customer)

# Memilih atribut yang diperlukan
dataCust <- data.frame(data$Age, data$Marital_Status, data$Children, data$Education, data$Seniority,
                       data$Income, data$Spending, data$MntWines, data$MntFruits, data$MntMeatProducts,
                       data$MntFishProducts, data$MntSweetProducts, data$MntGoldProds)

# Memilih Marital_Status yang bernilai Single dan Married
dataCust <- subset.data.frame(dataCust, data.Marital_Status=="Single" | data.Marital_Status=="Married")

# Cek missing value
library(mice)
md.pattern(dataCust)

# Hapus outliers
dataCust <- subset(dataCust, dataCust$data.Income < 600000)

# Isi nilai missing value
dataCust$data.Income[is.na(dataCust$data.Income)] <- mean(dataCust$data.Income, na.rm= TRUE)
md.pattern(dataCust)

str(dataCust)

# Inisialisasi Data Kategorik
# Atribut Education
# 1=undergraduated; 2=postgraduated
dataCust$data.Education[which(dataCust$data.Education=="Basic")] <- 1
dataCust$data.Education[which(dataCust$data.Education=="2n Cycle")] <- 1
dataCust$data.Education[which(dataCust$data.Education=="Graduation")] <- 2
dataCust$data.Education[which(dataCust$data.Education=="Master")] <- 2
dataCust$data.Education[which(dataCust$data.Education=="PhD")] <- 2
dataCust$data.Education <- as.integer(dataCust$data.Education)

# Atribut Marital Status
# 1=Single; 2=Married
dataCust$data.Marital_Status[which(dataCust$data.Marital_Status=="Single")] <- 1
dataCust$data.Marital_Status[which(dataCust$data.Marital_Status=="Married")] <- 2
dataCust$data.Marital_Status <- as.integer(dataCust$data.Marital_Status)

summary(dataCust)

# Normalisasi
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

library(factoextra)
library(ggplot2)

### CUSTOMER PERSONALITY UNTUK PEMBELIAN MEAT PRODUCTS ###
### 1. Menggabungkan data produk meat dan age berdasarkan urutan baris ###
MeatAge <- cbind(dataCust$data.MntMeatProducts, dataCust$data.Age)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(MeatAge, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada MeatAge
MeatAge_km <- kmeans(MeatAge, 4, nstart = 25)
print(MeatAge_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatAge
MeatAge_clustered <- data.frame(MeatAge, Cluster = MeatAge_km$cluster)

# Convert Cluster variable ke factor
MeatAge_clustered$Cluster <- factor(MeatAge_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(MeatAge_clustered, aes(x = dataCust$data.MntMeatProducts, y = dataCust$data.Age, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(4)) +
  theme_bw() +
  labs(x = "Pembelian Produk Meat", y = "Age")

### 2. Menggabungkan data produk meat dan marital_Status berdasarkan urutan baris ###
MeatMS <- cbind(dataCust$data.MntMeatProducts, dataCust$data.Marital_Status)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(MeatMS, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada MeatMarital_Status
MeatMS_km <- kmeans(MeatMS, 2, nstart = 25)
print(MeatMS_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatMarital_Status
MeatMS_clustered <- data.frame(MeatMS, Cluster = MeatMS_km$cluster)

# Convert Cluster variable ke factor
MeatMS_clustered$Cluster <- factor(MeatMS_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(MeatMS_clustered, aes(x = dataCust$data.MntMeatProducts, y = dataCust$data.Marital_Status, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(4)) +
  theme_bw() +
  labs(x = "Pembelian Produk Meat", y = "Marital Status")


### 3. Menggabungkan data produk meat dan Children berdasarkan urutan baris ###
MeatCn <- cbind(dataCust$data.MntMeatProducts, dataCust$data.Children)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(MeatCn, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada MeatChildren
MeatCn_km <- kmeans(MeatCn, 4, nstart = 25)
print(MeatCn_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatChildren
MeatCn_clustered <- data.frame(MeatCn, Cluster = MeatCn_km$cluster)

# Convert Cluster variable ke factor
MeatCn_clustered$Cluster <- factor(MeatCn_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(MeatCn_clustered, aes(x = dataCust$data.MntMeatProducts, y = dataCust$data.Children, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(4)) +
  theme_bw() +
  labs(x = "Pembelian Produk Meat", y = "Children")


### 4. Menggabungkan data produk meat dan Education berdasarkan urutan baris ###
MeatEd <- cbind(dataCust$data.MntMeatProducts, dataCust$data.Education)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(MeatEd, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada MeatEducation
MeatEd_km <- kmeans(MeatEd, 3, nstart = 25)
print(MeatEd_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatEducation
MeatEd_clustered <- data.frame(MeatEd, Cluster = MeatEd_km$cluster)

# Convert Cluster variable ke factor
MeatEd_clustered$Cluster <- factor(MeatEd_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(MeatEd_clustered, aes(x = dataCust$data.MntMeatProducts, y = dataCust$data.Education, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(3)) +
  theme_bw() +
  labs(x = "Pembelian Produk Meat", y = "Education")


### 5. Menggabungkan data produk meat dan Seniority berdasarkan urutan baris ###
MeatSn <- cbind(dataCust$data.MntMeatProducts, dataCust$data.Seniority)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(MeatSn, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada MeatSeniority
MeatSn_km <- kmeans(MeatSn, 7, nstart = 25)
print(MeatSn_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatSeniority
MeatSn_clustered <- data.frame(MeatSn, Cluster = MeatSn_km$cluster)

# Convert Cluster variable ke factor
MeatSn_clustered$Cluster <- factor(MeatSn_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(MeatSn_clustered, aes(x = dataCust$data.MntMeatProducts, y = dataCust$data.Seniority, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(7)) +
  theme_bw() +
  labs(x = "Pembelian Produk Meat", y = "Seniority (Member)")


### 6. Menggabungkan data produk meat dan Income berdasarkan urutan baris ###
MeatIn <- cbind(dataCust$data.MntMeatProducts, dataCust$data.Income)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(MeatIn, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada MeatIncome
MeatIn_km <- kmeans(MeatIn, 6, nstart = 25)
print(MeatIn_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatIncome
MeatIn_clustered <- data.frame(MeatIn, Cluster = MeatIn_km$cluster)

# Convert Cluster variable ke factor
MeatIn_clustered$Cluster <- factor(MeatIn_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(MeatIn_clustered, aes(x = dataCust$data.MntMeatProducts, y = dataCust$data.Income, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(6)) +
  theme_bw() +
  labs(x = "Pembelian Produk Meat", y = "Income")

### 7. Menggabungkan data produk meat dan Spending berdasarkan urutan baris ###
MeatSpn <- cbind(dataCust$data.MntMeatProducts, dataCust$data.Spending)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(MeatSpn, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada MeatSpending
MeatSpn_km <- kmeans(MeatSpn, 3, nstart = 25)
print(MeatSpn_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatSpending
MeatSpn_clustered <- data.frame(MeatSpn, Cluster = MeatSpn_km$cluster)

# Convert Cluster variable ke factor
MeatSpn_clustered$Cluster <- factor(MeatSpn_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(MeatSpn_clustered, aes(x = dataCust$data.MntMeatProducts, y = dataCust$data.Spending, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(3)) +
  theme_bw() +
  labs(x = "Pembelian Produk Meat", y = "Spending")


### CUSTOMER PERSONALITY UNTUK PEMBELIAN FISH PRODUCTS ###
### 1. Menggabungkan data produk fish dan age berdasarkan urutan baris ###
FishAge <- cbind(dataCust$data.MntFishProducts, dataCust$data.Age)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(FishAge, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada FishAge
FishAge_km <- kmeans(FishAge, 4, nstart = 25)
print(FishAge_km)

# Menggabungkan hasil klasterisasi dengan dataset FishAge
FishAge_clustered <- data.frame(FishAge, Cluster = FishAge_km$cluster)

# Convert Cluster variable ke factor
FishAge_clustered$Cluster <- factor(FishAge_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(FishAge_clustered, aes(x = dataCust$data.MntFishProducts, y = dataCust$data.Age, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(4)) +
  theme_bw() +
  labs(x = "Pembelian Produk Fish", y = "Age")

### 2. Menggabungkan data produk Fish dan marital_Status berdasarkan urutan baris ###
FishMS <- cbind(dataCust$data.MntFishProducts, dataCust$data.Marital_Status)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(FishMS, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada MeatMarital_Status
FishMS_km <- kmeans(FishMS, 2, nstart = 25)
print(FishMS_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatMarital_Status
FishMS_clustered <- data.frame(FishMS, Cluster = FishMS_km$cluster)

# Convert Cluster variable ke factor
FishMS_clustered$Cluster <- factor(FishMS_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(FishMS_clustered, aes(x = dataCust$data.MntFishProducts, y = dataCust$data.Marital_Status, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(2)) +
  theme_bw() +
  labs(x = "Pembelian Produk Fish", y = "Marital Status")


### 3. Menggabungkan data produk Fish dan Children berdasarkan urutan baris ###
FishCn <- cbind(dataCust$data.MntFishProducts, dataCust$data.Children)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(FishCn, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada MeatChildren
FishCn_km <- kmeans(FishCn, 6, nstart = 25)
print(FishCn_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatChildren
FishCn_clustered <- data.frame(FishCn, Cluster = FishCn_km$cluster)

# Convert Cluster variable ke factor
FishCn_clustered$Cluster <- factor(FishCn_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(FishCn_clustered, aes(x = dataCust$data.MntFishProducts, y = dataCust$data.Children, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(6)) +
  theme_bw() +
  labs(x = "Pembelian Produk Fish", y = "Children")


### 4. Menggabungkan data produk Fish dan Education berdasarkan urutan baris ###
FishEd <- cbind(dataCust$data.MntFishProducts, dataCust$data.Education)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(FishEd, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada FishEducation
FishEd_km <- kmeans(FishEd, 4, nstart = 25)
print(FishEd_km)

# Menggabungkan hasil klasterisasi dengan dataset FishEducation
FishEd_clustered <- data.frame(FishEd, Cluster = FishEd_km$cluster)

# Convert Cluster variable ke factor
FishEd_clustered$Cluster <- factor(FishEd_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(FishEd_clustered, aes(x = dataCust$data.MntFishProducts, y = dataCust$data.Education, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(4)) +
  theme_bw() +
  labs(x = "Pembelian Produk Fish", y = "Education")

### 5. Menggabungkan data produk Fish dan Seniority berdasarkan urutan baris ###
FishSn <- cbind(dataCust$data.MntFishProducts, dataCust$data.Seniority)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(FishSn, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada FishSeniority
FishSn_km <- kmeans(FishSn, 5, nstart = 25)
print(FishSn_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatSeniority
FishSn_clustered <- data.frame(FishSn, Cluster = FishSn_km$cluster)

# Convert Cluster variable ke factor
FishSn_clustered$Cluster <- factor(FishSn_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(FishSn_clustered, aes(x = dataCust$data.MntFishProducts, y = dataCust$data.Seniority, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(5)) +
  theme_bw() +
  labs(x = "Pembelian Produk Fish", y = "Seniority (Member)")


### 6. Menggabungkan data produk Fish dan Income berdasarkan urutan baris ###
FishIn <- cbind(dataCust$data.MntFishProducts, dataCust$data.Income)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(FishIn, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada FishIncome
FishIn_km <- kmeans(MeatIn, 6, nstart = 25)
print(FishIn_km)

# Menggabungkan hasil klasterisasi dengan dataset MeatIncome
FishIn_clustered <- data.frame(FishIn, Cluster = FishIn_km$cluster)

# Convert Cluster variable ke factor
FishIn_clustered$Cluster <- factor(FishIn_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(FishIn_clustered, aes(x = dataCust$data.MntFishProducts, y = dataCust$data.Income, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(6)) +
  theme_bw() +
  labs(x = "Pembelian Produk Fish", y = "Income")

### 7. Menggabungkan data produk Fish dan Spending berdasarkan urutan baris ###
FishSpn <- cbind(dataCust$data.MntFishProducts, dataCust$data.Spending)

# Menghitung WCSS untuk berbagai jumlah kluster
wcss <- vector()
for (i in 1:10) {
  kmeans_fit <- kmeans(FishSpn, centers = i, nstart = 10)
  wcss[i] <- kmeans_fit$tot.withinss
}

# Menampilkan grafik elbow method
plot(1:10, wcss, type = "b", xlab = "Jumlah kluster", ylab = "WCSS")

# Menerapkan algoritme kmeans pada FishSpending
FishSpn_km <- kmeans(FishSpn, 3, nstart = 25)
print(FishSpn_km)

# Menggabungkan hasil klasterisasi dengan dataset FishSpending
FishSpn_clustered <- data.frame(FishSpn, Cluster = FishSpn_km$cluster)

# Convert Cluster variable ke factor
FishSpn_clustered$Cluster <- factor(FishSpn_clustered$Cluster)

# Plot hasil klasterisasi dengan label yang sesuai
ggplot(MeatSpn_clustered, aes(x = dataCust$data.MntFishProducts, y = dataCust$data.Spending, color = Cluster)) +
  geom_point() +
  scale_color_manual(values = rainbow(3)) +
  theme_bw() +
  labs(x = "Pembelian Produk Fish", y = "Spending")

