# Libraries
library(cluster)
library(factoextra)
library(ggplot2)
library(ggplotify)
library(dplyr)
library(utils)
library(plotly)
library(mice)

# Baca data
data <- read.csv("marketing_campaign.csv", sep = "\t", header = TRUE)

### PRAPROSES DATA ###

# Membuat atribut age untuk mengetahui usia customer
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
dataCust <- data %>%
  select(Age, Marital_Status, Children, Education, Seniority, Income, Spending,
         MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds)

# Memilih Marital_Status yang bernilai Single dan Married
dataCust <- subset(dataCust, Marital_Status %in% c("Single", "Married"))

# Diskretisasi Atribut Age 
# 1 = Dewasa (27-61); 2 = Lansia (62-96); 3 = Manula (97-130)
dataCust$Age <- as.numeric(dataCust$Age)
dataCust$Age <- cut(dataCust$Age, breaks = c(0, 61, 96, 130), labels = c(1, 2, 3))

# Cek missing value
md.pattern(dataCust)

# Isi nilai missing value
dataCust$Income[is.na(dataCust$Income)] <- mean(dataCust$Income, na.rm = TRUE)
md.pattern(dataCust)

str(dataCust)

# Inisialisasi Data Kategorik
# Atribut Education
# 1=undergraduated; 2=postgraduated
dataCust$Education[which(dataCust$Education=="Basic")] <- 1
dataCust$Education[which(dataCust$Education=="2n Cycle")] <- 1
dataCust$Education[which(dataCust$Education=="Graduation")] <- 2
dataCust$Education[which(dataCust$Education=="Master")] <- 2
dataCust$Education[which(dataCust$Education=="PhD")] <- 2
dataCust$Education <- as.integer(dataCust$Education)

# Atribut Marital Status
# 1=Single; 2=Married
dataCust$Marital_Status <- recode(dataCust$Marital_Status, "Single" = 1, "Married" = 2)
dataCust$Marital_Status <- as.integer(dataCust$Marital_Status)

summary(dataCust)
str(dataCust)

# Normalisasi
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dataCust$Income <- normalize(dataCust$Income)
dataCust$Spending <- normalize(dataCust$Spending)
dataCust$MntWines <- normalize(dataCust$MntWines)
dataCust$MntFruits <- normalize(dataCust$MntFruits)
dataCust$MntMeatProducts <- normalize(dataCust$MntMeatProducts)
dataCust$MntFishProducts <- normalize(dataCust$MntFishProducts)
dataCust$MntSweetProducts <- normalize(dataCust$MntSweetProducts)
dataCust$MntGoldProds <- normalize(dataCust$MntGoldProds)

### MENENTUKAN SEGMENTASI JUMLAH CLUSTER ###

# Mengubah tipe data Age, Marital_Status, dan Children menjadi numerik
dataCust$Age <- as.numeric(as.character(dataCust$Age))
dataCust$Marital_Status <- as.numeric(dataCust$Marital_Status)
dataCust$Children <- as.numeric(dataCust$Children)

# Metode Elbow
df_wss <- data.frame(Clusters = 1:10, WSS = wss)
ggplot(df_wss, aes(x = Clusters, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(x = "Jumlah Cluster", y = "Total WCSS") +
  ggtitle("Metode Elbow") +
  theme_minimal()

set.seed(123)

# K-Means dengan k =3
jumlah_cluster <- 3  # Ganti dengan jumlah cluster optimal 
kmeans_model <- kmeans(dataCust, centers = jumlah_cluster)
print(kmeans_model)

dataCust$Cluster <- as.factor(kmeans_model$cluster)
table(dataCust$Cluster)

# hasil
# cluster 1 = 447 pelanggan
# cluster 2 = 460 pelanggan
# cluster 3 = 437 pelanggan

### Visualisasi jumlah pelanggan dengan ketiga cluster
pie_data <- table(dataCust$Cluster)
pie_chart <- plot_ly(labels = names(pie_data), values = pie_data, type = "pie") %>%
  layout(title = "Pie Chart of Customer Segmentation")
pie_chart

# Analisis Deskriptif
# Statistik ringkasan untuk setiap cluster
summary_data <- aggregate(dataCust[, c("Age", "Marital_Status", "Children", "Income", "Spending", "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds")], 
                          by = list(dataCust$Cluster), FUN = mean)
summary_data <- summary_data[, -1]  

# Jumlah kategori produk tiap cluster
product_purchases <- dataCust %>%
  group_by(Cluster) %>%
  summarize(
    Total_Wines = sum(MntWines),
    Total_Fruits = sum(MntFruits),
    Total_MeatProducts = sum(MntMeatProducts),
    Total_FishProducts = sum(MntFishProducts),
    Total_SweetProducts = sum(MntSweetProducts),
    Total_GoldProds = sum(MntGoldProds)
  )
print(product_purchases)

# Visualisasi jumlah pelanggan vs. banyaknya pembelian produk
ggplot(product_purchases, aes(x = Cluster)) +
  geom_bar(aes(y = Total_Wines), fill = "dark blue", stat = "identity") +
  geom_bar(aes(y = Total_Fruits), fill = "green", stat = "identity") +
  geom_bar(aes(y = Total_MeatProducts), fill = "red", stat = "identity") +
  geom_bar(aes(y = Total_FishProducts), fill = "yellow", stat = "identity") +
  geom_bar(aes(y = Total_SweetProducts), fill = "purple", stat = "identity") +
  geom_bar(aes(y = Total_GoldProds), fill = "yellow", stat = "identity") +
  labs(x = "Cluster", y = "Total Pembelian Produk", fill = "Product Category") +
  ggtitle("Bar Plot of Pembelian Produk by Cluster") +
  theme_minimal()

## kesimpulan 
## Pada cluster 1 memiliki pembelian produk yang relatif rendah untuk semua kategori produk, terdapat
## penurunan dalam pembelian Total_Wines, Total_Fruits, Total_MeatProducts, Total_FishProducts, Total_SweetProducts, dan Total_GoldProds dibandingkan dengan rata-rata total pembelian.
## Cluster 2 memiliki pembelian produk yang relatif tinggi untuk sebagian kategori produk. Menunjukkan peningkatan
## signifikan dalam pembelian Total_Wines, Total_Fruits, Total_MeatProducts, Total_FishProducts, Total_SweetProducts, dan Total_GoldProds dibandingkan dengan rata-rata total pembelian. 
## Cluster 3  memiliki pembelian produk yang paling rendah untuk beberapa kategori produk, terutama Total_Wines, Total_MeatProducts, Total_FishProducts, dan Total_SweetProducts. Hanya pada kategori Total_Fruits dan Total_GoldProds terdapat sedikit peningkatan dalam pembelian dibandingkan dengan rata-rata total pembelian

# Visualisasi jumlah pelanggan berdasarkan atribut age di setiap cluster
age_cluster_data <- as.data.frame.table(age_cluster_data)
names(age_cluster_data) <- c("Age", "Cluster", "Frequency")

# Membuat bar plot menggunakan ggplot
ggplot(age_cluster_data, aes(x = Age, y = Frequency, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age", y = "Frequency", fill = "Cluster") +
  ggtitle("Bar Plot of Age by Cluster") +
  theme_minimal()

# Visualisasi Box Plot Income dengan ketiga cluster
ggplot(dataCust, aes(x = Cluster, y = Income)) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Income") +
  ggtitle("Box Plot of Income by Cluster") +
  theme_minimal()
## kesimpulan =
## Pemasukan di cluster 1 lebih rendah.
## Median pada boxplot menujukkan median pendapatan pada ketiga cluster realtif serupa.
## Upper whisker yang lebih panjang pada cluster 3 menujukkan bahwa adanya beberapa pelanggan di cluster 3 yang memiliki pendapatan lebih tinggi dibandingkan dengan mayoritas pelanggan di cluster tersebut.

# Visualisasi Box Plot Spending dengan ketiga cluster
ggplot(dataCust, aes(x = Cluster, y = Spending)) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Spending") +
  ggtitle("Box Plot of Spending by Cluster") +
  theme_minimal()
## kesimpulan =
## terdapat perbedaan dalam pola pengeluaran antara ketiga cluster.
## Cluster 2 cenderung memiliki pengluaran lebih tinggi sedangkan cluster 3 paling rendah.
## Outliers pada boxplot menunjukkan pada cluster 2 tidak memiliki perbedaan
## yang signifikan dalam pola pengeluaran dibanding cluster 1 dan 3.
## Cluster 2 memiliki variabilitas lebih besar dalam pengeluaran dibandingkan dengan cluster 1 dan 3.

# Visualisasi Scatter Plot Income dan Spending ketiga cluster pelanggan
library(ggplot2)
ggplot(dataCust, aes(x = Income, y = Spending, color = Cluster)) +
  geom_point() +
  labs(x = "Income", y = "Spending", color = "Cluster") +
  ggtitle("Scatter Plot of Income vs. Spending by Cluster") +
  theme_minimal()
## kesimpulan = terlihat titik-titik dalam setiap cluster menaik
## seiring dengan naiknya pendapatan dan pengeluaran.
## Menujukkan adanya hubungan positif anatara pendapatan dan pengeluaran.

##Cluster 1: Pada cluster ini, terdapat titik-titik yang cenderung berada pada pendapatan rendah dan pengeluaran menengah.
##Cluster 2: Pada cluster ini, terdapat titik-titik yang berada pada pendapatan menengah dan pengeluaran paling tinggi.
##Cluster 3: Pada cluster ini, terdapat titik-titik yang cenderung berada pada pendapatan tinggi dan pengeluaran paling rendah.

# Visualisasi Bar Plot
barplot_data <- table(dataCust$Cluster, dataCust$Marital_Status)
barplot(barplot_data, beside = TRUE, legend = TRUE,
        main = "Bar Plot of Marital Status by Cluster",
        xlab = "Cluster", ylab = "Frequency")
##kesimpulan = Pada barplot terlihat kategori married memiliki tinggi batang yang lebih dari kategori single
## Menunjukkakn proporsi individu pelanggan yang memiliki status perkawinan menikah / married lebih tinggi dari yang belum menikah / single
## Terdapat sedikit perbedaan dalam frekuensi antara kategori married dan single di setiap cluster, namun tidak signifikan.

# Visualisasi Scatter Plot Seniority Vs. Spending di ketiga cluster
ggplot(dataCust, aes(x = Spending, y = Seniority, color = Cluster)) +
  geom_point() +
  labs(x = "Spending", y = "Seniority", color = "Cluster") +
  ggtitle("Scatter Plot of Seniority vs. Spending by Cluster") +
  theme_minimal()
## kesimpulan = terlihat titik-titik dalam setiap cluster semakin menyebar jika jumlah pembelian
## produknya (spending) semakin banyak
## Tidak menunjukkan adanya hubungan khusus antara lama pelanggan menjadi member (Seniority) dan total pembelian produk (Spending)

# Visualisasi jumlah pelanggan vs. banyaknya pembelian produk wines, fishprods, goldprods
ggplot(product_purchases, aes(x = Cluster)) +
  geom_bar(aes(y = Total_Wines), fill = "dark blue", stat = "identity") +
  geom_bar(aes(y = Total_FishProducts), fill = "yellow", stat = "identity") +
  geom_bar(aes(y = Total_GoldProds), fill = "blue", stat = "identity") +
  labs(x = "Cluster", y = "Total Pembelian Produk", fill = "Product Category") +
  ggtitle("Bar Plot of Pembelian Produk by Cluster") +
  theme_minimal()

# Visualisasi jumlah pelanggan vs. banyaknya pembelian produk fruits, sweetprods, meatprods
ggplot(product_purchases, aes(x = Cluster)) +
  geom_bar(aes(y = Total_Fruits), fill = "green", stat = "identity") +
  geom_bar(aes(y = Total_SweetProducts), fill = "purple", stat = "identity") +
  geom_bar(aes(y = Total_MeatProducts), fill = "red", stat = "identity") +
  labs(x = "Cluster", y = "Total Pembelian Produk", fill = "Product Category") +
  ggtitle("Bar Plot of Pembelian Produk by Cluster") +
  theme_minimal()



