#Membuat Tabel baru yang akan digunakan (belom normalisasi)
library(ggplot2)
library(plyr)
library(dplyr)

AggCountry <- aggregate(EcommDataClean$TotalPrice, by=list(EcommDataClean$Country), FUN=sum)
names(AggCountry) <- c("Country", "Total Sales")

CountryCustomer <- select(EcommDataClean, c(7,8))
count_CustID <- ddply(CountryCustomer, .(CountryCustomer$Country, CountryCustomer$CustomerID), nrow)
names(count_CustID) <- c("Country", "CustomerID", "Frequency")  

CountryCustomer <- select(count_CustID, c(1))
count_CustID <- ddply(CountryCustomer, .(CountryCustomer$Country), nrow)
names(count_CustID) <- c("Country", "Number of Unique Customer")
CountryCustomer <- count_CustID

AggCountry <- merge(AggCountry, CountryCustomer, by.x = "Country", by.y = "Country")

AggQuantity <- aggregate(EcommDataClean$Quantity, by=list(EcommDataClean$Country), FUN=sum)
names(AggQuantity) <- c("Country", "Total Quantity")

AggCountry <- merge(AggCountry, AggQuantity, by.x = "Country", by.y = "Country")

View(AggCountry)

##################################################################################################
#Mengganti nama row

row.names(AggCountry) <- AggCountry$Country
AggCountry <- AggCountry[,c(2:4)]

View(AggCountry)

#######

row.names(AggCountryNoUK) <- AggCountryNoUK$Country
AggCountryNoUK <- AggCountryNoUK[,c(2:4)]

View(AggCountryNoUK)

##################################################################################################
#Normalisasi Z

ZAggCountry <- as.data.frame( scale(AggCountry[1:3] ))

View(ZAggCountry)

ZAggCountryNoUK <- as.data.frame( scale(AggCountryNoUK[1:3] ))

View(ZAggCountryNoUK)

#Normalisasi MinMax

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
NormAggCountry <- as.data.frame(lapply(AggCountry[2:4], normalize))

##################################################################################################
#Menemukan jumlah kluster terbaik

set.seed(123)
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(ZAggCountry, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

############################################

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(ZAggCountryNoUK, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

##################################################################################################
#Clustering

set.seed(20)
CountryCluster <- kmeans(ZAggCountry, 2, nstart = 20)
CountryCluster$cluster


plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(CountryCluster$centers), max(CountryCluster$centers)), xlim = c(0, 3))

axis(1, at = c(1:3), labels = names(ZAggCountry))

for (i in c(1:5))
  lines(CountryCluster$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1,3,5),
                                                       "black", "blue"))

text(x = 0.5, y = CountryCluster$centers[, 1], labels = paste("Cluster", c(1:5)))

#########################

set.seed(20)
CountryCluster <- kmeans(ZAggCountryNoUK, 4, nstart = 20)
CountryCluster$cluster

plot(c(0), xaxt = 'n', ylab = "", type = "l",
     ylim = c(min(CountryCluster$centers), max(CountryCluster$centers)), xlim = c(0, 3))

axis(1, at = c(1:3), labels = names(ZAggCountryNoUK))

for (i in c(1:4))
  lines(CountryCluster$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1,3,5),
                                                                   "black", "blue"))

text(x = 0.5, y = CountryCluster$centers[, 1], labels = paste("Cluster", c(1:4)))


##################################################################################################
#Menghilangkan UK

NormAggCountryNoUK <- NormAggCountry[-c(35),]

AggCountryNoUK <- AggCountry[-c(35),]

View(AggCountryNoUK)

row.names(NormAggCountryNoUK) <- AggCountryNoUK$Country

##################################################################################################


with(data = ZAggCountry,
     scatterplot3d(x = ZAggCountry$`Total Sales`,
                   y = ZAggCountry$`Number of Unique Customer`,
                   z = ZAggCountry$`Total Quantity`,
     )
)

##################################################################################################

library(rgl)
library(car)


scatter3d(x = ZAggCountry$`Total Sales`, y = ZAggCountry$`Number of Unique Customer`, z = ZAggCountry$`Total Quantity`, groups = as.factor(CountryCluster$cluster), surface=FALSE)
rglwidget() 
  