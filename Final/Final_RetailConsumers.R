##Problem 1
Households <- read.csv("Households.csv")
mean(Households$MorningPct)
mean(Households$AfternoonPct)
table(Households$NumVisits, Households$MorningPct)
.2873369*2500
.5144897*2500
sum(Households$MorningPct == 0)
sum(Households$AfternoonPct == 0)
##WRONG, Use subset and MorningPct >= 100

##Problem 2
150plus <- subset(Households, AvgSalesValue >= 150)
mean(Households$NumVisits > 300)
Disc25Plus <- subset(Households, AvgDiscount > 25)
min(Disc25Plus$AvgSalesValue)

##Problem 3
#NumVisits

##Problem 4
library(caret)

preproc = preProcess(Households)

HouseholdsNorm = predict(preproc, Households)
max(HouseholdsNorm$NumVisits)
min(HouseholdsNorm$AfternoonPct)

##Correct!

##Problem 5
set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

##WRONG

##Problem 6
set.seed(200)
KMC = kmeans(HouseholdsNorm, centers = 10)
table(KMC$cluster)
?centroid
plot(KMC$cluster)
mean(KMC$cluster)
KMC$cluster

##Problem 12
set.seed(5000)
KMC = kmeans(HouseholdsNorm, centers = 5)
table(KMC$cluster)

##Problem 13




