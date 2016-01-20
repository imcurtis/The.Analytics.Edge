#1.1
airlines <- read.csv("AirlinesCluster.csv")
summary(airlines)
str(airlines)

#1.3
install.packages("caret")
library(caret)

preproc = preProcess(airlines)

airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)

#2.1

distances = dist(airlinesNorm, method = "euclidean")
airlinescluster = hclust(distances, method = "ward.D")

plot(airlinescluster)

#2.2
airlineTree = cutree(airlinescluster, k = 5)
table(airlineTree)

#2.3
tapply(airlines$Balance, airlineTree, mean)
tapply(airlines$QualMiles, airlineTree, mean)
tapply(airlines$BonusMiles, airlineTree, mean)
tapply(airlines$BonusTrans, airlineTree, mean)
tapply(airlines$FlightMiles, airlineTree, mean)
tapply(airlines$FlightTrans, airlineTree, mean)
tapply(airlines$DaysSinceEnroll, airlineTree, mean)

#2.4

#2.5
#BonusTrans
#BonusMiles

#2.6
#2,4,3

#2.7

#3.1
set.seed(88)
KMCairlines = kmeans(airlinesNorm, 7, iter.max = 1000)
table(KMCairlines$cluster)

#3.2
KMCairlines$centers

airlinescluster$centers

cluster1 = subset(airlines, airlineTree == 1)
cluster1





