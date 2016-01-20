#1.1
dailykos <- read.csv("dailykos.csv")

distances = dist(dailykos, method = "euclidean")
dailycluster = hclust(distances, method = "ward.D")

#1.2

plot(dailycluster)

#1.4
clusterKos = cutree(dailycluster, k = 7)

table(clusterKos)

cluster1 = subset(dailykos, clusterKos == 1)
cluster2 = subset(dailykos, clusterKos == 2)
cluster3 = subset(dailykos, clusterKos == 3)
cluster4 = subset(dailykos, clusterKos == 4)
cluster5 = subset(dailykos, clusterKos == 5)
cluster6 = subset(dailykos, clusterKos == 6)
cluster7 = subset(dailykos, clusterKos == 7)

sum(cluster1)

#1.5

tail(sort(colMeans(cluster1)))

#1.6

tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

#2.1
set.seed(1000)
kmeansCluster = kmeans(dailykos, 7)

str(kmeansCluster)

table(kmeansCluster$cluster)

kcluster1 = subset(dailykos, kmeansCluster$cluster == 1)
kcluster2 = subset(dailykos, kmeansCluster$cluster == 2)
kcluster3 = subset(dailykos, kmeansCluster$cluster == 3)
kcluster4 = subset(dailykos, kmeansCluster$cluster == 4)
kcluster5 = subset(dailykos, kmeansCluster$cluster == 5)
kcluster6 = subset(dailykos, kmeansCluster$cluster == 6)
kcluster7 = subset(dailykos, kmeansCluster$cluster == 7)

str(kcluster7)

#2.2
tail(sort(colMeans(kcluster1)))
tail(sort(colMeans(kcluster2)))
tail(sort(colMeans(kcluster3)))
tail(sort(colMeans(kcluster4)))
tail(sort(colMeans(kcluster5)))
tail(sort(colMeans(kcluster6)))
tail(sort(colMeans(kcluster7)))

#2.3
table(clusterKos, kmeansCluster$cluster)

#2.4
table(clusterKos, kmeansCluster$cluster)

#2.5
table(clusterKos, kmeansCluster$cluster)

#2.6
table(clusterKos, kmeansCluster$cluster)





