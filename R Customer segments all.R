setwd("C:/Users/Jennifer/Documents/ADM/HW 3")
bakery<-read.csv("bakery_binary.csv")
str(bakery)
summary(bakery)
bakery<-bakery[,c(1:6, 8:10, 12:26, 28:34, 36:38, 40:51)] 
str(bakery)
summary(bakery)

set.seed(123)
bakerycluster4 <- kmeans(bakery, centers=4) 
bakerycluster4$size
names(bakerycluster4)
library(fpc)
plotcluster(bakery, bakerycluster4$cluster, main="k=4") 
bakerycluster4$withinss
bakerycluster4$tot.withinss
bakerycluster4$betweenss
bakerycluster4$totss

set.seed(123)
bakerycluster5 <- kmeans(bakery, centers=5) 
bakerycluster5$size
plotcluster(bakery, bakerycluster5$cluster, main="k = 5") 
bakerycluster5$withinss
bakerycluster5$tot.withinss
bakerycluster5$betweenss
bakerycluster5$totss

set.seed(123)
bakerycluster6 <- kmeans(bakery, centers=6) 
bakerycluster6$size
plotcluster(bakery, bakerycluster6$cluster, main="k = 6") 
bakerycluster6$withinss
bakerycluster6$tot.withinss
bakerycluster6$betweenss
bakerycluster6$totss

set.seed(123)
bakerycluster7 <- kmeans(bakery, centers=7) 
bakerycluster7$size
plotcluster(bakery, bakerycluster7$cluster, main="k = 7") 
bakerycluster7$withinss
bakerycluster7$tot.withinss
bakerycluster7$betweenss
bakerycluster7$totss

set.seed(123)
bakerycluster8 <- kmeans(bakery, centers=8) 
bakerycluster8$size
plotcluster(bakery, bakerycluster8$cluster, main="k = 8") 
bakerycluster8$withinss
bakerycluster8$tot.withinss
bakerycluster8$betweenss
bakerycluster8$totss

clusters4<- bakerycluster4$tot.withinss/bakerycluster4$totss
clusters5<- bakerycluster5$tot.withinss/bakerycluster5$totss
clusters6<- bakerycluster6$tot.withinss/bakerycluster6$totss
clusters7<- bakerycluster7$tot.withinss/bakerycluster7$totss
clusters8<- bakerycluster8$tot.withinss/bakerycluster8$totss
totwithinss.metric <- c(clusters4, clusters5, clusters6, clusters7, clusters8)
print(totwithinss.metric)

clusters4<- bakerycluster4$betweenss/bakerycluster4$totss
clusters5<- bakerycluster5$betweenss/bakerycluster5$totss
clusters6<- bakerycluster6$betweenss/bakerycluster6$totss
clusters7<- bakerycluster7$betweenss/bakerycluster7$totss
clusters8<- bakerycluster8$betweenss/bakerycluster8$totss
betweenss.metric <- c(clusters4, clusters5, clusters6, clusters7, clusters8)
print(betweenss.metric) #Look for a ratio that is closer to 1.

#WithinSS
wss <- (nrow(bakery)-1)*sum(apply(bakery,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(bakery,centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within Sum of Squares", main = "Number of Clusters (k) versus Cluster Cohesiveness")

#BetweenSS
wss <- (nrow(bakery)-1)*sum(apply(bakery,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(bakery,centers=i)$betweenss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Between Group Sum of Squares", main = "Number of Clusters (k) versus Cluster Distinctiveness")

library(clusterSim)
#?index.G1 #read the ../doc/indexG1_details.pdf

a<-index.G1(bakery, bakerycluster4$cluster, centrotypes = "centroids") 
b<-index.G1(bakery, bakerycluster5$cluster, centrotypes = "centroids")
c<-index.G1(bakery, bakerycluster6$cluster, centrotypes = "centroids")
d<-index.G1(bakery, bakerycluster7$cluster, centrotypes = "centroids")
e<-index.G1(bakery, bakerycluster8$cluster, centrotypes = "centroids")
pseudoF<-c(a,b,c,d,e)
pseudoF

bakerycluster4$size 
Clusters_4<-data.frame(bakerycluster4$centers)
Clusters_4<-data.frame(t(bakerycluster4$centers)) 

bakerycluster5$size 
Clusters_5<-data.frame(bakerycluster5$centers)
Clusters_5<-data.frame(t(bakerycluster5$centers)) 

bakerycluster6$size 
Clusters_6<-data.frame(bakerycluster6$centers)
Clusters_6<-data.frame(t(bakerycluster6$centers)) 

bakerycluster7$size 
Clusters_7<-data.frame(bakerycluster7$centers)
Clusters_7<-data.frame(t(bakerycluster7$centers)) 

bakerycluster8$size 
Clusters_8<-data.frame(bakerycluster8$centers)
Clusters_8<-data.frame(t(bakerycluster8$centers)) 

bakery$cluster <- bakerycluster7$cluster
aggregate(data = bakery, Weekend ~ cluster, mean)
