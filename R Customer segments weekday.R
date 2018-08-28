setwd("C:/Users/Jennifer/Documents/ADM/HW 3")
bakery<-read.csv("bakery_binary.csv")
str(bakery)
summary(bakery)
bakery<-bakery[,c(1:6, 8:10, 12:13, 15:17, 19:21, 23:38, 40:51)] 
bakeryweekday=bakery[!(bakery$Weekend=="1"),]
str(bakeryweekday)
summary(bakeryweekday)

set.seed(123)
bakeryclusterwd4 <- kmeans(bakeryweekday, centers=4) 
bakeryclusterwd4$size
names(bakeryclusterwd4)
library(fpc)
plotcluster(bakeryweekday, bakeryclusterwd4$cluster, main="k=4") 
bakeryclusterwd4$withinss
bakeryclusterwd4$tot.withinss
bakeryclusterwd4$betweenss
bakeryclusterwd4$totss

set.seed(123)
bakeryclusterwd5 <- kmeans(bakeryweekday, centers=5) 
bakeryclusterwd5$size
plotcluster(bakeryweekday, bakeryclusterwd5$cluster, main="k = 5") 
bakeryclusterwd5$withinss
bakeryclusterwd5$tot.withinss
bakeryclusterwd5$betweenss
bakeryclusterwd5$totss

set.seed(123)
bakeryclusterwd6 <- kmeans(bakeryweekday, centers=6) 
bakeryclusterwd6$size
plotcluster(bakeryweekday, bakeryclusterwd6$cluster, main="k = 6") 
bakeryclusterwd6$withinss
bakeryclusterwd6$tot.withinss
bakeryclusterwd6$betweenss
bakeryclusterwd6$totss

set.seed(123)
bakeryclusterwd7 <- kmeans(bakeryweekday, centers=7) 
bakeryclusterwd7$size
plotcluster(bakeryweekday, bakeryclusterwd7$cluster, main="k = 7") 
bakeryclusterwd7$withinss
bakeryclusterwd7$tot.withinss
bakeryclusterwd7$betweenss
bakeryclusterwd7$totss

set.seed(123)
bakeryclusterwd8 <- kmeans(bakeryweekday, centers=8) 
bakeryclusterwd8$size
plotcluster(bakeryweekday, bakeryclusterwd8$cluster, main="k = 8") 
bakeryclusterwd8$withinss
bakeryclusterwd8$tot.withinss
bakeryclusterwd8$betweenss
bakeryclusterwd8$totss

clusterswd4<- bakeryclusterwd4$tot.withinss/bakeryclusterwd4$totss
clusterswd5<- bakeryclusterwd5$tot.withinss/bakeryclusterwd5$totss
clusterswd6<- bakeryclusterwd6$tot.withinss/bakeryclusterwd6$totss
clusterswd7<- bakeryclusterwd7$tot.withinss/bakeryclusterwd7$totss
clusterswd8<- bakeryclusterwd8$tot.withinss/bakeryclusterwd8$totss
totwithinss.metric <- c(clusterswd4, clusterswd5, clusterswd6, clusterswd7, clusterswd8)
print(totwithinss.metric)

clusterswd4<- bakeryclusterwd4$betweenss/bakeryclusterwd4$totss
clusterswd5<- bakeryclusterwd5$betweenss/bakeryclusterwd5$totss
clusterswd6<- bakeryclusterwd6$betweenss/bakeryclusterwd6$totss
clusterswd7<- bakeryclusterwd7$betweenss/bakeryclusterwd7$totss
clusterswd8<- bakeryclusterwd8$betweenss/bakeryclusterwd8$totss
betweenss.metric <- c(clusterswd4, clusterswd5, clusterswd6, clusterswd7, clusterswd8)
print(betweenss.metric) #Look for a ratio that is closer to 1.

#WithinSS
wss <- (nrow(bakeryweekday)-1)*sum(apply(bakeryweekday,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(bakeryweekday,centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within Sum of Squares", main = "Number of Clusters (k) versus Cluster Cohesiveness")

#BetweenSS
wss <- (nrow(bakeryweekday)-1)*sum(apply(bakeryweekday,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(bakeryweekday,centers=i)$betweenss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Between Group Sum of Squares", main = "Number of Clusters (k) versus Cluster Distinctiveness")

library(clusterSim)
#?index.G1 #read the ../doc/indexG1_details.pdf

a<-index.G1(bakeryweekday, bakeryclusterwd4$cluster, centrotypes = "centroids") 
b<-index.G1(bakeryweekday, bakeryclusterwd5$cluster, centrotypes = "centroids")
c<-index.G1(bakeryweekday, bakeryclusterwd6$cluster, centrotypes = "centroids")
d<-index.G1(bakeryweekday, bakeryclusterwd7$cluster, centrotypes = "centroids")
e<-index.G1(bakeryweekday, bakeryclusterwd8$cluster, centrotypes = "centroids")
pseudoF<-c(a,b,c,d,e)
pseudoF

bakeryclusterwd4$size 
Clusterswd_4<-data.frame(bakeryclusterwd4$centers)
Clusterswd_4<-data.frame(t(bakeryclusterwd4$centers)) 

bakeryclusterwd5$size 
Clusterswd_5<-data.frame(bakeryclusterwd5$centers)
Clusterswd_5<-data.frame(t(bakeryclusterwd5$centers)) 

bakeryclusterwd6$size 
Clusterswd_6<-data.frame(bakeryclusterwd6$centers)
Clusterswd_6<-data.frame(t(bakeryclusterwd6$centers)) 

bakeryclusterwd7$size 
Clusterswd_7<-data.frame(bakeryclusterwd7$centers)
Clusterswd_7<-data.frame(t(bakeryclusterwd7$centers)) 

bakeryclusterwd8$size 
Clusterswd_8<-data.frame(bakeryclusterwd8$centers)
Clusterswd_8<-data.frame(t(bakeryclusterwd8$centers)) 

bakeryweekday$cluster <- bakeryclusterwd7$cluster
aggregate(data = bakeryweekday, Weekend ~ cluster, mean)
