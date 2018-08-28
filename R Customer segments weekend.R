setwd("C:/Users/Jennifer/Documents/ADM/HW 3")
bakery<-read.csv("bakery_binary.csv")
str(bakery)
summary(bakery)
bakery<-bakery[,c(1:8, 10, 12:34, 36:51)] 
bakeryweekend=bakery[!(bakery$Weekend=="0"),]
str(bakeryweekend)
summary(bakeryweekend)

set.seed(123)
bakeryclusterwe4 <- kmeans(bakeryweekend, centers=4) 
bakeryclusterwe4$size
names(bakeryclusterwe4)
library(fpc)
plotcluster(bakeryweekend, bakeryclusterwe4$cluster, main="k=4") 
bakeryclusterwe4$withinss
bakeryclusterwe4$tot.withinss
bakeryclusterwe4$betweenss
bakeryclusterwe4$totss

set.seed(123)
bakeryclusterwe5 <- kmeans(bakeryweekend, centers=5) 
bakeryclusterwe5$size
plotcluster(bakeryweekend, bakeryclusterwe5$cluster, main="k = 5") 
bakeryclusterwe5$withinss
bakeryclusterwe5$tot.withinss
bakeryclusterwe5$betweenss
bakeryclusterwe5$totss

set.seed(123)
bakeryclusterwe6 <- kmeans(bakeryweekend, centers=6) 
bakeryclusterwe6$size
plotcluster(bakeryweekend, bakeryclusterwe6$cluster, main="k = 6") 
bakeryclusterwe6$withinss
bakeryclusterwe6$tot.withinss
bakeryclusterwe6$betweenss
bakeryclusterwe6$totss

set.seed(123)
bakeryclusterwe7 <- kmeans(bakeryweekend, centers=7) 
bakeryclusterwe7$size
plotcluster(bakeryweekend, bakeryclusterwe7$cluster, main="k = 7") 
bakeryclusterwe7$withinss
bakeryclusterwe7$tot.withinss
bakeryclusterwe7$betweenss
bakeryclusterwe7$totss

set.seed(123)
bakeryclusterwe8 <- kmeans(bakeryweekend, centers=8) 
bakeryclusterwe8$size
plotcluster(bakeryweekend, bakeryclusterwe8$cluster, main="k = 8") 
bakeryclusterwe8$withinss
bakeryclusterwe8$tot.withinss
bakeryclusterwe8$betweenss
bakeryclusterwe8$totss

clusterswe4<- bakeryclusterwe4$tot.withinss/bakeryclusterwe4$totss
clusterswe5<- bakeryclusterwe5$tot.withinss/bakeryclusterwe5$totss
clusterswe6<- bakeryclusterwe6$tot.withinss/bakeryclusterwe6$totss
clusterswe7<- bakeryclusterwe7$tot.withinss/bakeryclusterwe7$totss
clusterswe8<- bakeryclusterwe8$tot.withinss/bakeryclusterwe8$totss
totwithinss.metric <- c(clusterswe4, clusterswe5, clusterswe6, clusterswe7, clusterswe8)
print(totwithinss.metric)

clusterswe4<- bakeryclusterwe4$betweenss/bakeryclusterwe4$totss
clusterswe5<- bakeryclusterwe5$betweenss/bakeryclusterwe5$totss
clusterswe6<- bakeryclusterwe6$betweenss/bakeryclusterwe6$totss
clusterswe7<- bakeryclusterwe7$betweenss/bakeryclusterwe7$totss
clusterswe8<- bakeryclusterwe8$betweenss/bakeryclusterwe8$totss
betweenss.metric <- c(clusterswe4, clusterswe5, clusterswe6, clusterswe7, clusterswe8)
print(betweenss.metric) #Look for a ratio that is closer to 1.

#WithinSS
wss <- (nrow(bakeryweekend)-1)*sum(apply(bakeryweekend,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(bakeryweekend,centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within Sum of Squares", main = "Number of Clusters (k) versus Cluster Cohesiveness")

#BetweenSS
wss <- (nrow(bakeryweekend)-1)*sum(apply(bakeryweekend,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(bakeryweekend,centers=i)$betweenss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Between Group Sum of Squares", main = "Number of Clusters (k) versus Cluster Distinctiveness")

library(clusterSim)
#?index.G1 #read the ../doc/indexG1_details.pdf

a<-index.G1(bakeryweekend, bakeryclusterwe4$cluster, centrotypes = "centroids") 
b<-index.G1(bakeryweekend, bakeryclusterwe5$cluster, centrotypes = "centroids")
c<-index.G1(bakeryweekend, bakeryclusterwe6$cluster, centrotypes = "centroids")
d<-index.G1(bakeryweekend, bakeryclusterwe7$cluster, centrotypes = "centroids")
e<-index.G1(bakeryweekend, bakeryclusterwe8$cluster, centrotypes = "centroids")
pseudoF<-c(a,b,c,d,e)
pseudoF

bakeryclusterwe4$size 
Clusterswe_4<-data.frame(bakeryclusterwe4$centers)
Clusterswe_4<-data.frame(t(bakeryclusterwe4$centers)) 

bakeryclusterwe5$size 
Clusterswe_5<-data.frame(bakeryclusterwe5$centers)
Clusterswe_5<-data.frame(t(bakeryclusterwe5$centers)) 

bakeryclusterwe6$size 
Clusterswe_6<-data.frame(bakeryclusterwe6$centers)
Clusterswe_6<-data.frame(t(bakeryclusterwe6$centers)) 

bakeryclusterwe7$size 
Clusterswe_7<-data.frame(bakeryclusterwe7$centers)
Clusterswe_7<-data.frame(t(bakeryclusterwe7$centers)) 

bakeryclusterwe8$size 
Clusterswe_8<-data.frame(bakeryclusterwe8$centers)
Clusterswe_8<-data.frame(t(bakeryclusterwe8$centers)) 

bakeryweekend$cluster <- bakeryclusterwe7$cluster
aggregate(data = bakeryweekend, Weekend ~ cluster, mean)
