install.packages("cluster")
library(tidyverse)
library(cluster)
library(factoextra)

data(iris)

scaled_iris = scale(iris[,-5])

rnames=paste(iris[,5],1:nrow(iris),sep =",")
rownames(scaled_iris) = rnames
dist_iris = dist(scaled_iris, method = "euclidean")
names(dist_iris) = iris[,5]


library(factoextra)
png(filename = "dist_iris.png", width = 1200, height = 1200)
fviz_dist(dist_iris, order = F)
dev.off()

#GOWER
data("ChickWeight")
summary(ChickWeight)
str(ChickWeight)

gower_dist =daisy(ChickWeight, metric ="gower")
fviz_dist(gower_dist, order = F )
Summary(gower_dist)

#cLUSTERING ALGOS - 
library("factoextra")
#fviz_nbclust(x, FUNcluster, method = c("silhouette", "wss", "gap_stat"))
df= data.frame(scaled_iris)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 1)+ 
  geom_vline(xintercept = 3, linetype = 2)+  
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+ 
  labs(subtitle = "Silhouette method")


set.seed(12)
km_res <- kmeans(df, 3, nstart = 25)
aggregate(df, by=list(cluster=km_res$cluster), mean)
dd <- cbind(iris, cluster = km_res$cluster)
table(km_res$cluster)

fviz_cluster(km_res, iris[, -5], ellipse.type = "norm")


