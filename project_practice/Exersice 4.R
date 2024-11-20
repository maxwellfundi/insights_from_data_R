library(ggplot2)
library(factoextra)
library(dplyr)
library(tidyverse)

# Clustering with different distance measures:
#   1. a) Load the mtcars dataset
data(mtcars)

#check the types of variables
str(mtcars)
summary(mtcars)

#perform K-Means clustering
scaled_mtacars <- scale(mtcars)
dist_matrix <- dist(scaled_mtacars, method = "euclidean")
fviz_dist(dist_matrix, order= F)
fviz_nbclust(scaled_mtacars, kmeans, method = "wss") 


#Clustering 
df <- data.frame(scaled_mtacars)
set.seed(12)
km_res <- kmeans(df, 3, nstart = 25)
aggregate(df, by=list(cluster=km_res$cluster), mean)
# dd <- cbind(iris, cluster = km_res$cluster)
# table(km_res$cluster)

#Visualisation
fviz_cluster(km_res, df, ellipse.type = "norm")


# using two distance measures. Use the Elbow Method to determine the optimal number ofclusters for each measure. 

#b) Analyze clustering results to identify patterns and associations between main features (mpg, hp, wt) and clusters. Compare how results differ between the
# two distance measures.







# Clustering with Mixed Data Types:
#   2. Load the "ChickWeight" dataseta and check the type of variables in dataset. Select a
# suitable distance measure according to datatypes and prepare data for clustering.
# 3. Apply hierarchical clustering and select a suitable number of clusters. Explore if the clusters
# align with the "diet" variable.
# Clustering Tendency:
#   4. Generate a random matrix of size (200,5) and calculate Euclidean distance measure. Load
# the "iris" dataset in R and calculate the distance measure. Visualize both distance measures
# and their distributions in random matrix and Iris data sets. Compare the results.
# 5. Apply k-means clustering to evaluate clusters for both iris and random matrix datasets.
# Compare the results.