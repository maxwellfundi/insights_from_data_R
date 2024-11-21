library(ggplot2)
library(factoextra)
library(dplyr)
library(tidyverse)
library(cluster)


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


#Visualisation
fviz_cluster(km_res, df, ellipse.type = "norm")


# using two distance measures. Use the Elbow Method to determine the optimal number ofclusters for each measure. 

#b) Analyze clustering results to identify patterns and associations between main features (mpg, hp, wt) and clusters. Compare how results differ between the
# two distance measures.



# Clustering with Mixed Data Types:
#   2. Load the "ChickWeight" dataseta and check the type of variables in dataset. Select a
# suitable distance measure according to datatypes and prepare data for clustering.

ChickWeightdata <- ChickWeight
str(ChickWeightdata)


#using gower - It handles data of different types well \
gower_dist <- daisy(ChickWeightdata, metric = "gower")
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)
fviz_dist(gower_dist, order= F)

# 3. Apply hierarchical clustering and select a suitable number of clusters. Explore if the clusters
# align with the "diet" variable.

hc <- hclust(gower_dist, method = "ward.D2")
plot(hc)




# Clustering Tendency:
#   4. Generate a random matrix of size (200,5) and calculate Euclidean distance measure. 
# Set seed for reproducibility
set.seed(30)
random_matrix <- matrix(runif(200 * 5), nrow = 200, ncol = 5)
dim(random_matrix)
str(random_matrix)


# Compute the euclidian distance measure
dist_matrix <- dist(random_matrix, method = "euclidean")
dist_matrix
fviz_dist(dist_matrix, order= F)

#Load the "iris" dataset in R and calculate the distance measure. Visualize both distance measures
# and their distributions in random matrix and Iris data sets. Compare the results.

irisdata <- iris
irisnumeric <- irisdata[,-5]
dist_matrix <- dist(irisnumeric, method = "euclidean")
dist_matrix
fviz_dist(dist_matrix, order= F)

# 5. Apply k-means clustering to evaluate clusters for both iris and random matrix datasets.
# Compare the results.
#optimal no of clusters for iris data
fviz_nbclust(irisnumeric, kmeans, method = "wss") +   ggtitle("Elbow Method")
set.seed(42)  
kmeans_result <- kmeans(irisnumeric, centers = 2, nstart = 25)
fviz_cluster(kmeans_result, irisnumeric, ellipse.type = "norm")

#optimal no of clusters for random matrix data
fviz_nbclust(random_matrix, kmeans, method = "wss") +   ggtitle("Elbow Method")
set.seed(42)  
kmeans_result <- kmeans(random_matrix, centers = 2, nstart = 25)
fviz_cluster(kmeans_result, random_matrix, ellipse.type = "norm")
