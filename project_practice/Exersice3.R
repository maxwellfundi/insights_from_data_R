#Install and load packages 
install.packages("datasets")
install.packages("factoextra")
install.packages("dplR")
library(datasets)
library(ggplot2)
library(visdat)
library(factoextra)
library(dplyr)
library(tidyverse)

data <- USArrests
View(data)

# Data Preparation and Transformation 
# o Data Cleaning: Review the dataset for any inconsistencies or missing values. If
# necessary, apply appropriate data cleaning techniques.
ggplot(data =data, mapping = aes(y= Murder, x ="")) + geom_boxplot(outlier.colour = "red")

ggplot(data =data, mapping = aes(y= Assault, x ="")) + geom_boxplot(outlier.colour = "red")

ggplot(data =data, mapping = aes(y= UrbanPop, x ="")) + geom_boxplot(outlier.colour = "red")

ggplot(data =data, mapping = aes(y= Rape, x ="")) + geom_boxplot(outlier.colour = "red")

#replacing all above 40 values with mean of column
data$Rape[data$Rape > 40] <- round(mean(data$Rape, 2))

summary(data)

#Calculate the percentage of missing observations for each variable
vis_miss(data)

# o Data Transformation: Determine a suitable transformation method for the dataset before
# clustering and implement it.

#scale the data - 
scaleddf <- scale(data)
View(scaleddf)

#Visualise distance matrix
fviz_dist(scaleddf)


#2
# Distance Measure Selection: Choose an appropriate distance measure and calculate the distance
# matrix for the dataset.

# Compute the distance matrix
dist_matrix <- dist(scaleddf, method = "euclidean")
dist_matrix

#3
# K-means Clustering and Cluster Evaluation
# o Apply K-means clustering for values of k from 2 to 10.
# o Use the Elbow method and Silhouette Score to determine the optimal number of clusters.
# o Visualize the clusters using fviz_cluster from the factoextra package.


#4
# Hierarchical Clustering and Visualization
# o Apply hierarchical clustering on the scaled data.
# o Plot the dendrogram, and choose a suitable height to cut and identify clusters.
# Apply hierarchical clustering using Ward's method
hc <- hclust(dist_matrix, method = "ward.D2")
hc

# Step 3: Plot the dendrogram
plot(hc, main = "Dendrogram of Hierarchical Clustering", xlab = "", sub = "", cex = 0.6)



# 5
# Cluster Analysis and Interpretation
# o Based on the clustering results, answer:
#    How well do K-means and hierarchical clusters align?
#    Which states are grouped together, and do these clusters reflect regional trends in
# crime rates?
#    Highlight the strengths and limitations of both clustering methods


