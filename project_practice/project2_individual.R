#Project 2 - Maxwell Fundi. 

#loading libraries
library(visdat)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)

#Tasks 1:Data Loading and Preprocessing
#Load the Vaalikone dataset (vaalikone_questions_all.csv) and understand the dataset's structure and variables.
data_questions <- read.csv(file = "C:/Users/maxwe/Documents/insights_from_data_R/project_practice/Project 2/vaalikone_questions_all.csv", sep = ";")
View(data_questions)
str(data_questions)


#Handle missing values, outliers, or any data quality issues.
vis_miss(data_questions)

#drop all rows with missing values - because the NAs are majorly acorss the rows
data_questions_clean <- na.omit(data_questions)
View(data_questions_clean)
vis_miss(data_questions_clean)
str(data_questions_clean)

# Covert different variables in the dataset to a suitable data type (hint: all columns starting with Q to the factor and all the columns starting with W to the ordered factor)
# Convert columns starting with "Q" to  factors
data_questions_clean <- data_questions_clean %>% mutate(across(starts_with("Q"), as.factor))
str(data_questions_clean)

# Convert columns starting with "W" to ordered factors
data_questions_clean <- data_questions_clean %>% mutate(across(starts_with("W"), ~ as.ordered(.)))
str(data_questions_clean)

# Convert columns starting with "W" to ordered factors
data_questions_clean <- data_questions_clean %>% mutate(across(starts_with("C"), ~ as.ordered(.)))
str(data_questions_clean)

#save cleaned data
write.csv(data_questions_clean, "C:/Users/maxwe/Documents/insights_from_data_R/project_practice/Project 2/data.csv", row.names = FALSE)


#Tasks 2:Clustering Candidates: - #Select a suitable distance measure for clustering.
#using gower - It handles data of different types well \
gower_dist <- daisy(data_questions_clean[,-1], metric = "gower")
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)
fviz_dist(gower_dist, order= F)


#Apply an appropriate clustering algorithm to group candidates based on their answers.
# Perform hierarchical clustering
hc <- hclust(gower_dist, method = "ward.D2")
clusters <- cutree(hc, k = 3)

# Plot the dendrogram
plot(hc, xlab = "")

#evaluating clusters 
fviz_silhouette(silhouette(clusters, gower_dist))


# Load the Vaalikone Profiles (vaalikone_profiles_all.csv) dataset and use the "ID"
# column to merge with Vaalikone dataset and results (clusters).
data_profile <- read.csv(file = "C:/Users/maxwe/Documents/insights_from_data_R/project_practice/Project 2/vaalikone_profiles_all.csv", sep = ";")

View(data_profile)
str(data_profile)

# Add the cluster assignments to the data_questions_clean dataset
data_questions_clean$cluster <- clusters
View(data_questions_clean)

#save cleaned data
write.csv(data_questions_clean, "C:/Users/maxwe/Documents/insights_from_data_R/project_practice/Project 2/data_questions_clean_clustered.csv", row.names = FALSE)

# Merge the datasets by ID
merged_data <- merge(data_profile, data_questions_clean, by = "ID")
View(merged_data)
str(merged_data)

# Analyze the alignment of clusters with political parties
ggplot(merged_data, aes(x = factor(cluster), fill = Party)) +  geom_bar(position = "dodge") +
  labs(title = "Cluster Distribution by Political Party", x = "Cluster", y = "Count") 


















