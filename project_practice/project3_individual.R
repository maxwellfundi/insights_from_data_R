install.packages("class")
#loading libraries 
library(ggplot2)
library(factoextra)
library(ggplot2)
library(cluster)
library(dplyr)
library(caret)
library(class)
#Load data 
pima_data <- read.csv(file = "C:/Users/maxwe/Documents/insights_from_data_R/project_practice/Project3/pima-indians-diabetes_data.csv", sep = ",",header = FALSE)
View(pima_data)

#asign headers to the data 
colnames(pima_data) <- c("number_pregnant","plasma_glucose_conc","diastolic_pressure","triceps_thickness","serum_insulin","bmi","diabetes_pedigree_function","age","class")

#Task 1: - Replace these zero values with the median of the respective features.
# Identify columns with zero values
columns_with_zeros <- apply(pima_data, 2, function(col) any(col == 0))
print(columns_with_zeros)

#Colums with Zero - "number_pregnant","plasma_glucose_conc","diastolic_pressure","triceps_thickness","serum_insulin","bmi"
# replacing 0 in columns with mean of columns 

col_with_zero <- c("number_pregnant", "plasma_glucose_conc", "diastolic_pressure", "triceps_thickness", "serum_insulin", "bmi")

for (col in col_with_zero) {
    col_mean <- mean(pima_data[[col]][pima_data[[col]] != 0], na.rm = TRUE)
    pima_data[[col]][pima_data[[col]] == 0] <- col_mean
}

#check if columns have zeros anymore 
columns_with_zeros <- apply(pima_data, 2, function(col) any(col == 0))
print(columns_with_zeros)

#Task 2 - checking and handling outliers with boxplots  
num_cols <- c("number_pregnant", "plasma_glucose_conc", "diastolic_pressure", "triceps_thickness", "serum_insulin", "bmi", "diabetes_pedigree_function", "age")

for (col in num_cols) {
  boxplot <- ggplot(data = pima_data, mapping =  aes_string(y = col)) +  geom_boxplot() + labs(title = paste("Boxplot of", col), y = col)
  print(boxplot)
}

#replace outliers with the mean of the columns
#number_pregnant - >12.5
pima_data$number_pregnant[pima_data$number_pregnant > 12.5] <- round(mean(pima_data$number_pregnant,na.rm = TRUE))

#diastolic_pressure <38| >105
pima_data$diastolic_pressure[pima_data$diastolic_pressure > 105 | pima_data$diastolic_pressure < 38] <- round(mean(pima_data$diastolic_pressure,na.rm = TRUE))

#triceps_thickness  < 14 |> 40 
pima_data$triceps_thickness[pima_data$triceps_thickness > 40 | pima_data$triceps_thickness < 14] <- round(mean(pima_data$triceps_thickness,na.rm = TRUE))

#serum_insulin <80|>200
pima_data$serum_insulin[pima_data$serum_insulin > 200 | pima_data$serum_insulin < 80] <- round(mean(pima_data$serum_insulin,na.rm = TRUE))

#bmi > 48
pima_data$bmi[pima_data$bmi > 48] <- round(mean(pima_data$bmi,na.rm = TRUE))

#diabetes_pedigree_function > 1.05
pima_data$diabetes_pedigree_function[pima_data$diabetes_pedigree_function > 1.05] <- round(mean(pima_data$diabetes_pedigree_function,na.rm = TRUE))


#Data Exploration 
#Task 1 -  Plot the distribution of the outcome variable (diabetes diagnosis)
#using a bar chart. Determine if this is a balanced classification problem.

# Create a bar chart for the 'class' column
ggplot(data = pima_data , mapping =  aes(x = as.factor(class))) +  geom_bar()

#task 2 - Analyze the variance of input variables. Consider removing variables with exceptionally low variance, if necessary.
variance < - apply(pima_data[, -9], 2, var)
variance

#removing diabetes_pedigree_function as it has low variance
pima_data_var <- pima_data[,-7]
View(pima_data_var)

#Task 3  
#Correlation Analysis: Conduct correlation analysis among the input variables. Visualize
#the results and propose a strategy for handling highly correlated variables
cor_matrix <- cor(pima_data_var[, c("number_pregnant", "plasma_glucose_conc", "diastolic_pressure", "triceps_thickness", "serum_insulin", "bmi", "age")])

# Visualize the correlation 
melted_corr <- melt(cor_matrix)

# Plot the heatmap
ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) + geom_tile()


#Cluster Analysis: Apply k-means clustering 
# Task 2. Determine the Number of Clusters
# o Use the elbow method and silhouette analysis to determine the optimal number
# of clusters. Visualize and explain the selection process.


#scale the data 
cluster_data <- pima_data_var[,-8]
scaleddf <- scale(cluster_data)
View(scaleddf)

#optimal clusters using elbow method 
df= data.frame(scaleddf)
fviz_nbclust(df, kmeans, method = "wss") 

# optimal clusters using Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")

set.seed(12)
km_res <- kmeans(df, 2, nstart = 25)
aggregate(df, by=list(cluster=km_res$cluster), mean)
fviz_cluster(km_res, df)


#####
set.seed(123)  # For reproducibility
k <- 2  # Replace with the optimal number of clusters determined
kmeans_result <- kmeans(scaleddf, centers = k, nstart = 25)

# Add cluster assignments to the original data
df_clustered <- df %>% mutate(Cluster = kmeans_result$cluster)
# Perform PCA
pca_result <- prcomp(scaleddf, center = TRUE, scale. = TRUE)
pca_data <- data.frame(pca_result$x[, 1:2], Cluster = as.factor(kmeans_result$cluster))

# Plot the PCA clusters
ggplot(pca_data, aes(PC1, PC2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering (PCA)", x = "PC1", y = "PC2") +
  theme_minimal()


silhouette_score <- silhouette(kmeans_result$cluster, dist(scaleddf))
fviz_silhouette(silhouette_score)



##Classification Analysis: Use KNN to predict diabetes
# Task 3 - Train-Test Split: Split the dataset into training (80%) and testing (20%) sets, ensuring
# proportional representation of Outcome classes (0 and 1).

# Create a stratified split
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(pima_data_var$class, p = 0.8, list = FALSE)

# Split the data into training and testing sets
train_data <- pima_data_var[trainIndex, ]
test_data <- pima_data_var[-trainIndex, ]

# Separate features and target variable
train_x <- train_data[, -ncol(train_data)]
train_y <- train_data$class
test_x <- test_data[, -ncol(test_data)]
test_y <- test_data$class

# Train KNN classifiers with different values of K and present confusion matrices
k_values <- c(1, 3, 5, 7, 9)
for (k in k_values) {
  pred_y <- knn(train = train_x, test = test_x, cl = train_y, k = k)
  
  cat("\nConfusion Matrix for K =", k, ":\n")
  print(table(Predicted = pred_y, Actual = test_y))
  
  cat("\nClassification Report for K =", k, ":\n")
  print(confusionMatrix(as.factor(pred_y), as.factor(test_y)))
}

