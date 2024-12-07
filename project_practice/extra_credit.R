#Install packages
install.packages("readxl")

#load packages
library(readxl)
library(dplyr)
library(caret)
library(ggplot2)

#import the Excel file
wine_data <- read_excel("C:/Users/maxwe/Documents/insights_from_data_R/project_practice/extra_credit/wine_quality.xlsx")
View(wine_data)

#binarize type variable (0 for red, 1 for white)
wine_data$type <- ifelse(wine_data$type == "white", 1, 0)
View(wine_data)

#split the data into training (80%) and test (20%) sets
set.seed(123)
trainIndex <- createDataPartition(wine_data$quality, p = 0.80, list = FALSE)

train_data <- wine_data[trainIndex, ]
test_data <- wine_data[-trainIndex, ]


#fitting the linear regression model
model <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
              residual.sugar + chlorides + free.sulfur.dioxide + 
              total.sulfur.dioxide + density + pH + sulphates + 
              alcohol + type, data = train_data)

#view the summary of the model
options(scipen = 999)
summary(model)


#fitting the linear regression model without citric.acid and chlorides
model <- lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + free.sulfur.dioxide + 
              total.sulfur.dioxide + density + pH + sulphates + 
              alcohol + type, data = train_data)

#view the summary of the model
summary(model)

#Predict and view predictions on the wine quality on the test data
predictions <- round(predict(model, newdata = test_data), 0) 
head(predictions)


# ggplot2 Scatter Plot for Actual vs Predicted Wine Quality
ggplot(data = test_data, mapping =  aes(x = quality, y = predictions)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Actual vs Predicted Wine Quality",
       x = "Actual Wine Quality", y = "Predicted Wine Quality") + coord_cartesian(xlim = c(2.5, 8), ylim = c(3, 8))

# Calculate Mean Squared Error (MSE)
mse <- mean((predictions - test_data$quality)^2)
print(paste("Mean Squared Error: ", mse))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
print(paste("Root Mean Squared Error (RMSE): ", rmse))

# Calculate R-squared for the test set
rsq <- 1 - sum((predictions - test_data$quality)^2) / sum((test_data$quality - mean(test_data$quality))^2)
print(paste("R-squared: ", rsq))


#Calculate Adjusted R-squared
n <- nrow(test_data)
p <- length(coef(model)) - 1  
adj_rsq <- 1 - (1 - rsq) * (n - 1) / (n - p - 1)
print(paste("Adjusted R-squared: ", adj_rsq))

# Residual analysis and plotting
residuals <- predictions - test_data$quality
residuals <- data.frame(residuals)
# Plot residuals using ggplot2
ggplot(residuals, aes(x = residuals)) +
  geom_histogram(binwidth = 0.9, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") 





































