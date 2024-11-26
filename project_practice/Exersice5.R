#install packages
install.packages("caret")
install.packages("ModelMetrics")

#load packages 
library(ggplot2)
library(caret)
library(ModelMetrics)


# 1. Load Economic Data:
#   Use the ggplot2 package to load the economic dataset.
#   Familiarize yourself with the dataset using the help function
data <- economics
help("economics")
str(economics)
summary(economics)


#2Correlation Between Variables
#Perform the Pearson correlation analysis between the personal savings rate (psavert) and
#the number of unemployed (unemploy).
psavert_unemp_col <- cor(data$psavert, data$unemploy, method = "pearson")
psavert_unemp_col


# Conduct a second Pearson correlation analysis between the personal savings rate
#(psavert) and personal consumption expenditures (pce).

psavert_pce_col <- cor(data$psavert, data$pce, method = "pearson")
psavert_pce_col

#Explore if there is a linear relationship between these variables and visualize the results
#with scatter plot
ggplot(data= data, mapping = aes(x = unemploy, y = psavert)) + geom_point() + geom_smooth(method = "lm", se = TRUE)
ggplot(data= data, mapping = aes(x = pce, y = psavert)) + geom_point() + geom_smooth(method = "lm", se = TRUE)


# 3. Linear Regression:
#   o Conduct linear regression analysis to predict the personal saving rate from other
# variables (excluding the date).

# o Split the dataset into 80% training and 20% testing. Use only training data for creating
# the linear model.

# o Evaluate the model's performance in terms of MAE and correlation between actual and
# predicted personal saving rates in the test dataset.
data <- data[,-1]
set.seed(123)
split <- createDataPartition(data$psavert, p = 0.8 , list= FALSE)
train <- data[split,]
test <- data[-split,] 
lm_fit<- lm(psavert~.,data=train)
summary(lm_fit)

ypred<- predict(lm_fit,test)
df= data.frame(actual = test$psavert, Pred= ypred)
ggplot(df, aes(x= actual, y= Pred))+ geom_point()+geom_smooth(method = "lm")
cor(ypred, test$psavert)
mae(test$psavert, ypred)
# 
# 
# 
# Classification with Iris dataset: Select "Sepal.Length", "Sepal.Width", "Petal.Length", and
# "Petal.Width" as the input features (independent variables) and "Species" as the output variable
# (dependent label).
# 4. Calculate the variance of the selected input variables and compare their importance.
# 5. Classify the Iris dataset using the K-Nearest Neighbors (KNN) algorithm. Split the data into
# training and testing sets, performing 10-fold cross-validation, and evaluating the model's
# performance based on classification accuracy.
