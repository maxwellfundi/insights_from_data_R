# EX2.1 - Data cleaning 

#install and load packages
install.packages("visdat")
install.packages("plotly")
library(visdat)
library(ggplot2)
library(plotly)
library(tidyverse)


#loading dirty CSV data
data <- read.csv(file = "C:/Users/maxwe/Documents/insights_from_data_R/project_practice/dirty_iris_csv.csv", header = TRUE, stringsAsFactors = FALSE)

#Determine the type of different variables
str(data)

#viewing the heed of the data 
head(data)

#Calculate the percentage of missing observations for each variable
vis_miss(data)

#boxplots for each numeric column 
#boxplot for sepal length
slength <- ggplot(data = data, mapping = aes(y= Sepal.Length, x = "")) + geom_boxplot(outlier.colour = "red")
slength

#check outlier and replace - They seem mistyoed 
data$Sepal.Length[data$Sepal.Length > 10 |data$Sepal.Length < 1]
data$Sepal.Length[data$Sepal.Length == 73] <- 7.3
data$Sepal.Length[data$Sepal.Length == 49] <- 4.9
#Zero outlier replaced with mean
data$Sepal.Length[data$Sepal.Length == 0] <- round(mean(data$Sepal.Length,na.rm = TRUE), 1)

#ploting the graph with no outliers
sl_no_outlier <- ggplot(data = data, mapping = aes(y= Sepal.Length, x = "")) + geom_boxplot(outlier.colour = "red")
sl_no_outlier

#boxplot for sepal width
swidth <- ggplot(data = data, mapping = aes(y= Sepal.Width, x = "")) + geom_boxplot(outlier.colour = "red")
swidth

#Check outlier values based on the boxplot (they seem to be mistyped)
data$Sepal.Width[data$Sepal.Width > 10 | data$Sepal.Width <1]
data$Sepal.Width[data$Sepal.Width == 29] <- 2.9
data$Sepal.Width[data$Sepal.Width == 30] <- 3.0

data$Sepal.Width[data$Sepal.Width < 1]
data$Sepal.Width[data$Sepal.Width == -3] <- 3.0

#Zero outlier replaced with mean
data$Sepal.Width[data$Sepal.Width == 0] <- round(mean(data$Sepal.Width,na.rm = TRUE), 1)

#recreate the boxplot to check outliers
sw_no_outlier <- ggplot(data = data, mapping = aes(y= Sepal.Width, x = "")) + geom_boxplot(outlier.colour = "red")
sw_no_outlier

#boxplot for petal length
plength <- ggplot(data = data, mapping = aes(y= Petal.Length, x = "")) + geom_boxplot(outlier.colour = "red")
plength

#Check outlier values based on the boxplot (Again they seem to be mistyped)
data$Petal.Length[data$Petal.Length > 10]
data$Petal.Length[data$Petal.Length == 63] <- 6.3
data$Petal.Length[data$Petal.Length == 23] <- 2.3
data$Petal.Length[data$Petal.Length == 14] <- 1.4

#boxplot for petal length with no outliers
pl_no_outlier <- ggplot(data = data, mapping = aes(y= Petal.Length, x = "")) + geom_boxplot(outlier.colour = "red")
pl_no_outlier

#boxplot for petal width
data$Petal.Width[data$Petal.Width == Inf] <- NA
pwidth <- ggplot(data = data, mapping = aes(y= Petal.Width, x = "")) + geom_boxplot(outlier.colour = "red")
pwidth

View(data)


#Carrying out imputation -mean or median imputation to handle the NA values  
#finding mean for columns and replacing NA and replacing mean
#sepal Length
meansl<- round(mean(data$Sepal.Length,na.rm = TRUE), 1)
data[is.na(data$Sepal.Length),"Sepal.Length"] <- meansl

#sepal width
meansw<- round(mean(data$Sepal.Width,na.rm = TRUE), 1)
data[is.na(data$Sepal.Width),"Sepal.Width"] <- meansw


#Petal Length
meanpl<- round(mean(data$Petal.Length,na.rm = TRUE), 1)
data[is.na(data$Petal.Length),"Petal.Length"] <- meanpl

#Petal Width
meanpw<- round(mean(data$Petal.Width,na.rm = TRUE), 1)
data[is.na(data$Petal.Width),"Petal.Width"] <- meanpw

#view dataset missing values 
vis_miss(data)

View(data)


#EX2.2 - Data visualization and exploration 
#Visualize the distribution of Species in cleaned iris data set (barplot).
specis <- ggplot(data =data, mapping = aes(x=Species))+geom_bar() + geom_text(stat = 'count', aes(label = paste0(round(..count../sum(..count..)*100, 2), "%")), vjust = -0.25)
specis 


#Analyze the relationship between the sepal length and sepal width and between petal length and petal width (scatterplot, geom_point), use different color for each species. According to analysis answer the following questions:
# i.	Which species has smaller sepal lengths but larger sepal widths? - Setosa
#ii.	Which species has larger sepal lengths but smaller sepal widths?-Virginica
scatter_specis <- ggplot(data= data, mapping = aes(x=Sepal.Length, y = Sepal.Width, colour = Species)) + geom_point()
scatter_specis


#EX2.3

#a.	Compare Petal Length and Petal Width in different species by visualization techniques (scatterplot, geom_point)
scatter_pl_pw <- ggplot(data= data, mapping = aes(x=Petal.Width, y = Petal.Length, colour = Species)) + geom_point() 
scatter_pl_pw

#b.	Use histogram to visualize the distributions of different variables (petal length, petal width, sepal length, sepal with)
ggplot(data =data, mapping = aes(x=Sepal.Length)) + geom_histogram()
ggplot(data =data, mapping = aes(x=Sepal.Width)) + geom_histogram()
ggplot(data =data, mapping = aes(x=Petal.Length)) + geom_histogram()
ggplot(data =data, mapping = aes(x=Petal.Width)) + geom_histogram()



#EX2.4
#a.	Create a new variable, sepal_to_petal_ratio, defined as the ratio of sepal length to petal length for each observation and visualize its associations with sepal length and petal length.
#b.	Add new variable to the main dirt_iris dataset.

data$sepal_to_petal_ratio <- data$Sepal.Length / data$Petal.Length

ggplot(data = data, mapping = aes(x = sepal_to_petal_ratio, y= Sepal.Length )) + geom_point()
ggplot(data = data, mapping = aes(x = sepal_to_petal_ratio, y= Petal.Length )) + geom_point()


#EX2.5
#a.	Compute a suitable measure of central tendency and measure of dispersion for each variable, use a barplot to visualize it.

stats <- data %>%
  summarise(
    sl_mean = mean(Sepal.Length,na.rm = TRUE),
    sl_sd = sd(Sepal.Length,na.rm = TRUE),
    
    pl_mean = mean(Petal.Length,na.rm = TRUE),
    pl_sd = sd(Petal.Length,na.rm = TRUE),
    
    sw_mean = mean(Sepal.Width,na.rm = TRUE),
    sw_sd = sd(Sepal.Width,na.rm = TRUE),
    
    pw_mean = mean(Petal.Width,na.rm = TRUE),
    pw_sd = sd(Petal.Width,na.rm = TRUE)
  )

stats
View(stats)

class(stats)

stats_long <- stats %>% gather(key = "Statistic", value = "Value")

stats_long

View(stats_long)

#plot
ggplot(data=stats_long, mapping = aes(x = Statistic, y = Value, fill = Statistic)) + geom_bar(stat = "identity")

#b.	Create a new matrix to store statistical summaries of each variable in the dirty_iris dataset. For each variable, calculate the mean, median, variance, and standard deviation. Set up the matrix with columns named "Mean", "Median", "Variance", and "Standard Deviation", and rows corresponding to each variable name in the dataset.


  

