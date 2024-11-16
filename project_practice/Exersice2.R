# EX2.1 - Data cleaning 

#install and load packages
install.packages("visdat")
install.packages("plotly")
library(visdat)
library(ggplot2)
library(plotly)
library(tidyverse)


#loading dirty CSV data
data_dirty <- read.csv(file = "C:/Users/maxwe/Documents/insights_from_data_R/project_practice/dirty_iris_csv.csv", header = TRUE, stringsAsFactors = FALSE)

#Determine the type of different variables
str(data_dirty)

#viewing the heed of the data 
head(data_dirty)

#Calculate the percentage of missing observations for each variable
vis_miss(data_dirty)

#boxplots for each numeric column 
#boxplot for sepal length
slength <- ggplot(data = data_dirty, mapping = aes(y= Sepal.Length, x = "")) + geom_boxplot(outlier.colour = "red")
slength

#check outlier and replace - They seem mistyoed 
data_dirty$Sepal.Length[data_dirty$Sepal.Length > 10 |data_dirty$Sepal.Length < 1]
data_dirty$Sepal.Length[data_dirty$Sepal.Length == 73] <- 7.3
data_dirty$Sepal.Length[data_dirty$Sepal.Length == 49] <- 4.9
#Zero outlier replaced with mean
data_dirty$Sepal.Length[data_dirty$Sepal.Length == 0] <- round(mean(data_dirty$Sepal.Length,na.rm = TRUE), 1)

#ploting the graph with no outliers
sl_no_outlier <- ggplot(data = data_dirty, mapping = aes(y= Sepal.Length, x = "")) + geom_boxplot(outlier.colour = "red")
sl_no_outlier

#boxplot for sepal width
swidth <- ggplot(data = data_dirty, mapping = aes(y= Sepal.Width, x = "")) + geom_boxplot(outlier.colour = "red")
swidth

#Check outlier values based on the boxplot (they seem to be mistyped)
data_dirty$Sepal.Width[data_dirty$Sepal.Width > 10 | data_dirty$Sepal.Width <1]
data_dirty$Sepal.Width[data_dirty$Sepal.Width == 29] <- 2.9
data_dirty$Sepal.Width[data_dirty$Sepal.Width == 30] <- 3.0

data_dirty$Sepal.Width[data_dirty$Sepal.Width < 1]
data_dirty$Sepal.Width[data_dirty$Sepal.Width == -3] <- 3.0

#Zero outlier replaced with mean
data_dirty$Sepal.Width[data_dirty$Sepal.Width == 0] <- round(mean(data_dirty$Sepal.Width,na.rm = TRUE), 1)

#recreate the boxplot to check outliers
sw_no_outlier <- ggplot(data = data_dirty, mapping = aes(y= Sepal.Width, x = "")) + geom_boxplot(outlier.colour = "red")
sw_no_outlier

#boxplot for petal length
plength <- ggplot(data = data_dirty, mapping = aes(y= Petal.Length, x = "")) + geom_boxplot(outlier.colour = "red")
plength

#Check outlier values based on the boxplot (Again they seem to be mistyped)
data_dirty$Petal.Length[data_dirty$Petal.Length > 10]
data_dirty$Petal.Length[data_dirty$Petal.Length == 63] <- 6.3
data_dirty$Petal.Length[data_dirty$Petal.Length == 23] <- 2.3
data_dirty$Petal.Length[data_dirty$Petal.Length == 14] <- 1.4

#boxplot for petal length with no outliers
pl_no_outlier <- ggplot(data = data_dirty, mapping = aes(y= Petal.Length, x = "")) + geom_boxplot(outlier.colour = "red")
pl_no_outlier

#boxplot for petal width
data_dirty$Petal.Width[data_dirty$Petal.Width == Inf] <- NA
pwidth <- ggplot(data = data_dirty, mapping = aes(y= Petal.Width, x = "")) + geom_boxplot(outlier.colour = "red")
pwidth

View(data_dirty)


#Carrying out imputation -mean or median imputation to handle the NA values  
#finding mean for columns and replacing NA and replacing mean
#sepal Length
meansl<- round(mean(data_dirty$Sepal.Length,na.rm = TRUE), 1)
data_dirty[is.na(data_dirty$Sepal.Length),"Sepal.Length"] <- meansl

#sepal width
meansw<- round(mean(data_dirty$Sepal.Width,na.rm = TRUE), 1)
data_dirty[is.na(data_dirty$Sepal.Width),"Sepal.Width"] <- meansw


#Petal Length
meanpl<- round(mean(data_dirty$Petal.Length,na.rm = TRUE), 1)
data_dirty[is.na(data_dirty$Petal.Length),"Petal.Length"] <- meanpl

#Petal Width
meanpw<- round(mean(data_dirty$Petal.Width,na.rm = TRUE), 1)
data_dirty[is.na(data_dirty$Petal.Width),"Petal.Width"] <- meanpw

#missing values removed, so dirty data becomes clean data
data_clean <- data_dirty
#view dataset missing values 
vis_miss(data_clean)




#EX2.2 - Data visualization and exploration 
#Visualize the distribution of Species in cleaned iris data set (barplot).
specis <- ggplot(data =data_clean, mapping = aes(x=Species))+geom_bar() + geom_text(stat = 'count', aes(label = paste0(round(..count../sum(..count..)*100, 2), "%")), vjust = -0.25)
specis 


#Analyze the relationship between the sepal length and sepal width and between petal length and petal width (scatterplot, geom_point), use different color for each species. According to analysis answer the following questions:
# i.	Which species has smaller sepal lengths but larger sepal widths? - Setosa
#ii.	Which species has larger sepal lengths but smaller sepal widths?-Virginica
scatter_specis <- ggplot(data= data_clean, mapping = aes(x=Sepal.Length, y = Sepal.Width, colour = Species)) + geom_point()
scatter_specis


#EX2.3

#a.	Compare Petal Length and Petal Width in different species by visualization techniques (scatterplot, geom_point)
scatter_pl_pw <- ggplot(data= data_clean, mapping = aes(x=Petal.Width, y = Petal.Length, colour = Species)) + geom_point() 
scatter_pl_pw

#b.	Use histogram to visualize the distributions of different variables (petal length, petal width, sepal length, sepal with)
ggplot(data =data_clean, mapping = aes(x=Sepal.Length)) + geom_histogram()
ggplot(data =data_clean, mapping = aes(x=Sepal.Width)) + geom_histogram()
ggplot(data =data_clean, mapping = aes(x=Petal.Length)) + geom_histogram()
ggplot(data =data_clean, mapping = aes(x=Petal.Width)) + geom_histogram()



#EX2.4
#a.	Create a new variable, sepal_to_petal_ratio, defined as the ratio of sepal length to petal length for each observation and visualize its associations with sepal length and petal length.
#b.	Add new variable to the main dirt_iris dataset.

data_clean$sepal_to_petal_ratio <- data_clean$Sepal.Length / data_clean$Petal.Length

ggplot(data = data_clean, mapping = aes(x = sepal_to_petal_ratio, y= Sepal.Length )) + geom_point()
ggplot(data = data_clean, mapping = aes(x = sepal_to_petal_ratio, y= Petal.Length )) + geom_point()


#EX2.5
#a.	Compute a suitable measure of central tendency and measure of dispersion for each variable, use a barplot to visualize it.
stats <- data_clean %>%
  summarise(
    sl_mean = mean(Sepal.Length),
    sl_sd = sd(Sepal.Length),
    
    pl_mean = mean(Petal.Length),
    pl_sd = sd(Petal.Length),
    
    sw_mean = mean(Sepal.Width),
    sw_sd = sd(Sepal.Width),
    
    pw_mean = mean(Petal.Width),
    pw_sd = sd(Petal.Width)
  )

View(stats)

#stacking the data for plotting 
stats_long <- stats %>% gather(key = "Statistic", value = "Value")
View(stats_long)

#plot the stats on barchart
ggplot(data=stats_long, mapping = aes(x = Statistic, y = Value, fill = Statistic)) + geom_bar(stat = "identity")

#b.	Create a new matrix to store statistical summaries of each variable in the dirty_iris dataset. For each variable, calculate the mean, median, variance, and standard deviation. Set up the matrix with columns named "Mean", "Median", "Variance", and "Standard Deviation", and rows corresponding to each variable name in the dataset.
# Calculate statistical summaries for each variable
stats_summary <- data_clean %>%
  summarise(
    s.l_mean = mean(Sepal.Length),
    s.l_median = median(Sepal.Length),
    s.l_var = var(Sepal.Length),
    s.l_sd = sd(Sepal.Length),
    
    s.w_mean = mean(Sepal.Width),
    s.w_median = median(Sepal.Width),
    s.w_var = var(Sepal.Width),
    s.w_sd = sd(Sepal.Width),
    
    p.l_mean = mean(Petal.Length),
    p.l_median = median(Petal.Length),
    p.l_var = var(Petal.Length),
    p.l_sd = sd(Petal.Length),
    
    p.w_mean = mean(Petal.Width),
    p.w_median = median(Petal.Width),
    p.w_var = var(Petal.Width),
    p.w_sd = sd(Petal.Width)
  )
View(stats_summary)

# Creating the matrix of summary stats
stats_summary_matrix <- matrix(
  c(
    stats_summary$s.l_mean, stats_summary$s.l_median, stats_summary$s.l_var, stats_summary$s.l_sd,
    stats_summary$s.w_mean, stats_summary$s.w_median, stats_summary$s.w_var, stats_summary$s.w_sd,
    stats_summary$p.l_mean, stats_summary$Petal.Length_Median, stats_summary$p.l_var, stats_summary$p.l_sd,
    stats_summary$p.w_mean, stats_summary$p.w_median, stats_summary$p.w_var, stats_summary$p.w_sd
  ),
  nrow = 4,
  byrow = TRUE
)

# Set the row and column names for the matrix
rownames(stats_summary_matrix) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
colnames(stats_summary_matrix) <- c("Mean", "Median", "Variance", "Standard Deviation")

# View the matrix
stats_summary_matrix

