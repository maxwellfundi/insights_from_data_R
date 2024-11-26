
#loading libraries 
library(ggplot2)


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
  
