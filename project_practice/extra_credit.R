# Install the readxl package (if not already installed)
install.packages("readxl")

# Load the readxl package
library(readxl)

# Import the Excel file
wine_data <- read_excel("C:/Users/maxwe/Documents/insights_from_data_R/project_practice/extra_credit/wine_quality.xlsx")