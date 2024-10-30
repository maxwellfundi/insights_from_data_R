#install and load ggplot2
install.packages("ggplot2")
library(ggplot2)

#install and load tidyverse package
install.packages("tidyverse")
library(tidyverse)

#load mpg data 
data(mpg)

#geting info about the dataframe 
str(mpg)

#view first few rows of the data 
head(mpg)

#view the dataframe
View(mpg)


#Visualize the distribution of car classes (class variable) with a bar plot and determine the
#proportion of each car type. Identify the most common car type.

bar <- ggplot(data = mpg, mapping = aes(x=class)) + geom_bar() 

#view barchart
bar

#adding percentages to the barchart
barwithperct <- ggplot(data= mpg, mapping = aes(x=class)) + geom_bar() + geom_text(stat = 'count', aes(label = paste0(round(..count../sum(..count..)*100, 2), "%"))) 
barwithperct

#Engine Size and Fuel Efficiency
 #Plot a scatterplot of engine displacement (displ) vs. highway fuel efficiency (hwy).
 #Describe any observable trend. Does a larger engine size generally correlate with lower fuel efficiency?

scatter <- ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) + geom_point()
scatter



#Analyzing Drive Type
#Create subsets for each drive type (drv), i.e., f (front-wheel), r (rear-wheel), and 4 (4wd).
# Compare the average highway fuel efficiency (hwy) across these subsets using mean() and sd().
# Identify which drive type tends to have the highest and lowest fuel efficiency.
front_wheel <- mpg %>% filter(drv == "f") %>% select(hwy,drv) 
View(front_wheel)
summary(front_wheel, )
mean(front_wheel$hwy)
sd(front_wheel$hwy)

#rear wheel 
rear_wheel <- mpg %>% filter(drv == "r") %>% select(hwy,drv)
View(rear_wheel)
summary(rear_wheel)
mean(rear_wheel$hwy)
sd(rear_wheel$hwy)

#4 wd
fwd <- mpg %>% filter(drv == "4") %>% select(hwy,drv)
view(fwd)
summary(fwd)
mean(fwd$hwy)
sd(fwd$hwy)

#Exploring Fuel Economy by Cylinder Count
# Visualize the distribution of city fuel economy (cty) across cars with different numbers of cylinders (cyl) using a boxplot.
# Determine if there’s a noticeable trend in fuel efficiency with increasing cylinder count.
bplot <- ggplot(data = mpg, mapping = aes(x=cty, y= as.factor(cyl))) + geom_boxplot() + coord_flip()
bplot



#Factors Influencing Fuel Efficiency
#Investigate which variables might influence fuel efficiency (cty and hwy) the most.
# Use appropriate visualizations (scatterplots, boxplots) to examine relationships between class, displ, cyl, and fuel efficiency.
# Summarize your findings and suggest possible reasons for the relationships observed.

