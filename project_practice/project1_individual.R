#install and load ggplot2, Tidyverse
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

#load mpg data 
data(mpg)

#getting info about the dataframe 
str(mpg)

#view first few rows of the data 
head(mpg)

#view the dataframe
View(mpg)


#barchart of car classes

bar <- ggplot(data = mpg, mapping = aes(x=class)) + geom_bar() 
bar

#adding percentages to the barchart
barwithperct <- bar + geom_text(stat = 'count', aes(label = paste0(round(..count../sum(..count..)*100, 2), "%")), vjust = -0.25) 
barwithperct

#Adding title to the graph
finalbar <- barwithperct + ggtitle("Proportions of car classes")
finalbar

#scatterplot of engine displacement (displ) vs. highway fuel efficiency (hwy).
scatter <- ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) + geom_point() + ggtitle("Scatter of displacement vs highway fuel efficient")
scatter

#creating the front wheel subset 
front_wheel <- mpg %>% filter(drv == "f") %>% select(hwy,drv) 

#find mean and SD in 2 decimal places
round(mean(front_wheel$hwy),2)  
round(sd(front_wheel$hwy),2)

#creating the rear wheel subset 
rear_wheel <- mpg %>% filter(drv == "r") %>% select(hwy,drv)

#find mean and SD in 2 decimal places
round(mean(rear_wheel$hwy),2)
round(sd(rear_wheel$hwy),2)

#creating the 4wd wheel subset 
fwd <- mpg %>% filter(drv == "4") %>% select(hwy,drv)

#find mean and SD in 2 decimal places
round(mean(fwd$hwy),2)
round(sd(fwd$hwy),2)

#boplot of city fuel economy (cty) across of cylinders (cyl)
bplot <- ggplot(data = mpg, mapping = aes(x= as.factor(cyl), y= cty)) + geom_boxplot() 
bplot

#Adding title to the graph
finalbplot <- bplot + ggtitle("City fuel econony across cylinders")
finalbplot

#Boxplot of cty vs class vs cyl
bplotcty <- ggplot(data = mpg, mapping = aes(y =cty, x = class, fill = as.factor(cyl))) + geom_boxplot() + ggtitle("City fuel efficiency for each class and cylinders")
bplotcty

#Boxplot of hwy vs class vs cyl
bplothwy <- ggplot(data = mpg, mapping = aes(y =hwy, x = class, fill = as.factor(cyl))) +geom_boxplot() + ggtitle("Highway fuel efficiency for each class and cylinders")
bplothwy


#scatterplot for cty vs disp vs class 
scattercty <- ggplot(data =mpg, mapping = aes(x = displ, y = cty, colour = class)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + ggtitle("Comparison of city economy, displacemnet across car classes")
scattercty

#scatterplot for hwy vs disp vs class
scatterhwy <- ggplot(data =mpg, mapping = aes(x = displ, y = hwy, colour = class)) + geom_point()+ geom_smooth(method = "lm", se = FALSE) + ggtitle("Comparison of highway economy, displacemnet across car classes")
scatterhwy





