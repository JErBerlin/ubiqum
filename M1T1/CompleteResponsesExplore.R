# what the program does: ####
# explore data and visualisation from CompleteResponses

# load libraries ####
library(ggplot2)
theme_set(theme_bw())

# initializations ####
setwd("C:/Users/jeron/ubiqumR/M1T1")

# load data ####
Responses <- read.csv('../CompleteResponses.csv')

# transform y to factor levels ####
Responses$brand <- factor(Responses$brand, ordered = FALSE)

# visualisations with ggplot ####
# Scatterplot
qplot(salary, age, data=Responses, shape =brand, color = brand)


