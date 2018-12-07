# what the program does: ####
# explore data and visualisation

# load libraries ####
library(ggplot2)


# initializations ####
# setwd("C:/Users/jeron/ubiqumR/M1T1")

# load data ####

Responses <- read.csv('../CompleteResponses.csv')

# transform y and some x vars to factor levels ####
# partition of the two relevant variables: age, salary
# we let y-var as numeric to make to linear model
Responses$age <- cut(Responses$age, breaks = 3)
#Responses$salary <- cut(Responses$salary, breaks = 50, labels = 1:50)
# below: to include the labels with salary instead of levels
#Responses$salary <- cut(Responses$salary, breaks = 20, dig.lab = 6)

# visualisations with ggplot ####
# Scatterplot
gg <- ggplot(Responses, aes(x=salary, y=brand))

plot(gg)

