# what the program does: ####
# explore data and visualisation

# load libraries ####
library(ggplot2)
theme_set(theme_bw())


# initializations ####
# setwd("C:/Users/jeron/ubiqumR/M1T1")

# load data ####

Responses <- read.csv('../CompleteResponses.csv')

# transform y and some x vars to factor levels ####
Responses$brand <- factor(Responses$brand, ordered = FALSE)
# Responses$age <- cut(Responses$age, breaks = 30, labels=1:30)
# Responses$salary <- cut(Responses$salary, breaks = 50, labels = 1:50)
Responses$elevel <- factor(Responses$elevel, ordered = FALSE)
# below: to include the labels with salary instead of levels
# Responses$salary <- cut(Responses$salary, breaks = 20, labels=1:20)

# visualisations with ggplot ####
# Scatterplot
qplot(salary, age, data=Responses, shape = elevel, color = brand)

# gg <- ggplot(Responses, aes(x=salary, y=brand))

# plot(gg)

