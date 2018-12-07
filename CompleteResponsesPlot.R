# what the program does: ####
# explore data and visualisation

# load libraries ####
library(ggplot2)
theme_set(theme_bw())


# initializations ####
# setwd("C:/Users/jeron/ubiqumR/")

# load data ####
Responses <- read.csv('CompleteResponses.csv')

# transform y and (optionally) some x vars to factor levels ####
Responses$brand <- factor(Responses$brand, ordered = FALSE, labels = c('Acer','Sony'))

# visualisations ####
# Brand relative preference for complete survey
plotResponseBrandRelPreference <- ggplot(Responses, aes(brand), color=brand) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Brand preference for complete survey") +
  ylab("relative preference")

plotResponseBrandRelPreference

# # boxplot brand vs age 
# qplot(age, brand, data=Responses, geom = 'boxplot', color = brand)
# 
# # boxplot brand vs salary
# qplot(salary, brand, data=Responses, geom = 'boxplot', color = brand)
# 
# # Scatterplot salary vs age with brand coding point
# qplot(salary, age, data=Responses, shape = brand, color = brand)


