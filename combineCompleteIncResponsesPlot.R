# what the program does: ####
# explore data and visualisation from predicted brand from data in SurveyIncomplete.csv 
# (modelled by knn using data in CompleteResponses.csv)

# load libraries ####
library(caret)
library(ggplot2)

# initializations ####
setwd("C:/Users/jeron/ubiqumR") # set working directory, where the csv files are
theme_set(theme_bw())           # set theme black&white background for plots  
set.seed(111)                   # initialize random seed for reproducibility

# load data ####
Responses <- read.csv('CompleteResponses.csv')   # complete survey
incResponses <- read.csv('SurveyIncomplete.csv') # incomplete survey

# preprocessing data ####
# select columns
Responses <- Responses[, which(names(Responses) %in% c("brand","salary","age")) ]
incResponses <- incResponses[, which(names(incResponses) %in% c("brand","salary","age"))]

# transform y to factor levels
Responses$brand <- factor(Responses$brand, ordered = FALSE, labels = c('Acer','Sony'))
incResponses$brand <- factor(incResponses$brand, ordered = FALSE)

# splitting dataset 75%/25% train/test ####
inTrain <- createDataPartition( y = Responses$brand, 
                                p=0.75,
                                list = FALSE )
trainingSet <- Responses[inTrain,]
testingSet <- Responses[-inTrain,]

# define fit/train control: 10 fold cross validation ####
trControl <- trainControl(method = "cv", number = 10)

# train model: knn ####
knnFit <- train(
  brand ~ .,
  data = trainingSet,
  method = "knn",
  preProc = c("center", "scale"),
  tuneLength = 10,
  trControl = trControl
)

# apply model to incomplete responses
incResponses$brand <- predict(knnFit, incResponses)

# create matrix with all responses (complete and predicted) by binding matrices vertically
allResponses <- rbind(Responses, incResponses)

# visualisations ####
# bar diagram of brand preference 
# qplot(brand, data=allResponses, geom = 'bar', color=brand)

# Brand relative preference for both surveys
# plotAllResponseBrandRelPreference <- ggplot(allResponses, aes(brand), color=brand) + 
#   geom_bar(aes(y = (..count..)/sum(..count..))) + 
#   scale_y_continuous(labels=scales::percent) +
#   ggtitle("Brand preference for both surveys") +
#   ylab("relative preference") 
# 
# plotAllResponseBrandRelPreference

# boxplot brand vs age
qplot(age, brand, data=allResponses, geom = 'boxplot', color = brand)

# boxplot brand vs salary
qplot(salary, brand, data=allResponses, geom = 'boxplot', color = brand)

# Scatterplot salary vs age with brand coding point
qplot(salary, age, data=allResponses, shape = brand, color = brand)





