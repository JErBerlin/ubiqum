# what the program does: ####
# writes the result in file SurveyCompleted.csv of predicting brand from data 
# in SurveyIncomplete.csv and data in CompleteResponses.csv binding the data frames
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
# select relevant columns and make a reduced DF
ResponsesRed <- Responses[, which(names(Responses) %in% c("brand","salary","age")) ]
incResponsesRed <- incResponses[, which(names(incResponses) %in% c("brand","salary","age"))]

# transform y to factor levels
ResponsesRed$brand <- factor(Responses$brand, ordered = FALSE)
incResponsesRed$brand <- factor(incResponses$brand, ordered = FALSE)

# splitting dataset 75%/25% train/test ####
inTrain <- createDataPartition( y = ResponsesRed$brand, 
                                p=0.75,
                                list = FALSE )
trainingSet <- ResponsesRed[inTrain,]
testingSet <- ResponsesRed[-inTrain,]

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
incResponses$brand <- predict(knnFit, incResponsesRed)

# create matrix with all responses (complete and predicted) by binding matrices vertically
allResponses <- rbind(Responses, incResponses)

# write allResponses (Data Frame with both complete and predicted survey) as .csv
write.csv(allResponses, "SurveyCompleted.csv", quote = FALSE, row.names = FALSE)

