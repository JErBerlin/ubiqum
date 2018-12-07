# what the program does: ####
# explore data and visualisation

# load libraries ####
library(ggplot2)
library(caret)

# initializations ####
theme_set(theme_bw())
set.seed(111)


# load data ####
Responses <- read.csv('CompleteResponses.csv')   # complete survey
incResponses <- read.csv('SurveyIncomplete.csv') # incomplete survey

# preprocessing data ####
# select columns
Responses <- Responses[, which(names(Responses) %in% c("brand","salary","age")) ]
incResponses <- incResponses[, which(names(incResponses) %in% c("brand","salary","age"))]

# transform y to factor levels
Responses$brand <- factor(Responses$brand, ordered = FALSE)
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
  na.action = na.exclude,
  trControl = trControl
)

# apply model to incomplete responses
incResponses$brand <- predict(knnFit, incResponses)

# visualisations with ggplot ####
# Scatterplot salary vs age with elevel coding point
qplot(salary, age, data=Responses, shape = brand, color = brand)

# Scatterplot salary vs age with elevel coding point
qplot(salary, age, data=incResponses, shape = brand, color = brand)


