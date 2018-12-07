# dataframe = Responses
# y-value = brand

# load libraries (uncomment by convinience) ####
library(caret)

# initializations ####
set.seed(111) # initialize random seed for reproducibility

# load data
setwd("C:/Users/jeron/ubiqumR")
Responses <- read.csv('CompleteResponses.csv')

# preprocessing data ####
# select columns
Responses <- Responses[, which(names(Responses) %in% c("brand","salary","age")) ]

# transform y to factor levels
Responses$brand <- factor(Responses$brand, ordered = FALSE)

# splitting dataset 75%/25% train/test ####
inTrain <- createDataPartition( y = Responses$brand, 
                                p=0.75,
                                list = FALSE )
trainingSet <- Responses[inTrain,]
testingSet <- Responses[-inTrain,]

# define fit/train control: 10 fold cross validation ####
trControl <- trainControl(method = "cv", number = 10)

# train model: random forest ####
rfFit <- train(
  brand ~ .,
  data = trainingSet,
  method = "rf",
  preProc = c("center", "scale"),
  trControl = trControl
)

# show performance & make predictions ####
testPredRf <- predict(rfFit, testingSet) 

#performace measurement and other model info
postResample(testPredRf, testingSet$brand)
confusionMatrix(data = testPredRf, testingSet$brand)






