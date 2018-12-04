# dataframe = Responses
# y-value = brand

# initializations (uncoment by convinience)
## library(caret)
## library(ggplot2)

set.seed(111)

# load data
setwd("C:/Users/jeron/ubiqumR")
Responses <- read.csv('CompleteResponses.csv')

# preprocessing data ####
# select columns
Responses <- Responses[, which(names(Responses) %in% c("brand","salary","age")) ]

# transform y to factor levels
Responses$brand <- factor(Responses$brand, ordered = FALSE)

# factorize / discretize variables
#Responses$age <- cut(Responses$age, breaks = 3)
#Responses$salary <- cut(Responses$salary, breaks = 6)

# splitting dataset 75%/25% train/test ####
inTrain <- createDataPartition( y = Responses$brand, 
                                p=0.75,
                                list = FALSE )
trainingSet <- Responses[inTrain,]
testingSet <- Responses[-inTrain,]

# define fit/train control: 10 fold cross validation ####
trControl <- trainControl(method = "cv", number = 10)

## for repeated cross-validation, uncomment
## trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# train model: knn ####
tic() ## compute elapsed time in training
knnFit <- train(
  brand ~ .,
  data = trainingSet,
  method = "knn",
  preProc = c("center", "scale"),
  tuneLength = 100,
  na.action = na.exclude,
  trControl = trControl
)
toc()

# show performance & make predictions ####
testPredKnn <- predict(knnFit, testingSet) 

#performace measurement and other model info
postResample(testPredKnn, testingSet$brand)
confusionMatrix(data = testPredKnn, testingSet$brand)

# train alternative model ####

# compare models ####
###resamps <- resamples(list(xxx = xxxFit, knn = knnFit))

# plot stats summary and graph
###summary(resamps)
###xyplot(resamps, what = "BlandAltman")




