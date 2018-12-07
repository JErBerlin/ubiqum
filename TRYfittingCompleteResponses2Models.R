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

# fill NAs with column mean (uncomment if applies)
# replaceNAfMean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
# replace(Responses, TRUE, lapply(Responses, replaceNAfMean))

# transform y and some x vars to factor levels ####
Responses$brand <- factor(Responses$brand, ordered = FALSE)
# Responses$elevel <- factor(Responses$elevel, ordered = TRUE)
# Responses$car <- factor(Responses$car,ordered = FALSE)
# Responses$zipcode <- factor(Responses$zipcode, ordered = TRUE)
# Responses$age <- cut(Responses$age, breaks = 3)
# Responses$salary <- cut(Responses$salary, breaks = 10)
# Responses$credit <- cut(Responses$credit, breaks = 10)
# str(Responses)


# splitting dataset 75%/25% train/test ####
inTrain <- createDataPartition( y = Responses$brand, 
                                p=0.75,
                                list = FALSE )
trainingSet <- Responses[inTrain,]
testingSet <- Responses[-inTrain,]

# define fit/train control: 10 fold cross validation ####
trControl <- trainControl(method = "cv", number = 10)

# for repeated cross-validation, uncomment
# trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# train model: knn ####
tic()
knnFit <- train(
  brand ~ .,
  data = trainingSet,
  method = "knn",
  preProc = c("center", "scale"),
  tuneLength = 10,
  na.action = na.exclude,
  trControl = trControl
)
toc()

# show performance & make predictions ####
testPredKnn <- predict(knnFit, testingSet) 
testingSet$brandPred <- predict(knnFit, testingSet) 
# show predictor variables 
predictors(knnFit)

#performace measurement and other model info
postResample(testPredKnn, testingSet$brand)
knnProbs <- predict(knnFit, newdata = testingSet, type = "prob")
confusionMatrix(data = testPredKnn, testingSet$brand)

#plot predicted vs actual
# plot(testPredKnn,testingSet$brand)

# train alternative model ####

# compare models ####
###resamps <- resamples(list(xxx = xxxFit, knn = knnFit))

# plot stats summary and graph
###summary(resamps)
###xyplot(resamps, what = "BlandAltman")




