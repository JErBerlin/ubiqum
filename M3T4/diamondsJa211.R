############################################## diamondsJa.R ###
#
# approach A2: 
#     - preprocess: table as integer
#
#     - model LM regression (mixed factors + numbers)
#     -- use all vars but id
#     -- discard all vars but carat, x, y, z
#     -- transform target var to log10
#
##############################################

# load libraries ####
library(dplyr)
library(caret)

library(ggplot2)
library(raster) 

library(beepr)

# read data ####
## training and test set
trData.id = read.csv2(
  file ="train.csv", 
  header=T)

## validation set
vlData.id = read.csv2(
  file ="validation.csv", 
  header=T)

# exploratory ####
###exploratory
# qplot(trData$carat, log10(trData$price)) ## useful
# qplot(trData$cut, trData$price) ## not useful in any class
# qplot(trData$color, trData$price) ## not useful in any class
# qplot(trData$clarity, trData$price) ## not usefull in any class
# qplot(trData$table, trData$price) ## not useful for most of classes
# qplot(trData$depth, trData$price) ## probably not useful
# qplot(trData$x, trData$price) ## probably useful -- apply log10 to price or exp to x? yes!
# qplot(trData$y, trData$price) ## scale problem + outliers -- apply log10?
# qplot(trData$z, trData$price) ## scale problem + outliers -- apply log10?

# pre-process ####

## make df w/o id 
l <- length(trData.id)
trData <- trData.id[,2:l]

l <- length(vlData.id)
vlData <- vlData.id[,2:l]

## drop not useful vars
l <- length(trData)
trData <- trData[,c(1,(l-3),(l-2),(l-1),l)]

## transform target var to log10
trData$priceLog <- log10(trData$price)

### do the same for validation set
l <- length(vlData)
vlData <- vlData[,c(1,(l-2),(l-1),l)]

# model training ####
# ___ model: LM classification (?)
set.seed(1111)

### training control:
#### none
trControl <- trainControl(method="none")

#### n-fold cross-validation
# trControl <- trainControl(method = "cv", number = 10) # training control: 5-fold CV
# tuneGrid <- expand.grid(k = c(3,4,5,6,7,8,9,10,11,12,13,14,15)) # grid <- expand.grid(k = c(1,2,3,4,5))

#### model parameters
tuneGrid <- data.frame() #### model LM: 

### split test and training set: index intrain
intrain<-createDataPartition(y=trData$price,p=0.95,list=FALSE)

### split test and training set: trainData + testData 
trainData <- trData[intrain,]
trainData$price <- NULL
testData  <- trData[-intrain,]

### train model: lm(price ~ .), data = trainData
priceLogLM <- train(
  priceLog ~ ., 
  data = trainData,
  method = "lm",
  trControl = trControl #,
  # tuneGrid = tuneGrid
)

# model evaluation ####

## explore results of the model
# print(priceLM)

### prediction on train data
# trData.p <- trainData

### prediction on test data
trData.p <- testData

### prediction on validation data 
# trData.p <- vlData

## Predict: price
trData.p$predPriceLog <- predict(priceLogLM, trData.p)

### compute predPrice (anti log)
trData.p$predPrice <- 10^(trData.p$predPriceLog)

## compute errors ####
postResample(trData.p$predPriceLog, trData.p$priceLog)
postResample(trData.p$predPrice, trData.p$price)

diffPrice <- abs(trData.p$price - trData.p$predPrice)

hist(diffPrice, breaks=100, main='LM(Price ~ .), , use all vars but id')

# print("LM(Price ~ .), , use all vars but id")
# 
# mean(diffPrice)
# median(diffPrice)
# max(diffPrice)
# quantile(diffPrice, 0.75)
# quantile(diffPrice, 0.90)
# quantile(diffPrice, 0.95)
# quantile(diffPrice, 0.99)


# results ####
# > postResample(trData.p$predPrice, trData.p$price)
# RMSE     Rsquared          MAE 
# 1481.7785648    0.8606377  828.7060014 