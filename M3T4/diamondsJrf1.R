############################################## diamondsJa.R ###
#
# approach A: 
#     - preprocess: table as integer
#
#     - model RF regression (mixed factors + numbers)
#     -- use all vars but id
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
vlData = read.csv2(
  file ="validation.csv", 
  header=T)

# pre-process ####

## make df w/o id 
l <- length(trData.id)
trData <- trData.id[,2:l]

## convert types of var
trData$table <- as.integer(round(trData$table))

### also for the validation set
vlData$table <- as.integer(round(vlData$table))

# model training ####
# ___ model: RF regression (?)
set.seed(1111)

### training control:
#### none
trControl <- trainControl(method="none")

#### n-fold cross-validation
# trControl <- trainControl(method = "cv", number = 10) # training control: 5-fold CV
tuneGrid <- expand.grid(mtry = c(23)) 

#### model parameters
# tuneGrid <- data.frame(mtry = 2) #### model RF: rf (mtry = 23)

### split test and training set: index intrain
intrain<-createDataPartition(y=trData$price,p=0.95,list=FALSE)

### split test and training set: trainData + testData 
trainData <- trData[intrain,]
testData  <- trData[-intrain,]

### train model: rf(price ~ .), data = trainData
start_time <- Sys.time()
priceRF <- train(
  price ~ ., 
  data = trainData,
  method = "rf",
  trControl = trControl,
  tuneGrid = tuneGrid
)
end_time <- Sys.time()
print(end_time - start_time)
beep()

# model evaluation ####

## explore results of the model
print(priceRF)

### prediction on train data
# trData.p <- trainData

### prediction on test data
trData.p <- testData

### prediction on validation data 
# trData.p <- vlData

## Predict: price

start_time <- Sys.time()
trData.p$predPrice <- predict(priceRF, trData.p)
end_time <- Sys.time()
print(end_time - start_time)
beep();

## compute errors ####
postResample(trData.p$predPrice, trData.p$price)

diffPrice <- abs(trData.p$price - trData.p$predPrice)

hist(diffPrice, breaks=100, main='RF(Price ~ .), rf, use all vars but id')

# print("RF(Price ~ .), rf, use all vars but id")
# 
# mean(diffPrice)
# median(diffPrice)
# max(diffPrice)
# quantile(diffPrice, 0.75)
# quantile(diffPrice, 0.90)
# quantile(diffPrice, 0.95)
# quantile(diffPrice, 0.99)

# results ####
## mtry = default (intrain p=0.95)
# > postResample(trData.p$predPrice, trData.p$price)
# RMSE    Rsquared         MAE 
# 692.8950775   0.9702316 335.8900439 
## mtry = 23 (but intrain p = 0.5 because CV folds=10)
# mtry  RMSE       Rsquared   MAE     
# 2    1160.1536  0.9341262  688.0857
# 12     832.3720  0.9579447  398.5829
# 23     830.8906  0.9579497  391.2994