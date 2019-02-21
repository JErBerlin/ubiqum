############################################## diamondsJ.R ###
#
# approach B: 
#     - preprocess: table as integer
#
#     - model KNN regression (mixed factors + numbers)
#     -- use only id
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

## make training df with only id
trData <- cbind(trData.id[,c(1,8)]) ## select cols id and price

## convert types of var
# trData$table <- as.integer(round(trData$table))

### also for the validation set
# vlData$table <- as.integer(round(vlData$table))

# model training ####
# ___ model: KNN classification (?)
set.seed(1111)

### training control:
#### none
trControl <- trainControl(method="none")

#### n-fold cross-validation
# trControl <- trainControl(method = "cv", number = 5) # training control: 5-fold CV
# tuneGrid <- expand.grid(k = c(1,3,10)) # grid <- expand.grid(k = c(1,2,3,4,5))

#### model parameters
tuneGrid <- data.frame(k=1) #### model KNN: k=1

### split test and training set: index intrain
intrain<-createDataPartition(y=trData$price,p=0.95,list=FALSE)

### split test and training set: trainData + testData 
trainData <- trData[intrain,]
testData  <- trData[-intrain,]

### train model: knn(price ~ .), data = trainData
start_time <- Sys.time()
priceKNN <- train(
  price ~ ., 
  data = trainData,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)
end_time <- Sys.time()
print(end_time - start_time)
beep()

# model evaluation ####

## explore results of the model
print(priceKNN)

### prediction on train data
# trData.p <- trainData

### prediction on test data
trData.p <- testData

### prediction on validation data 
# trData.p <- vlData

## Predict: price

start_time <- Sys.time()
trData.p$predPrice <- predict(priceKNN, trData.p)
end_time <- Sys.time()
print(end_time - start_time)
beep();

## compute errors
postResample(trData.p$predPrice, trData.p$price)

diffPrice <- abs(trData.p$price - trData.p$predPrice)

hist(diffPrice, main='KNN(Price ~ .), k=1, use only id')

print("KNN(Price ~ .), k=1, use only id")

mean(diffPrice)
median(diffPrice)
max(diffPrice)
quantile(diffPrice, 0.75)
quantile(diffPrice, 0.90)
quantile(diffPrice, 0.95)
quantile(diffPrice, 0.99)

# results
# > postResample(trData.p$predPrice, trData.p$price)
# RMSE    Rsquared         MAE 
# 360.5680412   0.9917519  23.5005568
# [1] "KNN(Price ~ .), k=1, use only id"
# > 
#   > mean(diffPrice)
# [1] 23.50056
# > median(diffPrice)
# [1] 0
# > max(diffPrice)
# [1] 10756
# > quantile(diffPrice, 0.75)
# 75% 
# 0.5 
# > quantile(diffPrice, 0.90)
# 90% 
# 1 
# > quantile(diffPrice, 0.95)
# 95% 
# 2 
# > quantile(diffPrice, 0.99)
# 99% 
# 141.575 
> 
