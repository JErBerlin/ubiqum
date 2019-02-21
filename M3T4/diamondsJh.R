############################################## diamondsJb.R ###
#
# approach B: 
#     - preprocess: table as integer
#
#     - create new var: priceClass with log transform AND group the prices in ranges
#
#     - model KNN k=5 (1 or 5) regression (mixed factors + numbers)
#     -- use ONLY priceClass 
#
#     - predict in two rounds: predict priceClass --> predict price 
#      -- two models
#
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

## make df w/o id <-- deactivated
l <- length(trData.id)
trData <- trData.id[2:l]
# trData <- trData.id

## convert types of var
trData$table <- as.integer(round(trData$table))

### also for the validation set
vlData$table <- as.integer(round(vlData$table))

## create a new variable for range of price: priceClass
### transform scale with log and then round 
### multiply by a factor (25) to control number of classes (100)
### factor (50) --> classes (200)
# trData$classPrice <- as.integer(round(log(trData$price)*25)-145)
trData$classPrice <- as.integer(round(log(trData$price))-5)

# qplot(trData$classPrice,trData$price)
l <- length(trData)

trData1 <- trData[trData$classPrice==1,1:(l-1)]
intrain<-createDataPartition(y=trData1$price,p=0.95,list=FALSE)
trainData1 <- trData1[intrain,]
testData1  <- trData1[-intrain,]
# qplot(trData1$classPrice,trData1$price)
qplot(trData1$cut, trData1$price)

trData2 <- trData[trData$classPrice==2,1:(l-1)]
intrain<-createDataPartition(y=trData2$price,p=0.95,list=FALSE)
trainData2 <- trData2[intrain,]
testData2  <- trData2[-intrain,]
# qplot(trData2$classPrice,trData2$price)

trData3 <- trData[trData$classPrice==3,1:(l-1)]
intrain<-createDataPartition(y=trData3$price,p=0.95,list=FALSE)
trainData3 <- trData3[intrain,]
testData3  <- trData3[-intrain,]
# qplot(trData3$classPrice,trData3$price)

trData4 <- trData[trData$classPrice==4,1:(l-1)]
intrain<-createDataPartition(y=trData4$price,p=0.95,list=FALSE)
trainData4 <- trData4[intrain,]
testData4  <- trData4[-intrain,]
# qplot(trData4$classPrice,trData4$price)

trData5 <- trData[trData$classPrice==5,1:(l-1)]
intrain<-createDataPartition(y=trData5$price,p=0.95,list=FALSE)
trainData5 <- trData5[intrain,]
testData5  <- trData5[-intrain,]
# qplot(trData5$classPrice,trData5$price)



# training ####
set.seed(1111)

### training control:
#### none
trControl <- trainControl(method="none")

#### model parameters
tuneGrid <- data.frame(k=5) #### model KNN: k=5

## training model 1 ####
### train model: knn(price ~ .), data = trainData1 
start_time <- Sys.time()
priceKNN1 <- train(
  price ~ ., 
  data = trainData1,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)
end_time <- Sys.time()
print(end_time - start_time)
beep()

### predict
trData1.p <- testData1
trData1.p$predPrice <- predict(priceKNN1, testData1)

### evaluation & errors
postResample(trData1.p$predPrice, trData1.p$price)

## training model 2 ####
### train model: knn(price ~ .), data = trainData2 
priceKNN2 <- train(
  price ~ ., 
  data = trainData2,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)

### predict
trData2.p <- testData2
trData2.p$predPrice <- predict(priceKNN2, testData2)

### evaluation & errors
postResample(trData2.p$predPrice, trData2.p$price)

## training model 3 ####
### train model: knn(price ~ .), data = trainData3 
priceKNN3 <- train(
  price ~ ., 
  data = trainData3,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)

### predict
trData3.p <- testData3
trData3.p$predPrice <- predict(priceKNN3, testData3)

### evaluation & errors
postResample(trData3.p$predPrice, trData3.p$price)

## training model 4 ####
### train model: knn(price ~ .), data = trainData4 
priceKNN4 <- train(
  price ~ ., 
  data = trainData4,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)

### predict
trData4.p <- testData4
trData4.p$predPrice <- predict(priceKNN4, testData4)

### evaluation & errors
postResample(trData4.p$predPrice, trData4.p$price)

## training model 5 ####
### train model: knn(price ~ .), data = trainData5 
priceKNN5 <- train(
  price ~ ., 
  data = trainData5,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)

### predict
trData5.p <- testData5
trData5.p$predPrice <- predict(priceKNN5, testData5)

### evaluation & errors
postResample(trData5.p$predPrice, trData5.p$price)

# results ####
