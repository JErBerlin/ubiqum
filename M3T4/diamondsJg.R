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
#     - classify in two rounds
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
### exploratory - preliminar
# hist(trData$price)
# trData$rdPrice <- as.integer(round(log(trData$price)*2-10))
trData$rdPrice <- as.integer(round(log(trData$price)*25))
# trData$rdPrice <- as.factor(trData$rdPrice)
# hist(trData$rdPrice)
# str(trData$rdPrice)


# model training ####
# ___ model: KNN classification (?)
set.seed(1111)

### training control:
#### none
trControl <- trainControl(method="none")

#### n-fold cross-validation
# trControl <- trainControl(method = "cv", number = 5) # training control: 5-fold CV
# tuneGrid <- expand.grid(k = c(1,3,5)) # grid <- expand.grid(k = c(1,2,3,4,5))

#### model parameters
tuneGrid <- data.frame(k=5) #### model KNN: k=1

### split test and training set: index intrain
intrain<-createDataPartition(y=trData$price,p=0.95,list=FALSE)

### split test and training set: trainData + testData 
trainData <- trData[intrain,]
testData  <- trData[-intrain,]

### train model: knn(price ~ .), data = trainData
start_time <- Sys.time()
priceKNN <- train(
  price ~ rdPrice, 
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

## compute errors ####
postResample(trData.p$predPrice, trData.p$price)

diffPrice <- abs(trData.p$price - trData.p$predPrice)

hist(diffPrice, breaks=100, main='KNN(Price ~ .), k=1, use all vars AND rdPreis')

print("KNN(Price ~ .), k=1, use all vars AND rdPreis")

mean(diffPrice)
median(diffPrice)
max(diffPrice)
quantile(diffPrice, 0.75)
quantile(diffPrice, 0.90)
quantile(diffPrice, 0.95)
quantile(diffPrice, 0.99)


# results ####
