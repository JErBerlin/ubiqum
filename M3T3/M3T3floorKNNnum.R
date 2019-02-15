### M3T3floorKNNnum.R: 
#
# - exclude variables with too low variability
# - exclude correlated observations
# - modell
#
####

# load libraries ####
library(dplyr)
library(caret)

library(reshape2)
library(ggplot2)
library(raster) 

library(beepr)

# read data ####
## training and test set
trData = read.csv(
  file ="trainingData.csv", 
  header=T)

## validation set
vlData = read.csv(
  file ="validationData.csv", 
  header=T)

# pre-process data ####
## aux dataframes
### cols corresponding to waps are 1:520
trData.waps <- trData[,1:520]

# -0- # 
# _____to use for prediction
## replace 100's (no signal) 
## replace 100 by -110 in the main dataframe
trData[trData==100]<--110

vlData[vlData==100]<--110

# -0.1- # 
# _____type conversion of target variable (rounding)
## convert vars LATITUDE and LONGITUDE to integers
trData$LATITUDE  <- as.integer(trData$LATITUDE)
trData$LONGITUDE <- as.integer(trData$LONGITUDE)

vlData$LATITUDE  <- as.integer(vlData$LATITUDE)
vlData$LONGITUDE <- as.integer(vlData$LONGITUDE)

# -1- # 
# _____clean of empty rows and cols #
## keep only WASP cols that have NOT all values 100 (no signal) 
## -- get rid of the cols with no information: 55 cols
### make an index
ind.1 <- sapply(trData.waps, min) < 100
### select cols after index
trData.waps.1 <- trData.waps[,ind.1]
### drop the corresponding cols of the main dataframe:
### use the index of waps df (but add 9 extra cols TRUE at the end)
trData.1 <- trData[,c(ind.1,rep(TRUE,9))]

vlData.1 <- vlData[,c(ind.1,rep(TRUE,9))]

## keep only Measurement rows that have NOT all values 100 (no signal) 
## -- get rid of the rows with no information: 76
indR.1 <- apply(trData.waps,1, min) < 100

### select rows after index
trData.waps.1 <- trData.waps.1[indR.1,]
### drop the corresponding rows of the main dataframe: (76 rows)
### use the same index of waps rows
trData.1 <- trData.1[indR.1,]

# -2- #
# _____replace 100's (no signal) #
## replace 100 (no signal detected from wap) by NA to complete computations
trData.waps.2 <- trData.waps.1
trData.waps.2[trData.waps.1==100]<-NA
# replace 100 by -110 in the main dataframe --> done in step 0

# -3- #
## compute variance by variable
# varCol <- apply(trData.waps.2,2,var,na.rm=T)
# hist(varCol,breaks=175)
# sum(varCol < 75, na.rm=T) ## 212 (23 NA)

## keep only WAPS cols that have var > 75 -> varCut
### make an index
varCut <- 75
ind.3 <- sapply(trData.waps.2,var,na.rm=T) > varCut
ind.3[is.na(ind.3)] <- FALSE  ## Don't count NAs
### select cols after index
trData.waps.3 <- trData.waps.2[,ind.3]
### drop the corresponding cols of the main dataframe
trData.2 <- trData.1[,c(ind.3,rep(TRUE,9))]

vlData.2 <- vlData.1[,c(ind.3,rep(TRUE,9))]

# -3 bis- #
###drop the rows that are equal (f.ex. all NA)
trData.waps.3.dist <- distinct(trData.waps.3)

### drop the corresponding rows of the main dataframe ##
l <- length(trData.2)
ind <- duplicated(trData.2[,1:(l-9)])
trData.3 <- trData.2[!ind,]

### prepare next steps (shorten names)
trData.waps.3 <- trData.waps.3.dist
# dim(trData.3)
# dim(vlData.2)

# model training ####
# ___ modell: KNN
set.seed(1111)

## ALL target vars

### model parameters
# k = 20 ## KNN

# training control: 10-fold cross-validation
# trControl <- trainControl(method = "cv", number = 20) # training control: 3-fold cross-validation
# tuneGrid <- expand.grid(k = c(10))
trControl <- trainControl(method="none")
tuneGrid <- data.frame(k=3)

## FLOOR
l <- length(trData.3)
### copy waps data
trData.3m <- trData.3[,1:(l-9)]

### copy floor as integer
trData.3m$FLOOR <- as.integer(trData.3$FLOOR)
# names(trData.3m)[l-9+1]

#### convert integer to factor
# trData.3m$BID_FL <- as.factor(trData.3m$BID_FL)

### split test and training set: index intrain
intrain<-createDataPartition(y=trData.3m$FLOOR ,p=0.95,list=FALSE)

### split test and training set: trainData + testData 
trainData.3m <-trData.3m[intrain,]
testData.3m <-trData.3m[-intrain,]

### train model: knn(BID.FL ~ .), data = trData.3m
start_time <- Sys.time()
FloorKNN <- train(
  FLOOR ~ .,
  data = trainData.3m,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)
end_time <- Sys.time()
print(end_time - start_time)
beep();
# system("firefox https://www.youtube.com/watch?v=QDKBDduuJ_0")

## explore results of the KNN
print(FloorKNN)

## prediction: trData.3p  ####

### prediction on train data
# trData.3p <- trainData.3m

### prediction on test data
# trData.3p <- testData.3m

### prediction on validation data 
trData.3p <- vlData.2

### compute prediction

#### Floor
start_time <- Sys.time()
trData.3p$predFloor <- as.integer(round(predict(FloorKNN, trData.3p)))
end_time <- Sys.time()
print(end_time - start_time)
beep();
system("firefox https://www.youtube.com/watch?v=QDKBDduuJ_0")

## compute errors
postResample(trData.3p$predFloor, trData.3p$FLOOR)

diff <- as.numeric(sum(trData.3p$predFloor != trData.3p$FLOOR))
percDiff <- diff/length(trData.3p$predFloor)*100
percDiff

## plot errors
trData.3p$errFloor <- trData.3p$predFloor != trData.3p$FLOOR