#### approach B: 
#
# - exclude variables with too low variability
# - model position (latitude, longitude) as numeric (integers)
# - check with validation set
#
####

# load libraries ####
library(reshape2)
library(ggplot2)
library(dplyr)
library(caret)
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
# hist(varCol,breaks=100)
# sum(varCol < 100, na.rm=T) ## 237 (23 NA)
# sum(varCol < 50, na.rm=T) ## 195 (23 NA)
# sum(varCol < 25, na.rm=T) ## 151 (23 NA)
# sum(varCol < 10, na.rm=T) ## 80  (23 NA)

## keep only WAPS cols that have var > 100
### make an index
ind.3 <- sapply(trData.waps.2,var,na.rm=T) > 25
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
dim(trData.3)
dim(vlData.2)

# -5- #
# ___ model: KNN
set.seed(1111)

# model training ####

### training control:
#### none
# trControl <- trainControl(method="none")

#### n-fold cross-validation
trControl <- trainControl(method = "cv", number = 5) # training control: 5-fold CV
# tuneGrid <- expand.grid(k = c(1,3,10)) # grid <- expand.grid(k = c(1,2,3,4,5))

#### model parameters
tuneGrid <- data.frame(k=1) #### model KNN: k=1

## LATITUDE + LONGITUDE
l <- length(trData.3)
trData.3m <- trData.3[,1:(l-9)]
trData.3m <- cbind(trData.3m, trData.3$LONGITUDE, trData.3$LATITUDE)
names(trData.3m)[l-9+1] <- "LONGITUDE"
names(trData.3m)[l-9+2] <- "LATITUDE"

### split test and training set <-- skip
# intrain<-createDataPartition(y=trData.3m$LATITUDE,p=0.8,list=FALSE)
# trainData.3m <-trData.3m[intrain,]
# testData.3m <-trData.3m[-intrain,]
trainData.3m <-trData.3m

### train model: knn(LATITUDE ~ .), data = trData.3m 

start_time <- Sys.time()
LatKNN <- train(
  LATITUDE ~ ., 
  data = trainData.3m,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)
end_time <- Sys.time()
print(end_time - start_time)
beep();

### train model: knn(LONGITUDE ~ .), data = trData.3m 
start_time <- Sys.time()
LonKNN <- train(
  LONGITUDE ~ ., 
  data = trainData.3m,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)
end_time <- Sys.time()
print(end_time - start_time)
beep();

# model evaluation ####
 
## explore results of the model
print(LatKNN)
print(LonKNN)

## prediction
### prediction on training data
# trData.3p <- trData.3m
# trData.3p$predLat <- predict(LatLM, trData.3p)
# trData.3p$predLond <- predict(LonLM, trData.3p)

### prediction on validation data 

vlData.p <- vlData.2
# vlData.p <- trainData.3m
# vlData.p <- testData.3m

start_time <- Sys.time()
vlData.p$predLat <- predict(LatKNN, vlData.p)
end_time <- Sys.time()
print(end_time - start_time)
beep();

start_time <- Sys.time()
vlData.p$predLon <- predict(LonKNN, vlData.p)
end_time <- Sys.time()
print(end_time - start_time)
beep();

## compute errors

diffLON <- abs(vlData.p$LONGITUDE - vlData.p$predLon)
diffLAT <- abs(vlData.p$LATITUDE - vlData.p$predLat)

diffEUC <- sqrt(diffLON^2 + diffLAT^2)
hist(diffEUC, breaks=90, main='KNN k=1')

print("Metrics for KNN, k=1")

mean(diffEUC)
median(diffEUC)
max(diffEUC)

quantile(diffEUC, 0.75)
quantile(diffEUC, 0.90)
quantile(diffEUC, 0.95)
quantile(diffEUC, 0.99)

## plotting results
# plot(predLat ~ predLon, col="red", data=vlData.p, pch=16)
# points(vlData.p$LATITUDE ~ vlData.p$LONGITUDE, col = "black", pch=4)

############################ tools
