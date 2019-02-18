#### M3T3model3.R
#
# - exclude variables with too low variability
#
# - standardize RSSI by rows:
#   -- select 3 highest values
#   -- standardize
#
# - model position (latitude, longitude) as numeric (integers)
# - check with validation set
# - correction: prediction exclusively on waps
#
#   Obs: originally approach D
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

# -4- #
## __ select 3 highest RSSI values by row 
### write the 3 top values in 3 extra cols at the end of the DF -> trData.waps.4
max3byRow <- rbind.data.frame(apply(trData.waps.3, 1, function(x) cbind(x[order(x, decreasing=TRUE)[1:3]])))
trData.waps.4 <- cbind(trData.waps.3, t(max3byRow))

#### replicate procedure for validation set
##### preprocess
l <- length(vlData.2)
vlData.waps.2 <- vlData.2[,1:(l-9)]
vlData.waps.2[vlData.waps.2==-110]<-NA

max3byRow <- rbind.data.frame(apply(vlData.waps.2, 1, function(x) cbind(x[order(x, decreasing=TRUE)[1:3]])))
vlData.waps.4 <- cbind(vlData.waps.2, t(max3byRow))

### keep only the 3 top values per row, else write NA -> trData.waps.5
#### define the aux function f1 to operate on a row
  f1 <- function(x) {l <- length(x); ind <- (x[1:(l-5)] %in% x[(l-4):l]); x[!ind] <- NA; return(x)}
trData.waps.4 <- apply(trData.waps.4, 1, f1)

#### correct the format of the output of apply(): transpose and cast as DF
trData.waps.4 <- as.data.frame(t(trData.waps.4))

#### drop last 3 auxiliary cols 
  l <- length(trData.waps.4)
trData.waps.4 <- trData.waps.4[,1:(l-5)]

# varRow4 <- apply(trData.waps.4, 1, var,na.rm=T)
# sdRow4 <- apply(trData.waps.4, 1, sd,na.rm=T)
# meanRow4 <- apply(trData.waps.4, 1, mean,na.rm=T)

#### replicate procedure for validation set
vlData.waps.4 <- apply(vlData.waps.4, 1, f1)
vlData.waps.4 <- as.data.frame(t(vlData.waps.4))
l <- length(vlData.waps.4)
vlData.waps.4 <- vlData.waps.4[,1:(l-5)]

# -5- #
## __ convert to positive value and standardize by rows
# ### positivate: take abs value of every row
# trData.waps.5 <- t(apply(trData.waps.4, 1, abs))

### positivate: add 100 to every value
trData.waps.5 <- trData.waps.4 + 100

### replace NAs by 0
trData.waps.5[is.na(trData.waps.5)] <- 0

### divide by maximum of row and mult by 100 (so that all values are in [0,100])
  f2 <- function(x) {
    M <-max(x,na.rm=T); 
    return(as.integer(round(x/M,2)*100)); 
  }
trData.waps.5 <- as.data.frame(t(apply(trData.waps.5, 1, f2)))  
colnames(trData.waps.5) <- colnames(trData.waps.4)

#### replicate procedure for validation set
vlData.waps.5 <- vlData.waps.4 + 100
vlData.waps.5[is.na(vlData.waps.5)] <- 0
vlData.waps.5 <- as.data.frame(t(apply(vlData.waps.5, 1, f2)))
colnames(vlData.waps.5) <- colnames(vlData.waps.4)

### reconstruct with rest of vars (no WAPs)
l <- length(trData.3)
trData.5 <- cbind(trData.waps.5, trData.3[,(l-8):l])
rownames(trData.5)<- rownames(trData.3)

l <- length(vlData.2)
vlData.5 <- cbind(vlData.waps.5, vlData.2[,(l-8):l])
rownames(vlData.5)<- rownames(vlData.2)

### rename (compatibility)
vlData.2 <- vlData.5
trData.3 <- trData.5

# model training ####
# ___ model: KNN
set.seed(1112)

### training control:
#### none
trControl <- trainControl(method="none")

#### n-fold cross-validation
# trControl <- trainControl(method = "cv", number = 5) # training control: 5-fold CV
# tuneGrid <- expand.grid(k = c(1,3,5,10)) # grid <- expand.grid(k = c(1,2,3,4,5))

#### model parameters
tuneGrid <- data.frame(k=3) #### model KNN: k=3

### training data: trData.3
l <- length(trData.3)
trData.3m <- trData.3[,1:(l-9)]
trData.3m <- cbind(trData.3m, trData.3$LATITUDE, trData.3$LONGITUDE)
names(trData.3m)[l-9+1] <- "LATITUDE"
names(trData.3m)[l-9+2] <- "LONGITUDE"

### split test and training set: index intrain
# intrain<-createDataPartition(y=trData.3$LATITUDE,p=0.95,list=FALSE)

## LATITUDE 
trData.3mLat <- trData.3[,1:(l-9)]
trData.3mLat <- cbind(trData.3mLat, trData.3$LATITUDE)
names(trData.3mLat)[l-9+1] <- "LATITUDE"

### split test and training set: trainData + testData 
trainData.3mLat <-trData.3mLat # trData.3mLat[intrain,]
# testData.3mLat <-trData.3mLat[-intrain,]

### train model: knn(LATITUDE ~ .), data = trData.3m 
start_time <- Sys.time()
LatKNN <- train(
  LATITUDE ~ ., 
  data = trainData.3mLat,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid,
  na.action=na.exclude
)
end_time <- Sys.time()
print(end_time - start_time)
beep();

## LONGITUDE
trData.3mLon <- trData.3[,1:(l-9)]
trData.3mLon <- cbind(trData.3mLon, trData.3$LONGITUDE)
names(trData.3mLon)[l-9+1] <- "LONGITUDE"

### split test and training set: trainData + testData 
trainData.3mLon <-trData.3mLon # trData.3mLon[intrain,]
# testData.3mLon <-trData.3mLon[-intrain,]

### train model: knn(LONGITUDE ~ .), data = trData.3m 
start_time <- Sys.time()
LonKNN <- train(
  LONGITUDE ~ ., 
  data = trainData.3mLon,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid,
  na.action=na.exclude
)
end_time <- Sys.time()
print(end_time - start_time)
beep();

# model evaluation ####

## explore results of the model
# print(LatKNN)
print(LonKNN)

## prediction: trData.3p

### prediction on train data
# trData.3pLat <- trainData.3mLat
# trData.3pLon <- trainData.3mLon

### prediction on test data
# trData.3p <- testData.3m
# trData.3pLat <- testData.3mLat
# trData.3pLon <- testData.3mLon

### prediction on validation data 
# trData.3p <- vlData.2
trData.3pLat <- vlData.2
trData.3pLon <- vlData.2

## Lat: trData.3pLat

start_time <- Sys.time()
trData.3pLat$predLat <- predict(LatKNN, trData.3pLat)
end_time <- Sys.time()
print(end_time - start_time)
beep();

## Lon: trData.3pLon

start_time <- Sys.time()
trData.3pLon$predLon <- predict(LonKNN, trData.3pLon)
end_time <- Sys.time()
print(end_time - start_time)
beep();

## compute errors
postResample(trData.3pLat$predLat, trData.3pLat$LATITUDE)
postResample(trData.3pLon$predLon, trData.3pLon$LONGITUDE)

diffLAT <- abs(trData.3pLat$LATITUDE - trData.3pLat$predLat)
diffLON <- abs(trData.3pLon$LONGITUDE - trData.3pLon$predLon)

diffEUC <- sqrt(diffLON^2 + diffLAT^2)
hist(diffEUC, breaks=90, main='KNN k=3, top 3 RSSI, varCut=75')

print("Metrics for KNN, 3 RSSI")

mean(diffEUC)
median(diffEUC)
max(diffEUC)

quantile(diffEUC, 0.75)
quantile(diffEUC, 0.90)
quantile(diffEUC, 0.95)
quantile(diffEUC, 0.99)

# plotting results ####

### write euclidian dist errors as a new col in the df prediction
vlData.p <- cbind(vlData.p, diffEUC)

### classificate errors in 4 classes: <5, <10, <20, > 20 error
### write as a new col in the df prediction
getEUCclass <- function(x) {
  if(x < 5) return(as.integer(5))
  if(x < 10) return(as.integer(10));
  if(x < 20) return(as.integer(20));
  if(x >= 20) return(as.integer(40));
  return(NA);
}
# clEUC <- sapply(diffEUC, getEUCclass)
# vlData.p <- cbind(vlData.p, diffEUC)
vlData.p <- vlData.p %>% mutate(clErr = sapply(diffEUC, getEUCclass))

qplot(LATITUDE, predLat, data=vlData.p, color = clErr)
qplot(LONGITUDE, predLon, data=vlData.p, color = clErr)

qplot(predLat, predLon, data=vlData.p, color = clErr)
qplot(LATITUDE, LONGITUDE, data=vlData.p, color = clErr)

# plot(predLat ~ predLon, col="red", data=vlData.p, pch=16)
# points(vlData.p$LATITUDE ~ vlData.p$LONGITUDE, col = "black", pch=4)

############################ tools ####
#### aux function
# f2 <- function(x) {
#         if(any(!is.na(x))) { 
#           M <-max(x,na.rm=T); 
#           return(as.integer(round(x/M,2)*100)); 
#         } 
#         return(x)
# }

hist(vlData.2$PHONEID,breaks=25, xlim = c(1,25))
hist(trData.3$PHONEID,breaks=25, xlim = c(1,25))

hist(vlData.2$BUILDINGID, xlim = c(0,2))
hist(trData.3$BUILDINGID, xlim = c(0,2))

hist(vlData.2$FLOOR,breaks=4, xlim = c(0,4))
hist(trData.3$FLOOR,breaks=4, xlim = c(0,4))

### cols corresponding to waps are 1:520
l <- length(vlData.2)
vlData.2.waps <- vlData.2[,1:(l-9)]
l <- length(trData.3)
trData.3.waps <- trData.3[,1:(l-9)]

plot(raster(as.matrix(vlData.2.waps)),main="vlData.2.waps", useRaster=F)
plot(raster(as.matrix(trData.3.waps)),main="trData.3.waps", useRaster=F)


### function
getEUCclass <- function(x) {
  if(x < 5) return(as.integer(5))
  return(NA);
}
### apply function, write new col with computed values
vlData.p <- vlData.p %>% mutate(clErr = sapply(diffEUC, getEUCclass))

############################ results ####
# > postResample(trData.3pLat$predLat, trData.3pLat$LATITUDE)
# RMSE  Rsquared       MAE 
# 9.1631882 0.9830302 5.8223244 
# > postResample(trData.3pLon$predLon, trData.3pLon$LONGITUDE)
# RMSE  Rsquared       MAE 
# 9.2829195 0.9940455 6.1073339 
# > 
#   > diffLAT <- abs(trData.3pLat$LATITUDE - trData.3pLat$predLat)
# > diffLON <- abs(trData.3pLon$LONGITUDE - trData.3pLon$predLon)
# > 
#   > diffEUC <- sqrt(diffLON^2 + diffLAT^2)
# > hist(diffEUC, breaks=90, main='KNN top 3 RSSI, varCut=75')
# > 
#   > print("Metrics for KNN, 3 RSSI")
# [1] "Metrics for KNN k=3, 3 RSSI"
# > 
#   > mean(diffEUC)
# [1] 9.356384
# > median(diffEUC)
# [1] 6.863753
# > max(diffEUC)
# [1] 90.35486
# > 
#   > quantile(diffEUC, 0.75)
# 75% 
# 12.44544 
# > quantile(diffEUC, 0.90)
# 90% 
# 19.84943 
# > quantile(diffEUC, 0.95)
# 95% 
# 25.30253 
# > quantile(diffEUC, 0.99)
# 99% 
# 42.60259 