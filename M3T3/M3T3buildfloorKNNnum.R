#### M3T3buildfloorKNNnum.R: 
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
k <- 3 ## KNN

# training control: 10-fold cross-validation
# trControl <- trainControl(method = "cv", number = 10) # training control: 10-fold cross-validation
# grid <- expand.grid(k = c(2,3,4,5))
trControl <- trainControl(method="none")
tuneGrid <- data.frame(k=3) 

## BUILDINGID
# trData.3m <- trData.3[,1:(l-9)]
# trData.3m <- cbind(trData.3m, as.integer(trData.3$BUILDINGID))
# names(trData.3m)[length(trData.3m)] <- "BUILDINGID"
# trData.3m$BUILDINGID = factor(trData.3m$BUILDINGID)

#### model parameters
# k <- 3 ## model KNN: k=3

# ### train model: knn(BUILDINGID ~ .), data = trData.3m 
# BuildingKNN <- train(
#   BUILDINGID ~ ., 
#   data = trData.3m,
#   method = "knn",
#   trControl = trControl,
#   tuneGrid=grid
# )
# beep();
# system("firefox https://www.youtube.com/watch?v=QDKBDduuJ_0")

## BUILDING & FLOOR
l <- length(trData.3)
trData.3m <- trData.3[,1:(l-9)]

### as numerical (integer) var
#### build composed integer from integers building and floor
trData.3m$BID_FL <- as.integer(trData.3$BUILDINGID)*10+as.integer(trData.3$FLOOR)
# names(trData.3m)[l-9+1]

#### convert integer to factor
# trData.3m$BID_FL <- as.factor(trData.3m$BID_FL)

### split test and training set: index intrain
intrain<-createDataPartition(y=trData.3m$BID_FL ,p=0.95,list=FALSE)

### split test and training set: trainData + testData 
trainData.3m <-trData.3m[intrain,]
testData.3m <-trData.3m[-intrain,]

### train model: knn(BID.FL ~ .), data = trData.3m
start_time <- Sys.time()
BuildFloorKNN <- train(
  BID_FL ~ .,
  data = trainData.3m,
  method = "knn",
  trControl = trControl,
  tuneGrid = tuneGrid
)
end_time <- Sys.time()
print(end_time - start_time)
beep();
# system("firefox https://www.youtube.com/watch?v=QDKBDduuJ_0")

# ## BUILDING & FLOOR & SPACE
# trData.3m <- trData.3[,1:(l-9)]
# 
# #### build composed integer from integers building and floor
# trData.3m$BFS <- as.integer(trData.3$BUILDINGID)*10000+as.integer(trData.3$FLOOR)*1000+as.integer(trData.3$SPACEID)
# #### convert integer to factor
# trData.3m$BFS <- as.factor(trData.3m$BFS)
# 
# ### train model: knn(BID.FL ~ .), data = trData.3m 
# BuildFloorSpaceKNN <- train(
#   BFS ~ ., 
#   data = trData.3m,
#   method = "knn",
#   trControl = trControl,
#   tuneGrid=grid
# )
# beep();
# system("firefox https://www.youtube.com/watch?v=QDKBDduuJ_0")

## explore results of the KNN
# print(BuildingKNN)
print(BuildFloorKNN)
# print(BuildFloorSpaceKNN)

## prediction: trData.3p  ####

### prediction on train data
# trData.3p <- trainData.3m

### prediction on test data
# trData.3p <- testData.3m
# trData.3p$BUILDINGID <- trData.3$BUILDINGID[-intrain]
# trData.3p$FLOOR <- trData.3$FLOOR[-intrain]


### prediction on validation data 
trData.3p <- vlData.2
#### carry same operations for validation data as were done for training data
trData.3p$BID_FL <- as.integer(trData.3p$BUILDINGID)*10+as.integer(trData.3p$FLOOR)

### compute prediction

#### Build + Floor
start_time <- Sys.time()
trData.3p$predBuildFloor <- as.integer(round(predict(BuildFloorKNN, trData.3p)))
end_time <- Sys.time()
print(end_time - start_time)
beep();

trData.3p$predBuild <- trData.3p$predBuildFloor %/% 10 ## integer division 5%/%2 is 2 
trData.3p$predFloor <- trData.3p$predBuildFloor %% 10  ## modulus (x mod y) 5%%2 is 1


#### Floor
# trData.3p$predFloor <- predict(FloorKNN, trData.3p)

## compute errors

postResample(trData.3p$predBuildFloor, trData.3p$BID_FL)
postResample(trData.3p$predBuild, trData.3p$BUILDINGID)
postResample(trData.3p$predFloor, trData.3p$FLOOR)


####################################################################################################### tools


# ## explore results of the KNN
# # print(BuildingKNN)
# print(BuildFloorKNN)
# # print(BuildFloorSpaceKNN)
# 
# # resamps <- resamples()
# # summary(resamps)
# 
# ### prediction on training data and plotting: KNN ####
# trData.3p <- trData.3m
# trData.3p$predBuilding <- predict(BuildingKNN, trData.3p)
# trData.3p$predFloor <- predict(FloorKNN, trData.3p)
# 
# trData.3p <- trData.0
# trData.3p$predBuilding <- predict(BuildingKNN, trData.3p)
# trData.3p$predFloor <- predict(FloorKNN, trData.3p)
# 
# ### compute errors
# trData.3p$predBuilding <- as.numeric(trData.3p$predBuilding)
# trData.3p$BUILDINGID <- as.numeric(trData.3p$BUILDINGID)
# sum((trData.3p$predBuilding-trData.3p$BUILDINGID)!=0)
# 
# trData.3p$predFloor <- as.numeric(trData.3p$predFloor)
# trData.3p$FLOOR <- as.numeric(trData.3p$FLOOR)
# sum((trData.3p$predFloor-trData.3p$FLOOR)!=0)
# 
# ## plotting results KNN
# plot(predBuilding ~ BUILDINGID, data=trData.3p, pch=16)
# points(trData.3p$BUILDINGID ~ trData.3p$BUILDINGID, col = "red", pch=4)
