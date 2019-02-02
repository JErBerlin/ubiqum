#### approach B: 
#
# - exclude variables with too low variability
# - exclude correlated observations
# - modell
#
####

# load libraries
library(reshape2)
library(ggplot2)
library(dplyr)
library(caret)
library(beepr)

# read data
trData = read.csv(
  file ="trainingData.csv", 
  header=T)

# pre-process data
## aux dataframes
### cols corresponding to waps are 1:520
trData.waps <- trData[,1:520]

# -0- # --> to use for prediction
# _____replace 100's (no signal) #
# replace 100 by -110 in the main dataframe
trData.0<-trData
trData.0[trData.0==100]<--110

# -1- # 
# _____clean of empty rows and cols #
## keep only WASP cols that have min < 100
### make an index
ind.1 <- sapply(trData.waps, min) < 100
### select cols after index
trData.waps.1 <- trData.waps[,ind.1]
### drop the corresponding cols of the main dataframe:
### use the index of waps df (but add 9 extra cols TRUE at the end)
trData.1 <- trData[,c(ind.1,rep(TRUE,9))]

## keep only Measurement rows that have min < 100
indR.1 <- apply(trData.waps,1, min) < 100
### select rows after index
trData.waps.1 <- trData.waps.1[indR.1,]
### drop the corresponding rows of the main dataframe:
### use the same index of waps rows
trData.1 <- trData.1[indR.1,]

# -2- #
# _____replace 100's (no signal) #
## replace 100 (no signal detected from wap) by NA to complete computations
trData.waps.2 <- trData.waps.1
trData.waps.2[trData.waps.1==100]<-NA
# replace 100 by -110 in the main dataframe
trData.2<-trData.1
trData.2[trData.1==100]<--110

# -3- #
## compute variance by variable
# varCol <- apply(trData.waps.2,2,var,na.rm=T)
# hist(varCol,breaks=100)
# sum(varCol < 100, na.rm=T) ## 146

## keep only WASP cols that have var > 100
### make an index
ind.3 <- sapply(trData.waps.2,var,na.rm=T) > 100
ind.3[is.na(ind.3)] <- FALSE
### select cols after index
trData.waps.3 <- trData.waps.2[,ind.3]
### drop the corresponding cols of the main dataframe
trData.3 <- trData.2[,c(ind.3,rep(TRUE,9))]

# -3 bis- #
###drop the rows that are equal (f.ex. all NA)
trData.waps.3.dist <- distinct(trData.waps.3)

### drop the corresponding rows of the main dataframe ## <-- optimize!
l <- length(trData.3)
ind <- duplicated(trData.3[,1:(l-9)])
trData.3.dist <- trData.3[!ind,]

### prepare next steps (shorten names)
trData.3 <- trData.3.dist
trData.waps.3 <- trData.waps.3.dist

# -5- #
# ___ modell: KNN,
set.seed(1111)
l <- length(trData.3)
## BUILDINGID
trData.3m <- trData.3[,1:(l-9)]
trData.3m <- cbind(trData.3m, trData.3$BUILDINGID)
names(trData.3m)[length(trData.3m)] <- "BUILDINGID"
trData.3m$BUILDINGID = factor(trData.3m$BUILDINGID)

# training control: 10-fold cross-validation
trControl <- trainControl(method = "cv", number = 10) # training control: 10-fold cross-validation
grid <- expand.grid(k = c(2,3,4,5))

### train model: knn(BUILDINGID ~ .), data = trData.3m 
BuildingKNN <- train(
  BUILDINGID ~ ., 
  data = trData.3m,
  method = "knn",
  trControl = trControl,
  tuneGrid=grid
)
beep();
# shell.exec("https://www.youtube.com/watch?v=QDKBDduuJ_0")

## BUILDING & FLOOR
trData.3m <- trData.3[,1:(l-9)]

### as numerical (integer) var
# trData.3m <- cbind(trData.3m, as.integer(trData.3$BUILDINGID), as.integer(trData.3$FLOOR))
# names(trData.3m)[length(trData.3m)-1] <- "BUILDINGID"
# names(trData.3m)[length(trData.3m)] <- "FLOOR"

#### build composed integer from integers building and floor
trData.3m$BID_FL <- as.integer(trData.3$BUILDINGID)*10+as.integer(trData.3$FLOOR)

#### convert integer to factor
trData.3m$BID_FL <- as.factor(trData.3m$BID_FL)

### as composed factors
trData.3m <- cbind(trData.3m, as.factor(trData.3$BUILDINGID), as.factor(trData.3$FLOOR))
names(trData.3m)[length(trData.3m)-1] <- "BUILDINGID"
names(trData.3m)[length(trData.3m)] <- "FLOOR"

### build composed factor from factors building and floor
trData.3m$BID.FL <- with(trData.3m, interaction(BUILDINGID,  FLOOR))

#### drop aux cols
ind <- c(length(trData.3m)-2,length(trData.3m)-1)
trData.3m <- trData.3m[,-ind] 

### train model: knn(BID.FL ~ .), data = trData.3m 
BuidFloorKNN <- train(
  BID_FL ~ ., 
  data = trData.3m,
  method = "knn",
  trControl = trControl,
  tuneGrid=grid
)
beep();
shell.exec("https://www.youtube.com/watch?v=QDKBDduuJ_0")

### explore results of the KNN
print(BuildingKNN)
print(BuidFloorKNN)

# resamps <- resamples(list(knn = BuildingKNN))
# summary(resamps)

### prediction on training data and plotting: KNN ####
trData.3p <- trData.3m
trData.3p$predBuilding <- predict(BuildingKNN, trData.3p)
trData.3p$predFloor <- predict(FloorKNN, trData.3p)

trData.3p <- trData.0
trData.3p$predBuilding <- predict(BuildingKNN, trData.3p)
trData.3p$predFloor <- predict(FloorKNN, trData.3p)

### compute errors
trData.3p$predBuilding <- as.numeric(trData.3p$predBuilding)
trData.3p$BUILDINGID <- as.numeric(trData.3p$BUILDINGID)
sum((trData.3p$predBuilding-trData.3p$BUILDINGID)!=0)

trData.3p$predFloor <- as.numeric(trData.3p$predFloor)
trData.3p$FLOOR <- as.numeric(trData.3p$FLOOR)
sum((trData.3p$predFloor-trData.3p$FLOOR)!=0)

## plotting results KNN
plot(predBuilding ~ BUILDINGID, data=trData.3p, pch=16)
points(trData.3p$BUILDINGID ~ trData.3p$BUILDINGID, col = "red", pch=4)

############################ tools

# dim(trData.3)
# dim(trData.waps.3)

# plot(raster(as.matrix(trData.waps.5)),main="trData.waps.5",
#           useRaster=F
#           )
#
# hist(trData[trData$BUILDINGID==2,"SPACEID"],breaks=250)
# hist(trData.3[trData.3$BUILDINGID==2,"SPACEID"],breaks=250)
