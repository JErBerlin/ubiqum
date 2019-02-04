#### approach B: 
#
# - exclude variables with too low variability
# - modell position (latitude, longitude)
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

# -0- # 
# _____to use for prediction
## replace 100's (no signal) 
## replace 100 by -110 in the main dataframe
trData.0<-trData
trData.0[trData.0==100]<--110
## convert vars LATITUDE and LONGITUDE to integers
trData$LATITUDE  <- as.integer(trData$LATITUDE)
trData$LONGITUDE <- as.integer(trData$LONGITUDE)

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

ind <- apply(trData.waps.3.dist,1,max)

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

### training control: 10-fold cross-validation
trControl <- trainControl(method = "cv", number = 5) # training control: 5-fold cross-validation

## LATITUDE + LONGITUDE
l <- length(trData.3)
trData.3m <- trData.3[,1:(l-9)]
## convert vars LATITUDE and LONGITUDE to integers
trData.3$LATITUDE  <- as.integer(trData.3$LATITUDE)
trData.3$LONGITUDE <- as.integer(trData.3$LONGITUDE)
trData.3m <- cbind(trData.3m, trData.3$LATITUDE, trData.3$LONGITUDE)
names(trData.3m)[l-9+1] <- "LONGITUDE"
names(trData.3m)[l-9+2] <- "LATITUDE"

### train model: lm(LATITUDE ~ .), data = trData.3m 
LatLM <- train(
  LATITUDE ~ ., 
  data = trData.3m,
  method = "lm",
  trControl = trControl
)
beep();
# shell.exec("https://www.youtube.com/watch?v=QDKBDduuJ_0")

### train model: lm(LONGITUDE ~ .), data = trData.3m 
LonLM <- train(
  LONGITUDE ~ ., 
  data = trData.3m,
  method = "lm",
  trControl = trControl
)
beep();
# shell.exec("https://www.youtube.com/watch?v=QDKBDduuJ_0")

## explore results of the LM model
print(LatLM)
print(LonLM)

## prediction on training data and plotting: LM ####
# trData.3p <- trData.3m
# trData.3p$predLat <- predict(LatLM, trData.3p)
# trData.3p$predLond <- predict(LonLM, trData.3p)

trData.3p <- trData.0
trData.3p$predLat <- predict(LatLM, trData.3p)
trData.3p$predLond <- predict(LonLM, trData.3p)

## compute errors

## plotting results
plot(predLat ~ LATITUDE, data=trData.3p, pch=16)
points(trData.3p$predLon ~ trData.3p$LONGITUDE, col = "red", pch=4)

############################ tools

# dim(trData.3)
# dim(trData.waps.3)

# plot(raster(as.matrix(trData.waps.5)),main="trData.waps.5",
#           useRaster=F
#           )
#
# hist(trData[trData$BUILDINGID==2,"SPACEID"],breaks=250)
# hist(trData.3[trData.3$BUILDINGID==2,"SPACEID"],breaks=250)
