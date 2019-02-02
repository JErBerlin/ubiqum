#### approach A: 
#
# - select WAPS with at least one value > -5
#   (drop weak WAPS)
# - neglige values in Measurements under threshold of -60 (else set value=NA)
#
#   dim(trData.5)
#   [1] 1833   45
#
####

# load libraries
library(reshape2)
library(ggplot2)
library(dplyr)

# read data
trData = read.csv(
  file ="trainingData.csv", 
  header=T)

# pre-process data
## aux dataframes
### cols corresponding to waps are 1:520
trData.waps <- trData[,1:520]
# ncol(trData.waps)

# -1- #
## keep only WASP cols that have min < 100
### make an index
ind.1 <- sapply(trData.waps, min) < 100
### select cols after index
trData.waps.1 <- trData.waps[,ind.1]
### drop the corresponding cols of the main dataframe:
### use the index of waps df (but add 9 extra cols TRUE at the end)
trData.1 <- trData[,c(ind.1,rep(TRUE,9))]

# -2- #
## replace 100 (no signal detected from wap) by NA to complete computations
trData.waps.2 <- trData.waps.1
trData.waps.2[trData.waps.1==100]<-NA
# replace 100 by -110 in the main dataframe
trData.2<-trData.1
trData.2[trData.1==100]<--110

# -4- #
## keep only WASP cols that have max > -15 
## so that we have very strong signals (we keep very few cols)
## Obs: continuation from step 2.
ind.4 <- sapply(trData.waps.2, max, na.rm=T) > -15
trData.waps.4 <- trData.waps.2[,ind.4]
### drop the corresponding cols of the main dataframe
trData.4 <- trData.2[,c(ind.4,rep(TRUE,9))]

###drop the rows that are equal (f.ex. all NA)
# ind <- indRowAllWapsNA(trData.waps.4) 
# trData.waps.4.dist <- trData.waps.4[-ind,]
trData.waps.4.dist <- distinct(trData.waps.4)

### drop the corresponding rows of the main dataframe ## <-- optimize!
# trData.4.dist <- trData.4[-ind,]
l <- length(trData.4)
ind <- duplicated(trData.4[,1:(l-9)])
trData.4.dist <- trData.4[!ind,]

# -5- #
## keep only RSSI values > -70 (else set value=NA)
## so that we can later eliminate 
trData.waps.5 <- trData.waps.4.dist
trData.waps.5[trData.waps.5 < -70] <- NA
### replace the corresponding values of the main dataframe
trData.5 <- trData.4.dist
trData.5[trData.5 < -70] <- -110

### drop the rows that are equal (f.ex. all NA)
trData.waps.5.dist <- distinct(trData.waps.5)
### drop the corresponding rows of the main dataframe
l <- length(trData.5)
ind <- duplicated(trData.5[,1:(l-9)])
trData.5.dist <- trData.5[!ind,]

### prepare for next step
trData.5 <- trData.5.dist
trData.waps.5 <- trData.waps.5.dist

############################ tools

# plot(raster(as.matrix(trData.waps.5)),main="trData.waps.5",
#           useRaster=F
#           )
#
# hist(trData[trData$BUILDINGID==0,"SPACEID"],breaks=100)
# hist(trData.5[trData.5$BUILDINGID==0,"SPACEID"],breaks=100)


