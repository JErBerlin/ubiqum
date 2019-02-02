#### approach C: 
#
# - exclude variables with too low variability
# - standardize by the mean of every measurement
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

# -1- #
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
# sum(varCol < 50, na.rm=T) ## 146

## keep only WASP cols that have var > 50
### make an index
ind.3 <- sapply(trData.waps.2,var,na.rm=T) > 50
ind.3[is.na(ind.3)] <- FALSE
### select cols after index
trData.waps.3 <- trData.waps.2[,ind.3]
### drop the corresponding cols of the main dataframe
trData.3 <- trData.1[,c(ind.3,rep(TRUE,9))]

# -3 bis- #
###drop the rows that are equal (f.ex. all NA)
trData.waps.3.dist <- distinct(trData.waps.3)

### drop the corresponding rows of the main dataframe ## <-- optimize!
l <- length(trData.3)
ind <- duplicated(trData.3[,1:(l-9)])
trData.3.dist <- trData.3[!ind,]

### prepare next step
trData.waps.3 <- trData.waps.3.dist
trData.3 <- trData.3.dist

# -4- #
## normalize the RSSI so that all measurements get the same mean
# meanRow3 <- apply(trData.waps.3,1,mean, na.rm=T)
# 
# trData.waps.4 <- trData.waps.3  * diag(1 / meanRow3)
# 
# trData.waps.4 <- sweep(trData.waps.3, 2, meanRow3, "/")
# meanRow4 <- apply(trData.waps.4,1,mean, na.rm=T)
#
# sdRow <- apply(trData.waps.3,1,sd, na.rm=T)
# notNAcountRow <- apply(trData.waps.3,1,notNAcount)
#    notNAcount <- function(x){sum(!is.na(x))}



# -5- #
## keep only WASP cols that have max > -5 
## so that we have very strong signals (we keep very few cols)
## Obs: continuation from step 2.
ind.4 <- sapply(trData.waps.2, max, na.rm=T) > -5
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
## keep only RSSI values > -60 (else set value=NA)
## so that we can later eliminate 
trData.waps.5 <- trData.waps.4.dist
trData.waps.5[trData.waps.5 < -60] <- NA
### replace the corresponding values of the main dataframe
trData.5 <- trData.4.dist
trData.5[trData.5 < -60] <- -110

### drop the rows that are equal (f.ex. all NA)
trData.waps.5.dist <- distinct(trData.waps.5)
### drop the corresponding rows of the main dataframe
l <- length(trData.5)
ind <- duplicated(trData.5[,1:(l-9)])
trData.5.dist <- trData.5[!ind,]