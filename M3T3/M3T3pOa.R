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

## keep only WASP cols that have min < 100
### make an index
ind.1 <- (summarise_all(trData.waps, funs(min)) < 100)
### select cols after index
trData.waps.1 <- trData.waps[,ind.1[1,]]
# ncol(trData.waps.1)

## replace 100 (no signal detected from wap) by NA to complete computations
trData.waps.2 <- trData.waps.1
trData.waps.2[trData.waps.1==100]<-NA
# summarise_all(trData.waps.2, funs(max), na.rm=T)

## keep only WASP cols that have min > -50 (value 100 is now na)
## so that we have strong signals (we keep about the half of cases)
ind.3 <- summarise_all(trData.waps.2, funs(max), na.rm=T) > -50 
trData.waps.3 <- trData.waps.2[,ind.3[1,]]
# ncol(trData.waps.3) ##210

## keep only WASP cols that have min > -5 
## so that we have very strong signals (we keep very few cols)
ind.4 <- summarise_all(trData.waps.3, funs(max), na.rm=T) > -5 
trData.waps.4 <- trData.waps.3[,ind.4[1,]]
# ncol(trData.waps.4) ## 36

###drop the rows that are equal (f.ex. all NA)
trData.waps.4.dist <- distinct(trData.waps.4)

# drop the corresponding cols of the main dataframe



# drop now the rows of the main dataframe 
# that no have relevant info

## according to ind.4 (WASP cols that have min > -5)
