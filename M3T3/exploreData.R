# load libraries
library(reshape2)
library(ggplot2)
library(dplyr)
library(xcms)

# read data
trData = read.csv(
  file ="trainingData.csv", 
  header=T)

# organize (melt) data to plot simultaneously several boxplots of the WAPs
trData.boxplot <- melt(trData)

n <- 19937
trData.boxplot.1.100 <- trData.boxplot[1:100*n,]
r1 <- (400*n+1)
rf <- 500*n
trData.boxplot.400.500 <- trData.boxplot[r1:rf,]

ggplot(data = trData.boxplot.400.500, aes(x=variable, y=value)) + geom_boxplot()+theme(axis.text.x = element_text(angle = 90))

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

### plot hist
max.2 <- as.numeric(summarise_all(trData.waps.2, funs(max), na.rm=T))
hist(max.2)

## keep only WASP cols that have min > -50 (value 100 is now na)
## so that we have strong signals (we keep about the half of cases)
ind.3 <- summarise_all(trData.waps.2, funs(max), na.rm=T) > -50 
trData.waps.3 <- trData.waps.2[,ind.3[1,]]
# ncol(trData.waps.3)

### plot hist
max.3 <- as.numeric(summarise_all(trData.waps.3, funs(max), na.rm=T))
hist(max.3)

## keep only WASP cols that have min > -5 
## so that we have very strong signals (we keep very few cases)
ind.4 <- summarise_all(trData.waps.3, funs(max), na.rm=T) > -5 
trData.waps.4 <- trData.waps.3[,ind.4[1,]]
# ncol(trData.waps.4) ## 36

### plot hist
max.4 <- as.numeric(summarise_all(trData.waps.4, funs(max), na.rm=T))
hist(max.4)

### organize (melt) data to plot simultaneously several boxplots of the WAPs
trData.waps.4.dist.boxplot <- melt(trData.waps.4.dist)

ggplot(data = trData.waps.4.dist.boxplot, aes(x=variable, y=value)) + geom_boxplot()+theme(axis.text.x = element_text(angle = 90))

