# load libraries
library(reshape2)
library(ggplot2)
library(dplyr)
library(xcms)
library(corrplot)
library(gridplot)
library(raster)    

# read data
trData = read.csv(
  file ="trainingData.csv", 
  header=T)

# organize (melt) data to plot simultaneously several boxplots of the WAPs
trData.boxplot <- melt(trData)

# n <- 19937
# trData.boxplot.1.100 <- trData.boxplot[1:100*n,]
# r1 <- (400*n+1)
# rf <- 500*n
# trData.boxplot.400.500 <- trData.boxplot[r1:rf,]
# 
# ggplot(data = trData.boxplot.400.500, aes(x=variable, y=value)) + geom_boxplot()+theme(axis.text.x = element_text(angle = 90))

## aux dataframes
### cols corresponding to waps are 1:520
trData.waps <- trData[,1:520]
# nrow(trData.waps) ## 19937
# ncol(trData.waps) ## 520
# plot(raster(as.matrix(trData.waps)),main="trData.waps")

## keep only WASP cols that have min < 100
### make an index
ind.1 <- (sapply(trData.waps, min) < 100)
### select cols after index
trData.waps.1 <- trData.waps[,ind.1]
# nrow(trData.waps.1) ## 19937
# ncol(trData.waps.1) ## 465
# plot(raster(as.matrix(trData.waps.1)),main="trData.waps.1")

## replace 100 (no signal detected from wap) by NA to complete computations
trData.waps.2 <- trData.waps.1
trData.waps.2[trData.waps.1==100]<-NA
# summarise_all(trData.waps.2, funs(max), na.rm=T)
# nrow(trData.waps.2) ## 19937
# ncol(trData.waps.2) ## 465
# plot(raster(as.matrix(trData.waps.2)),main="trData.waps.2")

### plot hist
max.2 <- as.numeric(summarise_all(trData.waps.2, funs(max), na.rm=T))
hist(max.2)

###### .3 NOT ###
## keep only WASP cols that have min > -50 (value 100 is now na)
## so that we have strong signals (we keep about the half of cases)
# ind.3 <- sapply(trData.waps.2, max, na.rm=T) > -50 
# trData.waps.3 <- trData.waps.2[,ind.3]
# nrow(trData.waps.3) ## 19937
# ncol(trData.waps.3) ## 210
# plot(raster(as.matrix(trData.waps.3)),main="trData.waps.3")
### plot hist
# max.3 <- as.numeric(summarise_all(trData.waps.3, funs(max), na.rm=T))
# hist(max.3)

## keep only WASP cols that have max > -5 
## so that we have very strong signals (we keep very few cases)
## Obs: continuation from step 2.
ind.4 <- sapply(trData.waps.2, max, na.rm=T) > -5 
trData.waps.4 <- trData.waps.2[,ind.4]
# nrow(trData.waps.4) ## 19937
# ncol(trData.waps.4) ## 36
plot(raster(as.matrix(trData.waps.4)),main="trData.waps.4")
# ### plot hist
# max.4 <- as.numeric(summarise_all(trData.waps.4, funs(max), na.rm=T))
# hist(max.4)

###drop the rows that are equal (f.ex. all NA)
trData.waps.4.dist <- distinct(trData.waps.4)
# nrow(trData.waps.4.dist) ## 8796
# ncol(trData.waps.4.dist) ## 36
# plot(raster(as.matrix(trData.waps.4.dist)),main="trData.waps.4.dist",
#      #maxpixels=100,
#      useRaster=T,
#      interpolate=T
#      )

### organize (melt) data to plot simultaneously several boxplots of the WAPs
trData.waps.4.dist.boxplot <- melt(trData.waps.4.dist)
ggplot(data = trData.waps.4.dist.boxplot, aes(x=variable, y=value)) + geom_boxplot()+theme(axis.text.x = element_text(angle = 90))

## explore correlations
M.4 <- trData.waps.4.dist
M.4[is.na(M.4)] <- 0
Cor.4 <-cor(M.4)
# head(round(Cor.4,2))
### max coefs (out of the diagonal)
apply(abs(Cor.4)-diag(diag(Cor.4)),2,max)
hist(apply(abs(Cor.4)-diag(diag(Cor.4)),2,max))
# sum(apply(abs(Cor.4)-diag(diag(Cor.4)),2,max) > 0.)
corrplot(Cor.4, method="circle")

## 5. keep only RSSI values > -5 (else set value=NA)
## so that we can later eliminate 
trData.waps.5 <- trData.waps.4.dist
trData.waps.5[trData.waps.5 < -5] <- NA
# nrow(trData.waps.5) ## 8796
# ncol(trData.waps.5) ## 36
# plot(raster(as.matrix(trData.waps.5)),main="trData.waps.5")

###drop the rows that are equal (f.ex. all NA)
trData.waps.5.dist <- distinct(trData.waps.5)
# nrow(trData.waps.5.dist) ## 73
# ncol(trData.waps.5.dist) ## 36
# plot(raster(as.matrix(trData.waps.5.dist)),main="trData.waps.5.dist",
#      useRaster=F
#      )

## explore correlations
M.5 <- trData.waps.5.dist
M.5[is.na(M.5)] <- -110
Cor.5 <-cor(M.5)
# head(round(Cor.4,2))
## max coefs (out of the diagonal)
apply(abs(Cor.5)-diag(diag(Cor.5)),2,max)
hist(apply(abs(Cor.5)-diag(diag(Cor.5)),2,max))
# sum(apply(abs(Cor.4)-diag(diag(Cor.4)),2,max) > 0.)
corrplot(Cor.5, method="circle")
