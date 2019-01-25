# load libraries
library(plyr)
library(ggplot2)

# load data
Trs <- read.transactions("ElectronidexTransactions2017.csv", sep=',')
  
#data preprocessing is done!

# TrsMeanRow <- array(1:ncol(Trs@data))
# for (i in 1:ncol(Trs@data)) {
#   TrsMeanRow[i] <- sd(as.numeric(Trs@data[i,]))
# }

# test if the rows are independent: --> should be (simetrical) binomial dist
# make a sample of 2 cols (every col with 125 items or rows)
# calculate the difference of the 2 random cols
# plot an histogram of the difference
# if random -> should be binomial distribution with expectation d*p2 and variance dp2(1 − p2).
# where p2 := p^2 + (1 − p)^2 = 0.93 and d = 125, p being the probability of any item to be bought
# p approx = 0,035 
# p2 approx 0.93, d*p2 approx 117 == E(), d*p2(1-p2) approx 8.1 == stdev, [101,133] 
TrsDiffDist <- array(1:10) # 1:200
set.seed(111)
for (i in 1:10) { # 1:200
 Trs2Smp <- sample(Trs, 2)
 TrsDiffDist[i] <- count(abs(as.numeric(Trs2Smp@data[,1])-as.numeric(Trs2Smp@data[,2])))[1,2]
}
hist(TrsDiffDist, breaks = 20)

TrsDiffDist <- array(1:200) # 1:200
for (i in 1:200) { # 1:200
  set.seed(i*3)
  Trs1Smp <- sample(Trs, 1)
  TrsDiffDist[i] <- count(abs(as.numeric(Trs1Smp@data[,1])-as.numeric(Trs@data[,997])))[1,2]
}
hist(TrsDiffDist, breaks = 20)

N <- nrow(Trs@data)
M <- ncol(Trs@data)
# TrsItemMeans <- array(1:N*M, c(N,M))
TrsItemMeans <- array(1:N)
TrsItemP <- array(1:N)
P2 <- array(1:N)
ni <- array(1:N)
ind <- array(1:N)
DistSinib <- array(1:N)

for (i in 1:N) {
  # for(j in 1:M) {
  #   TrsItemMeans[i,j] <- as.numeric(Trs2Smp@data[i,j])
  # }
  TrsItemMeans[i] <- count(as.numeric(Trs@data[i,]))[2,2]
  TrsItemP[i] <- as.numeric(TrsItemMeans[i])/M
  P2[i] <-  TrsItemP[i]^2 +(1-TrsItemP[i])^2
  ni[i] <- 1
  ind[i] <- i
}                              
mean(TrsItemP)
mean(P2)
hist(TrsItemP, breaks = 50)

library(sinib)
for (i in 1:N) {
  DistSinib[i] <- dsinib(x = i, size = ni, prob = P2)
}
plot(DistSinib, type = 'h')

mydata <-data.frame(ind, P2)

p <-ggplot(mydata, aes(ind, P2))
p +geom_bar()

qplot(P2 , data = mydata)
