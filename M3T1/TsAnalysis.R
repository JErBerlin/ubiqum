# load libraries
library(fracdiff)
library(uroot)
library(forecast)
library(seasonal)

# make time series GAP by minute
TsGAP <- ts(consumeTb$Global_active_power, start=2006.958, frequency=525600)
### impute NA values
TsGAP <- na.interpolation(TsGAP)

# further analysis (grouping by date)
cns <- consumeTb ##for short

## mean
gapMeanD <- aggregate(cns$Global_active_power,by=list(cns$Date),mean)
colnames(gapMeanD) <- c('Date', 'GAP')
### impute NA values
gapMeanD <- na.interpolation(gapMeanD)

## max
gapMaxD <- aggregate(cns$Global_active_power,by=list(cns$Date),max)
colnames(gapMaxD) <- c('Date', 'GAP')
### impute NA values 
gapMaxD <- na.interpolation(gapMaxD)

## ratio: max/mean
gapMaxMeanD <- gapMaxD
gapMaxMeanD$GAP <- gapMeanMaxD$GAP/gapMeanD$GAP
### impute NA values
gapMaxMeanD <- na.interpolation(gapMaxMeanD)

## make time series max, mean, maxmean of GAP by day
TsgapMaxD <- ts(gapMaxD$GAP, start=2006.956, frequency=365)
# TsgapMeanD <- ts(gapMeanD$GAP, start=2006.956, frequency=365)
# TsgapMaxMeanD <- ts(gapMaxMeanD$GAP, start=2006.956, frequency=365)

### select time period for ts mean, max and maxmean
# TsMean09mid <- window(TsgapMeanD, start=2009.5)
# TsMean08 <- window(TsgapMeanD, start=2008)

TsMax09mid <- window(TsgapMaxD, start=2009.5)
TsMax08 <- window(TsgapMaxD, start=2008)

# TsMaxMean09mid <- window(TsgapMaxMeanD, start=2009.5)
# TsMaxMean08 <- window(TsgapMaxMeanD, start=2008)

## plots
ggplot(data=gapMaxD, aes(x=Date, y=GAP, color=GAP))+
       geom_line()+
       xlab("Day/Time")+
       ylab("Max Global Active Power (kilowatts)")+
       ggtitle("Maximum Global Active Power by Day")+
       theme(panel.background = element_rect(fill = rgb(240, 230, 200, maxColorValue = 255)))

# ggplot(data=gapMeanD, aes(x=Date, y=GAP, color=GAP))+
#   geom_line()+
#   xlab("Day/Time")+
#   ylab("Mean Global Active Power (kilowatts)")+
#   ggtitle("Mean Global Active Power by Day")+
#   theme(panel.background = element_rect(fill = rgb(240, 230, 200, maxColorValue = 255)))

# ggplot(data=gapMaxMeanD, aes(x=Date, y=GAP, color=GAP))+
#   geom_line()+
#   xlab("Day/Time")+
#   ylab("Max/Mean ratio GAP")+
#   ggtitle("Max/Mean ratio GAP by Day")+
#   theme(panel.background = element_rect(fill = rgb(240, 230, 200, maxColorValue = 255)))

## plots -- seasonality
ggseasonplot(TsgapMaxD)
ggseasonplot(TsgapMaxD, polar =T)

# ggseasonplot(TsgapMeanD)
# ggseasonplot(TsgapMeanD, polar =T)

# ggseasonplot(TsgapMaxMeanD)
# ggseasonplot(TsgapMaxMeanD, polar =T)

### -- seasonality - subseries
# ggsubseriesplot(TsgapMaxD) #?? do we need it
# ggsubseriesplot(TsgapMeanD)
# ggsubseriesplot(TsgapMaxMeanD)

### -- seasonality - lagplot
TsMax2010 <- window(TsgapMaxD, start=2010.500)
TsMax2009 <- window(TsgapMaxD, start=2008.800)
gglagplot(TsMax2010, set.lags = c(1,2,3,4,5,6,7,8,9))
gglagplot(TsMax2009, set.lags = c(30, 365, 730))

# TsMean2010 <- window(TsgapMeanD, start=2010.500)
# TsMean2009 <- window(TsgapMeanD, start=2008.800)
# gglagplot(TsMean2010, set.lags = c(1,2,3,4,5,6,7,8,9))
# gglagplot(TsMean2009, set.lags = c(30, 365, 730))

# TsMaxMean2010 <- window(TsgapMaxMeanD, start=2010.500)
# TsMaxMean2009 <- window(TsgapMaxMeanD, start=2008.800)
# gglagplot(TsMaxMean2010, set.lags = c(1,2,3,4,5,6,7,8,9))
# gglagplot(TsMaxMean2009, set.lags = c(30, 365, 730))

## autocorrelation
ggAcf(TsgapMaxD)
ggAcf(TsMax2010)
ggAcf(TsMax2009)

# ggAcf(TsgapMeanD)
# ggAcf(TsMean2010)
# ggAcf(TsMean2009)

# ggAcf(TsgapMaxMeanD)

# simple forecasting
## naïve, mean, seasonal naïve

### TsMean09mid
# autoplot(TsMean09mid) +
#   autolayer(meanf(TsMean09mid, h=365),
#             series="Mean", PI=FALSE) +
#   autolayer(naive(TsMean09mid, h=365),
#             series="Naïve", PI=FALSE) +
#   autolayer(snaive(TsMean09mid, h=365),
#             series="Seasonal naïve", PI=FALSE) +
#   ggtitle("Forecasts for Mean GAP, start from 2009mid") +
#   xlab("") + ylab("") +
#   guides(colour=guide_legend(title="Forecast"))

### TsMean08
# autoplot(TsMean08) +
#   autolayer(meanf(TsMean08, h=365),
#             series="Mean", PI=FALSE) +
#   autolayer(naive(TsMean08, h=365),
#             series="Naïve", PI=FALSE) +
#   autolayer(snaive(TsMean08, h=365),
#             series="Seasonal naïve", PI=FALSE) +
#   ggtitle("Forecasts for Mean GAP, start from 2008") +
#   xlab("") + ylab("") +
#   guides(colour=guide_legend(title="Forecast"))

### TsMax09mid
autoplot(TsMax09mid) +
  autolayer(meanf(TsMax09mid, h=365),
            series="Mean", PI=FALSE) +
  autolayer(naive(TsMax09mid, h=365),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(TsMax09mid, h=365),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for Max GAP, start from 2009mid") +
  xlab("") + ylab("") +
  guides(colour=guide_legend(title="Forecast"))

### TsMax08
autoplot(TsMax08) +
  autolayer(meanf(TsMax08, h=365),
            series="Mean", PI=FALSE) +
  autolayer(naive(TsMax08, h=365),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(TsMax08, h=365),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for Max GAP, start from 2008") +
  xlab("") + ylab("") +
  guides(colour=guide_legend(title="Forecast"))

### TsMaxMean09mid
# autoplot(TsMaxMean09mid) +
#   autolayer(meanf(TsMaxMean09mid, h=365),
#             series="Mean", PI=FALSE) +
#   autolayer(naive(TsMaxMean09mid, h=365),
#             series="Naïve", PI=FALSE) +
#   autolayer(snaive(TsMaxMean09mid, h=365),
#             series="Seasonal naïve", PI=FALSE) +
#   ggtitle("Forecasts for MaxMean GAP, start from 2009mid") +
#   xlab("") + ylab("") +
#   guides(colour=guide_legend(title="Forecast"))

### TsMaxMean08
# autoplot(TsMaxMean08) +
#   autolayer(meanf(TsMaxMean08, h=365),
#             series="Mean", PI=FALSE) +
#   autolayer(naive(TsMaxMean08, h=365),
#             series="Naïve", PI=FALSE) +
#   autolayer(snaive(TsMaxMean08, h=365),
#             series="Seasonal naïve", PI=FALSE) +
#   ggtitle("Forecasts for MaxMean GAP, start from 2008") +
#   xlab("") + ylab("") +
#   guides(colour=guide_legend(title="Forecast"))

## snaiv(): Residuals for snaiv method start=2008
### TsMean08: calculation snaiv and residuals
# TsMean08snaive <- snaive(TsMean08)
# resTsMean08snaive <- residuals(TsMean08snaive)
# #### plot residuals
# autoplot(resTsMean08snaive) + xlab("") + ylab("") +
#   ggtitle("Residuals from snaïve method, Mean GAP, start=2008")
# #### histogram
# gghistogram(resTsMean08snaive) + ggtitle("Residuals Mean GAP start=2008")
# #### acf
# ggAcf(resTsMean08snaive) + ggtitle("ACF for snaive of Mean start=2008")
# #### integrated check residuals
# checkresiduals(TsMean08snaive)

### TsMax08
TsMax08snaive <- snaive(TsMax08)
resTsMax08snaive <- residuals(TsMax08snaive)
#### plot residuals
autoplot(res) + xlab("") + ylab("") +
  ggtitle("Residuals from snaïve method, Max GAP, start=2008")
#### histogram
gghistogram(resTsMax08snaive) + ggtitle("Residuals Max GAP start=2008")
#### acf
ggAcf(resTsMax08snaive) + ggtitle("ACF for snaive of Max start=2008")
#### integrated check residuals
checkresiduals(TsMax08snaive) 

### TsMaxMean08
# TsMaxMean08snaive <-snaive(TsMaxMean08)
# resTsMaxMean08snaive <- residuals(TsMaxMean08snaive)
# autoplot(resTsMaxMean08snaive) + xlab("") + ylab("") +
#   ggtitle("Residuals from snaïve method, MaxMean GAP, start=2008")
# #### histogram
# gghistogram(resTsMaxMean08snaive) + ggtitle("Residuals MaxMean GAP start=2008")
# #### acf
# ggAcf(resTsMaxMean08snaive) + ggtitle("ACF for snaive of MaxMean start=2008")
# #### integrated check residuals
# checkresiduals(TsMaxMean08snaive)

## accuracy of predictions: snaiv()
TsMeanAll <- window(TsgapMeanD, start=2008.25)
TsMeanTrain <- window(TsgapMeanD, start=2008.25, end=2009.75)
TsMeanTest  <- window(TsgapMeanD, start=2009.75, end=2010.75)

TsMaxAll <- window(TsgapMaxD, start=2008.25)
TsMaxTrain <- window(TsgapMaxD, start=2008.25, end=2009.75)
TsMaxTest  <- window(TsgapMaxD, start=2009.75, end=2010.75)

TsMaxMeanAll <- window(TsgapMaxMeanD, start=2008.25)
TsMaxMeanTrain <- window(TsgapMaxMeanD, start=2008.25, end=2009.75)
TsMaxMeanTest  <- window(TsgapMaxMeanD, start=2009.75, end=2010.75)

### Mean
# TsMeanFit1 <- meanf(TsMeanTrain,h=365)
# TsMeanFit2 <- rwf(TsMeanTrain,h=365)
# TsMeanFit3 <- snaive(TsMeanTrain,h=365)
# autoplot(window(TsMeanAll)) +
#   autolayer(TsMeanFit1, series="Mean", PI=FALSE) +
#   autolayer(TsMeanFit2, series="Naïve", PI=FALSE) +
#   autolayer(TsMeanFit3, series="Seasonal naïve", PI=FALSE) +
#   ggtitle("Accuracy of Forecast Mean GAP") +
#   guides(colour=guide_legend(title="Accuracy of Forecast Mean GAP: meanf, rwf, snaive"))

# accuracy(TsMeanFit1, TsMeanTest)
# accuracy(TsMeanFit2, TsMeanTest)
# accuracy(TsMeanFit3, TsMeanTest)

### Max
TsMaxFit1 <- meanf(TsMaxTrain,h=365)
TsMaxFit2 <- rwf(TsMaxTrain,h=365)
TsMaxFit3 <- snaive(TsMaxTrain,h=365)
autoplot(window(TsMaxAll)) +
  autolayer(TsMaxFit1, series="Mean", PI=FALSE) +
  autolayer(TsMaxFit2, series="Naïve", PI=FALSE) +
  autolayer(TsMaxFit3, series="Seasonal naïve", PI=FALSE) +
  ggtitle("Accuracy of Forecast Max GAP") +
  guides(colour=guide_legend(title="Accuracy of Forecast Max GAP: meanf, rwf, snaive"))

# accuracy(TsMaxFit1, TsMaxTest)
# accuracy(TsMaxFit2, TsMaxTest)
# accuracy(TsMaxFit3, TsMaxTest)

### MaxMean
# TsMaxMeanFit1 <- meanf(TsMaxMeanTrain,h=365)
# TsMaxMeanFit2 <- rwf(TsMaxMeanTrain,h=365)
# TsMaxMeanFit3 <- snaive(TsMaxMeanTrain,h=365)
# autoplot(window(TsMaxMeanAll)) +
#   autolayer(TsMaxMeanFit1, series="Mean", PI=FALSE) +
#   autolayer(TsMaxMeanFit2, series="Naïve", PI=FALSE) +
#   autolayer(TsMaxMeanFit3, series="Seasonal naïve", PI=FALSE) +
#   ggtitle("Accuracy of Forecast MaxMean GAP") +
#   guides(colour=guide_legend(title="Accuracy of Forecast MaxMean GAP: meanf, rwf, snaive"))

# accuracy(TsMaxMeanFit1, TsMaxMeanTest)
# accuracy(TsMaxMeanFit2, TsMaxMeanTest)
# accuracy(TsMaxMeanFit3, TsMaxMeanTest)

# The forecast package in R

## model: ARIMA
### mean GAP
# mArimaMean <- arima(TsMeanTrain)
# fmArimaMean <- forecast(mArimaMean, h=200)

### mean GAP
mArimaMax <- arima(TsMaxTrain)
fmArimaMax <- forecast(mArimaMax, h=200)

### max mean GAP
# mArimaMaxMean <- arima(TsMaxMeanTrain)
# fmArimaMaxMean <- forecast(mArimaMaxMean, h=200)

# time series decomposition
## Moving Average (MA) 
### Mean, Max, MaxMean; m=7, m=30
# autoplot(TsMean08, series="Data") +
#   autolayer(ma(TsMean08,7), series="7-MA") +
#   ggtitle("Mean GAP") +
#   scale_colour_manual(values=c("Data"="grey50","7-MA"="red"),
#                       breaks=c("Data","7-MA"))
# 
# autoplot(TsgapMeanD, series="Data") +
#   autolayer(ma(TsgapMeanD,30), series="30-MA") +
#   ggtitle("Mean GAP") +
#   scale_colour_manual(values=c("Data"="grey50","30-MA"="red"),
#                       breaks=c("Data","30-MA"))

autoplot(TsMax08, series="Data") +
  autolayer(ma(TsMax08,7), series="7-MA") +
  ggtitle("Max GAP") +
  scale_colour_manual(values=c("Data"="grey50","7-MA"="red"),
                      breaks=c("Data","7-MA"))

autoplot(TsgapMaxD, series="Data") +
  autolayer(ma(TsgapMaxD,12), series="30-MA") +
  ggtitle("Max GAP") +
  scale_colour_manual(values=c("Data"="grey50","30-MA"="red"),
                      breaks=c("Data","30-MA"))

# autoplot(TsMaxMean08, series="Data") +
#   autolayer(ma(TsMaxMean08,7), series="7-MA") +
#   ggtitle("MaxMean GAP") +
#   scale_colour_manual(values=c("Data"="grey50","7-MA"="red"),
#                       breaks=c("Data","7-MA"))
# 
# autoplot(TsgapMaxMeanD, series="Data") +
#   autolayer(ma(TsgapMaxMeanD,12), series="30-MA") +
#   ggtitle("MaxMean GAP") +
#   scale_colour_manual(values=c("Data"="grey50","30-MA"="red"),
#                       breaks=c("Data","30-MA"))

## classical decomposition
# TsMean08 %>% decompose() %>%
#   autoplot() +
#   ggtitle("Decomposition: Mean GAP, start 2008")
# TsgapMeanD %>% decompose() %>%
#   autoplot() +
#   ggtitle("Decomposition: Mean GAP, all")

TsMax08 %>% decompose() %>%
  autoplot() +
  ggtitle("Decomposition: Max GAP, start 2008")
TsgapMaxD %>% decompose() %>%
  autoplot() +
  ggtitle("Decomposition: Max GAP, all")

# TsMaxMean08 %>% decompose() %>%
#   autoplot() +
#   ggtitle("Decomposition: MaxMean GAP, start 2008")
# TsgapMaxMeanD %>% decompose() %>%
#   autoplot() +
#   ggtitle("Decomposition: MaxMean GAP, all")

## x11 decomposition -- not working on these series!

# TsMean08 %>% seas(x11="") -> fit
# autoplot(fit) +
#   ggtitle("X11 Decomposition: Mean GAP, start 2008")
# TsgapMeanD %>% seas(x11="") -> fit
# autoplot(fit) +
#   ggtitle("X11 Decomposition: Mean GAP, all")
# 
# TsMax08 %>% seas(x11="") -> fit
# autoplot(fit) +
#   ggtitle("X11 Decomposition: Max GAP, start 2008")
# TsgapMaxD %>% seas(x11="") -> fit
# autoplot(fit) +
#   ggtitle("X11 Decomposition: Max GAP, all")
# 
# TsMaxMean08 %>% seas(x11="") -> fit
# autoplot(fit) +
#   ggtitle("X11 Decomposition: MaxMean GAP, start 2008")
# TsgapMaxMeanD %>% seas(x11="") -> fit
# autoplot(fit) +
#   ggtitle("X11 Decomposition: MaxMean GAP, all")

## STL decomposition
# TsMean08 %>% stl(s.window="periodic") %>%
#   autoplot() +
#   ggtitle("Decomposition: Mean GAP, start 2008")
# TsgapMeanD %>% stl(s.window="periodic") %>%
#   autoplot() +
#   ggtitle("Decomposition: Mean GAP, all")

TsMax08 %>% stl(s.window="periodic") -> stlMax08 
stlMax08 %>%
  autoplot() +
  ggtitle("Decomposition: Max GAP, start 2008")
TsgapMaxD %>% stl(s.window="periodic") -> stlMaxAll 
stlMaxAll %>%
  autoplot() +
  ggtitle("Decomposition: Max GAP, all")


# TsMaxMean08 %>% stl(s.window="periodic") %>%
#   autoplot() +
#   ggtitle("Decomposition: MaxMean GAP, start 2008")
# TsgapMaxMeanD %>% stl(s.window="periodic") %>%
#   autoplot() +
#   ggtitle("Decomposition: MaxMean GAP, all")

## strength of trend and seasonality
### seasonal component 
St <- stlMaxAll$time.series[,"seasonal"]
St08 <- stlMax08$time.series[,"seasonal"]
### trend component 
Tt <- stlMaxAll$time.series[,"trend"]
# Tt %>% autoplot()
Tt08 <- stlMax08$time.series[,"trend"]
### remain component
Rt <- stlMaxAll$time.series[,"remainder"]
Rt08 <- stlMax08$time.series[,"remainder"]

autoplot(TsgapMaxD) +
  autolayer(Tt, series="Trend") +
  autolayer(St, series="Seasonal") +
  autolayer(Rt, series="Remainder") +
  ggtitle("Decomposition of Max GAP")

autoplot(TsMax08) +
  autolayer(Tt08, series="Trend") +
  autolayer(St08, series="Seasonal") +
  autolayer(Rt08, series="Remainder") +
  ggtitle("Decomposition of Max GAP (Start 2008")

# varSe <- var(stlMaxAll$time.series[,"seasonal"])
# varTr <- var(stlMaxAll$time.series[,"trend"])
# varRe <- var(stlMaxAll$time.series[,"remainder"])

### compute strength of trend component 
Ft <- max(0,1 - var(Rt) / var(Tt+Rt)) # 0.09 => small
### compute strength of seasonal component          
Fs <- max(0,1 - var(Rt) / var(St+Rt)) # 0.43 => medium

## Forecast using decomposition
fit <- stlMaxAll
### naive
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("GAP") +
  ggtitle("Naive forecasts of seasonally adjusted data")
### naive + random walk 
fit %>% forecast(method="naive") %>%
  autoplot() + ylab("GAP")

## Heuristic forecast using decomposition
### max bound
tail(Tt08,1)+max(St08)+max(Rt08)      # 11.63
# tail(Tt08,1)+max(St08+Rt08) # 10.42
tail(Tt,1)+max(St)+max(Rt)            # 11.33
# tail(Tt,1)+max(St+Rt)       # 10.39

### accept one blockout in year
tail(Tt08,1)+tail(sort(St08),4)[1]+tail(sort(Rt08),4)[1]   #10.96
# tail(Tt08,1)+tail(sort(St08+Rt08),4)[1]     # 9.58
tail(Tt,1)+tail(sort(St),4)[1]+tail(sort(Rt),4)[1]         #10.67
# tail(Tt,1)+tail(sort(St+Rt),4)[1]           # 9.58

### accept two blockouts in year
tail(Tt08,1)+tail(sort(St08),7)[1]+tail(sort(Rt08),7)[1]   #10.29
tail(Tt,1)+tail(sort(St),7)[1]+tail(sort(Rt),7)[1]         #10.35

### accept two blockouts in year
tail(Tt08,1)+tail(sort(St08),10)[1]+tail(sort(Rt08),10)[1] #10.04
tail(Tt,1)+tail(sort(St),10)[1]+tail(sort(Rt),10)[1]       #10.12

# ## forecast of trend Tt
# fTtRw <- rwf(Tt,h=365, drift =TRUE)
# fTtNa <- snaive(Tt,h=365)
# 
# autoplot(window(Tt)) +
#   autolayer(fTt1, series="Mean", PI=FALSE) +
#   autolayer(fTt2, series="Naïve", PI=FALSE) +
#   autolayer(fTt3, series="Seasonal naïve", PI=FALSE) +
#   ggtitle("Forecast of trend Max GAP") +
#   guides(colour=guide_legend(title="Forecast of trend Max GAP: meanf, rwf, snaive"))
# 
# ### plot forecast 
# fit <- fTtRw
# fit %>% forecast(method="naive") %>%
#   autoplot() + ylab("GAP")

## Simple exponential smoothing
### all data
fSesMax <- ses(TsgapMaxD, h=365)
fit <- fSesMax
fit %>% forecast(method="ses") %>%
  autoplot() + autolayer(fitted(fit), series="Fitted")
#### metrics
round(accuracy(fit),2)
#                 ME RMSE  MAE   MPE  MAPE MASE ACF1
# Training set -0.01 1.37 1.03 -8.44 23.48 0.66 0.12

### last data (from 2009)
fSesMax09 <- ses(TsMax2009, h=365)
fit <- fSesMax09
fit %>% forecast(method="ses") %>%
  autoplot() + autolayer(fitted(fit), series="Fitted")
#### metrics
round(accuracy(fit),2)
#              ME RMSE  MAE   MPE  MAPE MASE ACF1
# Training set  0 1.33 1.01 -7.54 22.56 0.69 0.14

## Trend methods -- Holts
fHoltMax <- holt(TsgapMaxD, h=365)
fit <- fHoltMax
round(accuracy(fit),2)
#              ME RMSE  MAE   MPE  MAPE MASE ACF1
# Training set  0 1.38 1.03 -8.26 23.42 0.66 0.12
fHoltMax09 <- holt(TsMax2009, h=365)
fit <- fHoltMax09
round(accuracy(fit),2)
#                 ME RMSE  MAE   MPE MAPE MASE ACF1
# Training set -0.01 1.34 1.01 -7.57 22.5 0.69 0.13
fit %>% forecast(method="ses") %>%
  autoplot() + autolayer(fitted(fit), series="Fitted")

## Trend methods -- Holts (damped)
fHoltMax09 <- holt(TsMax2009, damped=TRUE, h=365)
fit <- fHoltMax09
round(accuracy(fit),2)
#                ME RMSE  MAE  MPE MAPE MASE ACF1
# Training set 0.01 1.33 1.01 -7.3 22.4 0.69 0.14
fit %>% forecast(method="ses") %>%
  autoplot() + autolayer(fitted(fit), series="Fitted")

## Trend methods -- Holts & Winters seasonal
# hw(TsMax2009)
# > Error: Frequency too high

## ETS models (State Space: Error, Trend, Seasonal)
# ets(TsMax2009)
# > Error: can handle frequency greater than 24. Try stlf()...


## STL + ETS(A,N,N)
## ETS(A,N,N): simple exponential smoothing with additive errors

### TsGAP (GAP values by minute, not aggregated)
fStGAPmin <- stlf(TsGAP)
fit <- fStGAPmin
round(accuracy(fit),2)
#              ME RMSE MAE   MPE  MAPE MASE ACF1
# Training set  0 0.22 0.1 -1.42 13.58 0.12 0.04

### TsgapMaxD and TsMax2009 (agregated values by day, max)
fStl09 <- stlf(TsMax2009, h=365)
fit <- fStl09
round(accuracy(fit),2)
#                 ME RMSE  MAE   MPE  MAPE MASE ACF1
# Training set -0.02 0.93 0.73 -4.48 16.08 0.49 0.21

fStl <- stlf(TsgapMaxD, h=365)
fit <- fStl
round(accuracy(fit),2)
#                 ME RMSE  MAE  MPE  MAPE MASE ACF1
# Training set -0.01 1.14 0.89 -6.4 20.35 0.57 0.13

fit %>% forecast(method="stl") %>%
  autoplot() + autolayer(fitted(fit), series="Fitted")

## Arima models
fArima09 <- auto.arima(TsMax2009)
fit <- fArima09
round(accuracy(fit),2)
#              ME RMSE MAE   MPE  MAPE MASE ACF1
# Training set  0 1.31   1 -7.52 22.36 0.68    0

f011Arima09 <- arima(TsMax2009, c(0,1,1))
fit <- f011Arima09
round(accuracy(fit),2)
#                 ME RMSE  MAE   MPE  MAPE MASE ACF1
# Training set -0.01 1.33 1.01 -7.64 22.54 0.81 0.14
fit %>% forecast() %>%
  autoplot() + autolayer(fitted(fit), series="Fitted")

## compute mean consumed energy yearly (kWh)
# mean(gapMeanD$GAP)*24*365

# compute recommended GAP value to contract
# based on actual data and on forecast data

## Rec1 is forecast based: Count maxs of (best) model
peakst <- fStl09$upper[,2]          # upper values conf=0.95
(sort(peakst) %>% tail(2))[1] %>%   # select the 2nd biggest value
  round(2)  -> gapRec1              # 9.97
                                    # and store as 1st GAP recommendation

### Rec2 is data based: accept two blockouts in year
diffY <- (Tt08[length(Tt08)]-Tt08[length(Tt08)-365])/2 
bOuts <- round(length(St08)/365*2) 
Tt08[length(Tt08)]+diffY+
  tail(sort(St08),bOuts+1)[1]+             # take bOuts+1-th greatest value
  tail(sort(Rt08),bOuts+1)[1]   -> gapRec2 #10.02
                                     # store as 2nd recommendation  

### make a final recomendation
gapRec <- round((gapRec1 + gapRec2)/2*0.98) ## make a ponderated guess
                                            ## allow for 2% margin
                                            ## give nearest integer

