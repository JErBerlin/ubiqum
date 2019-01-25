# Initialize, read data and pre-format ####

## load libraries 
# library(dplyr)
library(scales)
library(ggplot2)
library(dplyr)
library(stats)

## read table with formatting. The charcater "?" is interpreted as NA
consumeTb=read.table(
  file ="household_power_consumption.txt", 
  header=T, 
  sep=";", 
  colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), 
  na.strings="?")

## past Date and Time together and write as a new col at the end
consumeTb <-cbind(consumeTb,paste(consumeTb$Date,consumeTb$Time), stringsAsFactors=FALSE)

## name new col
colnames(consumeTb)[10] <-"DateTime"

## change order of columns
consumeTb <- consumeTb[,c(ncol(consumeTb), 1:(ncol(consumeTb)-1))]

## use time converting functions to create a time var
## we set reading format to GMT to avoid problems with seasonal time shift 
consumeTb$DateTime <- strptime(consumeTb$DateTime, format="%d/%m/%Y %H:%M:%S", tz="GMT")

## we need to convert dates to POSIXct format to work with some libraries like ggplot
consumeTb$DateTime <- as.POSIXct(consumeTb$DateTime)

## we convert also col Date from text to date object
consumeTb$Date <- as.Date(consumeTb$Date, "%d/%m/%Y")

## we drop the columns we don't need: reactive power, voltage are set to NULL
consumeTb$Global_reactive_power <- NULL
consumeTb$Voltage <- NULL

## temporarily keep only Global_Active_Power
consumeTb$Global_intensity <- NULL
consumeTb$Sub_metering_1 <- NULL
consumeTb$Sub_metering_2 <- NULL
consumeTb$Sub_metering_3 <- NULL

# START exploration ####

## summaries and quantitative exploration
summary(consumeTb$Global_active_power)

## histogram of Global Active Power
ggplot(data=consumeTb, aes(Global_active_power))+
  geom_histogram(binwidth = 1)+
  xlab("Global Active Power (kilowatts)")+
  ggtitle("Global Active Power")+
  theme(panel.background = element_rect(fill = rgb(240, 230, 200, maxColorValue = 255)))

### peak of Global_active_power > 10 KW
consumeTb[consumeTb$Global_active_power>10 & !is.na(consumeTb$Global_active_power),]

## inspect Global Active Power for one week (23/2/2009 - 01/03/2009) 

consumeW1=consumeTb[consumeTb$Date>="2009-02-23" & consumeTb$Date<="2009-03-01",]

ggplot(data=consumeW1, aes(x=DateTime, y=Global_active_power, color=Global_active_power))+
  geom_line()+
  xlab("Day/Time")+
  ylab("Global Active Power (kilowatts)")+
  ggtitle("Global Active Power by Time for 1 Week feb-march 2009")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(240, 230, 200, maxColorValue = 255)))

## inspect Submeter 1 Energy (Kitchen) for one week (23/2/2009 - 01/03/2009) 

ggplot(data=consumeW1, aes(x=DateTime, y=Sub_metering_1, color=Global_active_power))+
  geom_line()+
  xlab("Day/Time")+
  ylab("Kitchen Energy (watt-hour)")+
  ggtitle("Submeter 1 Energy (Kitchen) by Time for 1 Week feb-march 2009")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(240, 200, 200, maxColorValue = 255)))

## inspect Submeter 2 Energy (Laundry) for one week (23/2/2009 - 01/03/2009) 

ggplot(data=consumeW1, aes(x=DateTime, y=Sub_metering_2, color=Global_active_power))+
  geom_line()+
  xlab("Day/Time")+
  ylab("Laundry Energy (watt-hour)")+
  ggtitle("Submeter 2 Energy (Laundry) by Time for 1 Week feb-march 2009")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(180, 210, 240, maxColorValue = 255)))

## inspect Submeter 3 Energy (Service) for one week (23/2/2009 - 01/03/2009) 

ggplot(data=consumeW1, aes(x=DateTime, y=Sub_metering_3, color=Global_active_power))+
  geom_line()+
  xlab("Day/Time")+
  ylab("Service Energy (watt-hour)")+
  ggtitle("Submeter 3 Energy (Service) by Time for 1 Week feb-march 2009")+
  scale_x_datetime(breaks = date_breaks("1 day"),labels = date_format("%a %H:%M"))+
  theme(panel.background = element_rect(fill = rgb(200, 230, 200, maxColorValue = 255)))

ggplot(data=consumeW1, aes(x=DateTime, y=Sub_metering_3, color='SM3'))+
  geom_line()+
  geom_line(aes(x=DateTime, y=Sub_metering_2, color='SM2'))+
  geom_line(aes(x=DateTime, y=Sub_metering_1, color='SM1'))

