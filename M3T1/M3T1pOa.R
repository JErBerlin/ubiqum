# load libs
library(ggplot2)

# 1 -- load the data
## read table with formatting. The charcater "?" is interpreted as NA
cns=read.table(
  file ="household_power_consumption.txt", 
  header=T, 
  sep=";", 
  colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), 
  na.strings="?")

# 2 -- preprocess
## 2.2 -- Sample the data if appropriate
