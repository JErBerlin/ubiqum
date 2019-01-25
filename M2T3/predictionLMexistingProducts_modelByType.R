# what the program does: ####

## Preferred independent vars: 
## Option 1: x5, x2              (with interception?)
## Option 2: x4, x2, positiveRev (with interception?)

# load libraries ####
library(ggplot2)
library(caret)

# load data ####
oldProd <- read.csv2('existing_ok.csv') ## EUR notation -> using csv2

# preprocessing ####
## prepocessing 0: type conversions, NAs and other reasons to drop variables

### drop cols with too many NAs
oldProd$Rank <- NULL

### drop col oldProd$Stock because bad predictor: we will not know stock of new products in advance
### also drop oldProd$Margin because it is not logic that it affects the sold volume of a product
oldProd$Stock <- NULL
oldProd$Margin <- NULL

## prepocessing 1: supress cols that are not relevant cols for the LM

#### chose for option 5x:
oldProd5x <- oldProd[, which(names(oldProd) %in% c("X5StarReviews","X2StarReviews", "Type", "Volume"))]

#### chose for option 4x:
oldProd4x <- oldProd[, which(names(oldProd) %in% c("X4StarReviews","X2StarReviews", "PositiveServiceReview", "Type", "Volume"))]

## iterative modelling (make modell -> take out cols or rows, look for singificant type, take it out -> make modell)

### take out rows because Type significant => not comparable to 4 types of Group A
oldProd5x <- oldProd5x[ (oldProd5x$Type != 'Extended Warranty' & oldProd5x$Type != 'Software' & oldProd5x$Type != 'Printer'),] 
oldProd4x <- oldProd4x[ (oldProd4x$Type != 'Game Console' & oldProd4x$Type != 'Extended Warranty'),]      

# Group by Type
oldProd5x_Type <- aggregate(. ~ Type, oldProd5x, sum) # aggregate by Type (sum)
oldProd4x_Type <- aggregate(. ~ Type, oldProd4x, sum)

# train model ####

## caret: we use the methods caret package offer for LRM with cross-validation
trControl <- trainControl(method = "cv", number = 10) # training control: 10-fold cross-validation

### train model: lm(Volum ~ 5x, 2x, no intercept)
VolLMR_opt5x_Type <- train(
  Volume ~ X5StarReviews + X2StarReviews, #.,# X5StarReviews + X2StarReviews, # .,#
  data = oldProd5x_Type,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = TRUE) 
)

### train model: lm(Volum ~ 4x, 2x, posRev, no intercept)
VolLMR_opt4x_Type <- train(
  Volume ~ X4StarReviews + X2StarReviews + PositiveServiceReview, # .,# X4StarReviews + X2StarReviews + PositiveServiceReview, # ., # 
  data = oldProd4x,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = FALSE) 
)

# explore results of the LM ####
s <- summary(VolLMR_opt5x_Type)
print(s)                  
# 
s <- summary(VolLMR_opt4x_Type)
print(s)

# predict ####

## 5x model
## make copy of DF to add predicition col
pOldProd5x_Type <- oldProd5x_Type

## apply model to training data and compare
## create new col 'predVolume' with the predicted values for Volume
pOldProd5x_Type$predVolume <- round(predict(VolLMR_opt5x_Type, oldProd5x_Type))

# errors and interpretation ####
errPred5x <- pOldProd5x_Type$predVolume - pOldProd5x_Type$Volume
absErrPred5x <- abs(pOldProd5x_Type$predVolume - pOldProd5x_Type$Volume)
relErrPred5x <- (pOldProd5x_Type$predVolume - pOldProd5x_Type$Volume)/pOldProd5x_Type$Volume

# metrics of the prediction 
postResample(pOldProd5x_Type$predVolume, pOldProd5x_Type$Volume)

## 4x model
## make copy of DF to add predicition col
pOldProd4x_Type <- oldProd4x_Type

## apply model to training data and compare
## create new col 'predVolume' with the predicted values for Volume
pOldProd4x_Type$predVolume <- round(predict(VolLMR_opt4x_Type, oldProd4x_Type))

# errors and interpretation ####
errPred4x <- pOldProd4x_Type$predVolume - pOldProd4x_Type$Volume
absErrPred4x <- abs(pOldProd4x_Type$predVolume - pOldProd4x_Type$Volume)
relErrPred4x <- (pOldProd4x_Type$predVolume - pOldProd4x_Type$Volume)/pOldProd4x_Type$Volume

# metrics of the prediction 
postResample(pOldProd4x_Type$predVolume, pOldProd4x_Type$Volume)

