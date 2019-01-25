## trainRFexistingProductsWoFiltering.R
## 
## what the program does: ####
## 
## - 'Wo' means without filtering (taking out) product types
## - we apply the model with and without filtering outliers
## - trains a model Random Forest with data from existing_ok.csv after preprocessing steps
## - evaluates the model and compares with a simple linear regression model


# load libraries ####
# library(ggplot2)
library(caret)
# library(kernlab)

# load data ####
oldProd <- read.csv2('existing_ok.csv') ## EUR notation -> using csv2

# preprocessing ####
## prepocessing 0: type conversions, NAs and other reasons to drop variables
oldProd$WouldRecommend <- as.numeric(oldProd$WouldRecommend)/10

### drop cols with too many NAs
oldProd$Rank <- NULL

### drop col oldProd$Stock because bad predictor: we will not know stock of new products in advance
### also drop oldProd$Margin because it is not logic that it affects the sold volume of a product
oldProd$Stock <- NULL
oldProd$Margin <- NULL

## prepocessing 1: supress cols that are not relevant cols for the LM
### chose for option general:
oldProd <- oldProd[, which(names(oldProd) %in% c("X5StarReviews","X4StarReviews", "X3StarReviews", "X2StarReviews", "X1StarReviews", "WouldRecommend", "PositiveServiceReview","NegativeServiceReview", "Type", "Volume"))]
# oldProd <- oldProd[ (oldProd$Type != 'Game Console'),]

dummifyDF <- dummyVars(" ~ .", data = oldProd)
oldProd <- data.frame(predict(dummifyDF, newdata = oldProd))

## prepocessing 2: supress outliers

### Compute and plot Cook's distance (measure for detecting outliers, if C(x) > 4*mean)
### we can iterate the process (3-times)

# summary(oldProd$Type)
for (i in 1:5) { # we make 5 times straffing

  mod <- lm(oldProd$Volume ~ ., data=oldProd) # make a linear model to compute C(x) row-wise
  cooksd <- cooks.distance(mod)

  # plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
  # abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  # text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

  cooksdLabels <- ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),"") # make an index of the rows that are Cooks outliers
  oldProd[cooksdLabels <- cooksd>4*mean(cooksd, na.rm=T),]
  cooksdLabels[][is.na(cooksdLabels[])] <- FALSE                          # if NA's appear in the index, we turn them to FALSE

  # oldProd[cooksdLabels, ] # uncomment to see the outliers of the DF oldProd[]
  oldProd <- oldProd[!cooksdLabels, ] # drop the rows marked as outliers in cooksdLabels

}

## caret: we use the methods caret package offer for linear SVM and compare to LMR ####
trControl <- trainControl(method = "cv", number = 10) # training control: 10-fold cross-validation

### train model: rf(Volum ~ .), data = oldProd 
VolRF <- train(
  Volume ~ ., 
  data = oldProd,
  method = "rf",
  trControl = trControl,
  preProc = c("center","scale")
)

### train model: lm(Volum ~ ., intercept)
VolLMR <- train(
  Volume ~ ., #
  data = oldProd,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = TRUE) # intercept significant and less residuals if TRUE
)

### explore results of the LM
# s <- summary(VolLMR)
# print(s)                  

### prediction on training data LMR and RF and plotting  ####
oldProd$predVolumeLMR <- predict(VolLMR, oldProd)
oldProd$predVolumeRF <- predict(VolRF, oldProd)


## plotting results RF
plot(oldProd$Volume ~ oldProd$X5StarReviews, pch=16)
points(oldProd$predVolumeLMR ~ oldProd$X5StarReviews, col = "blue", pch=4)
points(oldProd$predVolumeRF ~ oldProd$X5StarReviews, col = "red", pch=4)

plot(oldProd$Volume ~ oldProd$PositiveServiceReview, pch=16)
points(oldProd$predVolumeLMR ~ oldProd$PositiveServiceReview, col = "blue", pch=4)
points(oldProd$predVolumeRF ~ oldProd$PositiveServiceReview, col = "red", pch=4)

## compare errors in models: LMR vs RF
mean(abs(oldProd$predVolumeLMR- oldProd$Volume))
mean(abs(oldProd$predVolumeRF- oldProd$Volume))

sqrt(mean((oldProd$predVolumeLMR- oldProd$Volume)^2))
sqrt(mean((oldProd$predVolumeRF- oldProd$Volume)^2))

# options(digits=2) # to supress cientific notation
abs(oldProd$predVolumeLMR- oldProd$Volume) 
abs(oldProd$predVolumeRF- oldProd$Volume)

oldProd$errLMR <- abs(oldProd$predVolumeLMR- oldProd$Volume) / oldProd$Volume
oldProd$errRF <- abs(oldProd$predVolumeRF- oldProd$Volume) / oldProd$Volume

oldProd[, which(names(oldProd) %in% c("Type", "Volume", "predVolumeLMR", "predVolumeRF", "errLMR", "errRF"))]

## evaluate and compare models
resamps <- resamples(list(lm = VolLMR, rf = VolRF))
summary(resamps)

#### --> Conclusions: the modelization with random forest does not yield better results (errors) than the linear model
####                  These conclusions come after applying the fitted model to the training data. No need to make predictions
####                  on the new products data set, since the model is unaccurate.




                




