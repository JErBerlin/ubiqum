## trainKNNexistingProductsWoFiltering.R
## 
## what the program does: ####
## 
## - 'Wo' means without filtering (taking out) product types. We can though uncomment the filtering part
## - we apply the model with and without filtering outliers
## - trains a model SVM with data from existing_ok.csv after preprocessing steps
## - evaluates the model and compares with a simple linear regression model

# load libraries ####
# library(ggplot2)
library(caret)
# library(e1071)
library(kernlab)

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
# 
# dummifyDF <- dummyVars(" ~ .", data = oldProd)
# oldProd <- data.frame(predict(dummifyDF, newdata = oldProd))

## prepocessing 2: supress outliers

### Compute and plot Cook's distance (measure for detecting outliers, if C(x) > 4*mean)
### we can iterate the process (3-times)

# summary(oldProd$Type)
# for (i in 1:4) {
# 
#   mod <- lm(oldProd$Volume ~ ., data=oldProd) # make a linear model to compute C(x) row-wise
#   cooksd <- cooks.distance(mod)
# 
#   # plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
#   # abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
#   # text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# 
#   cooksdLabels <- ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),"") # make an index of the rows that are Cooks outliers
#   oldProd[cooksdLabels <- cooksd>4*mean(cooksd, na.rm=T),]
#   cooksdLabels[][is.na(cooksdLabels[])] <- FALSE                          # if NA's appear in the index, we turn them to FALSE
# 
#   # oldProd[cooksdLabels, ] # uncomment to see the outliers of the DF oldProd[]
#   oldProd <- oldProd[!cooksdLabels, ] # drop the rows marked as outliers in cooksdLabels
# 
# }

## caret: we use the methods caret package offer for linear SVM and compare to LMR
trControl <- trainControl(method = "cv", number = 10) # training control: 10-fold cross-validation

### train model: svm(Volum ~ .) data = oldProd

### train model: svmLinear(Volum ~ .), data = oldProd 
VolSVM <- train(
  Volume ~ ., 
  data = oldProd,
  method = "svmLinear",
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
s <- summary(VolLMR)
print(s)                  

### prediction on training data LMR and SVM
oldProd$predVolumeLMR <- predict(VolLMR, oldProd)
oldProd$predVolumeSVM <- predict(VolSVM, oldProd)

## predictions and plotting results SVM
plot(oldProd$Volume ~ oldProd$X5StarReviews, pch=16)
points(oldProd$predVolumeLMR ~ oldProd$X5StarReviews, col = "blue", pch=4)
points(oldProd$predVolumeSVM ~ oldProd$X5StarReviews, col = "red", pch=4)

# VolSVM <- svm(Volume ~ oldProd$X5StarReviews + oldProd$X2StarReviews, oldProd)

## compare errors in models: LMR vs SVM
mean(abs(oldProd$predVolumeLMR- oldProd$Volume))
mean(abs(oldProd$predVolumeSVM- oldProd$Volume))

sqrt(mean((oldProd$predVolumeLMR- oldProd$Volume)^2))
sqrt(mean((oldProd$predVolumeSVM- oldProd$Volume)^2))

# options(digits=2)
abs(oldProd$predVolumeLMR- oldProd$Volume) 
abs(oldProd$predVolumeSVM- oldProd$Volume)

oldProd$errLMR <- abs(oldProd$predVolumeLMR- oldProd$Volume) / oldProd$Volume
oldProd$errSVM <- abs(oldProd$predVolumeSVM- oldProd$Volume) / oldProd$Volume

oldProd[, which(names(oldProd) %in% c("Type", "Volume", "predVolumeLMR", "predVolumeSVM", "errLMR", "errSVM"))]

resamps <- resamples(list(lm = VolLMR, svm = VolSVM))
summary(resamps)

#### --> Conclusions: the modelization with SVM does not yield better results (errors) than the linear model
####                  These conclusions come after applying the fitted model to the training data. No need to make predictions
####                  on the new products data set, since the model is unaccurate.





                




