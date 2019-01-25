# what the program does: ####

## Preferred independent vars: 
## Option 1: x5, x2              (with interception)
## Option 2: x4, x2, positiveRev (with interception?)

# load libraries ####
library(ggplot2)
# library(caret)
# library(e1071)
library(kernlab)

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

## iterative modelling (make modell -> take out cols or rows, look for singificant type, take it out -> make modell)
### take out rows: Type == ExtendedWarranty || Type == 'Game Console' because significant => not comparable to 4 types
oldProd5x <- oldProd5x[ (oldProd5x$Type != 'Extended Warranty') ,] # iteration 1: take out Warranty,     Pr(>|t|) = 0.021
oldProd5x <- oldProd5x[ (oldProd5x$Type != 'Software') ,]          # iteration 2: take out Software,          Pr(>|t|) = 0.081
oldProd5x <- oldProd5x[ (oldProd5x$Type != 'Printer') ,]           # iteration 3: take out Printer,           Pr(>|t|) = 0.084

## prepocessing 2: supress outliers

### Compute and plot Cook's distance (measure for detecting outliers, if C(x) > 4*mean)
### we can iterate the process (3-times)

# summary(oldProd5x$Type)
for (i in 1:4) {
  
  mod <- lm(oldProd5x$Volume ~ ., data=oldProd5x) # make a linear model to compute C(x) row-wise
  cooksd <- cooks.distance(mod)
  
  # plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
  # abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  # text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
  
  cooksdLabels <- ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),"") # make an index of the rows that are Cooks outliers
  oldProd5x[cooksdLabels <- cooksd>4*mean(cooksd, na.rm=T),]
  cooksdLabels[][is.na(cooksdLabels[])] <- FALSE                          # if NA's appear in the index, we turn them to FALSE
  
  # oldProd5x[cooksdLabels, ] # uncomment to see the outliers of the DF oldProd5x[]
  oldProd5x <- oldProd5x[!cooksdLabels, ] # drop the rows marked as outliers in cooksdLabels
  
}

## caret: we use the methods caret package offer for linear SVM and compare to LMR
trControl <- trainControl(method = "cv", number = 10) # training control: 10-fold cross-validation

### train model: svm(Volum ~ .) data = oldProd
VolSVM <- train(
  Volume ~ .,
  data = oldProd,
  method = "svmLinear",
  trControl = trControl
)

### train model: svmLinear(Volum ~ 5x, 2x), data = oldProd5x 
VolSVM_opt5x <- train(
  Volume ~ X5StarReviews + X2StarReviews, #.,# X5StarReviews + X2StarReviews, # .,#
  data = oldProd5x,
  method = "svmLinear",
  trControl = trControl,
  preProc = c("center","scale")
)

### train model: lm(Volum ~ 5x, 2x, intercept)
VolLMR_opt5x <- train(
  Volume ~ X5StarReviews + X2StarReviews, #.,# X5StarReviews + X2StarReviews, # .,#
  data = oldProd5x,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = TRUE) # intercept significant and less residuals if TRUE
)

### explore results of the LM
s <- summary(VolLMR_opt5x)
print(s)                  

### prediction on training data LMR and SVM
oldProd5x$predVolumeLMR <- predict(VolLMR_opt5x, oldProd5x)
oldProd5x$predVolumeSVM <- predict(VolSVM_opt5x, oldProd5x)
# oldProd5x$predVolumeSVM <- predict(VolSVM, oldProd5x)

## predictions and plotting results SVM

# oldProd$predVolumeSVM <- predict(VolSVM, oldProd)
# plot(oldProd$Volume ~ oldProd$X5StarReviews, pch=16)
# points(oldProd$X5StarReviews ~ oldProd$predVolumeSVM, col = "red", pch=4)

plot(oldProd5x$Volume ~ oldProd5x$X5StarReviews, pch=16)
points(oldProd5x$predVolume ~ oldProd5x$X5StarReviews, col = "blue", pch=4)
points(oldProd5x$predVolumeSVM ~ oldProd5x$X5StarReviews, col = "red", pch=4)

# VolSVM <- svm(Volume ~ oldProd5x$X5StarReviews + oldProd5x$X2StarReviews, oldProd5x)

## compare errors in models: LMR vs SVM
mean(abs(oldProd5x$predVolume- oldProd5x$Volume))
mean(abs(oldProd5x$predVolumeSVM- oldProd5x$Volume))

sqrt(mean((oldProd5x$predVolume-svm- oldProd5x$Volume)^2))
sqrt(mean((oldProd5x$predVolumeSVM- oldProd5x$Volume)^2))

# options(digits=2)
abs(oldProd5x$predVolume- oldProd5x$Volume) 
abs(oldProd5x$predVolumeSVM- oldProd5x$Volume)

oldProd5x$errLMR <- abs(oldProd5x$predVolume- oldProd5x$Volume) / oldProd5x$Volume
oldProd5x$errSVM <- abs(oldProd5x$predVolumeSVM- oldProd5x$Volume) / oldProd5x$Volume

oldProd5x[, which(names(oldProd5x) %in% c("Type", "Volume", "predVolume", "predVolumeSVM", "errLMR", "errSVM"))]

resamps <- resamples(list(lm5x = VolLMR_opt5x, svm = VolSVM))
summary(resamps)

#### --> Conclusions  
####  




                




