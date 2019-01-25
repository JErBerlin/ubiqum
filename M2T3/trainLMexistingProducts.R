## trainLMexistingProducts.R
## 
## what the program does: ####
## 
## - trains a linear regression model with data from existing_ok.csv after preprocessing steps
## - we choose a group of mutually independent predictors
## - we let out some rows belonging to types that behave differently from the 4 types we are interested in: PC, laptop, netbook and smartphone
## - we apply the model with and without filtering outliers (cooks distance)
## - we use cross-validation to estimate accuracy (not for parameter tuning, since there is none) 
##   but we don't split data in training/test
## - finally we evaluate the model and calculate its metrics and the errors on the training set (not yet on the test set)


## Preferred independent vars: 
## Option 1: x5, x2              (with interception)
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

## prepocessing 1: supress attributes and observations that are not relevant for the LM
### chose for option 5x:
oldProd5x <- oldProd[, which(names(oldProd) %in% c("X5StarReviews","X2StarReviews", "Type", "Volume"))]

### chose rows: take out rows corresponding to types that are not comparable to the four types we are stimating 
### take out: Type == ExtendedWarranty || Type == 'Game Console' because significant => not comparable to 4 types
### iterative modelling (make modell -> take out cols or rows, look for singificant type, take it out -> make modell)
oldProd5x <- oldProd5x[ (oldProd5x$Type != 'Extended Warranty') ,] # iteration 1: take out Warranty,          Pr(>|t|) = 0.021
oldProd5x <- oldProd5x[ (oldProd5x$Type != 'Software') ,]          # iteration 2: take out Software,          Pr(>|t|) = 0.081
oldProd5x <- oldProd5x[ (oldProd5x$Type != 'Printer') ,]           # iteration 3: take out Printer,           Pr(>|t|) = 0.084

## prepocessing 2: supress outliers

### Compute and plot Cook's distance (measure for detecting outliers, if C(x) > 4*mean)
### we can iterate the process (we do it 4-times, until outliers are not that strong)

# summary(oldProd5x$Type)
for (i in 1:4) {
  
  mod <- lm(oldProd5x$Volume ~ ., data=oldProd5x) # make a linear model to compute C(x) row-wise
  cooksd <- cooks.distance(mod)
  
  plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
  abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
  
  cooksdLabels <- ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),"") # make an index of the rows that are Cooks outliers
  oldProd5x[cooksdLabels <- cooksd>4*mean(cooksd, na.rm=T),]
  cooksdLabels[][is.na(cooksdLabels[])] <- FALSE                          # if NA's appear in the index, we turn them to FALSE
  
  oldProd5x[cooksdLabels, ] # uncomment to see the outliers of the DF oldProd5x[]
  oldProd5x <- oldProd5x[!cooksdLabels, ] # drop the rows marked as outliers in cooksdLabels
  
}

## modelling: we use the methods caret package offer for LRM with cross-validation ####
trControl <- trainControl(method = "cv", number = 10) # training control: 10-fold cross-validation

### train model: lm(Volum ~ 5x, 2x, intercept)
VolLMR_opt5x <- train(
  Volume ~ X5StarReviews + X2StarReviews, #.,# X5StarReviews + X2StarReviews, # .,#
  data = oldProd5x,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = TRUE) # intercept significant and less residuals if TRUE
)

### explore and print results of the LM
# s <- summary(VolLMR_opt5x)
# print(s)                  
capture.output(s, file = "summLMR5x_caret3.txt") 

### prediction on training data ####
oldProd5x$predVolume <- predict(VolLMR_opt5x, oldProd5x)

### plots: scatter plot of observed and predicted volumes on two predictors charts ####
plot(oldProd5x$Volume ~ oldProd5x$X5StarReviews, pch=16)
points(oldProd5x$predVolume ~ oldProd5x$X5StarReviews, col = "blue", pch=4)

plot(oldProd5x$Volume ~ oldProd5x$X2StarReviews, pch=16)
points(oldProd5x$predVolume ~ oldProd5x$X2StarReviews, col = "blue", pch=4)

#### --> Conclusions 5x, caret: Multiple R-squared:  0.9803,	Adjusted R-squared:  0.9793 ####
####                            Residual standard error: 66.49




