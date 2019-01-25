# what the program does: ####
#
# Analysis of the predictors, their coefficients and significance with different choses of vars:
# (data form existing_ok.csv)
#
# Option 1: (Volum ~ 5x, 4x, 2x, negative, postive) --- general model, Achtung: colinearity!
# Option 2: (Volum ~ 5x, 2x, negative, postive)     --- To make estimations about 5x (we let 4x out)
# Option 3: (Volum ~ 4x, 2x, negative, postive)     --- about 4x (we let 5x out) 
# Option 4: (Volum ~ 5x, 4x, 2x, postive)           --- about 2x (we let 'negative' out)
# Option 5: (Volum ~ 5x, 4x, negative, positive)    --- about 'negative' (we let 2x out)


# load libraries ####
library(ggplot2)
library(caret)

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

## prepocessing 1: supress cols that are not relevant cols for the LM and for every given predictor, let the-most-correlated one out

#### chose for option Lin:
oldProdPol <- oldProd[, which(names(oldProd) %in% c("X5StarReviews","X4StarReviews","X2StarReviews","PositiveServiceReview","NegativeServiceReview","WouldRecommend","Type","Volume"))]
oldProdLin <- oldProd[, which(names(oldProd) %in% c("X5StarReviews","X4StarReviews","X2StarReviews","PositiveServiceReview","NegativeServiceReview","Type","Volume"))]
oldProdLin5x <- oldProd[, which(names(oldProd) %in% c("X5StarReviews","X2StarReviews","PositiveServiceReview","NegativeServiceReview","Type","Volume"))]
oldProdLin4x <- oldProd[, which(names(oldProd) %in% c("X4StarReviews","X2StarReviews","PositiveServiceReview","NegativeServiceReview","Type","Volume"))]
oldProdLin2x <- oldProd[, which(names(oldProd) %in% c("X5StarReviews","X4StarReviews","X2StarReviews","PositiveServiceReview","Type","Volume"))]
oldProdLinNeg <- oldProd[, which(names(oldProd) %in% c("X5StarReviews","X4StarReviews","PositiveServiceReview","NegativeServiceReview","Type","Volume"))]

## caret: we use the methods caret package offer for LRM with cross-validation
trControl <- trainControl(method = "cv", number = 10) # training control: 10-fold cross-validation

VolLMR_pol <- train(
  Volume ~ poly(X5StarReviews + X4StarReviews + X2StarReviews + PositiveServiceReview + NegativeServiceReview,2), 
  data = oldProdPol,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = FALSE) # intercept significant and less residuals if TRUE
)

### train model: lm(Volum ~ 5x, 4x, 2x, negative, postive)
VolLMR_lin <- train(
  Volume ~ X5StarReviews + X4StarReviews + X2StarReviews + PositiveServiceReview + NegativeServiceReview, 
  data = oldProdLin,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = FALSE) # intercept significant and less residuals if TRUE
)

### train model: lm(Volum ~ 5x, 2x, negative, postive)
VolLMR_lin5x <- train(
  Volume ~ X5StarReviews + X2StarReviews + NegativeServiceReview + PositiveServiceReview, 
  data = oldProdLin5x,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = FALSE) # intercept significant and less residuals if TRUE
)

### train model: lm(Volum ~ 4x, 2x, negative, postive)
VolLMR_lin4x <- train(
  Volume ~ X4StarReviews + X2StarReviews + PositiveServiceReview + NegativeServiceReview, 
  data = oldProdLin4x,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = FALSE) # intercept significant and less residuals if TRUE
)

### train model: lm(Volum ~ 5x, 4x, 2x, postive)
VolLMR_lin2x <- train(
  Volume ~ X5StarReviews + X4StarReviews + X2StarReviews + PositiveServiceReview, 
  data = oldProdLin2x,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = FALSE) # intercept significant and less residuals if TRUE
)

### train model: lm(Volum ~ 5x, 4x, negative, positive)
VolLMR_linNeg <- train(
  Volume ~ X5StarReviews + X4StarReviews + PositiveServiceReview + NegativeServiceReview, 
  data = oldProdLinNeg,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = FALSE) # intercept significant and less residuals if TRUE
)

### explore results of the LM
s <- summary(VolLMR_pol)
print(s)                  
capture.output(s, file = "summLMRVolPol_5x4x2xPosNeg.txt") 


### summaries and write to file
s <- summary(VolLMR_lin)
# print(s)                  
capture.output(s, file = "summLMRVolLIn_5x4x2xPosNeg.txt") 

s <- summary(VolLMR_lin5x)
# print(s)                  
capture.output(s, file = "summLMRVolLIn_f5x.txt") 

s <- summary(VolLMR_lin4x)
# print(s)                  
capture.output(s, file = "summLMRVolLIn_f4x.txt") 

s <- summary(VolLMR_lin2x)
# print(s)                  
capture.output(s, file = "summLMRVolLIn_f2x.txt") 

s <- summary(VolLMR_linNeg)
# print(s)                  
capture.output(s, file = "summLMRVolLIn_fNeg.txt") 

#### --> Conclusions Lin general model: Multiple R-squared:  0.9902,	Adjusted R-squared:  0.9896 
####                                    Residual standard error: 156.5
####
#### --> About the coefficients:        coef(5x) is approx.   4.2 +- 0.25 i.e. coef(5x) in (3.95, 4.45) 
#### -->                                coef(4x) not possible to tell 
####                                    coef(2x) not possible to tell
####                                    coef(pos) not possible to tell
####                                    coef(neg) is approx. -14.9 +- 3.25 i.e. coef(neg) in (-11.65, -18.15)


