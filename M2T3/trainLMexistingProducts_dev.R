# what the program does: ####

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

### convert relevant factors to numeric
oldProd$WouldRecommend = as.numeric(oldProd$WouldRecommend)

## prepocessing 1: supress cols that are not relevant cols for the LM and make build new useful DFs for later use
### Preferred vars: we can take as possible predictors x4, x2, positiveRev and wouldRec
### Another option: x5, 2x, positiveRev and wouldRec

#### take out rows: Type == ExtendedWarranty || Type == 'Extended Warranty'
oldProd <- oldProd[(oldProd$Type != 'Extended Warranty' & oldProd$Type != 'Game Console'),]

#### only opt5x:
#### also take out Type == 'Printer' for the opt5x, final chose, no intercept
#### because it seems to be slightly significant too ( Pr(>|t|) = 0.0812 .)
# oldProd <- oldProd[(oldProd$Type != 'Printer') & (oldProd$Type!= 'Software'),]

#### final chose 5x:
# oldProd <- oldProd[, which(names(oldProd) %in% c("X5StarReviews","X2StarReviews", "Type", "Volume"))]

#### final chose 4x:
# oldProd <- oldProd[, which(names(oldProd) %in% c("X4StarReviews","X2StarReviews", "PositiveServiceReview", "Type", "Volume"))]


### group A: Type = PC | Laptop |Notebook | Smartphone and group B: with none of these ####
# oldProdA <- oldProd[indexA <-(oldProd$Type == 'PC' | oldProd$Type == 'Laptop' | oldProd$Type == 'Notebook' | oldProd$Type == 'Smartphone'),]
# oldProd_byType <- aggregate(. ~ Type, oldProd, sum)
# oldProdA_byType <- aggregate(. ~ Type, oldProdA, sum)

## standard: make a linear model for Volume with standard lm(): lm(Volume ~ .) # point after '~' means all variables as predictors)
# VolLMR <- lm(oldProd$Volume ~., data = oldProd, na.action = na.exclude)

### explore results of the LM
# s <- summary(VolLMR)
# print(s)                                           
# capture.output(s, file = "summaryVolLMR_opt5x.txt")   # first version (without iteration)
# capture.output(s, file = "summaryVolLMR_opt5xfinal.txt") # final version, chose (5x, 2x) only

 # VolLMR <- lm(oldProd$Volume ~ oldProd$X5StarReviews + oldProd$X2StarReviews, data = oldProd, na.action = na.exclude)
 # s <- summary(VolLMR)
 # print(s) 
# capture.output(s, file = "summaryVolLMR_opt5xfinalRestricted.txt") # final version, chose (5x, 2x) and restricted to those

# capture.output(s, file = "summaryVolLMR_opt4x.txt")   # first version (without iteration)
# capture.output(s, file = "summaryVolLMR_opt4xfinal.txt") # final version, chose (4x, 2x, positiveR)

# VolLMR <- lm(oldProd$Volume ~ oldProd$X4StarReviews + oldProd$X2StarReviews + oldProd$PositiveServiceReview, data = oldProd, na.action = na.exclude)
# s <- summary(VolLMR)
# print(s) 
# capture.output(s, file = "summaryVolLMR_opt4xfinalRestricted.txt") # final version, and restricted

### --> conclusion 5x: we just need (5x, 2x), because the other attributes are not significant
###     the R2 = 
###     also: the TypeWarranty is also significant, we may evaluate taking Warranty products out of the model

### --> conclusion 5x: (5x, 2x)            with Multiple R-squared:  0.9817,	Adjusted R-squared:  0.9781
###                                             Residual standard error: 189.2
### --- Restricted:                        with Multiple R-squared:  0.9804,	Adjusted R-squared:  0.9798 
###                                             Residual standard error: 181.7
### --> conclusion 4x: (4x, 2x, positiveR) with Multiple R-squared:  0.8154,	Adjusted R-squared:  0.7751
###                                             Residual standard error: 606.1
### --- Restricted:                        with Multiple R-squared:  0.8096,	Adjusted R-squared:  0.8007
###                                             Residual standard error: 570.6  
###

## caret: now we use the methods caret package offer for LRM
trControl <- trainControl(method = "cv", number = 10) # training control: 10-fold cross-validation

### train model: lm(Volum ~.)
VolLMR_caret <- train(
  Volume ~ .,
  data = oldProd,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = TRUE)
)

### train model: lm(Volum ~ 5x, 2x, intercept)
# VolLMR_caret <- train(
#   Volume ~ X5StarReviews + X2StarReviews,
#   data = oldProd,
#   method = "lm",
#   trControl = trControl,
#   tuneGrid  = expand.grid(intercept = TRUE)
# )

### train model: lm(Volum ~ 4x, 2x, intercept)
VolLMR_caret <- train(
  Volume ~ X4StarReviews + X2StarReviews + PositiveServiceReview,
  data = oldProd,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = TRUE)
)

### explore results of the LM
s <- summary(VolLMR_caret)
print(s)                                              # print it in screen
capture.output(s, file = "summaryVolLMR5x_caret.txt") # also write it in a file
# capture.output(s, file = "summaryVolLMR4x_caret.txt") # also write it in a file


#### --> Conclusions 5x, caret: Multiple R-squared:  0.9817,	Adjusted R-squared:  0.9781 
####     Residual standard error: 189.2
#### --> Conclusions 4x, caret: Multiple R-squared:  0.8154,	Adjusted R-squared:  0.7751 
####     Residual standard error: 606.1



