# what the program does: ####

# load libraries ####
library(ggplot2)

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
### Preferred vars: we can take as possible predictors x4, x2, positiveRev, negativeRev and wouldRec
### Another option: x5, 2x, positiveRev, negativeRev and wouldRec
# oldProd <- oldProd[, which(names(oldProd) %in%
#  c("X5StarReviews","X4StarReviews","X3StarReviews","X2StarReviews","X1StarReviews","PositiveServiceReview", "NegativeServiceReview", "WouldRecommend", "Type", "Volume"))]
oldProd <- oldProd[, which(names(oldProd) %in%
 c("X5StarReviews","X2StarReviews","X1StarReviews","PositiveServiceReview", "NegativeServiceReview", "WouldRecommend", "Type", "Volume"))]

### group A: Type = PC | Laptop |Notebook | Smartphone and group B: with none of these ####
oldProdA <- oldProd[indexA <-(oldProd$Type == 'PC' | oldProd$Type == 'Laptop' | oldProd$Type == 'Notebook' | oldProd$Type == 'Smartphone'),]
oldProd_byType <- aggregate(. ~ Type, oldProd, sum)
oldProdA_byType <- aggregate(. ~ Type, oldProdA, sum)

# exploratory data analysis
boxplot(Volume ~ Type, data=oldProd) # Volume vs type
boxplot(Volume~ Type, data=oldProdA) # Volume vs type, group A


