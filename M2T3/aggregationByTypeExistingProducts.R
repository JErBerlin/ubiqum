## aggregationByTypeExistingProducts.R
## 
## - does a pre-processing of the data from existing_ok.csv 
## - divides the data in two groups 4 types (PC, laptop, notebook,smartphone) and the rest
## - aggregates by Type
##


# load data ####
oldProd <- read.csv2('existing_ok.csv') ## EUR notation -> using csv2

# preprocessing ####

## prepocessing 0: type conversions, NAs and other reasons to drop variables

### convert factors that were numeric in origin to numeric
oldProd$Price = as.numeric(oldProd$Price)
oldProd$WouldRecommend = as.numeric(oldProd$WouldRecommend)
oldProd$Weight = as.numeric(oldProd$Weight )
oldProd$Width = as.numeric(oldProd$Width )
oldProd$Depth = as.numeric(oldProd$Depth )
oldProd$Height = as.numeric(oldProd$Height )
oldProd$Margin = as.numeric(oldProd$Margin )

### drop cols with too many NAs
oldProd$Rank <- NULL

### drop col oldProd$Stock because bad predictor: we will not know stock of new products in advance
### also drop oldProd$Margin because it is not logic that it affects the sold volume of a product
oldProd$Stock <- NULL
oldProd$Margin <- NULL

## prepocessing 1:

### use only relevant cols for our aim: estimating Volume by user reviews and service reviews
### we run first the program without taking any more column out and use the information gained 
### below in this program from the preliminar LM to drop the most clearly not relevant columns 
oldProd <- oldProd[, which(names(oldProd) %in%
  c("X5StarReviews","X4StarReviews","X3StarReviews","X2StarReviews","X1StarReviews","PositiveServiceReview", "NegativeServiceReview", "WouldRecommend", "Type", "Volume"))]

### using separatelty group A: data with Type = PC | Laptop |Notebook | Smartphone and B: with none of these ####
oldProdA <- oldProd[indexA <-(oldProd$Type == 'PC' | oldProd$Type == 'Laptop' | oldProd$Type == 'Notebook' | oldProd$Type == 'Smartphone'),]
oldProdB <- oldProd[-indexA,]

### using rows of the data grouped by product ####

#### we group now the rows by product type (adding values)
oldProd_byType <- aggregate(. ~ Type, oldProd, sum)

#### group A (4 types) we group now the rows by product type
oldProdA_byType <- aggregate(. ~ Type, oldProdA, sum)

#### group B (rest of types) we group now the rows by product type
oldProdB_byType <- aggregate(. ~ Type, oldProdB, sum)


