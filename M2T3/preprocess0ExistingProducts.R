## preprocess1ExistingProducts.R
## 
## does a pre-processing of the data from existing_ok.csv
##
## aiming to do a linear regression model to predict volume (possibly grouped by type of product)
## we make type cast, take out the cols with too many NAs and other cols after consideration of the real object
## that we are modelling 

# load data ####
oldProd <- read.csv2('existing_ok.csv') ## EUR notation -> using csv2

# preprocessing ####

## convert factors that were numeric in origin to numeric
oldProd$Price = as.numeric(oldProd$Price)
oldProd$WouldRecommend = as.numeric(oldProd$WouldRecommend)/10
oldProd$Weight = as.numeric(oldProd$Weight)
oldProd$Width = as.numeric(oldProd$Width)
oldProd$Depth = as.numeric(oldProd$Depth)
oldProd$Height = as.numeric(oldProd$Height)
oldProd$Margin = as.numeric(oldProd$Margin)

## drop cols with too many NAs
oldProd$Rank <- NULL

## drop col oldProd$Stock because bad predictor: we will not know stock of new products in advance
## also drop oldProd$Margin because it is not logic that it affects the sold volume of a product
oldProd$Stock <- NULL
oldProd$Margin <- NULL

