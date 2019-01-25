## preprocess1ExistingProducts.R
## 
## does a pre-processing of the data from existing_ok.csv
##
## aiming to do a linear regression model to predict volume (possibly grouped by type of product)
## - we select the cols that are relevant for the LMF
## --> c("X4StarReviews", "X2StarReviews","PositiveServiceReview","WouldRecommend", "Type", "Volume")
## - we consider if we use all rows or only the ones related to the 4 given product types PC | Laptop |Notebook | Smartphone
## --> we decide to use all rows

# load data ####
oldProd <- read.csv2('existing_ok.csv') ## EUR notation -> using csv2

# preprocessing ####

## use only relevant cols for our aim: estimating Volume by user reviews and service reviews
## we run first the program without taking any column out and use the information gained below in this program 
## from the correlation btw variables to drop the columns 
## (we can actually out comment this step in the final version. See next step)
oldProd <- oldProd[, which(names(oldProd) %in%
 c("X5StarReviews","X4StarReviews","X3StarReviews","X2StarReviews","X1StarReviews","PositiveServiceReview", "NegativeServiceReview", "WouldRecommend", "Type", "Volume"))]

## take the cols (vars) that have the highest correlation with the col $Volume (but not > 0.89)
## and drop the cols that have too strong correlation with other predictors
# oldProd <- oldProd[, which(names(oldProd) %in% 
#  c("X4StarReviews", "X2StarReviews","PositiveServiceReview","WouldRecommend", "Type", "Volume"))]

# using all rows of the data (all product types) ####

## correlation of vars
oldProd_DM <- data.matrix(oldProd) #create a data matrix from the data frame to aply cor()
corG <- cor(oldProd_DM)
plot(corG)

### we group now the rows by product type and calculate corr again (adding values)
oldProd_byType <- aggregate(. ~ Type, oldProd, sum)

### correlation grouped by type of product

# using separatelty group A: data with Type = PC | Laptop |Notebook | Smartphone and B: with none of these ####
oldProdA <- oldProd[indexA <-(oldProd$Type == 'PC' | oldProd$Type == 'Laptop' | oldProd$Type == 'Notebook' | oldProd$Type == 'Smartphone'),]
oldProdB <- oldProd[-indexA,]

## correlation of vars
oldProdA_DM <- data.matrix(oldProdB) #create a data matrix from the data
corA <- cor(oldProdA_DM)
plot(corA)

oldProdB_DM <- data.matrix(oldProdB) # now for group B of rows
corB <- cor(oldProdB_DM)
plot(corB)

#### Data obs1: we see that the correlations are almost the same for groupA, groupB and the general group
#### it means that we can build a model LMR with individual observations and all the rows, no need to sort out the other groups

#### Data Obs2: we see also that there are strong outliers out of the linear trend that can compromise the linear modelling 
#### --> we should take out this outliers in the next preprocessing

# we finally sum up the operations of preprocessing-1 to incorporate them directly at the start of the data pipe line 
## just copy-paste and uncomment

# # preprocessing - 1 ####
# 
# ## select predictors with hight correlation with estimating var y = Volume but exclude high in-correlation among predictors
# oldProd <- oldProd[, which(names(oldProd) %in% 
#     c("X4StarReviews", "X2StarReviews","PositiveServiceReview","WouldRecommend", "Type", "Volume"))]
# 
# ## create a data frame with the rows concerning the four types of product we are interested in (for later puroposes down the pipe)
# oldProdA <- oldProd[indexA <-(oldProd$Type == 'PC' | oldProd$Type == 'Laptop' | oldProd$Type == 'Notebook' | oldProd$Type == 'Smartphone'),]
