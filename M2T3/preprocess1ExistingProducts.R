## preprocess1ExistingProducts.R
## 
## does a pre-processing of the data from existing_ok.csv
##
## aiming to do a linear regression model to predict volume (possibly grouped by type of product)
##
## - we select the cols that are relevant for the LMF
## --> c("X4StarReviews", "X2StarReviews","PositiveServiceReview","WouldRecommend", "Type", "Volume")
##
## - we consider if we use all rows for the modelling (group G = group A + group B) 
##   or only the ones related to the given 4 types of product  (group A)
##   types PC | Laptop |Notebook | Smartphone 
## --> we decide to use more types than the only 4, since otherwise we would not have enough data left
##
## - we consider if we have to let some specific type of group B out, in case it has a too much influence on the model
## --> we decide to take some types out, since some have significant coefficient in the linear model. We can
##   check later again after making some more preprocessing
##   (most influence has Type: ext Warranty)

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

#### make a linear model for Volume with all rows: lm(Volume ~ .) # point after '~' means all variables as predictors)
VolLMR_allRow <- lm(oldProd$Volume ~., data = oldProd)

#### explore results of the LM
# s <- summary(VolLMR_allRow)
# print(s)                                              # print it in screen
# capture.output(s, file = "summaryMVolLMR_allRow.txt") # also write it in a file

#### --> conclusion: most of the attributes are not significant for an lm-model
#### --> we should take out Type Warranty, because is too significant (there is indeed some irregular issue in the data)
####     also take out Type Game Console, because it gets significant (later) and we identified it as special case in the box plot

#### take out rows: Type = ExtendedWarranty || Type == 'Extended Warranty'
oldProd <- oldProd[(oldProd$Type != 'Extended Warranty' & oldProd$Type != 'Game Console'),]

### we build group A and group B data set and model again: data with 
### Type = PC | Laptop |Notebook | Smartphone and group B: with none of these ####
oldProdA <- oldProd[indexA <-(oldProd$Type == 'PC' | oldProd$Type == 'Laptop' | oldProd$Type == 'Notebook' | oldProd$Type == 'Smartphone'),]
oldProdB <- oldProd[-indexA,]

#### we repeat the same modelling operation we used for all rows, now to groupA data set (only 4 types of product)
VolLMR_A <- lm(oldProdA$Volume ~., data = oldProdA)
# s <- summary(VolLMR_A)
# print(s)

#### the same for groupB data set (rest of types of product)
VolLMR_B <- lm(oldProdB$Volume ~., data = oldProdB)
# s <- summary(VolLMR_B)
# print(s) 

#### --> conclusion: we have to use all data rows (if not, we don't get enough data to model)
#### We drop the least significant attribute(s) and itereate the process

### using rows of the data grouped by product: having dropped some cols, we repeat the modelling ####
### but now grouping by Type

#### group G (all) we group now the rows by product type (adding values) and calculate LM again 
oldProd_byType <- aggregate(. ~ Type, oldProd, sum) 
oldProdA_byType <- aggregate(. ~ Type, oldProdA, sum)  # we make it also for group A, for the predictions later

VolLMR_byType <- lm(oldProd_byType$Volume ~ oldProd_byType$X5StarReviews + oldProd_byType$X4StarReviews +
 oldProd_byType$X3StarReviews + oldProd_byType$X2StarReviews + oldProd_byType$X1StarReviews + 
 oldProd_byType$PositiveServiceReview + oldProd_byType$NegativeServiceReview , data = oldProd_byType)

# s <- summary(VolLMR_byType)
# print(s)                                              # print it in screen
# capture.output(s, file = "summaryMVolLMR_byType.txt") # also write it in a file

## correlation of vars: we finally investigate correlation between the remaining vars
oldProdDM <- data.matrix(oldProd) #create a data matrix from the data frame to aply cor()
s <- cor(oldProdDM)
# print(s)                                              # print it in screen
# capture.output(s, file = "corOldProd.txt")            # also write it in a file

### correlation grouped by type of product
oldProd_byTypeDM <- data.matrix(oldProd_byType) #create a data matrix from the data frame to aply cor()
s <- cor(oldProd_byTypeDM)
# print(s)                                              # print it in screen
# capture.output(s, file = "corOldProdByType.txt")      # also write it in a file

### --> conclusion: we can take as possible predictors opt4x: (x4, x2, positiveRev and wouldRec)
###    Another option opt5x: (x5, 2x, positiveRev and wouldRec)
### But we will do this selection later, when we optimize the linear model iteratively

######################################################################################################################

# we finally sum up the operations of preprocessing-1 to incorporate them directly at the start of the data pipe line 
## just copy-paste and uncomment

# ## prepocessing 0: type conversions, NAs and other reasons to drop variables
#
# ### drop cols with too many NAs
# oldProd$Rank <- NULL
#
# ### drop col oldProd$Stock because bad predictor: we will not know stock of new products in advance
# ### also drop oldProd$Margin because it is not logic that it affects the sold volume of a product
# oldProd$Stock <- NULL
# oldProd$Margin <- NULL
#
# ### convert factors that were numeric in origin to numeric
# oldProd$WouldRecommend = as.numeric(oldProd$WouldRecommend)
#
# ## prepocessing 1: supress cols that are not relevant cols for the LM and make build new useful DFs for later use
# oldProd <- oldProd[, which(names(oldProd) %in%
#  c("X5StarReviews","X4StarReviews","X3StarReviews","X2StarReviews","X1StarReviews","PositiveServiceReview", "NegativeServiceReview", "WouldRecommend", "Type", "Volume"))]
#
# #### take out rows: Type = ExtendedWarranty || Type == 'Extended Warranty'
# oldProd <- oldProd[(oldProd$Type != 'Extended Warranty' & oldProd$Type != 'Game Console'),]
# 
# ### group A: Type = PC | Laptop |Notebook | Smartphone and group B: with none of these ####
# oldProdA <- oldProd[indexA <-(oldProd$Type == 'PC' | oldProd$Type == 'Laptop' | oldProd$Type == 'Notebook' | oldProd$Type == 'Smartphone'),]
# oldProd_byType <- aggregate(. ~ Type, oldProd, sum)
# oldProdA_byType <- aggregate(. ~ Type, oldProdA, sum)

