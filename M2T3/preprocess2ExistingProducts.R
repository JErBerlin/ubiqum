## preprocess2ExistingProducts.R
## 
## does a pre-processing of the data from existing_ok.csv
##
## - aiming to do a linear regression model to predict volume
## - our preferred variables are (see preprocess 1): 
##   option 5x: (5x, 2x, positiveRev, wouldRec)
##   option 4x: (4x, 2x, positiveRev, wouldRec)
## - we will take out outliers iteratively using the cooks distance as a criterium

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
oldProd <- oldProd[, which(names(oldProd) %in%
                             c("X5StarReviews","X4StarReviews","X3StarReviews","X2StarReviews","X1StarReviews","PositiveServiceReview", "NegativeServiceReview", "WouldRecommend", "Type", "Volume"))]

#### take out rows: Type = ExtendedWarranty || Type == 'Extended Warranty'
oldProd <- oldProd[(oldProd$Type != 'Extended Warranty' & oldProd$Type != 'Game Console'),]

### group A: Type = PC | Laptop |Notebook | Smartphone and group B: with none of these ####
oldProdA <- oldProd[indexA <-(oldProd$Type == 'PC' | oldProd$Type == 'Laptop' | oldProd$Type == 'Notebook' | oldProd$Type == 'Smartphone'),]
oldProd_byType <- aggregate(. ~ Type, oldProd, sum)
oldProdA_byType <- aggregate(. ~ Type, oldProdA, sum)


# Outliers 

## identify outliers in all-rows DF 

### Plot of data with outliers.
par(mfrow=c(1, 4)) # to plot 4 graphs in a line of the plot plael
plot(oldProd$Volume, oldProd$X4StarReviews, main="with Outliers", ylab="4* Reviews", xlab="Volume", pch="*", col="red", cex=2)
abline(lm(Volume ~ X4StarReviews, data=oldProd), col="blue")

plot(oldProd$Volume, oldProd$X2StarReviews, main="with Outliers", ylab="2* Reviews", xlab="Volume", pch="*", col="red", cex=2)
abline(lm(Volume ~ X2StarReviews, data=oldProd), col="blue")

plot(oldProd$Volume, oldProd$PositiveServiceReview, main="with Outliers",ylab="PositiveServiceReview", xlab="Volume", pch="*", col="red", cex=2)
abline(lm(Volume ~ PositiveServiceReview, data=oldProd), col="blue")

plot(oldProd$Volume, oldProd$WouldRecommend, main="with Outliers", ylab="WouldRecommend", xlab="Volume", pch="*", col="red", cex=2)
abline(lm(Volume ~ WouldRecommend, data=oldProd), col="blue")

par(mfrow=c(1, 1)) # set plot paner parameters to default
#dev.off()


### Compute and plot Cook's distance (measure for detecting outliers, if C(x) > 4*mean)
### we can iterate the process

mod <- lm(oldProd$Volume ~ ., data=oldProd) # make a linear model to compute C(x) row-wise
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

cooksdLabels <- ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),"") # make an index of the rows that are Cooks outliers
oldProd[cooksdLabels <- cooksd>4*mean(cooksd, na.rm=T),]
cooksdLabels[][is.na(cooksdLabels[])] <- FALSE                          # if NA's appear in the index, we turn them to FALSE

oldProd[cooksdLabels, ] # uncomment to see the outliers of the DF oldProd[]

oldProd <- oldProd[!cooksdLabels, ] # drop the rows marked as outliers in cooksdLabels


### identify outliers in 4-product-type DF (oldProdA)
#### Plot of data with outliers.
par(mfrow=c(1, 4))
plot(oldProdA$Volume, oldProdA$X4StarReviews, main="with Outliers", xlab="4* Reviews", ylab="Volume", pch="*", col="red", cex=2)
abline(lm(Volume ~ X4StarReviews, data=oldProdA), col="blue")

plot(oldProdA$Volume, oldProdA$X2StarReviews, main="with Outliers", xlab="2* Reviews", ylab="Volume", pch="*", col="red", cex=2)
abline(lm(Volume ~ X2StarReviews, data=oldProdA), col="blue")

plot(oldProdA$Volume, oldProdA$PositiveServiceReview, main="with Outliers",xlab="PositiveServiceReview", ylab="Volume", pch="*", col="red", cex=2)
abline(lm(Volume ~ PositiveServiceReview, data=oldProdA), col="blue")

plot(oldProdA$Volume, oldProdA$WouldRecommend, main="with Outliers", xlab="WouldRecommend", ylab="Volume", pch="*", col="red", cex=2)
abline(lm(Volume ~ WouldRecommend, data=oldProdA), col="blue")

## grouping by type ####

### we group now the rows (adding values) by product type (and if needed calculate corr again) 
oldProd_byType <- aggregate(. ~ Type, oldProd, sum)



