# what the program does: ####

## Preferred independent vars: 
## Option 1: x5, x2              (with interception? -> no)

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

# (parameters and seed initialization) ####
# splitting dataset (80%/20%) or (100%/0%) train/test, if we are sure about the model
pTraining = 1.0

set.seed(113) # initialize random seed for reproducibility

## prepocessing 1: supress cols that are not relevant cols for the LM

#### chose for option 5x:
oldProd5x <- oldProd[, which(names(oldProd) %in% c("X5StarReviews","X2StarReviews", "Type", "Volume"))]

## iterative modelling (make modell -> take out cols or rows, look for singificant type, take it out -> make modell)

### take out rows because Type significant => not comparable to 4 types of Group A
oldProd5x <- oldProd5x[ (oldProd5x$Type != 'Extended Warranty' & oldProd5x$Type != 'Software' & oldProd5x$Type != 'Printer'),] 

## trimm outliers
oldProd5x <- oldProd5x[-c(18,45,52),] # the last two Product = (73,80) are Type = Game Console, first Product = (18) is Accessories
oldProd5x <- oldProd5x[-c(29,31),]    # these are two Product = (48,50) are also Type = Accessories
oldProd5x <- oldProd5x[-28,]          # Product = 47 is also accessory

# split the data: trainingSet <--> testingSet (for models: oldProd5x and oldProd4x ), using all data for training data
# Obs: we should spare this step if we are using all data for training; letting as it is for generality purpose
inTrainOldProd5x <- createDataPartition( y = oldProd5x$Volume,
                                p=pTraining,
                                list = FALSE )

trainingSet5x <- oldProd5x[inTrainOldProd5x,]
# testingSet5x  <- oldProd5x[-inTrainOldProd5x,]

# train model ####

## simple standard function for linear modelling in R: lm(). For plotting
VolLMR_opt5x <- lm(trainingSet5x$Volume ~ trainingSet5x$X5StarReviews + trainingSet5x$X2StarReviews + 0)

### plotting of residuals errors
par(mfrow = c(2, 2))  # Split the plotting panel 
plot(VolLMR_opt5x)  # Plot the model information
dev.off()           # set params plot to default

## caret: we use the methods caret package offer for LRM with cross-validation
trControl <- trainControl(method = "cv", number = 10) # training control: 10-fold cross-validation, we have small data

# ### train model: lm(Volum ~ 5x, 2x, no intercept)
VolLMR_opt5x <- train(
  Volume ~ X5StarReviews + X2StarReviews,
  data = trainingSet5x,
  method = "lm",
  trControl = trControl,
  tuneGrid  = expand.grid(intercept = FALSE)
)

# predict ####

## apply model to testing data / again to training data and compare computed vs expected

## model 5x
### create new col 'predVolume' with the predicted values for Volume
trainingSet5x$predVolume <- predict(VolLMR_opt5x, trainingSet5x)

### errors per product
errPred5x <- trainingSet5x$predVolume - trainingSet5x$Volume

### errors per product type
trainingSet5x_Type <- aggregate(. ~ Type, trainingSet5x, sum) # aggregate by Type
errPred5x_Type <- trainingSet5x_Type$predVolume - trainingSet5x_Type$Volume

### metrics of the prediction
postResample(trainingSet5x$predVolume, trainingSet5x$Volume)

