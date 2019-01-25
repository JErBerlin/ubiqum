## exploratoryExistingProducts.R
## does an exploratory analysis of the data from existing_ok.csv
## aiming to do a linear regression model to predict volume (possibily grouped by type of product

# load libraries ####
library(ggplot2)

# load data ####
oldProd <- read.csv2('existing_ok.csv') ## EUR notation -> using csv2

# using all rows of the data (all product types) ####

# correlation of vars ####

## correlation grouped by type of product