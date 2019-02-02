# load libs
library(dplyr)

# Construct a 5x6 df
X <- matrix(rnorm(30), nrow=5, ncol=6)
X <- as.data.frame(X)

# Sum the values of each column with `apply()`
apply(X,2,sum)

# What would be the equivalent of this with a for loop?
for(v in X) {
  print(sum(v))
}

# Define 3 dataframes and list them:
A <- data.frame(v1 = c(1,2,3), v2 = c(4,5,6), v3 = c(7,8,9))
B <- data.frame(v1 = c(4,5,6,7), v2 = c(8,9,10,11), v3 = c(12,13,14,15))
C <- data.frame(v1 = c(8,9,10), v2 = c(8,9,10))
myList <- list(A, B, C)

#Extract the 2nd column from myList, using lapply and the dplyr function "select"
lapply(myList, select,2)

#Extract the 1st row from myList, using lapply and the dplyr function "slice"
lapply(myList,slice,1)

#Extract one single element from every dataframe in the list. You can use "[" as a function
lapply(myList,"[[",1,1)

# Initialize `Z`
Z <- sapply(myList,"[", 1,1 ) # as.numeric(lapply(myList,"[[",1,1))
Z

# Replicate the values of `Z` 3 times, 1 time and 2 times
Z <- rep(Z,c(3,1,2))
Z

# Create a 4x4 matrix
Q1 <- matrix(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)),4,4)
Q1

# Or use `mapply()`
Q1 <- mapply(rep,1:4,4)
Q1 

# the same row-wise
Q2 <- matrix(c(1:4, 1:4, 1:4, 1:4),4,4)
Q2


