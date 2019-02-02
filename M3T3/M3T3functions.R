# WapsAllNa2 <- function(x) {
#   for (a in x[,1:36]) {
#     if (!is.na(a)) return(0)
#   }
#   return(1)
# }
# indRowAllWapsNA <- function (x) {
#   index <- NULL
#   for(i in 1:nrow(x)) {
#     if(WapsAllNa2(x[i,])) index <- c(index,i)   
#   }
#   index
# }
# ind <- indRowAllWapsNA(trData.4) 