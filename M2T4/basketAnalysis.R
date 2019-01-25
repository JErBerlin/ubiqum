# install packages 
install.packages("arules")
install.packages("arulesViz")

# load libraries ####
library(plyr)
library(arules)
library(arulesViz)

# load data ####
Trs <- read.transactions("ElectronidexTransactions2017.csv", sep=',', rm.duplicates = TRUE)

# inspect data ####
# nrow(Trs)
# ncol(Trs)
itemFrequencyPlot(Trs, support = 0.05, topN = 10)
# image(Trs[1:100,1:125])
# image(sample(Trs, 3))

# generating the rules

## creating the baskets
baskets <- Trs

## we create groups of rules ####
basket_rules1 = apriori(baskets,parameter = list(sup = 0.003, conf = 0.75))
basket_rules2 = apriori(baskets,parameter = list(sup = 0.015, conf = 0.5,maxlen=3)) 
basket_rules3 = apriori(baskets,parameter = list(sup = 0.001, conf = 0.5,maxlen=2))
basket_rules4 = apriori(baskets,parameter = list(sup = 0.04, conf = 0.15,maxlen=2, minlen=2))
basket_rules5 = apriori(baskets,parameter = list(sup = 0.07, conf = 0.07,maxlen=1))

## ordering the rules by lift, confidence or support #### 
basket_rules1 = sort(basket_rules1, by = 'support', decreasing = TRUE)
basket_rules2 = sort(basket_rules2, by = 'support', decreasing = TRUE)

basket_rules4 = sort(basket_rules4, by = 'support', decreasing = TRUE)
basket_rules5 = sort(basket_rules5, by = 'support', decreasing = TRUE)

## inspecting and summarizing ####
summary(basket_rules1)
inspect(basket_rules1[])

inspect(basket_rules2[])

inspect(basket_rules3[])

inspect(basket_rules4[])

inspect(basket_rules5[])


## plotting the rules
### scatterplot
plot(basket_rules4)

### graph
plot(basket_rules2[1:3],method="graph")
plot(basket_rules4[1:8],method="graph")

# advanced rules

## rules which have rhs = "iMac"
basket_rules6 = apriori(baskets, parameter=list(supp=0.0012,conf = 0.81,maxlen=3),
                        appearance = list(default="lhs",rhs="iMac"),
                        control = list(verbose=F))
basket_rules6 = sort(basket_rules6, decreasing=TRUE,by="support")
inspect(basket_rules6)

## rules which have rhs = "HP Laptop"
basket_rules7 = apriori(baskets, parameter=list(supp=0.0018,conf = 0.81,maxlen=3),
                        appearance = list(default="lhs",rhs="HP Laptop"),
                        control = list(verbose=F))
basket_rules7 = sort(basket_rules7, decreasing=TRUE,by="support")
inspect(basket_rules7)

## rules which have rhs = "Acer Desktop"
basket_rules8 = apriori(baskets, parameter=list(supp=0.02,conf = 0.05,maxlen=2),
                        appearance = list(default="lhs",rhs="Acer Desktop"),
                        control = list(verbose=F))
basket_rules8 = sort(basket_rules8, decreasing=TRUE,by="confidence")
inspect(basket_rules8)
plot(basket_rules8, method="graph")

## rules which have lhs = "Acer Desktop"
basket_rules9 = apriori(baskets, parameter=list(supp=0.02,conf = 0.1,minlen=2, maxlen=2),
                        appearance = list(default="rhs",lhs="Acer Desktop"),
                        control = list(verbose=F))
basket_rules9 = sort(basket_rules9, decreasing=TRUE,by="confidence")
basket_rules9 = sort(basket_rules9, by = 'support', decreasing = TRUE)
inspect(basket_rules9)
plot(basket_rules9, method="graph")

## rules which have rhs = "Acer Aspire"
basket_rules10 = apriori(baskets, parameter=list(supp=0.02,conf = 0.05,maxlen=2),
                        appearance = list(default="lhs",rhs="Acer Aspire"),
                        control = list(verbose=F))
basket_rules10 = sort(basket_rules10, decreasing=TRUE,by="confidence")
inspect(basket_rules10)
plot(basket_rules10, method="graph")

## rules which have lhs = "Acer Aspire"
basket_rules11 = apriori(baskets, parameter=list(supp=0.01,conf = 0.1,minlen=2, maxlen=2),
                        appearance = list(default="rhs",lhs="Acer Aspire"),
                        control = list(verbose=F))
basket_rules11 = sort(basket_rules11, decreasing=TRUE,by="confidence")
inspect(basket_rules11)
plot(basket_rules11[1:8], method="graph")



