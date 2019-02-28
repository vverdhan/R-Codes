
setwd("D:/Vaibhav/Zhe Consulting/Training")

install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)
library(datasets)

# Load the data set
data(Groceries)

head(Groceries)

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")

#We set the minimum support to 0.001
#We set the minimum confidence of 0.8
#We then show the top 5 rules
# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])

#This reads easily, for example: if someone buys yogurt and cereals, they are 81% likely to buy whole milk too.
#We can get summary info. about the rules that give us some interesting information such as:
 # The number of rules generated: 410
#The distribution of rules by length: Most rules are 4 items long
#The summary of quality measures: interesting to see ranges of support, lift, and confidence.
#The information on the data mined: total data mined, and minimum parameters.


#Sorting stuff out
rules<-sort(rules, by="confidence", decreasing=TRUE)

summary(rules)
#maxlen
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))

#Redundancies
#subset.matrix <- is.subset(rules, rules)
#subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
#redundant <- colSums(subset.matrix, na.rm=T) >= 1
#rules.pruned <- rules[!redundant]
#rules<-rules.pruned


#Targeting Items
#Now that we know how to generate rules, limit the output, lets say we wanted to target items 
#to generate rules. There are two types of targets we might be interested in that are illustrated 
#with an example of "whole milk":
 # What are customers likely to buy before buying whole milk
#What are customers likely to buy if they purchase whole milk?
#This essentially means we want to set either the Left Hand Side and Right Hand Side. 
#This is not difficult to do with R!
 # Answering the first question we adjust our apriori() function as follows:
  rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
                 appearance = list(default="lhs",rhs="yogurt"),
                 control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:10])



#Likewise, we can set the left hand side to be "whole milk" and find its antecedents.
#Note the following:
#  We set the confidence to 0.15 since we get no rules with 0.8
#We set a minimum length of 2 to avoid empty left hand side items
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="yogurt"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

#Visualization
library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading=NA)
