####This is the Real time fraud detection problem
remove.packages("MASS")
install.packages("MASS")
library(MASS)
library(party)
getwd()
setwd("D:/Vaibhav/Zhe Consulting/Real Time Fraud Detection - Suraj PPH/Delivery")
sampleData <- read.csv("DataSetToBeUsed.csv")
sampleData
attach(sampleData)
str(sampleData)

summary(sampleData$Status.of.existing.checking.account)
summary((is.na(sampleData)))

nrow(sampleData)
####1000

newData<-na.omit(sampleData)
nrow(newData)
####1000 no NA rows

Job.freq = table(Job)
barplot(Job.freq)
colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan")

barplot(Job.freq,col=colors)

data_tree <- ctree(Fraud~., data=sampleData)

print(data_tree)
plot(data_tree)

##----------------------build the logistic regression model here

setwd("D:/Vaibhav/Zhe Consulting/Real Time Fraud Detection - Suraj PPH/Delivery")
fraudData <- read.csv("DataSetToBeUsed.csv")
fraudData
attach(fraudData)


summary(fraudData)


##find out all the KPIs of the data

mean(fa)
##there are no missing values in the data

boxplot(Duration.in.months)
