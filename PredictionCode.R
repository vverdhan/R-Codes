
#This code is written for Predicting Fraud Txn 
#Author: Zhe Consulting
#July 2017

#Load all the required librarires here

library(party)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rattle)
library(caTools)
library(InformationValue)
library(ROCR)
library(e1071)

#clear the memory
rm(list=ls())

#load data
setwd("D:/Vaibhav/Zhe Consulting/Real Time Fraud Detection - Suraj PPH/FinalDelivery")
train <- read.csv('DataSetToBeUsed.csv', header = T)

#create training and validation data from given data

set.seed(88)
split <- sample.split(train$Fraud, SplitRatio = 0.75)

#get training and test data
trainData <- subset(train, split == TRUE)
testData <- subset(train, split == FALSE)

#The EDA is already done
#Here we build the logistic Regression model
#------------------------------------------------------------------
#------------------------------------------------------------------
#####logistic regression model
model <- glm (Fraud ~ ., data = trainData, family = binomial)
summary(model)
predict <- predict(model, type = 'response')
#predict
#confusion matrix on Train Data Set
table(trainData$Fraud, predict > 0.5)

predictTest <- predict(model,newdata=testData, type = 'response')

#confusion matrix on Test Data Set
table(testData$Fraud, predictTest > 0.5)
#0.27

optCutOff <- optimalCutoff(trainData$Fraud, predictTest)[1] 
#optCutOff
#0.1256477

table(trainData$Fraud, predict > 0.1256)
table(testData$Fraud, predictTest > 0.1256)

#again with significant variables
model <- glm (Fraud ~ Status.of.existing.checking.account
              +Duration.in.months
              +Savings.account.bonds
              +Present.employment.since
              +Other.installment.plans
              , data = trainData, family = binomial)
summary(model)
predict <- predict(model, type = 'response')
#confusion matrix
table(trainData$Fraud, predict > 0.5)

predictTest <- predict(model,newdata=testData, type = 'response')

table(testData$Fraud, predictTest > 0.5)

optCutOff <- optimalCutoff(trainData$Fraud, predictTest)[1] 
optCutOff
#0.18874

table(trainData$Fraud, predict > 0.1887)
table(testData$Fraud, predictTest > 0.1887)


#ROCR Curve

ROCRpred <- prediction(predict, trainData$Fraud)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, col = "green", lty=2)

ROCRpredTest <- prediction(predictTest, testData$Fraud)
ROCRperfTest <- performance(ROCRpredTest, 'tpr','fpr')
plot(ROCRperfTest, col = "red", lty=2)

plot(ROCRperf,main="ROC Curve",col="green")
par(new=TRUE)
plot(ROCRperfTest,col="red",lty=2)
legend("bottomright", c("Train","Test"), cex=0.8,
       col=c("green","red"),lty=1:2)


#AUC
performance (ROCRpred,"auc")

performance (ROCRpredTest,"auc")


#Lift Chart

# Plot the lift chart.
lifttrain<- performance(ROCRpred, "lift", "rpp")
lifttest<- performance(ROCRpredTest, "lift", "rpp")
# Plot the lift chart.
plot(lifttrain, col="green", lty=1, xlab="Caseload (%)", add=FALSE,main="Lift Chart")
par(new=TRUE)
plot(lifttest, col="red", lty=2, xlab="Caseload (%)", add=FALSE)

legend("topright",c("Train","Test"), cex=0.8,
       col=c("green","red"),lty=1:2)


#logistic regression ends here
#-------------------------------------------------------------------------------------------
  
###apply decision tree here

  
rtree_fit <- rpart(Fraud ~ .,method= "class", data= trainData) 
print(rtree_fit)

printcp(rtree_fit) # display the results 
plotcp(rtree_fit) # visualize cross-validation results 
summary(rtree_fit) # detailed summary of splits

# plot tree 
plot(rtree_fit, uniform=TRUE, 
     main="Classification Tree for Fraud Detection")
text(rtree_fit, use.n=TRUE, all=TRUE, cex=.8)

fancyRpartPlot(rtree_fit)

predTrain <- predict(rtree_fit, newdata=trainData, type="class")
pred.probTrain <- predict(rtree_fit, newdata=trainData, type="prob")

table(predTrain, trainData$Fraud)

predTest <- predict(rtree_fit, newdata=testData, type="class")
pred.probTest <- predict(rtree_fit, newdata=testData, type="prob")

table(predTest, testData$Fraud)

#signiicant variables
rtree_fit <- rpart(Fraud ~ Status.of.existing.checking.account
                   +Duration.in.months
                   +Savings.account.bonds
                   +Purpose
                   , trainData) 

printcp(rtree_fit) # display the results 
plotcp(rtree_fit) # visualize cross-validation results 
summary(rtree_fit) # detailed summary of splits

# plot tree 
plot(rtree_fit, uniform=TRUE, 
     main="Classification Tree for Fraud Detection")
text(rtree_fit, use.n=TRUE, all=TRUE, cex=.8)

fancyRpartPlot(rtree_fit)

pred <- predict(rtree_fit, newdata=trainData, type="class")
pred.prob <- predict(rtree_fit, newdata=trainData, type="prob")

table(pred, trainData$Fraud)

pred <- predict(rtree_fit, newdata=testData, type="class")
pred.prob <- predict(rtree_fit, newdata=testData, type="prob")

table(pred, testData$Fraud)


########################
#######################Decion Tree Ends Here

#random forest starts here


randomModel <-  randomForest(Fraud~.,  data=trainData)
pred <- predict(randomModel, newdata=trainData, type="class")
pred.prob <- predict(randomModel, newdata=trainData, type="prob")

summary(randomModel)
table(pred, trainData$Fraud)

pred <- predict(randomModel, newdata=testData, type="class")
pred.prob <- predict(randomModel, newdata=testData, type="prob")

table(pred, testData$Fraud)

##making with soem selected variables
randomModel <-  randomForest(Fraud~Status.of.existing.checking.account
                             +Credit.History+Savings.account.bonds
                             +Present.employment.since
                             +Other.installment.plans,  data=trainData
                             ,ntrees=1000, cutoff = c(0.7,1-0.7))

pred <- predict(randomModel, newdata=trainData, type="class")
pred.prob <- predict(randomModel, newdata=trainData, type="prob")

table(pred, trainData$Fraud)

pred <- predict(randomModel, newdata=testData, type="class")
pred.prob <- predict(randomModel, newdata=testData, type="prob")

table(pred, testData$Fraud)

##########################################
##Develop SVM Model

svm.model <- svm(Fraud ~ ., data = trainData, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, trainData)

y<-trainData$Fraud

table(svm.pred,y)

svm.pred <- predict(svm.model, testData)

y<-testData$Fraud

table(svm.pred,y)

#tune the model
tuneResult <- tune(svm, Fraud ~ .,  data = trainData,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
) 


print(tuneResult)
plot(tuneResult)

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, trainData) 

y<-trainData$Fraud

table(tunedModelY,y)

tunedModelY <- predict(tunedModel, testData) 

y<-testData$Fraud

table(tunedModelY,y)
################################
