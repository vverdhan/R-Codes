head(cars)

summary(cars)
str(cars)
#Graphical Analysis
#The aim of this exercise is to build a simple regression model that 
#we can use to predict Distance (dist) by establishing a statistically 
#significant linear relationship with Speed (speed). But before jumping in to the syntax, 
#lets try to understand these variables graphically. 
#Typically, for each of the independent variables (predictors), 
#the following plots are drawn to visualize the following behavior:
  
#  Scatter plot: Visualize the linear relationship between the predictor and response
#Box plot: To spot any outlier observations in the variable. Having outliers in your predictor 
#can drastically 
#affect the predictions as they can easily affect the direction/slope of the line of best fit.
#Density plot: To see the distribution of the predictor variable. Ideally, a close to 
#normal distribution 
#(a bell shaped curve), without being skewed to the left or right is preferred. 
#Let us see how to make each one 
#of them.

#Scatter Plot
#Scatter plots can help visualize any linear relationships between the dependent (response) variable 
#and independent (predictor) variables. Ideally, if you are having multiple predictor variables, 
#a scatter plot is drawn for each one of them against the response, along with the line of best as seen below.


scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot
#The scatter plot along with the smoothing line above suggests a linearly increasing relationship 
#between the 'dist' and 'speed' variables. This is a good thing, because, one of the underlying assumptions 
#in linear regression 
#is that the relationship between the response and predictor variables is linear and additive.


#BoxPlot - Check for outliers

#Generally, any datapoint that lies outside the 1.5 * interquartile-range (1.5 * IQR) 
#is considered an outlier, where, IQR is calculated as the distance between the 25th percentile and 
#75th percentile values for that variable.

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'


#Density plot - Check if the response variable is close to normality

install.packages("e1071")
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")

#Correlation
cor(cars$speed, cars$dist)  # calculate correlation between speed and distance 

#Build Linear Model
linearMod <- lm(dist ~speed, data=cars)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

#The p Value: Checking for statistical significance
#The summary statistics above tells us a number of things. One of them is the model 
#p-Value (bottom last line) and 
#the p-Value of individual predictor variables (extreme right column under 'Coefficients'). 
#The p-Values are very important because, We can consider a linear model to be statistically 
#significant 
#only when both these p-Values are less that the pre-determined statistical significance level, 
#which is ideally 0.05. 
#This is visually interpreted by the significance stars at the end of the row. 
#The more the stars beside the variable's p-Value, 
#the more significant the variable.

#Null and alternate hypothesis

#When there is a p-value, there is a hull and alternative hypothesis associated with it. 
#In Linear Regression, 
#the Null Hypothesis is that the coefficients associated with the variables is equal to zero. 
#The alternate hypothesis is that the coefficients are not equal to zero 
#(i.e. there exists a relationship 
#between the independent variable in question and the dependent variable).

#t-value

#We can interpret the t-value something like this. A larger t-value indicates that it is less likely
#that the coefficient is not equal to zero purely by chance. So, higher the t-value, the better.

#Pr(>|t|) or p-value is the probability that you get a t-value as high or higher 
#than the observed value when the 
#Null Hypothesis (the ?? coefficient is equal to zero or that there is no relationship) is true. 
#So if the Pr(>|t|) is low, 
#the coefficients are significant (significantly different from zero). If the Pr(>|t|) is high,
#the coefficients are not significant.

#What this means to us? when p Value is less than significance level (< 0.05), we can safely reject the null hypothesis that
#the co-efficient ?? of the predictor is zero. In our case, linearMod, both these p-Values 
#are well below the 0.05 threshold, 
#so we can conclude our model is indeed statistically significant.

#It is absolutely important for the model to be statistically significant before we can go ahead 
#and use it to predict (or estimate) 
#the dependent variable, otherwise, the confidence in predicted values from that model reduces 
#and may be construed as an event of chance.



###Predicing Model

#Step 1: Create the training (development) and test (validation) data samples from original data.

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

#Step 2: Develop the model on the training data and use it to predict the distance on test data

# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
#Step 3: Review diagnostic measures.

summary (lmMod)  # model summary
AIC (lmMod)

#Step 4: Calculate prediction accuracy and error rates

#A simple correlation between the actuals and predicted values can be used 
#as a form of accuracy measure.
#A higher correlation accuracy implies that the actuals and predicted values have 
#similar directional movement, 
#i.e. when the actuals values increase the predicteds also increase and vice-versa.

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

#From the model summary, the model p value and predictor's p value are less than the 
#significance level, 
#so we know we have a statistically significant model. Also, the R-Sq and Adj R-Sq are 
#comparative to the original model built on full data.

#Step 4: Calculate prediction accuracy and error rates

#A simple correlation between the actuals and predicted values can be used as a form of 
#accuracy measure. 
#A higher correlation accuracy implies that the actuals and predicted values have similar 
#directional movement, i.e. when the actuals values increase the predicteds also increase 
#and vice-versa. 

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy# 82.7%
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape
# => 48.38%, mean absolute percentage deviation


###
##k- Fold Cross validation
#Suppose, the model predicts satisfactorily on the 20% split (test data), 
#is that enough to believe that your model will 
#perform equally well all the time? It is important to rigorously test the 
#model's performance as much as possible. 
#One way is to ensure that the model equation you have will perform well, 
#when it is 'built' on a different subset of 
#training data and predicted on the remaining data.
#
#How to do this is? Split your data into 'k' mutually exclusive random sample portions. 
#Keeping each portion as test data, 
#we build the model on the remaining (k-1 portion) data and calculate the mean squared error 
#of the predictions. This is done 
#for each of the 'k' random sample portions. Then finally, the average of these mean squared 
#errors (for 'k' portions) is computed. 
#We can use this metric to compare different linear models.

#By doing this, we need to check two things:
  
#If the model's prediction accuracy isn't varying too much for any one particular sample, and
#If the lines of best fit don't vary too much with respect the the slope and level.
#In other words, they should be parallel and as close to each other as possible. 
#You can find a more detailed explanation for 
#interpreting the cross validation charts when you learn about advanced linear model building.

library(DAAG)
cvResults <- suppressWarnings(CVlm(df=cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')
