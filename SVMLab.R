
install.packages("e1071")
library(e1071)
###SVM
setwd("D:/Vaibhav/Zhe Consulting/Training")

data <- read.csv('SVMData.csv', header = TRUE)

# Plot the data
plot(data, pch=16)

# Create a linear regression model
model <- lm(Y ~ X, data)

# Add the fitted line
abline(model)
predictedY <- predict(model, data)


# display the predictions
points(data$X, predictedY, col = "blue", pch=4)
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)
predictionRMSE

model <- svm(Y ~ X , data)

predictedY <- predict(model, data)

points(data$X, predictedY, col = "red", pch=4)
# /!\ this time  svrModel$residuals  is not the same as data$Y - predictedY
# so we compute the error like this
error <- data$Y - predictedY
svrPredictionRMSE <- rmse(error)
svrPredictionRMSE

# perform a grid search
tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
) 

print(tuneResult)
plot(tuneResult)
tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data) 

error <- data$Y - tunedModelY  

tunedModelRMSE <- rmse(error)    
tunedModelRMSE

