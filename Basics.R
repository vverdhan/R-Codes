# rm(list = ls())

#READING DATA
library(astsa)
kings <- scan("kings.dat",skip=3)
kings
class(kings)
kingstimeseries <- ts(kings)
kingstimeseries
class(kingstimeseries)


births <- scan('nybirths.dat')
birthstimeseries <- ts(births, frequency = 12, start = c(1946, 1))
birthstimeseries

souvenir <- scan('fancy.dat')
souvenirtimeseries <- ts(souvenir, frequency = 12, start = c(1987, 1))
souvenirtimeseries

#PLOTTING
plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)
plot(decompose(birthstimeseries))
monthplot(birthstimeseries)
library(forecast)
par(mfrow = c(1,1))
seasonplot(souvenirtimeseries)
seasonplot(log(ts(souvenir, frequency = 12)))
lag1.plot(kings,1) # Plots x versus lag 1 of x.


#DECOMPOSING
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal
plot(birthstimeseriescomponents)


#SIMPLE and WEIGHTED MOVING AVERAGES
library(TTR)
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)

plot.ts(kingstimeseries)
lines(kingstimeseriesSMA3, col = 'red')
lines(kingstimeseriesSMA8, col = 'green')

kingstimeseriesWMA3 <- WMA(kingstimeseries, n = 3)
kingstimeseriesWMA8 <- WMA(kingstimeseries, n = 8)

plot.ts(kingstimeseries)
lines(kingstimeseriesWMA3, col = 'red')
lines(kingstimeseriesWMA8, col = 'green')

##Test Train Split, Holt Winters and Forecast
plot(kings.hw)
length(kingstimeseries)
train <- kingstimeseries[1:37]
test  <- kingstimeseries[38:42]
kings.hw<-HoltWinters(train, beta=FALSE, gamma=FALSE)
kings.hw$fitted
kings.hw$SSE

library(forecast)
fc.kings.hw<-forecast.HoltWinters(kings.hw, h = 5)
fc.kings.hw
plot.forecast(fc.kings.hw) #SAME AS EXPONENTIAL SMOOTHING

library(DMwR)
regr.eval(test, fc.kings.hw$mean)



#FULL HOLTWINTERS SMOOTHING
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts
plot(souvenirtimeseriesforecasts)
souvenirtimeseriesforecasts2 <- forecast.HoltWinters(souvenirtimeseriesforecasts, h=48)
plot.forecast(souvenirtimeseriesforecasts2)


###ARIMA ON KINGS
acf2(kingstimeseries, max.lag = 20)
kingsdff <- diff(kingstimeseries, diff= 2)
plot(kingsdff, type = 'b')
acf2(kingsdff, max.lag = 20)

kingstimeseriesarima <- arima(kingstimeseries, order=c(3,1,1))
auto.arima(kingstimeseries)
kingstimeseriesarima2 <- arima(kingstimeseries, order=c(0,1,1))

kingstimeseriesforecasts <- forecast.Arima(kingstimeseriesarima, h=5)
kingstimeseriesforecasts
plot.forecast(kingstimeseriesforecasts)
accuracy(kingstimeseriesforecasts, test = c(36, 42))
accuracy(kingstimeseriesarima2, test = c(36, 42))




################################
############AUTOREGRESSION MODELS################

###EX 1
x=EQcount
# write.csv(x, 'quakes.dat')
x=ts(x, start = 1900) #this makes sure R knows that x is a time series
plot(x, type="b") #time series plot of x with points marked as "o"
abline(a = mean(x), b = 0, col = 'red')

lag1.plot(x,1) # Plots x versus lag 1 of x.
acf(x, xlim=c(1,19)) # Plots the ACF of x for lags 1 to 19
#A series with a high cor with lag 1 will have such an acf
pacf(x, xlim = c(1,19))
#The PACF for an AR model will have 
#a single/few spikes followed by insignificant values
#HENCE PACF IS A GOOD INDICATOR FOR CHOOSING AR MODELS
xlag1=lag(x,-1) # Creates a lag 1 of x variable.
y=cbind(x,xlag1) # See note 3 below
ar1fit=lm(y[,1]~y[,2])#Does regression, stores results object named ar1fit
summary(ar1fit) # This lists the regression results
plot(ar1fit$fit,ar1fit$residuals) #plot of residuals versus fits
acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18


#######RUNNING DIAGNOSTICS
sarima(x, 0, 0, 1)
sarima(x, 1, 0, 0)
tsdiag(arima(x, c(1, 0, 0)))

###########SIMULATING TIME SERIES DATA
sim.ar<-arima.sim(list(ar=c(0.4,0.4)),n=1000)
sim.ma<-arima.sim(list(ma=c(0.6,-0.4)),n=1000)
par(mfrow=c(2,2))
acf(sim.ar,main="ACF of AR(2) process")
acf(sim.ma,main="ACF of MA(2) process")
pacf(sim.ar,main="PACF of AR(2) process")
pacf(sim.ma,main="PACF of MA(2) process")

