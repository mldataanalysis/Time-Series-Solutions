### Answers to Chapter 2 Exercises of Forecasting: principles and practice by R Hyndman
### Date: 17/09/2016

library(fma)

#Question 1
plot(dole)
lambda <- BoxCox.lambda(dole)
plot(BoxCox(dole, lambda))

plot(usdeaths)
usdeathscomponents <- decompose(usdeaths)
usdeathsSA <- usdeaths - usdeathscomponents$seasonal
plot(usdeathsSA)

plot(bricksq)
bricksqcomponents <- decompose(bricksq)
bricksSA <- bricksq - bricksqcomponents$seasonal
plot(bricksSA)

#Question 2
plot(dowjones)
preds <- rwf(dowjones, drift=TRUE)

plot(preds)
x0 = 1
x1 = 78
y0 = 110.94
y1 = 121.23
segments(x0, y0, x1, y1)

preds <- naive(dowjones)
plot(preds)

preds <- meanf(dowjones)
plot(preds)

preds <- snaive(dowjones)
plot(preds)

#I feel that the naive method is the best method for this time series. Predicting
#stock prices is very tricky and using data from from far in the past is unlikely to
#be a good indicator of future share price movements.

#Question 3
plot(ibmclose)

train <- window(ibmclose, start=c(1), end=c(300)) 
test <- window(ibmclose, start=c(300))

mean_fit <- meanf(train, h=70)
accuracy(mean_fit, test)
#RMSE-132.16, MAE-130.62, MAPE-35.48, MASE-25.63

naive_fit <- naive(train, h=70)
accuracy(naive_fit, test)
#RMSE-20.25, MAE-17.03, MAPE-4.69, MASE-4.69

drift_fit <- rwf(train, h=70, drift=TRUE)
accuracy(drift_fit, test)
#RMSE-17.06, MAE-13.94, MAPE-3.707, MASE-2.74

#Looking at the accuracy measures we can see that the drift method performs the best
#out of the three benchmark models fit.

#Question 4
plot(hsales)
#There are seasonal affects in the time series.
plot(decompose(hsales))
#The trend declines and increases over time, probably reflecting the business cycle.

train <- window(hsales, start=c(1973,1), end=c(1993,12))
test <- window(hsales, start=c(1993,12))

mean_fit <- meanf(train, h=24)
accuracy(mean_fit, test)
#RMSE      MAE       MAPE     MASE
#9.216133 7.850759  13.75973 0.924979

drift_fit <- rwf(train, h=24, drift=TRUE)
accuracy(drift_fit, test)
#RMSE      MAE       MAPE     MASE
#9.761548 8.393037  14.50303 0.9888703

naive_fit <- naive(train, h=24)
accuracy(naive_fit, test)
#RMSE      MAE       MAPE     MASE
#9.670664 8.304348  14.381673 0.9784210

snaive_fit <- snaive(train, h=24)
accuracy(snaive_fit, test)
#RMSE      MAE       MAPE     MASE
#6.160886 5.0000    9.12828 0.5891016

#The seasonal naive method comes out on top. This is what we would expect given the clear
#seasonal component present in this time series.


