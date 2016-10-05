library(fpp)

'''
Question 4: For this exercise, use the monthly Australian short-term overseas visitors data,
            May 1985--April 2005. (Data set: visitors.)

a) Make a time plot of your data and describe the main features of the series. 
'''

plot(visitors)

# There is a positive linear trend, seasonality throughout the series and increasing
# variance.

'''
b) Forecast the next two years using Holt-Winters'' multiplicative method. 
'''

two_years <- hw(visitors, h=24, seasonal='multiplicative')

'''
c) Why is multiplicative seasonality necessary here?
'''

# It is necessary because the plot shows that the variance increases through the series.

'''
d) Experiment with making the trend exponential and/or damped. 
'''

two_years1 <- hw(visitors, h=24, seasonal='multiplicative', damped=TRUE)
two_years2 <- hw(visitors, h=24, seasonal='multiplicative', exponential=TRUE)

'''
e)  Compare the RMSE of the one-step forecasts from the various methods. Which do you
    prefer?
'''

plot(two_years,ylab="Monthly Australian visitors", plot.conf=FALSE,
     fcol="white", xlab="Year")
lines(fitted(two_years), col="red", lty=2)
lines(fitted(two_years1), col="green", lty=2)
lines(fitted(two_years2), col='blue', lty=2)
lines(two_years$mean, type="o", col="red")
lines(two_years1$mean, type="o", col="green")
lines(two_years2$mean, type="o", col="blue")
legend("topleft",lty=1, pch=1, col=1:3,
       c("data","H-W Multiplicative", 'Damped H-W Multiplicative',
         'H-W Exponential'))

# 1:  14.8295  H-W Multiplicative
# 2:  14.4480  Damped H-W Multiplicative
# 3:  14.4942  H-W Exponential

# All three models have similar RMSEs. I prefer the one with the smallest RMSE:
# the damped H-W multiplicative model.

'''
f)  Now fit each of the following models to the same data:
'''

# a) a multiplicative Holt-Winters' method

fit1 <- holt(visitors, seasonal='multiplicative')

# b) an ETS model

fit2 <- ets(visitors)

# c) an additive ETS model applied to a Box-Cox transformed series

lambda <- BoxCox.lambda(visitors)
boxcox_visitors <- BoxCox(visitors, lambda)
fit3 <- ets(boxcox_visitors, model='AAZ')

# d) a seasonal naive method applied to the Box-Cox transformed series

fit4 <- snaive(boxcox_visitors)

# e)  an STL decomposition applied to the Box-Cox transformed data followed by an ETS model
#     applied to the seasonally adjusted (transformed) data. 

decomposed <- decompose(boxcox_visitors)
seasonal_visitors <- decomposed$seasonal

fit5 <- ets(seasonal_visitors)

'''
g)  For each model, look at the residual diagnostics and compare the forecasts for the next
    two years. Which do you prefer? 
'''

plot((fitted(fit1)), residuals(fit1))
# Unbiased and heteroskedastic.

plot((fitted(fit2)), residuals(fit2))
# Unbiased and homoskedastic.

plot((fitted(fit3)), residuals(fit3))
# Slightly biased and borderline heteroskedastic.

plot((fitted(fit4)), residuals(fit4))
# Biased and heteroskedastic.

plot((fitted(fit5)), residuals(fit5))
# Biased and heteroskedastic.

plot(forecast(fit1, h=10))
plot(forecast(fit2, h=10))

# The second model (ETS) has the best looking residual plot and it's forecasts appear
# plausible (as does fit 1 but fit 1 has heteroskedasticity present in the residuals.)



