library(fma)
library(fpp)

'''
a) Produce a time plot of the data and describe the patterns in the graph. 
Identify any unusual or unexpected fluctuations in the time series. 
'''

plot(fancy)

# We can see a clear seasonal pattern in the data. There is a spike in sales in the peak 
# Christmas season and another smaller bump in sales during March. The seasonal
# seasonal fluctuations and random fluctuations seem to increase with the level of the 
# time series.

'''
b) Explain why it is necessary to take logarithms of these data before fitting a model. 
'''

# The increasing seasonal fluctuations in the data mean that tt is necessary to take
# logarithms in order to get an additive model.

'''
c) Use R to fit a regression model to the logarithms of these sales data with a linear 
   trend, seasonal dummies and a "surfing festival" dummy variable. 
'''

log_fancy <- log(fancy)
dummy_fest = rep(0, length(fancy))
dummy_fest[seq_along(dummy_fest)%%12 == 3] <- 1
dummy_fest[3] <- 0
dummy_fest <- ts(dummy_fest, freq = 12, start=c(1987,1))
my_data <- data.frame(
  log_fancy,
  dummy_fest
)

fit <- tslm(log_fancy ~ trend + season + dummy_fest, data=my_data)

future_data <- data.frame(
  dummy_fest = rep(0, 12)
)
future_data[3,] <- 1
forecast(fit, newdata=future_data)

'''
d)  Plot the residuals against time and against the fitted values. 
    Do these plots reveal any problems with the model? 
'''

plot(residuals(fit), type='p')
plot(as.numeric(fitted(fit)), residuals(fit), type='p')

# The residuals plotted against the fitted values show no pattern and vary from -0.03 to
# 0.03. Such a plot shows unbiased and homoscedastic residuals.
# The residuals plotted against time also vary from -0.03 to 0.03. There is a trend for
# the residuals to increase from 1991 to 1994. The residuals appear random prior to this.

'''
e)  Do boxplots of the residuals for each month. Does this reveal any problems with 
    the model? 
'''

boxplot(resid(fit) ~ cycle(resid(fit)))

# The residuals for the second half of the year show greater variance (paticularly August,
# September and October) than the first half of the year, which suggests that our model 
# may not be capturing some information relevant to this time period.

'''
f) What do the values of the coefficients tell you about each variable? 
'''
'''
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   7.6196670  0.0742471 102.626  < 2e-16 ***
  trend       0.0220198  0.0008268  26.634  < 2e-16 ***
  season2     0.2514168  0.0956790   2.628 0.010555 *  
  season3     0.2660828  0.1934044   1.376 0.173275    
  season4     0.3840535  0.0957075   4.013 0.000148 ***
  season5     0.4094870  0.0957325   4.277 5.88e-05 ***
  season6     0.4488283  0.0957647   4.687 1.33e-05 ***
  season7     0.6104545  0.0958039   6.372 1.71e-08 ***
  season8     0.5879644  0.0958503   6.134 4.53e-08 ***
  season9     0.6693299  0.0959037   6.979 1.36e-09 ***
  season10    0.7473919  0.0959643   7.788 4.48e-11 ***
  season11    1.2067479  0.0960319  12.566  < 2e-16 ***
  season12    1.9622412  0.0961066  20.417  < 2e-16 ***
  dummy_fest  0.5015151  0.1964273   2.553 0.012856 *  
'''

# The value of the coefficients show how much the model thinks each month contributes
# to the conditional mean of the model. We can see that as the year progresses the size 
# of the coefficient increases. All months have positive coefficents and are statistically
# significant except for March, this is unsuprising given that the dummy variable is occurs
# in this month.

'''
g) What does the Durbin-Watson statistic tell you about your model?
'''

dwtest(fit, alt="two.sided")

# The Durbin-Watson test shows that there is some autocorrelation remaining in the 
# residuals. This means there is some information remaining in the residuals that can 
# be exploited to obtain better forecasts.

'''
h) Regardless of your answers to the above questions, use your regression model to 
   predict the monthly sales for 1994, 1995, and 1996. Produce prediction intervals for 
   each of your forecasts.
'''

future_data <- data.frame(
  dummy_fest = rep(0, 36)
)
preds <- forecast(fit, newdata=future_data)

'''
i) Transform your predictions and intervals to obtain predictions and intervals for the 
   raw data.
'''

df <- as.data.frame(preds)
df <- exp(df)

'''
j) How could you improve these predictions by modifying the model?
'''

# We could consider using a dynamic-regression model which works better when we have 
# autocorrelation remaining in the residuals.