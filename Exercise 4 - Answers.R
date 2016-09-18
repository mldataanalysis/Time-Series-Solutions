library(fma)

'''
Question 1

a) Plot the data and find the regression model for Mwh with temperature as an explanatory
   variable. Why is there a negative relationship?                              
'''

mwh <- c(16.3,	16.8,	15.5,	18.2,	15.2,	17.5,	19.8,	19.0,	17.5,	16.0,	19.6,	18.0)
temp <- c(29.3,	21.7,	23.7,	10.4,	29.7,	11.9,	9.0,	23.4,	17.8,	30.0,	8.6,	11.8)

df <- data.frame(mwh, temp)

mwf <- ts(mwh)
temp <- ts(temp)

par(mfrow=c(2,1))
plot(mwf)
plot(temp)

lm_fit <- lm(mwh~temp)

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   20.19952    0.73040   27.66 8.86e-11 ***
#  temp        -0.14516    0.03549   -4.09  0.00218 **  

#One plausible reason for the negative relationship is that households consume less
#power when it's warm. With warmer temperatures there is less need to consume
#energy to maintain warmth and more incentive to go outside and have fun! Thus reducing
#energy consumption.

'''
 b) Produce a residual plot. Is the model adequate? 
   Are there any outliers or influential observations?
'''

par(mfrow=c(1,1))
plot(fitted(lm_fit), residuals(lm_fit),
     xlab="Predicted scores", ylab="Residuals")

#The model is decent the majority of residuals lie between 1 and -1. There is one outlier.

'''
 c) Use the model to predict the electricity consumption that you would expect for a day
    with maximum temperature 10??? and a day with maximum temperature 35???. 
    Do you believe these predictions?
'''

newdata <- data.frame(temp = c(10, 35))
predict(lm_fit, newdata)
#1        2 
#18.74795 15.11902 

#These predictions seem plausible when one looks at the following scatter plot.
par(mfrow=c(1,1))
plot(df$mwh, df$temp)

'''
 d) Give prediction intervals for your forecasts.
'''

forecast(lm_fit, newdata)
#  Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#1       18.74795 17.27010 20.22579 16.34824 21.14766
#2       15.11902 13.50469 16.73335 12.49768 17.74035

#10 degress: 16.35 - 21.15 MWH (95% confidence interval)
#35 degress: 12.5  - 17.74 MWH (95% confidence interval)

'''
 Question 2

 a) Update the data set `olympic` to include the winning times from the last few Olympics. 
'''

df <- data.frame(olympic)

new.data <- data.frame(Year = c(2000, 2004, 2008, 2012, 2016), 
                       time = c(43.84, 44.00, 43.75, 43.94, 43.03))

df <- rbind(df, new.data)

'''
 b) Plot the winning time against the year. Describe the main features of the scatterplot. 
'''

plot(df$Year, df$time)

#There is a downward trend throughout the period. The biggest time reduction was achieved 
#at the start. Improvment in finishing times appear to diminish as time goes on. 

'''
 c) Fit a regression line to the data. Obviously the winning times have been decreasing,
    but at what average rate per year? 
'''

lm_fit <- lm(time~Year, df)

#Coefficients:
#(Intercept)     Year  
#196.07988     -0.07679 

#Winning times have been decreased at a rate of -0.077 per year.

'''
 d) Plot the residuals against the year. What does this indicate about the suitability 
   of the fitted line? 
'''

plot(fitted(lm_fit), residuals(lm_fit),
     xlab="Predicted scores", ylab="Residuals")

#The residuals show a downward trend over time. A non-random pattern in the residuals
#indicates that the predictor variables of the model is not capturing some explanatory 
#information that is "leaking" into the residuals. 

'''
 e) Predict the winning time for the mens 400 meters final in the 2000, 2004, 2008 and 
    2012 Olympics. 
'''

old_df <- data.frame(olympic)
newdata <- data.frame(Year = c(2000, 2004, 2008, 2012))

forecast(lm_fit, newdata)
#Point Forecast      Lo 95    Hi 95
#1       42.49977 40.05401 44.94554
#2       42.19261 39.72657 44.65866
#3       41.88545 39.39782 44.37308
#4       41.57829 39.06780 44.08879

actual <- c(43.84, 44.00, 43.75, 43.94)

accuracy(preds, actual)
#               ME     RMSE      MAE      MPE     MAPE
#Test set 1.843467 1.878634 1.843467 4.200364 4.200364

#We can see that our predictions were over optimistic. Perhaps the outlier at the start
#of the data set is harming our predictions.

