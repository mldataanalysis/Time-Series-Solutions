library(fma)
library(fpp)
library(segmented)

'''
a) Do a scatterplot of consumption against price. The data are clearly not linear. 
'''

df <- (texasgas)
plot(df$price, df$consumption)

'''
b) Can you explain why the slope of the fitted line should change with price?
'''

# The data is not linear so the slope needs to change in order to capture that information
# in our model.

'''
c) Fit the three models and find the coefficients, and residual variance in each case.
'''

# First model - basic linear regression

fit <- lm(consumption ~ exp(price), df)

'''
Coefficients:
#               Estimate   Std. Error t value  Pr(>|t|)    
#(Intercept)   7.086e+01   7.670e+00   9.238      2.98e-08 ***
#  exp(price)  -1.642e-43  1.711e-43  -0.959      0.35  
'''
# The slope for price is -1.642.

# Residual variance
(summary(fit)$sigma)**2 
# 1101.359

# Second model - piecewise linear regression

#fit <- lm(consumption ~ price*(price < 60) + price*(price > 60), df)
#summary(fit)

lin.mod <- lm(consumption ~ price, df)
segmented.mod <- segmented(lin.mod, seg.Z = ~price, psi=60)

slope(segmented.mod)

#         Est.   St.Err. t value CI(95%).l CI(95%).u
#slope1 -3.1470  0.5102  -6.169   -4.2290   -2.0660
#slope2 -0.3075  0.2220  -1.385   -0.7782    0.1632

# The slope for B1 is -3.147 and B2 for it's -0.308. This is makes sense when we consider
# the graph we saw earlier.

# Residual variance
(summary(segmented.mod)$sigma)**2 
# 167.8511 The residual variance is nearly ten times smaller than the basic linear model
# fitted above.

# Third model - polynomial regression

poly_fit <- lm(consumption ~ poly(price, 2), df)

# Residual variance
(summary(poly_fit)$sigma)**2
# 206.5276 - As was the case with piecewise regression, a polynomial fit greatly reduces
# residual variance.

'''
d) For each model, find the value of R2 and AIC, and produce a residual plot. 
Comment on the adequacy of the three models.
'''

# First model - basic linear regression

# Adjusted R-squared: -0.004 
# AIC: 200.736

resid <- residuals(fit)
plot(fit$fitted.values, resid, ylab='residuals', xlab='fitted values',
     main='linear regression')
abline(0,0)

# Second model - piecewise linear regression

# Adjusted R-squared:  0.847
# AIC: 164.756

resid <- residuals(segmented.mod)
plot(segmented.mod$fitted.values, resid, ylab='residuals', xlab='fitted values', 
     main='piecewise linear regression')
abline(0,0)

# Third model - polynomial regression.

# Adjusted R-squared:  0.812
# AIC: 168.116

resid <- residuals(poly_fit)
plot(poly_fit$fitted.values, resid, ylab='residuals', xlab='fitted values', 
     main='polynomial linear regression')
abline(0,0)

# All three of the residual plots show heteroskedasticity. The linear regression
# model residual plot shows huge problems, nearly all the values are predicted to be
# around 71 and most of these predictions are wildly inaccurate. The piecewise model has
# a slightly larger R^2 and a slightly lower AIC than the polynomial model so it is
# the best model of the three.

'''
e) For prices 40, 60, 80, 100, and 120 cents per 1,000 cubic feet, compute the 
   forecasted per capita demand using the best model of the three above.
'''

new.data <- data.frame(price=c(40, 60, 80, 100, 120))
predict(segmented.mod, new.data)

'''
f) Compute 95% prediction intervals. Make a graph of these prediction intervals and discuss their interpretation.
'''

newx <- seq(min(new.data), max(new.data), length.out=5)
intervals <- predict(segmented.mod, new.data, interval="predict") 

plot(consumption ~ price, data = df, type = 'n')

polygon(c(rev(newx), newx), c(rev(intervals[ ,3]), intervals[ ,2]), col = 'grey80', border = NA)

# We can see that the prediction intervals are fairly wide meaning we only have a rough
# idea of how much energy will be demanded.