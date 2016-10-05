library(fpp)

'''
Question 3: For this exercise, use the quarterly UK passenger vehicle production data from 
            1977:1--2005:1 (data set ukcars).

a) Plot the data and describe the main features of the series.
'''

cars <- ukcars
plot(ukcars)

# There is a strong seasonal effect throughout the series. The trend was declining until
# the early 80s then it started increasing until 2000. Production recovered in a year or
# two.

'''
b) Decompose the series using STL and obtain the seasonally adjusted data. 
'''

decomposed <- stl(cars, s.window="periodic", robust=TRUE)
seasonal <- decomposed$time.series[,1]

cars_sa <- cars - seasonal

'''
c) Forecast the next two years of the series using an additive damped trend method applied
   to the seasonally adjusted data. Then reseasonalize the forecasts. Record the 
   parameters of the method and report the RMSE of the one-step forecasts from your method. 
'''

fit1 <- holt(cars_sa, h=8, damped = TRUE)

lastyear <- rep(decomposed$time.series[110:113,"seasonal"],2)
reseasonalized_fc <- fit$mean + lastyear

summary(fit)

# Call:
#   holt(x = cars_sa, h = 8, damped = TRUE) 
# 
# Smoothing parameters:
#   alpha = 0.5666 
#   beta  = 3e-04 
#   phi   = 0.9117 
# 
# Initial states:
#   l = 346.0865 
#   b = -9.7583 
# 
# RMSE: 25.20318

'''
d) Forecast the next two years of the series using Holts linear method applied to the 
   seasonally adjusted data. Then reseasonalize the forecasts. Record the parameters of
   the method and report the RMSE of of the one-step forecasts from your method.
'''

fit2 <- holt(cars_sa, h=8)
 
reseasonalized_fc <- fit$mean + lastyear

summary(fit)

# Call:
#   holt(x = cars_sa, h = 8) 
# 
# Smoothing parameters:
#   alpha = 0.6012 
#   beta  = 1e-04 
# 
# Initial states:
#   l = 343.3854 
#   b = 0.6617
#
# RMSE: 25.39072

'''
e) Now use ets() to choose a seasonal model for the data.
'''

fit3 <- ets(cars_sa)

# ETS(A,N,N)
# RMSE: 25.29416

'''
f) Compare the RMSE of the fitted model with the RMSE of the model you obtained using an 
   STL decomposition with Holt''s method. Which gives the better in-sample fits? 
'''

# All models have all most identical RMSE values.

'''
g) Compare the forecasts from the two approaches? Which seems most reasonable? 
'''

predict(ets(cars_sa))

# Holt's linear method appears to be the most reasonable as it shows some trend.