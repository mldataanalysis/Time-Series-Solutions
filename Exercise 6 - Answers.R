library(fma)
library(fpp)

'''
Question 1
'''

'''
a) Plot the time series of sales of product A. 
   Can you identify seasonal fluctuations and/or a trend?  
'''

ts <- plastics
plot(plastics)

# There are seasonal fluctuation with sales peaking in summer and reaching troughs in
# winter. There is a positive trend throughout the series.

'''
b) Use a classical multiplicative decomposition to calculate the trend-cycle and 
   seasonal indices. 
'''

fit <- decompose(ts, type="multiplicative")

trend_indices <- fit$trend
seasonal_indices <- fit$seasonal

'''
c) Do the results support the graphical interpretation from part (a)? 
'''

# The graph indicates that the summer months should have higher seasonal indices than the 
# winter months and this is indeed the case.

'''
d)  Compute and plot the seasonally adjusted data. 
'''
first <- seasadj(fit)
plot(first)

'''
e) Change one observation to be an outlier (e.g., add 500 to one observation), 
   and recompute the seasonally adjusted data. What is the effect of the outlier? 
'''
ts2 <- ts
ts2[13] = ts2[13] + 500

fit2 <- decompose(ts2, type="multiplicative")

second <- seasadj(fit2)
plot(second)

# The outlier causes a spike in the graph at the month it was inserted.

'''
f) Does it make any difference if the outlier is near the end rather than in the middle of
   the time series?
'''

ts3 <- ts
ts3[29] = ts3[29] + 500

fit3 <- decompose(ts3, type='multiplicative')

third <- seasadj(fit3)

ts4 <- ts
ts4[51] = ts4[51] + 500

fit4 <- decompose(ts4, type='multiplicative')

fourth <- seasadj(fit4)

plot(third)
plot(fourth)

# It appears that it doesn't matter where the outlier occurs. Whereever it is inserted 
# we see a similar (perhaps identical) sized spike.

'''
g) Use a random walk with drift to produce forecasts of the seasonally adjusted data. 
'''

fit5 <- rwf(first, drift=TRUE)

'''
h)  Reseasonalize the results to give forecasts on the original scale. 
'''

fit5 <- stl(ts, t.window=15, s.window="periodic", robust=TRUE)
fit6 <- forecast(fit5, method="naive")


'''
Question 2
'''

'''
a) Write about 3-5 sentences describing the results of the seasonal adjustment. 
   Pay particular attention to the scales of the graphs in making your interpretation.

Isolating the trend component from the seasonal component shows that the trend 
has increased throught the majority of the time frame, with a few stationary
periods occuring in the early 90s. The monthly breakdown of the seasonal
component shows that a few months show greater velocities in their variations
than other months. 
'''

'''
b) Is the recession of 1991/1992 visible in the estimated components? 
'''

# Yes the large residuals during that time period capture this information.