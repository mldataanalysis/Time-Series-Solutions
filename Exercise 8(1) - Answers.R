library(fpp)

#Figure 8.24: https://www.otexts.org/sites/default/files/resize/fpp/images/wnacfplus-540x174.png

'''
Question 1

a) Figure 8.24 shows the ACFs for 36 random numbers, 360 random numbers and for 
   1,000 random numbers. Explain the differences among these figures. 
   Do they all indicate the data are white noise?
'''

# All three three plots indicate that the data is white noise. This is because none of the
# spikes are larger than the critical value range for any of the plots.

'''
b) Why are the critical values at different distances from the mean of zero? Why are the
   autocorrelations different in each figure when they each refer to white noise? 
'''

# The formula for the critical values is +/- 1.96/(sqrt(T - d)) where T is the sample size
# and d is the amount of differencing used. As the sample size increases the critical
# values get smaller. This explains why the cricial value region gets smaller (from left to
# right in the plot) as the sample size increases.

'''
Question 2 

A classic example of a non-stationary series is the daily closing IBM stock prices 
(data set ibmclose). Use R to plot the daily closing prices for IBM stock and the ACF and
PACF. Explain how each plot shows the series is non-stationary and should be differenced.
'''

plot(ibmclose)
plot(acf(ibmclose))
plot(pacf(ibmclose))

# There is clearly a trend element throughout the plot. The ACF plot shows that there
# are significant autocorrelations throughout. Therefore the data should be differenced
# in order to remove autocorrelation. 