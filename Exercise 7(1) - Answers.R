library(fpp)

'''
1) Data set books contains the daily sales of paperback and hardcover books at the same store. The task is to forecast the next four days' sales for paperback and hardcover books (data set books).

a) Plot the series and discuss the main features of the data. 
'''

books <- books
plot(books)

# Both types of book have a positive trend and both show fairly large fluctations in daily 
# sales. This may be due to sales spiking at certain periods of the week, probably at 
# weekends.

'''
b) Use simple exponential smoothing with the ses function (setting initial="simple") and 
   explore different values of alpha for the paperback series. Record the within-sample 
   SSE for the one-step forecasts. Plot SSE against alpha and find which value of alpha
   works best. What is the effect of alpha on the forecasts? 
'''

pb <- books[,1]

fit1 <- ses(pb, initial='simple', alpha=0.2, h=3)
sum((pb - fitted(fit1))) #51.94
fit2 <- ses(pb, initial='simple', alpha=0.4, h=3)
sum((pb - fitted(fit2))) #50.28
fit3 <- ses(pb, initial='simple', alpha=0.6, h=3)
sum((pb - fitted(fit3))) #47.45
fit4 <- ses(pb, initial='simple', alpha=0.8, h=3)
sum((pb - fitted(fit4))) #46.67
fit5 <- ses(pb, initial='simple', alpha=0.9, h=3)
sum((pb - fitted(fit5))) #47.10 - previous sse was smaller so choose smaller alpha.
fit6 <- ses(pb, initial='simple', alpha=0.85, h=3)
sum((pb - fitted(fit6))) #46.82 - previous sse was smaller so choose smaller alpha.
fit7 <- ses(pb, initial='simple', alpha=0.83, h=3)
sum((pb - fitted(fit7))) #46.74 - previous sse was smaller so choose smaller alpha.
fit8 <- ses(pb, initial='simple', alpha=0.82, h=3)
sum((pb - fitted(fit8))) #46.71 - previous sse was smaller so choose smaller alpha.
fit9 <- ses(pb, initial='simple', alpha=0.81, h=3)
sum((pb - fitted(fit9))) #46.69 - previous sse was smaller so choose smaller alpha.
fit10 <- ses(pb, initial='simple', alpha=0.75, h=3)
sum((pb - fitted(fit10))) #46.66 - new minimum found.
fit11 <- ses(pb, initial='simple', alpha=0.3, h=3)
sum((pb - fitted(fit11))) #52.24
fit12 <- ses(pb, initial='simple', alpha=0.5, h=3)
sum((pb - fitted(fit12))) #48.62
fit13 <- ses(pb, initial='simple', alpha=1, h=3)
sum((pb - fitted(fit13))) #48

alpha <- c(0.2, 0.4, 0.6, 0.8, 0.9, 0.85, 0.83, 0.82, 0.81, 0.75, 0.3, 0.5, 1)
sse <- c(51.94, 50.28, 47.45, 46.67, 47.10, 46.82, 46.74, 46.71, 46.68, 46.66, 52.24, 48.62, 48)

plot(alpha, sse)

# 0.75 provides the smallest SSE. Increasing alpha from 0.2 to 0.75 provides increasing 
# improvements in the SSE, after this minimum increasing alpha increases SSE.

'''
c) Now let ses select the optimal value of alpha. Use this value to generate forecasts for
   the next four days. Compare your results with b. 
'''

fit1 <- ses(pb, initial='simple', h=4)
fit2 <- ses(pb, initial='simple', alpha=0.75, h=4)

par(mfrow=c(2,1))
plot(fit1, main="Automatic alpha")
plot(fit2, main="Custom alpha: 0.75")

# The model SES chooses predicts lower forecasts than the previous model. The prediction
# intervals are also smaller for the model SES chooses.

'''
d) Repeat but with initial="optimal". How much difference does an optimal initial level
   make? 
'''

fit3 <- ses(pb, initial='optimal', h=4)
sum((pb - fitted(fit3)))

# The model is more pessimistic than the others and has a higher SSE.

'''
e) Repeat steps (b)-(d) with the hardcover series. 
'''

hb <- books[,2]

fit1 <- ses(hb, initial='simple', alpha=0.8, h=3)
sum((hb - fitted(fit1))) #142.59
fit2 <- ses(hb, initial='simple', alpha=0.2, h=3)
sum((hb - fitted(fit2))) #465.09
fit3 <- ses(hb, initial='simple', alpha=0.9, h=3)
sum((hb - fitted(fit3))) #129.61
fit4 <- ses(hb, initial='simple', alpha=0.5, h=3)
sum((hb - fitted(fit4))) #213.47
fit5 <- ses(hb, initial='simple', alpha=0.95, h=3)
sum((hb - fitted(fit5))) #124.42
fit6 <- ses(hb, initial='simple', alpha=0.97, h=3)
sum((hb - fitted(fit6))) #122.56
fit7 <- ses(hb, initial='simple', alpha=0.99, h=3)
sum((hb - fitted(fit7))) #120.82
fit8 <- ses(hb, initial='simple', alpha=1, h=3)
sum((hb - fitted(fit8))) #120

alpha <- c(1, 0.99, 0.97, 0.95, 0.5, 0.9, 0.2, 0.8)
sse <- c(120, 120.82, 122.56, 124.42, 213.47, 129.61, 465.09, 142.59)

plot(alpha, sse)

# As alpha increases the SSE declines with diminishing returns.

fit1 <- ses(hb, initial='simple', h=4)
fit2 <- ses(hb, initial='simple', alpha=1, h=4)

par(mfrow=c(2,1))
plot(fit1, main="Automatic alpha")
plot(fit2, main="Custom alpha: 1")

# Letting the SES function automatically choose alpha gives lower point forecasts
# than the previous custom alpha produced.

fit3 <- ses(hb, initial='optimal', h=4)
sum((hb - fitted(fit3)))

# Setting the intial paramater to 'optimal' gives very similar results to 'simple'.


