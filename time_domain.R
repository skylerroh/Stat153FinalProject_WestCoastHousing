#time_domain
load(file = '~/Github/Stat153FinalProject_WestCoastHousing/west_homes_ts.RData')
library('astsa')

west_pre08 = ts(west[time(west)<2008], frequency = 12, 
                start = 1973, end = c(2007, 12), class = 'ts')
###up to 2008
plot(west_pre)
acf(west_pre, lag.max = floor(length(west_pre)/2))
pacf(west_pre, lag.max = floor(length(west_pre)/2))

west_pre_d1 = diff(west_pre, differences = 1)
plot(west_pre_d1)
acf(west_pre_d1, lag.max = floor(length(west_pre)/2)) # ma seasonal by year
pacf(west_pre_d1, lag.max = floor(length(west_pre)/2)) # ~ sinusoidal with decay

west_pre_d1_s1 = diff(west_pre_d1, lag = 12) #seasonal difference
plot(west_pre_d1_s1)
acf(west_pre_d1_s1, lag.max = 48) # suggests seasonal MA 1
pacf(west_pre_d1_s1, lag.max = 72) # decays over seasons, possibly suggests seasonal AR 4
fit.1 = sarima(west_pre, 0, 1, 1, 0, 1, 1, 12) #1 ARIMA (0, 1, 1) X (0, 1, 1)_12
fit.2 = sarima(west_pre, 4, 1, 0, 0, 1, 1, 12) #2 ARIMA (4, 1, 0) X (0, 1, 1)_12
fit.3 = sarima(west_pre, 4, 1, 1, 0, 1, 1, 12) #3 ARIMA (4, 1, 1) X (0, 1, 1)_12

fit.4 = sarima(west_pre, 0, 1, 1, 4, 1, 1, 12) #1 ARIMA (0, 1, 1) X (4, 1, 1)_12
fit.5 = sarima(west_pre, 4, 1, 0, 4, 1, 1, 12) #2 ARIMA (4, 1, 0) X (4, 1, 1)_12
fit.6 = sarima(west_pre, 4, 1, 1, 4, 1, 1, 12) #3 ARIMA (4, 1, 1) X (4, 1, 1)_12

criteria_pre = sapply(list(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6), 
                      function(fit) c(AIC = fit$AIC, AICc = fit$AICc, BIC = fit$BIC))
colnames(criteria_pre) = c(paste('model', as.character(1:6), sep = ' '))

### post 2008
plot(west_post)
west_post_d1 = diff(west_post, differences = 1)
plot(west_post_d1)
acf(west_post_d1, lag.max = 84)
pacf(west_post_d1, lag.max = 84) 

west_post.fit = sarima(west_post, 0, 1, 1)
west_post.fit$fit
resid(west_post.fit$fit)
which(abs(resid(west_post.fit$fit)) > 2)
sarima.for(west_post, 24, 0, 1, 1)



###combined, all data 1973 - 2015
#d = 0
plot(west)
acf(west)
mtext('d = 0', outer = TRUE, cex = 1.25)
tapply(west, rep(1:10, times = c(rep(52, 4), rep(51, 6))), mean)

#d = 1
west_d1 = diff(west, differences = 1)
plot(west_d1)

acf(west_d1, lag.max = floor(length(west_d1)/2))
mtext('d = 1', outer = TRUE, cex = 1.25)
tapply(west_d1, rep(1:10, times = c(rep(52, 3), rep(51, 7))), mean)

#log diff 1
log.west_d1 = diff(log(west), 1)
plot(log.west_d1)
acf(log.west_d1, lag.max = floor(length(west_d3)/2))

log.west_d1_s1 = diff(log.west_d1, lag = 12) #seasonal difference
acf(log.west_d1_s1, lag.max = 60)
pacf(log.west_d1_s1, lag.max = 84)
tapply(log.west_d1, rep(1:10, times = c(rep(52, 3), rep(51, 7))), mean)


dif.log.fit4 = sarima(log(west), 4, 1, 1, 2, 1, 1, 12) #ARIMA (4, 1, 1) X (2, 1, 1)_12
dif.log.fit1 = sarima(log(west), 4, 1, 1, 0, 1, 1, 12) #ARIMA (4, 1, 1) X (0, 1, 1)_12 # best
dif.log.fit2 = sarima(log(west), 4, 1, 1, 0, 1, 2, 12) #ARIMA (4, 1, 1) X (4, 1, 1)_12
dif.log.fit3 = sarima(log(west), 4, 1, 1, 4, 1, 1, 12) #ARIMA (4, 1, 1) X (4, 1, 1)_12

criteria = sapply(list(dif.log.fit1, dif.log.fit2, dif.log.fit3, dif.log.fit4), 
                  function(fit) c(AIC = fit$AIC, AICc = fit$AICc, BIC = fit$BIC))
colnames(criteria) = c(paste('model', as.character(1:4), sep = ' '))
sarima.for(log(west), 12, 4, 1, 1, 0, 1, 1, 12)








