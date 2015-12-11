load(file = '~/Github/Stat153FinalProject_WestCoastHousing/west_homes_ts.RData')

west_d1 = diff(west, differences = 1)
west_d2 = diff(west, differences = 2)
west_d3 = diff(west, differences = 3)

par(mfrow = c(1,2))
par(mfrow = c(1,1))
#d = 0
plot(west)
acf(west)
mtext('d = 0', outer = TRUE, cex = 1.25)
tapply(west, rep(1:10, times = c(rep(52, 4), rep(51, 6))), mean)

#d = 1
plot(west_d1)
acf(west_d1)
acf(west_d1, lag.max = floor(length(west_d1)/2))
mtext('d = 1', outer = TRUE, cex = 1.25)
tapply(west_d1, rep(1:10, times = c(rep(52, 3), rep(51, 7))), mean)

#d = 2
plot(west_d2)
acf(west_d2)
acf(west_d2, lag.max = floor(length(west_d2)/2))
mtext('d = 2', outer = TRUE, cex = 1.25)
tapply(west_d2, rep(1:10, times = c(rep(52, 2), rep(51, 8))), mean)

#d = 3
plot(west_d3)
acf(west_d3, lag.max = floor(length(west_d3)/2))
mtext('d = 3', outer = TRUE, cex = 1.25)
tapply(west_d3, rep(1:10, times = c(rep(52, 1), rep(51, 9))), mean)

#log diff 1
log.west_d1 = diff(log(west), 1)
plot(log.west_d1)
acf(log.west_d1)
acf(log.west_d1, lag.max = floor(length(west_d3)/2))
pacf(log.west_d1)
tapply(log.west_d1, rep(1:10, times = c(rep(52, 3), rep(51, 7))), mean)

pacf(west)
pacf(west_d1)
pacf(west_d2)
pacf(west_d3)

plot(west_d1, type="b", ylab="thousands of homes")


dif.log.fit = arima(log(west), order = c(1, 1, 1), seasonal = list(order = c(9, 0, 0), period = 4))
dif.log.fit1 = sarima(log(west), 1, 1, 1, 9, 0, 0, 4)
dif.log.fit$aic
plot(dif.log.fit$residuals)
acf(dif.log.fit$residuals)
acf(dif.log.fit$residuals, lag.max = floor(length(west_d3)/2))
pacf(dif.log.fit$residuals)


box_smoother = function(L, ts = west){#x is time series, L is box length (*must be odd number)
  if (L%%2 == 0) stop('L must be an odd integer')
  mvspec(ts, kernel = kernel('daniell', (L-1)/2))
}
box_smoother(13, west_d1)
mvspec(log(west), span = 6)







# #parameter estimation
# p_list = 0:4
# d_list = 0:2
# q_list = 0:6
# p_seasonal = 0:2
# d_seasonal = 0:2
# q_seasonal = 0:2
# 
# grid = expand.grid(p_list, d_list, q_list, p_seasonal, d_seasonal, q_seasonal)
# names(grid) = c('p', 'd', 'q', 'p_s', 'd_s', 'q_s')


# find_param_fit = function(ts, param_grid){
#   min_aic = 9999
#   tryCatch(
#   for (i in 1:nrow(grid)){
#     print(param_grid[i,])
#     p = param_grid[i, 'p']
#     d = param_grid[i, 'd']
#     q = param_grid[i, 'q']
#     p_s = param_grid[i, 'p_s']
#     d_s = param_grid[i, 'd_s']
#     q_s = param_grid[i, 'q_s']
#     arima_fit = arima(west, order = c(p, d, q), seasonal = list(order = c(p_s, d_s, q_s), period = 12))
#     if (arima_fit$aic < min_aic){
#       min_aic = arima_fit$aic
#       fit.best = arima_fit
#     }
#   }, error = function(e) NULL)
#   return(fit.best)
# }
# 
# fit = find_param_fit(west, grid)




