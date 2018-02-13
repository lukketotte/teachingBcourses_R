# Note, please direct all questions regarding the R-code to Lucas Arnroth
# Lukas.Arnroth@statistik.uu.se room B341

# read data
dat <- read.csv("B3_HWA2.csv")


# we want 400, and leave 4 out for forecasting
datm <- dat[1:400, ] # subset 400 first rows of dat, include all columns
datf <- dat[401:404, ] # subset last 4 rows of dat, include all columns

# process 1 example
y_1 <- as.ts(datm[ , 2])

# use the example from before for plotting
# split up plot window
layout(matrix(c(1, 1, 2,
                1, 1, 3), nrow=2, byrow=TRUE)) 
ts.plot(y_1, main = "Time Series") # time series plot
acf(y_1, lag.max = 20, type = "correlation", plot = T, main = "ACF") # ACF
acf(y_1, lag.max = 20, type = "partial", plot = T, main = "PACF") # PCAF
par(mfrow = c(1,1)) # set plot window to default

### Estimate model
E <- as.ts(datm$E)

# Think the function to look at is arima()
?arima
# x: y_1
# order c(AR-order, degree of differencing, and the MA order)
# ARMA(1,1)
m1 <- arima(y_1, order = c(1, 0 , 1))
summary(m1) # nothing really useful
# store sigma^2 for ploting later
sigma2 <- m1$sigma2
# check residuals by plot
E1 <- residuals(m1) # take out residuals from the fitted model

# split up plot window as before
layout(matrix(c(1, 1, 2,
                1, 1, 3), nrow=2, byrow=TRUE)) 
ts.plot(E1, ylab = "Residuals", col = "blue", main = "Residuals from Fitted Model")
abline(a = mean(E1), b = 0) # adds horizontal line with mean(E1) as intercept and 0 slope
abline(a = mean(E1) + sigma2, b = 0, lty="dotted") # same as above + sigma2
abline(a = mean(E1) - sigma2, b = 0, lty="dotted") # same as above - sigma2

acf(E1, lag.max = 20, type = "correlation", plot = T, main = "ACF") # ACF

acf(E1, lag.max = 20, type = "partial", plot = T, main = "PACF") # PCAF

par(mfrow = c(1,1)) # set plot window to default

### For testing auto correlations, why is t = 0 included? Not the case in 
# eviews. What does it mean? Check with Lars
e1_acf <- acf(E1, lag.max = 20, type = "correlation", plot = F) 


### Forecasting
# use the predict function. The object parameter is the model from before
# and n.ahead gives number of time periods to forecast
y_1_pred <- predict(object = m1, n.ahead = 4)

# we can submit more than one time series to the ts.plot() function. In this case
# i add, apart from  predicted y, predicty y +- se of prediction
ts.plot(y_1_pred$pred, y_1_pred$pred + y_1_pred$se, 
        y_1_pred$pred - y_1_pred$se, ylab = "Predicted Y", 
        main = expression(paste("Predicted ", Y['1'])), # we can use mathematical notation in r plots!
        lty=c(1:3),
        col = c("blue", "black", "black"))

# compare fitted and actual
ts.plot(datf$Y1, y_1_pred$pred, col = c("red", "blue"))
# x & y need to be adjusted manually. A good idea is to take min(x) and add 1 as x - coordinate
# and max y remove 1 as y coordinate. Then fine tune 
legend(x = 401, y = 2, legend = c("actual", "fitted"), col = c("red", "blue"), lty=c(1,1))

### need some measurements of the forecast
y1 <- as.vector(dat$Y1[401:404]) # no real need for these to be time series objects...
y1_hat <- as.vector(y_1_pred$pred) # ... so just store them as your plain vanilla vectors :)

# RMSEA
sqrt(mean((y1-y1_hat)^2))
# MAE 
mean(abs(y1-y1_hat))
# MAPE, not exactly sure whether n = 4 or 404 in the calculations
mape <- 0
for(i in 1:4) {
  mape <- mape + abs((y1[i] - y1_hat[i])/y1[i])
}
100*mean(abs((y1-y1_hat)/y1))
