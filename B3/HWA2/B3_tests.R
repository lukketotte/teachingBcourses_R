
###############################
#### Test for stationarity ####
###############################


# The Box-Jenkins approach requires a stationary time series
# To test if a time series is stationary we perform a formal test. 
# The Augmented-Dickey fuller test has the following hypotheses
# H0: The series contains a unit root (which implies nonstationarity)
# H1: The series is stationary


# install the package tseries by removing the comment hashtag below. 
# remove the line or comment it again to avoid reinstalling the package
# everytime you run the code.
# install.packages("tseries")
library(tseries)

# Let y be the time series to test. We may use the deafult number of lags used in the function. 
adf.test(y)

# If the p-value is less than the chosen level of significance we reject the hypothesis 
# of a unit root and accept the alternative of stationarity. If the series does not pass the test -
# try applying a transformation to make it stationary. 



###########################################
#### Test for residual autocorrelation ####
###########################################


# Install the package gridExtra. Remove the line 
# or comment it again to avoid reinstalling the package
# everytime you run the code.

# install.packages("gridExtra")
library(gridExtra)

# Let E1 be the residuals from the fitted Arima model. 
# acf returns a list, in order to make the actual values into a vector we
# need to access the values in the list 

E1 <- dat$E
acf_vals <- acf(E1, lag.max = 20, type = "correlation", plot = F)[[1]][2:21] # Extract elements 2-21 (lag 1-20) from the first element of the list
pacf_vals <- pacf(E1, lag.max = 20, type = "partial", plot = F)[[1]][1:20] # Extract elements 1-20 (lag 1-20) from the first element of the list

# Make zero vectors for storing Q-stat and p-values
qstat = rep(0, 20)
pval = rep(0, 20)

# p = number of AR terms
# q = number of MA terms
# Adjust p and q in accordance with the fitted model 
p = 1
q = 1

# Ljung-Box tests for testing that several autocorrelations are simultaneously zero.
# From 1 to 20 lags. 
lags = 20
for(i in 1:lags){
  if(i > (q+p)){
    test <- Box.test(E1, type = "Ljung-Box", lag = i, fitdf=p+q)
  } else {
  test <- Box.test(E1, type = "Ljung-Box", lag = i)
  }
  qstat[i] <- test[[1]] # X-squared/Qstat
  pval[i] <- test[[3]] # p-value of statistic
}


# combine the 4 vectors into a DF
df <- data.frame(ACF = round(acf_vals, 3),
                  PACF = round(pacf_vals, 3),
                  Q.Stat = round(qstat, 2),
                  p.value = round(pval,3))

# the type of object grid.arrange() takes
y <- tableGrob(df)

# print it to the Plots window. 
grid.arrange(y)
