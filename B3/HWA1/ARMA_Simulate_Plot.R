# simulate ARMA(1,1) process

# Set parameters
phi_1        = 0  # AR(1) parameter
theta_1      = 0 # MA(1) parameter
sigma2       = 1    # variance of noise term
NumObsSim    = 5000 # Simulated Sample size
numObsToPlot = 500  
ACFLagstoPlot= 20
sigma2       = 1    # variance of noise term

# Set seed for random number generation.
# Choose birthdate of any student in your
# group. 
seed_by <- 960231
set.seed(seed_by)

# Simulate/generate realization of ARMA
y_ARMA <- arima.sim(model = list(ar = phi_1, ma = theta_1), n = NumObsSim, sd = sqrt(sigma2))

# should be stored as time series object, makes for easier ploting.
y_ARMA <- as.ts(y_ARMA) 


#############
# Time series plot, ACF and PACF in one
layout(matrix(c(1, 1, 1, 1,
                2, 2, 3, 3), nrow=2, byrow=TRUE))

ts.plot(y_ARMA[1:numObsToPlot], ylab = "Y", main = "Time series plot of ARMA")
acf(y_ARMA[1:NumObsSim], lag.max = ACFLagstoPlot, 
    type = "correlation", plot = T, main = "ACF of ARMA",
    xlim=c(1,ACFLagstoPlot))
acf(y_ARMA[1:NumObsSim], lag.max = ACFLagstoPlot, type = "partial", plot = T, main = "PACF of ARMA")
par(mfrow = c(1,1)) # resets to default

# values can be accessed in the following manner
acf(y_ARMA[1:NumObsSim], lag.max = ACFLagstoPlot, type = "correlation", plot = F)
acf(y_ARMA[1:NumObsSim], lag.max = ACFLagstoPlot, type = "partial", plot = F)


