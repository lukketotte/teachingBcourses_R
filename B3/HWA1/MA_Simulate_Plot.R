# simulate MA(2) process
# Parameter values and settings
theta_1      = 0.4  # MA(1) parameter
theta_2      = 0.3    # MA(2) parameter
NumObsSim    = 5000 # Simulated Sample size 
numObsToPlot = 500  
ACFLagstoPlot= 20
sigma2       = 1    # variance of noise term

# Set seed for random number generation.
# Choose birthdate of any student in your
# group. 
seed_by <- 960231
set.seed(seed_by)

# Simulate MA(2) - using built in arima.sim()
y_MA <- arima.sim(model = list(ma = c(theta_1, theta_2)), n = NumObsSim, sd = sqrt(sigma2))

#############
# Time series plot, ACF and PACF in one
layout(matrix(c(1, 1, 1, 1,
                2, 2, 3, 3), nrow=2, byrow=TRUE))

ts.plot(y_MA[1:numObsToPlot], ylab = "Y", main = "Time series plot of MA")
acf(y_MA[1:NumObsSim], lag.max = ACFLagstoPlot, 
    type = "correlation", plot = T, main = "ACF of MA",
    xlim=c(1,ACFLagstoPlot))
acf(y_MA[1:NumObsSim], lag.max = ACFLagstoPlot, type = "partial", plot = T, main = "PACF of MA")
par(mfrow = c(1,1)) # resets to default

# values can be accessed in the following manner
acf(y_MA[1:NumObsSim], lag.max = ACFLagstoPlot, type = "correlation", plot = F)
acf(y_MA[1:NumObsSim], lag.max = ACFLagstoPlot, type = "partial", plot = F)

