# simulate AR(2) process

# Set parameters
phi_1        = 0.5  # AR(1) parameter
phi_2        = 0  # AR(2) parameter
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

# initiate series. Let the first two elements of y_AR be innovations
y_AR <- c(rnorm(1)*sqrt(sigma2), rnorm(1)*sqrt(sigma2), rep(0, NumObsSim-2))
e    <- rnorm(NumObsSim)*sqrt(sigma2) # residual

# Simulate/generate realization of AR
for(i in 3:NumObsSim){
  y_AR[i] <- phi_1*y_AR[i-1] + phi_2*y_AR[i-2] + e[i]
}
# should be stored as time series object, makes for easier ploting.
y_AR <- as.ts(y_AR) 

#############
# Time series plot, ACF and PACF in one
layout(matrix(c(1, 1, 1, 1,
                2, 2, 3, 3), nrow=2, byrow=TRUE))

ts.plot(y_AR[1:numObsToPlot], ylab = "Y", main = "Time series plot of AR")
acf(y_AR[1:NumObsSim], lag.max = ACFLagstoPlot, 
    type = "correlation", plot = T, main = "ACF of AR",
    xlim=c(1,ACFLagstoPlot))
acf(y_AR[1:NumObsSim], lag.max = ACFLagstoPlot, type = "partial", plot = T, main = "PACF of AR")
par(mfrow = c(1,1)) # resets to default

# values can be accessed in the following manner
acf(y_AR[1:NumObsSim], lag.max = ACFLagstoPlot, type = "correlation", plot = F)
acf(y_AR[1:NumObsSim], lag.max = ACFLagstoPlot, type = "partial", plot = F)


