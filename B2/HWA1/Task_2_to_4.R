################
#### Task 2 ####
################

### B ###

# for Jarque–Bera test of normality we need the package "normtest".
# To install the package, simply remove the comment hashtag below
# to use the install command. Remember to remove this line or keep
# it as a comment in the future, no need to reinstall the package
# every time you want to run the code! 

# install.package("normtest")
library(normtest)

# note that the estimate_data is from the file "Main_Sim.R"
jb.norm.test(estimate_data$resid)

# Example of calculation correlation:
# You need to pick out the correct quantities from the regression in task 2A
# and use the commands below
a <- rnorm(20) # 20 N(0,1)'s
b <- rnorm(20, mean = 1, sd = 1.5) # 20 N(1, 1.5)'s
cor(a, b)

# the objects you replace a and b with are up to you 
# to decide.

####################
#### Task 3 & 4 ####
####################

# Use this code to complete the third and fourth task
# You simply use the objects x & y generated from the
# file 'Main_Sim.R' and change the weights accordingly

# wx = weight for the independent variable. 
# wy = weight for the dependent variable.
# If x or y is to remain unchanged you simply
# keep the weight as 1.
w_x = 10
w_y = 1

# rescale x & y
x_resc = x * w_x
y_resc = y * w_y

# fit the linear model. Note that no data argument is
# used in this call as the lm() function is supplied
# with two seperate vectors
resc_fit <- lm(y_resc ~ x_resc)

# to inspect the result, simply use summary
summary(resc_fit)

# Next is the plotting. Focus on just using this and getting out
# the result rather than making sense of the code. It's good if 
# you have a general understanding of what is going on it, but you
# are not expected to create this on your own!

# split the plot window into two rows and one column
par(mfrow = c(2,1))

### plot in the first position
# represent the actualy values as dots. Note that ylab and xlab
# will change when you change the weights!
plot(y = y_resc, x = x_resc, col = "blue", 
     ylab = paste(w_y, "* Y"), xlab = paste(w_x, "* X"), 
     main = "Model Evaluation", ylim = c(-5, max(y))*w_y) 
# add line from the linear model fit
abline(resc_fit, col = "red") 

# plot in the second position
plot(y = resc_fit$resid, x = x_resc, ylab = "Resid", xlab = paste(w_x, "* X"))
abline(a = 0, b = 0, col = "gray", lty = 2)
lines(y = resc_fit$resid, x = x_resc)

# reset plot window
par(mfrow = c(1,1))


