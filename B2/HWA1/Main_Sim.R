# Compares four estimators of beta2 in single linear regression
# By Johan Lyhagen, Uppsala University, 2005-02-22
# Translated to R by Lukas Arnroth, Uppsala University, 2016-06-12

library(reshape2)
library(ggplot2)

### Simulation program starts

# set size of sample by assigning a value to n which
# will be used later in the program
n <- 40

# Number of replications
reps <-2000

# Set seed of random number generator
# IMPORTANT!
# Set the seed to the birthdate of one of the group members!
# For mee it would look like this:
seed_group <- 911021
set.seed(seed_group) 

# so in your code, change the 911021 to the birthdate of
# any one of your group members.

# Parameters of the regression
beta_1 <- 0
beta_2 <- 1
sigma_2 <- 1 # Note that this is the variance, not the standard error

# Generate t, which is the independent variable
# (wrote x as in the eviews code)
x <- 1:n # 1, ..., 40

xx = x*x # 1^2, ..., 40^2
sumxx<- sum(xx) # sum of squares of x


# Create matrix to store the results from the simulations
# We want one row (nrow) per repetition and one column for 
# each estimator. 
out <- matrix(nrow = reps, ncol = 4, dimnames = list(NULL, c("OLS", "Mean", "End", "OLS_inter")))

# generate Y and residuals
set.seed(seed_group) 
for(i in 1:reps){
  # residuals
  u = rnorm(n)*sqrt(sigma_2)
  # data generating process of y, the dependent variable
  y = beta_1 + beta_2*x + u
  yx = y*x
  # estimator 1, replication i
  out[i, 1] <- sum(yx)/sumxx
  # estimator 2, replication i
  out[i, 2] <- mean(y)/mean(x)
  # estimator 3, replication i
  # n'th Y and T 
  out[i, 3] <- y[n]/n
  # estimator 4, replication i
  out[i, 4] <- cov(y, x)/var(x)
}

# The melt function is from the reshape2 package. It makes the 
# matrix into a data frame which is more suitable for plotting in ggplot
data <- melt(out, value.name = "Estimate")
# calling 'data' you notice that the matrix has been transformed into a data frame
# with 3 variables, first one is the time(rowname), second is estimator name (column name) 
# and third is the estimate (elements of the out matrix)

# change the column names to be more intuitive
colnames(data) <- c("row Id", "Estimator", "Estimate")



################################
#### End of data simulation ####
################################

# density plot
ggplot(data) + 
  stat_density(aes(x = Estimate, color = Estimator), 
               geom = "line", position = "identity", size = 1) +
  theme_light() + 
  scale_colour_discrete("Estimator") + 
  xlab(NULL)

### 2A
# in evaluating a OLS model we will use the 'lm' (linear model) function.
# calling ?lm() gives the R documentation for this function
?lm()
# the requirement in this case is to supply the lm with its y and x
# note in the documentation that the formula uses '~' instead of '='
# as '=' is reserved in the R syntax as assignment (same as '<-')
# Look into the global environment and consider how to do the estimate
# Remember to save the estimate in the global environment by assigning 
# the lm() call to an object as follows:

# estimate <- lm(a ~ b)

# When evaluating the model, use the summary() function on the linear model object
# you saved in the global environment:

# summary(estimate)

# solution
estimate <- lm(y ~ x) # ignore data argument of lm as we're calling two seperate vectors
summary(estimate)
# evaluate by plots
# begin by combining necessary results into one data frame
estimate_data <- data.frame(y_hat = estimate$fitted.values,
                            resid = estimate$residuals,
                            y = y,
                            t = x)



