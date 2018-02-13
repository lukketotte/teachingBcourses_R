## regression with data from
## simulation program

## No heteroskedasticity
set.seed(1234)              # set seed for reproducibility
n <- 100                    # sample size

x <- rnorm(n)               # independent variable

beta <- 2                   # the real coefficient of x on y

error <- rnorm(n, sd = 2)   # errors are normally distributed

y <- 1 + x*beta + error     # Generate data

dat <- data.frame(Y = y,    # Combine x and y into one data frame
                  X = x)

## With heteroskedasticity
set.seed(1234)
error_h <- error
error_h[ x > .5] <- error_h[ x > .5]*3.5   # residuals values are now a function of x! 
y <- 1 + x*beta + error_h
dat_h <- data.frame(Y = y, X = x)


