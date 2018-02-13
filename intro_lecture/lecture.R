### The data.frame
# the traditional data set. The difference from matricies
# is that the columns of the dataframe doesn't all have to 
# be the same type

dat

# we can subset the data frame the same way as a matrix
# mat[row index, column index]

# second column of dat
dat[ , 2] # all rows, column 2
dat[1 , ] # row 1, all columns

# We can select columns (variables) using the $ operator
# lets check the variable names
colnames(dat)
# choose variable Y using $
dat$Y
# is equivalent to
dat[, 1]

# change variable names
colnames(dat) <- c("Y", "Z")
colnames(dat) <- c("Y", "X")

# R does a implicit type conversion of 26 and 52 to
# strings.
matrix(c("Alice", "Trevor", 26, 52), ncol = 2, nrow = 2)
# A data frame would allow this however
data.frame(Name = c("Alice", "Trevor"),
           Age = c(26, 52))


## The str() & summary()
# your first diagnostic of the data
str(dat) 
summary(dat)

##### Fit a regression model using lm() ######
?lm
# the formula argument: y = b_0 + x*b_1, the problem is
# that '=' is reserved for assignment. Instead we use '~' tilde
# the data argument: data = your data.frame

m1 <- lm(Y ~ X, data = dat) # y_hat = 1.074 + 1.948*x + res
# m1 is a named list
# but for all intents and purposes look at it as data frame
# and use the $ - operator for choosing elements from it

# summary() has unique interactions with lots of functions,
# lm is one of them

summary(m1)

### Let's look at the results by plotting
# first you call plot(...), R will then default to some type
# of plot. 
?plot
plot(dat$X, dat$Y)
# change the titles of the y and x axis
# xlab & ylab arguments 
plot(dat$X, dat$Y, xlab = "X", ylab = "Y", 
     main = "Y vs X")

# adding type = "n" (as in null) doesn't create
# any graphics in the plots
# we want to use the lm object 'm1' and plot
# the regression line onto the scatter
# for that we use the abline()
abline(m1, col = "blue", lwd = 2) #col = colour, lwd = line width

# Quick note: make sure that the plot(.) call has the dependent
# variable on the y-axis and independent on x-axis
plot(dat$Y, dat$X , main = "Y vs X")
abline(m1, col = "blue", lwd = 2)

## Let's investigate the residuals by plotting them
# against the fitted values

# let's extract residuals and fitted values from m1
# using $

res <- m1$residuals
fit <- m1$fitted.values

plot(fit, res)
# add a dashed line at res = 0
abline(a = 0, b = 0, lty = 2, col = "gray") #lty = line type


### Use dat_h to fit a lm() of Y = b_0 + b_1*X
colnames(dat_h)
m1_h <- lm(Y ~ X, data = dat_h)

# as a sidenote, if you want to have no intercept
lm(Y ~ X - 1, data = dat_h)

### Create a scatter plot of Y vs X and plot the linear model
### onto the scatter plot
plot(dat_h$X, dat_h$Y, xlab = "X", ylab = "Y")
abline(m1_h, col = "red")

### Extract the residuals and fitted values from m1_h
### store them in res_h & fit_h. Create a scatterplot
### of res_h vs fit_h

res_h <- m1_h$residuals
fit_h <- m1_h$fitted.values

plot(fit_h, res_h)
abline(a = 0, b = 0, lty = 2, col = "gray")
?abline
