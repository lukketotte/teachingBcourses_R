# As a side comment, if you're getting stressed about the number of objects in 
# the global environment you can either use the broom symbol to clear it out or
# 'rm(list = ls())'.

# rm(list = ls())

### Create the vectors which will be combined into a data frame

# The dependent vectors
Y_1 <- c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 
         7.24, 4.26, 10.84, 4.82, 5.68)

Y_2 <- c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10,
         6.13, 3.10, 9.13, 7.26, 4.74)

Y_3 <- c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84,
         6.08, 5.39, 8.15, 6.42, 5.73)

Y_4 <- c(6.58, 5.76, 7.70, 8.84, 8.47, 7.04,
         5.25, 12.5, 5.56, 7.91, 6.89)

# The independent vectors
X_1 <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
X_2 <- X_1
X_3 <- X_1
X_4 <- c(rep(8, 7), 19, rep(8,3))

# combine it all into a data frame
data_5 <- data.frame(Y1 = Y_1, Y2 = Y_2, Y3 = Y_3, Y4 = Y_4,
                     X1 = X_1, X2 = X_2, X3 = X_3, X4 = X_4)

### Fit the four models
m1 <- lm(Y1 ~ X1, data = data_5)
m2 <- lm(Y2 ~ X2, data = data_5)
m3 <- lm(Y3 ~ X3, data = data_5)
m4 <- lm(Y4 ~ X4, data = data_5)

# To present the results we'll split the
# plot window into four cells (two rows 
# and two columns) 
par(mfrow=c(2,2))

# plot the actual Y's
plot(data_5$Y1, data_5$X1, xlab="x", ylab="y", 
     main = "Model Fit X1 & Y1",pch=16, col = "gray")
# add fitted line to the plot
abline(m1, col = "red")


### for x2 & y2
# plot the actual Y's
plot(data_5$Y2, data_5$Xx, xlab="x", ylab="y", 
     main = "Model Fit X2 & Y2",pch=16, col = "gray")
# add fitted line to the plot
abline(m2, col = "red")


### for x3 & y3
# plot the actual Y's
plot(data_5$Y3, data_5$X3, xlab="x", ylab="y", 
     main = "Model Fit X3 & Y3",pch=16, col = "gray")
# add fitted line to the plot
abline(m3, col = "red")

### for x4 & y4
# plot the actual Y's
plot(data_5$Y4, data_5$X4, xlab="x", ylab="y", 
     main = "Model Fit X4 & Y4",pch=16, col = "gray")
# add fitted line to the plot
abline(m4, col = "red")

