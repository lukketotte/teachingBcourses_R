### Ploting using base functions of R

# this is the simple way of plotting it, ignoring the fact that y can be 
# seen as a time series

# set up the plot by specifiying x and y with respective limits
plot(x, y, xlim=range(1:n), ylim=range(estimate_data$y), xlab="x", ylab="y", 
     main = "Model Fit",pch=16, col = "gray")
# add fitted line to the plot object
abline(estimate, col = "red")

# if you don't like the look of the scatterplot, and not using ggplot,
# the most simple way is plotting actual as a time series object
# using as.ts(), standing for as time series.

# par(mfrow = c()) splits up the plot window into sections, 
# in this call 2 rows and 1 columns. When calling the first plot
# after, only the upper region will be filled.
par(mfrow = c(2, 1)) 
# par(mfrow = c(1,1)) # back to default, one plot in the window
ts.plot(y = as.ts(estimate_data$y), col = "blue", ylab = "Y", xlab = "", 
        main = "Model Evaluation") # plot actual y's
abline(estimate, col = "red") # add fited y's
# adding legend, 40 is x-coordinate of legend, col are the colours, and lty specifies type of icon
legend(40, c("actual", "fitted"), col = c("blue", "red"), lty=c(1,1), cex = 0.75)


ts.plot(y = as.ts(estimate_data$resid), main = "", ylab = "Residuals", xlab = "")
abline(0, 0, col = "gray")

par(mfrow = c(1,1))
?plot
plot(y = estimate_data$y, x = estimate_data$t, col = "blue", ylab = "Y", xlab = "", 
        main = "Model Evaluation", type = "n", ylim = c(-5, max(y))) # plot actual y's
lines(estimate_data$y, col = "blue")
abline(estimate, col = "red") # add fited y's
lines(estimate_data$resid, col = "black")
abline(a = 0, b = 0, col = "gray", lty = 2)

# adding legend, 40 is x-coordinate of legend, col are the colours, and lty specifies type of icon
legend(40, c("actual", "fitted", "residual"), col = c("blue", "red", "black"), lty=c(1,1,1), cex = 0.75)

?lines
