install.packages("moments")

# for kurtosis and skewnewss
library(moments)
# mean, median, variance, skewness and kurtosis of one out column
# out

# For OLS, column one: remember matrix[row index, col index]
# Note you need to change to 
# summary(out[, 2]) for the second estimator etc
summary(out[, 1]) # mean and median found in summary call
var(out[, 1])
skewness(out[, 1])
kurtosis(out[, 1])

