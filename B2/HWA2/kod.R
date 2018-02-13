library(normtest) # for Jarque-Bera test of normality
library(ggplot2) # for plots
library(fmsb) # for VIF

# Example 
data <- read.csv("japanese_emmigration.txt", sep = ";")
?read.csv
### Data
# http://www.stat.ufl.edu/~winner/data/japanemg.txt
### variable descrition
# X1 - prefecture
# Y - emigrants per 1 million residents
# X2 - % land cultivated by tenant farmers
# X3 - % change in ratio of tenant farmlands
# X4 - avarage area of arable land per farm
# X5 - number of government labors workin in Hawaii in 100s
# X6 - existence of pioneer immigrants 1=y, 0=n

# The variable names are not very informative
colnames(data)

# Lets assign more informative variable names for this assignment
colnames(data) <- c("prefecture","emigrants","cultivated","farmland","arable","labors","pioneer")

# two usable functions as a step 1 is to look at (1) the structure of the data using the
# str() function. It shows each variable, what type R has read it as, aswell as the first 10 observations
str(data)

# (2) look at summary statistics of the variables using the summary() function
summary(data)

### Now I move forward by estimating the regression functions. 
# looking at ?lm we see that calling lm as done below a intercept will be included.
# It's the default of the lm() function

# model 1
m1 <- lm(data = data, emigrants ~ cultivated + farmland)
# model 2
m2 <- lm(data = data, emigrants ~ cultivated + farmland + arable)
# model 3
m3 <- lm(data = data, emigrants ~ cultivated + farmland + arable + labors)


# If i wanted to use log-transformed variables i would define m1 as
m1_log <- lm(data = data, log(emigrants) ~ log(cultivated) + log(farmland))

# if you run the above line of code you should notice the error message
# NaN's produced when transforming the farmland variable. Why might that be?
# Hint: is the logarithm defined for all values of farmland

# For this example you should come to the conclusion that using the log of farmland
# is not a good idea as data is lost.

### Evaluate m1
summary(m1)
# R^2 = 0.01708
# F-statistic = 0.3659, p-val = 0.6964
# Not good. The model does not explain variation in emigrants

### normality of residuals
u <- m1$residuals
jb.norm.test(u)
# Null is rejected on 5% level, residuals are not normally distributed

# Investigate by plots. What happens if you change the nclass argument?
hist(u, main = "Histogram of Residuals", nclass = 15)

# nclass determined how many bins you create in displaying the data
# as a histogram
hist(u, main = "Histogram of Residuals", nclass = 2)

# QQ-plot
ggplot() +
  stat_qq(aes(sample =(u-mean(u))/var(u))) +
  theme_bw()

### check for multicollinearity using VIF
# to do this you need to fit a linear model with the 
# independent variable you're checking as dependent of 
# the other independent variables

# Say I want to investigate cultivated:
m_vif <- lm(data = data, cultivated ~ farmland)
VIF(m_vif)
# Less than 10, suggesting its not a problem!


#################
#################

#### White's test for heteroskedasticity function ####
# student supply the function with a dataframe and a vector of variable names
white_test <- function(dat, names_independent, model){
  # how many variables have the user submitted?
  n_vars <- length(names_independent)
  
  #subset supplied data
  sub_data <- dat[, which(colnames(dat) %in% names_independent)]
 
   # set up dataframe that will be used
  out_dat <- data.frame(V1 = rep(NA, nrow(dat)))
  
  # how many columns the final data set should contain
  # number of crossproducts = (n_vars^2-n_vars)/2, see quadratic form
  n_cols <- n_vars*2 + (n_vars^2-n_vars)/2
  
  ### Squares
  for(i in 1:n_vars) {
    out_dat[, i] <- (sub_data[, i])^2
  }
  ### non squares
  for(i in (n_vars+1):(2*n_vars)){
    out_dat[, i] <- sub_data[, (i-n_vars)]
  }
  ### Crossproducts
  ind_vec <- (2*n_vars+1):n_cols
  ind <- 0
  for(j in 1:(n_vars-1)){
    for(k in (j+1):n_vars){ 
      # if(k == j) next # squares are already dealt with
      ind <- ind + 1
      out_dat[, ind_vec[ind]] <- sub_data[, j]*sub_data[, k]
    }
  }
  # add new dependent variable as outlined by white's test
  out_dat$u2 <- (model$residuals)^2
  
  # what we want to return
  result <- summary(lm(data = out_dat, u2 ~ .))
  # returns list with F-statistic and it's corresponding p-value
  return( list("F-statistic" = result$fstatistic[[1]],
               "p-value" = pf(result$fstatistic[1], result$fstatistic[2], result$fstatistic[3], 
                              lower.tail = F)[[1]]))
} 

### Trying out  
# white_test <- function(dat, names_independent, model)
# m1
white_test(dat = data, names_independent = c("cultivated", "farmland"), model = m1)
# m2
white_test(dat = data, names_independent = c("cultivated", "farmland", "arable"), model = m2)
# m3
white_test(dat = data, names_independent = c("cultivated", "farmland", "arable", "labors"), model = m3)



#################
#################
