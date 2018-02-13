# install.packages("dummies")
library(dummies)    # to create dummy variables based on the unique values of a variable

# Read in data
# na.strings = "No answer" simply means that answers coded in as "No answer" in the data is NA
dat_vote <- read.csv("vote_parties.csv", na.strings = "No answer")
# continue by removing the NA values
dat_vote <- na.omit(dat_vote)

######################
# Create dummy variables using the dummy() function from the dummies package
# I will be using the variable social democrats as my dependent variable, and look at the f32 variable
# start by extracting these variables to a new dataframe
model_dat <- data.frame(f32 = dat_vote$f32, 
                        soc_dem = dat_vote$socialdemocrats)

# drop levels of the dummy factor, what levels should we drop?
levels(dat_vote$f32)

# drop "Refusal":
# step (1): get row positions of these values
idx <- which(dat_vote$f32 == "Refusal")
# step(2): simply remove these rows from model_dat
model_dat <- model_dat[-idx, ]   # -idx keeps all row values but thouse found in the idx vectoridx <- which(dat_vote$f32 == "Don't know")



# now we can create dummy variables and append them to the model_dat dataframe
# there are many ways to do this, but lets not take any shortcuts. Assign the dummies
# as a new object in the global environment. Please take a look at this new matrix
# as well. R has sort of "spread" out the values of the f32 variable into a number of
# columns where each column is a dummy for a unique value of the f32 variable.
# Factor nivå är den kategoriska variabeln värde (center partiet etc)
dummies <- dummy("f32", model_dat, sep = "_")
ncol(dummies)
length(unique(model_dat$f32))
# note the number of columns of this new matrix (this is the same number
# as the levels of the factor variable). 

# there are many ways to combine your results into one object I choose to use
# cbind(), where c stands for columnns
model_dat <- cbind(model_dat$soc_dem, dummies)
# i use the as.data.frame() call here because cbind gives back a matrix
model_dat <- as.data.frame(model_dat)
# Here R has not carried along the name from the model_dat for social dem called V1 now
colnames(model_dat)
# lets fix it
(colnames(model_dat)[1] <- "soc_dem")

## Now we estimate the logit model
# PUnkten betyder alla andra data i 'data'
m <- glm(as.factor(soc_dem) ~ ., data = model_dat, 
         family = binomial(link='logit'))
summary(m)

# Note that if you call summary(m) you would have one coefficient as NA.
# This is because you have all your dummies included in the model fit. If
# you want to control this and choose which should get included in the intercept, 
# simply remove one (your so called reference level for the dummy). I'll remove
# the second column from model_dat in the glm() call, directly in the data parameter
# -2 means all cols in model_dat drop column 2, level 1 on f32 which becomes reference
m <- glm(as.factor(soc_dem) ~ ., data = model_dat[, -2], 
         family = binomial(link='logit'))

summary(m)

# Getting log likelihood value (for test of likert restriction)
logLik(m)

# McFadden R2
# start with fitting a model that only has an intercept
m_null = glm(as.factor(soc_dem) ~ 1, data = model_dat, family = binomial(link='logit'))
# McFadden R2 is then defined as:
1 - logLik(m)/logLik(m_null)

# Count R2
# for this one we need the predicted probabilities based on our estimated model
# preds is a vector with probabilities that dependent variable is one
preds <- predict.glm(m, model_dat, type="response")
# then we (1) need to keep in mind that values above 0.5 will be classified
# as 1, the individual voting for the socialdemocrats and (2) compare with what
# we know in our data. I recommend that you simply copy my code here

# we will store our correct classifications in the total vector
# where the values 1 will represent correct classification
total = rep(0, nrow(model_dat))   
# && is 'and'; or is || 
for(i in 1:nrow(model_dat)){
  if(preds[i] > 0.5 && model_dat$soc_dem == 1){  # && means "and". The statement in if() will only be TRUE if both booleans are true
    # this is if model predicts 1, and the true value is 1
    total[i] = 1
  } else if((preds[i] < 0.5) && (model_dat$soc_dem[i] == 0)){
    # this is if the model predicts 0, and the true value is 0
    total[i] = 1
  }
}

# divide the no of correct classification with the sample size
# This can be done by simply taking the mean of the total vector
mean(total)
 
######################
# Estimate probit model
m_prob <- glm(as.factor(soc_dem) ~ ., data = model_dat[, -2], 
              family = binomial(link='probit'))

summary(m_prob)


# Predict individ 5
id5 = model_dat[5, ]
# response returns predictions as probability (not log odds)
pred <- predict.glm(m, id5, type="response")

# change a value of f32_D to 0 of id5
id5$f32_D = 0
pred2 = predict.glm(m, id5, type="response")

# Difference in probability 
pred-pred2




