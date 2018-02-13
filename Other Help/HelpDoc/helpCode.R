library(datasets)
?datasets
library(help = "datasets")

str(iris)
which((iris$Species == "setosa") | (iris$Species == "virginica"))

vec <- NULL
for(i in 1:nrow(iris)){
  if((iris$Species == "setosa") || (iris$Species == "virginica") == TRUE){
    vec[i] <- i
  }
}

c("setosa", "virginica") %in% iris$Species

which(c("setosa", "virginica") == iris$Species)
dat <- read.csv("japanese_emmigration.txt", sep = ";")
dat
nrow(dat);ncol(dat)

(colnames(dat) <- c("prefecture","emigrants","cultivated","farmland",
                    "arable","labors","pioneer"))

fac <- as.factor(dat$pioneer)
levels(fac) <- c("No pioneer", "Pioneer")

dat <- data.frame(Name = c("Mikaela", "Anders", "Regina", "Gustav"),
                  Age = c(24, 15, 57, 32))
?data.frame

(names <- as.character(c("Mikaela", "Anders", "Regina", "Gustav", "Anders")))
# convert to factor
(names <- factor(names, levels = c("Anders", "Gustav", "Mikaela", "Regina")))
# each unique name becomes a level of the factor
levels(names) <- c("Mikaela", "Anders", "Regina", "Gustav")
names
