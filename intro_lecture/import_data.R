# This is a short introduction to loading .csv files 
# into R. The main function to use is read.csv()
# The main argument you supply is the file name
# as a string. If you have the file in your .Rproject
# folder (you can check the location of this using
# getwd()) you only have to give the filename. Otherwise
# you have to supply a string which specifies the 
# location of the file aswell. For example
#  "C:/Users/lukas/Desktop/Econometrics_intro_R/faithful.csv"

### Old Faithful Geyser Data
# waiting: waiting time between eruptions
# eruptions: duration of the eruption

# Yellowstone national park, Wyoming, USA


# In the folder you find three different versions of the faithful data
# you can inspect these in a basic text editor (such as notepad) to
# see the difference. Don't open these in excel, altough sometimes your 
# computer will default to excel. Excel will not show you the information
# you want! 

# When you inspect the file you want to see what seperates the values. 
# Often it's a comma, but sometimes it's whitespace, tab or ;. This you have
# to specify in the sep argument of read.csv. 

# csv, comma seperated files
# read into R using the function read.csv
dat <- read.csv("faithful.csv")                 # comma is default
dat <- read.csv("faithful_tab.csv", sep = "\t") # \t is tab (or indent to exact)
dat <- read.csv("faithful_ws.csv", sep = " ")   # white space seperates the entries

## Fit your own linear model using the 
# faithful data and plot the results
