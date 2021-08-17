### Limpopo Resilience Lab Virtual R Workshop
### August 18-20, 2021
### Instructor:  Max Glines
### original code provided by Rachel Pilla


### DAY 1:  Introduction & Basics in R ###


######################################
### Introduction to R and packages ###
######################################

### PTT:
# - what is R?
# - layout/panes of RStudio
# - packages and installing
# - help files
# - using an R script (ALWAYS use script; how to run code; adding comments;
#   case sensitive; spaces, parentheses, quotations; carrot in console vs. +)
# - functions, arguments, and objects
# - types of data (numeric, integer, character, logical, complex)
# - data structures (vector, matrix, data frame)


### LIVE CODING (remainder of Day 1)


##########################################
### Basics in R from a prepared script ###
##########################################

# INSTALLING PACKAGES

install.packages(c("dpylr", "tidyr", "ggplot2", "lubridate", "ggpubr"))
#OR
install.packages(c('tidyverse','lubridate','ggpubr'))



# LOADING PACKAGES

library(dplyr)
#OR
library(tidyverse)


# R is a calculator

5 + 4




# WHAT IS AN OBJECT?
# Saving objects in R, and viewing object data
# (Environment Pane; how to name "best" - must begin with letter, caps or lowercase accepted;
# name objects clearly and consistently)

a <- 5 + 4







# Saving a script (file naming...) - do this now
# Scripts can be shared, edited, resaved, copy/pasted, etc. (much like a text editor)


# Style tips for writing code:
# 1) R is case sensitive ("A" does not equal "a")
# 2) Best practice is to put spaces between object, values, commas, etc. (though R does not require this)
# 3) Missing parentheses, commas, or quotation marks cause a vast majority of errors
# 4) Your collaborators and future self will apprecite detailed comments
# 5) Make sure the console has a blue ">" before running (if it has a "+" then it DID NOT FINISH the previous code for some reason)


# Errors and warning messages
# Errors BREAK the code, warnings run it (often not an issue, but keep an eye out that R is doing what you want)

A
c <- b - 3) * 2
cbind(c(1, 2, 3), c(1, 2, 3, 4))


# Basic functions

vector1 <- c(a, b, 1:3)
vector1






# Types of data in R:
# numeric/integer, character/strings, logical, factor (others are less common)

# Types of data structures in R:
# 1) vector - 1D, holds only 1 type of data
# 2) matrix - 2D, holds only 1 type of data
# 3) data frame - 2D, each column can be different type of data
# data frame is very common and useful; other types are generally less common (e.g., lists)

df1 <- data.frame("Column1" = c("A", "B", "C", "D", "E"),
                  "Column2" = vector1,
                  "Column3" = c("hi", "my", "name", "is", "Rachel"))
df1








######################
### Getting R Help ###
######################

?mean
?c

??csv
??average

# Google search is a great tool!
# StackOverflow often has really useful answers.

# For example, how do we read in data in a CSV file into R?




#############################
### Importing Data into R ###
#############################


# Create a CSV file for worldwide tropical cyclone data since 2000
# How to save as CSV file and then import to R
# (screen share data file to enter, then save as CSV together)


# Load in data file to R
# "TropicalCycloneData.csv" - pull up to have copied and saved

# OPTION 1 -- selecting file to read in with "read.csv"

file <- file.choose()
storms <- read.csv(file)







# OPTION 2 -- working directories (especially useful if you have lots of data files to import)

getwd()

# point-and-click from "Session" menu OR use "setwd()"

setwd( ... )
storms <- read.csv("TropicalCycloneData.csv")




# Viewing & subsetting data
# (when using square brackets, row ALWAYS comes first)

View(storms)


# select one column by name using "$"

storms$Year





# select one row OR one column using brackets "[]"

storms[5, ]

storms[ , 2]

mean(storms[ , 2])

# select one element/cell

storms[5, 1]

storms$Year[5]

storms$NumberTropicalCyclones[storms$Year == 2005]


# PRACTICE!
# 1) index 5 most recent years of storm data
# 2) find the maximum number of storms
# 3) index the year that had the maximum number of storms

# 1)


# OR




# 2)



# 3)


# OR





# OPEN the file "A7H008YRPK.CSV" in Excel.
# Note there are a few extra rows in the top,
# and we need to tell R to skip them.



# PRACTICE loading another CSV file into R
# choose whichever method to import you'd like!

# import the file "A7H008YRPK.CSV"

beit.bridge <- read.csv("A7H008YRPK.CSV", skip = 7)


head(beit.bridge)
tail(beit.bridge)
str(beit.bridge)
View(beit.bridge)

beit.bridge.clean <- beit.bridge[1:28, ]



################
### PRACTICE ###
################

# 1) import the "A7H008YRPK.CSV", 
#    remembering to skip the first 7 lines of the file





# 2) calculate the average Level and Flow across all years




# 3) find the minimum and maximum water levels AND which years they occurred




# 4) find the years with the highest and lowest flow

