# This is to test your distribution of R and RStudio

x <- rnorm(1000) # This command generates random numbers that are normally distributed.
plot(x) # This is a built-in plotting tool that will show the values generated.

install.packages("tidyverse")
library(tidyverse)

y <- data.frame(x)
ggplot(y) +
      geom_histogram(aes(x)) # This command is part of ggplot2 and will generate a histogram of the data generated.
