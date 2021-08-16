# This is to test your distribution of R and RStudio

x <- rnorm(1000)
plot(x)

install.packages("tidyverse")
library(tidyverse)

y <- data.frame(x)
ggplot(y) +
      geom_histogram(aes(x))
