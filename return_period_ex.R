# Return period based on annual maximums

# Load needed libraries, includes custom package
library(readr)
library(dplyr)
library(devtools)
install_github("LimpopoLab/hydrostats")
library(hydrostats)

## ANNUAL FLOOD EXAMPLE 1
# This is an example from Chow, Maidment, and Mays, 1988, Applied Hydrology
z <- read_csv("ChowMaidmentMays_Ex12_3_3.csv")
#z <- rename(z, AnnualMaxDischarge_cfs = q) # remember, units are cfs

# Compare data and log-transformed data
hist(z$AnnualMaxDischarge_cfs, xlab = "Annual Flood Level (cfs)", main = "")
z$log_q <- log(z$AnnualMaxDischarge_cfs, 10) # this is log base 10
hist(z$log_q, xlab = "log of Annual Flood Level", main = "") # this is much closer to a normal distribution

## Return Period
T_R <- 50 # return period in years (in this case)
Fx <- 1 - (1/T_R)

## LOGNORMAL ANALYSIS
m <- mean(z$log_q)
s <- sd(z$log_q)

# Note, the reason the distribution is so important is because 
# the probabilities and return period are taken from the 
# distribution given the statistics, m and s ONLY.

# the random variable (mean 0, standard deviation 1) that 
# produces a cummulative probability, or probability of 
# non-exceedence, of Fx.

lnd.x <- qnorm(Fx)

lnd.y <- (lnd.x * s) + m
lnd.z <- 10^lnd.y # this is the T_R flood level

## log-Pearson type III analysis
# For this distribution, we use the skewness to determine the 
# inverse CDF.
c <- skew(z$log_q)
lp3.x <- pt3(c,Fx)
lp3.y <- (lp3.x * s) + m
lp3.z <- 10^lp3.y # this is the T_R flood level

## Compare the Lognormal and log-Pearson type III results (note the skewness is close to zero)
print(paste0("Lognormal ", T_R, " year flood level ", lnd.z, " cfs"))
print(paste0("log-Pearson type III ", T_R, " year flood level ", lp3.z, " cfs"))

