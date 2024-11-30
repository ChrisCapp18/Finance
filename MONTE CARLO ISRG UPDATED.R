## Packages Needed
install.packages("quantmod")
library(quantmod)

install.packages("tidyquant")
library(tidyquant)

##Data Sourcing
getSymbols("ISRG", from = "2014-10-05", to = "2024-11-05",
           auto.assign = TRUE, warnings = FALSE)
attach(ISRG)
View(ISRG)

## Daily mean and standard deviation
daily_mean <- mean(dailyReturn(ISRG))
daily_std_dev <- sd(dailyReturn(ISRG))

## Define number of days to be simulated
no_of_days <- 365 #Set variable to 365 days
starting_price <- last(ISRG$ISRG.Close)[[1]] #Closing price from (11.05.24) - last item in our frame

## Generate daily return
set.seed(101) #Set seed for reproducibility of random numbers

returns <- 1+rnorm(no_of_days, mean = daily_mean, sd = daily_std_dev) #Generate random variables
prices <- cumprod(c(starting_price, returns)) #Calculate cumulative product

## Plot of simulated price action of 120 days in the future
plot(prices, type = 'l', ylab = "Simulated price of ISRG", xlab = "Days")

##Multiple simulations
no_of_sims <- 10000
returns_list <- matrix(0, nrow = no_of_sims, ncol = no_of_days) #define matrices
prices_list <- matrix(0, nrow = no_of_sims, ncol = no_of_days + 1)
#Note: returns_list and prices_list are actually matrices 

for (i in 1:no_of_sims) { # for loop - 100000 iterations
  returns_list[i, ] <- rnorm(no_of_days, mean = daily_mean, sd = daily_std_dev) #Generate random variables
  prices_list[i, ] <- cumprod(c(starting_price, 1+returns_list[i,])) #Calculate cumulative product
  
}

#export into csv file
write.csv(prices_list, file = "ISRG GBM Simultions.csv", row.names = TRUE)

getwd()

##SUMMARY STATS

#BASE
median(prices_list)

#BEAR
quantile(prices_list, .25)

#BULL
quantile(prices_list, .75)

mean(prices_list)

sd(prices_list)

count1 = sum(prices_list > 469)

print(count1)

prob_greaterthanbase <- count1/length(prices_list)

print(prob_greaterthanbase)
