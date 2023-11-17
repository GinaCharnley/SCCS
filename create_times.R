
library(zoo)
library(tidyverse)

new_times <- function(t){
  # define the start of the observation period 
  st <- as.yearmon("2010-01-01")
  # define the end of the observation period 
  en <- as.yearmon("2020-12-01")
  # create the sequence starting at "st" and ending at "en" 
  t <- as.tibble(as.Date(seq(st, en, 1/12), frac = 1))
  # seperate the date into year, months and day columns 
  t <- t %>% separate(value, into = c("year", "month", "day"), sep = "-")
  # remove days 
  t$day <- NULL
  # create continuous months 
  t$continuous_months <- seq(1, 132, 1)
  # create a fraction of the observation period 
  t <- t %>% mutate(frac = continuous_months/132*100)
}

# create an empty tibble to run the function 
t <- tibble()
# create the time dataset 
t_data <- new_times(t)
rm(t)
t_data$year <- as.numeric(t_data$year)
t_data$month <- as.numeric(t_data$month)




