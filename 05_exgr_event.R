##### Format the data for model fitting 
## Creating the exgr and event 

# Load packages
library(dplyr)
library(tidyverse)

# Load data the exposure data 
tmp <- read_csv("generated_data/tmp_extremes.csv")
tmp$...1 <- NULL

# Load the event data 
outb <- read_csv("generated_data/outb.csv")

# Temporal data is needed 
# Run utils/create_times.R 

# Format the exposure data 
# Subset the climate data to extremes only 
tmp_h <- filter(tmp, tmp_H == 1)
tmp_l <- filter(tmp, tmp_L == 1)

# Only need the location and the exday 
# Remove any duplicate exposures 
tmp_h <- tibble(lctn_pr = tmp_h$lctn_pr,
                exday = tmp_h$continuous_months)
tmp_h <- tmp_h %>% distinct()

tmp_l <- tibble(lctn_pr = tmp_l$lctn_pr,
                exday = tmp_l$continuous_months)
tmp_l <- tmp_l %>% distinct()

# Format the event data 
# Only need the location and the event 
outb <- left_join(outb, t_data, by = c("year", "month"))
outb <- tibble(lctn_pr = outb$lctn_pr,
               eventday = outb$continuous_months)
outb <- outb %>% distinct()

# Merge to form one dataset
dataH <- full_join(outb, tmp_h, by = c("lctn_pr"))
dataH <- na.omit(dataH)
dataH <- dataH %>% distinct()
dataL <- full_join(outb, tmp_l, by = c("lctn_pr"))
dataL <- na.omit(dataL)
dataL <- dataL %>% distinct()

# Merge the exposures to the time data 
list1 <- list(dataH = dataH, dataL = dataL)
data <- names(list1)

list2 <- map(list1, function(data){
  # Create a dataset of all possible months there could be an exposure for each lctn_pr
  lctn_pr <- tibble(lctn_pr = unique(data$lctn_pr))
  t <- tibble(exday = t_data$continuous_months)
  t <- cross_join(t,lctn_pr)
  # Join this with all the months there was an exposure 
  data2 <- right_join(data, t, by = c("lctn_pr", "exday"))
  data2 <- data2 %>% distinct()
  data2 <- data2 %>% rename_with(~"time", matches("exday"))
  # Transform all months with no exposure to NA 
  data2 <- data2 %>% arrange(lctn_pr, time) %>% 
    mutate(exday = case_when(!is.na(eventday) ~ time, is.na(eventday) ~ NA))
  data2$eventday <- NULL
  data2 <- data2 %>% distinct()

  # Create a dataset of all possible months there could be an event for each lctn_pr
  t <- tibble(eventday = t_data$continuous_months)
  t <- cross_join(t,lctn_pr)
  # Join this with all the months there was an event
  data3 <- right_join(data, t, by = c("lctn_pr", "eventday"))
  data3 <- data3 %>% distinct()
  data3 <- data3 %>% rename_with(~"time", matches("eventday"))
  # Transform all months with no event to NA 
  data3 <- data3 %>% arrange(lctn_pr, time) %>% 
    mutate(eventday = case_when(!is.na(exday) ~ time, is.na(exday) ~ NA))
  data3$exday <- NULL
  data3 <- data3 %>% distinct()
  # Merge so each location period has: 
    # All months possible months with an exp/event
    # All months with an exposure 
    # All months with an event 
  data1 <- left_join(data2, data3, by = c("lctn_pr", "time"))
})

# Create the exgr and event
list3 <- map(list2, function(data){
  data <- data %>% mutate(exgr = case_when(exday == time ~ 1, is.na(exday) ~ 0))
  data <- data %>% mutate(event = case_when(eventday == time ~ 1, is.na(eventday) ~ 0))
  data$exday <- NULL; data
  data$eventday <- NULL; data
})

# Create the interval 
list3 <- map(list3, function(data){
  data <- data %>% group_by(lctn_pr) %>% mutate(interval = 1)
  data <- data %>% group_by(lctn_pr) %>% mutate(loginterval = log(interval))
})

# Merge extreme high and extreme low into one dataset 
list2env(list3, envir = .GlobalEnv)
dataH <- dataH %>% rename_with(~"exgrH", matches("exgr")) 
dataL <- dataL %>% rename_with(~"exgrL", matches("exgr")) 
data <- full_join(dataH, dataL, by = c("lctn_pr", "time", "event", "interval", "loginterval"))
data$exgrH[is.na(data$exgrH)]<-0
data$exgrL[is.na(data$exgrL)]<-0

