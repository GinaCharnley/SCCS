# Conflict data algorithm - Using if/else
# This is to create my exposure periods 
# Data imported is the conflict datasets, including province, epiweek and epiyear

library(dplyr)
conflict_data <- conflict_data %>% distinct()
conflict_data <-  conflict_data %>%
  mutate(years_from_start = MMWRyear - min(MMWRyear)) %>%
  mutate(weeks_from_start = MMWRweek + years_from_start*52) %>%
  mutate(year_week = paste0(MMWRyear, "_", MMWRweek))
conflict_data <- conflict_data %>% mutate(years_week = paste0(MMWRyear, "_", MMWRweek))
conflict_data <- conflict_data %>%
  arrange(weeks_from_start)%>%
  group_by(admin1) %>%
  mutate(conflict_gap = weeks_from_start - lag(weeks_from_start)) %>%
  mutate(conflict_gap = ifelse(is.na(conflict_gap), 0, conflict_gap))

exposure_periods <- function(conflict_data, gap = 10){
    
    df1 <- group_by(admin1) %>% mutate(exp_start = 
                                         if(conflict_gap == 0) {
                                           years_week
                                         } else if (conflict_gap >= gap) {
                                           years_week
                                         } else(exp_start = NA)) %>%
      group_by(admin1) %>% mutate(exp_end =
                                    if(lead(conflict_gap) >= gap){
                                      years_week
                                    } else if(is.na(lead(conflict_gap))){
                                      years_week
                                    } else {
                                      NA
                                    })

  df1 <- df1 %>% group_by(admin1, exp_start, exp_end) %>% distinct()
  
  df2 <- df1 %>% select(admin1, exp_start) %>% distinct() 
  df2 <- filter(!is.na(exp_start)) 
  df2 <- df2 %>% group_by(admin1) %>% 
    mutate(outbreak_index = paste0(admin1, "_", 1:n()))
  
  df3 <- df1 %>% select(admin1, exp_end) %>% distinct() 
  df3 <- filter(!is.na(exp_end))
  df3 <- df3 %>% group_by(admin1) %>% 
    mutate(outbreak_index = paste0(admin1, "_", 1:n()))
  
  output <- left_join(df2, df3)
  return(output)
} 

# gap is changed depending on sensitivity analysis  

  
