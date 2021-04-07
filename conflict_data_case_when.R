# Conflict data algorithm - Using case_when
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

gap = 10 # change depending on sensitivity analysis 

df1 <- conflict_data %>% group_by(admin1) %>% 
  mutate(exp_start = case_when(
  conflict_gap == 0 ~ years_week, 
  conflict_gap >= gap ~ years_week))

df1 <- df1 %>% group_by(admin1) %>% 
  mutate(exp_end = case_when(
  lead(conflict_gap) >= gap ~ years_week, 
  is.na(lead(conflict_gap)) ~ years_week)) %>% 
  arrange(admin1)

df1 <- df1 %>% group_by(admin1, exp_start, exp_end) %>% distinct()

df2 <- df1 %>% select(admin1, exp_start) %>% distinct() 
df2 <- df2 %>% filter(!is.na(exp_start)) 
df2 <- df2 %>% group_by(admin1) %>% 
  mutate(outbreak_index = paste0(admin1, "_", 1:n()))

df3 <- df1 %>% select(admin1, exp_end) %>% distinct() 
df3 <- df3 %>% filter(!is.na(exp_end))
df3 <- df3 %>% group_by(admin1) %>% 
  mutate(outbreak_index = paste0(admin1, "_", 1:n()))

output <- left_join(df2, df3)
