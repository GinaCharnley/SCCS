## Analysis: National and Sub-National, with lags 

# Load packages 
library(dplyr)
library(survival)

# Read in data 
outb <- readxl::read_excel("drc_dat.xlsx", sheet = "outb")
conf <- readxl::read_excel("drc_dat.xlsx", sheet = "conf")  

outb <- readxl::read_excel("gui_dat.xlsx", sheet = "outb")
conf <- readxl::read_excel("gui_dat.xlsx", sheet = "conf") 

# Subset the outbreaks data to those with >1 case 
outb <- subset(outb, case == 1)

# Format outbreaks and conflict data 
outb <- data.frame(admin = outb$admin, eventday = outb$continuous_weeks)
conf <- data.frame(admin = conf$admin, exday = conf$continuous_weeks) 
outb <- outb %>% distinct()
conf <- conf %>% distinct()

# Subset the conflict data to only those with outbreaks 
admins <- unique(outb$admin)
conf <- subset(conf, admin %in% admins)

# Create observation period 
conf$start <- 1 
conf$end <- 183

# Create the exposure gaps 
conf <-  conf %>%
  mutate(time_from_start = exday - start)
conf <- conf %>%
  arrange(time_from_start, admin) %>%
  group_by(admin) %>%
  mutate(extreme_gap = time_from_start - lag(time_from_start)) %>%
  mutate(extreme_gap = ifelse(is.na(extreme_gap), 0, extreme_gap))

# Create the exposure periods 
list1 <- split(conf, f = conf$admin, drop = FALSE)
lctn <- names(list1)

df1 <- lapply(list1, function(lctn) {
  lctn <- lctn %>% 
    arrange(exday) %>% 
    mutate(grp =  exday - lag(exday, default = 0) > 1) %>%
    group_by(grp = cumsum(grp)) %>% 
    mutate(exp_start = case_when(exday == 1 ~ 1, 
                                 extreme_gap == 1 ~ min(exday), 
                                 extreme_gap != 1 ~ exday)) %>% 
    group_by(grp) %>% 
    mutate(exp_end = max(exday))
})

df1 <- plyr::ldply(df1, data.frame)
# admin, start, end, exp_start, exp_end, exposure
df1 <- df1[c(2,4,5,9,10)]
df1 <- df1 %>% distinct()
df1$exposure <- "Conflict"

# Create the lag periods 
names(df1)[5]<-paste("Lag1")
df1$Lag2 <- df1$Lag1 + 2
df1$Lag4 <- df1$Lag1 + 4
df1$Lag6 <- df1$Lag1 + 6
df1$Lag8 <- df1$Lag1 + 8
df1$Lag10 <- df1$Lag1 + 10 
df1 <- df1 %>% tidyr::gather(lag, exp_end, 5,7,8,9,10,11)
df1 <- df1 %>% mutate(exp_end = case_when(exp_end > end ~ end,
                                          exp_end <= end ~ exp_end))

df1$lctn_lag <- paste(df1$admin, "-", df1$lag)
list2 <- split(df1, f = df1$lctn_lag, drop = FALSE)
lctn <- names(list2)

df2 <- lapply(list2, function(lctn) {
  lctn %>% 
  arrange(exp_start) %>% 
    mutate(grp = cumsum(c(1, head(lctn$exp_end, -1) < tail(lctn$exp_start,-1)))) %>% 
    group_by(grp) %>% 
    mutate(exp_start = min(exp_start)) %>% 
    mutate(exp_end = max(exp_end)) %>% 
    ungroup() %>% distinct()
})

# Create the un-exposure periods 
df3 <- lapply(df2, function(lctn) {
 lctn %>% 
    mutate(exp_start2 = case_when(
      exp_start == min(exp_start) ~ start,
      exp_start != min(exp_start) ~ lag(exp_end))) %>%
    mutate(exp_end2 = case_when(
      exp_start == min(exp_start) ~ exp_start - 1,
      lag(exp_start) == exp_start ~ lead(exp_start) - 1,
      lag(exp_start) != max(exp_start) ~ exp_start - 1))
})

df4 <- lapply(df2, function(lctn){  
  lctn <- lctn %>% 
    mutate(exp_start3 = case_when(exp_start == max(exp_start) ~ max(exp_end) + 1)) %>%
    mutate(exp_end3 = case_when(max(exp_end) == end ~ NA,
                                exp_end == max(exp_end) ~ end)) %>% 
    mutate(exp_start3 = case_when(exp_start3 > end ~ NA, 
                                  exp_start3 < end ~ exp_start3)) %>% 
    mutate(exp_end = case_when(exp_end >= end ~ end,
                               exp_end < end ~ exp_end))
})

# Merge the exposure periods and arrange the dataframe for binding 
df2 <- plyr::ldply(df2, data.frame)
# admin, lag, start, end, exp_start, exp_end, exposure
df2 <- df2[c(2,7,3,4,5,8,6)]

# Merge the non-exposure periods and arrange the dataframe for binding 
df3 <- plyr::ldply(df3, data.frame)
df3$exposure <- "Peace"
# admin, lag, start, end, exp_start2, exp_end2, exposure
df3 <- df3[c(2,7,3,4,11,12,6)]
names(df3)[5]<-paste("exp_start")
names(df3)[6]<-paste("exp_end")
df3 <- df3[!(df3$exp_start == 1 & df3$exp_end == 0 ),]

df4 <- plyr::ldply(df4, data.frame)
df4$exposure <- "Peace"
# admin, lag, start, end, exp_start2, exp_end2, exposure
df4 <- df4[c(2,7,3,4,11,12,6)]
names(df4)[5]<-paste("exp_start")
names(df4)[6]<-paste("exp_end")
df4 <- na.omit(df4)
df3 <- rbind(df3, df4)

# Merge the exposure and non-exposure periods 
df2 <- rbind(df2, df3)

# Create the exgr 
df2 <- df2 %>% 
  mutate(exgr = case_when(exposure == "Conflict" ~ 1,
                          exposure == "Peace" ~ 0)) 
df2$exgr[is.na(df2$exgr)]<-0

# Create the interval 
df2 <- df2 %>% 
    mutate(interval = case_when(exp_start == exp_end ~ 1,
                                exp_start != exp_end ~ exp_end-exp_start)) %>% 
    mutate(loginterval = log(interval))

# Create the event
df2 <- merge(df2, outb, by = "admin", all = TRUE)
df2 <- na.omit(df2)

df2$ID <- paste(df2$admin, "-", df2$eventday, "-", df2$lag)
list3 <- split(df2, f = df2$ID, drop = TRUE)
ids <- names(list3)

list3 <- lapply(list3, function(ids){
  ids <- ids %>% mutate(event = case_when(between(eventday, ids$exp_start, ids$exp_end) ~ 1))
  ids$event[is.na(ids$event)]<-0;ids
})

df3 <- plyr::ldply(list3, data.frame)
# admin, lag, start, end, exp_start, exp_end, exposure, exgr, event, interval, loginterval
df3 <- df3[c(2:9,14,10,11)]
df3 <- df3 %>% distinct()
df3 <- df3 %>% group_by(admin, lag, start, end, exp_start, exp_end, exposure, exgr, interval, loginterval) %>% 
  mutate(event2 = sum(event)) %>% ungroup()
df3$event <- NULL
df3 <- df3 %>% distinct()
names(df3)[11]<-paste("event")

# Fit the model - National 
list5 <- split(df3, f = df3$lag, drop = FALSE)
lags <- unique(df3$lag)

mods1 <- lapply(list5, function(lags) {
  clogit <- clogit(event ~ exgr + strata(admin) + offset(loginterval), data = lags)
})

# Extract the results 
results1 <- lapply(mods1, function(lags) {
  lags <- summary(lags)
  lags <- data.frame(lags$coefficients, lags$conf.int)
  row.names(lags)<-NULL; lags
})
results1 <- plyr::ldply(results1, data.frame)

# Fit the model - Sub-National 
df3$lctn_lag <- paste(df3$admin, "-", df3$lag)
list6 <- split(df3, f = df3$lctn_lag, drop = FALSE)
list6 <- list6[sapply(list6, nrow) > 1]
lctn <- names(list6)

mods2 <- lapply(list6, function(lctn) {
  clogit <- clogit(event ~ exgr + offset(loginterval), data = lctn)
})

# Extract the results 
results2 <- lapply(mods2, function(lctn) {
  lctn <- summary(lctn)
  lctn <- data.frame(lctn$coefficients, lctn$conf.int)
  row.names(lctn)<-NULL; lctn
})
results2 <- plyr::ldply(results2, data.frame)


####################################################
## Analysis: National and Sub-National, with lags and event type 

# Read in data 
outb <- readxl::read_excel("drc_dat.xlsx", sheet = "outb")
conf <- readxl::read_excel("drc_dat.xlsx", sheet = "conf")  

outb <- readxl::read_excel("gui_dat.xlsx", sheet = "outb")
conf <- readxl::read_excel("gui_dat.xlsx", sheet = "conf") 

# Subset the outbreaks data to those with >1 case 
outb <- subset(outb, case == 1)

# Format outbreaks and conflict data 
outb <- data.frame(admin = outb$admin, eventday = outb$continuous_weeks)
conf <- data.frame(admin = conf$admin, exday = conf$continuous_weeks, event_type = conf$event_type) 
outb <- outb %>% distinct()
conf <- conf %>% distinct()

# Subset the conflict data to only those with outbreaks 
admins <- unique(outb$admin)
conf <- subset(conf, admin %in% admins)

# Create observation period 
conf$start <- 1 
conf$end <- 183

# Create the exposure gaps 
conf <-  conf %>%
  mutate(time_from_start = exday - start)
conf <- conf %>%
  arrange(time_from_start, admin) %>%
  group_by(admin) %>%
  mutate(extreme_gap = time_from_start - lag(time_from_start)) %>%
  mutate(extreme_gap = ifelse(is.na(extreme_gap), 0, extreme_gap))

# Create the exposure periods 
conf$event_admin <- paste(conf$admin, "-", conf$event_type)
list1 <- split(conf, f = conf$event_admin, drop = FALSE)
lctn_loc <- names(list1)

df1 <- lapply(list1, function(lctn_loc) {
  lctn_loc <- lctn_loc %>% 
    arrange(exday) %>% 
    mutate(grp =  exday - lag(exday, default = 0) > 1) %>%
    group_by(grp = cumsum(grp)) %>% 
    mutate(exp_start = case_when(exday == 1 ~ 1, 
                                 extreme_gap == 1 ~ min(exday), 
                                 extreme_gap != 1 ~ exday)) %>% 
    group_by(grp) %>% 
    mutate(exp_end = max(exday))
})

df1 <- plyr::ldply(df1, data.frame)
# event_type, admin, start, end, exp_start, exp_end
df1 <- df1[c(2,4,5,6,11,12)]
df1 <- df1 %>% distinct()
df1$exposure <- "Conflict"

# Create the lag periods 
names(df1)[6]<-paste("Lag1")
df1$Lag2 <- df1$Lag1 + 2
df1$Lag4 <- df1$Lag1 + 4
df1$Lag6 <- df1$Lag1 + 6
df1$Lag8 <- df1$Lag1 + 8
df1$Lag10 <- df1$Lag1 + 10 
df1 <- df1 %>% tidyr::gather(lag, exp_end, 6,8,9,10,11,12)
df1 <- df1 %>% mutate(exp_end = case_when(exp_end > end ~ end,
                                          exp_end <= end ~ exp_end))

df1$lctn_lag <- paste(df1$admin, "-", df1$lag, "-", df1$event_type)
list2 <- split(df1, f = df1$lctn_lag, drop = FALSE)
lctn <- names(list2)

df2 <- lapply(list2, function(lctn) {
  lctn %>% 
    arrange(exp_start) %>% 
    mutate(grp = cumsum(c(1, head(lctn$exp_end, -1) < tail(lctn$exp_start,-1)))) %>% 
    group_by(grp) %>% 
    mutate(exp_start = min(exp_start)) %>% 
    mutate(exp_end = max(exp_end)) %>% 
    ungroup() %>% distinct()
})

# Create the un-exposure periods 
df3 <- lapply(df2, function(lctn) {
  lctn %>% 
    mutate(exp_start2 = case_when(
      exp_start == min(exp_start) ~ start,
      exp_start != min(exp_start) ~ lag(exp_end))) %>%
    mutate(exp_end2 = case_when(
      exp_start == min(exp_start) ~ exp_start - 1,
      lag(exp_start) == exp_start ~ lead(exp_start) - 1,
      lag(exp_start) != max(exp_start) ~ exp_start - 1))
})

df4 <- lapply(df2, function(lctn){  
  lctn <- lctn %>% 
    mutate(exp_start3 = case_when(exp_start == max(exp_start) ~ max(exp_end) + 1)) %>%
    mutate(exp_end3 = case_when(max(exp_end) == end ~ NA,
                                exp_end == max(exp_end) ~ end)) %>% 
    mutate(exp_start3 = case_when(exp_start3 > end ~ NA, 
                                  exp_start3 < end ~ exp_start3)) %>% 
    mutate(exp_end = case_when(exp_end >= end ~ end,
                               exp_end < end ~ exp_end))
})

# Merge the exposure periods and arrange the dataframe for binding 
df2 <- plyr::ldply(df2, data.frame)
# event_type, admin, lag, start, end, exp_start, exp_end, exposure
df2 <- df2[c(2,3,8,4,5,6,9,7)]

# Merge the non-exposure periods and arrange the dataframe for binding 
df3 <- plyr::ldply(df3, data.frame)
df3$exposure <- "Peace"
# event_type, admin, lag, start, end, exp_start2, exp_end2, exposure
df3 <- df3[c(2,3,8,4,5,12,13,7)]
names(df3)[6]<-paste("exp_start")
names(df3)[7]<-paste("exp_end")
df3 <- df3[!(df3$exp_start == 1 & df3$exp_end == 0 ),]

df4 <- plyr::ldply(df4, data.frame)
df4$exposure <- "Peace"
# event_type, admin, lag, start, end, exp_start2, exp_end2, exposure
df4 <- df4[c(2,3,8,4,5,12,13,7)]
names(df4)[6]<-paste("exp_start")
names(df4)[7]<-paste("exp_end")
df4 <- na.omit(df4)
df3 <- rbind(df3, df4)

# Merge the exposure and non-exposure periods 
df2 <- rbind(df2, df3)

# Create the exgr 
df2 <- df2 %>% 
  mutate(exgr = case_when(exposure == "Conflict" ~ 1,
                          exposure == "Peace" ~ 0)) 
df2$exgr[is.na(df2$exgr)]<-0

# Create the interval 
df2 <- df2 %>% 
  mutate(interval = case_when(exp_start == exp_end ~ 1,
                              exp_start != exp_end ~ exp_end-exp_start)) %>% 
  mutate(loginterval = log(interval))

# Create the event
df2 <- merge(df2, outb, by = "admin", all = TRUE)
df2 <- na.omit(df2)

df2$ID <- paste(df2$admin, "-", df2$eventday, "-", df2$lag, "-", df2$event_type)
list3 <- split(df2, f = df2$ID, drop = TRUE)
ids <- names(list3)

list3 <- lapply(list3, function(ids){
  ids <- ids %>% mutate(event = case_when(between(eventday, ids$exp_start, ids$exp_end) ~ 1))
  ids$event[is.na(ids$event)]<-0;ids
})

df3 <- plyr::ldply(list3, data.frame)
# event_type, admin, lag, start, end, exp_start, exp_end, exposure, exgr, event, interval, loginterval
df3 <- df3[c(3,2,4:10,15,11,12)]
df3 <- df3 %>% distinct()
df3 <- df3 %>% group_by(event_type, admin, lag, start, end, exp_start, exp_end, exposure, exgr, interval, loginterval) %>% 
  mutate(event2 = sum(event)) %>% ungroup()
df3$event <- NULL
df3 <- df3 %>% distinct()
names(df3)[12]<-paste("event")

# Fit the model - National 
df3$lag_event <- paste(df3$lag, "-", df3$event_type)
list5 <- split(df3, f = df3$lag_event, drop = FALSE)
lags <- names(list5)

mods1 <- lapply(list5, function(lags) {
  clogit <- clogit(event ~ exgr + strata(admin) + offset(loginterval), data = lags)
})

# Extract the results 
results1 <- lapply(mods1, function(lags) {
  lags <- summary(lags)
  lags <- data.frame(lags$coefficients, lags$conf.int)
  row.names(lags)<-NULL; lags
})
results1 <- plyr::ldply(results1, data.frame)

# Fit the model - Sub-National 
df3$lag_event_lctn <- paste(df3$admin, "-", df3$lag, "-", df3$event_type)
list6 <- split(df3, f = df3$lag_event_lctn, drop = FALSE)
list6 <- list6[sapply(list6, nrow) > 1]

mods2 <- lapply(list6, function(lctn) {
  clogit <- clogit(event ~ exgr + offset(loginterval), data = lctn)
})

# Extract the results 
results2 <- lapply(mods2, function(lctn) {
  lctn <- summary(lctn)
  lctn <- data.frame(lctn$coefficients, lctn$conf.int)
  row.names(lctn)<-NULL; lctn
})
results2 <- plyr::ldply(results2, data.frame)



