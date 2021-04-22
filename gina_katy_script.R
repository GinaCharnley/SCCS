## SCCS Manual Script - Katy 
## At the moment all my data is in continuous days 
## I have also tried this in continuous weeks and I get the same results 
library(dplyr)
library(gnm)
library(survival)
library(readxl)
## For the original dataframe with one exposure period (exp = first/last conflict reports)
## This is based of the ox.txt and oxford.r example 
## So I first get my data looking like theirs 
original <- read_excel("Data/sccs_new.xlsx", sheet = "original")

original$province <- NULL

names(original) <- c("start", "end", "start1", "end1", "eventday")

indiv <- seq(1,298,1)
original <- cbind(original, indiv) # create an id number for each outbreak
original$exday <- original$start1 # exposure day (exday) is first day of exposure period
## Using the oxford.r code
## Start of exposure period
ex1 <- original$start1
## End of exposure period
ex2 <- original$end1
expo <- cbind(ex1, ex2)
expolev <- c(1, 0)

ncuts <- ncol(expo) + 2
nevents <- nrow(original)

#create an ordered list of individual events and 
#cut points for start, end and exposure groups
ind <- rep(1:nevents, times = ncuts)
cutp <- c(as.matrix(original$start), as.matrix(original$end), expo)
o <- order(ind, cutp)
ind = as.factor(ind[o])
cutp = cutp[o]

#calculate interval lengths, set to 0 if before start or after end
interval <- c(0, cutp[2:length(ind)]-cutp[1:length(ind)-1])
interval <- ifelse(cutp<=original$start[ind], 0, interval)
interval <- ifelse(cutp>original$end[ind], 0, interval)

#event = 1 if event occurred in interval, otherwise 0
event <- ifelse(original$eventday[ind]>cutp-interval, 1, 0)
event <- ifelse(original$eventday[ind]<=cutp, event, 0)

#exposure groups
exgr <- rep(0, nevents*ncuts)
for(i in 1:ncol(expo)){
  exgr <- ifelse(cutp > expo[,i][ind], expolev[i], exgr)
}
exgr <- as.factor(exgr)

#put all data in a data frame, take out data with 0 interval lengths
chopdat <- data.frame(indiv = ind[interval!=0], event = event[interval!=0], 
                      interval = interval[interval!=0], exgr = exgr[interval!=0], 
                      loginterval = log(interval[interval!=0]))

## Fit model using gnm 
original_gnm <- gnm(event ~ exgr + offset(loginterval), data = chopdat,            
            family = poisson(link = "log"),
            eliminate = indiv) # corresponds to strata(indiv)
summary(original_gnm)
AIC(original_gnm)
BIC(original_gnm)

## Fit model using survival
original_clogit <- clogit(event ~ exgr + strata(indiv) + offset(loginterval), data = chopdat)
summary(original_clogit)
AIC(original_clogit) # AIC is a lot better though
BIC(original_clogit)

## I refer to read-out to gnm better, so I use that from here 

## Alternatively, 
## This is based on the oxford.r file but in dplyr (you get the same results)
datLong <- plyr::ddply(.data = original,
                       .variables = "indiv",
                       .fun = function(df) {
                         
                         ## Cut points
                         cuts <- with(df, sort(c(start, end, start1, end1)))
                         
                         ## Split observation times
                         out <- data.frame(indiv = df$indiv,
                                           exday = df$exday,
                                           eventday = df$eventday,
                                           start = head(cuts, -1),
                                           end   = cuts[-1])
                         
                         ## Event status for each interval
                         ## 1 if event day is within the interval
                         out$event <- as.numeric(out$start <= df$eventday & df$eventday <= out$end)
                         
                         ## Exposure status for each interval
                         ## 1 if interval is within the window
                         out$exgr <- as.numeric(df$start1 <= out$start & out$end <= df$end1)
                         
                         ## Interval length
                         out$interval <- out$end - out$start
                         out$loginterval <- log(out$interval)
                         
                         out
                       })

datLong <- subset(datLong, start < 3789) # remove any beyond the observation period
datLong[datLong=="-Inf"]<-0 # change log0 to zero not -Inf

mod_original <- gnm(event ~ exgr + offset(loginterval),
            data = datLong,
            family = poisson(link = "log"),
            eliminate = factor(indiv)) 
summary(mod_original)



## For the sensitivity dataframes with multiple exposure period 
## This is still all based on the Oxford example files, the same as above 
sens_1 <- read_excel("Data/sccs_new.xlsx", sheet = "sens_1")
names(sens_1)[2]<-paste("start")
names(sens_1)[3]<-paste("end")
names(sens_1)[4]<-paste("start1")
names(sens_1)[5]<-paste("end1")
names(sens_1)[6]<-paste("eventday")
sens_1$exday <- sens_1$start1
sens_1 <- sens_1 %>% 
  mutate(indiv = group_indices(., province, eventday, start1)) # create an id number for each exposure period
# not convinced how I have assigned indiv for multi-expo is correct
# but its the only way I can get it to work 
sens_1 <- sens_1 %>% distinct()
datLong <- plyr::ddply(.data = sens_1,
                       .variables = "indiv",
                       .fun = function(df) {
                         
                         cuts <- with(df, sort(c(start, end, start1, end1)))
                         
                         out <- data.frame(indiv = df$indiv,
                                           exday = df$exday,
                                           eventday = df$eventday,
                                           start = head(cuts, -1),
                                           end   = cuts[-1])
                         
                         out$event <- as.numeric(out$start <= df$eventday & df$eventday <= out$end)
                         
                         out$exgr <- as.numeric(df$start1 <= out$start & out$end <= df$end1)
                         
                         out$interval <- out$end - out$start
                         out$loginterval <- log(out$interval)
                         
                         out
                       })
datLong <- subset(datLong, start < 3789) 
datLong[datLong=="-Inf"]<-0
mod_sens1 <- gnm(event ~ exgr + offset(loginterval),
                    data = datLong,
                    family = poisson(link = "log"),
                    eliminate = factor(indiv)) 
summary(mod_sens1)
## I repeated this for the other 3 sensitivity analysis 

## Alternatively, 
## I wanted to see if I could adapt the itp.r code for my exposure periods
## I needed to make a new df to do this though, without exp_start and exp_end
## I imported all conflicts and all outbreaks 
outbreaks <- read_excel("sccs_new.xlsx", sheet = "outbreaks")
conflict <- read_excel("sccs_new.xlsx", sheet = "conflict")
outbreaks <- outbreaks[c(2,3,11)] # I have kept it all in days for consistency (but also did it in weeks, get more significant values in days)
conflict <- conflict[c(2,10)]
names(outbreaks)[3]<-paste("eventday")
names(conflict)[2]<-paste("exday")
data1 <- merge(conflict, outbreaks, by = "admin1", all = TRUE)
data1$start <- 1
data1$end <- 3789

ex1 <- data1$exday - 1
ex2 <- data1$exday + 70 # sens1
ex3 <- data1$exday + 56 # sens2
ex4 <- data1$exday + 42 # sens3
ex5 <- data1$exday + 28 # sens4
expo <- cbind(ex1, ex2, ex3, ex4, ex5)
expolev <- c(1, 2, 3, 4, 0)

ncuts <- ncol(expo) + 2
nevents <- nrow(data1)
ind <- rep(1:nevents, times = ncuts)

cutp <- c(as.matrix(data1$start), as.matrix(data1$end), expo)
o <- order(ind, cutp)
ind = as.factor(ind[o])
cutp = cutp[o]

interval <- c(0, cutp[2:length(ind)]-cutp[1:length(ind)-1])
interval <- ifelse(cutp<=data1$start[ind], 0, interval)
interval <- ifelse(cutp>data1$end[ind], 0, interval)

event <- ifelse(data1$eventday[ind]>cutp-interval, 1, 0)
event <- ifelse(data1$eventday[ind]<=cutp, event, 0)

exgr <- rep(0, nevents*ncuts)
for(i in 1:ncol(expo)){
  exgr <- ifelse(cutp > expo[,i][ind], expolev[i], exgr)
}
exgr <- as.factor(exgr)

chopdat <- data.frame(indiv = ind[interval!=0], event = event[interval!=0], 
                      interval = interval[interval!=0], exgr = exgr[interval!=0], 
                      loginterval = log(interval[interval!=0]))

mod <- gnm(event ~ exgr + offset(loginterval), data = chopdat,            
                    family = poisson(link = "log"),
                    eliminate = indiv)
summary(mod)

## I don't think this is right, it only gives me a read out or one of the expo groups  
## Plus when you check chopdat, the exgr column only has 1 in it, and it should have 1,2,3 & 4
## I think this is where it goes wrong 
expo <- cbind(ex1, ex2, ex3, ex4, ex5)
expolev <- c(1, 2, 3, 4, 0)
# I have also tried 
expolev <- c(1, 2, 3, 4, 5)
expolev <- c(0, 1, 2, 3, 4)
# With similar odd results 

## Another method of doing this would be to try and adapt my dplyr/oxford method 
## Start of exposure period
data1$start1 <- data1$exday + 0
## End of exposure periods
data1$end1   <- data1$exday + 0 # now instead of the original being first/last report, its just day of report 
data1$end2   <- data1$exday + 70
data1$end3   <- data1$exday + 56
data1$end4   <- data1$exday + 42
data1$end5   <- data1$exday + 28 # I ran these one at once through the model
data1 <- data1 %>% 
  mutate(indiv = group_indices(., admin1, exday, eventday)) # new id for each exposure period for each outbreak
data1 <- data1 %>% distinct()
datLong <- plyr::ddply(.data = data1,
                       .variables = "indiv",
                       .fun = function(df) {
                         
                         cuts <- with(df, sort(c(start, end, start1, end1))) # change exp_end here
                         
                         out <- data.frame(indiv = df$indiv,
                                           exday = df$exday,
                                           eventday = df$eventday,
                                           start = head(cuts, -1),
                                           end   = cuts[-1])
                         
                         out$event <- as.numeric(out$start <= df$eventday & df$eventday <= out$end)
                         
                         out$exgr <- as.numeric(df$start1 <= out$start & out$end <= df$end1) # change exp_end here
                         
                         out$interval <- out$end - out$start
                         out$loginterval <- log(out$interval)
                         
                         out
                       })

datLong <- subset(datLong, start < 3789)
datLong[datLong=="-Inf"]<-0

gnm1 <- gnm(event ~ exgr + offset(loginterval),
            data = datLong,
            family = poisson(link = "log"),
            eliminate = factor(indiv)) 
summary(gnm1)
## This one may be the winner, as you get the same as we did the first time we ran it split by year and province 
## E.g., an optimum time after the conflict
## My model outputs are in the data file 
## I also ran it for weeks instead of days 
## How it was edited for weeks is shown below, but it is obviously very similar to the above
data2$start1 <- data2$exday + 0
data2$end1   <- data2$exday + 0
data2$end2   <- data2$exday + 10
data2$end3   <- data2$exday + 8
data2$end4   <- data2$exday + 6
data2$end5   <- data2$exday + 4 
data2 <- data2 %>% 
  mutate(indiv = group_indices(., admin1, exday, eventday)) 
data2 <- data2 %>% distinct()
datLong <- plyr::ddply(.data = data2,
                       .variables = "indiv",
                       .fun = function(df) {
                         
                         cuts <- with(df, sort(c(start, end, start1, end1))) 
                         
                         out <- data.frame(indiv = df$indiv,
                                           exday = df$exday,
                                           eventday = df$eventday,
                                           start = head(cuts, -1),
                                           end   = cuts[-1])
                         
                         out$event <- as.numeric(out$start <= df$eventday & df$eventday <= out$end)
                         
                         out$exgr <- as.numeric(df$start1 <= out$start & out$end <= df$end1)
                         
                         out$interval <- out$end - out$start
                         out$loginterval <- log(out$interval)
                         
                         out
                       })

datLong <- subset(datLong, start < 542)
datLong[datLong=="-Inf"]<-0

gnm1 <- gnm(event ~ exgr + offset(loginterval),
            data = datLong,
            family = poisson(link = "log"),
            eliminate = factor(indiv)) 
summary(gnm1)


