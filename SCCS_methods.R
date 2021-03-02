# Self-Controlled Case Series - SCCS Methods 
library(lubridate)
library(MMWRweek)
library(zoo)
library(dplyr)

# data
cod_cholera <- read_excel("Data/data_2010.xlsx", sheet = "cod_cholera_2010", 
                          col_types = c("skip", "text", "text", "numeric"))
cod_conflict <- read_excel("Data/data_2010.xlsx", sheet = "cod_conflict_2010", 
                           col_types = c("skip","text", "text", "text", "numeric"))

# COD
# reformat my date data 
# Cholera
date1 <- as.Date(cod_cholera$Date)
df_date <- data.frame(date = date1, year = as.numeric(format(date1, format = "%Y")),
                      month = as.numeric(format(date1, format = "%m")),
                      day = as.numeric(format(date1, format = "%d")))
epi_weeks <- cbind(date1, MMWRweek(date1))
names(epi_weeks)[1]<-paste("date")
cod_cholera_date <- cbind(epi_weeks, df_date)
cod_cholera <- cbind(cod_cholera_date, cod_cholera)
cod_cholera <- cod_cholera[-c(5,6,9)]
# Conflict 
date2 <- as.Date(cod_conflict$Date)
df_date <- data.frame(date = date2, year = as.numeric(format(date2, format = "%Y")),
                      month = as.numeric(format(date2, format = "%m")),
                      day = as.numeric(format(date2, format = "%d")))
epi_weeks <- cbind(date2, MMWRweek(date2))
names(epi_weeks)[1]<-paste("date")
cod_conflict_date <- cbind(epi_weeks, df_date)
cod_conflict <- cbind(cod_conflict_date, cod_conflict)
cod_conflict <- cod_conflict[-c(5,6,9)]

# subset to just 2010 
# Cholera
cod_cholera_2010 <- subset(cod_cholera, MMWRyear == "2010")
cod_cholera_2010 <- cod_cholera_2010[-c(11,2),] # I had two columns I didn't want 
number <- c(0.61954170, 0.46465628, 0.15178772)
number_mean <- mean(number)
cod_cholera_2010[4, 8] = 0.41199523
cod_cholera_2010[3, 8] = 0.41199523
cod_cholera_2010[5, 8] = 0.41199523
cod_cholera_2010 <- cod_cholera_2010 %>% distinct() # I had three columns with the same outbreak, but a different incidence rate 
# Conflict 
cod_conflict_2010 <- subset(cod_conflict, MMWRyear == "2010")
# only need the states where I have outbreaks
cod_conflict_tshopo <- subset(cod_conflict_2010, admin1 == "Tshopo")
cod_conflict_haut_katanga <- subset(cod_conflict_2010, admin1 == "Haut-Katanga")
cod_conflict_sud_kivu <- subset(cod_conflict_2010, admin1 == "Sud-Kivu")
cod_conflict_tanganyika <- subset(cod_conflict_2010, admin1 == "Tanganyika")
cod_conflict_2010 <- rbind(cod_conflict_haut_katanga, cod_conflict_tshopo, cod_conflict_sud_kivu, cod_conflict_tanganyika)

# I wrote two csv files from this and produced the following data
SCCS_data
province observation_start observation_end outbreak exposure_start exposure_end
1 Tshopo              1              52       24              1           10
2 Haut-Katanga        1              52       45             47           48
3 Tanganyika_1        1              52       45             52           52
4 Tanganyika_2        1              52       40             52           52
5 Sud-Kivu_1          1              52       31              5           52
6 Sud-Kivu_2          1              52       36              5           52
7 Sud-Kivu_3          1              52       37              5           52
# each number is epiweek and the exposure are start and end weeks of conflict 

# within the SCCS package there is a function to create sccs data for you 
library(SCCS)
simulatesccsdata(nindivs, astart, aend, adrug, aedrug, expogrp=c(0), eexpo,
                 washout=NULL, ewashout=NULL, agegrp=NULL, eage=NULL)
simdata <- simulatesccsdata(nindivs=110, astart=366, aend=730,
                            adrug=arisk, aedrug=arisk+20, eexpo=2.5)
# this should give you a dataframe with columns: 
# "indiv" = individual identifier 
# "astart" = age on the day observation period starts 
# "adrug" = age on the day exposure starts 
# "aedrug" = age at the end of exposure related risk period 
# "aend" = age at the end of observation period 
# "aevent" = age on the day of outcome event.
# https://rdrr.io/cran/SCCS/man/simulatesccsdata.html
# or 
data <- formatdata(indiv = nindiv, astart = observation_start, aend = observation_end, 
                   aevent = outbreak, adrug = exposure_start, aedrug = exposure_end, 
                   data = SCCS_data)

# each event has to be numberic, so I assigned each province a number 
nindiv <- as.numeric(seq(1,7,1))
SCCS_data <- cbind(SCCS_data, nindiv)
# this is for an event dependant exposure SCCS 
# this assumes that no exposure is possible following a unique event
# it requires that exposure is of a fixed finite length 
eventdepenexp(indiv=nindiv, astart=observation_start, aend=observation_end,
                                aevent=outbreak, adrug=exposure_start,aedrug=exposure_end, 
                                dataformat="multi", data=SCCS_data)
# the dataformat function accepts:
# "multi" refering to a data assembled with one row representing one event 
# "stack" refering to a data frame where repeated exposures of the same type are stacked in one column
# so with my original data will all the different conflict periods 
nindiv <- as.numeric(list(1,1,2,3,4,5,5,5,5,5,6,6,6,6,6,7,7,7,7,7))
sccs_data_2010_cod <- cbind(sccs_data_2010_cod, nindiv) # or
sccs_data_2010_cod <- sccs_data_2010_cod %>% mutate(nindiv = group_indices(., province))
eventdepenexp(indiv=nindiv, astart=observation_start, aend=observation_end,aevent=outbreak, 
              adrug=exposure_start,aedrug=exposure_end, 
              dataformat="stack", data=sccs_data_2010_cod)

# the other method is event dependant observation periods 
# an assumption of sccs is that observation periods for each individual is independant of event times
# if the event increases the incidence of death, this assumption is void 
# to fit these you need an equation, event~exposure
# you also need a vector of indicators for whether an observational period were censored 
# 1 = observation period ended early, 0 = fully observed
cen <- as.numeric(c("0","0","0","0","0","0","0")) #or
cen <- as.numeric(rep(c(0),times=7))
data1 <- cbind(data1, cen)
eventdepenobs(outbreak~exposure_start, indiv=nindiv, astart=observation_start, aend=observation_end, 
              aevent=outbreak, adrug=exposure_start, aedrug=exposure_end, 
              agegrp=NULL, data=data1, censor = cen)

# https://cran.r-project.org/web/packages/SCCS/SCCS.pdf

# using the formatdata() function data above and different packages
library(survival)
names(data)[8]<-paste("exgr")
loginterval <- log(data$interval)
data <- cbind(data, loginterval)
mod <- clogit(event ~ exgr + strata(indiv) + offset(loginterval), data = data)
summary(mod)
# the below link has some example data and r scripts
# http://sccs-studies.info/r.html

library(gnm)
gnm1 <- gnm(event ~ exgr + offset(loginterval), data = data, family = poisson(link = "log"), eliminate = as.factor(indiv))
summary(gnm1)
# https://rpubs.com/kaz_yos/sccs1

# Fitting my data to the model 
# Re-run for COD Sensitivity analysis 1 
data2 <- sccs_data_after_2010 %>% mutate(nindiv = group_indices(., province))
data3 <- data2[c(1,3,4,5,6,7,11)]
data3 <- na.omit(data3)
eventdepenexp(indiv=nindiv, astart=observation_start, 
              aend=observation_end,aevent=outbreak, 
              adrug=exposure_start,aedrug=exposure_end,dataformat="stack", data=data3)
# Data is stack because now more than one row represents a single outbreak 
# I get the following warning message: 
# Warning message:
#  In eventdepenexp(indiv = nindiv, astart = observation_start, aend = observation_end,  :
#                    Multiple events per case detected: analysis restricted to first events
# This would create issues, as only the first exposure event is being analysed 
# So I used event dependant observations instead
cen <- as.numeric(rep(c(0), times = 400))
data3 <- cbind(data3, cen)
eventdepenobs(outbreak~exposure_start, indiv=nindiv, 
              astart=observation_start, aend=observation_end,aevent=outbreak, 
              adrug=exposure_start, aedrug=exposure_end, 
              agegrp=NULL, data=data3, censor = cen, dataformat="stack")

# original; p of 0.93
# sens 1; p of 0.0538 
# sens 2; p of 0.0357
# sens 3; p of 0.00342
# sens 4; p of 0.0888

# summary	
# exposure related relative incidence estimates along with their 95% confidence intervals, age related relative incidence estimates and estimates of interactions with covariates if there are any.
# modelfit	
# model fit of the 4 different weight functions and their AIC values.




