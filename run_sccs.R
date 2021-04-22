
run_sccs <- function(sheet_name){
  dat <- read_excel("Data/sccs_new.xlsx", sheet = sheet_name)
  
  names(dat) <- c("province", "start", "end", "start1", "end1", "eventday")
  
  if(sheet_name=="original"){
    indiv <- seq(1,298,1)
    dat <- cbind(dat, indiv) # create an id number for each outbreak
  } else {
    dat <- dat %>% 
      mutate(indiv = group_indices(., province, eventday, start1)) 
  }
  
  dat$exday <- dat$start1 # exposure day (exday) is first day of exposure period
  
  dat <- dat %>% distinct()
  
  datLong <- plyr::ddply(.data = dat,
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
  
  dat_gnm <- gnm(event ~ exgr + offset(loginterval),
                      data = datLong,
                      family = poisson(link = "log"),
                      eliminate = factor(indiv)) 
  
  
  ## Fit model using survival
  dat_clogit <- clogit(event ~ exgr + strata(indiv) + offset(loginterval), data = datLong)
  
  return(list(gnm_fit = dat_gnm, clogit_fit = dat_clogit))
}

orig_fits <- run_sccs("original")
lapply(X = c(1:2), FUN = function(x)AIC(orig_fits[[x]]))

sens1_fits <- run_sccs("sens_1")
lapply(X = c(1:2), FUN = function(x)AIC(sens1_fits[[x]]))

sens2_fits <- run_sccs("sens_2")
lapply(X = c(1:2), FUN = function(x)AIC(sens2_fits[[x]]))
