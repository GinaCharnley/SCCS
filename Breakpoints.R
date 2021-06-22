# Pairwise Regression for Breakpoints

library(segmented)
library(dplyr)

cod_conflict <- read_excel("Library/Mobile Documents/com~apple~Preview/Documents/PhD/Chapter 3 - Conflict:Cholera, DRC&NGA/Data/Conflict/Conflict.xlsx", sheet = "COD")

cod_conflict <- cod_conflict[c(1,4)]
date1 <- cod_conflict$Date
df_date <- data.frame(date = date1, year = as.numeric(format(date1, format = "%Y")),
                      month = as.numeric(format(date1, format = "%m")),
                      day = as.numeric(format(date1, format = "%d")))
cod_conflict <- cbind(cod_conflict, df_date)
cod_conflict <- cod_conflict %>% group_by(month, year) %>% tally()
cod_conflict$month_cont <- seq(1,280, 1)
cod_conflict$log.n <- log10(cod_conflict$n)
lin.mod <- lm(log.n~month_cont, data = cod_conflict)
segmented.mod <- segmented(lin.mod, seg.Z = ~month_cont)
summary(segmented.mod)

***Regression Model with Segmented Relationship(s)***
  
  Call: 
  segmented.lm(obj = lin.mod, seg.Z = ~month_cont)

Estimated Break-Point(s):
  Est. St.Err
psi1.month_cont 120.997 14.149

Meaningful coefficients of the linear terms:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   1.2447737  0.0516426  24.104   <2e-16 ***
  month_cont    0.0009123  0.0007408   1.232    0.219    
U1.month_cont 0.0048059  0.0008833   5.441       NA    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2811 on 276 degrees of freedom
Multiple R-Squared: 0.5716,  Adjusted R-squared: 0.567 

Convergence attained in 3 iter. (rel. change 9.1903e-06)






