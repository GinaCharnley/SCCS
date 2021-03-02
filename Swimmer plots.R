# Swimmer Plots
# Swimmer plots help to tell a graphical story of your time and response data 
# They can show an intervention or a negative event 

# https://cran.r-project.org/web/packages/swimplot/vignettes/Introduction.to.swimplot.html
# https://cran.rstudio.com/web/packages/swimplot/swimplot.pdf

# there is a swimmer plot package, which id based on ggplot and so a lot of the complementary functions work 

library(swimplot)
library(ggplot2)
library(dplyr)
library(viridis)

# to seperate your bars your data needs to be presented slighly differently than when you run the SCCS
# you need to create groups for the fill aesthetic
# example:
swim_dat
outbreak_id       stability start end outbreak incidence province 
1  Tshopo          Conflict     1  10       24      1.55    Tshopo       
2  Tshopo          Peace       11  52       24      1.55    Tshopo       
3  Haut-Katanga    Conflict    47  48       45      0.15    Haut-Katanga     
4  Haut-Katanga    Peace       1   47       45      0.15    Haut-Katanga       
5  Haut-Katanga    Peace       48  52       45      0.15    Haut-Katanga       

# this creates your basic swim plot, with bars seperated by peace and conflict
swimmer_plot(df = swim_dat, id = 'outbreak_id', end = 'end', name_fill = 'stability',  
             col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') 

# this will add points
swimmer_plot(df = swim_dat, id = 'outbreak_id', end = 'end', name_fill = 'stability',  
             col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = swim_dat, id = 'outbreak_id', time = 'outbreak', 
                 size = 2.5, fill = 'white', shape = 'triangle') 

# this changes your point colour depending on a second variable, in this case incidence rate
swimmer_plot(df = swim_dat, id = 'outbreak_id', end = 'end', name_fill = 'stability',  
             col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = swim_dat, id = 'outbreak_id', time = 'outbreak', 
                 size = 2.5, fill = 'white', shape = 'triangle', name_col = 'incidence') 

# you can then change your labels if you want 
swimmer_plot(df = swim_dat, id = 'outbreak_id', end = 'end', name_fill = 'stability',  
             col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = swim_dat, id = 'outbreak_id', time = 'outbreak', 
                 size = 2.5, fill = 'white', shape = 'triangle', name_col = 'incidence') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability", color = "Incidence Rate") 

# you can also stratify your plots 
swimmer_plot(df = swim_dat, id = 'outbreak_id', end = 'end', name_fill = 'stability',
             col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id', stratify= 'province')

# you may want to change the colours 
swimmer_plot(df = swim_dat, id = 'outbreak_id', end = 'end', name_fill = 'stability',  
             col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = swim_dat, id = 'outbreak_id', time = 'outbreak', 
                 size = 2.5, fill = 'white', shape = 'triangle', name_col = 'incidence') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability", color = "Incidence Rate") +
  scale_fill_manual(values = c("#FA7876", "#EA4F88")) + scale_color_viridis_c(option = "magma")

# I keep getting this error:
# Error in aggregate.data.frame(df[, end], by = list(df[, id]), max) : 
#  arguments must have same length

# I want to stratify my plots my province anyway, so lets start with Sud Kivu 
# This code fixes the bug using the aggregate()
# I am also making some conflict plots, to show event numbers and fatalities 

unique(data_main$province) 
[1] "Haut-Katanga"   "Sud-Kivu"       "Tanganyika"    
[4] "Tsphopo"        "Equateur"       "Ituri"         
[7] "Kinshasa"       "Maï-Ndombe"     "Nord-Kivu"     
[10] "Kongo-Central"  "Haut-Lomani"    "Maniema"       
[13] "Tshopo"         "Mongala"        "Nord-Ubangi"   
[16] "Bas-Uele"       "Kasaï"          "Kwilu"         
[19] "Kasaï-Oriental" "Lualaba" 
data_sud_kivu <- subset(data_main, province == "Sud-Kivu")
data_sud_kivu$end[data_sud_kivu$end == "51"] <- "52"
data_sud_kivu$end[data_sud_kivu$end == "53"] <- "52"
data_s_kivu <- aggregate(data_sud_kivu,by = list(outbreak_id = data_sud_kivu$outbreak_id, 
                                                 stability = data_sud_kivu$stability, 
                                                 outbreak = data_sud_kivu$outbreak,
                                                 end = data_sud_kivu$end),
                         FUN = "mean", na.rm=TRUE) 
data_s_kivu <- data_s_kivu[c(1,2,3,4)]
data_s_kivu$end <- as.numeric(data_s_kivu$end)
s_kivu_incidence <- data_sud_kivu$incidence
data_s_kivu <- cbind(data_s_kivu, s_kivu_incidence)
names(data_s_kivu)[5]<-paste("Incidence")
swimmer_plot(df = data_s_kivu, id = 'outbreak_id', end = 'end', 
             name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = data_s_kivu, id = 'outbreak_id', time = 'outbreak',
                 size = 1.5, fill = 'white', shape = 'triangle', name_col = 'Incidence') + 
  labs(y = "Epi Week", x = "Outbreak ID", fill = "Stability", title = "Sud-Kivu", color = "Incidence") + 
  scale_fill_manual(values = c("#238A8DFF", "#3CBB75FF")) + scale_color_viridis_c(option = "magma")
s_kivu_conflict <- data_sud_kivu[c(2,8,9)]
s_kivu_conflict <- s_kivu_conflict %>% distinct()
s_kivu_conflict$year <- as.character(s_kivu_conflict$year)
skivu_conflict_plot_1 <- ggplot(s_kivu_conflict) + 
  geom_bar(aes(x = year, y = no_of_events), stat = "identity", color = "#ED5983", fill = "#ED5983") + 
  theme_bw() + labs(y = "No. of Events", x = "Year")
skivu_conflict_plot_2 <- ggplot(s_kivu_conflict) + 
  geom_bar(aes(x = year, y = fatalities_total), stat = "identity", color = "#FA7876", fill = "#FA7876") + 
  theme_bw() + labs(y = "Total Annual Fatalities", x = "Year")
ggarrange(skivu_conflict_plot_1, skivu_conflict_plot_2, ncol = 1, nrow = 2)

# I also got this error:
# Warning messages:
#  1: In mean.default(X[[i]], ...) :
#  argument is not numeric or logical: returning NA
# So I changed the aggregate() FUN option, which can be  
# FUN=c("count","sum","mean","median","sd","se","min","max")

data_bas_uele <- subset(data_main, province == "Bas-Uele")
data_bas_uele_2 <- aggregate(data_bas_uele,by = list(outbreak_id = data_bas_uele$outbreak_id, 
                                                     stability = data_bas_uele$stability, 
                                                     outbreak = data_bas_uele$outbreak,
                                                     end = data_bas_uele$end),
                             FUN = "median", na.rm=TRUE) 
# I think it occurs when I don't have enough outbreaks to calculate the mean 

# Some useful pages on personalising your colour palette 
# https://ggplot2.tidyverse.org/reference/scale_manual.html
# https://bookdown.org/rdpeng/exdata/plotting-and-color-in-r.html


