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

# so to remake for my full timeseries I had to change the conflict data, as I now needed to for 2010 to 2020 not just the years with outbreaks 
library(data.table)
bas_uele <- subset(Conflict, admin1 == "Bas-Uele" & MMWRyear > 2009)
equateur <- subset(Conflict, admin1 == "Equateur"& MMWRyear > 2009)
haut_katanga <- subset(Conflict, admin1 == "Haut-Katanga"& MMWRyear > 2009)
haut_lomani <- subset(Conflict, admin1 == "Haut-Lomami"& MMWRyear > 2009)
ituri<- subset(Conflict, admin1 == "Ituri"& MMWRyear > 2009)
kasai<- subset(Conflict, admin1 == "Kasaï"& MMWRyear > 2009)
kasai_oriental <- subset(Conflict, admin1 == "Kasaï-Oriental"& MMWRyear > 2009)
kinshasa <- subset(Conflict, admin1 == "Kinshasa"& MMWRyear > 2009)
kongo_central <- subset(Conflict, admin1 == "Kongo-Central"& MMWRyear > 2009)
kwilu <- subset(Conflict, admin1 == "Kwilu"& MMWRyear > 2009)
lualaba <- subset(Conflict, admin1 == "Lualaba"& MMWRyear > 2009)
mai_ndombe<- subset(Conflict, admin1 == "Maï-Ndombe"& MMWRyear > 2009)
maniema <- subset(Conflict, admin1 == "Maniema"& MMWRyear > 2009)
mongala <- subset(Conflict, admin1 == "Mongala"& MMWRyear > 2009)
nord_kivu <- subset(Conflict, admin1 == "Nord-Kivu"& MMWRyear > 2009)
nord_ubangi <- subset(Conflict, admin1 == "Nord-Ubangi"& MMWRyear > 2009)
sud_kivu <- subset(Conflict, admin1 == "Sud-Kivu"& MMWRyear > 2009)
tanganyika <- subset(Conflict, admin1 == "Tanganyika"& MMWRyear > 2009)
tshopo <- subset(Conflict, admin1 == "Tshopo"& MMWRyear > 2009)

conflict2 <- rbind(bas_uele, equateur)
conflict2 <- rbind(conflict2, haut_katanga)
conflict2 <- rbind(conflict2, haut_lomani)
conflict2 <- rbind(conflict2, ituri)
conflict2 <- rbind(conflict2, kasai)
conflict2 <- rbind(conflict2, kasai_oriental)
conflict2 <- rbind(conflict2, kinshasa)
conflict2 <- rbind(conflict2, kongo_central)
conflict2 <- rbind(conflict2, kwilu)
conflict2 <- rbind(conflict2, lualaba)
conflict2 <- rbind(conflict2, mai_ndombe)
conflict2 <- rbind(conflict2, maniema)
conflict2 <- rbind(conflict2, mongala)
conflict2 <- rbind(conflict2, nord_kivu)
conflict2 <- rbind(conflict2, nord_ubangi)
conflict2 <- rbind(conflict2, sud_kivu)
conflict2 <- rbind(conflict2, tanganyika)
conflict2 <- rbind(conflict2, tshopo)

conflict2 <- conflict2 %>% distinct()
conflict_events <- conflict2 %>% group_by(admin1) %>% tally()
conflict3 <- merge(conflict2, conflict_events, by = "admin1", all = TRUE)
names(conflict3)[5]<-paste("events_total")
write.csv(conflict3, "conflict3.csv")

# I have had a lot of problems making swimmer plots using the swimmer_plot() function
# So I am making my own on the basis of a lollipop plot 
library(scales) # also discovered this package, which can generate you some codes for specific colours 
show_col(viridis_pal(option = "magma")(20))
# It isn't happy with 2010_1, so I have converted them all back to numbers, using continuous epiweeks for 2010-2020 
# So I have conflicts df for each sensitivity analysis and an outbreak df 
# I can then merge this with my epiweeks data I have created, to match the continuous numbers 
names(epi_weeks)[1]<-paste("start")
sens1_swim <- merge(sens1_swim, epi_weeks, by = "start")
names(sens1_swim)[5]<-paste("start_2")
names(epi_weeks)[1]<-paste("end")
sens1_swim <- merge(sens1_swim, epi_weeks, by = "end")
names(sens1_swim)[6]<-paste("end_2")
# then merge the outbreak and the conflict data 
sens1_swim <- merge(sens1_swim, outbreaks, by = "province", all = TRUE)
sens1_swim <- na.omit(sens1_swim)
# then replot 
ggplot(swim_dat_new) + geom_segment(aes(x=province, xend=province, y=start, yend=end, color=stability), size = 4) + 
  coord_flip() + labs(x = "Province", y = "Epiweeks (2010-2020)", color = "Stability") + theme_bw() + 
  scale_colour_manual(values = c("#FD9A6AFF", "#AB337CFF")) + 
  geom_point(aes(x=province, y=outbreak), size=2, shape = "triangle")
# Because now my epiweeks are a little confusing to interpret on the figure, I have created some of my own labels 
pos <- as.numeric(c(1,50,100,150,200,250,300,350,400,450,500,540))
labels <- as.character(c("2010_1", "2010_50","2011_48","2012_46","2013_44","2014_42", 
                         "2015_40", "2016_38", "2017_36", "2018_34", "2019_32", "2020_20"))
ggplot(swim_dat_new) + geom_segment(aes(x=province, xend=province, y=start, yend=end, color=stability), size = 4) + 
  coord_flip() + labs(x = "Province", y = "Epiweeks (2010-2020)", color = "Stability") + theme_bw() + 
  scale_colour_manual(values = c("#FD9A6AFF", "#AB337CFF")) + 
  geom_point(aes(x=province, y=outbreak), size=2, shape = "triangle") + 
  scale_y_discrete(limits = pos, labels = labels) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

# need to make sure outbreaks is numeric, otherwise it wont plot 
outbreak <- as.numeric(outbreaks$outbreak)
outbreaks$outbreak <- NULL
outbreaks <- cbind(outbreaks, outbreak)
rm(outbreak)

