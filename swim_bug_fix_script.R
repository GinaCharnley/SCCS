unique(data_main$province) 
[1] "Haut-Katanga"   "Sud-Kivu"       "Tanganyika"    
[4] "Tsphopo"        "Equateur"       "Ituri"         
[7] "Kinshasa"       "Maï-Ndombe"     "Nord-Kivu"     
[10] "Kongo-Central"  "Haut-Lomani"    "Maniema"       
[13] "Tshopo"         "Mongala"        "Nord-Ubangi"   
[16] "Bas-Uele"       "Kasaï"          "Kwilu"         
[19] "Kasaï-Oriental" "Lualaba"   
data_ituri <- subset(data_main, province == "Ituri")
data_ituri$end[data_ituri$end == "51"] <- "52"
data_ituri$end[data_ituri$end == "53"] <- "52"
data_ituri_2 <- aggregate(data_ituri,by = list(outbreak_id = data_ituri$outbreak_id, 
                                                 stability = data_ituri$stability, 
                                                 outbreak = data_ituri$outbreak,
                                                 end = data_ituri$end),
                         FUN = mean) 
data_ituri_2 <- data_ituri_2[c(1,2,3,4)]
data_ituri_2$end <- as.numeric(data_ituri_2$end)
ituri_incidence <- data_ituri$incidence
data_ituri_2 <- cbind(data_ituri_2, ituri_incidence)
names(data_ituri_2)[5]<-paste("Incidence")
swimmer_plot(df = data_ituri_2, id = 'outbreak_id', end = 'end', 
             name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = data_ituri_2, id = 'outbreak_id', time = 'outbreak',
                 size = 1.5, fill = 'white', shape = 'triangle', name_col = 'Incidence') + 
  labs(y = "Epi Week", x = "Outbreak ID", fill = "Stability", title = "Ituri", color = "Incidence") + 
  scale_fill_manual(values = c("#238A8DFF", "#3CBB75FF")) + scale_color_viridis_c(option = "magma")
ituri_conflict <- data_ituri[c(2,8,9)]
ituri_conflict <- ituri_conflict %>% distinct()
ituri_conflict$year <- as.character(ituri_conflict$year)
ituri_conflict_plot_1 <- ggplot(ituri_conflict) + 
  geom_bar(aes(x = year, y = no_of_events), stat = "identity", color = "#ED5983", fill = "#ED5983") + 
  theme_bw() + labs(y = "No. of Events", x = "Year")
ituri_conflict_plot_2 <- ggplot(ituri_conflict) + 
  geom_bar(aes(x = year, y = fatalities_total), stat = "identity", color = "#FA7876", fill = "#FA7876") + 
  theme_bw() + labs(y = "Total Annual Fatalities", x = "Year")
ggarrange(ituri_conflict_plot_1, ituri_conflict_plot_2, ncol = 1, nrow = 2)
