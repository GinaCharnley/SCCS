# To make figure 4, which used the orignal dataset 
library(swimplot)
library(ggplot2)
library(viridis)
sccs_2 <- aggregate(sccs,by = list(outbreak_id = sccs$outbreak_id, stability = sccs$stability,
                                   outbreak = sccs$outbreak,end = sccs$end),FUN = mean) 
sccs_2 <- sccs_2[-c(5,6,11)]
sccs_3 <- merge(sccs, sccs_2, all=TRUE)
sccs_3[135, 3] <- 14
sccs_3[136, 3] <- 14 # two were missing in the aggregated data, so I merged the dataframes to identify them and inputted the data manually

sccs_3$province[sccs_3$province == "Tsphopo"] <- "Tshopo" # I had spelt a province incorrectly 
unique(sccs_3$province)
[1] "Bas-Uele"       "Equateur"       "Haut-Katanga"   "Haut-Lomani"   
[5] "Ituri"          "Kasaï"          "Kasaï-Oriental" "Kinshasa"      
[9] "Kongo-Central"  "Kwilu"          "Lualaba"        "Maï-Ndombe"    
[13] "Maniema"        "Mongala"        "Nord-Kivu"      "Nord-Ubangi"   
[17] "Sud-Kivu"       "Tanganyika"     "Tshopo"   # find out how many provinces I have to plot

# Create seperate df and plots for each province 
bas_uele <- subset(sccs_3, province == "Bas-Uele")
bas_uele_plot <- swimmer_plot(df = bas_uele, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = bas_uele, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))
equateur <- subset(sccs_3, province == "Equateur")
equateur_plot <- swimmer_plot(df = equateur, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = equateur, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))
haut_katanga <- subset(sccs_3, province == "Haut-Katanga")
haut_katanga_plot <- swimmer_plot(df = haut_katanga, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = haut_katanga, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))                                                                                          
haut_lomani <- subset(sccs_3, province == "Haut-Lomani")
haut_katanga_plot <- swimmer_plot(df = haut_katanga, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = haut_katanga, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
ituri <- subset(sccs_3, province == "Ituri")
ituri_plot <- swimmer_plot(df = ituri, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = ituri, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
kasai <- subset(sccs_3, province == "Kasaï")
kasai_plot <- swimmer_plot(df = kasai, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = kasai, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88")) 
kasai_oriental <- subset(sccs_3, province == "Kasaï-Oriental")
kasai_oriental_plot <- swimmer_plot(df = kasai_oriental, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = kasai_oriental, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
kinshasa <- subset(sccs_3, province == "Kinshasa")
kinshasa_plot <- swimmer_plot(df = kinshasa, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = kinshasa, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
kongo_central <- subset(sccs_3, province == "Kongo-Central")
kongo_central_plot <- swimmer_plot(df = kongo_central, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = kongo_central, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
kwilu <- subset(sccs_3, province == "Kwilu")
kwilu_plot <- swimmer_plot(df = kwilu, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = kwilu, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
lualaba <- subset(sccs_3, province == "Lualaba")
lualaba_plot <- swimmer_plot(df = lualaba, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = lualaba, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
mai_ndombe <- subset(sccs_3, province == "Maï-Ndombe")
mai_ndombe_plot <- swimmer_plot(df = mai_ndombe, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = mai_ndombe, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
maniema <- subset(sccs_3, province == "Maniema")
maniema_plot <- swimmer_plot(df = maniema, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = maniema, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
mongala <- subset(sccs_3, province == "Mongala")
mongala_plot <- swimmer_plot(df = mongala, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = mongala, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
nord_kivu <- subset(sccs_3, province == "Nord-Kivu")
nord_kivu_plot <- swimmer_plot(df = nord_kivu, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = nord_kivu, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
nord_ubangi <- subset(sccs_3, province == "Nord-Ubangi")
nord_ubangi_plot <- swimmer_plot(df = nord_ubangi, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = nord_ubangi, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
sud_kivu <- subset(sccs_3, province == "Sud-Kivu")
sud_kivu_plot <- swimmer_plot(df = sud_kivu, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = sud_kivu, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
tanganyika <- subset(sccs_3, province == "Tanganyika")
tanganyika_plot <- swimmer_plot(df = tanganyika, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = tanganyika, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      
tshopo <- subset(sccs_3, province == "Tshopo")
tshopo_plot <- swimmer_plot(df = tshopo, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id') + 
  swimmer_points(df_points = tshopo, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88"))      

smaller_plots <- plot_grid(bas_uele_plot + theme(legend.position="none",axis.title.y=element_blank()), kasai_plot + theme(legend.position="none",axis.title.y=element_blank()), kwilu_plot + theme(legend.position="none",axis.title.y=element_blank()), lualaba_plot + theme(legend.position="none",axis.title.y=element_blank()), nord_ubangi_plot + theme(legend.position="none",axis.title.y=element_blank()), 
                           labels = c('Bas-Uele', 'Kasaï', 'Kwilu', 'Lualaba', 'Nord-Ubangi'), label_size = 10, hjust = -0.2)
legend <- get_legend(nord_kivu_plot + theme(legend.box.margin = margin(0, 0, 0, 12)))
smaller_plots <- plot_grid(smaller_plots, legend, rel_widths = c(3, .4))

kivu <- merge(nord_kivu, sud_kivu, all = TRUE)
plots_1 <- swimmer_plot(df = kivu, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id', stratify= 'province') + 
  swimmer_points(df_points = kivu, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88")) 

katanga_kinshasa <- merge(haut_katanga, kinshasa, all = TRUE)
katanga_kinshasa$end[katanga_kinshasa$end == 51] <- 52
plots_2 <- swimmer_plot(df = katanga_kinshasa, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id', stratify= 'province') + 
  swimmer_points(df_points = katanga_kinshasa, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88")) 

smaller_data <- merge(bas_uele, kasai, all = TRUE)
smaller_data <- merge(smaller_data, kwilu, all = TRUE)
smaller_data <- merge(smaller_data,lualaba, all = TRUE)
smaller_data <- merge(smaller_data, nord_ubangi, all = TRUE)
smaller_data <- merge(smaller_data, mongala, all = TRUE)
plots_3 <- swimmer_plot(df = smaller_data, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id', stratify= 'province') + 
  swimmer_points(df_points = smaller_data, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88")) 

sccs_4 <- sccs_3[sccs_3$province != "Nord-Kivu", ]
sccs_4 <- sccs_4[sccs_4$province != "Sud-Kivu", ]
sccs_4 <- sccs_4[sccs_4$province != "Haut-Katanga", ]
sccs_4 <- sccs_4[sccs_4$province != "Kinshasa", ]
sccs_4 <- sccs_4[sccs_4$province != "Bas-Uele", ]
sccs_4 <- sccs_4[sccs_4$province != "Kasaï", ]
sccs_4 <- sccs_4[sccs_4$province != "Kwilu", ]
sccs_4 <- sccs_4[sccs_4$province != "Lualaba", ]
sccs_4 <- sccs_4[sccs_4$province != "Nord-Ubangi", ]
sccs_4 <- sccs_4[sccs_4$province != "Mongala", ]
sccs_4$end[sccs_4$end == 50] <- 52
sccs_4$end[sccs_4$end == 51] <- 52
plots_4 <- swimmer_plot(df = sccs_4, id = 'outbreak_id', end = 'end', name_fill = 'stability', col = 'black', alpha = 0.75, width = 0.8, id_order = 'outbreak_id', stratify= 'province') + 
  swimmer_points(df_points = sccs_4, id = 'outbreak_id', time = 'outbreak', size = 2, fill = 'white', shape = 'triangle') + 
  labs(y = "Time (Epi Weeks)", x = "Outbreak ID", fill = "Stability") + scale_fill_manual(values = c("#FA7876", "#EA4F88")) 

# https://wilkelab.org/cowplot/articles/shared_legends.html
# https://wilkelab.org/cowplot/articles/plot_grid.html
  


