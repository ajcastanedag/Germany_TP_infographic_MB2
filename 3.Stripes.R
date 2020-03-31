#############################################################################################################
##################################  Stripe Precipitation Graph  #############################################
#############################################################################################################
theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
  )

col_strip <- brewer.pal(11, "BrBG")

Main_P_DF$MeanMod <- rescale(Main_P_DF$Mean, to=c(0.5,1.5))

model_gif <- loess(Main_P_DF$MeanMod ~ seq(1:length(Main_P_DF$MeanMod)), data = Main_P_DF, span = 0.9)

Main_P_DF$Trend <- model_gif$fitted

rm(model_gif)

P_Stripes_Mod <- ggplot(Main_P_DF, aes(x = Main_P_DF$Year, y = 1, fill = Main_P_DF$Mean)) +
  geom_tile()+
  scale_x_continuous(breaks=seq(1881,2019,6), c(as.numeric(substr(Years[1],1,4)),as.numeric(substr(Years[length(Years)],1,4))))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = col_strip)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = paste("Mean Germany Air Temperature",Years[1],"-", Years[length(Years)]),
       caption = "Data: ftp://opendata.dwd.de/") +
  theme_strip +
  geom_line(aes(x=Main_P_DF$Year,y=Main_P_DF$MeanMod)) +
  geom_smooth(aes(x=Main_P_DF$Year,y=Main_P_DF$MeanMod),color = "black", method = "loess")

P_Stripes_Mod

setwd(Circles_Fo)

ggsave("P_Stripes_Mod.png",P_Stripes_Mod,width=80, height=60, limitsize = FALSE, bg = "transparent")






