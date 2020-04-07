#############################################################################################################
######################################## LIBRARIES ##########################################################
#############################################################################################################
library(stringr)
library(raster)
library(RColorBrewer)
library(ggplot2)
library(scales)
#############################################################################################################
##################################### FOLDER DECLARATION ####################################################
#############################################################################################################
### Choose Main Folder
Main_Fo <- choose.dir()
setwd(Main_Fo)

### Declare Folders
Script_Fo <- paste0(Main_Fo,"\\1.Script")
Data_Fo <- paste0(Main_Fo,"\\2.Data")
Graphs_Fo <- paste0(Main_Fo,"\\5.Graphs")

### Declare SubFolders
Temper_Fo_Or <- paste0(Data_Fo,"\\1.Temperature")
Precip_Fo_Or <-  paste0(Data_Fo,"\\2.Precipitation")

Graphs_Fo <- paste0(Main_Fo,"\\5.Graphs")

Strip_Fo <- paste0(Graphs_Fo,"\\3.Stripes")

#############################################################################################################
#####################################   Import Data Frame   #################################################
#############################################################################################################
# Import Precipitation Dataframe
setwd(Precip_Fo_Or)
Main_P_DF <- read.csv(file = "Precipitation.csv",header = TRUE)

# Import Temperature Dataframe
setwd(Temper_Fo_Or)
Main_T_DF <- read.csv(file = "Temperature.csv",header = TRUE)

#############################################################################################################
####################################   Create Stripe Theme  #################################################
#############################################################################################################
theme_strip <- theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(vjust = 1, color = "white", size = 15),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 50, face = "bold", color = "white"),
        plot.caption = element_text(size = 20, color = "white"),
        plot.background=element_rect(fill = "#20231fff"),
        legend.title = element_text(color = "white", face = "bold", size = 30),
        legend.title.align = 0.5,
        legend.text = element_text(color = "white", size = 30),
        legend.key.height = unit(5,"line"),
        legend.key.width = unit(10,"line")
        
  )
#############################################################################################################
########################################  Temperature Stripes  ##############################################
#############################################################################################################
#-> Select BrBG color pallete
col_strip <- rev(brewer.pal(11, "RdBu"))

#-> Rescale values to match the strip height
Main_T_DF$MeanMod <- rescale(Main_T_DF$Mean, to=c(0.5,1.5))

#-> Create a model data to extract the tendency of the data
model_gif <- loess(Main_T_DF$MeanMod ~ seq(1:length(Main_T_DF$MeanMod)), data = Main_T_DF, span = 0.9)

#-> Add tendency values to dataframe
Main_T_DF$Trend <- model_gif$fitted

#-> Delete model
rm(model_gif)

#-> Create Plot
T_Stripes_Mod <- ggplot(Main_T_DF, aes(x = Main_T_DF$Year, y = 1, fill = Main_T_DF$Mean)) +
  geom_tile()+
  scale_x_continuous(breaks=seq(Main_T_DF$Year[1],Main_T_DF$Year[length(Main_T_DF$Year)],6))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = col_strip)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = paste("Yearly mean temperature stipes",Main_T_DF$Year[1],"-", Main_T_DF$Year[length(Main_T_DF$Year)]),
       caption = "Data: ftp://opendata.dwd.de/",
       fill = "T in °C" ) +
  theme_strip +
  geom_line(aes(x=Main_T_DF$Year,y=Main_T_DF$MeanMod)) +
  geom_smooth(aes(x=Main_T_DF$Year,y=Main_T_DF$MeanMod), color = "black", method = "loess")

#-> Export created plot to png file
setwd(Strip_Fo)
ggsave("T_Stripes_Mod.png", T_Stripes_Mod, width=50, height=18, units = "cm", limitsize = FALSE)

#############################################################################################################
################################## Precipitation Stripes ####################################################
#############################################################################################################
#-> Select BrBG color pallete
col_strip <- brewer.pal(11, "BrBG")

#-> Rescale values to match the strip height
Main_P_DF$MeanMod <- rescale(Main_P_DF$Mean, to=c(0.5,1.5))

#-> Create a model data to extract the tendency of the data
model_gif <- loess(Main_P_DF$MeanMod ~ seq(1:length(Main_P_DF$MeanMod)), data = Main_P_DF, span = 0.9)

#-> Add tendency values to dataframe
Main_P_DF$Trend <- model_gif$fitted

#-> Delete model
rm(model_gif)

#-> Create Plot
P_Stripes_Mod <- ggplot(Main_P_DF, aes(x = Main_P_DF$Year, y = 1, fill = Main_P_DF$Mean)) +
  geom_tile()+
  scale_x_continuous(breaks=seq(Main_P_DF$Year[1],Main_P_DF$Year[length(Main_P_DF$Year)],6))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = col_strip)+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = paste("Yearly mean precipitation stripes",Main_P_DF$Year[1],"-", Main_P_DF$Year[length(Main_P_DF$Year)]),
       caption = "Data: ftp://opendata.dwd.de/",
       fill = "P in mm") +
  theme_strip +
  geom_line(aes(x=Main_P_DF$Year,y=Main_P_DF$MeanMod)) +
  geom_smooth(aes(x=Main_P_DF$Year,y=Main_P_DF$MeanMod),color = "black", method = "loess")


#-> Export created plot to png file
setwd(Strip_Fo)
ggsave("P_Stripes_Mod.png", P_Stripes_Mod, width=50, height=18, units = "cm", limitsize = FALSE)






