#############################################################################################################
######################################## LIBRARIES ##########################################################
#############################################################################################################
library(stringr)
library(raster)
library(RColorBrewer)
library(ggplot2)
#############################################################################################################
##################################### FOLDER DECLARATION ####################################################
#############################################################################################################
### Choose Main Folder
Main_Fo <- "C:\\Users\\Cowboybebop\\Desktop\\Map_Final"#choose.dir()

setwd(Main_Fo)

### Create Folders
Script_Fo <- paste0(Main_Fo,"\\1.Script")
Data_Fo <- paste0(Main_Fo,"\\2.Data")
Grids_Fo <- paste0(Main_Fo,"\\3.Grids")
Raster_Fo <- paste0(Main_Fo,"\\4.Raster")
QGI_Fo <- paste0(Main_Fo,"\\5.QGis")

### Create SubFolders
Temper_Fo_Or <- paste0(Data_Fo,"\\1.Temperature")
Precip_Fo_Or <-  paste0(Data_Fo,"\\2.Precipitation")

Temper_Fo_Grd <- paste0(Grids_Fo,"\\1.Temperature")
Precip_Fo_Grd <-  paste0(Grids_Fo,"\\2.Precipitation")

Temper_Fo_Rst <- paste0(Raster_Fo,"\\1.Temperature")
Precip_Fo_Rst <-  paste0(Raster_Fo,"\\2.Precipitation")

Shapes_Fo <- paste0(QGI_Fo,"\\1.Shapes")
Circles_Fo <- paste0(QGI_Fo,"\\2.Circles")
Strip_Fo <- paste0(QGI_Fo,"\\3.Strips")

#############################################################################################################
#####################################    VARIABLES    #######################################################
#############################################################################################################
#-> Create month chracter List
Month_List <- c("January","February","March","April","May","June","July","August","September","October","November","December")

#-> Standarize List to access online data
Month_ID <- c("01_Jan/","02_Feb/","03_Mar/","04_Apr/","05_May/","06_Jun/","07_Jul/","08_Aug/","09_Sep/","10_Oct/","11_Nov/","12_Dec/")

#-> URL for all temperature Data
Temp_Url <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/air_temperature_mean/"

#-> URL for all temperature Data
Prec_Url <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/precipitation/"

#############################################################################################################
#####################################   Import Data Frame   #################################################
#############################################################################################################
setwd(Precip_Fo_Or)
Main_P_DF <- read.csv(file = "Precipitation.csv",header = TRUE)

#############################################################################################################
###############################  Big Circle Precipitation Graph  ############################################
#############################################################################################################
#-> Change directory 
setwd(Precip_Fo_Grd)

#-> Read *.gri path name and reblace / with // 
GridFilesTemp <- paste0(grep("*.grd*", list.files(path = getwd(), pattern="*.grd$", full.names = TRUE), value=T))
GridFilesTemp <- str_replace_all(string = GridFilesTemp, pattern = "/", replacement = "//" )

### -> Creation of Dataframe to store all values for circle plot
#-> Create control ID
ID <- seq(1:length(GridFilesTemp))

#-> Extract Years from readed files
Years <- unique(substr(GridFilesTemp, start = 71, stop = 77))

#-> Create inverted list of months 
Month_Lst_Inv <- c("12.Jan","11.Feb","10.Mar","09.Apr","08.May","07.Jun","06.Jul","05.Aug","04.Sep","03.Oct","02.Nov","01.Dec")

#-> Create dataframe to store all data for circle plot
Circle_Preci_DF <- as.data.frame(ID)
Circle_Preci_DF$Year <- Years 
Circle_Preci_DF$Month <- Years 
Circle_Preci_DF$Year <- substr(Circle_Preci_DF$Year,1,4)
Circle_Preci_DF$Month <- Month_Lst_Inv[as.numeric(substr(Circle_Preci_DF$Month,6,7))]

#-> Extract mean from grid files per month
for(i in 1:length(Years)){
  TMP <- raster(GridFilesTemp[i]) 
  Circle_Preci_DF$P_Mean[i] <-  data.frame(TMP.mean=cellStats(TMP, "mean"))
}

#-> Change class of mean to numeric class
Circle_Preci_DF$P_Mean <- as.numeric(Circle_Preci_DF$P_Mean)

#-> Get color names of pallete
Precipitation_Colors <- brewer.pal(n = 9, name = "BrBG") #c("#8C510A","#FBFCBF","#01668C","#0166BE")#

#-> Assign color names of pallete to function
pie.colors <- colorRampPalette(Precipitation_Colors)

Value_List <- unique(Circle_Preci_DF$P_Mean)

Value_List <- sort(Value_List, decreasing = FALSE)

Tmp_DF<-data.frame(value=Value_List, color.name=pie.colors(length(Value_List))) 

Circle_Preci_DF$color <- ""

for(i in 1:length(Circle_Preci_DF$P_Mean)){
  for(j in 1:length(Tmp_DF$value)){
    if(Circle_Preci_DF$P_Mean[i] == Tmp_DF$value[j]){
      Circle_Preci_DF$color[i] <- as.character(Tmp_DF$color.name[j])
    }
  }
}

#########################################################################

Mes <- Month_Lst_Inv
Mes <- substr(Mes,4,7)
Mean_Df <- as.data.frame(Mes)

Mean_Df$Min_Cir <- seq(0,11)
Mean_Df$Max_Cir <- seq(1,12)
#########
#Aca calcular desde el stack la media por mes, pongo números cualquiera mientras tanto
for(i in 2:13){
  Mean_Df$Med_Mes[i-1] <- mean(Main_P_DF[1:139,i])
  Mean_Df$Min_Mes [i-1] <- min(Main_P_DF[1:139,i])
  Mean_Df$Max_Mes[i-1] <- max(Main_P_DF[1:139,i])
}

Mean_Df$Med_Trans <- ((Mean_Df$Med_Mes - Mean_Df$Min_Mes)/(Mean_Df$Max_Mes - Mean_Df$Min_Mes)) + Mean_Df$Min_Cir

#########
Curves <- data.frame(Leng=seq(0.5,138.5))

Curves <- cbind(Curves, Main_P_DF[1:139, names(Main_P_DF) %in% Month_List])

for(i in 1:12){
  Curves[i+1]<- ((Curves[i+1] - Mean_Df$Min_Mes[i])/(Mean_Df$Max_Mes[i] - Mean_Df$Min_Mes[i])) + Mean_Df$Min_Cir[i]
}


#Fecasha importates que pueden influenciar la temperatura
Inicio_Guerra <- 1939-1881
Fin_Guerra <- 1945 -1881
Caida_Muro <- 1961 -1881
# Poner datos de www.globalcarbonproject.org max y min de emision de CO2 

#########################################################################
theme_circle <- theme(axis.text.y = element_blank(),
                      axis.text.x = element_blank(),
                      axis.line.y = element_blank(),
                      axis.line.x = element_blank(),
                      axis.title = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid.major = element_blank(),
                      legend.title = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      legend.box.spacing = unit(0, "mm"),
                      legend.position = "none")

Circel_P <- ggplot()+
  ##### Color Bars
  geom_bar(aes(x = Circle_Preci_DF$Year, y = 1),stat="identity", fill = Circle_Preci_DF$color, width = 1) +
  ##### Month Spacing in white
  geom_hline(yintercept = seq(0,11), color="gray", size= 1) +
  ##### Delete axis
  theme_circle +
  ##### Data Lines
  geom_line(data = Curves, aes(x=Leng, y=January)) +
  geom_line(data = Curves, aes(x=Leng, y=February)) +
  geom_line(data = Curves, aes(x=Leng, y=March)) +
  geom_line(data = Curves, aes(x=Leng, y=April)) +
  geom_line(data = Curves, aes(x=Leng, y=May)) +
  geom_line(data = Curves, aes(x=Leng, y=June)) +
  geom_line(data = Curves, aes(x=Leng, y=July)) +
  geom_line(data = Curves, aes(x=Leng, y=August)) +
  geom_line(data = Curves, aes(x=Leng, y=September)) +
  geom_line(data = Curves, aes(x=Leng, y=October)) +
  geom_line(data = Curves, aes(x=Leng, y=November)) +
  geom_line(data = Curves, aes(x=Leng, y=December)) +
  #### Fill
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$January), ymax=Mean_Df$Med_Trans[1]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$February), ymax=Mean_Df$Med_Trans[2]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$March), ymax=Mean_Df$Med_Trans[3]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$April), ymax=Mean_Df$Med_Trans[4]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$May), ymax=Mean_Df$Med_Trans[5]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$June), ymax=Mean_Df$Med_Trans[6]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$July), ymax=Mean_Df$Med_Trans[7]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$August), ymax=Mean_Df$Med_Trans[8]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$September), ymax=Mean_Df$Med_Trans[9]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$October), ymax=Mean_Df$Med_Trans[10]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$November), ymax=Mean_Df$Med_Trans[11]), fill="black") +
  geom_ribbon(data= Curves,aes(x=Leng,ymin=pmin(Curves$December), ymax=Mean_Df$Med_Trans[12]), fill="black") +
  ##### Mean Lines
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[1]),linetype=1, color="white", size = 3) +
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[2]),linetype=1, color="white", size = 3) +  
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[3]),linetype=1, color="white", size = 3) +  
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[4]),linetype=1, color="white", size = 3) +  
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[5]),linetype=1, color="white", size = 3) +  
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[6]),linetype=1, color="white", size = 3) +  
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[7]),linetype=1, color="white", size = 3) +  
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[8]),linetype=1, color="white", size = 3) +  
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[9]),linetype=1, color="white", size = 3) +  
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[10]),linetype=1, color="white", size = 3) + 
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[11]),linetype=1, color="white", size = 3) +
  geom_line(data = Curves, aes(x=Leng,y=Mean_Df$Med_Trans[12]),linetype=1, color="white", size = 3) +
  ##### Trend Lines Using Smooth
  geom_smooth(data = Curves, aes(x=Leng, y=January), method = "loess", size = 4, color= "beige")+
  geom_smooth(data = Curves, aes(x=Leng, y=February), method = "loess", size = 4, color= "beige") +
  geom_smooth(data = Curves, aes(x=Leng, y=March), method = "loess", size = 4, color= "beige") +
  geom_smooth(data = Curves, aes(x=Leng, y=April), method = "loess", size = 4, color= "beige") +
  geom_smooth(data = Curves, aes(x=Leng, y=May), method = "loess", size = 4, color= "beige") +
  geom_smooth(data = Curves, aes(x=Leng, y=June), method = "loess", size = 4, color= "beige") +
  geom_smooth(data = Curves, aes(x=Leng, y=July), method = "loess", size = 4, color= "beige") +
  geom_smooth(data = Curves, aes(x=Leng, y=August), method = "loess", size = 4, color= "beige") +
  geom_smooth(data = Curves, aes(x=Leng, y=September), method = "loess", size = 4, color= "beige") +
  geom_smooth(data = Curves, aes(x=Leng, y=October), method = "loess", size = 4, color= "beige") +
  geom_smooth(data = Curves, aes(x=Leng, y=November), method = "loess", size = 4, color= "beige") +
  geom_smooth(data = Curves, aes(x=Leng, y=December), method = "loess", size = 4, color= "beige") +
  ##### Maybe additional Lines  
  #geom_vline(xintercept = Inicio_Guerra) +
  #geom_vline(xintercept = Fin_Guerra) +
  #geom_vline(xintercept = Caida_Muro) +#
  ##### Flip Coordinates       
  coord_polar("y")

Circel_P

setwd(Circles_Fo)

ggsave("P_Circle.png",Circel_P,width=70, height=70, limitsize = FALSE, bg = "transparent")




















