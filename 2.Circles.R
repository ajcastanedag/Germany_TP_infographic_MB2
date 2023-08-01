#############################################################################################################
######################################## OBJECTIVE ##########################################################
#############################################################################################################
# Second part on a series of 4 codes to build an infographic to portray the variation of temperature and
# precipitation in Germany since 1881. The data is downloaded from ftp://opendata.dwd.de/ and this code is 
# developed as the final task of the Introduction to Programming and Geostatistics - MB2 class. This second
# code aims to build a mill graph to show in a radial configuration the temperature and the precipitation 
# variations. This code relies on the first code or in the .csv files provided in the git repository.
#
# Code made by: Antonio José Castañeda Gómez (s386454)
# Email: antonio.castanedag@gmail.com
#
#############################################################################################################
######################################## LIBRARIES ##########################################################
#############################################################################################################
pacman::p_load(stringr, raster, RColorBrewer, ggplot2)
#############################################################################################################
##################################### FOLDER DECLARATION ####################################################
#############################################################################################################
### Choose Main Folder
Main_Fo <- rchoose.dir()
setwd(Main_Fo)

### Declare Folders
Data_Fo <- paste0(Main_Fo,"/2.Data")
Grids_Fo <- paste0(Main_Fo,"/3.Grids")
Raster_Fo <- paste0(Main_Fo,"/4.Raster")
Graphs_Fo <- paste0(Main_Fo,"/5.Graphs")

### Create SubFolders
Temper_Fo_Or <- paste0(Data_Fo,"/1.Temperature")
Precip_Fo_Or <-  paste0(Data_Fo,"/2.Precipitation")

Temper_Fo_Grd <- paste0(Grids_Fo,"/1.Temperature")
Precip_Fo_Grd <-  paste0(Grids_Fo,"/2.Precipitation")

Temper_Fo_Rst <- paste0(Raster_Fo,"/1.Temperature")
Precip_Fo_Rst <-  paste0(Raster_Fo,"/2.Precipitation")

Shapes_Fo <- paste0(Graphs_Fo,"/1.Shapes")
Circles_Fo <- paste0(Graphs_Fo,"/2.Circles")
Strip_Fo <- paste0(Graphs_Fo,"/3.Strips")

#############################################################################################################
#####################################    VARIABLES    #######################################################
#############################################################################################################
#-> Create month chracter List
Month_List <- c("January","February","March","April","May","June","July","August","September","October","November","December")

#-> Standarize List to access online data
Month_ID <- c("01_Jan/","02_Feb/","03_Mar/","04_Apr/","05_May/","06_Jun/","07_Jul/","08_Aug/","09_Sep/","10_Oct/","11_Nov/","12_Dec/")
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
####################################   Create Circle Theme  #################################################
#############################################################################################################
# Define theme for the graphs
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
                      plot.background = element_blank(),
                      legend.box.spacing = unit(0, "mm"),
                      legend.position = "none",
                      plot.title = element_text(size = 180,
                                                color= "white",
                                                margin=margin(0,0,30,30)))
#############################################################################################################
###############################  Big Circle Temperature Graph  ##############################################
#############################################################################################################
#-> Change directory 
setwd(Temper_Fo_Grd)

#-> Read *.gri path name and reblace / with // 
GridFilesTemp <- paste0(grep("*.grd*", list.files(path = getwd(), pattern="*.grd$", full.names = TRUE), value=T))
GridFilesTemp <- str_replace_all(string = GridFilesTemp, pattern = "/", replacement = "//" )

#-> Select values ONLY if there is data for the whole year
while(length(GridFilesTemp) %% 12 != 0){
  GridFilesTemp <- GridFilesTemp[-length(GridFilesTemp)]
}

### -> Creation of Dataframe to store all values for circle plot
#-> Create control ID
ID <- seq(1:length(GridFilesTemp))

#-> Extract Years character from readed files "1881_01"
Start <- nchar(GridFilesTemp[1]) - 14
Stop <- nchar(GridFilesTemp[1]) - 8

#-> Create a list of Years
Years <- unique(substr(GridFilesTemp, start = Start, stop = Stop))

#-> Create inverted list of months (Inverted because of the way it will be ploted) 
Month_Lst_Inv <- c("12.Jan","11.Feb","10.Mar","09.Apr","08.May","07.Jun","06.Jul","05.Aug","04.Sep","03.Oct","02.Nov","01.Dec")

### -> Create dataframe to store all data for circle plot
Circle_Temp_DF <- as.data.frame(ID)
#-> Assign Year list to Year and Month 
Circle_Temp_DF$Year <- Years 
Circle_Temp_DF$Month <- Years
#-> Substract values to match year and month
Circle_Temp_DF$Year <- substr(Circle_Temp_DF$Year,1,4)
Circle_Temp_DF$Month <- Month_Lst_Inv[as.numeric(substr(Circle_Temp_DF$Month,6,7))]

#-> Extract Mean value from grid files per month
for(i in 1:length(Years)){
  TMP <- raster(GridFilesTemp[i]) 
  Circle_Temp_DF$T_Mean[i] <-  data.frame(TMP.mean=cellStats(TMP, "mean"))
}

#-> Change class of mean to numeric class
Circle_Temp_DF$T_Mean <- as.numeric(Circle_Temp_DF$T_Mean)


###-> Create color names for all data based on max and min
#-> Get color names of pallete
Temperature_Colors <- rev(brewer.pal(n = 9, name = "RdYlBu"))

#-> Assign color names of pallete to function
pie.colors <- colorRampPalette(Temperature_Colors)

#-> Create a list of unique values of Temperature
Value_List <- unique(Circle_Temp_DF$T_Mean)

#-> Sort values of Temperature
Value_List <- sort(Value_List, decreasing = FALSE)

#-> Create a temporal dataframe to store the color name and its value
Tmp_DF<-data.frame(value=Value_List, color.name=pie.colors(length(Value_List))) 

#-> Create empty column in Circle_Temp_DF to match values and colors
Circle_Temp_DF$color <- ""

#-> For loop to compare values in Tmp_DF and Circle_Temp_DF and assign them
for(i in 1:length(Circle_Temp_DF$T_Mean)){
  for(j in 1:length(Tmp_DF$value)){
    if(Circle_Temp_DF$T_Mean[i] == Tmp_DF$value[j]){
      Circle_Temp_DF$color[i] <- as.character(Tmp_DF$color.name[j])
    }
  }
}

###-> Create Mean_Df to store mean values per month
Month <- Month_Lst_Inv
Month <- substr(Month,4,7)
Mean_Df <- as.data.frame(Month)
#-> Due to the graph will be wrapped on the Y axis and its originally a bar plot
# each month value has limits between the month ID, for exaple January is a bar
# and its limits are between 0 and 1
Mean_Df$Min_Cir <- seq(0,11)
Mean_Df$Max_Cir <- seq(1,12)

#-> Calculate the mean value per month from the Main_T_DF
for(i in 2:13){
  Mean_Df$Med_Month[i-1] <- mean(Main_T_DF[1:139,i])
  Mean_Df$Min_Month [i-1] <- min(Main_T_DF[1:139,i])
  Mean_Df$Max_Month[i-1] <- max(Main_T_DF[1:139,i])
}

#-> Linear regresion to calculate the mean value per month based on the limits on the barplot
Mean_Df$Med_Trans <- ((Mean_Df$Med_Month - Mean_Df$Min_Month)/(Mean_Df$Max_Month - Mean_Df$Min_Month)) + Mean_Df$Min_Cir

###-> Create new data frame to store all values of temperatures and adjust values to plot range
# leng will be the x for the barplot and starts from 0.5 to fit the middle of the bar
Curves <- data.frame(Leng = seq(0.5,138.5))

#-> Paste Main_T_DF dataframe by month
Curves <- cbind(Curves, Main_T_DF[1:139, names(Main_T_DF) %in% Month_List])

#-> Addjust values to each month interval
for(i in 1:12){
  Curves[i+1] <- ((Curves[i+1] - Mean_Df$Min_Month[i])/(Mean_Df$Max_Month[i] - Mean_Df$Min_Month[i])) + Mean_Df$Min_Cir[i]
}

# Change to capital letters all the month list to be ploted
Month <- as.character(lapply(Month, toupper))

###-> Create GGPLOT
Circel_T <- ggplot()+
  ##### Title
  ggtitle(bquote(bold('TEMPERATURE') ~ 'mill graph')) +
  ##### Color Bars
  geom_bar(aes(x = Circle_Temp_DF$Year, y = 1),stat="identity", fill = Circle_Temp_DF$color, width = 1) +
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
  ##### Additional Lines
  # Outter Ring
  geom_segment(aes(x=150,xend=150,y=0,yend=12),colour="white", size = 4)+
  # Tick marks
  geom_segment(aes(x=140, xend=160,y=seq(0,11), yend=seq(0,11)), colour="white", size = 4)+
  # Labels
  geom_text(mapping=aes(x=155, y=seq(0.4,11.4), label=Month), color= "white", fontface = "bold", size = 40, angle=seq(-13,-343,length.out=12), vjust=-0.4, hjust=0) +
  ##### Wrap Y Coordinate       
  coord_polar("y")

##### Export PNG file
setwd(Circles_Fo)
ggsave("T_Circle.png",Circel_T,width=70, height=70, limitsize = FALSE, bg = "transparent")

#############################################################################################################
###############################  Big Circle Precipitation Graph  ############################################
#############################################################################################################
#-> Change directory 
setwd(Precip_Fo_Grd)

#-> Read *.gri path name and reblace / with // 
GridFilesTemp <- paste0(grep("*.grd*", list.files(path = getwd(), pattern="*.grd$", full.names = TRUE), value=T))
GridFilesTemp <- str_replace_all(string = GridFilesTemp, pattern = "/", replacement = "//" )

#-> Select values ONLY if there is data for the whole year
while(length(GridFilesTemp) %% 12 != 0){
  GridFilesTemp <- GridFilesTemp[-length(GridFilesTemp)]
}

### -> Creation of Dataframe to store all values for circle plot
#-> Create control ID
ID <- seq(1:length(GridFilesTemp))

#-> Extract Years from readed files "1881_01"
Start <- nchar(GridFilesTemp[1]) - 14
Stop <- nchar(GridFilesTemp[1]) - 8

#-> Create a list of Years
Years <- unique(substr(GridFilesTemp, start = Start, stop = Stop))

#-> Create inverted list of months 
Month_Lst_Inv <- c("12.Jan","11.Feb","10.Mar","09.Apr","08.May","07.Jun","06.Jul","05.Aug","04.Sep","03.Oct","02.Nov","01.Dec")

### -> Create dataframe to store all data for circle plot
Circle_Preci_DF <- as.data.frame(ID)
#-> Assign Year list to Year and Month 
Circle_Preci_DF$Year <- Years 
Circle_Preci_DF$Month <- Years 
#-> Substract values to match year and month
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

#-> Create a list of unique values of Temperature
Value_List <- unique(Circle_Preci_DF$P_Mean)

#-> Sort values of Temperature
Value_List <- sort(Value_List, decreasing = FALSE)

#-> Create a temporal dataframe to store the color name and its value
Tmp_DF<-data.frame(value=Value_List, color.name=pie.colors(length(Value_List))) 

#-> Create empty column in Circle_Preci_DF to match values and colors
Circle_Preci_DF$color <- ""

#-> For loop to compare values in Tmp_DF and Circle_Preci_DF and assign them
for(i in 1:length(Circle_Preci_DF$P_Mean)){
  for(j in 1:length(Tmp_DF$value)){
    if(Circle_Preci_DF$P_Mean[i] == Tmp_DF$value[j]){
      Circle_Preci_DF$color[i] <- as.character(Tmp_DF$color.name[j])
    }
  }
}

###-> Create Mean_Df to store mean values per month
Month <- Month_Lst_Inv
Month <- substr(Month,4,7)
Mean_Df <- as.data.frame(Month)

#-> Due to the graph will be wrapped on the Y axis and its originally a bar plot
# each month value has limits between the month ID, for exaple January is a bar
# and its limits are between 0 and 1
Mean_Df$Min_Cir <- seq(0,11)
Mean_Df$Max_Cir <- seq(1,12)

#-> Calculate the mean value per month from the Main_T_DF
for(i in 2:13){
  Mean_Df$Med_Month[i-1] <- mean(Main_P_DF[1:139,i])
  Mean_Df$Min_Month [i-1] <- min(Main_P_DF[1:139,i])
  Mean_Df$Max_Month[i-1] <- max(Main_P_DF[1:139,i])
}

#-> Linear regresion to calculate the mean value per month based on the limits on the barplot
Mean_Df$Med_Trans <- ((Mean_Df$Med_Month - Mean_Df$Min_Month)/(Mean_Df$Max_Month - Mean_Df$Min_Month)) + Mean_Df$Min_Cir

###-> Create new data frame to store all values of temperatures and adjust values to plot range
# leng will be the x for the barplot and starts from 0.5 to fit the middle of the bar
Curves <- data.frame(Leng = seq(0.5,138.5))

#-> Paste Main_T_DF dataframe by month
Curves <- cbind(Curves, Main_P_DF[1:139, names(Main_P_DF) %in% Month_List])

#-> Addjust values to each month interval
for(i in 1:12){
  Curves[i+1]<- ((Curves[i+1] - Mean_Df$Min_Month[i])/(Mean_Df$Max_Month[i] - Mean_Df$Min_Month[i])) + Mean_Df$Min_Cir[i]
}

# Change to capital letters all the month list to be ploted
Month <- as.character(lapply(Month, toupper))

###-> Create GGPLOT
Circel_P <- ggplot()+
  ##### Title
  ggtitle(bquote(bold('PRECIPITATION') ~ 'mill graph')) +
  ##### Color Bars
  geom_bar(aes(x = Circle_Preci_DF$Year, y = 1), stat="identity", fill = Circle_Preci_DF$color, width = 1) +
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
  ##### Additional Lines
  # Outter Ring
  geom_segment(aes(x=150,xend=150,y=0,yend=12),colour="white", size = 4)+
  # Tick marks
  geom_segment(aes(x=140, xend=160,y=seq(0,11), yend=seq(0,11)), colour="white", size = 4)+
  # Labels
  geom_text(mapping=aes(x=155, y=seq(0.4,11.4), label=Month), color= "white", fontface = "bold", size = 40, angle=seq(-13,-343,length.out=12), vjust=-0.4, hjust=0) +
  ##### Flip Coordinates       
  coord_polar("y")

##### Export PNG file
setwd(Circles_Fo)
ggsave("P_Circle2.png",Circel_P,width=70, height=70, limitsize = FALSE, bg = "transparent")
