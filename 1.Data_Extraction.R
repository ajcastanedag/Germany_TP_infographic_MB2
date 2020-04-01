#############################################################################################################
######################################## LIBRARIES ##########################################################
#############################################################################################################
library(RCurl)
library(stringr)
library(raster)
library(ggplot2)
library(tidyverse)
library(sp)
library(RStoolbox)
library(gridExtra)
library(RColorBrewer)
library(ggdark)
library(tidyr)
library(gridExtra)
library(scales)
library(broom)
library(grid)
library(gridExtra)
library(gganimate)
library(rasterVis)
library(ggdark)
#############################################################################################################
##################################### FOLDER CREATION #######################################################
#############################################################################################################
### Choose Main Folder
Main_Fo <- "C:\\Users\\Cowboybebop\\Documents\\EAGLE\\I_SEMESTER\\Introdution_to_Progrmming_and_Geostatistics\\Final_Task"#choose.dir()

setwd(Main_Fo)

### Create Folders
dir.create("2.Data", showWarnings = FALSE)
Data_Fo <- paste0(Main_Fo,"\\2.Data")
dir.create("3.Grids", showWarnings = FALSE)
Grids_Fo <- paste0(Main_Fo,"\\3.Grids")
dir.create("4.Raster", showWarnings = FALSE)
Raster_Fo <- paste0(Main_Fo,"\\4.Raster")
dir.create("5.QGis", showWarnings = FALSE)
QGI_Fo <- paste0(Main_Fo,"\\5.QGis")

### Create SubFolders
setwd(Data_Fo)
dir.create("1.Temperature", showWarnings = FALSE)
Temper_Fo_Or <- paste0(Data_Fo,"\\1.Temperature")
dir.create("2.Precipitation", showWarnings = FALSE)
Precip_Fo_Or <-  paste0(Data_Fo,"\\2.Precipitation")

setwd(Grids_Fo)
dir.create("1.Temperature", showWarnings = FALSE)
Temper_Fo_Grd <- paste0(Grids_Fo,"\\1.Temperature")
dir.create("2.Precipitation", showWarnings = FALSE)
Precip_Fo_Grd <-  paste0(Grids_Fo,"\\2.Precipitation")

setwd(Raster_Fo)
dir.create("1.Temperature", showWarnings = FALSE)
Temper_Fo_Rst <- paste0(Raster_Fo,"\\1.Temperature")
dir.create("2.Precipitation", showWarnings = FALSE)
Precip_Fo_Rst <-  paste0(Raster_Fo,"\\2.Precipitation")

setwd(QGI_Fo)
dir.create("1.Shapes", showWarnings = FALSE)
Shapes_Fo <- paste0(QGI_Fo,"\\1.Shapes")
dir.create("2.Circles", showWarnings = FALSE)
Circles_Fo <- paste0(QGI_Fo,"\\2.Circles")
dir.create("3.Strips", showWarnings = FALSE)
Strip_Fo <- paste0(QGI_Fo,"\\3.Strips")
dir.create("4.InkScape", showWarnings = FALSE)

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

#-> Create List to store Names of Monthly Raster for Temperature
Monthly_T_Rst_List <- list()

#-> Create List to store Names of Monthly Raster for Precipitation
Monthly_P_Rst_List <- list()

#############################################################################################################
###############################   Download Temperature Data #################################################
#############################################################################################################
#-> Change the directory to the Temperature folder
setwd(Temper_Fo_Or)

#-> Download all temperature data sorted by month Loop
for(i in 1:length(Month_ID)){
  
  #-> Create the directory of the month
  tmp_dir <- Month_ID[i]
  dir.create(tmp_dir,showWarnings = FALSE)
  
  #-> Change direcotry to subfolders
  setwd(paste0(Temper_Fo_Or,"\\",tmp_dir))
  
  #-> Change the url of the month mean temperature in the loop
  http <- paste0(Temp_Url,Month_ID[i])
  
  #-> Read filenames string available to download
  Av_Data <- getURL(http, verbose=TRUE, ftp.use.epsv=TRUE, dirlistonly = TRUE)
  
  #-> Split single string to directios and save it other variable
  Av_Data_s <- str_split(Av_Data, "\n|\r\n")  # sometimes \r needed
  
  #-> Take the first element where all is stored
  Av_Data_s <- Av_Data_s[[1]]
  
  #-> Sort the filenames increasingly
  Av_Data_s <- sort(Av_Data_s, decreasing = F)
  
  #-> Delete firs element because it is emty
  Av_Data_s <- Av_Data_s[2:length(Av_Data_s)]
  
  #-> Take the numeric part of the character name
  Years <<- regmatches(Av_Data_s, gregexpr("[[:digit:]]+", Av_Data_s))
  
  #-> Delete the month number "01" at the end of each number making sure to have 4 digits
  if(nchar(Years[1]) > 4){Years <- as.numeric(substr(Years,1,nchar(Years)-2))}
  
  #-> List of Data available 
  Av_Data_s <- Av_Data_s[c(seq(1,length(Years), by=1))]
  
  #-> Start download loop for i month, j file
  for (j in 1:length(Av_Data_s)) {
    if(file.exists(Av_Data_s[j])){
      print(paste0(Av_Data_s[j], sep=" ", "file already exists"))
    }
    else
    {
      download.file(paste0(http, Av_Data_s[j]), Av_Data_s[j])
    }
  }
  
  #-> Return to big folder
  setwd(Temper_Fo_Or)
  
}

setwd(Data_Fo)

#############################################################################################################
############################# Read Temperature ASCII-Grid-File ##############################################
#############################################################################################################
#-> Big loop function to read the ascii files inside all folders and store them
# as Raster files naming all layers inside Rasters with corresponding years
#-> Loop through all 12 Months (since those are the existing folder files)
for(k in 1:length(Month_ID)){
  
  #-> Create temporal directory to loop over folders
  tmp_dir <- Month_ID[k]
  setwd(paste0(Temper_Fo_Or,"\\",tmp_dir))
  
  #-> Get all succesfully downloaded folders and save the names (again) 
  filenames <- paste0(grep("*temp*", list.files(path = getwd(), pattern="*.gz$", full.names = FALSE), value=T))
  
  #-> Loop for creating grids  and export single grid files
  for (l in 1:length(filenames)){
    if (l == 1){
      # for the first run define our final raster file ...
      current_ascii <- read.asciigrid(filenames[l])
      #-> remove the raster in case it already exists to avoid duplicate entries
      if(exists("my_raster")){rm(my_raster)}
      my_raster <- raster(current_ascii)
      # 1/10 Modifiction
      my_raster <- my_raster/10
      #Project Raster
      crs(my_raster) <- CRS("+init=epsg:31467")
      #Save single File in Grid Folder
      Sinlge_Rst_Name <- paste0(Temper_Fo_Grd,"\\",
                                substr(str_extract(filenames[l], "[[:digit:]]+"),1,4),
                                "_",
                                substr(Month_ID[k],1,2),
                                "_",
                                substr(Month_List[k], 1,3))
      
      if(file.exists(paste0(Sinlge_Rst_Name,".grd"))){
        print(paste0(Sinlge_Rst_Name, " grid file already exist..."))
      }else{
        writeRaster(my_raster,Sinlge_Rst_Name)
           }
 
    } else {
      # fill it with each additional run with another layer
      current_ascii <- read.asciigrid(filenames[l])
      current_raster <- raster(current_ascii)
      # 1/10 Modifiction
      current_raster <- current_raster/10
      #Project Raster
      crs(current_raster) <- CRS("+init=epsg:31467")
      #Save single File in Grid Folder
      Sinlge_Rst_Name <- paste0(Temper_Fo_Grd,"\\",
                                substr(str_extract(filenames[l], "[[:digit:]]+"),1,4),
                                "_",
                                substr(Month_ID[k],1,2),
                                "_",
                                substr(Month_List[k], 1,3))
      # Check in grid file exist
      if(file.exists(paste0(Sinlge_Rst_Name,".grd"))){
        print(paste0(Sinlge_Rst_Name, " grid file already exist..."))
      } else{
        # Write single grid file
        writeRaster(current_raster,Sinlge_Rst_Name)
      }
      #Stack mod raster
      my_raster <- stack(my_raster, current_raster)
      # Delete all variables that will be redone in the next month folder
      rm(current_ascii, current_raster)
    }
  }
  
  #-> Create single raster per month containing all years
  Raster_name <- paste0("T_",k,"_",Month_List[k])
  
  #-> Create a temporal list of the files in the month folder extracting the numerical portion of the names
  Years_tmp <- regmatches(filenames, gregexpr("[[:digit:]]+", filenames))
  
  #-> Transform the temporal list in a vector
  Years_tmp <- Years_tmp[1:length(filenames)]
  
  #-> Delete the month in the list to het only the years
  if(nchar(Years_tmp[1]) > 4){Years_tmp <- as.numeric(substr(Years_tmp,1,nchar(Years_tmp)-2))}
  
  #-> Add the sufix "Year_"
  Years_tmp <- paste0("Year_", Years_tmp)
  
  #-> Change raster layer names
  names(my_raster) <- Years_tmp
  
  #-> Create the Month raster file
  assign(Raster_name,my_raster)
  
  #-> Plot creation message
  Temp_File_Name <- paste0("T_",k,"_",Month_List[k])
  print(paste0(Temp_File_Name, " raster created..."))
  
  #-> Add Raster to Raster Stack List
  Monthly_T_Rst_List[k] <- Temp_File_Name
  
  #-> Save Raster Stack
  if(file.exists(paste0(Temper_Fo_Rst,"\\","T_",k,"_",Month_List[k],".grd"))){
    print("Raster exist...")
  } else{
    crs(my_raster) <- CRS("+init=epsg:31467")
    writeRaster(my_raster,paste0(Temper_Fo_Rst,"\\","T_",k,"_",Month_List[k]))
    print(paste0("T_",k,"_",Month_List[k], " raster saved..."))
  }
  
  #-> Clean the workspace
  rm(Raster_name,tmp_dir,my_raster,k,l,Years_tmp)
}

#############################################################################################################
################### Extract Values and Create Main Temperature Data Frame ###################################
#############################################################################################################
#-> Create empty DataFrame to store all values from Raster Files
Main_T_DF <- data.frame(matrix(NA, nrow=length(Years), ncol=16))

#-> Name the columns
names(Main_T_DF) <- c("Year",Month_List,"Min","Max","Mean")

#-> Fill the Year column with the years availables
Main_T_DF$Year <- Years

#-> Create a list to access raster from loop
list_Raster <- c(T_1_January,T_2_February,T_3_March,T_4_April,T_5_May,T_6_June,T_7_July,T_8_August,T_9_September,T_10_October,T_11_November,T_12_December)

#-> For loop to fill Dataframe with values from Raster Stack
for(m in 1:length(Month_List)){
  #-> For loop to extraxt values and store them
  for (n in 1:length(Years)){
    #-> Access to Raster in month m
    current_Raster <- list_Raster[[m]]
    
    #-> Access to Raster layer in month m and year n
    current_layer <- current_Raster[[n]]
    
    #-> Calculate the mean of that month in each year 
    current_mean <- mean(current_layer@data@values, na.rm=T)
    
    #-> Save each value in the corresponding column m+1 for starting in the column 2
    Main_T_DF[n,m+1] <- current_mean
    
    #-> Clean Workspace
    rm(current_layer, current_mean, n, current_Raster, current_layer2)
  }
}

#-> Fill mean, max, min values in dataframe
for(i in 1:length(Years)){
  #-> Calculate Min and Max Value
  Main_T_DF$Min[i] <- min(Main_T_DF[i,2:13])
  Main_T_DF$Max[i] <- max(Main_T_DF[i,2:13])
  ###-> Calculate mean value (without the year value of course [i,2:13])
  Main_T_DF$Mean[i] <- mean(as.double(Main_T_DF[i,2:13]))
  Main_T_DF$Mean[i] <- mean(as.double(Main_T_DF[i,2:13]))
}

#-> Export DataFrame
setwd(Temper_Fo_Or)
write.csv(Main_T_DF, "Temperature.csv", row.names = FALSE)

#############################################################################################################
######################### Clear Workspace to process Precipitation ##########################################
#############################################################################################################

rm(T_1_January,T_2_February,T_3_March,T_4_April,T_5_May,T_6_June,T_7_July,T_8_August,T_9_September,T_10_October,T_11_November,T_12_December,list_Raster)

#############################################################################################################
############################### Download Precipitation Data #################################################
#############################################################################################################
#-> Change the directory to the Temperature folder
setwd(Precip_Fo_Or)

#-> Download all temperature data sorted by month Loop
for(i in 1:length(Month_ID)){
  
  #-> Create the directory of the month
  tmp_dir <- Month_ID[i]
  dir.create(tmp_dir,showWarnings = FALSE)
  
  #-> Change direcotry to subfolders
  setwd(paste0(Precip_Fo_Or,"\\",tmp_dir))
  
  #-> Change the url of the month mean temperature in the loop
  http <- paste0(Prec_Url,Month_ID[i])
  
  #-> Read filenames string available to download
  Av_Data <- getURL(http, verbose=TRUE, ftp.use.epsv=TRUE, dirlistonly = TRUE)
  
  #-> Split single string to directios and save it other variable
  Av_Data_s <- str_split(Av_Data, "\n|\r\n")  # sometimes \r needed
  
  #-> Take the first element where all is stored
  Av_Data_s <- Av_Data_s[[1]]
  
  #-> Sort the filenames increasingly
  Av_Data_s <- sort(Av_Data_s, decreasing = F)
  
  #-> Delete firs element because it is emty
  Av_Data_s <- Av_Data_s[2:length(Av_Data_s)]
  
  #-> Take the numeric part of the character name
  Years <<- regmatches(Av_Data_s, gregexpr("[[:digit:]]+", Av_Data_s))
  
  #-> Delete the month number "01" at the end of each number making sure to have 4 digits
  if(nchar(Years[1]) > 4){Years <- as.numeric(substr(Years,1,nchar(Years)-2))}
  
  #-> List of Data available 
  Av_Data_s <- Av_Data_s[c(seq(1,length(Years), by=1))]
  
  #-> Start download loop for i month, j file
  for (j in 1:length(Av_Data_s)) {
    if(file.exists(Av_Data_s[j])){
      print(paste0(Av_Data_s[j], sep=" ", "file already exists"))
    }
    else
    {
      download.file(paste0(http, Av_Data_s[j]), Av_Data_s[j])
    }
  }
  
  #-> Return to big folder
  setwd(Precip_Fo_Or)
  
}

setwd(Data_Fo)

#############################################################################################################
############################# Read Precipitation ASCII-Grid-File ############################################
#############################################################################################################
#-> Big loop function to read the ascii files inside all folders and store them
# as Raster files naming all layers inside Rasters with corresponding years
#-> Loop through all 12 Months (since those are the existing folder files)
for(k in 1:length(Month_ID)){
  
  #-> Create temporal directory to loop over folders
  tmp_dir <- Month_ID[k]
  setwd(paste0(Precip_Fo_Or,"\\",tmp_dir))
  
  #-> Get all succesfully downloaded folders and save the names (again) 
  filenames <- paste0(grep("*prec*", list.files(path = getwd(), pattern="*.gz$", full.names = FALSE), value=T))
  
  #-> Loop for creating grids  and export single grid files
  for (l in 1:length(filenames)){
    if (l == 1){
      # for the first run define our final raster file ...
      current_ascii <- read.asciigrid(filenames[l])
      #-> remove the raster in case it already exists to avoid duplicate entries
      if(exists("my_raster")){rm(my_raster)}
      my_raster <- raster(current_ascii)
      #Project Raster
      crs(my_raster) <- CRS("+init=epsg:31467")
      #Save single File in Grid Folder
      Sinlge_Rst_Name <- paste0(Precip_Fo_Grd,"\\",
                                substr(str_extract(filenames[l], "[[:digit:]]+"),1,4),
                                "_",
                                substr(Month_ID[k],1,2),
                                "_",
                                substr(Month_List[k], 1,3))
      
      if(file.exists(paste0(Sinlge_Rst_Name,".grd"))){
        print(paste0(Sinlge_Rst_Name, " grid file already exist..."))
      }else{
        writeRaster(my_raster,Sinlge_Rst_Name)
      }
      
    } else {
      # fill it with each additional run with another layer
      current_ascii <- read.asciigrid(filenames[l])
      current_raster <- raster(current_ascii)
      #Project Raster
      crs(current_raster) <- CRS("+init=epsg:31467")
      #Save single File in Grid Folder
      Sinlge_Rst_Name <- paste0(Precip_Fo_Grd,"\\",
                                substr(str_extract(filenames[l], "[[:digit:]]+"),1,4),
                                "_",
                                substr(Month_ID[k],1,2),
                                "_",
                                substr(Month_List[k], 1,3))
      # Check in grid file exist
      if(file.exists(paste0(Sinlge_Rst_Name,".grd"))){
        print(paste0(Sinlge_Rst_Name, " grid file already exist..."))
      } else{
        # Write single grid file
        writeRaster(current_raster,Sinlge_Rst_Name)
      }
      #Stack mod raster
      my_raster <- stack(my_raster, current_raster)
      # Delete all variables that will be redone in the next month folder
      rm(current_ascii, current_raster)
    }
  }
  
  #-> Create single raster per month containing all years
  Raster_name <- paste0("P_",k,"_",Month_List[k])
  
  #-> Create a temporal list of the files in the month folder extracting the numerical portion of the names
  Years_tmp <- regmatches(filenames, gregexpr("[[:digit:]]+", filenames))
  
  #-> Transform the temporal list in a vector
  Years_tmp <- Years_tmp[1:length(filenames)]
  
  #-> Delete the month in the list to het only the years
  if(nchar(Years_tmp[1]) > 4){Years_tmp <- as.numeric(substr(Years_tmp,1,nchar(Years_tmp)-2))}
  
  #-> Add the sufix "Year_"
  Years_tmp <- paste0("Year_", Years_tmp)
  
  #-> Change raster layer names
  names(my_raster) <- Years_tmp
  
  #-> Create the Month raster file
  assign(Raster_name,my_raster)
  
  #-> Plot creation message
  Temp_File_Name <- paste0("P_",k,"_",Month_List[k])
  print(paste0(Temp_File_Name, " raster created..."))
  
  #-> Add Raster to Raster Stack List
  Monthly_P_Rst_List[k] <- Temp_File_Name
  
  #-> Save Raster Stack
  if(file.exists(paste0(Precip_Fo_Rst,"\\","P_",k,"_",Month_List[k],".grd"))){
    print("Raster exist...")
  } else{
    crs(my_raster) <- CRS("+init=epsg:31467")
    writeRaster(my_raster,paste0(Precip_Fo_Rst,"\\","P_",k,"_",Month_List[k]))
    print(paste0("P_",k,"_",Month_List[k], " raster saved..."))
  }
  
  #-> Clean the workspace
  rm(Raster_name,tmp_dir,my_raster,k,l,Years_tmp)
}

#############################################################################################################
################### Extract Values and Create Main Precipitation Data Frame #################################
#############################################################################################################
#-> Create empty DataFrame to store all values from Raster Files
Main_P_DF <- data.frame(matrix(NA, nrow=length(Years), ncol=16))

#-> Name the columns
names(Main_P_DF) <- c("Year",Month_List,"Min","Max","Mean")

#-> Fill the Year column with the years availables
Main_P_DF$Year <- Years

#-> Create a list to access raster from loop
list_Raster <- c(P_1_January,P_2_February,P_3_March,P_4_April,P_5_May,P_6_June,P_7_July,P_8_August,P_9_September,P_10_October,P_11_November,P_12_December)

#-> For loop to fill Dataframe with values from Raster Stack
for(m in 1:length(Month_List)){
  #-> For loop to extraxt values and store them
  for (n in 1:length(Years)){
    #-> Access to Raster in month m
    current_Raster <- list_Raster[[m]]
    
    #-> Access to Raster layer in month m and year n
    current_layer <- current_Raster[[n]]
    
    #-> Calculate the mean of that month in each year 
    current_mean <- mean(current_layer@data@values, na.rm=T)
    
    #-> Save each value in the corresponding column m+1 for starting in the column 2
    Main_P_DF[n,m+1] <- current_mean
    
    #-> Clean Workspace
    rm(current_layer, current_mean, n, current_Raster, current_layer2)
  }
}

#-> Fill mean, max, min values in dataframe
for(i in 1:length(Years)){
  #-> Calculate Min and Max Value
  Main_P_DF$Min[i] <- min(Main_P_DF[i,2:13])
  Main_P_DF$Max[i] <- max(Main_P_DF[i,2:13])
  ###-> Calculate mean value (without the year value of course [i,2:13])
  Main_P_DF$Mean[i] <- mean(as.double(Main_P_DF[i,2:13]))
  Main_P_DF$Mean[i] <- mean(as.double(Main_P_DF[i,2:13]))
}

#-> Export DataFrame
setwd(Precip_Fo_Or)
write.csv(Main_P_DF, "Precipitation.csv", row.names = FALSE)

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
  TMP <- stack(GridFilesTemp[i]) 
  Circle_Preci_DF$P_Mean[i] <-  data.frame(TMP.mean=cellStats(TMP, "mean"))
}

#-> Change class of mean to numeric class
Circle_Preci_DF$P_Mean <- as.numeric(Circle_Preci_DF$P_Mean)

#-> Get color names of pallete
Precipitation_Colors <- brewer.pal(n = 9, name = "BrBG")

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



(max(Circle_Preci_DF$P_Mean)-min(Circle_Preci_DF$P_Mean))/2


