#############################################################################################################
######################################## OBJECTIVE ##########################################################
#############################################################################################################
# First part on a series of 4 codes to build an infographic to portray the variation of temperature and
# precipitation in Germany since 1881. The data is downloaded from ftp://opendata.dwd.de/ and this code is 
# developed as the final task of the Introduction to Programming and Geostatistics - MB2 class. This first
# code aims to download all data in .gz format, convert them into raster files and extract the mean values
# and export the information as .csv files.
#
# Code made by: Antonio José Castañeda Gómez (s386454)
# Email: antonio.castanedag@gmail.com
#
#############################################################################################################
######################################## LIBRARIES ##########################################################
#############################################################################################################
library(RCurl)
library(gridExtra)
library(stringr)
library(raster)
library(sp)
#############################################################################################################
##################################### FOLDER CREATION #######################################################
#############################################################################################################
### Choose Main Folder
Main_Fo <- choose.dir()

### Change working Directory
setwd(Main_Fo)

### Create Folders
dir.create("2.Data", showWarnings = FALSE)
Data_Fo <- paste0(Main_Fo,"\\2.Data")
dir.create("3.Grids", showWarnings = FALSE)
Grids_Fo <- paste0(Main_Fo,"\\3.Grids")
dir.create("4.Raster", showWarnings = FALSE)
Raster_Fo <- paste0(Main_Fo,"\\4.Raster")
dir.create("5.Graphs", showWarnings = FALSE)
Graphs_Fo <- paste0(Main_Fo,"\\5.Graphs")

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

setwd(Graphs_Fo)
dir.create("1.Shapes", showWarnings = FALSE)
Shapes_Fo <- paste0(Graphs_Fo,"\\1.Shapes")
dir.create("2.Circles", showWarnings = FALSE)
Circles_Fo <- paste0(Graphs_Fo,"\\2.Circles")
dir.create("3.Stripes", showWarnings = FALSE)
Stripes_Fo <- paste0(Graphs_Fo,"\\3.Stripes")
dir.create("4.Bivariate", showWarnings = FALSE)
Bivariate_Fo <- paste0(Graphs_Fo,"\\4.Bivariate")
dir.create("5.InkScape", showWarnings = FALSE)
InkSkape_Fo <- paste0(Graphs_Fo,"\\5.InkScape")

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

#-> Change working 
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

#-> Return to data folder
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