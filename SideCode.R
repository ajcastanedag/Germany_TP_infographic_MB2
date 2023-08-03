################################################################################
############################### Load Libraries ################################# 
################################################################################
pacman::p_load(rChoiceDialogs, ggplot2,stringr,lattice,rasterVis, latticeExtra,
               viridis,ggthemes,scales, grid, raster,grid,egg, sp,sf,tidyverse,
               ggpubr)
################################################################################
####################### Set folder structure / Location ######################## 
################################################################################
### Choose Main Folder
Main_Fo <- rchoose.dir()
setwd(Main_Fo)

### Declare Folders
Graphs_Fo <- paste0(Main_Fo,"/5.Graphs")
Data_Fo <- paste0(Main_Fo,"/2.Data")
Grids_Fo <- paste0(Main_Fo,"/3.Grids")
Raster_Fo <- paste0(Main_Fo,"/4.Raster")


### Declare SubFolders
Shapes_Fo <- paste0(Graphs_Fo,"/1.Shapes")
Temper_Fo_Or <- paste0(Data_Fo,"/1.Temperature")
Temper_Fo_Grd <- paste0(Grids_Fo,"/1.Temperature")
Temper_Fo_Rst <- paste0(Raster_Fo,"/1.Temperature")

################################################################################
############################## Set of variables ################################ 
################################################################################
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

################################################################################
################## Read Temperature ASCII-Grid-File ############################
################################################################################
#-> Big loop function to read the ascii files inside all folders and store them
# as Raster files naming all layers inside Rasters with corresponding years
#-> Loop through all 12 Months (since those are the existing folder files)
for(k in 1:length(Month_ID)){
  
  #-> Create temporal directory to loop over folders
  tmp_dir <- Month_ID[k]
  setwd(paste0(Temper_Fo_Or,"/",tmp_dir))
  
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
      Sinlge_Rst_Name <- paste0(Temper_Fo_Grd,"/",
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
      Sinlge_Rst_Name <- paste0(Temper_Fo_Grd,"/",
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
  if(file.exists(paste0(Temper_Fo_Rst,"/","T_",k,"_",Month_List[k],".grd"))){
    print("Raster exist...")
  } else{
    crs(my_raster) <- CRS("+init=epsg:31467")
    writeRaster(my_raster,paste0(Temper_Fo_Rst,"/","T_",k,"_",Month_List[k]))
    print(paste0("T_",k,"_",Month_List[k], " raster saved..."))
  }
  
  #-> Clean the workspace
  rm(Raster_name,tmp_dir,my_raster,k,l,Years_tmp)
}

################################################################################
####################### Median per year ########################################
################################################################################
# Get all the names of the first year
YearLayerNames <- names(T_1_January)

# Loop to calculate the median per Year excluding 2023
for(i in 1:(length(YearLayerNames)-1)){
  
  raster_stack <- stack(T_1_January[[which(names(T_1_January) == YearLayerNames[i])]],
                        T_2_February[[which(names(T_2_February) == YearLayerNames[i])]],
                        T_3_March[[which(names(T_3_March) == YearLayerNames[i])]],
                        T_4_April[[which(names(T_4_April) == YearLayerNames[i])]],
                        T_5_May[[which(names(T_5_May) == YearLayerNames[i])]],
                        T_6_June[[which(names(T_6_June) == YearLayerNames[i])]],
                        T_7_July[[which(names(T_7_July) == YearLayerNames[i])]],
                        T_8_August[[which(names(T_8_August) == YearLayerNames[i])]],
                        T_9_September[[which(names(T_9_September) == YearLayerNames[i])]],
                        T_10_October[[which(names(T_10_October) == YearLayerNames[i])]],
                        T_11_November[[which(names(T_11_November) == YearLayerNames[i])]],
                        T_12_December[[which(names(T_12_December) == YearLayerNames[i])]])
  
  print(paste0("Averaging: ",  YearLayerNames[i]))
  
  median_raster <- overlay(raster_stack, fun = median)
  
  names(median_raster) <- YearLayerNames[i]
  
  #Stack mod raster
  if(i <= 1){
    
    FinalRaster <<- stack(median_raster)
    
  } else 
    FinalRaster <<- stack(FinalRaster, median_raster)
  
}

# Create the median of All times (1881 - 2022)
BaseRaster <- overlay(FinalRaster, fun = median)

# Duplicate the FinalRaster Stack to extract the BaseRaster values
FinalRaster2 <- FinalRaster

# Loop through all rasters to extract the base line
for(i in 1:nlayers(FinalRaster2)){
  
  Name <- names(FinalRaster2[[i]])
  
  FinalRaster2[[i]] <- FinalRaster2[[i]] - BaseRaster
  
  names(FinalRaster2[[i]]) <- Name
  
}

# Create Auxilary Data frame to store the values for the Scatter plot
AuxDF <- data.frame(
  "Year" = YearLayerNames,
  "MinAv" = NA,
  "MaxAv"= NA,
  "MedAv"= NA,
  "Min" = NA,
  "Max"= NA,
  "Med"= NA
)

AuxDF <- AuxDF[1:142,]

AuxDF$Year <- as.numeric(AuxDF$Year)

# Loop through all the data to extract the values and fill the DF
for(i in 1:length(AuxDF$Year)){
  test_spdf <- as(FinalRaster2[[i]], "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  
  AuxDF$MinAv[i] <- min(test_df[,1], na.rm = T)
  AuxDF$MaxAv[i] <- max(test_df[,1], na.rm = T)
  AuxDF$MedAv[i] <- median(test_df[,1], na.rm = T)
  
  test_spdf <- as(FinalRaster[[i]], "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  
  AuxDF$Min[i] <- min(test_df[,1], na.rm = T)
  AuxDF$Max[i] <- max(test_df[,1], na.rm = T)
  AuxDF$Med[i] <- median(test_df[,1], na.rm = T)
  
  AuxDF$Year[i] <- str_split(AuxDF$Year[i],"_")[[1]][2] %>% as.numeric()
  
}
################################################################################
############################# Functions ########################################
################################################################################
ToDF <- function(Index){
  test_spdf <- as(FinalRaster2[[Index]], "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  
  return(test_df)
}
################################################################################
########################### Plot Set-up ########################################
################################################################################
# Declare new theme
USTheme <- function(){ 
  theme(
    #axis.text.x = element_text(angle = 90),
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  )
}

# Create vector with set of color pallet
Colours = c("#e5eeffff",
           "#9ab1d7ff",
           "#294e80ff",
           "#2b7593ff",
           "#759387ff",
           "#bfa96fff",
           "#b8925eff",
           "#b27a54ff",
           "#a64c4eff",
           "#9e204aff",
           "#460f28ff")

# Get Germany shape
Germany <- getData("GADM", country="DEU", level=1) %>%
  spTransform(CRS=CRS("+init=epsg:31467")) %>% st_as_sf()

# Create smooth plot and extract the values
ScatDat <- ggplot(AuxDF, aes(x=Year, y=MedAv)) +  geom_smooth()
ScatDat <- ggplot_build(ScatDat)$data[[1]]
################################################################################
############################## Map-Plot ########################################
################################################################################
MakeMap <- function(ID){
  
  # Select dataset
  DatMap <- ToDF(ID)
  
  # Plot dataset
  Map <-  ggplot() +  
    geom_tile(data=DatMap, aes(x=x, y=y, fill=DatMap[,1])) + 
    geom_sf(data=Germany,fill=NA, color="white", size=0.25) +
    scale_fill_gradientn(colours = Colours,
                         limits=c(-4, 4)) +
    coord_sf() +
    theme_map() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15),
      plot.subtitle=element_text(hjust = 0.5, size = 10),
      legend.position = c(0.95, 0.25),
      legend.justification = "center",
      legend.box="vertical",
      legend.direction = "vertical", 
      legend.title = element_text(hjust = 0.5),
      legend.title.align = 0.5,
      legend.background = element_rect(fill=NA),
      legend.key.height = unit(1.5,"line"),
      legend.key.width = unit(0.5,"line"))+
    guides(fill = guide_colourbar(title.position="top",
                                  title.hjust = 0.5))+
    labs(fill= "°C") 
  
  
  return(Map)
  
}

MakeMap(10)
################################################################################
############################# Scat-Plot ########################################
################################################################################

MakeScat <- function(ID){
  # Create Main scatter plot
  Scat <- ggplot(data=AuxDF, aes(Year, MedAv, group = 1)) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept = AuxDF$Year[ID])+
    geom_line(alpha = .1) +
    geom_smooth(se=F, color="black",size = 0.5, alpha = .2) +
    coord_cartesian(ylim=c(-3,3)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0), limits = c(1881,2022)) +
    geom_ribbon(data=ScatDat, aes(x=x, y=y, ymax = pmax(y, 0), ymin = 0), fill = "#a64c4eff", alpha = 0.5)+
    geom_ribbon(data=ScatDat, aes(x=x, y=y,ymin = pmin(y, 0), ymax = 0), fill = "#294e80ff", alpha = 0.5) +
    labs(x = "Years", 
         y = "Temperature anomaly in °C", 
         caption = "Department of Data Science | Urbansens 2023\n Data Source: ftp://opendata.dwd.de/\n Antonio Castañeda") +
    USTheme()
  
  return(Scat)
}

MakeScat(10)
################################################################################
############################# Make Grid ########################################
################################################################################

MakeGrid <- function(ID){
  GridPlot <- egg::ggarrange(plots = list(MakeMap(ID), MakeScat(ID)), nrow = 1, ncol = 2, widths = c(1,2))
  Text <- paste0("Germany temperature anomaly year: ", stringr::str_split(YearLayerNames[ID],"_")[[1]][2], "\nCompared to median value between 1881 - 2022\n")
  annotate_figure(GridPlot, top=Text)
  
  return(annotate_figure(GridPlot, top=Text))
  
}

MakeGrid(40)

#AuxDF$Year[64]
#AuxDF$Year[78] "Kohlekrise??"
#AuxDF$Year[101]
# ggsave(filename = paste0(i,".png"),plot = ScatPlot,path = "D:/Germany_TP_infographic_MB2/Data/2.Data/4.RasterPerYear")

################################################################################
####################### Model temperature ######################################
################################################################################
# Create New Raster stack to modify
SmoothRaster <- FinalRaster

# Loop to change the pixel value in all raster layers for the "smooth" one
for(x in 1:nrow(SmoothRaster)){
  for(y in 1:ncol(SmoothRaster)){
     
    if(!is.na(SmoothRaster[[1]][x,y])){
      #print(SmoothRaster[[1]][x,y])
      
      # Create an empty data frame to fill all the values from the time series
      NormDF <- data.frame(
        "Year" = rep(NA,nlayers(SmoothRaster)),
        "RastVal" = NA,
        "RastPred" = NA
      )
      
      # Loop over all the layers of the stack to get the values
      for(i in 1:nlayers(SmoothRaster)){
        NormDF$Year[i] <- stringr::str_split(names(SmoothRaster[[i]]),"_")[[1]][2] %>% as.numeric()
        NormDF$RastVal[i] <- SmoothRaster[[i]][x,y]
      }
      
      # Create the regression for the set of data
      model <- lm(NormDF$RastVal ~ poly(NormDF$Year,4))
      
      # Assign the new values to the dataframe
      NormDF$RastPred <- predict(model,data.frame(x= NormDF$Year),interval='confidence',level=0.99)[,1]
      
      # Replace value of pixel in each layer
      for(i in nlayers(SmoothRaster)){
        SmoothRaster[[i]][x,y] <- NormDF$RastPred[i]
      }
    }
  }
}

  




