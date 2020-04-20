#############################################################################################################
######################################## OBJECTIVE ##########################################################
#############################################################################################################
# Fourth part on a series of 4 codes to build an infographic to portray the variation of temperature and
# precipitation in Germany since 1881. The data is downloaded from ftp://opendata.dwd.de/ and this code is 
# developed as the final task of the Introduction to Programming and Geostatistics - MB2 class. This fourth
# code aims to build bivariate maps of Germany realting the temperature and precipitation mean values. This
# code relies on the first code or in the .csv files provided in the git repository. 
#
# Code made by: Antonio José Castañeda Gómez (s386454)
# Email: antonio.castanedag@gmail.com
#
#############################################################################################################
######################################## LIBRARIES ##########################################################
#############################################################################################################
library(biscale)
library(sf)
library(raster)
library(ggplot2)
library(RStoolbox)
library(stringr)
library(RColorBrewer)
library(ggplot2)
library(cowplot)
#############################################################################################################
##################################### FOLDER DECLARATION ####################################################
#############################################################################################################
### Choose Main Folder
Main_Fo <- choose.dir()
setwd(Main_Fo)

### Declare Folders
Data_Fo <- paste0(Main_Fo,"\\2.Data")
Grids_Fo <- paste0(Main_Fo,"\\3.Grids")
Raster_Fo <- paste0(Main_Fo,"\\4.Raster")
Graphs_Fo <- paste0(Main_Fo,"\\5.Graphs")

### SubFolders
Temper_Fo_Or <- paste0(Data_Fo,"\\1.Temperature")
Precip_Fo_Or <-  paste0(Data_Fo,"\\2.Precipitation")

Temper_Fo_Grd <- paste0(Grids_Fo,"\\1.Temperature")
Precip_Fo_Grd <-  paste0(Grids_Fo,"\\2.Precipitation")

Temper_Fo_Rst <- paste0(Raster_Fo,"\\1.Temperature")
Precip_Fo_Rst <-  paste0(Raster_Fo,"\\2.Precipitation")

Bivariate_Fo <- paste0(Graphs_Fo,"\\4.Bivariate")

#############################################################################################################
######################################  Temperature Data ####################################################
#############################################################################################################
#-> Read *.gri path name and reblace / with // 
GridFilesTemp <- paste0(grep("*.grd*", list.files(path = Temper_Fo_Grd, pattern="*.grd$", full.names = TRUE), value=T))
GridFilesTemp <- str_replace_all(string = GridFilesTemp, pattern = "/", replacement = "//" )

#-> Select values ONLY if there is data for the whole year
while(length(GridFilesTemp) %% 12 != 0){
  GridFilesTemp <- GridFilesTemp[-length(GridFilesTemp)]
}

#-> Create Stack of all grid files
Rst_Tem_Stk <- lapply(GridFilesTemp,raster) 
Rst_Tem_Stk <- stack(Rst_Tem_Stk)
Rst_Tem_Mean <- stackApply(Rst_Tem_Stk, indices =  rep(1,nlayers(Rst_Tem_Stk)), fun = "mean", na.rm = T)

#############################################################################################################
##################################### Precipitation Data ####################################################
#############################################################################################################
#-> Read *.gri path name and reblace / with // 
GridFilesPreci <- paste0(grep("*.grd*", list.files(path = Precip_Fo_Grd, pattern="*.grd$", full.names = TRUE), value=T))
GridFilesPreci <- str_replace_all(string = GridFilesPreci, pattern = "/", replacement = "//" )

#-> Select values ONLY if there is data for the whole year
while(length(GridFilesPreci) %% 12 != 0){
  GridFilesPreci <- GridFilesPreci[-length(GridFilesPreci)]
}

Rst_Pre_Stk <- lapply(GridFilesPreci,raster) 
Rst_Pre_Stk <- stack(Rst_Pre_Stk)
Rst_Pre_Mean <- stackApply(Rst_Pre_Stk, indices =  rep(1,nlayers(Rst_Pre_Stk)), fun = "mean", na.rm = T)

#############################################################################################################
##################################### Germany Boundaries ####################################################
#############################################################################################################
#-> Get Shape of germany and assign coordinate system, delete all values different from Name_* and
#   create the zonal statistics for temperature and precipitation 
for(i in 1:4){
  #-> Create the name for the shape to Download
  Name <- paste0("Germany_", i)
  #-> Create the name for the temperature stat zone
  Name_Tmp <- paste0("Zon_Mean_Temp_", i)
  #-> Create the name for the temperature stat zone
  Name_Prec <- paste0("Zon_Mean_Preci_", i)
  #-> Download data
  Data <- getData("GADM", country="DEU", level=i, path=Shapes_Fo)
  #-> Assign coordinate system
  Data <- spTransform(Data, CRS=CRS("+init=epsg:31467"))
  #-> Delet all columns different from Name_i
  Data <-  Data[,(c(match(paste0("NAME_",i),names(Data))))]
  #-> Create ID field for values
  Data$ID <- seq(1,length(Data[1]))
  #-> Extract the Mean Temperature by regions
  Zon_Mean_Temp <- extract(Rst_Tem_Mean, Data, fun=mean, na.rm=TRUE, df=TRUE)
  print(paste0("Zon_Mean_Temp created for level ",i))
  #-> Extract the Mean Precipitation by regions
  Zon_Mean_Preci <- extract(Rst_Pre_Mean, Data, fun=mean, na.rm=TRUE, df=TRUE)
  print(paste0("Zon_Mean_Preci created for level ",i))
  #-> Add Temp_Mean field
  Data$Temp_Mean <- 0
  #-> Add Preci_Mean field
  Data$Preci_Mean <- 0
  #-> Assign Data to name
  assign(Name, Data)
  #-> Assign temperature stat zone to Name_Tmp
  assign(Name_Tmp, Zon_Mean_Temp)
  #-> Assign temperature stat zone to Name_Prec
  assign(Name_Prec, Zon_Mean_Preci)
  #-> Remove data from workspace
  rm(Data, Zon_Mean_Temp, Zon_Mean_Preci)
}

#############################################################################################################
########################################### Match Values ####################################################
#############################################################################################################
#-> Assign values from statistical Zone 1
for(i in 1:length(Germany_1$ID)){
  for(j in 1: length(Germany_1$Temp_Mean)){
    if(as.numeric(Germany_1$ID[i]) == Zon_Mean_Temp_1$ID[j]){
      Germany_1$Temp_Mean[i] <- Zon_Mean_Temp_1$index_1[j]
    }
  }
  for(k in 1: length(Germany_1$Preci_Mean)){
    if(as.numeric(Germany_1$ID[i]) == Zon_Mean_Preci_1$ID[k]){
      Germany_1$Preci_Mean[i] <- Zon_Mean_Preci_1$index_1[k]
    }
  }
}

#-> Assign values from statistical Zone 2
for(i in 1:length(Germany_2$ID)){
  for(j in 1: length(Germany_2$Temp_Mean)){
    if(as.numeric(Germany_2$ID[i]) == Zon_Mean_Temp_2$ID[j]){
      Germany_2$Temp_Mean[i] <- Zon_Mean_Temp_2$index_1[j]
    }
  }
  for(k in 1: length(Germany_2$Preci_Mean)){
    if(as.numeric(Germany_2$ID[i]) == Zon_Mean_Preci_2$ID[k]){
      Germany_2$Preci_Mean[i] <- Zon_Mean_Preci_2$index_1[k]
    }
  }
}

#-> Assign values from statistical Zone 3
for(i in 1:length(Germany_3$ID)){
  for(j in 1: length(Germany_3$Temp_Mean)){
    if(as.numeric(Germany_3$ID[i]) == Zon_Mean_Temp_3$ID[j]){
      Germany_3$Temp_Mean[i] <- Zon_Mean_Temp_3$index_1[j]
    }
  }
  for(k in 1: length(Germany_3$Preci_Mean)){
    if(as.numeric(Germany_3$ID[i]) == Zon_Mean_Preci_3$ID[k]){
      Germany_3$Preci_Mean[i] <- Zon_Mean_Preci_3$index_1[k]
    }
  }
  print(paste0("Progress: ",(i*100/4680)))
}

#-> Assign values from statistical Zone 4
for(i in 1:length(Germany_4$ID)){
  for(j in 1: length(Germany_4$Temp_Mean)){
    if(as.numeric(Germany_4$ID[i]) == Zon_Mean_Temp_4$ID[j]){
      Germany_4$Temp_Mean[i] <- Zon_Mean_Temp_4$index_1[j]
    }
  }
  for(k in 1: length(Germany_4$Preci_Mean)){
    if(as.numeric(Germany_4$ID[i]) == Zon_Mean_Preci_4$ID[k]){
      Germany_4$Preci_Mean[i] <- Zon_Mean_Preci_4$index_1[k]
    }
  }
  print(paste0("Progress: ",(i*100/11302)))
}

#-> Convert geometry to SF object
Germany_1 <- st_as_sf(Germany_1)
Germany_2 <- st_as_sf(Germany_2)
Germany_3 <- st_as_sf(Germany_3)
Germany_4 <- st_as_sf(Germany_4)

#############################################################################################################
######################################### Bivariate Data ####################################################
#############################################################################################################
#-> Create Bivariate values
Bivariate_1 <- bi_class(Germany_1, x = Temp_Mean, y = Preci_Mean, style = "quantile", dim = 3)
Bivariate_2 <- bi_class(Germany_2, x = Temp_Mean, y = Preci_Mean, style = "quantile", dim = 3)
Bivariate_3 <- bi_class(Germany_3, x = Temp_Mean, y = Preci_Mean, style = "quantile", dim = 3)
Bivariate_4 <- bi_class(Germany_4, x = Temp_Mean, y = Preci_Mean, style = "quantile", dim = 3)

#-> Save Bivariate Maps as gpkg
setwd(Shapes_Fo)
st_write(Bivariate_1, dsn="Bivariate_1.gpkg", layer='Bivariate')
st_write(Bivariate_2, dsn="Bivariate_2.gpkg", layer='Bivariate')
st_write(Bivariate_3, dsn="Bivariate_3.gpkg", layer='Bivariate')
st_write(Bivariate_4, dsn="Bivariate_4.gpkg", layer='Bivariate')

#-> Delete "NA-NA" values
Bivariate_3 <- Bivariate_3[!grepl("NA-NA", Bivariate_3$bi_class),]
Bivariate_4 <- Bivariate_4[!grepl("NA-NA", Bivariate_4$bi_class),]

#-> Check classes
unique(Bivariate_1$bi_class)
unique(Bivariate_2$bi_class)
unique(Bivariate_3$bi_class)
unique(Bivariate_4$bi_class)

#############################################################################################################
########################################## Bivariate Map ####################################################
#############################################################################################################
#-> Change directory and Export
setwd(Bivariate_Fo)

#-> Bivariate map theme
BivTheme <- theme(panel.background = element_blank(),
               plot.background = element_blank(),
               panel.grid = element_line(colour="white", size=1),
               plot.title = element_text(size = 130, color= "white"),
               axis.text = element_text(size = 50, color= "white")) 

#-> List for produce graphs in loop
Biv_Lis <- list(Bivariate_1, Bivariate_2, Bivariate_3, Bivariate_4)

#-> Ggplot loop and export
for(i in 1:length(Biv_Lis)){
  #-> Define line thinckness
  Size <- c(5,1,0.5,0.001) 
  #-> Create plot
  BivMap <- ggplot() +
    #Title
    ggtitle(paste0('Bivariate mean values by region L', i)) +
    #Geometry
    geom_sf(data = Biv_Lis[[i]], mapping = aes(fill = bi_class),
            color = "#20231fff", size = Size[i],
            show.legend = FALSE) +
    #Scale
    bi_scale_fill(pal = "GrPink", dim = 3) +
    #Theme
    BivTheme
  
  #-> Export Map as .png
  ggsave(paste0("BivMap_L",i,".png"), BivMap, width=60, height=70, limitsize = FALSE, bg = "transparent")
}




#-> Legend
bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Mean Temperature ",
                    ylab = "Mean Precipitation",
                    size = 10)