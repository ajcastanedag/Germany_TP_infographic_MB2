#############################################################################################################
######################################## LIBRARIES ##########################################################
#############################################################################################################
library(magick)
#############################################################################################################
##################################### FOLDER DECLARATION ####################################################
#############################################################################################################
### Choose Main Folder
Main_Fo <- choose.dir()
setwd(Main_Fo)

### Declare Folders
Script_Fo <- paste0(Main_Fo,"\\1.Script")
Data_Fo <- paste0(Main_Fo,"\\2.Data")
Grids_Fo <- paste0(Main_Fo,"\\3.Grids")
Raster_Fo <- paste0(Main_Fo,"\\4.Raster")
Graphs_Fo <- paste0(Main_Fo,"\\5.Graphs")

### Create SubFolders
Temper_Fo_Or <- paste0(Data_Fo,"\\1.Temperature")
Precip_Fo_Or <-  paste0(Data_Fo,"\\2.Precipitation")

Temper_Fo_Grd <- paste0(Grids_Fo,"\\1.Temperature")
Precip_Fo_Grd <-  paste0(Grids_Fo,"\\2.Precipitation")

Temper_Fo_Rst <- paste0(Raster_Fo,"\\1.Temperature")
Precip_Fo_Rst <-  paste0(Raster_Fo,"\\2.Precipitation")

Shapes_Fo <- paste0(Graphs_Fo,"\\1.Shapes")
Circles_Fo <- paste0(Graphs_Fo,"\\2.Circles")
Strip_Fo <- paste0(Graphs_Fo,"\\3.Strips")

#############################################################################################################
######################################### Import Images #####################################################
#############################################################################################################
setwd(Circles_Fo)

Circle_Temperature <- image_read("T_Circle.png")
Circle_Precipitation <- image_read("P_Circle.png")













