#install.packages("biscale")
#remotes::install_github("slu-openGIS/biscale")
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
Main_Fo <- "C:\\Users\\Cowboybebop\\Documents\\EAGLE\\I_SEMESTER\\Introdution_to_Progrmming_and_Geostatistics\\Final_Task"#choose.dir()

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
######################################  Temperature Data ####################################################
#############################################################################################################

#-> Read *.gri path name and reblace / with // 
GridFilesTemp <- paste0(grep("*.grd*", list.files(path = Temper_Fo_Grd, pattern="*.grd$", full.names = TRUE), value=T))
GridFilesTemp <- str_replace_all(string = GridFilesTemp, pattern = "/", replacement = "//" )

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

Rst_Pre_Stk <- lapply(GridFilesPreci,raster) 
Rst_Pre_Stk <- stack(Rst_Pre_Stk)
Rst_Pre_Mean <- stackApply(Rst_Pre_Stk, indices =  rep(1,nlayers(Rst_Pre_Stk)), fun = "mean", na.rm = T)

#############################################################################################################
##################################### Germany Boundaries ####################################################
#############################################################################################################
#-> Create Temperature and precipitation Stack
#Stack <- stack(Rst_Tem_Mean,Rst_Pre_Mean)

#-> Get Shape of germany
Germany_1 <- getData("GADM", country="DEU", level=1, path=Shapes_Fo)
Germany_2 <- getData("GADM", country="DEU", level=2, path=Shapes_Fo)
Germany_3 <- getData("GADM", country="DEU", level=3, path=Shapes_Fo)
Germany_4 <- getData("GADM", country="DEU", level=4, path=Shapes_Fo)

#-> Add coordinate system
Germany_1 <- spTransform(Germany_1,CRS=CRS("+init=epsg:31467"))
Germany_2 <- spTransform(Germany_2,CRS=CRS("+init=epsg:31467"))
Germany_3 <- spTransform(Germany_3,CRS=CRS("+init=epsg:31467"))
Germany_4 <- spTransform(Germany_4,CRS=CRS("+init=epsg:31467"))

#-> Only Name_*
Germany_1 <- Germany_1[,(c(4))]
Germany_2 <- Germany_2[,(c(7))]
Germany_3 <- Germany_3[,(c(10))]
Germany_4 <- Germany_4[,(c(10))]

#-> Create ID
Germany_1$ID <- seq(1,length(Germany_1$NAME_1))
Germany_2$ID <- seq(1,length(Germany_2$NAME_2))
Germany_3$ID <- seq(1,length(Germany_3$NAME_3))
Germany_4$ID <- seq(1,length(Germany_4$NAME_4))

#-> Extract the Mean Temperature by regions
Zon_Mean_Temp_1 <- extract(Rst_Tem_Mean, Germany_1, fun=mean, na.rm=TRUE, df=TRUE)
Zon_Mean_Temp_2 <- extract(Rst_Tem_Mean, Germany_2, fun=mean, na.rm=TRUE, df=TRUE)
Zon_Mean_Temp_3 <- extract(Rst_Tem_Mean, Germany_3, fun=mean, na.rm=TRUE, df=TRUE)
Zon_Mean_Temp_4 <- extract(Rst_Tem_Mean, Germany_4, fun=mean, na.rm=TRUE, df=TRUE)

#-> Extract the Mean Precipitation by regions
Zon_Mean_Preci_1 <- extract(Rst_Pre_Mean, Germany_1, fun=mean, na.rm=TRUE, df=TRUE)
Zon_Mean_Preci_2 <- extract(Rst_Pre_Mean, Germany_2, fun=mean, na.rm=TRUE, df=TRUE)
Zon_Mean_Preci_3 <- extract(Rst_Pre_Mean, Germany_3, fun=mean, na.rm=TRUE, df=TRUE)
Zon_Mean_Preci_4 <- extract(Rst_Pre_Mean, Germany_4, fun=mean, na.rm=TRUE, df=TRUE)

#-> Create fields in SP object
Germany_1$Temp_Mean <- 0
Germany_1$Preci_Mean <- 0
Germany_2$Temp_Mean <- 0
Germany_2$Preci_Mean <- 0
Germany_3$Temp_Mean <- 0
Germany_3$Preci_Mean <- 0
Germany_4$Temp_Mean <- 0
Germany_4$Preci_Mean <- 0

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
########################################## Bivariate Map ####################################################
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

#-> Check classes
unique(Bivariate_1$bi_class)
unique(Bivariate_2$bi_class)
unique(Bivariate_3$bi_class)
unique(Bivariate_4$bi_class)

#-> Delete "NA-NA" values
Bivariate_3 <- Bivariate_3[!grepl("NA-NA", Bivariate_3$bi_class),]
Bivariate_4 <- Bivariate_4[!grepl("NA-NA", Bivariate_4$bi_class),]

#-> Bivariate map theme
BivTheme <- theme(panel.background = element_blank(),
               plot.background = element_blank(),
               panel.grid = element_line(colour="white", size=1),
               plot.title = element_text(size = 130, color= "white"),
               axis.text = element_text(size = 50, color= "white")) 

#-> Bivariate ggplot level_1
Biv_Map_1 <- ggplot() +
  #Title
  ggtitle(bquote(bold('Bivariate') ~ 'mean values by region L1')) +
  #Geometry
  geom_sf(data = Bivariate_1, mapping = aes(fill = bi_class),
          color = "#20231fff", size = 5,
          show.legend = FALSE) +
  #Scale
  bi_scale_fill(pal = "GrPink", dim = 3) +
  #Theme
  BivTheme

#-> Bivariate ggplot level_2
Biv_Map_2 <- ggplot() +
  #Title
  ggtitle(bquote(bold('Bivariate') ~ 'mean values by region L2')) +
  #Geometry
  geom_sf(data = Bivariate_2, mapping = aes(fill = bi_class),
          color = "#20231fff", size = 1,
          show.legend = FALSE) +
  #Scale
  bi_scale_fill(pal = "GrPink", dim = 3) +
  #Theme
  BivTheme

#-> Bivariate ggplot level_3
Biv_Map_3 <- ggplot() +
  #Title
  ggtitle(bquote(bold('Bivariate') ~ 'mean values by region L3')) +
  #Geometry
  geom_sf(data = Bivariate_3, mapping = aes(fill = bi_class),
          color = "#20231fff", size = 0.05,
          show.legend = FALSE) +
  #Scale
  bi_scale_fill(pal = "GrPink", dim = 3) +
  #Theme
  BivTheme

#-> Bivariate ggplot level_4
Biv_Map_4 <- ggplot() +
  #Title
  ggtitle(bquote(bold('Bivariate') ~ 'mean values by region L4')) +
  #Geometry
  geom_sf(data = Bivariate_4, mapping = aes(fill = bi_class),
          color = "#20231fff", size = 0.001,
          show.legend = FALSE) +
  #Scale
  bi_scale_fill(pal = "GrPink", dim = 3) +
  #Theme
  BivTheme

#-> Change directory and Export
setwd(QGI_Fo)
ggsave("BivMap_L1.png",Biv_Map_1, width=60, height=70, limitsize = FALSE, bg = "transparent")
ggsave("BivMap_L2.png",Biv_Map_2, width=60, height=70, limitsize = FALSE, bg = "transparent")
ggsave("BivMap_L3.png",Biv_Map_3, width=60, height=70, limitsize = FALSE, bg = "transparent")
ggsave("BivMap_L4.png",Biv_Map_4, width=60, height=70, limitsize = FALSE, bg = "transparent")

#legend <- bi_legend(pal = "GrPink",
#                    dim = 3,
#                    xlab = "Mean Temperature ",
#                    ylab = "Mean Precipitation",
#                    size = 80)

#FinalPlot <- ggdraw() +
#  draw_plot(map, 0, 0, 1, 1) +
#  draw_plot(legend, 0, 0, 0.2, 0.2)



