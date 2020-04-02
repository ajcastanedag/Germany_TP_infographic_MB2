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
Germany <- getData("GADM", country="DEU", level=1, path=Shapes_Fo)
Germany <- spTransform(Germany,CRS=CRS("+init=epsg:31467"))

#-> Only Name_1 and CC_1 needed
Germany <- Germany[,-(c(1,2,3,5,6,7,8,10))]

#-> Extract the Mean Temperature by regions
Zon_Mean_Temp <- extract(Rst_Tem_Mean, Germany, fun=mean, na.rm=TRUE, df=TRUE)

#-> Extract the Mean Temperature by regions
Zon_Mean_Preci <- extract(Rst_Pre_Mean, Germany, fun=mean, na.rm=TRUE, df=TRUE)

#-> Create fields in SP object
Germany$Temp_Mean <- 0
Germany$Preci_Mean <- 0

#-> Assign values from statistical Zone 
for(i in 1:length(Germany$CC_1)){
  for(j in 1: length(Germany$Temp_Mean)){
    if(as.numeric(Germany$CC_1[i]) == Zon_Mean_Temp$ID[j]){
      Germany$Temp_Mean[i] <- Zon_Mean_Temp$index_1[j]
    }
  }
  for(k in 1: length(Germany$Preci_Mean)){
    if(as.numeric(Germany$CC_1[i]) == Zon_Mean_Preci$ID[k]){
      Germany$Preci_Mean[i] <- Zon_Mean_Preci$index_1[i]
    }
  }
}

#-> PLot Temperature 
Temp.palette <- rev(brewer.pal(n = 11, name = "RdYlBu"))
spplot(Germany, "Temp_Mean",
       main = "Mean Temperature",
       col.regions = Temp.palette,
       cuts = 10)

#-> PLot Precipitation 
Preci.palette <- brewer.pal(n = 11, name = "BrBG")
spplot(Germany, "Preci_Mean",
       main = "Mean Precipitation",
       col.regions = Preci.palette,
       cuts = 10)

#-> Convert Shape to SF object
Germany <- st_as_sf(Germany)

#############################################################################################################
########################################## Bivariate Map ####################################################
#############################################################################################################
Bivariate <- bi_class(Germany, x = Temp_Mean, y = Preci_Mean, style = "quantile", dim = 3)

BivTheme <- theme(panel.background = element_blank(),
               plot.background = element_blank(),
               panel.grid = element_line(colour="white", size=1),
               plot.title = element_text(size = 130, color= "white"),
               axis.text = element_text(size = 50, color= "white")) 

map <- ggplot() +
  #Title
  ggtitle(bquote(bold('Bivariate') ~ 'mean values by region')) +
  geom_sf(data = Bivariate, mapping = aes(fill = bi_class),
          color = "#20231fff", size = 5,
          show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  BivTheme
  
map

#legend <- bi_legend(pal = "GrPink",
#                    dim = 3,
#                    xlab = "Mean Temperature ",
#                    ylab = "Mean Precipitation",
#                    size = 80)

#FinalPlot <- ggdraw() +
#  draw_plot(map, 0, 0, 1, 1) +
#  draw_plot(legend, 0, 0, 0.2, 0.2)

setwd(QGI_Fo)

ggsave("Bivariate.png",map, width=60, height=70, limitsize = FALSE, bg = "transparent")

