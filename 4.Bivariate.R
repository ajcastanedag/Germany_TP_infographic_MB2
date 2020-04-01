#install.packages("biscale")
#remotes::install_github("slu-openGIS/biscale")
library(biscale)
library(sf)
library(raster)
library(ggplot2)
library(RStoolbox)

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
Rst_Tem_Stk <- lapply(GridFilesPreci,raster) 
Rst_Tem_Stk <- stack(Rst_Tem_Stk)
Rst_Tem_Mean <- stackApply(Rst_Tem_Stk, indices =  rep(1,nlayers(Rst_Tem_Stk)), fun = "mean", na.rm = T)

ggR(Rst_Tem_Mean, geom_raster = T)+
labs(x="",y="")+
ggtitle(paste0("Temperature "))+
theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
theme(legend.title = element_text(size = 12, face = "bold"))+
theme(legend.text = element_text(size = 10))+
theme(axis.text.y = element_text(angle=90, vjust=0.5,hjust=0.5))+
scale_y_continuous(breaks = seq(5400000,6000000,200000))+
xlab("")+
ylab("")


#############################################################################################################
##################################### Precipitation Data ####################################################
#############################################################################################################

#-> Read *.gri path name and reblace / with // 
GridFilesPreci <- paste0(grep("*.grd*", list.files(path = Precip_Fo_Grd, pattern="*.grd$", full.names = TRUE), value=T))
GridFilesPreci <- str_replace_all(string = GridFilesPreci, pattern = "/", replacement = "//" )

Rst_Pre_Stk <- lapply(GridFilesPreci,raster) 
Rst_Pre_Stk <- stack(Rst_Pre_Stk)
Rst_Pre_Mean <- stackApply(Rst_Pre_Stk, indices =  rep(1,nlayers(Rst_Pre_Stk)), fun = "mean", na.rm = T)

ggR(Rst_Pre_Mean, geom_raster = T)+
  labs(x="",y="")+
  ggtitle(paste0("Precipitation "))+
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15))+
  theme(legend.title = element_text(size = 12, face = "bold"))+
  theme(legend.text = element_text(size = 10))+
  theme(axis.text.y = element_text(angle=90, vjust=0.5,hjust=0.5))+
  scale_y_continuous(breaks = seq(5400000,6000000,200000))+
  xlab("")+
  ylab("")

#############################################################################################################
##################################### Germany Boundaries ####################################################
#############################################################################################################
#-> Get Shape of germany
Germany <- getData("GADM", country="DEU", level=1, path=Shapes_Fo)
Germany <- spTransform(Germany,CRS=CRS("+init=epsg:31467"))
Germany.data <- data.frame(id=rownames(Germany@data), Germany@data)

Germany.df   <- fortify(Germany)
Germany.df   <- merge(Germany.df,Germany.data, by="id",group=NAME_1)

ggplot(Germany.df, aes(x=long, y=lat, group=group))+
  geom_polygon()+
  geom_path(colour="black")+
  theme(axis.text=element_blank())+
  coord_fixed() +
  scale_colour_manual(values = c("red", "blue", "green"))









