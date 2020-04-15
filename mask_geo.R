### for climate

setwd("C:/Users/FRota/Desktop/topoclim/climate/")

library(raster)

d<-dir("models_past/", full.names=T)

for(i in 1:length(d)){
  setwd(d[i])
  d2<-dir(".", pattern=glob2rx("*bin*.tif"))
  df <- stack(d2)
  geo<- raster("../../../geology/geology_endemic_dolo.tif")
  df_geo <- stack(mask(df, geo))
  writeRaster(df_geo, filename=paste0(names(df_geo),"_geo.tif"), file.path(d[i]), bylayer=TRUE,format="GTiff", overwrite=TRUE)
  setwd("..")
  setwd("..")
}


### for topography
setwd("C:/Users/FRota/Desktop/topoclim/topography/")

library(raster)

d<-dir("models/", full.names=T)

for(i in 1:length(d)){
  setwd(d[i])
  d2<-dir(".", patter=".tif")
  df <- stack(d2)
  geo <- raster("../../../geology/geology_endemic_dolo.tif")
  df_geo <- stack(mask(df, geo))
  writeRaster(df_geo, filename=paste0(names(df_geo),"_geo.tif"), file.path(d[i]), bylayer=TRUE,format="GTiff", overwrite=TRUE)
  setwd("..")
  setwd("..")
}

