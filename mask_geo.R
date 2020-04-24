### for climate

setwd("/data/models/")

library(raster)

d<-dir("bin/", full.names=T)
geo<- raster("geology/geology_endemic_dolo_all.tif")
s <- read.table("endemic_dolo50.txt", head = TRUE, sep = "\t")
species <- c(3,4) ### numero di specie c(1:8)
for(i in species){
  sp.names<-levels(factor(s[,1]))[i]
  d2<-list.files("bin/", pattern = sp.names, full.names = TRUE)
  df <- stack(d2)
  t2 <- df@extent
  cn  <- crop(geo, t2)
  df_geo <- mask(df, cn)
  if(!dir.exists("bin_geo")){
    dir.create("bin_geo")
  }
  writeRaster(df_geo, filename=paste0("bin_geo/",names(df_geo),"_geo.tif"), bylayer=TRUE,format="GTiff", overwrite=TRUE)
}


