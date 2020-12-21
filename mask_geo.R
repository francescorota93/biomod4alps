### Launch Mask Geo

setwd("/data/models/")
#library(foreach)

library(raster)
#library(doParallel)
#source("biomod4alps/mask_geo_function.R")

d<-dir("bin/", full.names=T)
geo<- raster("geology/geology_endemic_dolo_all.tif")
s <- read.table("endemic_dolo50.txt", head = TRUE, sep = "\t")

species <- c(1:8) ### numero di specie c(1:8)

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

# ### made loop for each species
# 
# lapply(1:8, mask_geo, s, geo)


