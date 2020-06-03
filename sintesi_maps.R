setwd("/data/models/maps/") 
library(raster)
library(dplyr)
library(tidyr)

name.list <- unlist(strsplit(dir(., pattern = "*reclass.tif" ) ,split = "_"))
sp_names <- unique(paste0(name.list[1], "_", name.list[2]))

for(i in sp_names){
  sp <- name.list[grep(i, name.list)]
  sp_pres <- raster(sp[grep("present", sp)])
  sp_opt <- raster(sp[grep("optimistic", sp)])
  sp_pes <- raster(sp[grep("pessimistic", sp)])
  opt_map <- sum(sp_pres, sp_opt)
  pes_map <- sum(sp_pres, sp_pes)
  writeRaster(opt_map, paste0(i, "_optimistic_sintesi.tif"))
  writeRaster(pes_map, paste0(i, "_pessimistic_sintesi.tif"))
}
