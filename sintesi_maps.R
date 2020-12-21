setwd("/data/models/maps/") 
library(raster)
library(dplyr)
library(tidyr)

#name.list <- unlist(strsplit(dir(".", pattern = "*reclass.tif" ) ,split = "_"))
#sp_names <- paste0(name.list[1], "_", name.list[2])
sp1<-read.table("../endemic_dolo50.txt", sep="\t", h=T) ### load spec occurrence file
spek <- sub(pattern = "_", replacement = "\\_", x = as.character(unique(sp1$species)))
sp <- list.files(".", pattern = "*reclass.tif" )
for(i in 1:length(spek)){
  #sp <- name.list[grep(spek[i], name.list)]
  sp_pres <- raster(sp[grep(paste0(spek[i],"_present"), sp)])
  sp_opt <- raster(sp[grep(paste0(spek[i],"_optimistic"), sp)])
  sp_pes <- raster(sp[grep(paste0(spek[i],"_pessimistic"), sp)])
  opt_map <- sum(sp_pres, sp_opt)
  pes_map <- sum(sp_pres, sp_pes)
  writeRaster(opt_map, paste0(spek[i], "_optimistic_sintesi.tif"))
  writeRaster(pes_map, paste0(spek[i], "_pessimistic_sintesi.tif"))
}

### plot sintesi stack

sint <- list.files(".", pattern = "*sintesi.tif" )
#for(i in 1:length(spek)){
  #sp <- name.list[grep(spek[i], name.list)]

plot(stack(sint[1:2]))



