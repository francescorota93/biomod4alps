setwd("/home/lv71284/frota/data/")
library(raster)
library(parallel)
library(doParallel)
library(foreach)

sp<-read.table("endemic_dolo50.txt", sep="\t", h=T) ### load spec occurrence file
spek <- sub(pattern = "_", replacement = "\\.", x = as.character(unique(sp$species)))
spek1 <- sub(pattern = "_", replacement = "\\_", x = as.character(unique(sp$species)))
work_dir <- paste0(getwd(), "/habitat_model/")
setwd(work_dir)

registerDoParallel(cores = 8)
## try for one species


# foreach(i = 1:8) %dopar% {
# 
# # differenza percentuale
# pr_mean <- raster(paste0(spek[i], "_mean_present.tif"))
# fut45_mean <- raster(paste0(spek[i], "_mean_fut45.tif"))
# fut85_mean <- raster(paste0(spek[i], "_mean_fut85.tif"))
# dif_45pres <- ((fut45_mean - pr_mean)/((fut45_mean + pr_mean)/2))
# #plot(dif_45pres)
# dif_85pres <- ((fut85_mean - pr_mean)/((fut85_mean + pr_mean)/2))
# #plot(dif_85pres)
# 
# writeRaster(dif_45pres, paste0(work_dir, spek[i], "_dif45.tif"), overwrite=TRUE)
# writeRaster(dif_85pres, paste0(work_dir, spek[i], "_dif85.tif"), overwrite=TRUE)
# }

foreach(i = 1:8) %dopar% {
  
  # differenza
  pr_mean <- raster(paste0(spek[i], "_mean_present.tif"))
  fut45_mean <- raster(paste0(spek[i], "_mean_fut45.tif"))
  fut85_mean <- raster(paste0(spek[i], "_mean_fut85.tif"))
  dif_45pres <- fut45_mean - pr_mean
  #plot(dif_45pres)
  dif_85pres <- fut85_mean - pr_mean
  #plot(dif_85pres)
  
  writeRaster(dif_45pres, paste0(work_dir, spek[i], "_dif45_int.tif"), overwrite=TRUE)
  writeRaster(dif_85pres, paste0(work_dir, spek[i], "_dif85_int.tif"), overwrite=TRUE)
}
