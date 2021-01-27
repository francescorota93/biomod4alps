setwd("/data/models/env_predictors/dtm_10m")
library(raster)
library(rgdal)
library(gdalUtils)
all <- list.files(".", pattern = "extent")
ex1 <- raster("extent10m_1.tif")
ex_A <- raster("cut_austria.tif")
re <- resample(ex_A, ex1, method = "bilinear")
ex_a_crop <- crop(re, ex1)
it <- stack(all)
all.1 <- stack(it, ex_a_crop)

dtm10 <- stackApply(all.1, indices = rep(1, nlayers(all.1)), fun = max)
writeRaster(dtm10, file="dtm10m_dolomites.tif", format="GTiff", overwrite=TRUE)


# dtm10 <- overlay(all.1, fun = mean)
# writeRaster(dtm10, file="dtm10m_dolomites.tif", format="GTiff", overwrite=TRUE)
# 
# 
# e <- extent(ex1)
# template <- raster(e)
# projection(template) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" 
# writeRaster(template, file="dtm10m_dolomites.tif", format="GTiff")
# 
# mosaic_rasters(gdalfile=all, dst_dataset="dtm10m_dolomites.tif",of="GTiff")
