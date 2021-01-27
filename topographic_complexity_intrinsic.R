setwd("/data/models/env_predictors/")
library(raster)
dtm50 <- raster("DTM_50.tif")
dtm10 <- raster("dtm_10m/dtm10m_dolomites.tif")

### fai calcolo dei 10 m, poi aggregate con sum e factor = 5, e poi fai diviso area raster a 50 m
slope10 <- terrain(dtm10, opt = "slope", neighbors=8, unit = "degrees")
area_dem <- dtm10
area_dem[area_dem] <- 100
cos_slop <- cos(slope10*pi/180)

dt_3d <- area_dem/cos_slop

area3d_10m <- aggregate(dt_3d, fact = 5, fun = sum)
topo_intr <- area3d_10m/2500
plot(topo_intr)
writeRaster(topo_intr, file = "intrinsic_topographic_complexity.tif")
