mask_geo <- function(geo, s, df) {
  sp.names<-levels(factor(s[,1]))
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