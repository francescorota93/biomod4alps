setwd("/data/models/mean_LGM")
library(raster)
library(foreach)
library(doParallel)
#library(terra)

### decide if insert glacial maximum rasterized
#ice<- raster("LGM/.tif")  ### load LGM glaciers extension file (1,0)

### made loop
d<-dir(".", full.names=TRUE)
spek <- c("Campanula.morettiana", "Primula.tyrolensis", "Rhizobotrya.alpina", "Saxifraga.facchinii")
s_mean<-list.files(path= ".", pattern="*\\mean", recursive=F, full.names= TRUE)
#s_sd<-list.files(path= ".", pattern="*\\sd", recursive=F, full.names= TRUE)

registerDoParallel(cl <- makeCluster(4))
foreach(i = 1:length(spek), .packages = "raster") %dopar% {
#i=1
 s_mean[grepl(spek[i], s_mean)]
 #s_sd[grepl(spek[i], s_sd)]
 print(s_mean)
 s1<-stack(s_mean)
 sc <- calc(s1, fun = mean, na.rm = TRUE)
 sd1 <- calc(s1, fun = sd, na.rm = TRUE)

 name1 <- paste0(spek[i],"LGM_mean.tif")
 name2 <- paste0(spek[i],"LGM_sd.tif")

 writeRaster(sc, paste0(out_dir, name1), overwrite=TRUE)
 writeRaster(sd1, paste0(out_dir, name2), overwrite=TRUE)
 setwd("..")

 }

