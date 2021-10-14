setwd("/home/lv71418/frrota/data/") ###setwd("/home/lv71284/frota/data/")
library(raster)
library(foreach)
library(doParallel)
#library(terra)

work_dir <- paste0(getwd(), "/models_LGM/")
print(paste0("the work dir is ", work_dir))
out_dir <- paste0(getwd(), "/mean_LGM/")  

### decide if insert glacial maximum rasterized
#ice<- raster("LGM/.tif")  ### load LGM glaciers extension file (1,0)

### made loop
setwd(work_dir)
d<-dir(".", full.names=TRUE)

library(doParallel)
registerDoParallel(cl <- makeCluster(20))
foreach(i = 1:length(d), .packages = "raster") %dopar% {
 #i=1
 print(d[i])
 setwd(d[i])
 b<-dir(".",full.names=TRUE)
 s<-list.files(path= b, pattern="*\\.gri", recursive=F, full.names= TRUE)
 s<-s[ !grepl("IPSL|CNRM", s) ] 
 print(s)
 s1<-stack(s)/1000
 sc <- calc(s1, fun = mean, na.rm = TRUE)
 sd1 <- calc(s1, fun = sd, na.rm = TRUE)
 
 name1 <- paste0(substring(d[i],3,),"_mean.tif")
 name2 <- paste0(substring(d[i],3,),"_sd.tif")
 
 writeRaster(sc, paste0(out_dir, name1), overwrite=TRUE)
 writeRaster(sd1, paste0(out_dir, name2), overwrite=TRUE)
 setwd("..")

 }

stopCluster(cl)
