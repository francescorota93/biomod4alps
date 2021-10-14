setwd("/home/lv71418/frrota/data/") ###setwd("/home/lv71284/frota/data/")
library(raster)
#library(foreach)
#library(terra)
#library(PresenceAbsence)
source("biomod4alps/mean_LGM.R")
#####################################
# loading results and points
####################################
sp<-read.table("endemic_dolo50.txt", sep="\t", h=T) ### load spec occurrence file
spek1 <- sub(pattern = "_", replacement = "\\.", x = as.character(unique(sp$species)))
spek <- spek1[c(1,5,6,7)]
work_dir <- paste0(getwd(), "/models_LGM/")
print(paste0("the work dir is ", work_dir))
out_dir <- paste0(getwd(), "/mean_LGM/")  ### cartella dove salva i file, crea cartella bin in wd
### decide if insert glacial maximum rasterized
#ice<- raster("LGM/.tif")  ### load LGM glaciers extension file (1,0)

# cut on extent per species  t <- table(xmin,ymax) 
t <- read.table("spec_extents.txt", head = TRUE, sep = "\t")

### made loop for each species

lapply(spek, mean_lgm, sp=sp, t=t, work_dir=work_dir, out_dir=out_dir)
#foreach(1:4) %dopar% mean_lgm(spek, sp=sp, t=t, work_dir=work_dir, out_dir=out_dir)
