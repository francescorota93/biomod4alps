#setwd("/home/lv71284/frota/data/") ###setwd("/home/lv71284/frota/data/")
library(raster)
library(PresenceAbsence)
source("binarization.R")
#####################################
# loading results and points
####################################
sp<-read.table("endemic_dolo50.txt", sep="\t", h=T) ### load spec occurrence file
spek <- sub(pattern = "_", replacement = "\\.", x = as.character(unique(sp$species)))
work_dir <- paste0(getwd(), "/models_future/")
out_dir <- paste0(getwd(), "/bin/")  ### cartella dove salva i file, crea cartella bin in wd
#geo<- raster("geology/geology_endemic_dolo_all.tif")  ### load geology file (1,0)

# cut on extent per species  t <- table(xmin,ymax) 
t <- read.table("spec_extents.txt", head = TRUE, sep = "\t")

### made loop for each species

lapply(spek, binarization, sp,  t, work_dir, out_dir)
