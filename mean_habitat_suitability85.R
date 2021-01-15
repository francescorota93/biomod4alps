setwd("/home/lv71284/frota/data/")
library(raster)
library(parallel)
library(doParallel)
library(foreach)

sp<-read.table("endemic_dolo50.txt", sep="\t", h=T) ### load spec occurrence file
spek <- sub(pattern = "_", replacement = "\\.", x = as.character(unique(sp$species)))
spek1 <- sub(pattern = "_", replacement = "\\_", x = as.character(unique(sp$species)))
work_dir <- paste0(getwd(), "/models_future/")
setwd(work_dir)
out_dir <- paste0("/home/lv71284/frota/data/", "habitat_model/")  ### cartella dove salva i file, crea cartella bin in wd

#geo<- raster("../geology/geology_endemic_dolo_all.tif")  ### load geology file (1,0)

# cut on extent per species  t <- table(xmin,ymax) 
t <- read.table("../spec_extents.txt", head = TRUE, sep = "\t")

d3 <- list.files(path = ".", )

registerDoParallel(cores = 8)
## try for one species


foreach(i = 1:8, .errorhandling = "pass") %dopar% {
tryCatch(
  {
    
    selected<- sub(pattern = "\\.", replacement = "_", x = spek)
    sp.names<-levels(factor(t[,1]))
    # t1 <- subset(t, t[,1]== selected)
    # t2 <- extent(t1[1,2], t1[1,3], t1[1,4], t1[1,5])
    # geo <- crop(geo, t2)
    
    d3.1 <- grep(spek[i], d3, value = TRUE)
    ## current
    d6 <- dir( path = d3.1, pattern = glob2rx("*current_*.gri"), recursive = TRUE)
    ##stack, mask su maps e media e sd
    pr_st <- stack(paste0(d3.1,"/",d6))
    map <- raster(paste0("../maps/", spek1[i], "_optimistic_sintesi.tif"))
    map[map == 0] <- NA
    pr_st1 <- mask(pr_st, map)
    pr_mean <- mean(pr_st1)
    #geo1 <- crop(geo, pr_mean)
    #pr_mean1 <- mask(pr_mean, geo1)
    #pr_mean[pr_mean < 510] <- NA
    
    writeRaster(pr_mean,  paste0(out_dir, spek[i], "_mean_present.tif"), overwrite=TRUE)
    print(spek[i])
    #pr_mean[pr_mean == 0] <- NA
    #pr_sd <- calc(pr_st, sd)
    #writeRaster(pr_sd,  paste0(out_dir, spek[i], "_sd_present.tif"), overwrite=TRUE)
    
    # ### list 45 files (optimistic)
    # d4 <- dir( path = d3.1, pattern = glob2rx("*45_*.gri"), recursive = TRUE)
    # ## stack e media e sd
    # fut45_st <- stack(paste0(d3.1,"/",d4))
    # map <- raster(paste0("../maps/", spek1[i], "_optimistic_sintesi.tif"))
    # map[map == 0] <- NA
    # fut45_st1 <- mask(fut45_st, map)
    # fut45_mean <- mean(fut45_st1)
    # #geo1 <- crop(geo, fut45_mean)
    # #fut45_mean1 <- mask(fut45_mean, geo1)
    # writeRaster(fut45_mean,  paste0(out_dir, spek[i], "_mean_fut45.tif"), overwrite=TRUE)
    # print(spek[i])
    # #fut45_mean[fut45_mean == 0] <- NA
    # fut45_sd <- calc(fut45_st1, sd)
    # writeRaster(fut45_sd,  paste0(out_dir, spek[i], "_sd45.tif"), overwrite=TRUE)
    
    ### list 85 files (pessimistic)
    d5 <- dir( path = d3.1, pattern = glob2rx("*85_*.gri"), recursive = TRUE)
    ## stack e media e sd
    fut85_st <- stack(paste0(d3.1,"/",d5))
    map <- raster(paste0("../maps/", spek1[i], "_pessimistic_sintesi.tif"))
    map[map == 0] <- NA
    fut85_st1 <- mask(fut85_st, map)
    fut85_mean <- mean(fut85_st1)
    writeRaster(fut85_mean,  paste0(out_dir, spek[i], "_mean_fut85.tif"), overwrite=TRUE)
    print(spek[i])

    fut85_sd <- calc(fut85_st1, sd)
    writeRaster(fut85_sd,  paste0(out_dir, spek[i], "_sd85.tif"), overwrite=TRUE)

    # ### scenario 45 optimistic, difference future - present
    # 
    # # on_0_1000:logical, if TRUE (default), 0 - 1 probabilities are converted into a 0 - 1000 integer scale. This implies a lot of memory saving. User that want to
    # # comeback on a 0 - 1 scale latter will just have to divide all projections by 1000 
    # 
    # ## differenza percentuale
    # 
    # dif_45pres <- ((fut45_mean - pr_mean)/((fut45_mean + pr_mean)/2))
    # #plot(dif_45pres)
    # 
    # #dif_45pres1 <- mask(dif_45pres, geo)######
    # 
    # dif_85pres <- ((fut85_mean - pr_mean)/((fut85_mean + pr_mean)/2))
    # #plot(dif_85pres)
    # 
    # #dif_85pres1 <- mask(dif_85pres, geo)  ####
    # # ## rescale to -1 to 1 values -
    # # r.min = cellStats(dif_45pres, "min")
    # # r.max = cellStats(dif_45pres, "max")
    # #r.scale45 <- ((dif_45pres - r.min) / (r.max - r.min))
    # 
    # writeRaster(dif_45pres, paste0(out_dir, spek[i], "_dif45.tif"), overwrite=TRUE)
    # writeRaster(dif_85pres, paste0(out_dir, spek[i], "_dif85.tif"), overwrite=TRUE)
    # print(spek[i])
    # 
  },
  error=function(cond) {
    message(cond)
    return(traceback())
  })
}



