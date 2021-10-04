mean_lgm <- function(spek, sp, t, work_dir, out_dir)
{
  setwd(work_dir)
  d<-dir(".", full.names=T, pattern = spek)
  selected<- sub(pattern = "\\.", replacement = "_", x = spek)
  sp <- sp[sp$species %in% selected,]
  sp.names<-levels(factor(t[,1]))
  t1 <- subset(t, t[,1]== selected)
  t2 <- extent(t1[1,2], t1[1,3], t1[1,4], t1[1,5])
  ## create extent dalla tabella
  
  wd<-getwd() ##1:length(d)
  for(i in 1:length(d)){
    #i =1
    print(d[i])
    setwd(d[i])
    d2<-dir(".", full.names=T, pattern= "proj_")
    sc<-mean(stack(dir(d2, full.names=T, pattern = "*\\.gri")))/1000
    sd1<-sd(stack(dir(d2, full.names=T, pattern = "*\\.gri")))/1000
    
    st2<-unlist(strsplit(dir(".", full.names=T)[2], "[.]"))
    st3<-unlist(strsplit(st2[2], "/"))
    name1<-paste0(st3[2],"_",st2[3],"_",st2[4],"_mean.tif")
    name2<-paste0(st3[2],"_",st2[3],"_",st2[4],"_sd.tif")

      #geo <- crop(geo, t2)
      
      writeRaster(sc, paste0(out_dir, name1), overwrite=TRUE)
      writeRaster(sd1, paste0(out_dir, name2), overwrite=TRUE)
    
    setwd("..")
  }
  return(spek)
  rm(list=ls())
}
