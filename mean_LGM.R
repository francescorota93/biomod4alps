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
    s <- stack(dir(d2, full.names=T, pattern = "*\\.gri"))/1000
    sc<-calc(s, fun = mean, na.rm = TRUE)
    sd1<-calc(s, fun = sd, na.rm = TRUE)    
    
    st2<-unlist(strsplit(dir(".", full.names=T)[2], "[.]"))
    st3<-unlist(strsplit(st2[2], "/"))
    name1<-paste0(substring(d[i],3,),"_mean.tif")
    name2<-paste0(substring(d[i],3,),"_sd.tif")

      #geo <- crop(geo, t2)
      
      writeRaster(sc, paste0(out_dir, name1), overwrite=TRUE)
      writeRaster(sd1, paste0(out_dir, name2), overwrite=TRUE)
    
    print(spek)
  }
  return(spek)
  rm(list=ls())
}
