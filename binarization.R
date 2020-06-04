binarization <- function(spek, sp, t, work_dir, out_dir)
{
  setwd(work_dir)
  d<-dir(".", full.names=T, pattern = spek)
  selected<- sub(pattern = "\\.", replacement = "_", x = spek)
  sp <- sp[sp$species %in% selected,]
  sp.names<-levels(factor(t[,1]))
  t1 <- subset(t, t[,1]== selected)
  t2 <- extent(t1[1,2], t1[1,3], t1[1,4], t1[1,5])
  ## create extent dalla tabella
  #geo <- crop(geo, t2)
  wd<-getwd() ##1:length(d)
  for(i in 1:length(d)){
    #i =1
    print(d[i])
    setwd(d[i])
    d2<-dir(".", full.names=T, pattern= "proj_")
    sc<-mean(stack(dir(d2, full.names=T, pattern = "*\\.gri")))/1000
    pa<-rasterize(sp[,2:3], sc, field=1, background=0)
    df1<-stack(pa,sc)
    df<-as.data.frame(df1, na.rm=T)
    id<-c(1:nrow(df))
    df1<-cbind(id,df)
    ot1<-optimal.thresholds(df1, opt.methods="MaxSens+Spec")
    ot2<-optimal.thresholds(df1, opt.methods="Sens=Spec")
    ot3<-optimal.thresholds(df1, opt.methods="MinROCdist")
    ot1<-ot1[1,2]*1000
    ot2<-ot2[1,2]*1000
    ot3<-ot3[1,2]*1000
    m1<-c(0,ot1,0, ot1,1000,1)
    m2<-c(0,ot2,0, ot2,1000,1)
    m3<-c(0,ot3,0, ot3,1000,1)
    rclmat1 <- matrix(m1, ncol=3, byrow=TRUE)
    rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)
    rclmat3 <- matrix(m3, ncol=3, byrow=TRUE)
    
    for(k in 1:length(d2)){
      # k=1
      print(d2[k])
      setwd(d2[k])
      s<-stack(dir(".", full.names=T, pattern = "*\\.gri"))
      sm<-mean(s)
      smr1<-reclassify(sm,rclmat1)
      smr2<-reclassify(sm,rclmat2)
      smr3<-reclassify(sm,rclmat3)
      st2<-unlist(strsplit(dir(".", full.names=T)[2], "[.]"))
      st3<-unlist(strsplit(st2[2], "/"))
      name1<-paste0(st3[2],"_",st2[3],"_",st2[4],"_bin_t1.tif")
      name2<-paste0(st3[2],"_",st2[3],"_",st2[4],"_bin_t2.tif")
      name3<-paste0(st3[2],"_",st2[3],"_",st2[4],"_bin_t3.tif")
      
      setwd("..")
      
      writeRaster(smr1, paste0(out_dir, name1), overwrite=TRUE)
      writeRaster(smr2, paste0(out_dir, name2), overwrite=TRUE)
      writeRaster(smr3, paste0(out_dir, name3), overwrite=TRUE)
    }
    
    setwd("..")
  }
  return(spek)
  rm(list=ls())
}
