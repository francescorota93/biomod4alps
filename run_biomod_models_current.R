
run_biomod_models_current = function(myBiomodModelOut,cur,t) {


  spname = sub("[.]","_",myBiomodModelOut@sp.name)
  spname = sub("\\..*","",spname)
  
  t1 <- subset(t, t[,1]==spname)
  t2 <- extent(t1[1,2], t1[1,3], t1[1,4], t1[1,5])
  ## create extent dalla tabella
  cn  <- crop(cur, t2)
  cn<- stack(cn)
  print(paste("done cropping",spname,"current"))
  ###########################################################################
  ###########################      CURRENT        ###########################
  ###########################################################################
  
  # 5. Individual models projections on current environmental conditions
  
  myBiomodProj<- BIOMOD_Projection(
    modeling.output = myBiomodModelOut,
    new.env = cn,
    proj.name = 'current',
    selected.models = 'all',
    binary.meth = NULL ,
    compress = 'FALSE',
    build.clamping.mask = F)
  
  rm(myBiomodProj,cn) # to save memory
  
  return(paste0(spname,"_done"))
}
