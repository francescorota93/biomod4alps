run_biomod_models_future = function(myBiomodModelOut,k,t,lf,env_predictors_dir) {
  
  #browser()
  # loading species occurances data
  spname = sub("[.]","_",myBiomodModelOut@sp.name)
  spname = sub("\\..*","",spname) # the species name with underscore "_" in the name
  
  ###########################################################################
  ###########################         FUTURE           ######################
  ###########################################################################
  
  name<-lf[k]
  
  nm<-paste0(env_predictors_dir,name)
  fut=stack(dir(nm, full.names=T))
  
  ## create extent dalla tabella
  t1 <- subset(t, t[,1]==spname)
  t2 <- extent(t1[1,2], t1[1,3], t1[1,4], t1[1,5])
  fn  <- crop(fut, t2)
  rm(fut) # to save memory
  fn<- stack(fn)
  print(paste("done cropping",spname,model))
  # 5. Individual models projections on future environmental conditions
  
  myBiomodProj<- BIOMOD_Projection(
    modeling.output = myBiomodModelOut,
    new.env = fn,
    proj.name = name,
    selected.models = 'all',
    binary.meth = NULL,
    compress = 'FALSE',
    build.clamping.mask = F)
  
  rm(myBiomodProj,fn) # to save memory
  
  return(paste0(spname,"_",name,"_done"))
}
