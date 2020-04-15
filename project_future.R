project_future = function(myBiomodModelOut,k,t,lf,
                                    env_predictors_dir,read_from_file = TRUE) {
  
  #browser()
  if(read_from_file){myBiomodModelOut = readRDS(myBiomodModelOut)}
  
  name_model = unlist(strsplit(x = myBiomodModelOut@sp.name,split = "[.]"))
  spname = paste0(name_model[1],"_",name_model[2])# the species name with underscore "_" in the name
  model = name_model[3]
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
