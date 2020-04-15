
run_biomod_models_current = function(myBiomodModelOut,cur,t,read_from_file = TRUE) {
  
  if(read_from_file){myBiomodModelOut = readRDS(myBiomodModelOut)}
  name_model = unlist(strsplit(x = myBiomodModelOut@sp.name,split = "[.]"))
  spname = paste0(name_model[1],"_",name_model[2])# the species name with underscore "_" in the name
  model = name_model[3]
  
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
  
  return(paste0(spname,"_",model,"_current","_done"))
}
