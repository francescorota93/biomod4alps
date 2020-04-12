build_biomod_models = function(i,model,s,t,#s,#sp.names,
                               cur,
                               PA.nb.rep = 10,
                               PA.nb.absences = 1000,
                               PA.strategy = 'random',
                               write_models_rds = TRUE){
  
  
  if(model %in% c("GLM","GAM")){
    PA.nb.rep = 10
    PA.nb.absences = 1000
    PA.strategy = "random"
    
  }else if(model %in% c("RF","GMB","CTA")){
    PA.nb.rep = 10
    PA.nb.absences = "stratified"
    PA.strategy = "sre"
  }
  
  if(PA.nb.absences!="stratified"){
    PA.nb.absences = as.numeric(PA.nb.absences)
  }
  
  #browser()
  #i=1
  # s <- read.table(spieces_tab, head = TRUE, sep = "\t")
  sp.names<-levels(factor(s[,1]))
  sp_name = sp.names[i]
  ###########################################################################
  ###########################     model    CURRENT     ######################
  ###########################################################################
  
  # loading species occurances data
  s1<-subset(s, s[,1]==sp_name)
  rw<-nrow(s1)
  matr <- matrix(data = 0, nrow = rw, ncol = 3)
  colnames(matr)=c('x','y',sp_name)
  matr[,1]<-s1[,2]
  matr[,2]<-s1[,3]
  matr[,3]<-1
  
  spocc <- matr
  myRespName <- paste (sp_name, ".",model, sep = "")
  myRespXY <- spocc[,1:2] # coordinates of points
  myResp <- as.numeric(spocc[,3]) # species occurences
  
  # cut on extent per species  t <- table(xmin,ymax) ----------
  #t <- read.table(spec_extents, head = TRUE, sep = "\t")
  t1 <- subset(t, t[,1]==sp_name)
  t2 <- extent(t1[1,2], t1[1,3], t1[1,4], t1[1,5])
  ## create extent dalla tabella
  cn  <- crop(cur, t2)
  cn<- stack(cn)
  
  if(PA.nb.absences=="stratified"){
    PA.nb.absences=rw
  }
  
  # 1. Formatting Data
  
  myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                       expl.var = cn,
                                       resp.xy = myRespXY,
                                       resp.name = myRespName,
                                       eval.resp.var = NULL,
                                       eval.expl.var = NULL,
                                       eval.resp.xy = NULL,
                                       PA.nb.rep = PA.nb.rep,
                                       PA.nb.absences = PA.nb.absences,
                                       PA.strategy = PA.strategy,
                                       PA.dist.min = NULL,
                                       PA.dist.max = NULL,
                                       PA.sre.quant = 0.15,
                                       na.rm = TRUE)
  
  
  # 2. Defining Models Options using default options.
  myBiomodOption <- BIOMOD_ModelingOptions()
  
  # 3. Computing the models
  myBiomodModelOut <- BIOMOD_Modeling(myBiomodData,
                                      models = c(model),
                                      models.options = myBiomodOption,
                                      NbRunEval=10,
                                      DataSplit=70,
                                      Yweights=NULL,
                                      VarImport=0,
                                      models.eval.meth = c('TSS', 'ROC', 'KAPPA', 'POD', 'FAR'),
                                      SaveObj = TRUE,
                                      rescal.all.models = TRUE,
                                      do.full.models=FALSE)
  
  ###Models evaluations
  myBiomodModelEval <- getModelsEvaluations(myBiomodModelOut)
  dimnames(myBiomodModelEval)
  
  if(!dir.exists("eval")){dir.create("eval")}
  m<-myBiomodModelEval["TSS","Testing.data",,,]
  nome1<-paste(getwd(),"/eval/",model,"_TSS_", sp_name, ".txt", sep="")
  write.table(m, file=nome1, sep="\t")
  
  n<-myBiomodModelEval["ROC","Testing.data",,,]
  nome2<-paste(getwd(),"/eval/",model,"_ROC_", sp_name, ".txt", sep="")
  write.table(n, file=nome2, sep="\t")
  
  o<-myBiomodModelEval["KAPPA","Testing.data",,,]
  nome3<-paste(getwd(),"/eval/",model,"_KAPPA_", sp_name, ".txt", sep="")
  write.table(o, file=nome3, sep="\t")
  
  o1<-myBiomodModelEval["POD","Testing.data",,,]
  nome4<-paste(getwd(),"/eval/",model,"_POD_", sp_name, ".txt", sep="")
  write.table(o1, file=nome4, sep="\t")
  
  
  o2<-myBiomodModelEval["FAR","Testing.data",,,]
  nome5<-paste(getwd(),"/eval/",model,"_FAR_", sp_name, ".txt", sep="")
  write.table(o2, file=nome5, sep="\t")
  
  rm(myBiomodData) # to save memory
  
  if(write_models_rds){
    if(!dir.exists("models_rds")){dir.create("models_rds")}
    saveRDS(myBiomodModelOut,paste0("models_rds/",sp_name,"_",model,".rds"))
  }
  
  return(myBiomodModelOut)
  
}
