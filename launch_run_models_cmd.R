if(!interactive()){
  setwd("/home/lv71284/g_genova/data/")
}else{
  setwd("/data/OneDrive/01_PhD/05_projects/boimod2/")  
  }

library(biomod2)
library(raster)
library(doParallel)
library(foreach)
source("biomod4alps/build_biomod_models.R")
source("biomod4alps/run_biomod_models_current.R")
source("biomod4alps/run_biomod_models_future.R")
####################################
# choose species (numeric vector)
species = c(3:4)

# loading species occurrences data
s <- read.table("endemic_dolo50.txt", head = TRUE, sep = "\t")
t <- read.table("spec_extents.txt", head = TRUE, sep = "\t")
#sp.names<-levels(factor(s[,1]))
num_sp<-length(levels(factor(s[,1])))
#####################################
# loading current environmental data
cur=stack(dir("env_predictors/climate/present", full.names=T))

#####################################
# loading FUTURE list
env_predictors_dir = paste0(getwd(),"/env_predictors/climate/future/")
lf<-list.files(path = env_predictors_dir)
# choose future env variables
future_pred = 1:length(lf)
#####################################
# model options

if(!interactive()){
  args = commandArgs(trailingOnly=TRUE)
  model =  args[1]
  n_cores = as.numeric(args[2])
}else {
  model = "RF"
  n_cores = 2
}

out_dir = paste0(getwd(),"/models_future")
if(!dir.exists(out_dir)){dir.create(out_dir)}
setwd(out_dir)
#####################################


rasterOptions(tmpdir = paste0(getwd(),"/temp_rast_dir"),
              maxmemory = 4.9e+09,
              memfrac = (1/n_cores)*0.8
              )
#raster::tmpDir(create = TRUE)
tmpDir()

# cl <- makeCluster(n_cores)
# registerDoParallel(cl)
registerDoParallel(cores = n_cores)
#getDoParWorkers()

## do the job - models
models = foreach(i = species) %dopar% {
  
  build_biomod_models(i,
                      model = model,
                      s = s,
                      t = t,
                      #sp.names = sp.names,
                      cur = cur,
                      #out_dir = out_dir,
                      write_models_rds = FALSE)
}

unlink(tmpDir(), recursive=T, force=FALSE)

registerDoParallel(cores = n_cores)


## do the job - current projections
models_current = foreach(mod = models) %dopar% {
  
  run_biomod_models_current(myBiomodModelOut = mod,cur = cur,t = t,
                            read_from_file = FALSE)
  
}

models_current

unlink(tmpDir(), recursive=T, force=FALSE)

rm(cur) # to save some memory

registerDoParallel(cores = n_cores)

## do the job - future projections
models_future = foreach(k = future_pred) %:%
  foreach(mod = models) %dopar% {
    
    run_biomod_models_future(myBiomodModelOut = mod,
                             k = k,t = t,lf = lf,
                             env_predictors_dir = env_predictors_dir,
                             read_from_file = FALSE)
  }

models_future

unlink(tmpDir(), recursive=T, force=FALSE)

print("End of the script")


# .errorhandling = "pass",
# .packages = c("biomod2", "raster", "rgdal","gbm", "mda", "randomForest","Hmisc",
#               "plyr", "maptools","foreach","doParallel","iterators")
