if(!interactive()){
  args = commandArgs(trailingOnly=TRUE)
  model =  args[1]
  n_cores = as.numeric(args[2])
  
  user = "g_genova"
  setwd(paste0("/home/lv71284/",user,"/data/"))
  
  source("biomod4alps/project_future.R")
  
}else{
  
  model = "RF"
  n_cores = 2
  
  #setwd("/data/OneDrive/01_PhD/05_projects/boimod4alps/")
  source("project_future.R")
}

library(biomod2)
library(raster)
library(doParallel)
library(foreach)

####################################
# loading species extent
t <- read.table("spec_extents.txt", head = TRUE, sep = "\t")

#####################################
# loading FUTURE list
env_predictors_dir = paste0(getwd(),"/env_predictors/climate/future/")
lf<-list.files(path = env_predictors_dir)
# choose future env variables
future_pred = 1:length(lf)

#####################################

out_dir = paste0(getwd(),"/models_future")
if(!dir.exists(out_dir)){dir.create(out_dir)}
setwd(out_dir)
#####################################

registerDoParallel(cores = n_cores)

rasterOptions(tmpdir = paste0(getwd(),"/temp_rast_dir"),
              maxmemory = 4.9e+09#,
              #memfrac = (1/n_cores)*0.8
)
#raster::tmpDir(create = TRUE)
tmpDir()

models_rds = list.files(path = "models_rds" , pattern = ".rds",full.names = T)
if(model!="all"){models_rds = models_rds[grep(pattern = model,x = models_rds)]}

## do the job - future projections
models_future = foreach(k = future_pred) %:%
  foreach(mod = models_rds) %dopar% {
    
    project_future(myBiomodModelOut = mod,
                             k = k,t = t,lf = lf,
                             env_predictors_dir = env_predictors_dir,
                             read_from_file = TRUE)
  }

models_future

unlink(tmpDir(), recursive=T, force=FALSE)

print("End of the script")
