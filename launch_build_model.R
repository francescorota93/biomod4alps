if(!interactive()){
  setwd("/home/lv71284/g_genova/data/")
}else{
  setwd("/data/OneDrive/01_PhD/05_projects/boimod2/")  
}

library(biomod2)
library(raster)
library(doParallel)
library(foreach)
source("biomod4alps/build_model.R")
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
# model options

if(!interactive()){
  args = commandArgs(trailingOnly=TRUE)
  model =  args[1]
  n_cores = as.numeric(args[2])
  if(model=="all"){model = c("GLM","GAM","RF","GBM","CTA")}
}else {
  model = c("GAM","RF")
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
models = foreach(i = species) %:% 
  foreach(model = model) %dopar% {
    
    build_model(i,model = model,
                        s = s,
                        t = t,
                        cur = cur,
                        write_models_rds = TRUE)
  }

unlink(tmpDir(), recursive=T, force=FALSE)

print("End of the script")
