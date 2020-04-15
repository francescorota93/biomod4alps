if(!interactive()){
  args = commandArgs(trailingOnly=TRUE)
  model =  args[1]
  n_cores = as.numeric(args[2])
  if(model=="all"){model = c("GLM","GAM","RF","GBM","CTA")}
  
  user = "g_genova"
  setwd(paste0("/home/lv71284/",user,"/data/"))
  source("biomod4alps/build_model.R")
  
}else{
  
  model = c("RF")
  n_cores = 2
  
  #setwd("/data/OneDrive/01_PhD/05_projects/boimod4alps/") 
  source("build_model.R")
}

library(biomod2)
library(raster)
library(doParallel)
library(foreach)

####################################
# choose species (numeric vector)
species = c(3)

# loading species occurrences data
s <- read.table("endemic_dolo50.txt", head = TRUE, sep = "\t")
t <- read.table("spec_extents.txt", head = TRUE, sep = "\t")
#sp.names<-levels(factor(s[,1]))
num_sp<-length(levels(factor(s[,1])))
#####################################
# loading current environmental data
cur=stack(dir("env_predictors/climate/present", full.names=T))

#####################################

out_dir = paste0(getwd(),"/models_future")
if(!dir.exists(out_dir)){dir.create(out_dir)}
setwd(out_dir)
#####################################
rasterOptions(tmpdir = paste0(getwd(),"/temp_rast_dir"),
              maxmemory = 4.9e+09#,
              #memfrac = (1/n_cores)*0.8
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
