if(interactive()){
  
  species = c(3,4)
  model = c("RF")
  n_cores = 2
  outdir ="/models_future"
  #setwd("/data/OneDrive/01_PhD/05_projects/boimod4alps/") 
  source("build_model.R")
  
}else{
  
  library(argparser)
  p <- arg_parser("Get options for biomod models and projections")
  p <- add_argument(parser = p,arg =  "--models", help="algorithms", nargs = Inf)
  p <- add_argument(parser = p, arg = "--cores", help="number of cores", default=5, type = "numeric")
  p <- add_argument(parser = p, arg = "--species", help="which species", nargs = Inf)
  p <- add_argument(parser = p, arg = "--workdir", help="directory where input data is")
  p <- add_argument(parser = p, arg = "--scriptdir", help="directory where script is")
  p <- add_argument(parser = p, arg = "--outdir", help="directory to sotore output relative to workdir",
                    default = "/models_future")
  p <- add_argument(parser = p, arg = "--user", help="which user is running the job", default = "frota")
  # Parse the command line arguments
  argv <- parse_args(p)
  
  n_cores = argv$cores
  user = argv$user
  workdir = argv$workdir
  outdir = argv$outdir
  scriptdir = argv$scriptdir
  
  if(is.na(argv$models) | is.null(argv$models)){
    model = c("GLM","GAM","RF","GBM","CTA")
  }else {model = argv$models}
  
  if(is.na(argv$species) | is.null(argv$species)){
    species = 1:8}else {species = as.numeric(argv$species)}
  
  setwd(workdir)
  source(paste0(scriptdir,"/build_model.R"))
  
  model;species;n_cores;workdir;outdir;scriptdir;user
  
}

library(biomod2)
library(raster)
library(doParallel)
library(foreach)

####################################
# choose species (numeric vector)

# loading species occurrences data
s <- read.table("endemic_dolo50.txt", head = TRUE, sep = "\t")
t <- read.table("spec_extents.txt", head = TRUE, sep = "\t")
#sp.names<-levels(factor(s[,1]))
num_sp<-length(levels(factor(s[,1])))
#####################################
# loading current environmental data
cur=stack(dir("env_predictors/climate/present", full.names=T))

#####################################

out_dir = paste0(getwd(),outdir)
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
