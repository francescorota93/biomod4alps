if(interactive()){
  
  species = c(3,8)
  model = c("RF","GAM")
  n_cores = 2
  outdir ="/models_future"
  #setwd("/data/OneDrive/01_PhD/05_projects/boimod4alps/") 
  source("project_future.R")
 
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
  p <- add_argument(parser = p, arg = "--temprastdir", help="dir where to create temprastdir, default global", default = "/global/lv71284/frota/temp_rast_dir")
  
  # Parse the command line arguments
  argv <- parse_args(p)
  
  n_cores = argv$cores
  user = argv$user
  workdir = argv$workdir
  outdir = argv$outdir
  scriptdir = argv$scriptdir
  temprastdir = argv$temprastdir
  
  if(any(is.na(argv$models)) | any(is.null(argv$models))){
    model = c("GLM","GAM","RF","GBM","CTA")
  }else {model = argv$models}
  
  if(any(is.na(argv$species)) | any(is.null(argv$species))){
    species = 1:8}else {species = as.numeric(argv$species)}
  
  setwd(workdir)
  source(paste0(scriptdir,"/project_future.R"))
  
  print(paste0("Model: ",model))
  print(paste0("Species: ",species))
  print(paste0("Cores: ",n_cores))
  print(paste0("Workdir: ",workdir))
  print(paste0("Outdir: ",outdir))
  print(paste0("Scriptdir: ",scriptdir))
  print(paste0("User: ",user))
  
}

library(biomod2)
library(raster)
library(doParallel)
library(foreach)

####################################
# loading species extent
t <- read.table("spec_extents.txt", head = TRUE, sep = "\t")
s <- read.table("endemic_dolo50.txt", head = TRUE, sep = "\t")
sp.names<-levels(factor(s[,1]))[species]
#####################################
# loading FUTURE list
env_predictors_dir = paste0(getwd(),"/env_predictors/climate/future/")
lf<-list.files(path = env_predictors_dir)
# choose future env variables
future_pred = 1:length(lf)

#####################################

out_dir = paste0(getwd(),outdir)
if(!dir.exists(out_dir)){dir.create(out_dir)}
setwd(out_dir)
#####################################

registerDoParallel(cores = n_cores)

rasterOptions(tmpdir = temprastdir,
              maxmemory = 4.9e+09#,
              #memfrac = (1/n_cores)*0.8
)
#raster::tmpDir(create = TRUE)
tmpDir()

models_rds = list.files(path = "models_rds" , pattern = ".rds",full.names = T)
models_rds = models_rds[grepl(pattern = paste(model,collapse = "|"),x = models_rds)]
models_rds = models_rds[grepl(pattern = paste(sp.names,collapse = "|"),x = models_rds)]

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
