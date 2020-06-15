### sintesi script

setwd("/data/models/bin_geo/") 

d<-dir("/data/models/bin_geo/", full.names=T)

library(dplyr)
library(tidyr)
out_dir <- "/data/models/maps/"
all_file <- list.files("/data/models/bin_geo/", pattern = ".tif")
name_curr <- all_file[grepl("current", all_file)]
top <- all_file[!grepl("topo", all_file)]
name <- top[!grepl("current", top)]


df = t(data.frame(strsplit(name,split = "_")))
colnames(df) <- c("proj","GCM_scenario","year","genre","species","algo","bin","threshold", "extention")
df = as_tibble(df)
df = df %>%
  separate(GCM_scenario, c("GCM", "scenario"), 2)

df_curr = t(data.frame(strsplit(name_curr,split = "_")))
colnames(df_curr) <- c("proj","scenario","genre","species","algo","bin","threshold", "extention")
df_curr = as_tibble(df_curr)
df_curr = df_curr %>% mutate(GCM = NA,year = NA)

df_tot = bind_rows(df,df_curr)

df_tot_split = df_tot %>%
  group_by_at(vars(-GCM,-algo,-threshold,-year)) %>% 
  group_split()

get_lists = function(x){
  if (unique(x$scenario) == "current"){
    name = "present"
  }
  else if (unique(x$scenario) == "45"){
    name = "optimistic"
  } 
  else if (unique(x$scenario) == "85"){
    name = "pessimistic"
  } 
  x = x %>% transmute(!!name := ifelse(scenario=="current",
                                       paste0(proj,"_",
                                              scenario,"_",genre,"_",
                                              species,"_",algo,"_",
                                              bin,"_",threshold, "_", extention),
                                       paste0(proj,"_",
                                              GCM,scenario,"_",
                                              year,"_",genre,"_",
                                              species,"_",algo,"_",
                                              bin,"_",threshold, "_", extention))
  ) %>% as.list()
  #x = list(current = as.list(x))
}

paths = sapply(df_tot_split, get_lists )

#paths
#print("ciao")
for(i in 1:length(paths)){ 
  print(i)
  name1 = paths[i]
  print(names(name1))
  x = paths[[i]]
  print(x)
  pr = stack(x)
  ps<-sum(pr)
  if(names(name1) %in% c("present","topo")){
    name.list <- unlist(strsplit(x[1],split = "_"))
    sp_name <- paste0(name.list[3],"_", name.list[4],"_", names(name1))
    name2 <- paste0(sp_name, "_consensus.tif")
    reclass.val <- 1
    }
  else if(names(name1) %in% c("optimistic", "pessimistic")){
    name.list <- unlist(strsplit(x[1],split = "_"))
    sp_name <- paste0(name.list[4],"_", name.list[5],"_", names(name1))
    name2 <- paste0(sp_name,"_consensus.tif")
    reclass.val <- 2
    }
  
  writeRaster(ps, paste0(out_dir, name2), overwrite=TRUE)
  
  perc_tile <- length(x)/2
  tot_tile <- length(x)
  ps_r<-reclassify(ps, c(0,perc_tile,0, perc_tile,tot_tile, reclass.val))
  writeRaster(ps_r, paste0(out_dir, sp_name,"_", "reclass.tif"))
}


# m85_80_s<-sum(ps_r,m85_80_r)
# 
# m45_80_s<-sum(ps_r,m45_80_r)
# 
# writeRaster(m45_80_s,"Campanula_morettiana_45_80_sintesi.tif")
# writeRaster(m45_80_s,"Campanula_morettiana_45_80_sintesi.tif")


# d <- list.files(".")
# s <- read.table("../endemic_dolo50.txt", head = TRUE, sep = "\t")
# species <- c(3,4)
# for(i in species){
#   sp.names<-levels(factor(s[,1]))[i]
#   d2<-list.files(".", pattern = sp.names, full.names = TRUE)
#   df <- stack(d2)
# 
# # for(i in 1:length(sp)){
# al1<-sp[1]
# al2<-sp[2]
# al3<-sp[3]
# al4<-sp[4]
# al5<-sp[5]
# 
# d2<-dir(".", patter="geo.tif")
# 
# ######presente############
# presGBM <-stack(dir(al1, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
# presGLM<-stack(dir(al2, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
# presMARS<-stack(dir(al3, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
# presMAX<-stack(dir(al4, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
# presRF<-stack(dir(al5, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
# 
# 
# p<-stack(presGBM,presGLM,presMARS,presMAX,presRF)
# ps<-sum(p)
# plot(ps)
# name1<-paste0("Campanula_morettiana","_pres_consensus.tif")
# writeRaster(ps,name1,overwrite=TRUE)
# 
# ######pessimistic############
# ##80
# gbm85_80<-stack(dir(al1, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
# glm85_80<-stack(dir(al2, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
# mars85_80<-stack(dir(al3, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
# max85_80<-stack(dir(al4, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
# rf85_80<-stack(dir(al5, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
# 
# s85_80<-stack(gbm85_80, glm85_80,mars85_80,max85_80,rf85_80)
# m85_80<-sum(s85_80)
# plot(m85_80)
# name2<-paste0("Campanula_morettiana","_85_80_consensus.tif")
# writeRaster(m85_80,name2,overwrite=TRUE)
# 
# ps_r<-reclassify(ps, c(0,7,0, 7,15,1))
# m85_80_r<-reclassify(m85_80, c(0,37,0, 37,75,2))
# 
# m85_80_s<-sum(ps_r,m85_80_r)
# 
# writeRaster(m85_80_s,"Campanula_morettiana_85_80_sintesi.tif") 
# 
# ######optimistic############
# ##80
# gbm45_80<-stack(dir(al1, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
# glm45_80<-stack(dir(al2, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
# mars45_80<-stack(dir(al3, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
# max45_80<-stack(dir(al4, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
# rf45_80<-stack(dir(al5, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
# 
# 
# s45_80<-stack(gbm45_80, glm45_80,mars45_80,max45_80,rf45_80)
# m45_80<-sum(s45_80)
# plot(m45_80)
# name2<-paste0("Campanula_morettiana","_45_80_consensus.tif")
# writeRaster(m45_80,name2,overwrite=TRUE)
# 
# ps_r<-reclassify(ps, c(0,7,0, 7,15,1))
# m45_80_r<-reclassify(m45_80, c(0,37,0, 37,75,2))
# 
# m45_80_s<-sum(ps_r,m45_80_r)
# 
# writeRaster(m45_80_s,"Campanula_morettiana_45_80_sintesi.tif") 