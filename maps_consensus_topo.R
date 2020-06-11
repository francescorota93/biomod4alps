### sintesi script

setwd("/data/models/bin_geo/") 

d<-dir("/data/models/bin_geo/", full.names=T)

library(dplyr)
library(tidyr)
out_dir <- "/data/models/maps/"
all_file <- list.files("/data/models/bin_geo/", pattern = ".tif")
#name_curr <- all_file[grepl("current", all_file)]
top <- all_file[grepl("topo", all_file)]
#name <- top[!grepl("current", top)]

df_topo = t(data.frame(strsplit(top,split = "_")))
colnames(df_topo) <- c("proj","scenario","genre","species","algo","bin","threshold", "extention")
df_topo = as_tibble(df_topo)
df_topo = df_topo %>% mutate(GCM = NA,year = NA)

#df_tot = bind_rows(df,df_topo)

df_tot_split = df_topo %>%
  group_by_at(vars(-GCM,-algo,-threshold,-year)) %>% 
  group_split()

get_lists = function(x){
  if (unique(x$scenario) == "topo"){
    name = "topo"
  }
  else if (unique(x$scenario) == "45"){
    name = "optimistic"
  }
  else if (unique(x$scenario) == "85"){
    name = "pessimistic"
  }
  x = x %>% transmute(!!name := ifelse(scenario=="topo",
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
#paths = df_tot_split

for(i in 1:length(paths)){ 
  print(i)
  name1 = paths[i]
  print(names(name1))
  x = paths[[i]]
  print(x)
  pr = stack(x)
  ps<-sum(pr)
  names(name1) %in% c("present","topo")
  name.list <- unlist(strsplit(x[1],split = "_"))
  sp_name <- paste0(name.list[3],"_", name.list[4],"_", names(name1))
  name2 <- paste0(sp_name, "_consensus.tif")
  reclass.val <- 1

  
  writeRaster(ps, paste0(out_dir, name2), overwrite=TRUE)
  
  perc_tile <- length(x)/2
  tot_tile <- length(x)
  ps_r<-reclassify(ps, c(0,perc_tile,0, perc_tile,tot_tile, reclass.val))
  writeRaster(ps_r, paste0(out_dir, sp_name,"_", "reclass.tif"))
}
