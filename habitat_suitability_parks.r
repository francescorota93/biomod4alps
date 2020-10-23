library(rgdal)
library(dplyr)
library(tidyr)
library(raster)

setwd("/data/models/bin_geo/") 

d<-dir("/data/models/bin_geo/", full.names=T)

all_file <- list.files("/data/models/bin_geo/", pattern = ".tif")
# name_curr <- all_file[grepl("current", all_file)]
# top <- all_file[!grepl("topo", all_file)]
# name <- top[!grepl("current", top)]

### for loop
threshold <- c("t1","t2", "t3")
algo <- c("CTA", "GAM", "GBM", "GLM", "RF")
spec <- c("Campanula_morettiana", "Festuca_austrodolomitica", "Gentiana_brentae", "Nigritella_buschmanniae",
             "Primula_tyrolensis", "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum")

count_zone_loss <- function(x, na.rm, y =1){
      value <- sum(x == y, na.rm = na.rm) 
      return(value)
     }

count_zone_gain <- function(x, na.rm, y =3){
       value <- sum(x == y, na.rm = na.rm) 
       return(value)
     }

count_zone_stable <- function(x, na.rm, y =4){
      value <- sum(x == y, na.rm = na.rm) 
      return(value)
     }

df_habitat_parks <- data.frame(zone = NA, gcm = NA, scenario = NA, species = NA, algo = NA, threshold = NA, stable = NA, loss = NA, gain = NA)

for(i in spec){
camp_mor <- all_file[grepl(i, all_file)]
camp_mor_clim <- camp_mor[!grepl("topo", camp_mor)]
  for(j in algo){
   camp_mor_clim_CTA <- camp_mor_clim[grepl(j, camp_mor_clim)]
   for(k in threshold){
    print(paste("started", i, j, k))
    camp_mor_clim_CTA_t1 <- camp_mor_clim_CTA[grepl(k, camp_mor_clim_CTA)]
    camp_mor_clim_CTA_t1_fut <- camp_mor_clim_CTA_t1[!grepl("current", camp_mor_clim_CTA_t1)]
     cm <- stack(camp_mor_clim_CTA_t1_fut)
     cm[cm == 1] <- 3
     camp_mor_clim_CTA_t1_cur <- camp_mor_clim_CTA_t1[grepl("current", camp_mor_clim_CTA_t1)]
     cm_cur <- raster(camp_mor_clim_CTA_t1_cur)
     st <- cm + cm_cur
     names(st) <- names(cm)
     park <- raster("/data/models/Protected_areas/parks_dolo.tif")
     park1 <- crop(park, st)
    loss <- zonal(st, park1, fun= count_zone_loss, na.rm = T)
    gain <- zonal(st, park1, fun= count_zone_gain, na.rm = T)
    stable <- zonal(st, park1, fun= count_zone_stable, na.rm = T)

names_df <- c("proj","GCM_scenario","year","genre","species","algo","bin","threshold")
df1 <- as.data.frame(stable)
df1 <- pivot_longer(df1, -zone, values_to = "stable") %>% 
              separate(name, names_df, sep = "_") %>%
              dplyr::select(-proj,-year,-bin) %>%
              unite(species, genre, species) %>%
              separate(GCM_scenario, c("gcm", "scenario"), 2)
df2 <- as.data.frame(loss)
df2 <- pivot_longer(df2, -zone, values_to = "loss") %>% 
              separate(name, names_df, sep = "_") %>%
              dplyr::select(-proj,-year,-bin) %>%
              unite(species, genre, species) %>%
              separate(GCM_scenario, c("gcm", "scenario"), 2)
df3 <- as.data.frame(gain)
df3 <- pivot_longer(df3, -zone, values_to = "gain") %>% 
              separate(name, names_df, sep = "_") %>%
              dplyr::select(-proj,-year,-bin) %>%
              unite(species, genre, species) %>%
              separate(GCM_scenario, c("gcm", "scenario"), 2)

df_all <- full_join(df1, df2)
df_all1 <- full_join(df_all, df3)
assign("df_habitat_parks", bind_rows(df_habitat_parks, df_all1))  
print(paste("done", i, j, k))
  }
 }
}

write.csv(df_habitat_parks, filename = "df_habitat_parks.csv")
