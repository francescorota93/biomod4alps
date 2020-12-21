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

write.csv(df_habitat_parks, "df_habitat_parks.csv")

df_habitat_parks <- read.csv("/data/models/Protected_areas/df_habitat_parks.csv")
str(df_habitat_parks)
df_habitat_parks <- df_habitat_parks %>% mutate(zone, code = ifelse(zone == 0, "out_PA",
                                              ifelse(zone == 9, "PnAB",
                                              ifelse(zone == 5, "PnMC",
                                              ifelse(zone == 8, "PnPP",
                                              ifelse(zone == 2, "PNDB",
                                              ifelse(zone == 1, "PnDF",
                                              ifelse(zone == 3, "PnDS",
                                              ifelse(zone == 7, "PnFS",
                                              ifelse(zone == 10, "PnDA",
                                              ifelse(zone == 4, "PnPO",
                                              ifelse(zone == 6, "PnSC", NA))))))))))))

df_habitat_parks <- df_habitat_parks %>% mutate(zone, PA = ifelse(zone == 0, "out_PA",
                                              ifelse(zone == 9, "PA",
                                              ifelse(zone == 5, "PA",
                                              ifelse(zone == 8, "PA",
                                              ifelse(zone == 2, "PA",
                                              ifelse(zone == 1, "PA",
                                              ifelse(zone == 3, "PA",
                                              ifelse(zone == 7, "PA",
                                              ifelse(zone == 10, "PA",
                                              ifelse(zone == 4, "PA",
                                              ifelse(zone == 6, "PA", NA))))))))))))

df_habitat_parks <- na.omit(df_habitat_parks)
df_habitat_parks <- subset(df_habitat_parks, code != "PnMC") 

df_habitat_parks <- df_habitat_parks %>% mutate(loss, rel_loss = loss/(loss+stable))
df_habitat_parks <- df_habitat_parks %>% mutate(loss, rel_gain = gain/(loss+stable))


library(ggplot2)
library(ggbiplot)
library(tidyverse)
library(forcats)

myTheme <- theme(
  panel.background = element_rect(fill = "white", colour = "black"), 
  panel.grid.major =  element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_text(color = "black", size=14),
  axis.text.x = element_text(color = "black", size=14),
)
levels(df_habitat_parks$species) <- c("Cm", "Fa", "Gb", "Nb", "Pt", "Ra", "Sf", "Sd")


### optimistic

df_habitat_parks_opt <- subset(df_habitat_parks, scenario == 45)

b <-ggplot(data =  df_habitat_parks_opt, aes(x=fct_reorder(code, rel_loss), y= rel_loss)) +
  geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)+
  #scale_fill_manual(values=c( "grey60", "white")) +
  labs(x = "Protected Areas")+
  ## p value in scenario per parks
  #stat_compare_means(aes(as_label("p.value")), method = "kruskal")+
  myTheme +
  scale_y_continuous("range loss", breaks = c(0,0.5,1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
  facet_wrap(~species, nrow = 4)
ggsave(b, filename="models/Protected_areas/habitat_suitability_parks_opt.png", dpi = 300)


### pessimistic

df_habitat_parks_pes <- subset(df_habitat_parks, scenario == 85)

p <-ggplot(data =  df_habitat_parks_pes, aes(x=fct_reorder(code, rel_loss), y= rel_loss)) +
  geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)+
  #scale_fill_manual(values=c( "grey60", "white")) +
  labs(x = "Protected Areas")+
  ## p value in scenario per parks
  #stat_compare_means(aes(as_label("p.value")), method = "kruskal")+
  myTheme +
  scale_y_continuous("range loss", breaks = c(0,0.5,1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
  facet_wrap(~species, nrow = 4)
ggsave(p, filename="models/Protected_areas/habitat_suitability_parks_pes.png", dpi = 300)


