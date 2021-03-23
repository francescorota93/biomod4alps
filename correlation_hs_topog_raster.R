setwd("/data/models/")
library(raster)
library(rgdal)
library(sp)
library(RStoolbox)
library(ggplot2)
library(ggbiplot)
library(tidyverse)
library(ggsignif)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(permutations)
library(PMCMRplus)
library(forcats)
library(spatialEco)

## calculate topographic heterogeneity
## add elevation slope eastness northness and PC1 and PC2 topo al df_habitat_velocity
## fai df con aree parco aggiunte

dem <- raster("env_predictors/DTM_50.tif")
names(dem) <- "elev"
summary(dem)
#sd_elev <- raster.deviation(dem, type = "mean", s = 3, global = FALSE)
#sd_elev_df <- as.data.frame(sd_elev, xy = T)
TCI <- raster("env_predictors/intrinsic_topographic_complexity.tif")
names(TCI) <- "TCI"
TCI[TCI == NA] <- 0
tci1 <- setExtent(TCI, extent(dem), keepres=T)
### https://www.rdocumentation.org/packages/spatialEco/versions/1.3-2/topics/tri
# a <- tri(dem1, s=c(3,3), exact = T)
# names(a) <- "TRI"
# writeRaster(a, filename = "env_predictors/TRI.tif")
TRI <- raster("env_predictors/TRI.tif")
st <- stack(dem, TRI, tci1)
# heterogeneity_df <- as.data.frame(st, xy = T, na.rm = T)

# head(heterogeneity_df)


spec <- c("Campanula.morettiana", "Festuca.austrodolomitica", "Gentiana.brentae", "Nigritella.buschmanniae",
             "Primula.tyrolensis", "Rhizobotrya.alpina", "Saxifraga.facchinii", "Sempervivum.dolomiticum")
velocity45 <- raster("results_climate_change_velocity/future/logSpeed_45.tif")
velocity85 <- raster("results_climate_change_velocity/future/logSpeed_85.tif")
st1 <- crop(st, velocity45)
velocity_all <- stack(velocity45, velocity85, st1)

### create stack for each species hs continuos
for(i in seq_along(spec)){
map45 <- raster(paste0("habitat_model/",spec[i],"_dif45.tif"))
map85 <- raster(paste0("habitat_model/",spec[i],"_dif85.tif"))
map <- stack(map45, map85)
df <-crop(velocity_all, map)
st2 <- stack(df, map)
names(st2) <- c("logSpeed_45","logSpeed_85", "elev", "TRI", "TCI","habitat_opt", "habitat_pes")
writeRaster(st2, paste0("habitat_topohetero_models/",spec[i],"stack_habitat.tif"), overwrite = TRUE)      
}       

### create stack for each species hs loss, stable, gain (consensus = majority (50%))
spec1 <- c("Campanula_morettiana", "Festuca_austrodolomitica", "Gentiana_brentae", "Nigritella_buschmanniae",
             "Primula_tyrolensis", "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum")
for(i in seq_along(spec1)){
map45_50 <- raster(paste0("maps/reclass_50/",spec1[i],"_optimistic_sintesi.tif"))
map85_50 <- raster(paste0("maps/reclass_50/",spec1[i],"_pessimistic_sintesi.tif"))
map45_100 <- raster(paste0("maps/reclass_100/",spec1[i],"_optimistic_sintesi.tif"))
map85_100 <- raster(paste0("maps/reclass_100/",spec1[i],"_pessimistic_sintesi.tif"))
map <- stack(map45_50, map85_50, map45_100, map85_100)
df <-crop(velocity_all, map)
st2 <- stack(df, map)
names(st2) <- c("logSpeed_45","logSpeed_85", "elev", "TRI", "TCI","habitat_opt_50", "habitat_pes_50", "habitat_opt_100", "habitat_pes_100")
writeRaster(st2, paste0("habitat_topohetero_models/",spec1[i],"_stack_habitat_category.tif"), overwrite = TRUE)      
}       


# read rasters
for(i in seq_along(spec1)){}
srtm <- stack(paste0("habitat_topohetero_models/",spec1[i],"_stack_habitat_category.tif"))
names(srtm) <- c("logSpeed_45","logSpeed_85", "elev", "TRI", "TCI","habitat_opt_50", "habitat_pes_50", "habitat_opt_100", "habitat_pes_100")

r1 <- srtm[[5]] # elev temperature (nel caso tuo topographic complexity raster / o climatic velocity??)
r2 <- as.factor(srtm[[6]]) # habitat_opt elevation (nel caso tuo habitat suitabilitiy raster)

r1 <- srtm[[3]] # elev temperature (nel caso tuo topographic complexity raster / o climatic velocity??)
r2 <- srtm[[4]] # tri elevation (nel caso tuo habitat suitabilitiy raster)

r1 <- srtm[[1]] # climate_vel temperature (nel caso tuo topographic complexity raster / o climatic velocity??)
r2 <- srtm[[3]] # elev
#relazione tra le due:
plot(getValues(r2) ~ getValues(r1),pch='.', main="tci vs hs_opt")
#boxplot(getValues(r2) ~ getValues(r1), main="tci vs hs_opt_50")

### ### Pairwise Mann-Whitney
#p <- test$p.value

### Note that the values in the table are p-values comparing each
###   pair of groups.
library(ggplot2)
library(multcompView)
library(rcompanion)
## trasform stack in df
spec1 <- c("Campanula_morettiana", "Festuca_austrodolomitica", "Gentiana_brentae", "Nigritella_buschmanniae",
             "Primula_tyrolensis", "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum")
#for(i in seq_along(spec1)){
for(i in 1:8){
srtm <- stack(paste0("habitat_topohetero_models/",spec1[i],"_stack_habitat_category.tif"))
names(srtm) <- c("CCV_45", "CCV_85", "elev",  "TRI",   "TCI",
         "habitat_opt_50",  "habitat_pes_50", "habitat_opt_100", "habitat_pes_100")
b <- as.data.frame(srtm, na.rm =TRUE)
names(b) <- c("CCV_45", "CCV_85", "elev",  "TRI",   "TCI",
         "habitat_opt_50",  "habitat_pes_50", "habitat_opt_100", "habitat_pes_100")
## OPTIMISTIC
## df means and sd for opt_50
a_elev <- aggregate(elev ~ habitat_opt_50, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_tci <- aggregate(TCI ~ habitat_opt_50, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_tri <- aggregate(TRI ~ habitat_opt_50, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_ccv <- aggregate(CCV_45 ~ habitat_opt_50, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a <- cbind(a_elev$habitat_opt_50, a_elev$elev[,1:2], a_tci$TCI[,1:2],a_tri$TRI[,1:2], a_ccv$CCV_45[,1:2])
df <- as.data.frame(a)
names(df) <- c("habitat_opt_50", "elev.mean", "elev.sd",  "TCI.mean",   "TCI.sd",
         "TRI.mean",  "TRI.sd", "CCV_45.mean", "CCV_45.sd")
dodge <- position_dodge(width = 0.9)

##values
data_df <- data.frame(x = df[,1],                            # Reshape data frame
                       y = c(df[,2], df[,4], df[,6],df[,8]),
                       group = c(rep("elev.mean", nrow(df)),
                                 #rep("elev.sd", nrow(df)),
                                 rep("TCI.mean", nrow(df)),
                                 #rep("TCI.sd", nrow(df)),
                                 rep("TRI.mean", nrow(df)),
                                 #rep("TRI.sd", nrow(df)),
                                 rep("CCV_45.mean", nrow(df))))#,
                                 #rep("CCV_45.sd", nrow(df))))

data_df1 <- data.frame(x = df[,1],                            # Reshape data frame
                       y = c(df[,3], df[,5], df[,7],df[,9]),
                       group = c(#rep("elev.mean", nrow(df)),
                                 rep("elev.sd", nrow(df)),
                                 #rep("TCI.mean", nrow(df)),
                                 rep("TCI.sd", nrow(df)),
                                 #rep("TRI.mean", nrow(df)),
                                 rep("TRI.sd", nrow(df)),
                                 #rep("CCV_45.mean", nrow(df))))#,
                                 rep("CCV_45.sd", nrow(df))))

limits <- aes(ymax = data_df$y + data_df1$y,
            ymin = data_df$y - data_df1$y)
p <- ggplot(data = data_df, aes(x = as.factor(x), y = y), fill = x)
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) + 
  labs(x = spec1[i], y = NULL) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none") +
  facet_wrap(~group, scales="free")+
  scale_x_discrete(labels = c("NS", "Loss", "Gain", "Stable"))+
  theme_bw() 
  
ggsave(paste0("habitat_topohetero_models/graphs/",spec1[i],"_habitat_opt_50.pdf"))

## df means and sd for opt_100
a_elev <- aggregate(elev ~ habitat_opt_100, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_tci <- aggregate(TCI ~ habitat_opt_100, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_tri <- aggregate(TRI ~ habitat_opt_100, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_ccv <- aggregate(CCV_45 ~ habitat_opt_100, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a <- cbind(a_elev$habitat_opt_100, a_elev$elev[,1:2], a_tci$TCI[,1:2],a_tri$TRI[,1:2], a_ccv$CCV_45[,1:2])
df <- as.data.frame(a)
names(df) <- c("habitat_opt_100", "elev.mean", "elev.sd",  "TCI.mean",   "TCI.sd",
         "TRI.mean",  "TRI.sd", "CCV_45.mean", "CCV_45.sd")
dodge <- position_dodge(width = 0.9)

##values
data_df <- data.frame(x = df[,1],                            # Reshape data frame
                       y = c(df[,2], df[,4], df[,6],df[,8]),
                       group = c(rep("elev.mean", nrow(df)),
                                 #rep("elev.sd", nrow(df)),
                                 rep("TCI.mean", nrow(df)),
                                 #rep("TCI.sd", nrow(df)),
                                 rep("TRI.mean", nrow(df)),
                                 #rep("TRI.sd", nrow(df)),
                                 rep("CCV_45.mean", nrow(df))))#,
                                 #rep("CCV_45.sd", nrow(df))))

data_df1 <- data.frame(x = df[,1],                            # Reshape data frame
                       y = c(df[,3], df[,5], df[,7],df[,9]),
                       group = c(#rep("elev.mean", nrow(df)),
                                 rep("elev.sd", nrow(df)),
                                 #rep("TCI.mean", nrow(df)),
                                 rep("TCI.sd", nrow(df)),
                                 #rep("TRI.mean", nrow(df)),
                                 rep("TRI.sd", nrow(df)),
                                 #rep("CCV_45.mean", nrow(df))))#,
                                 rep("CCV_45.sd", nrow(df))))

limits <- aes(ymax = data_df$y + data_df1$y,
            ymin = data_df$y - data_df1$y)
p <- ggplot(data = data_df, aes(x = as.factor(x), y = y), fill = x)
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) + 
  labs(x = spec1[i], y = NULL) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none") +
  facet_wrap(~group, scales="free")+
  scale_x_discrete(labels = c("NS", "Loss", "Gain", "Stable"))+
  theme_bw() 
  
ggsave(paste0("habitat_topohetero_models/graphs/",spec1[i],"_habitat_opt_100.pdf"))

}

## PESSIMISTIC -----------
## df means and sd for pes_50
a_elev <- aggregate(elev ~ habitat_pes_50, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_tci <- aggregate(TCI ~ habitat_pes_50, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_tri <- aggregate(TRI ~ habitat_pes_50, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_ccv <- aggregate(CCV_45 ~ habitat_pes_50, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a <- cbind(a_elev$habitat_pes_50, a_elev$elev[,1:2], a_tci$TCI[,1:2],a_tri$TRI[,1:2], a_ccv$CCV_45[,1:2])
df <- as.data.frame(a)
names(df) <- c("habitat_pes_50", "elev.mean", "elev.sd",  "TCI.mean",   "TCI.sd",
         "TRI.mean",  "TRI.sd", "CCV_45.mean", "CCV_45.sd")
dodge <- position_dodge(width = 0.9)

##values
data_df <- data.frame(x = df[,1],                            # Reshape data frame
                       y = c(df[,2], df[,4], df[,6],df[,8]),
                       group = c(rep("elev.mean", nrow(df)),
                                 #rep("elev.sd", nrow(df)),
                                 rep("TCI.mean", nrow(df)),
                                 #rep("TCI.sd", nrow(df)),
                                 rep("TRI.mean", nrow(df)),
                                 #rep("TRI.sd", nrow(df)),
                                 rep("CCV_45.mean", nrow(df))))#,
                                 #rep("CCV_45.sd", nrow(df))))

data_df1 <- data.frame(x = df[,1],                            # Reshape data frame
                       y = c(df[,3], df[,5], df[,7],df[,9]),
                       group = c(#rep("elev.mean", nrow(df)),
                                 rep("elev.sd", nrow(df)),
                                 #rep("TCI.mean", nrow(df)),
                                 rep("TCI.sd", nrow(df)),
                                 #rep("TRI.mean", nrow(df)),
                                 rep("TRI.sd", nrow(df)),
                                 #rep("CCV_45.mean", nrow(df))))#,
                                 rep("CCV_45.sd", nrow(df))))

limits <- aes(ymax = data_df$y + data_df1$y,
            ymin = data_df$y - data_df1$y)
p <- ggplot(data = data_df, aes(x = as.factor(x), y = y), fill = x)
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) + 
  labs(x = spec1[i], y = NULL) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none") +
  facet_wrap(~group, scales="free")+
  scale_x_discrete(labels = c("NS", "Loss", "Gain", "Stable"))+
  theme_bw() 
  
ggsave(paste0("habitat_topohetero_models/graphs/",spec1[i],"_habitat_pes_50.pdf"))

## df means and sd for pes_100
a_elev <- aggregate(elev ~ habitat_pes_100, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_tci <- aggregate(TCI ~ habitat_pes_100, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_tri <- aggregate(TRI ~ habitat_pes_100, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a_ccv <- aggregate(CCV_45 ~ habitat_pes_100, data=b, function(x) c(mean = mean(x), sd = sd(x)))
a <- cbind(a_elev$habitat_pes_100, a_elev$elev[,1:2], a_tci$TCI[,1:2],a_tri$TRI[,1:2], a_ccv$CCV_45[,1:2])
df <- as.data.frame(a)
names(df) <- c("habitat_pes_100", "elev.mean", "elev.sd",  "TCI.mean",   "TCI.sd",
         "TRI.mean",  "TRI.sd", "CCV_45.mean", "CCV_45.sd")
dodge <- position_dodge(width = 0.9)

##values
data_df <- data.frame(x = df[,1],                            # Reshape data frame
                       y = c(df[,2], df[,4], df[,6],df[,8]),
                       group = c(rep("elev.mean", nrow(df)),
                                 #rep("elev.sd", nrow(df)),
                                 rep("TCI.mean", nrow(df)),
                                 #rep("TCI.sd", nrow(df)),
                                 rep("TRI.mean", nrow(df)),
                                 #rep("TRI.sd", nrow(df)),
                                 rep("CCV_45.mean", nrow(df))))#,
                                 #rep("CCV_45.sd", nrow(df))))

data_df1 <- data.frame(x = df[,1],                            # Reshape data frame
                       y = c(df[,3], df[,5], df[,7],df[,9]),
                       group = c(#rep("elev.mean", nrow(df)),
                                 rep("elev.sd", nrow(df)),
                                 #rep("TCI.mean", nrow(df)),
                                 rep("TCI.sd", nrow(df)),
                                 #rep("TRI.mean", nrow(df)),
                                 rep("TRI.sd", nrow(df)),
                                 #rep("CCV_45.mean", nrow(df))))#,
                                 rep("CCV_45.sd", nrow(df))))

limits <- aes(ymax = data_df$y + data_df1$y,
            ymin = data_df$y - data_df1$y)
p <- ggplot(data = data_df, aes(x = as.factor(x), y = y), fill = x)
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) + 
  labs(x = spec1[i], y = NULL) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none") +
  facet_wrap(~group, scales="free")+
  scale_x_discrete(labels = c("NS", "Loss", "Gain", "Stable"))+
  theme_bw() 
  
ggsave(paste0("habitat_topohetero_models/graphs/",spec1[i],"_habitat_pes_100.pdf"))

}

#############################
##### other for loop
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$elev, factor(b$habitat_opt_50), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters_",spec1[i],"_habitat_opt_50.txt"), sep = "\t",
            row.names = FALSE)
