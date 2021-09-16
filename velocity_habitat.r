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

## calculate topographic heterogeneity
## add elevation slope eastness northness and PC1 and PC2 topo al df_habitat_velocity
## fai df con aree parco aggiunte

dem <- raster("env_predictors/DTM_50.tif")
names(dem) <- "elev"
summary(dem)
library(spatialEco)
#sd_elev <- raster.deviation(dem, type = "mean", s = 3, global = FALSE)
#sd_elev_df <- as.data.frame(sd_elev, xy = T)
TCI <- raster("env_predictors/intrinsic_topographic_complexity.tif")
names(TCI) <- "TCI"
TCI[TCI == NA] <- 0
tci1 <- setExtent(TCI, extent(dem), keepres=T)
### https://www.rdocumentation.org/packages/spatialEco/versions/1.3-2/topics/tri
library(spatialEco)
# a <- tri(dem1, s=c(3,3), exact = T)
# names(a) <- "TRI"
# writeRaster(a, filename = "env_predictors/TRI.tif")
a <- raster("env_predictors/TRI.tif")
st <- stack(dem, a, tci1)
# heterogeneity_df <- as.data.frame(st, xy = T, na.rm = T)

# head(heterogeneity_df)


spec <- c("Campanula.morettiana", "Festuca.austrodolomitica", "Gentiana.brentae", "Nigritella.buschmanniae",
             "Primula.tyrolensis", "Rhizobotrya.alpina", "Saxifraga.facchinii", "Sempervivum.dolomiticum")
velocity45 <- raster("results_climate_change_velocity/future/logSpeed_45.tif")
velocity85 <- raster("results_climate_change_velocity/future/logSpeed_85.tif")
st1 <- crop(st, velocity45)
velocity_all <- stack(velocity45, velocity85, st1)
df_habitat_velocity_100cons <- data.frame(x = NA, y = NA, logSpeed_45 = NA, logSpeed_85 = NA, elev = NA, TRI = NA, TCI = NA, habitat_opt = NA, habitat_pes = NA, species = NA)

### df_habita_velocity for 100% consensus models continuos hs differenza percentuale
for(i in seq_along(spec)){
map45 <- raster(paste0("habitat_model/habitat_model/",spec[i],"_dif45.tif"))
map85 <- raster(paste0("habitat_model/habitat_model/",spec[i],"_dif85.tif"))
map <- stack(map45, map85)

df <-crop(velocity_all, map)
st <- stack(df, map)
df_spec <- as.data.frame(st, xy = T, rownames = T, na.rm = T)
df_spec$species <- spec[i]
names(df_spec) <- c("x", "y", "logSpeed_45","logSpeed_85", "elev", "TRI", "TCI","habitat_opt", "habitat_pes", "species")

assign("df_habitat_velocity_100cons", bind_rows(df_habitat_velocity_100cons, df_spec))
       
}       


#head(df_spec)
write.csv(df_habitat_velocity_100cons, "results_climate_change_velocity/df_habitat_velocity_100cons.csv")

### df_habita_velocity for 100% consensus models continuos hs differenza percentuale
df_habitat_velocity_100cons_int <- data.frame(x = NA, y = NA, logSpeed_45 = NA, logSpeed_85 = NA, elev = NA, TRI = NA, TCI = NA, habitat_opt = NA, habitat_pes = NA, species = NA)
for(i in seq_along(spec)){
  map45 <- raster(paste0("habitat_model/",spec[i],"_dif45_int.tif"))
  map85 <- raster(paste0("habitat_model/",spec[i],"_dif85_int.tif"))
  map <- stack(map45, map85)
  
  df <-crop(velocity_all, map)
  st <- stack(df, map)
  df_spec <- as.data.frame(st, xy = T, rownames = T, na.rm = T)
  df_spec$species <- spec[i]
  names(df_spec) <- c("x", "y", "logSpeed_45","logSpeed_85", "elev", "TRI", "TCI","habitat_opt", "habitat_pes", "species")
  
  assign("df_habitat_velocity_100cons_int", bind_rows(df_habitat_velocity_100cons_int, df_spec))
  
}       

#head(df_spec)
write.csv(df_habitat_velocity_100cons_int, "results_climate_change_velocity/df_habitat_velocity_100cons_int.csv")





### load df habitat velocity
df_habitat_velocity <- read.csv("/data/models/results_climate_change_velocity/df_habitat_velocity_100cons.csv")

# ## remove 0 and NA from habitats
# df_habitat_velocity <- df_habitat_velocity[!(df_habitat_velocity$habitat_opt == 0 | df_habitat_velocity$habitat_pes == 0),]
# df_habitat_velocity <- na.omit(df_habitat_velocity)
# df_habitat_velocity$habitat_opt <- factor(df_habitat_velocity$habitat_opt)
# df_habitat_velocity$habitat_pes <- factor(df_habitat_velocity$habitat_pes)
#library(Hmisc)
#library(qwraps2)
str(df_habitat_velocity)
summary(df_habitat_velocity)

# ## SimultaneousAutoregressive Spatial Models in hglm "SAR"
# spec <- c("Campanula_morettiana", "Festuca_austrodolomitica", "Gentiana_brentae", "Nigritella_buschmanniae",
#              "Primula_tyrolensis", "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum")
# 
# for(i in seq_along(spec)){
# df_habitat_velocity_spec <- subset(df_habitat_velocity, species == spec[i])
# model_opt <- summary(glm(logSpeed_45 ~ habitat_opt, data=df_habitat_velocity_spec ))
# model_pes <- summary(glm(logSpeed_85 ~ habitat_pes, data=df_habitat_velocity_spec ))
# print(model_opt)
# print(model_pes)
# }
# 
# for(i in seq_along(spec)){
# df_habitat_velocity_spec <- subset(df_habitat_velocity, species == spec[i])
# test_opt <- posthoc.kruskal.nemenyi.test(logSpeed_45 ~ habitat_opt, data = df_habitat_velocity_spec, dist="Tukey")
# test_pes <- posthoc.kruskal.nemenyi.test(logSpeed_85 ~ habitat_pes, data = df_habitat_velocity_spec, dist="Tukey")
# print(test_opt)
# print(test_pes)
# }
# 
# 
# library(ggplot2)
# library(ggbiplot)
# library(tidyverse)
# library(forcats)
# 
# myTheme <- theme(
#   panel.background = element_rect(fill = "white", colour = "black"), 
#   panel.grid.major =  element_blank(),
#   panel.grid.minor = element_blank(),
#   axis.text = element_text(color = "black", size=14),
#   axis.text.x = element_text(color = "black", size=14),
# )
# #levels(df_habitat_parks$species) <- c("Cm", "Fa", "Gb", "Nb", "Pt", "Ra", "Sf", "Sd")
# ### optimistic
# 
# 
# b <-ggplot(data =  df_habitat_velocity, aes(x=fct_reorder(as.factor(habitat_opt)), y= logSpeed_45)) +
#   geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)+
#   #scale_fill_manual(values=c( "grey60", "white")) +
#   labs(x = "Habitat Suitability")+
#   ## p value in scenario per parks
#   #stat_compare_means(aes(as_label("p.value")), method = "kruskal")+
#   myTheme +
#   scale_y_continuous("climate velocity", breaks = c(0,0.5,1)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
#   facet_wrap(~species, nrow = 4)
# #ggsave(b, filename="models/Protected_areas/habitat_suitability_parks_opt.png", dpi = 300)
# 
# ## comaprazione per classi di heterogeneity (selezione random valori 50+) random draw piú repliche, bootstrapping
# ## confrontare distribuzione eterogneitá nelle classi di habitat (velocity ~ topo_heterogeneity + factor(habitat))
# ## subset campanula
# cm <-  subset(df_habitat_velocity, species == "Campanula_morettiana") 
# cm
# d <-ggplot(data =  cm, aes(x= TRI, y= logSpeed_45)) +
#   geom_point()+
#   #scale_fill_manual(values=c( "grey60", "white")) +
#   labs(x = "Topographic Heterogeneity", y = "Climate Velocity")+
#   ## p value in scenario per parks
#   #stat_compare_means(aes(as_label("p.value")), method = "kruskal")+
#   myTheme +
#   facet_wrap(~habitat_opt, nrow = 3)

## BY SPECIES sax fac ####################################################
sf <-  subset(df_habitat_velocity, species == "Saxifraga_facchinii") 
sf

# e <-ggplot(data =  sf, aes(x= TRI, y= logSpeed_45)) +
#   geom_point()+
#   #scale_fill_manual(values=c( "grey60", "white")) +
#   labs(x = "Topographic Heterogeneity", y = "Climate Velocity")+
#   ## p value in scenario per parks
#   #stat_compare_means(aes(as_label("p.value")), method = "kruskal")+
#   myTheme +
#   facet_wrap(~habitat_opt, ncol = 3)

###normalize
library(bestNormalize)

velocity45_norm <- bestNormalize(sf$logSpeed_45)
velocity85_norm <- bestNormalize(sf$logSpeed_85)
TRI_norm <- bestNormalize(sf$TRI)

# ## random subset
# random_ss <- sf[sample(nrow(sf), 10000), ]
# ## check spatial autocorrelation
# library(nlme)
# m2 <- gls(log1p(logSpeed_45) ~ TRI , data = random_ss)
# vario2 <- Variogram(m2, form = ~x + y, resType = "pearson")
# plot(vario2, smooth = TRUE, ylim = c(0, 1.2))

### Prepare DF for each species with only loss = 1 and stable = 0
sf_loss_stable <- sf[!sf$habitat_opt == 2, ] 
sf_loss_stable$habitat_opt <- as.integer(sf_loss_stable$habitat_opt)

## transform stable from 3 to 0 ->  STABLE = 0 ####################
sf_loss_stable$habitat_opt[sf_loss_stable$habitat_opt == 3] <- 0 
sf_loss_stable$habitat_opt <- as.factor(sf_loss_stable$habitat_opt)
sf_loss_stable <- na.omit(sf_loss_stable)
sf_loss_stable_opt <- sf_loss_stable[,c(2,3,4,6,7,8,10)]
sf_loss_stable_opt$species <- factor(sf_loss_stable_opt$species)

# sf_loss_stable_opt$habitat_opt[which(is.nan(sf_loss_stable_opt$habitat_opt))] = NA
# sf_loss_stable_opt$habitat_opt[which(sf_loss_stable_opt$habitat_opt==Inf)] = NA
# sf_loss_stable_opt <- na.omit(sf_loss_stable_opt)

# library(hglm)
# ################## SIMULTANEOUS AUTOREGRESSIVE (SAR) MODELS IN R ####################
# #####################################################################################
# 
# #------------------   Define coordinates, neighbourhoods, and spatial weights ----------------------------#
# 
# #######################
# #Make a matrix of coordinates (X and Y coordinates)
# coords<-data.frame(x=as.numeric(sf_loss_stable$x),y=as.numeric(sf_loss_stable$y))
# coords<-as.matrix(coords)
# 
# ###############################
# 
# #Define neighbourhood (here distance )
# library(spatialreg)
# nb1.5<-dnearneigh(coords,100,500,longlat = FALSE)
# 
# 
# #######################
# #Spatial weights, illustrated with coding style "W" (row standardized)
# set.ZeroPolicyOption(TRUE)
# nb1.5.w<-nb2listw(nb1.5, glist=NULL, style="W", zero.policy=TRUE)
# 
# 
# #######################
# #SARerr model with neighbourhood distance 1.5 and coding style "W"
# 
# #Specify SARerr model for ER
# sem.nb1.5<-errorsarlm(ma1, listw=nb1.5.w, na.exclude)
# summary(sem.nb1.5, Nagelkerke=TRUE, adj.se=TRUE)
# 
# m2 <- sacsarlm(ma1,  nb1.5.w, listw2 = NULL, na.action, type="sac",
#          method = "eigen", quiet = NULL, zero.policy = NULL, tol.solve = 1e-10,
#          llprof=NULL, interval1=NULL, interval2=NULL, trs1=NULL, trs2=NULL,
#          control = list())
# #########################################

# ### gee Spatial generalised estimating equations (GEE) ###################
# 
# #### geepack
# library(gee)
# library(geepack)
# 
# #### create clusters of contiguos coordinates at 1 km #####
# library(fields)
# threshold.in.km <- 1
# ## df utm coords
# coors <- sf_loss_stable_opt[,1:2]
# ##trasfrom in lat/long df
# library(rgdal)
# sputm <- SpatialPoints(coors, proj4string=CRS("+proj=utm +zone=32 +datum=WGS84")) 
# spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
# coors1 <- as.data.frame(spgeo, row.names = T)
# coors1$ID <- seq(from = 1, to = nrow(coors1), by = 1)
# ## 1% rows random subset
# coors_ss <- coors1[sample(nrow(coors1), round(nrow(coors1)*1/100, digits = 0)), ]
# 
# #distance matrix
# dist.in.km.matrix <- rdist.earth(coors_ss,miles = F,R=6371)
# #clustering
# fit <- hclust(as.dist(dist.in.km.matrix), method = "single")
# clusters <- cutree(fit,h = threshold.in.km)
# id1 <- as.vector(clusters)
# ##make the subset on the random coords subset
# sf_loss_stable_opt$ID <- seq(from = 1, to = nrow(sf_loss_stable_opt), by = 1)
# ss <- as.vector(coors_ss$ID)
# df_ss <- subset(sf_loss_stable_opt, ID %in% ss)
# 
# 
# #### GEE 
# ## formula ordered model  corstr=("exchangeable")
# mf2_or <- formula(ordered(habitat_opt) ~ TRI + logSpeed_45)
# 
# gee1 <- ordgee(mf2_or, data=df_ss, id = id1, mean.link = "logit",  corstr=("independence"))
# summary(gee1)
# 
# 
# ## formula model  
# mf2 <- formula(habitat_opt ~ TRI + logSpeed_45)
# 
# gee2 <- summary(geeglm(mf2, data=df_ss, id = id1,  family=binomial(link = "logit"), corstr="fixed"))
# 

##### GAM ---------
library(mgcv)
mf1 <- formula(habitat_opt ~ TRI + TCI + elev + logSpeed_45 + TRI*elev + TRI*logSpeed_45 + TCI*elev + TCI*logSpeed_45 +elev*logSpeed_45 + s(x, y))
# ## 1% rows random subset
# df_ss1 <- sf_loss_stable_opt[sample(nrow(sf_loss_stable_opt), round(nrow(sf_loss_stable_opt)*1/100, digits = 0)), ]
# summary(gam.bino <- gam(mf1, data=df_ss1, family=binomial))
# 
# ### bam no subset, Generalized additive models for very large datasets
# summary(m_bam <- bam(mf1, family=binomial(), data= sf_loss_stable_opt))

## in parallel... 
### load df habitat velocity
#df_habitat_velocity <- read.csv("/data/models/results_climate_change_velocity/df_habitat_velocity.csv")
df_habitat_velocity <- read.csv("/data/models/results_climate_change_velocity/df_habitat_velocity_100cons.csv")

# ## remove NA from habitats
# df_habitat_velocity <- df_habitat_velocity[!(df_habitat_velocity$habitat_opt == 0 | df_habitat_velocity$habitat_pes == 0),]
df_habitat_velocity <- na.omit(df_habitat_velocity)
# df_habitat_velocity$habitat_opt <- factor(df_habitat_velocity$habitat_opt)
# df_habitat_velocity$habitat_pes <- factor(df_habitat_velocity$habitat_pes)
spec <- c("Campanula.morettiana", "Festuca.austrodolomitica", "Gentiana.brentae", "Nigritella.buschmanniae",
          "Primula.tyrolensis", "Rhizobotrya.alpina", "Saxifraga.facchinii", "Sempervivum.dolomiticum")
library(mgcv)
library(parallel)
#library(corrgram)

#### continuous hs ----------------
### OPTIMISTIC with autocorrelation s(x,y)
mf1 <- formula(habitat_opt ~ TRI + TCI + elev + logSpeed_45 + TRI*elev + TRI*logSpeed_45 + TCI*elev + TCI*logSpeed_45 +elev*logSpeed_45 + s(x, y))

for(i in seq_along(spec)){

sf <-  subset(df_habitat_velocity, species == spec[i]) 
### Prepare DF for each species with only loss = 1 and stable = 0
sf_loss_stable <- sf[!sf$habitat_opt == 2, ] 
sf_loss_stable$habitat_opt <- as.integer(sf_loss_stable$habitat_opt)

## transform stable from 3 to 0 ->  STABLE = 0 ####################
sf_loss_stable$habitat_opt[sf_loss_stable$habitat_opt == 3] <- 0 
sf_loss_stable$habitat_opt <- as.factor(sf_loss_stable$habitat_opt)
sf_loss_stable <- na.omit(sf_loss_stable)
sf_loss_stable_opt <- sf_loss_stable[,c(2,3,4,6,7,8,10)]
sf_loss_stable_opt$species <- factor(sf_loss_stable_opt$species)
cl <- makeCluster(4)
system.time(b3 <- bam(mf1, family=binomial(), data= sf_loss_stable_opt, chunk.size=5000,cluster=cl ))

saveRDS(b3, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_models_opt_100.rds"))
model_resid <- residuals.gam(b3)
saveRDS(model_resid, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_residual_opt_100.rds"))
stopCluster(cl)
## correlogram among residuals, x and y
df_res <- as.data.frame(cbind(model_resid, sf_loss_stable_opt$x, sf_loss_stable_opt$y))
names(df_res) <- c("residuals", "x", "y")
# cg <- corrgram(df_res[1:1000,] , order=TRUE, lower.panel=panel.ellipse,
#          upper.panel=panel.pts, text.panel=panel.txt,
#          diag.panel=panel.minmax,
#          main="Residual, x and y correlogram")
cg <- cor(df_res)
write.table(cg, file = paste0("habitat_topohetero_models/opt/",spec[i],"_cor_resid_opt_100.txt"), sep ="\t", row.names = T)

}

### PESSIMISTIC with autocorrelation s(x,y)

mf1 <- formula(habitat_pes ~ TRI + elev + logSpeed_85 + TRI*elev + TRI*logSpeed_85 + elev*logSpeed_85 + s(x, y))
library(parallel)


for(i in seq_along(spec)){
  
  sf <-  subset(df_habitat_velocity, species == spec[i]) 
  ### Prepare DF for each species with only loss = 1 and stable = 0
  sf_loss_stable <- sf[!sf$habitat_pes== 2, ] 
  sf_loss_stable$habitat_pes<- as.integer(sf_loss_stable$habitat_pes)
  
  ## transform stable from 3 to 0 ->  STABLE = 0 ####################
  sf_loss_stable$habitat_pes[sf_loss_stable$habitat_pes== 3] <- 0 
  sf_loss_stable$habitat_pes<- as.factor(sf_loss_stable$habitat_pes)
  sf_loss_stable <- na.omit(sf_loss_stable)
  sf_loss_stable_pes <- sf_loss_stable[,c(2,3,5,6,7,9,10)]
  sf_loss_stable_pes$species <- factor(sf_loss_stable_pes$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=binomial(), data= sf_loss_stable_pes, chunk.size=5000,cluster=cl ))
  saveRDS(b3, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_models_pes.rds"))
  model_resid <- residuals.gam(b3)
  saveRDS(model_resid, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_residuals_pes.rds"))
  stopCluster(cl)
  ## correlogram among resiudls, x and y
  df_res <- as.data.frame(cbind(model_resid, sf_loss_stable_pes$x, sf_loss_stable_pes$y))
  names(df_res) <- c("residuals", "x", "y")
  #jpeg(paste0(spec[i],"_corplot_pes.jpeg"), quality = 75)
  # cg <- corrgram(df_res , order=TRUE, lower.panel=panel.ellipse,
  #                upper.panel=panel.pts, text.panel=panel.txt,
  #                diag.panel=panel.minmax,
  #                main="Residual, x and y correlogram") 
  cg <- cor(df_res)
  write.table(cg, file = paste0("habitat_topohetero_models/pes/",spec[i],"_cor_resid_pes.txt"), sep ="\t", row.names = T)
  
}


### OPTIMISTIC without spatial autocorrelation
mf1 <- formula(habitat_opt ~ TRI + elev + logSpeed_45 + TRI*elev + TRI*logSpeed_45 + elev*logSpeed_45)


for(i in seq_along(spec)){
  
  sf <-  subset(df_habitat_velocity, species == spec[i]) 
  ### Prepare DF for each species with only loss = 1 and stable = 0
  sf_loss_stable <- sf[!sf$habitat_opt == 2, ] 
  sf_loss_stable$habitat_opt <- as.integer(sf_loss_stable$habitat_opt)
  
  ## transform stable from 3 to 0 ->  STABLE = 0 ####################
  sf_loss_stable$habitat_opt[sf_loss_stable$habitat_opt == 3] <- 0 
  sf_loss_stable$habitat_opt <- as.factor(sf_loss_stable$habitat_opt)
  sf_loss_stable <- na.omit(sf_loss_stable)
  sf_loss_stable_opt <- sf_loss_stable[,c(2,3,4,6,7,8,10)]
  sf_loss_stable_opt$species <- factor(sf_loss_stable_opt$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=binomial(), data= sf_loss_stable_opt, chunk.size=5000,cluster=cl ))
  
  saveRDS(b3, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_models_opt_no_aut.rds"))
  model_resid <- residuals.gam(b3)
  saveRDS(model_resid, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_residual_opt_no_aut.rds"))
  stopCluster(cl)
  ## correlogram among residuals, x and y
  df_res <- as.data.frame(cbind(model_resid, sf_loss_stable_opt$x, sf_loss_stable_opt$y))
  names(df_res) <- c("residuals", "x", "y")
  # cg <- corrgram(df_res[1:1000,] , order=TRUE, lower.panel=panel.ellipse,
  #          upper.panel=panel.pts, text.panel=panel.txt,
  #          diag.panel=panel.minmax,
  #          main="Residual, x and y correlogram")
  cg <- cor(df_res)
  write.table(cg, file = paste0("habitat_topohetero_models/opt/",spec[i],"_cor_resid_opt_no_aut.txt"), sep ="\t", row.names = T)
  
}

### PESSIMISTIC without spatial autocorrelation

mf1 <- formula(habitat_pes ~ TRI + elev + logSpeed_85 + TRI*elev + TRI*logSpeed_85 + elev*logSpeed_85 )
library(parallel)


for(i in seq_along(spec)){
  
  sf <-  subset(df_habitat_velocity, species == spec[i]) 
  ### Prepare DF for each species with only loss = 1 and stable = 0
  sf_loss_stable <- sf[!sf$habitat_pes== 2, ] 
  sf_loss_stable$habitat_pes<- as.integer(sf_loss_stable$habitat_pes)
  
  ## transform stable from 3 to 0 ->  STABLE = 0 ####################
  sf_loss_stable$habitat_pes[sf_loss_stable$habitat_pes== 3] <- 0 
  sf_loss_stable$habitat_pes<- as.factor(sf_loss_stable$habitat_pes)
  sf_loss_stable <- na.omit(sf_loss_stable)
  sf_loss_stable_pes <- sf_loss_stable[,c(2,3,5,6,7,9,10)]
  sf_loss_stable_pes$species <- factor(sf_loss_stable_pes$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=binomial(), data= sf_loss_stable_pes, chunk.size=5000,cluster=cl ))
  saveRDS(b3, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_models_pes_no_aut.rds"))
  model_resid <- residuals.gam(b3)
  saveRDS(model_resid, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_residuals_pes_no_aut.rds"))
  stopCluster(cl)
  ## correlogram among resiudls, x and y
  df_res <- as.data.frame(cbind(model_resid, sf_loss_stable_pes$x, sf_loss_stable_pes$y))
  names(df_res) <- c("residuals", "x", "y")
  #jpeg(paste0(spec[i],"_corplot_pes.jpeg"), quality = 75)
  # cg <- corrgram(df_res , order=TRUE, lower.panel=panel.ellipse,
  #                upper.panel=panel.pts, text.panel=panel.txt,
  #                diag.panel=panel.minmax,
  #                main="Residual, x and y correlogram") 
  cg <- cor(df_res)
  write.table(cg, file = paste0("habitat_topohetero_models/pes/",spec[i],"_cor_resid_pes_no_aut.txt"), sep ="\t", row.names = T)
  
}

#### loss gain 1 0 ----------------
### OPTIMISTIC with autocorrelation s(x,y)
mf1 <- formula(habitat_opt ~ TRI + elev + logSpeed_45 + TRI*elev + TRI*logSpeed_45 + elev*logSpeed_45 + s(x, y))

for(i in seq_along(spec)){
  
  sf <-  subset(df_habitat_velocity, species == spec[i]) 
  ### Prepare DF for each species with only loss = 1 and gain = 0
  sf_loss_gain <- sf[!sf$habitat_opt == 3, ] 
  sf_loss_gain$habitat_opt <- as.integer(sf_loss_gain$habitat_opt)
  
  ## transform gain from 3 to 0 ->  gain = 0 ####################
  sf_loss_gain$habitat_opt[sf_loss_gain$habitat_opt == 2] <- 0 
  sf_loss_gain$habitat_opt <- as.factor(sf_loss_gain$habitat_opt)
  sf_loss_gain <- na.omit(sf_loss_gain)
  sf_loss_gain_opt <- sf_loss_gain[,c(2,3,4,6,7,8,10)]
  sf_loss_gain_opt$species <- factor(sf_loss_gain_opt$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=binomial(), data= sf_loss_gain_opt, chunk.size=5000,cluster=cl ))
  
  saveRDS(b3, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_models_gain_opt.rds"))
  model_resid <- residuals.gam(b3)
  saveRDS(model_resid, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_residual_gain_opt.rds"))
  stopCluster(cl)
  ## correlogram among residuals, x and y
  df_res <- as.data.frame(cbind(model_resid, sf_loss_gain_opt$x, sf_loss_gain_opt$y))
  names(df_res) <- c("residuals", "x", "y")
  # cg <- corrgram(df_res[1:1000,] , order=TRUE, lower.panel=panel.ellipse,
  #          upper.panel=panel.pts, text.panel=panel.txt,
  #          diag.panel=panel.minmax,
  #          main="Residual, x and y correlogram")
  cg <- cor(df_res)
  write.table(cg, file = paste0("habitat_topohetero_models/opt/",spec[i],"_cor_resid_gain_opt.txt"), sep ="\t", row.names = T)
  
}

### PESSIMISTIC with autocorrelation s(x,y)

mf1 <- formula(habitat_pes ~ TRI + elev + logSpeed_85 + TRI*elev+ TRI*logSpeed_85 + elev*logSpeed_85 + s(x, y))
library(parallel)


for(i in seq_along(spec)){
  
  sf <-  subset(df_habitat_velocity, species == spec[i]) 
  ### Prepare DF for each species with only loss = 1 and gain = 0
  sf_loss_gain <- sf[!sf$habitat_pes== 3, ] 
  sf_loss_gain$habitat_pes<- as.integer(sf_loss_gain$habitat_pes)
  
  ## transform gain from 3 to 0 ->  gain = 0 ####################
  sf_loss_gain$habitat_pes[sf_loss_gain$habitat_pes== 2] <- 0 
  sf_loss_gain$habitat_pes<- as.factor(sf_loss_gain$habitat_pes)
  sf_loss_gain <- na.omit(sf_loss_gain)
  sf_loss_gain_pes <- sf_loss_gain[,c(2,3,5,6,7,9,10)]
  sf_loss_gain_pes$species <- factor(sf_loss_gain_pes$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=binomial(), data= sf_loss_gain_pes, chunk.size=5000,cluster=cl ))
  saveRDS(b3, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_models_gain_pes.rds"))
  model_resid <- residuals.gam(b3)
  saveRDS(model_resid, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_residuals_gain_pes.rds"))
  stopCluster(cl)
  ## correlogram among resiudls, x and y
  df_res <- as.data.frame(cbind(model_resid, sf_loss_gain_pes$x, sf_loss_gain_pes$y))
  names(df_res) <- c("residuals", "x", "y")
  #jpeg(paste0(spec[i],"_corplot_pes.jpeg"), quality = 75)
  # cg <- corrgram(df_res , order=TRUE, lower.panel=panel.ellipse,
  #                upper.panel=panel.pts, text.panel=panel.txt,
  #                diag.panel=panel.minmax,
  #                main="Residual, x and y correlogram") 
  cg <- cor(df_res)
  write.table(cg, file = paste0("habitat_topohetero_models/pes/",spec[i],"_cor_resid_gain_pes.txt"), sep ="\t", row.names = T)
  
}



# ######## PLOT GAM models ####
# 
# library(mgcViz)
# viz <- getViz(Campanula_morettiana_BAM_models_opt)
# print(plot(viz, allTerms = T), pages = 1)

# The same model selection and model validation
# steps should be applied as we did with logistic regression and discussed in previous
# sections. The anova(P2), summary(P2), and plot(P2) commands can
# be used.
mt <- anova(Campanula_morettiana_BAM_models_opt)
mt

sm <- summary(Campanula_morettiana_BAM_models_opt)
sm$p.table

# ### tables
# library(xtable)
# xtable(sm, caption="ANOVA table for GAM", digits=4) 

# for linux:
install.packages("ncf") #,contriburl="http://asi23.ent.psu.edu/onb1/R/src")
library(ncf)
?correlog
correlog1.1 <- correlog(sf_loss_stable_pes$x, sf_loss_stable_pes$y, residuals(Sempervivum_dolomiticum_BAM_models_pes), na.rm=T, increment=1, resamp=0)

# make a map of the residuals:
plot(sf_loss_stable_pes$x, sf_loss_stable_pes$y, col=c("blue", "red")[sign(resid(Sempervivum_dolomiticum_BAM_models_pes))/2+1.5], pch=19,cex=abs(resid(Sempervivum_dolomiticum_BAM_models_pes))/max(resid(Sempervivum_dolomiticum_BAM_models_pes))*2, xlab="geographical x-coordinates", ylab="geographical y-coordinates")

# ## MULTINOMIAL, not working with BAM --------------
# summary(m_bam <- gam(mf1, family=multinom(K=3), data= sf_opt))
# 
# ## in parallel... 
# ### load df habitat velocity
# df_habitat_velocity <- read.csv("/data/models/results_climate_change_velocity/df_habitat_velocity.csv")
# 
# ## remove 0 and NA from habitats
# df_habitat_velocity <- df_habitat_velocity[!(df_habitat_velocity$habitat_opt == 0 | df_habitat_velocity$habitat_pes == 0),]
# df_habitat_velocity <- na.omit(df_habitat_velocity)
# df_habitat_velocity$habitat_opt <- factor(df_habitat_velocity$habitat_opt)
# df_habitat_velocity$habitat_pes <- factor(df_habitat_velocity$habitat_pes)
# spec <- c("Campanula_morettiana", "Festuca_austrodolomitica", "Gentiana_brentae", "Nigritella_buschmanniae",
#           "Primula_tyrolensis", "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum")
# library(mgcv)
# library(parallel)
# #library(corrgram)
# 
# ### OPTIMISTIC with autocorrelation s(x,y)
# mf1 <- formula(habitat_opt ~ TRI + elev + logSpeed_45 + TRI*logSpeed_45 + elev*logSpeed_45 + s(x, y))
# 
# for(i in seq_along(spec)){
#   
#   sf <-  subset(df_habitat_velocity, species == spec[i]) 
#   ### Prepare DF for each species with loss = 1, gain = 2, stable = 3
#   sf_opt <- sf[,c(2,3,4,6,7,8,10)]
#   sf_opt$species <- factor(sf_opt$species)
#   cl <- makeCluster(4)
#   system.time(b3 <- gam(mf1, family=multinom(), data= sf_opt, chunk.size=5000,cluster=cl ))
#   
#   saveRDS(b3, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_multi_models_opt.rds"))
#   model_resid <- residuals.gam(b3)
#   saveRDS(model_resid, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_multi_residual_opt.rds"))
#   stopCluster(cl)
#   ## correlogram among residuals, x and y
#   df_res <- as.data.frame(cbind(model_resid, sf_opt$x, sf_opt$y))
#   names(df_res) <- c("residuals", "x", "y")
#   # cg <- corrgram(df_res[1:1000,] , order=TRUE, lower.panel=panel.ellipse,
#   #          upper.panel=panel.pts, text.panel=panel.txt,
#   #          diag.panel=panel.minmax,
#   #          main="Residual, x and y correlogram")
#   cg <- cor(df_res)
#   write.table(cg, file = paste0("habitat_topohetero_models/opt/",spec[i],"_cor_multi_resid_opt.txt"), sep ="\t", row.names = T)
#   
# }
# 
# ### PESSIMISTIC with autocorrelation s(x,y)
# 
# mf1 <- formula(habitat_pes ~ TRI + elev + logSpeed_85 + TRI*logSpeed_85 + elev*logSpeed_85 + s(x, y))
# library(parallel)
# 
# 
# for(i in seq_along(spec)){
#   
#   sf <-  subset(df_habitat_velocity, species == spec[i]) 
#   ### Prepare DF for each species with loss = 1, gain = 2, stable = 3
#   sf_pes <- sf[,c(2,3,5,6,7,9,10)]
#   sf_pes$species <- factor(sf_pes$species)
#   cl <- makeCluster(4)
#   system.time(b3 <- bam(mf1, family=multinom(K=3), data= sf_pes, chunk.size=5000,cluster=cl ))
#   
#   saveRDS(b3, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_multi_models_pes.rds"))
#   model_resid <- residuals.gam(b3)
#   saveRDS(model_resid, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_multi_residual_pes.rds"))
#   stopCluster(cl)
#   ## correlogram among residuals, x and y
#   df_res <- as.data.frame(cbind(model_resid, sf_pes$x, sf_pes$y))
#   names(df_res) <- c("residuals", "x", "y")
#   # cg <- corrgram(df_res[1:1000,] , order=TRUE, lower.panel=panel.ellipse,
#   #          upper.panel=panel.pts, text.panel=panel.txt,
#   #          diag.panel=panel.minmax,
#   #          main="Residual, x and y correlogram")
#   cg <- cor(df_res)
#   write.table(cg, file = paste0("habitat_topohetero_models/pes/",spec[i],"_cor_multi_resid_pes.txt"), sep ="\t", row.names = T)
#   
# }
# 
# 
