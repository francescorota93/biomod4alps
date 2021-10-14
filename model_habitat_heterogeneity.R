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
library(mgcv)
library(parallel)
#library(corrgram)
# ## with differenza percentuale
# df_habitat_velocity <- read.csv("/data/models/results_climate_change_velocity/df_habitat_velocity_100cons.csv")
# 
#with only difference
df_habitat_velocity <- read.csv("/data/models/results_climate_change_velocity/df_habitat_velocity_100cons_int.csv")

# # ## remove NA from habitats
# df_habitat_velocity <- df_habitat_velocity[!(df_habitat_velocity$habitat_opt == 0 | df_habitat_velocity$habitat_pes == 0),]
df_habitat_velocity <- na.omit(df_habitat_velocity)
str(df_habitat_velocity)
# df_habitat_velocity$habitat_opt <- factor(df_habitat_velocity$habitat_opt)
# df_habitat_velocity$habitat_pes <- factor(df_habitat_velocity$habitat_pes)
spec <- c("Campanula.morettiana", "Festuca.austrodolomitica", "Gentiana.brentae", "Nigritella.buschmanniae",
          "Primula.tyrolensis", "Rhizobotrya.alpina", "Saxifraga.facchinii", "Sempervivum.dolomiticum")

# #### continuous hs ---------------- 
# ### OPTIMISTIC with autocorrelation s(x,y) all

mf1 <- formula(habitat_opt ~ TRI + TCI + elev + logSpeed_45 + TRI*elev + TRI*logSpeed_45 + TCI*elev + TCI*logSpeed_45 + elev*logSpeed_45 + s(x, y))

for(i in seq_along(spec)){

  sf <-  subset(df_habitat_velocity, species == spec[i])

  sf_loss_stable_opt <- sf[,c(2,3,4,6,7,8,9,11)]
  sf_loss_stable_opt$species <- factor(sf_loss_stable_opt$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=gaussian(), data= sf_loss_stable_opt, chunk.size=5000,cluster=cl ))

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

# ### divide by negative (== loss) and positive (== stable + gain)
# ## LOSS
# mf1 <- formula(habitat_opt ~ TRI + TCI + elev + logSpeed_45 + TRI*elev + TRI*logSpeed_45 + TCI*elev + TCI*logSpeed_45 + elev*logSpeed_45 + s(x, y))
# 
# for(i in seq_along(spec)){
#   
#   sf <-  subset(df_habitat_velocity, species == spec[i] & habitat_opt < 0) 
#   sf_loss_stable_opt <- sf[,c(2,3,4,6,7,8,9,11)]
#   sf_loss_stable_opt$species <- factor(sf_loss_stable_opt$species)
#   cl <- makeCluster(4)
#   system.time(b3 <- bam(mf1, family=gaussian(), data= sf_loss_stable_opt, chunk.size=5000,cluster=cl ))
#   
#   saveRDS(b3, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_models_opt_100_LOSS.rds"))
#   model_resid <- residuals.gam(b3)
#   saveRDS(model_resid, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_residual_opt_100_LOSS.rds"))
#   stopCluster(cl)
#   ## correlogram among residuals, x and y
#   df_res <- as.data.frame(cbind(model_resid, sf_loss_stable_opt$x, sf_loss_stable_opt$y))
#   names(df_res) <- c("residuals", "x", "y")
#   # cg <- corrgram(df_res[1:1000,] , order=TRUE, lower.panel=panel.ellipse,
#   #          upper.panel=panel.pts, text.panel=panel.txt,
#   #          diag.panel=panel.minmax,
#   #          main="Residual, x and y correlogram")
#   cg <- cor(df_res)
#   write.table(cg, file = paste0("habitat_topohetero_models/opt/",spec[i],"_cor_resid_opt_100_LOSS.txt"), sep ="\t", row.names = T)
#   
# }


##with only difference
df_habitat_velocity <- read.csv("/data/models/results_climate_change_velocity/df_habitat_velocity_100cons_int.csv")

# ## remove NA from habitats
# df_habitat_velocity <- df_habitat_velocity[!(df_habitat_velocity$habitat_opt == 0 | df_habitat_velocity$habitat_pes == 0),]
df_habitat_velocity <- na.omit(df_habitat_velocity)
# df_habitat_velocity$habitat_opt <- factor(df_habitat_velocity$habitat_opt)
# df_habitat_velocity$habitat_pes <- factor(df_habitat_velocity$habitat_pes)
spec <- c("Campanula.morettiana", "Festuca.austrodolomitica", "Gentiana.brentae", "Nigritella.buschmanniae",
          "Primula.tyrolensis", "Rhizobotrya.alpina", "Saxifraga.facchinii", "Sempervivum.dolomiticum")


#### continuous hs ---------------- 
### OPTIMISTIC with autocorrelation s(x,y) all

### divide by negative (== loss) and positive (== stable + gain)
## LOSS
mf1 <- formula(habitat_opt ~ TRI + TCI + elev + logSpeed_45 + TRI*elev + TRI*logSpeed_45 + TCI*elev + TCI*logSpeed_45 + elev*logSpeed_45 + s(x, y))

for(i in seq_along(spec)){

  sf <-  subset(df_habitat_velocity, species == spec[i] & habitat_opt < 0)
  sf_loss_stable_opt <- sf[,c(2,3,4,6,7,8,9,11)]
  sf_loss_stable_opt$species <- factor(sf_loss_stable_opt$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=gaussian(), data= sf_loss_stable_opt, chunk.size=5000,cluster=cl ))

  saveRDS(b3, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_models_opt_100_LOSS.rds"))
  model_resid <- residuals.gam(b3)
  saveRDS(model_resid, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_residual_opt_100_LOSS.rds"))
  stopCluster(cl)
  ## correlogram among residuals, x and y
  df_res <- as.data.frame(cbind(model_resid, sf_loss_stable_opt$x, sf_loss_stable_opt$y))
  names(df_res) <- c("residuals", "x", "y")
  # cg <- corrgram(df_res[1:1000,] , order=TRUE, lower.panel=panel.ellipse,
  #          upper.panel=panel.pts, text.panel=panel.txt,
  #          diag.panel=panel.minmax,
  #          main="Residual, x and y correlogram")
  cg <- cor(df_res)
  write.table(cg, file = paste0("habitat_topohetero_models/opt/",spec[i],"_cor_resid_opt_100_LOSS.txt"), sep ="\t", row.names = T)

}

## STABLE + GAIN
mf1 <- formula(habitat_opt ~ TRI + TCI + elev + logSpeed_45 + TRI*elev + TRI*logSpeed_45 + TCI*elev + TCI*logSpeed_45 + elev*logSpeed_45 + s(x, y))

for(i in seq_along(spec)){

  sf <-  subset(df_habitat_velocity, species == spec[i] & habitat_opt >= 0)
  sf_loss_stable_opt <- sf[,c(2,3,4,6,7,8,9,11)]
  sf_loss_stable_opt$species <- factor(sf_loss_stable_opt$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=gaussian(), data= sf_loss_stable_opt, chunk.size=5000,cluster=cl ))

  saveRDS(b3, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_models_opt_100_STABLE.rds"))
  model_resid <- residuals.gam(b3)
  saveRDS(model_resid, file = paste0("habitat_topohetero_models/opt/",spec[i],"_BAM_residual_opt_100_STABLE.rds"))
  stopCluster(cl)
  ## correlogram among residuals, x and y
  df_res <- as.data.frame(cbind(model_resid, sf_loss_stable_opt$x, sf_loss_stable_opt$y))
  names(df_res) <- c("residuals", "x", "y")
  # cg <- corrgram(df_res[1:1000,] , order=TRUE, lower.panel=panel.ellipse,
  #          upper.panel=panel.pts, text.panel=panel.txt,
  #          diag.panel=panel.minmax,
  #          main="Residual, x and y correlogram")
  cg <- cor(df_res)
  write.table(cg, file = paste0("habitat_topohetero_models/opt/",spec[i],"_cor_resid_opt_100_STABLE.txt"), sep ="\t", row.names = T)

}



### PESSIMISTIC with autocorrelation s(x,y) ----

mf1 <- formula(habitat_pes ~ TRI + TCI + elev + logSpeed_85 + TRI*elev + TRI*logSpeed_85 + TCI*elev + TCI*logSpeed_85 + elev*logSpeed_85 + s(x, y))
library(parallel)


for(i in seq_along(spec)){

  sf <-  subset(df_habitat_velocity, species == spec[i])

  sf_loss_stable_pes <- sf[,c(2,3,5,6,7,8,10,11)]
  sf_loss_stable_pes$species <- factor(sf_loss_stable_pes$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=gaussian(), data= sf_loss_stable_pes, chunk.size=5000,cluster=cl ))
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


# ### divide by negative (== loss) and positive (== stable + gain)
## LOSS
mf1 <- formula(habitat_pes ~ TRI + TCI + elev + logSpeed_85 + TRI*elev + TRI*logSpeed_85 + TCI*elev + TCI*logSpeed_85 + elev*logSpeed_85 + s(x, y))

for(i in seq_along(spec)){

  sf <-  subset(df_habitat_velocity, species == spec[i] & habitat_pes < 0)

  sf_loss_stable_pes <- sf[,c(2,3,5,6,7,8,10,11)]
  sf_loss_stable_pes$species <- factor(sf_loss_stable_pes$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=gaussian(), data= sf_loss_stable_pes, chunk.size=5000,cluster=cl ))
  saveRDS(b3, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_models_pes_LOSS.rds"))
  model_resid <- residuals.gam(b3)
  saveRDS(model_resid, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_residuals_pes_LOSS.rds"))
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
  write.table(cg, file = paste0("habitat_topohetero_models/pes/",spec[i],"_cor_resid_pes_LOSS.txt"), sep ="\t", row.names = T)

}


## STABLE + GAIN
mf1 <- formula(habitat_pes ~ TRI + TCI + elev + logSpeed_85 + TRI*elev + TRI*logSpeed_85 + TCI*elev + TCI*logSpeed_85 + elev*logSpeed_85 + s(x, y))

for(i in seq_along(spec)){

  sf <-  subset(df_habitat_velocity, species == spec[i] & habitat_pes >= 0)

  sf_loss_stable_pes <- sf[,c(2,3,5,6,7,8,10,11)]
  sf_loss_stable_pes$species <- factor(sf_loss_stable_pes$species)
  cl <- makeCluster(4)
  system.time(b3 <- bam(mf1, family=gaussian(), data= sf_loss_stable_pes, chunk.size=5000,cluster=cl ))
  saveRDS(b3, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_models_pes_STABLE.rds"))
  model_resid <- residuals.gam(b3)
  saveRDS(model_resid, file = paste0("habitat_topohetero_models/pes/",spec[i],"_BAM_residuals_pes_STABLE.rds"))
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
  write.table(cg, file = paste0("habitat_topohetero_models/pes/",spec[i],"_cor_resid_pes_STABLE.txt"), sep ="\t", row.names = T)

}



###### plot models -----
library(mgcViz)

b <- getViz(Rhizobotrya.alpina_BAM_models_opt_100_LOSS)

check(b,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))
library(visreg)
visreg(Saxifraga.facchinii_BAM_models_opt_100, 
       xlab="elevation", ylab="Habitat suitability")

vis.gam(Saxifraga.facchinii_BAM_models_opt_100, view=c( "elev", "logSpeed_45"),plot.type="persp") # theta=-30,phi=30
vis.gam(Saxifraga.facchinii_BAM_models_opt_100, view=c( "elev", "TCI"),plot.type="persp") # theta=-30,phi=30
vis.gam(Saxifraga.facchinii_BAM_models_opt_100, view=c( "elev", "TRI"),plot.type="persp") # theta=-30,phi=30
vis.gam(Saxifraga.facchinii_BAM_models_opt_100, view=c( "TCI", "TRI"),plot.type="persp", theta=-30,phi=30) # 

visreg(Saxifraga.facchinii_BAM_models_opt_100, 
       xvar="elev", 
       points.par=list(cex=1), 
       band=F, 
       #ylim=c(250, 600),
       main="elevation")

## PLOT
library(visreg)
m1 <- readRDS("/data/models/habitat_topohetero_models/opt/Campanula.morettiana_BAM_models_opt_100.rds")
visreg(m1, "elev", gg= TRUE)

b <- getViz(m1)

# Default smooth effect plotting
print(plot(b), ask = FALSE)
print(plot(b, select = 2:5), pages = 1)

library(tidymv)
plot_smooths(
  model = Rhizobotrya.alpina_BAM_models_opt_100_LOSS
)

plot(Rhizobotrya.alpina_BAM_models_opt_100_LOSS, residuals=F, se=TRUE, select= 1, scheme = 1, pch=19, cex=0.75)

plot_smooths(Rhizobotrya.alpina_BAM_models_opt_100_STABLE, comparison = "habitat_opt", facet_terms = c("elev", "logSpeed45", "TRI", "TCI"))

vis.gam(m1, view=c("logSpeed_45", "elev"),type="link", theta=-60, phi=30) # theta=-30,phi=30


### PLOTTARE ---
library(ggplot2)
library(cowplot)
library("ggsci")
library(ggpubr)
######### !!!!!! linee specie con risposta significativa metti in nero altrimenti metti in grigio
myTheme <- theme(
  panel.background = element_rect(fill = "white", colour = "black"), 
  panel.grid.major =  element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_text(color = "black", size=14),
  axis.text.x = element_text(color = "black", size=14)
)
update_geom_defaults("smooth", list(size = .5))
## rcp 4.5 OPTIMISTIC
### LOSS 
df_loss <- subset(df_habitat_velocity, habitat_opt < 0)

elev_opt_loss <- ggplot(df_loss, aes(elev, habitat_opt, color = species, linetype = species))+ #, group = species))+ #, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(x = "Elevation", y = "Habitat Suitability Difference") + 
  #scale_color_manual(values=rep(c("black"), 8)) +
  myTheme +
  theme(legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA)) +
  ylim(c(NA,0))+
  #theme(legend.position = "none") +
  scale_color_npg()


ggsave("elev_opt_loss.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  

ccv_opt_loss <- ggplot(df_loss, aes( logSpeed_45, habitat_opt, color = species, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(x = "Climate Change Velocity" , y = "Habitat Suitability Difference") +
  #scale_color_manual(values=rep(c("black"), 8)) +
  ylim(c(NA,0))+
  scale_color_npg()+
  myTheme #+
  #theme(legend.position = "none")
ggsave("ccv_opt_loss.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  


tri_opt_loss <- ggplot(df_loss, aes( TRI, habitat_opt, color = species, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(x = "TRI", y = "Habitat Suitability Difference")+
  #scale_color_manual(values=rep(c("black"), 8)) +
  ylim(c(NA,0))+
  scale_color_npg()+
  myTheme #+
  #theme(legend.position = "none")
ggsave("tri_opt_loss.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  

tci_opt_loss <- ggplot(df_loss, aes( TCI, habitat_opt, color = species, linetype = species)) + 
  stat_smooth(method = "lm", se = TRUE, level = 0.95) + 
  labs(x = "TCI", y = "Habitat Suitability Difference") +
  #scale_color_manual(values=rep(c("black"), 8)) +
  ylim(c(NA,0))+
  scale_color_npg()+
  myTheme #+
  #theme(legend.position = "none")
  ggsave( "tci_opt_loss.png", width = 18.169466667, height = 11.218333333, unit = "cm",  dpi = 300)  

## STABLE + GAIN
df_stable <- subset(df_habitat_velocity, habitat_opt >= 0)

elev_opt_stable <- ggplot(df_stable, aes(elev, habitat_opt, color = species, linetype = species))+ #, group = species))+ #, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(x = "Elevation", y = "Habitat Suitability Difference") + 
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  ylim(c(0,NA))+
  myTheme


ggsave( "elev_opt_stable.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  

ccv_opt_stable <- ggplot(df_stable, aes(logSpeed_45,habitat_opt, color = species, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(x = "Climate Change Velocity", y = "Habitat Suitability Difference") +
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  ylim(c(0,NA))+
  myTheme
ggsave("ccv_opt_stable.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  

tri_opt_stable <- ggplot(df_stable, aes(TRI, habitat_opt,  color = species, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(x = "TRI", y = "Habitat Suitability Difference")+
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  ylim(c(0,NA))+
  myTheme
ggsave( "tri_opt_stable.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  


tci_opt_stable <- ggplot(df_stable, aes(TCI, habitat_opt, color = species, linetype = species)) + 
  stat_smooth(method = "lm", se = TRUE, level = 0.95) + 
  labs(x = "TCI", y = "Habitat Suitability Difference") +
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  ylim(c(0,NA))+
  myTheme
ggsave( "tci_opt_stable.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  

## rcp 8.5 PESSIMISTIC
### LOSS 
df_loss <- subset(df_habitat_velocity, habitat_pes < 0)

elev_pes_loss <- ggplot(df_loss, aes( elev, habitat_pes, color = species, linetype = species))+ #, group = species))+ #, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(y = "Habitat Suitability Difference", x = "Elevation") + 
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  myTheme
ggsave("elev_pes_loss.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  

ccv_pes_loss <- ggplot(df_loss, aes( logSpeed_85, habitat_pes, color = species, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(y = "Habitat Suitability Difference", x = "Climate Change Velocity") +
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  myTheme
ggsave( "ccv_pes_loss.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)


tri_pes_loss <- ggplot(df_loss, aes( TRI, habitat_pes, color = species, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(y = "Habitat Suitability Difference", x = "TRI")+
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  myTheme
ggsave("tri_pes_loss.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  


tci_pes_loss <- ggplot(df_loss, aes( TCI, habitat_pes, color = species, linetype = species)) + 
  stat_smooth(method = "lm", se = TRUE, level = 0.95) + 
  labs(y = "Habitat Suitability Difference", x = "TCI") +
  scale_color_npg()+
  #scale_color_manual(values=rep(c("black"), 8)) +
  myTheme
ggsave("tci_pes_loss.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  

## STABLE + GAIN
df_stable <- subset(df_habitat_velocity, habitat_pes >= 0)

elev_pes_stable <- ggplot(df_stable, aes( elev, habitat_pes, color = species, linetype = species))+ #, group = species))+ #, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(y = "Habitat Suitability Difference", x = "Elevation") + 
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  myTheme
ggsave("elev_pes_stable.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  

ccv_pes_stable <- ggplot(df_stable, aes( logSpeed_85, habitat_pes, color = species, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(y = "Habitat Suitability Difference", x = "Climate Change Velocity") +
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  myTheme
ggsave("ccv_pes_stable.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  


tri_pes_stable <- ggplot(df_stable, aes( TRI, habitat_pes, color = species, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(y = "Habitat Suitability Difference", x = "TRI")+
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  myTheme
ggsave("tri_pes_stable.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  

tci_pes_stable <- ggplot(df_stable, aes( TCI, habitat_pes, color = species, linetype = species)) + 
  stat_smooth(method = "lm", se = TRUE, level = 0.95) + 
  labs(y = "Habitat Suitability Difference", x = "TCI") +
  scale_color_npg()+
  #scale_color_manual(values=rep(c("black"), 8)) +
  myTheme
ggsave("tci_pes_stable.png",  width = 18.169466667, height = 11.218333333, unit = "cm", dpi = 300)  

# ggplot(df_stable, aes(habitat_pes,  elev, color = species, linetype = species)) + 
#   stat_smooth(method = "lm", se = TRUE, level = 0.95) + 
#   labs(x = "elevation", y = "Habitat Suitability Difference") +
#   #scale_color_manual(values=rep(c("black"), 8)) +
#   myTheme

df_stable <- subset(df_habitat_velocity, habitat_pes >0)
elev_pes_stable_all <- ggplot(df_habitat_velocity, aes( TCI, habitat_pes, color = species, linetype = species))+ #, group = species))+ #, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "lm", se = TRUE, level = 0.95) + 
  labs(y = "Habitat Suitability Difference", x = "Elevation") + 
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  myTheme

### ggarrange
elev_plot_opt <- ggarrange(elev_opt_stable, elev_opt_loss, ncol = 1, align = "v", common.legend=TRUE)

df_habitat_velocity$gain_loss_opt <- ifelse(df_habitat_velocity$habitat_opt >= 10, "gain", ifelse(df_habitat_velocity$habitat_opt <= -10, "loss", "stable"))
df_habitat_velocity$gain_loss_opt <- factor(df_habitat_velocity$gain_loss_opt, levels = c("gain", "stable", "loss"))                   
df_habitat_velocity$gain_loss_opt <- ifelse(df_habitat_velocity$habitat_pes >= 10, "gain", ifelse(df_habitat_velocity$habitat_pes <= -10, "loss", "stable"))
df_habitat_velocity$gain_loss_pes <- factor(df_habitat_velocity$gain_loss_pes, levels = c("gain","stable", "loss"))                   

elev_opt <- ggplot(df_habitat_velocity, aes(elev, habitat_opt, color = species, linetype = species))+ #, group = species))+ #, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "gam", se = TRUE, level = 0.95) + 
  labs(x = "Elevation", y = "Habitat Suitability Difference") +
  #facet_wrap(~gain_loss_opt, ncol = 1, scales = "free_y") +
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  #ylim(c(0,NA))+
  myTheme

tci_opt <- ggplot(df_habitat_velocity, aes(TCI, habitat_opt, color = species, linetype = species))+ #, group = species))+ #, linetype = species)) + 
  stat_smooth(aes(linetype = species), method = "gam", se = TRUE, level = 0.95) + 
  labs(x = "TCI", y = "Habitat Suitability Difference") +
  #facet_wrap(~gain_loss_opt, ncol = 1, scales = "free_y") +
  #scale_color_manual(values=rep(c("black"), 8)) +
  scale_color_npg()+
  #ylim(c(0,NA))+
  myTheme


########## plot BAM
m1 <- readRDS("/data/models/habitat_topohetero_models/opt/Campanula_morettiana_BAM_models_opt_LOSS.rds")
library(mgcv)
summary(m1)
plot(m1, select=2)
