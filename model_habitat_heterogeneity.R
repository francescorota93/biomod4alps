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

df_habitat_velocity <- read.csv("/data/models/results_climate_change_velocity/df_habitat_velocity_100cons.csv")

# ## remove NA from habitats
# df_habitat_velocity <- df_habitat_velocity[!(df_habitat_velocity$habitat_opt == 0 | df_habitat_velocity$habitat_pes == 0),]
df_habitat_velocity <- na.omit(df_habitat_velocity)
# df_habitat_velocity$habitat_opt <- factor(df_habitat_velocity$habitat_opt)
# df_habitat_velocity$habitat_pes <- factor(df_habitat_velocity$habitat_pes)
spec <- c("Campanula.morettiana", "Festuca.austrodolomitica", "Gentiana.brentae", "Nigritella.buschmanniae",
          "Primula.tyrolensis", "Rhizobotrya.alpina", "Saxifraga.facchinii", "Sempervivum.dolomiticum")

#### continuous hs ---------------- 
### OPTIMISTIC with autocorrelation s(x,y) all

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

### divide by negative (== loss) and positive (== stable + gain)
## LOSS
mf1 <- formula(habitat_opt ~ TRI + TCI + elev + logSpeed_45 + TRI*elev + TRI*logSpeed_45 + TCI*elev + TCI*logSpeed_45 + elev*logSpeed_45 + s(x, y))

for(i in seq_along(spec)){
  
  sf <-  subset(df_habitat_velocity, species == spec[i] & habitat_opt < -0.1) 
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





library(mgcViz)
b <- getViz(Campanula.morettiana_BAM_models_opt_100)

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
b <- getViz(Saxifraga.facchinii_BAM_models_opt_100)

# Default smooth effect plotting
print(plot(b), ask = FALSE)
print(plot(b, select = 2:5), pages = 1)

### PESSIMISTIC with autocorrelation s(x,y)

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


### OLS 



