library(rgeos)
library(raster)

 ### CAMPANULA MORETTIANA coefficiente = 26.25 ------  
 ## load raster occurrence
 spec_occ_cm <- raster("models/species/occurrence_50/Occurrence50_Campanula morettiana.tif")
 spec_df <- as.data.frame(spec_occ_cm, xy =T,na.rm =T) #spec_occ <- stack()
 
 ## load maps sintesi for each species
 
 ## optimistic -----
 map <- raster("models/maps/Campanula_morettiana_optimistic_sintesi.tif")
 cm <- crop(spec_occ_cm, map)
 
#### create spatial points df 
 b <- rasterToPoints(cm, spatial=TRUE)
 
## relative buffer (75 y * relative annual dispersal m VITTOZ = 26.25)
 rel_buf <- extract(map, b, buffer = 26.25)
 col_rel_buf <- data.frame(stepping_stone_rel_opt = NA)
 for (i in 1:length(rel_buf)){
   z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
   col_rel_buf1 <- data.frame(stepping_stone_rel_opt = z)
   col_rel_buf <- rbind(col_rel_buf, col_rel_buf1)
 }
 
 ## local refugia optimistic
 loc_refugia_opt <- extract(map, b)
 
 ## pessimistic -----
 map <- raster("models/maps/Campanula_morettiana_pessimistic_sintesi.tif")
 cm <- crop(spec_occ_cm, map)
 #### create spatial points df 
 b <- rasterToPoints(cm, spatial=TRUE)
 
 ## relative buffer (74 y * relative annual dispersal m VITTOZ = 100)
 rel_buf <- extract(map, b, buffer = 26.25)
 col_rel_buf_pes <- data.frame(stepping_stone_rel_pes = NA)
 for (i in 1:length(rel_buf)){
   z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
   col_rel_buf1_pes <- data.frame(stepping_stone_rel_pes = z)
   col_rel_buf_pes <- rbind(col_rel_buf_pes, col_rel_buf1_pes)
 }
 
 ## local refugia pessimistic
 loc_refugia_pes <- extract(map, b)
 
 refugia_df_points = data.frame(x = spec_df[,1], y = spec_df[,2], stepping_stone_rel_opt = col_rel_buf[-1,], stepping_stone_rel_pes = col_rel_buf_pes[-1,], local_refugia_opt = loc_refugia_opt, local_refugia_pes =  loc_refugia_pes)
 
campanula <- rep("Campanula_morettiana", 145)

campanula_refugia <- cbind(campanula, refugia_df_points) 

### FESTUCA AUSTRODOLOMITICA coefficiente = 1125------ 

## load raster occurrence
spec_occ_cm <- raster("models/species/occurrence_50/Occurrence50_Festuca austrodolomitica.tif")
spec_df <- as.data.frame(spec_occ_cm, xy =T,na.rm =T) #spec_occ <- stack()

## load maps sintesi for each species

## optimistic -----
map <- raster("models/maps/Festuca_austrodolomitica_optimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)

#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (75 y * relative annual dispersal m VITTOZ = 1125)
rel_buf <- extract(map, b, buffer = 1125)
col_rel_buf <- data.frame(stepping_stone_rel_opt = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1 <- data.frame(stepping_stone_rel_opt = z)
  col_rel_buf <- rbind(col_rel_buf, col_rel_buf1)
}

## local refugia optimistic
loc_refugia_opt <- extract(map, b)

## pessimistic -----
map <- raster("models/maps/Festuca_austrodolomitica_pessimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)
#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (74 y * relative annual dispersal m VITTOZ = 100)
rel_buf <- extract(map, b, buffer = 1125)
col_rel_buf_pes <- data.frame(stepping_stone_rel_pes = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1_pes <- data.frame(stepping_stone_rel_pes = z)
  col_rel_buf_pes <- rbind(col_rel_buf_pes, col_rel_buf1_pes)
}

## local refugia pessimistic
loc_refugia_pes <- extract(map, b)

refugia_df_points = data.frame(x = spec_df[,1], y = spec_df[,2], stepping_stone_rel_opt = col_rel_buf[-1,], stepping_stone_rel_pes = col_rel_buf_pes[-1,], local_refugia_opt = loc_refugia_opt, local_refugia_pes =  loc_refugia_pes)

festuca <- rep("Festuca_austrodolomitica", 161)

festuca_refugia <- cbind(festuca, refugia_df_points) 

### GENTIANA BRENTAE coefficiente = 90 ------ 
## load raster occurrence
spec_occ_cm <- raster("models/species/occurrence_50/Occurrence50_Gentiana brentae.tif")
spec_df <- as.data.frame(spec_occ_cm, xy =T,na.rm =T) #spec_occ <- stack()

## load maps sintesi for each species

## optimistic -----
map <- raster("models/maps/Gentiana_brentae_optimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)

#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (74 y * relative annual dispersal m VITTOZ = 90)
rel_buf <- extract(map, b, buffer = 90)
col_rel_buf <- data.frame(stepping_stone_rel_opt = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1 <- data.frame(stepping_stone_rel_opt = z)
  col_rel_buf <- rbind(col_rel_buf, col_rel_buf1)
}

## local refugia optimistic
loc_refugia_opt <- extract(map, b)

## pessimistic -----
map <- raster("models/maps/Gentiana_brentae_pessimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)
#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (74 y * relative annual dispersal m VITTOZ = 100)
rel_buf <- extract(map, b, buffer = 90)
col_rel_buf_pes <- data.frame(stepping_stone_rel_pes = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1_pes <- data.frame(stepping_stone_rel_pes = z)
  col_rel_buf_pes <- rbind(col_rel_buf_pes, col_rel_buf1_pes)
}

## local refugia pessimistic
loc_refugia_pes <- extract(map, b)

refugia_df_points = data.frame(x = spec_df[,1], y = spec_df[,2], stepping_stone_rel_opt = col_rel_buf[-1,], stepping_stone_rel_pes = col_rel_buf_pes[-1,], local_refugia_opt = loc_refugia_opt, local_refugia_pes =  loc_refugia_pes)

gentiana <- rep("Gentiana_brentae", 72)

gentiana_refugia <- cbind(gentiana, refugia_df_points) 

### NIGRITELLA BUSHMANNIAE coefficiente = 22.5 ------ 
## load raster occurrence
spec_occ_cm <- raster("models/species/occurrence_50/Occurrence50_Nigritella buschmanniae.tif")
spec_df <- as.data.frame(spec_occ_cm, xy =T,na.rm =T) #spec_occ <- stack()

## load maps sintesi for each species

## optimistic -----
map <- raster("models/maps/Nigritella_buschmanniae_optimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)

#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (75 y * relative annual dispersal m VITTOZ = 22.5)
rel_buf <- extract(map, b, buffer = 22.5)
col_rel_buf <- data.frame(stepping_stone_rel_opt = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1 <- data.frame(stepping_stone_rel_opt = z)
  col_rel_buf <- rbind(col_rel_buf, col_rel_buf1)
}

## local refugia optimistic
loc_refugia_opt <- extract(map, b)

## pessimistic -----
map <- raster("models/maps/Nigritella_buschmanniae_pessimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)
#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (74 y * relative annual dispersal m VITTOZ = 22.5)
rel_buf <- extract(map, b, buffer = 22.5)
col_rel_buf_pes <- data.frame(stepping_stone_rel_pes = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1_pes <- data.frame(stepping_stone_rel_pes = z)
  col_rel_buf_pes <- rbind(col_rel_buf_pes, col_rel_buf1_pes)
}

## local refugia pessimistic
loc_refugia_pes <- extract(map, b)

refugia_df_points = data.frame(x = spec_df[,1], y = spec_df[,2], stepping_stone_rel_opt = col_rel_buf[-1,], stepping_stone_rel_pes = col_rel_buf_pes[-1,], local_refugia_opt = loc_refugia_opt, local_refugia_pes =  loc_refugia_pes)

nigritella <- rep("Nigritella_buschmanniae", 124)

nigritella_refugia <- cbind(nigritella, refugia_df_points) 

### PRIMULA TYROLENSIS coefficiente = 75------ 
## load raster occurrence
spec_occ_cm <- raster("models/species/occurrence_50/Occurrence50_Primula tyrolensis.tif")
spec_df <- as.data.frame(spec_occ_cm, xy =T,na.rm =T) #spec_occ <- stack()

## load maps sintesi for each species

## optimistic -----
map <- raster("models/maps/Primula_tyrolensis_optimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)

#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (75 y * relative annual dispersal m VITTOZ = 75)
rel_buf <- extract(map, b, buffer = 75)
col_rel_buf <- data.frame(stepping_stone_rel_opt = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1 <- data.frame(stepping_stone_rel_opt = z)
  col_rel_buf <- rbind(col_rel_buf, col_rel_buf1)
}

## local refugia optimistic
loc_refugia_opt <- extract(map, b)

## pessimistic -----
map <- raster("models/maps/Primula_tyrolensis_pessimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)
#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (75 y * relative annual dispersal m VITTOZ = 75)
rel_buf <- extract(map, b, buffer = 75)
col_rel_buf_pes <- data.frame(stepping_stone_rel_pes = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1_pes <- data.frame(stepping_stone_rel_pes = z)
  col_rel_buf_pes <- rbind(col_rel_buf_pes, col_rel_buf1_pes)
}

## local refugia pessimistic
loc_refugia_pes <- extract(map, b)

refugia_df_points = data.frame(x = spec_df[,1], y = spec_df[,2], stepping_stone_rel_opt = col_rel_buf[-1,], stepping_stone_rel_pes = col_rel_buf_pes[-1,], local_refugia_opt = loc_refugia_opt, local_refugia_pes =  loc_refugia_pes)

primula <- rep("Primula_tyrolensis", 147)

primula_refugia <- cbind(primula, refugia_df_points) 

### RHIZOBOTRYA ALPINA coefficiente = 75 ------ 
## load raster occurrence
spec_occ_cm <- raster("models/species/occurrence_50/Occurrence50_Rhizobotrya alpina.tif")
spec_df <- as.data.frame(spec_occ_cm, xy =T,na.rm =T) #spec_occ <- stack()

## load maps sintesi for each species

## optimistic -----
map <- raster("models/maps/Rhizobotrya_alpina_optimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)

#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (75 y * relative annual dispersal m VITTOZ = 75)
rel_buf <- extract(map, b, buffer = 75)
col_rel_buf <- data.frame(stepping_stone_rel_opt = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1 <- data.frame(stepping_stone_rel_opt = z)
  col_rel_buf <- rbind(col_rel_buf, col_rel_buf1)
}

## local refugia optimistic
loc_refugia_opt <- extract(map, b)

## pessimistic -----
map <- raster("models/maps/Rhizobotrya_alpina_pessimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)
#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (75 y * relative annual dispersal m VITTOZ = 75)
rel_buf <- extract(map, b, buffer = 75)
col_rel_buf_pes <- data.frame(stepping_stone_rel_pes = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1_pes <- data.frame(stepping_stone_rel_pes = z)
  col_rel_buf_pes <- rbind(col_rel_buf_pes, col_rel_buf1_pes)
}

## local refugia pessimistic
loc_refugia_pes <- extract(map, b)

refugia_df_points = data.frame(x = spec_df[,1], y = spec_df[,2], stepping_stone_rel_opt = col_rel_buf[-1,], stepping_stone_rel_pes = col_rel_buf_pes[-1,], local_refugia_opt = loc_refugia_opt, local_refugia_pes =  loc_refugia_pes)

rhizo <- rep("Rhizobotrya_alpina", 112)

rhizo_refugia <- cbind(rhizo, refugia_df_points) 

### SAXIFRAGA FACCHINII ------ coefficiente = 75
## load raster occurrence
spec_occ_cm <- raster("models/species/occurrence_50/Occurrence50_Saxifraga facchinii.tif")
spec_df <- as.data.frame(spec_occ_cm, xy =T,na.rm =T) #spec_occ <- stack()

## load maps sintesi for each species

## optimistic -----
map <- raster("models/maps/Saxifraga_facchinii_optimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)

#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (75 y * relative annual dispersal m VITTOZ = 75)
rel_buf <- extract(map, b, buffer = 75)
col_rel_buf <- data.frame(stepping_stone_rel_opt = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1 <- data.frame(stepping_stone_rel_opt = z)
  col_rel_buf <- rbind(col_rel_buf, col_rel_buf1)
}

## local refugia optimistic
loc_refugia_opt <- extract(map, b)

## pessimistic -----
map <- raster("models/maps/Saxifraga_facchinii_pessimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)
#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (75 y * relative annual dispersal m VITTOZ = 75)
rel_buf <- extract(map, b, buffer = 75)
col_rel_buf_pes <- data.frame(stepping_stone_rel_pes = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1_pes <- data.frame(stepping_stone_rel_pes = z)
  col_rel_buf_pes <- rbind(col_rel_buf_pes, col_rel_buf1_pes)
}

## local refugia pessimistic
loc_refugia_pes <- extract(map, b)

refugia_df_points = data.frame(x = spec_df[,1], y = spec_df[,2], stepping_stone_rel_opt = col_rel_buf[-1,], stepping_stone_rel_pes = col_rel_buf_pes[-1,], local_refugia_opt = loc_refugia_opt, local_refugia_pes =  loc_refugia_pes)

saxifraga <- rep("Saxifraga_facchinii", 93)

saxifraga_refugia <- cbind(saxifraga, refugia_df_points) 

### SEMPERVIVUM DOLOMITICUM ------ coefficiente = 75
## load raster occurrence
spec_occ_cm <- raster("models/species/occurrence_50/Occurrence50_Sempervivum dolomiticum.tif")
spec_df <- as.data.frame(spec_occ_cm, xy =T,na.rm =T) #spec_occ <- stack()

## load maps sintesi for each species

## optimistic -----
map <- raster("models/maps/Sempervivum_dolomiticum_optimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)

#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)


## relative buffer (75 y * relative annual dispersal m VITTOZ = 75)
rel_buf <- extract(map, b, buffer = 75)
col_rel_buf <- data.frame(stepping_stone_rel_opt = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1 <- data.frame(stepping_stone_rel_opt = z)
  col_rel_buf <- rbind(col_rel_buf, col_rel_buf1)
}

## local refugia optimistic
loc_refugia_opt <- extract(map, b)

## pessimistic -----
map <- raster("models/maps/Sempervivum_dolomiticum_pessimistic_sintesi.tif")
cm <- crop(spec_occ_cm, map)
#### create spatial points df 
b <- rasterToPoints(cm, spatial=TRUE)

## relative buffer (75 y * relative annual dispersal m VITTOZ = 75)
rel_buf <- extract(map, b, buffer = 75)
col_rel_buf_pes <- data.frame(stepping_stone_rel_pes = NA)
for (i in 1:length(rel_buf)){
  z <- sum(rel_buf[[i]] == c(2,3),  na.rm = TRUE)
  col_rel_buf1_pes <- data.frame(stepping_stone_rel_pes = z)
  col_rel_buf_pes <- rbind(col_rel_buf_pes, col_rel_buf1_pes)
}

## local refugia pessimistic
loc_refugia_pes <- extract(map, b)

refugia_df_points = data.frame(x = spec_df[,1], y = spec_df[,2], stepping_stone_rel_opt = col_rel_buf[-1,], stepping_stone_rel_pes = col_rel_buf_pes[-1,], local_refugia_opt = loc_refugia_opt, local_refugia_pes =  loc_refugia_pes)

sempervivum <- rep("Sempervivum_dolomiticum", 49)

sempervivum_refugia <- cbind(sempervivum, refugia_df_points) 

### rbind all species occurrence point
refugia_endemic_dolomites_df <- rbind(campanula_refugia[,-1], festuca_refugia[,-1], gentiana_refugia[,-1], nigritella_refugia[,-1], primula_refugia[,-1], rhizo_refugia[,-1], saxifraga_refugia[,-1], sempervivum_refugia[,-1])
spec_names <- rep(c("Campanula_morettiana","Festuca_austrodolomitica","Gentiana_brentae", "Nigritella_buschmanniae", "Primula_tyrolensis", 
                    "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum"), c(145,161,72,124,147,112,93,49))

refugia_endemic_dolomites_df <- cbind(spec_names, refugia_endemic_dolomites_df)
write.csv(refugia_endemic_dolomites_df, "models/refugia/refugia_endemic_dolomites_df.csv")
refugia_endemic_dolomites_df <- read.csv("refugia/refugia_endemic_dolomites_df.csv")
a <- read.csv("refugia/refugia_endemic_dolomites_df.csv")



### fare resume optimistic relative
library(dbplyr)
library(dplyr)

# relative optimistic
summary_endemic_rel_opt <- refugia_endemic_dolomites_df %>% 
  group_by(species, loc_refugia_opt, step_stone_opt) %>%
  dplyr::summarize(n = n(), elev= mean(elev, na.rm =T))

#refugia_endemic_dolomites_df[] <- lapply(refugia_endemic_dolomites_df, unclass)

write.csv(summary_endemic_rel_opt, "refugia/summary_endemic_rel_opt.csv")

# summary_endemic_max_opt <- refugia_endemic_dolomites_df %>% 
#   group_by(spec_names, local_refugia_opt, stepping_stone_max_opt) %>%
#   summarise(n = n())
# write.csv(summary_endemic_max_opt, "models/refugia/summary_endemic_max_opt.csv")

summary_endemic_rel_pes <- refugia_endemic_dolomites_df %>% 
  group_by(species, loc_refugia_pes, step_stone_pes) %>%
  dplyr::summarize(n = n(), elev= mean(elev, na.rm =T))
write.csv(summary_endemic_rel_pes, "refugia/summary_endemic_rel_pes.csv")

# summary_endemic_max_pes <- refugia_endemic_dolomites_df %>% 
#   group_by(spec_names, local_refugia_pes, stepping_stone_max_pes) %>%
#   summarise(n = n())
# write.csv(summary_endemic_max_pes, "models/refugia/summary_endemic_max_pes.csv")


# if 3 in local refugia ("in situ survival")
# if 1 in local refugia puo essere stepping-stone se valore stepping stone é > 0, se no è loss

## add elevation for each point
elev <- raster("env_predictors/DTM_50.tif")
plot(elev)

pointCoordinates=read.csv("refugia/refugia_endemic_dolomites_df.csv")
coordinates(pointCoordinates)= ~ x + y
rasValue=extract(elev, pointCoordinates)
combinePointValue=cbind(pointCoordinates,rasValue)
names(combinePointValue) <- c("X", "species", "step_stone_opt", "step_stone_pes", "loc_refugia_opt", "loc_refugia_pes", "elev" )
write.table(combinePointValue,file= "refugia/refugia_endemic_dolomites_df.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)
