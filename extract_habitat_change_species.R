setwd("/data/models/maps/") 
library(raster)
library(dplyr)
library(tidyr)

#name.list <- unlist(strsplit(dir(".", pattern = "*reclass.tif" ) ,split = "_"))
#sp_names <- paste0(name.list[1], "_", name.list[2])
sp1<-read.table("../endemic_dolo50.txt", sep="\t", h=T) ### load spec occurrence file
spek <- sub(pattern = "_", replacement = "\\_", x = as.character(unique(sp1$species)))
sp <- list.files(".", pattern = "*sintesi.tif" )
df<-data.frame(species=NA, x=NA, y=NA,opt=NA, pes =NA)

for(i in 1:length(spek)){
  #sp <- name.list[grep(spek[i], name.list)]
  point_coord <- subset(sp1, grepl(spek[i], sp1$species))
  point_sp <- subset(sp1, grepl(spek[i], sp1$species))
  coordinates(point_sp)= ~ x + y
  crs(point_sp) = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  sp_opt <- raster(sp[grep(paste0(spek[i],"_optimistic"), sp)])
  sp_pes <- raster(sp[grep(paste0(spek[i],"_pessimistic"), sp)])
  opt_pt <- raster::extract(sp_opt, point_sp)
  pes_pt <- raster::extract(sp_pes, point_sp)
  comb =cbind(point_sp,opt_pt, pes_pt)
  names(comb@data) <- c("species","opt", "pes")
  df2<-data.frame(species=spek[i], x = point_coord$x , y = point_coord$y , opt = opt_pt, pes = pes_pt)
  df<-rbind(df,df2)
  }
df[is.na(df)]
df <- na.omit(df)
###
write.csv(df, file = "../habitat_change_populations_dolomites.csv")

df$species <- factor(df$species)
df$opt <- factor(df$opt)
df$pes <- factor(df$pes)

## OPTIMISTIC
library(dplyr)
library(ggplot2)
library(stringr)

plot_data <- df %>% 
  group_by(species, opt) %>% 
  tally %>% 
  mutate(percent = n/sum(n))

plot_data$percent <- lapply(plot_data$percent, round, 3)

p <- ggplot(plot_data, aes(x = opt, y = percent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(plot_data$percent, digits = 3), vjust = 0)) +
  labs(title = "Habitat Suitability", y = "Percent", x = "2080 rcp 4.5 - optimistic") +
  #scale_y_continuous(labels = percent, limits = c(0,1)) +
  facet_wrap(plot_data$species, ncol = 4) +scale_x_discrete(labels=c("Not Suitable", "Loss", "Stable"))+
  theme_bw()

## PESSIMISTIC

library(dplyr)
library(ggplot2)
library(stringr)

plot_data1 <- df %>% 
  group_by(species, pes) %>% 
  tally %>% 
  mutate(percent = n/sum(n))

p1 <- ggplot(plot_data1, aes(x = pes, y = percent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(plot_data1$percent, digits = 3), vjust = 0.25)) +
  labs(title = "Habitat Suitability", y = "Percent", x = "2080 rcp 8.5 - pessimistic") +
  #scale_y_continuous(labels = percent, limits = c(0,1)) +
  facet_wrap(plot_data1$species, ncol = 4) +scale_x_discrete(labels=c("Not Suitable", "Loss", "Stable"))+
  theme_bw()



