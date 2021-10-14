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
library(ggplot2)
library(multcompView)
library(rcompanion)
df_all_topo <- data.frame(CCV_45 = NA, CCV_85 = NA, elev = NA,  TRI = NA,   TCI = NA,
         habitat_opt_50 = NA,  habitat_pes_50 = NA, habitat_opt_100 = NA, habitat_pes_100 = NA, species = NA)
spec1 <- c("Campanula_morettiana", "Festuca_austrodolomitica", "Gentiana_brentae", "Nigritella_buschmanniae",
             "Primula_tyrolensis", "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum")
for(i in seq_along(spec1)){
srtm <- stack(paste0("habitat_topohetero_models/",spec1[i],"_stack_habitat_category.tif"))
names(srtm) <- c("CCV_45", "CCV_85", "elev",  "TRI",   "TCI",
         "habitat_opt_50",  "habitat_pes_50", "habitat_opt_100", "habitat_pes_100")
b <- as.data.frame(srtm, na.rm =TRUE)
names(b) <- c("CCV_45", "CCV_85", "elev",  "TRI",   "TCI",
         "habitat_opt_50",  "habitat_pes_50", "habitat_opt_100", "habitat_pes_100")

#cols <- c("habitat_opt_50",  "habitat_pes_50", "habitat_opt_100", "habitat_pes_100")
#b[cols] <- lapply(b[cols], factor)
#b1 <- subset(b, habitat_opt_50!="0")
#b1<- droplevels(b1)
b[,"species"] <- rep(spec1[i], nrow(b))
df_all_topo <- rbind(df_all_topo, b)
}

write.csv(df_all_topo[-1,], file = "habitat_topohetero_models/df_all_topo.csv")
## make it for both optimistic and pessimistic
#### summarize data ######### and then plot
df_all_topo <- read.csv("habitat_topohetero_models/df_all_topo.csv")
### correlation matrix
df_cor <- df_all_topo %>% select(CCV_45,CCV_85,elev,TCI,TRI)
cor_var <- cor(df_cor, use = "all.obs")
write.table(cor_var, "habitat_topohetero_models/correlation_matrix_test_variables.txt")
### dividi per ottimistico e pessimistico due subset
## opt
df_summary_opt_100 <- df_all_topo %>% 
               group_by(species, habitat_opt_100) %>%
               summarize(CCV_45.mean = mean(CCV_45), 
                          CCV_45.sd = sd(CCV_45), 
                          elev.mean = mean(elev),
                          elev.sd = sd(elev),
                          TCI.mean = mean(TCI),
                          TCI.sd = sd(TCI),
                          TRI.mean = mean(TRI),
                          TRI.sd = sd(TRI))
 
### make dataframe and use it for the plot
## means
data_df <- as.data.frame(df_summary_opt_100)
df <- data_df
dodge <- position_dodge(width = 0.9)
### Reshape data frame values
data_df_1 <- data.frame(habitat = df[,2], 
                       species = df[,1],                           # Reshape data frame
                       y = c(df[,3], df[,5], df[,7],df[,9]),
                       group = c(rep("CCV_45.mean", nrow(df)),
                                 rep("elev.mean", nrow(df)),
                                 rep("TCI.mean", nrow(df)),
                                 rep("TRI.mean", nrow(df))))
## remove 0 not suitable
data_df_1 <- data_df_1 %>% filter(habitat!= 0)
data_df_1$habitat <- factor(data_df_1$habitat)

data_df_2 <- data.frame(habitat = df[,2], 
                       species = df[,1],                           # Reshape data frame
                       y = c(df[,4], df[,6], df[,8],df[,10]),
                       group = c(rep("CCV_45.sd", nrow(df)),
                                 rep("elev.sd", nrow(df)),
                                 rep("TCI.sd", nrow(df)),
                                 rep("TRI.sd", nrow(df))))
## remove 0 not suitable
data_df_2 <- data_df_2 %>% filter(habitat!= 0)

limits <- aes(ymax = data_df_1$y + data_df_2$y,
            ymin = data_df_1$y - data_df_2$y)

# #### grouped means
# p <- ggplot(data = data_df_1, aes(x = habitat, y = y), fill = habitat)
# p + geom_bar(stat = "identity", position = dodge) +
#   geom_errorbar(limits, position = dodge, width = 0.25) + 
#   #labs(x = species, y = NULL) +
#   facet_wrap( ~ group, scales="free", ncol=4)+
#   scale_x_discrete(labels = c("Loss", "Gain", "Stable"))+
#    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#         axis.title.x=element_blank(), legend.position="none", vjust = 0.5, hjust=1) +
#   theme_bw() 
  
# ggsave(paste0("habitat_topohetero_models/graphs/",spec1[i],"_habitat_opt_100.pdf"))

#### by species
p <- ggplot(data = data_df_1, aes(x = habitat, y = y), fill = habitat)
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) + 
  #labs(x = species, y = NULL) +
  facet_grid(cols = vars(species) , rows = vars(group), switch = "y",scales="free")+
  #facet_wrap(species ~ group, scales="free", ncol=4)+
  scale_x_discrete(labels = c("Loss", "Gain", "Stable"))+
   theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none", vjust = 0.5, hjust=1, strip.placement = "outside",
        strip.text.x = element_text(size = 6, colour = "black", angle = 90)) +
  theme_bw() 
ggsave("habitat_topohetero_models/graphs/endemics_habitat_opt_100.pdf", width = 210, height = 297, units = "mm")

## statistics to be added on graph
## Note that the values in the table are p-values comparing each
###   pair of groups.
### Pairwise Mann-Whitney
#p <- test$p.value
## select only opt 100 variables
b_opt_100 <- df_all_topo %>% select(species,CCV_45, elev, TRI, TCI, habitat_opt_100) 
b_opt_100$habitat_opt_100 <- factor(b_opt_100$habitat_opt_100)
## remove 0 not suitable
b1 <- b_opt_100 %>% filter(habitat_opt_100 != "0")
b1$habitat_opt_100 <- factor(b1$habitat_opt_100)
## species
spec1 <- c("Campanula_morettiana", "Festuca_austrodolomitica", "Gentiana_brentae", "Nigritella_buschmanniae",
             "Primula_tyrolensis", "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum")

#####################
### CLIMATE CHANGE VELOCITY 
############
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$CCV_45, factor(b$habitat_opt_100), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values/p_values_",spec1[i],"_CCV_habitat_opt_100.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters/letters_",spec1[i],"_CCV_habitat_opt_100.txt"), sep = "\t",
            row.names = FALSE)
}

#####################
### ELEVATION
############
# select only opt 100 variables
#b_opt_100 <- df_all_topo %>% select(species,CCV_45, elev, TRI, TCI, habitat_opt_100) 
#b_opt_100$habitat_opt_100 <- factor(b_opt_100$habitat_opt_100)
## remove 0 not suitable
#b1 <- b_opt_100 %>% filter(habitat_opt_100 != "0")
#b1$habitat_opt_100 <- factor(b1$habitat_opt_100)
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$elev, factor(b$habitat_opt_100), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values/p_values_",spec1[i],"_elev_habitat_opt_100.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters/letters_",spec1[i],"_elev_habitat_opt_100.txt"), sep = "\t",
            row.names = FALSE)
}

#####################
### TCI
############
# select only opt 100 variables
#b_opt_100 <- df_all_topo %>% select(species,CCV_45, elev, TRI, TCI, habitat_opt_100) 
#b_opt_100$habitat_opt_100 <- factor(b_opt_100$habitat_opt_100)
## remove 0 not suitable
#b1 <- b_opt_100 %>% filter(habitat_opt_100 != "0")
#b1$habitat_opt_100 <- factor(b1$habitat_opt_100)
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$TCI, factor(b$habitat_opt_100), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values/p_values_",spec1[i],"_TCI_habitat_opt_100.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters/letters_",spec1[i],"_TCI_habitat_opt_100.txt"), sep = "\t",
            row.names = FALSE)
}

#####################
### TRI
############
# select only opt 100 variables
#b_opt_100 <- df_all_topo %>% select(species,CCV_45, elev, TRI, TCI, habitat_opt_100) 
#b_opt_100$habitat_opt_100 <- factor(b_opt_100$habitat_opt_100)
## remove 0 not suitable
#b1 <- b_opt_100 %>% filter(habitat_opt_100 != "0")
#b1$habitat_opt_100 <- factor(b1$habitat_opt_100)
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$TRI, factor(b$habitat_opt_100), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values/p_values_",spec1[i],"_TRI_habitat_opt_100.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters/letters_",spec1[i],"_TRI_habitat_opt_100.txt"), sep = "\t",
            row.names = FALSE)
}


#########################################
## PESSIMISTIC
############################### 
## pes
df_summary_pes_100 <- df_all_topo %>% 
               group_by(species, habitat_pes_100) %>%
               summarize(CCV_85.mean = mean(CCV_85), 
                          CCV_85.sd = sd(CCV_85), 
                          elev.mean = mean(elev),
                          elev.sd = sd(elev),
                          TCI.mean = mean(TCI),
                          TCI.sd = sd(TCI),
                          TRI.mean = mean(TRI),
                          TRI.sd = sd(TRI))
 
### make dataframe and use it for the plot
## means
data_df <- as.data.frame(df_summary_pes_100)
df <- data_df
dodge <- position_dodge(width = 0.9)
### Reshape data frame values
data_df_1 <- data.frame(habitat = df[,2], 
                       species = df[,1],                           # Reshape data frame
                       y = c(df[,3], df[,5], df[,7],df[,9]),
                       group = c(rep("CCV_85.mean", nrow(df)),
                                 rep("elev.mean", nrow(df)),
                                 rep("TCI.mean", nrow(df)),
                                 rep("TRI.mean", nrow(df))))
## remove 0 not suitable
data_df_1 <- data_df_1 %>% filter(habitat!= 0)
data_df_1$habitat <- factor(data_df_1$habitat)

data_df_2 <- data.frame(habitat = df[,2], 
                       species = df[,1],                           # Reshape data frame
                       y = c(df[,4], df[,6], df[,8],df[,10]),
                       group = c(rep("CCV_85.sd", nrow(df)),
                                 rep("elev.sd", nrow(df)),
                                 rep("TCI.sd", nrow(df)),
                                 rep("TRI.sd", nrow(df))))
## remove 0 not suitable
data_df_2 <- data_df_2 %>% filter(habitat!= 0)

limits <- aes(ymax = data_df_1$y + data_df_2$y,
            ymin = data_df_1$y - data_df_2$y)

# #### grouped means
# p <- ggplot(data = data_df_1, aes(x = habitat, y = y), fill = habitat)
# p + geom_bar(stat = "identity", position = dodge) +
#   geom_errorbar(limits, position = dodge, width = 0.25) + 
#   #labs(x = species, y = NULL) +
#   facet_wrap( ~ group, scales="free", ncol=4)+
#   scale_x_discrete(labels = c("Loss", "Gain", "Stable"))+
#    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#         axis.title.x=element_blank(), legend.position="none", vjust = 0.5, hjust=1) +
#   theme_bw() 
  
# ggsave(paste0("habitat_topohetero_models/graphs/",spec1[i],"_habitat_pes_100.pdf"))

#### by species
p <- ggplot(data = data_df_1, aes(x = habitat, y = y), fill = habitat)
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) + 
  #labs(x = species, y = NULL) +
  facet_grid(cols = vars(species) , rows = vars(group), switch = "y",scales="free")+
  #facet_wrap(species ~ group, scales="free", ncol=4)+
  scale_x_discrete(labels = c("Loss", "Gain", "Stable"))+
   theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none", vjust = 0.5, hjust=1, strip.placement = "outside",
        strip.text.x = element_text(size = 6, colour = "black", angle = 90)) +
  theme_bw() 
ggsave("habitat_topohetero_models/graphs/endemics_habitat_pes_100.pdf", width = 210, height = 297, units = "mm")



#### BOXPLOT ALL DF
## subset for opt 100
df_opt_100 <- df_all_topo %>% select(species,CCV_45, elev, TRI, TCI, habitat_opt_100) %>% filter(habitat_opt_100 != 0) 
str(df_opt_100)

df_opt_100$habitat_opt_100 <- as.factor(df_opt_100$habitat_opt_100)

# Reshape data frame
data_df <- data.frame(habitat = df_opt_100[,6], 
                       species = df_opt_100[,1],                           
                       y = c(df_opt_100[,2], df_opt_100[,3], df_opt_100[,5],df_opt_100[,4]),
                       group = c(rep("CCV_45", nrow(df_opt_100)),
                                 rep("elev", nrow(df_opt_100)),
                                 rep("TCI", nrow(df_opt_100)),
                                 rep("TRI", nrow(df_opt_100))))
#### by species
p <- ggplot(data = data_df, aes(x = habitat, y = y))
p + geom_boxplot(outlier.shape = NA) +
  #facet_grid(cols = vars(species) , rows = vars(group), switch = "y",scales="free")+
  facet_wrap(species~group, scales="free", ncol=8)+
  scale_x_discrete(labels = c("Loss", "Gain", "Stable"))+
   theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none", vjust = 0.5, hjust=1, strip.placement = "outside",
        strip.text.x = element_text(size = 6, colour = "black", angle = 90)) +
        stat_summary(fun.y=mean, colour="darkred", geom="point", 
                           shape=18, size=3,show_guide = FALSE) +
  theme_bw() 


ggsave("habitat_topohetero_models/graphs/boxplot_endemics_habitat_opt_100.pdf", width = 210, height = 297, units = "mm")




## statistics to be added on graph
## Note that the values in the table are p-values comparing each
###   pair of groups.
### Pairwise Mann-Whitney
#p <- test$p.value
## select only pes 100 variables
b_pes_100 <- df_all_topo %>% select(species,CCV_85, elev, TRI, TCI, habitat_pes_100) 
b_pes_100$habitat_pes_100 <- factor(b_pes_100$habitat_pes_100)
## remove 0 not suitable
b1 <- b_pes_100 %>% filter(habitat_pes_100 != "0")
b1$habitat_pes_100 <- factor(b1$habitat_pes_100)
## species
spec1 <- c("Campanula_morettiana", "Festuca_austrodolomitica", "Gentiana_brentae", "Nigritella_buschmanniae",
             "Primula_tyrolensis", "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum")
#####################
### CLIMATE CHANGE VELOCITY 
############
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$CCV_85, factor(b$habitat_pes_100), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values/p_values_",spec1[i],"_CCV_habitat_pes_100.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters/letters_",spec1[i],"_CCV_habitat_pes_100.txt"), sep = "\t",
            row.names = FALSE)
}


#####################
### ELEVATION
############
# select only pes 100 variables
#b_pes_100 <- df_all_topo %>% select(species,CCV_85, elev, TRI, TCI, habitat_pes_100) 
#b_pes_100$habitat_pes_100 <- factor(b_pes_100$habitat_pes_100)
## remove 0 not suitable
b1 <- b_pes_100 %>% filter(habitat_pes_100 != "0")
b1$habitat_pes_100 <- factor(b1$habitat_pes_100)
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$elev, factor(b$habitat_pes_100), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values/p_values_",spec1[i],"_elev_habitat_pes_100.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters_",spec1[i],"elev_habitat_pes_100.txt"), sep = "\t",
            row.names = FALSE)
}

#####################
### TCI
############
# select only pes 100 variables
#b_pes_100 <- df_all_topo %>% select(species,CCV_85, elev, TRI, TCI, habitat_pes_100) 
#b_pes_100$habitat_pes_100 <- factor(b_pes_100$habitat_pes_100)
## remove 0 not suitable
b1 <- b_pes_100 %>% filter(habitat_pes_100 != "0")
b1$habitat_pes_100 <- factor(b1$habitat_pes_100)
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$TCI, factor(b$habitat_pes_100), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values/p_values_",spec1[i],"_TCI_habitat_pes_100.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters/letters_",spec1[i],"_TCI_habitat_pes_100.txt"), sep = "\t",
            row.names = FALSE)
}

#####################
### TRI
############
# select only pes 100 variables
#b_pes_100 <- df_all_topo %>% select(species,CCV_85, elev, TRI, TCI, habitat_pes_100) 
#b_pes_100$habitat_pes_100 <- factor(b_pes_100$habitat_pes_100)
## remove 0 not suitable
b1 <- b_pes_100 %>% filter(habitat_pes_100 != "0")
b1$habitat_pes_100 <- factor(b1$habitat_pes_100)
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$TRI, factor(b$habitat_pes_100), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values/p_values_",spec1[i],"_TRI_habitat_pes_100.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters/letters_",spec1[i],"_TRI_habitat_pes_100.txt"), sep = "\t",
            row.names = FALSE)
}

#########################################
## majority consensus 50
#############################################
#### summarize data ######### and then plot
df_all_topo <- read.csv("habitat_topohetero_models/df_all_topo.csv")
### dividi per ottimistico e pessimistico due subset
## opt
df_summary_opt_50 <- df_all_topo %>% 
               group_by(species, habitat_opt_50) %>%
               summarize(CCV_45.mean = mean(CCV_45), 
                          CCV_45.sd = sd(CCV_45), 
                          elev.mean = mean(elev),
                          elev.sd = sd(elev),
                          TCI.mean = mean(TCI),
                          TCI.sd = sd(TCI),
                          TRI.mean = mean(TRI),
                          TRI.sd = sd(TRI))
 
### make dataframe and use it for the plot
## means
data_df <- as.data.frame(df_summary_opt_50)
df <- data_df
dodge <- position_dodge(width = 0.9)
### Reshape data frame values
data_df_1 <- data.frame(habitat = df[,2], 
                       species = df[,1],                           # Reshape data frame
                       y = c(df[,3], df[,5], df[,7],df[,9]),
                       group = c(rep("CCV_45.mean", nrow(df)),
                                 rep("elev.mean", nrow(df)),
                                 rep("TCI.mean", nrow(df)),
                                 rep("TRI.mean", nrow(df))))
## remove 0 not suitable
data_df_1 <- data_df_1 %>% filter(habitat!= 0) %>% mutate(habitat = recode(habitat, 
     "1" = "Loss",
     "2" = "Gain",
     "3" = "Stable"))
data_df_1$habitat <- factor(data_df_1$habitat, levels= c("Loss", "Stable", "Gain"))

data_df_2 <- data.frame(habitat = df[,2], 
                       species = df[,1],                           # Reshape data frame
                       y = c(df[,4], df[,6], df[,8],df[,10]),
                       group = c(rep("CCV_45.sd", nrow(df)),
                                 rep("elev.sd", nrow(df)),
                                 rep("TCI.sd", nrow(df)),
                                 rep("TRI.sd", nrow(df))))
## remove 0 not suitable and trasform in letters 
data_df_2 <- data_df_2 %>% filter(habitat!= 0) %>% mutate(habitat = recode(habitat, 
     "1" = "Loss",
     "2" = "Gain",
     "3" = "Stable")
  )
data_df_2$habitat <- factor(data_df_2$habitat, levels= c("Loss", "Stable", "Gain"))

limits <- aes(ymax = data_df_1$y + data_df_2$y,
            ymin = data_df_1$y - data_df_2$y)

# #### grouped means
# p <- ggplot(data = data_df_1, aes(x = habitat, y = y), fill = habitat)
# p + geom_bar(stat = "identity", position = dodge) +
#   geom_errorbar(limits, position = dodge, width = 0.25) + 
#   #labs(x = species, y = NULL) +
#   facet_wrap( ~ group, scales="free", ncol=4)+
#   scale_x_discrete(labels = c("Loss", "Gain", "Stable"))+
#    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#         axis.title.x=element_blank(), legend.position="none", vjust = 0.5, hjust=1) +
#   theme_bw() 
  
# ggsave(paste0("habitat_topohetero_models/graphs/",spec1[i],"_habitat_opt_50.pdf"))

#### by species
p <- ggplot(data = data_df_1, aes(x = habitat, y = y), fill = habitat)
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) + 
  #labs(x = species, y = NULL) +
  facet_grid(cols = vars(species) , rows = vars(group), switch = "y",scales="free")+
  #facet_wrap(species ~ group, scales="free", ncol=4)+
  scale_x_discrete(labels = c("Loss", "Stable", "Gain"))+
   theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none", vjust = 0.5, hjust=1, strip.placement = "outside",
        strip.text.x = element_text(size = 6, colour = "black", angle = 90)) +
  theme_bw() 
ggsave("habitat_topohetero_models/graphs/endemics_habitat_opt_50.pdf", width = 210, height = 297, units = "mm")

#### without bars #######################
#geom_point()
p <- ggplot(data = data_df_1, aes(x = habitat, y = y), fill = habitat)
p + geom_point(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) + 
  #labs(x = species, y = NULL) +
  facet_grid(cols = vars(species) , rows = vars(group), switch = "y",scales="free")+
  #facet_wrap(species ~ group, scales="free", ncol=4)+
  scale_x_discrete(labels = c("Loss",  "Stable", "Gain"))+
   theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none", vjust = 0.5, hjust=1, strip.placement = "outside",
        strip.text.x = element_text(size = 6, colour = "black", angle = 90)) +
  theme_bw() 
ggsave("habitat_topohetero_models/graphs/points_endemics_habitat_opt_50.pdf", width = 210, height = 297, units = "mm")




## statistics to be added on graph
## Note that the values in the table are p-values comparing each
###   pair of groups.
### Pairwise Mann-Whitney
#p <- test$p.value
## select only opt 50 variables
b_opt_50 <- df_all_topo %>% select(species,CCV_45, elev, TRI, TCI, habitat_opt_50) 
b_opt_50$habitat_opt_50 <- factor(b_opt_50$habitat_opt_50)
#b_opt_50$habitat_opt_50 <- factor(b_opt_50$habitat_opt_50, levels= c("Loss", "Stable", "Gain"))

## remove 0 not suitable and reorder levels
b1 <- b_opt_50 %>% filter(habitat_opt_50 != "0")  %>% mutate(habitat_opt_50 = recode(habitat_opt_50, 
     "1" = "Loss",
     "2" = "Gain",
     "3" = "Stable"))

b1$habitat_opt_50 <- factor(b1$habitat_opt_50 , levels= c("Loss", "Stable", "Gain"))

## species
spec1 <- c("Campanula_morettiana", "Festuca_austrodolomitica", "Gentiana_brentae", "Nigritella_buschmanniae",
             "Primula_tyrolensis", "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum")

#####################
### CLIMATE CHANGE VELOCITY 
############
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$CCV_45, factor(b$habitat_opt_50), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values_50/p_values_",spec1[i],"_CCV_habitat_opt_50.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters_50/letters_",spec1[i],"_CCV_habitat_opt_50.txt"), sep = "\t",
            row.names = FALSE)
}

#####################
### ELEVATION
############

for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$elev, factor(b$habitat_opt_50), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values_50/p_values_",spec1[i],"_elev_habitat_opt_50.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters_50/letters_",spec1[i],"_elev_habitat_opt_50.txt"), sep = "\t",
            row.names = FALSE)
}

#####################
### TCI
############

for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$TCI, factor(b$habitat_opt_50), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values_50/p_values_",spec1[i],"_TCI_habitat_opt_50.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters_50/letters_",spec1[i],"_TCI_habitat_opt_50.txt"), sep = "\t",
            row.names = FALSE)
}

#####################
### TRI
############
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$TRI, factor(b$habitat_opt_50), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values_50/p_values_",spec1[i],"_TRI_habitat_opt_50.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters_50/letters_",spec1[i],"_TRI_habitat_opt_50.txt"), sep = "\t",
            row.names = FALSE)
}


#########################################
## PESSIMISTIC
############################### 
## pes
df_summary_pes_50 <- df_all_topo %>% 
               group_by(species, habitat_pes_50) %>%
               summarize(CCV_85.mean = mean(CCV_85), 
                          CCV_85.sd = sd(CCV_85), 
                          elev.mean = mean(elev),
                          elev.sd = sd(elev),
                          TCI.mean = mean(TCI),
                          TCI.sd = sd(TCI),
                          TRI.mean = mean(TRI),
                          TRI.sd = sd(TRI))
 
### make dataframe and use it for the plot
## means
data_df <- as.data.frame(df_summary_pes_50)
df <- data_df
dodge <- position_dodge(width = 0.9)
### Reshape data frame values
data_df_1 <- data.frame(habitat = df[,2], 
                       species = df[,1],                           # Reshape data frame
                       y = c(df[,3], df[,5], df[,7],df[,9]),
                       group = c(rep("CCV_85.mean", nrow(df)),
                                 rep("elev.mean", nrow(df)),
                                 rep("TCI.mean", nrow(df)),
                                 rep("TRI.mean", nrow(df))))
## remove 0 not suitable and change to loss stable and gain
data_df_1 <- data_df_1 %>% filter(habitat!= 0) %>% mutate(habitat = recode(habitat, 
     "1" = "Loss",
     "2" = "Gain",
     "3" = "Stable"))
data_df_1$habitat <- factor(data_df_1$habitat, levels= c("Loss", "Stable", "Gain"))

data_df_2 <- data.frame(habitat = df[,2], 
                       species = df[,1],                           # Reshape data frame
                       y = c(df[,4], df[,6], df[,8],df[,10]),
                       group = c(rep("CCV_45.sd", nrow(df)),
                                 rep("elev.sd", nrow(df)),
                                 rep("TCI.sd", nrow(df)),
                                 rep("TRI.sd", nrow(df))))
## remove 0 not suitable and trasform gain == 4
data_df_2 <- data_df_2 %>% filter(habitat!= 0) %>% mutate(habitat = recode(habitat, 
     "1" = "Loss",
     "2" = "Gain",
     "3" = "Stable")
  )
data_df_2$habitat <- factor(data_df_2$habitat, levels= c("Loss", "Stable", "Gain"))

#### by species
p <- ggplot(data = data_df_1, aes(x = habitat, y = y), fill = habitat)
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) + 
  #labs(x = species, y = NULL) +
  facet_grid(cols = vars(species) , rows = vars(group), switch = "y",scales="free")+
  #facet_wrap(species ~ group, scales="free", ncol=4)+
  scale_x_discrete(labels = c("Loss", "Gain", "Stable"))+
   theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none", vjust = 0.5, hjust=1, strip.placement = "outside",
        strip.text.x = element_text(size = 6, colour = "black", angle = 90)) +
  theme_bw() 
ggsave("habitat_topohetero_models/graphs/endemics_habitat_pes_50.pdf", width = 210, height = 297, units = "mm")


#### without bars #######################
#geom_point()
p <- ggplot(data = data_df_1, aes(x = habitat, y = y), fill = habitat)
p + geom_point(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) + 
  #labs(x = species, y = NULL) +
  facet_grid(cols = vars(species) , rows = vars(group), switch = "y",scales="free")+
  #facet_wrap(species ~ group, scales="free", ncol=4)+
  scale_x_discrete(labels = c("Loss",  "Stable", "Gain"))+
   theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.x=element_blank(), legend.position="none", vjust = 0.5, hjust=1, strip.placement = "outside",
        strip.text.x = element_text(size = 6, colour = "black", angle = 90)) +
  theme_bw() 
ggsave("habitat_topohetero_models/graphs/points_endemics_habitat_pes_50.pdf", width = 210, height = 297, units = "mm")


## statistics to be added on graph
## Note that the values in the table are p-values comparing each
###   pair of groups.
### Pairwise Mann-Whitney
#p <- test$p.value
## select only pes 50 variables
b_pes_50 <- df_all_topo %>% select(species,CCV_85, elev, TRI, TCI, habitat_pes_50) 
b_pes_50$habitat_pes_50 <- factor(b_pes_50$habitat_pes_50)
#b_pes_50$habitat_pes_50 <- factor(b_pes_50$habitat_pes_50, levels= c("Loss", "Stable", "Gain"))

## remove 0 not suitable and reorder levels
b1 <- b_pes_50 %>% filter(habitat_pes_50 != "0")  %>% mutate(habitat_pes_50 = recode(habitat_pes_50, 
     "1" = "Loss",
     "2" = "Gain",
     "3" = "Stable"))

b1$habitat_pes_50 <- factor(b1$habitat_pes_50 , levels= c("Loss", "Stable", "Gain"))

## species
spec1 <- c("Campanula_morettiana", "Festuca_austrodolomitica", "Gentiana_brentae", "Nigritella_buschmanniae",
             "Primula_tyrolensis", "Rhizobotrya_alpina", "Saxifraga_facchinii", "Sempervivum_dolomiticum")

#####################
### CLIMATE CHANGE VELOCITY 
############
for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$CCV_85, factor(b$habitat_pes_50), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values_50/p_values_",spec1[i],"_CCV_habitat_pes_50.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters_50/letters_",spec1[i],"_CCV_habitat_pes_50.txt"), sep = "\t",
            row.names = FALSE)
}


#####################
### ELEVATION
############

for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$elev, factor(b$habitat_pes_50), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values_50/p_values_",spec1[i],"_elev_habitat_pes_50.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters_50/letters_",spec1[i],"_elev_habitat_pes_50.txt"), sep = "\t",
            row.names = FALSE)
}

#####################
### TCI
#####################

for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$TCI, factor(b$habitat_pes_50), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values_50/p_values_",spec1[i],"_TCI_habitat_pes_50.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters_50/letters_",spec1[i],"_TCI_habitat_pes_50.txt"), sep = "\t",
            row.names = FALSE)
}

#####################
### TRI
############

for(i in seq_along(spec1)){
b <- b1 %>% filter(species == spec1[i])
# statistical test ## wilkoxon 
test <- pairwise.wilcox.test(b$TRI, factor(b$habitat_pes_50), p.adj = "bonf")
PT = test$p.value    ### Extract p-value table
PT1 = fullPTable(PT)
write.table(PT1,file = paste0("habitat_topohetero_models/graphs/p_values_50/p_values_",spec1[i],"_TRI_habitat_pes_50.txt"), sep = "\t",
            row.names = FALSE)
#PT1
### Produce compact letter display
let <- multcompLetters(PT1,
								compare="<",
								threshold=0.05,  # p-value to use as significance threshold
								Letters=letters,
								reversed = FALSE)
let1 <- as.data.frame(print(let))
write.table(let1, file = paste0("habitat_topohetero_models/graphs/letters_50/letters_",spec1[i],"_TRI_habitat_pes_50.txt"), sep = "\t",
            row.names = FALSE)
}
