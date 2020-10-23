setwd("/data/models/env_predictors/climate")
library(raster)
library(ggplot2)
cu <- list.files("present/")
cur <- stack(paste0("present/",cu))
list_fut <- list.dirs("./future")
i = 1:length(list_fut)
### future
print(list_fut[1])

h <- list.files(path=list_fut[1], recursive=TRUE, full.names=TRUE, pattern='.tif')

print(h)
PC1 <- h[grep("PC1",h)]
PC2 <- h[grep("PC2",h)]
opt_PC1 <- mean(stack(PC1[grep("45", PC1)]))
opt_PC2 <- mean(stack(PC2[grep("45", PC2)]))
pes_PC1 <- mean(stack(PC1[grep("85", PC1)]))
pes_PC2 <- mean(stack(PC2[grep("85", PC2)]))

## elevation
elev <- raster("../DTM_50.tif")
elev <- crop(elev, cur)
### stack env present, future and elevation
env <- stack(cur, opt_PC1, opt_PC2, pes_PC1, pes_PC2, elev)
names(env) <- c("PC1_cur", "PC2_cur", "PC1_opt", "PC2_opt", "PC1_pes", "PC2_pes", "elev")


## species
sp1<-read.table("../../endemic_dolo50.txt", sep="\t", h=T)
coordinates(sp1)= ~ x + y
rasValue=extract(env, sp1)
combinePointValue=cbind(sp1,rasValue)
write.table(combinePointValue,file="species_env_values.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)

## env df
df <- as.data.frame(env, xy = TRUE, na.rm =TRUE)
names(df) <- c("x","y","PC1_cur", "PC2_cur", "PC1_opt", "PC2_opt", "PC1_pes", "PC2_pes", "elev")
save(df, file = "../env_df_dolomites_50m.RData")

## species values from df
#df[grepl(sp1[,2:3], df)]


spec <- read.csv("../../results_pca/species_env_values.csv") 
df_cur <- rep("cur", 906)
df_opt <- rep("opt", 906)
df_pes <- rep("pes", 906)

col1 <- rep(spec$species, 3)
col2 <- c(spec$PC1_cur, spec$PC1_opt,  spec$PC1_pes)
col3 <- c(spec$PC2_cur, spec$PC2_opt,  spec$PC2_pes)
col4 <- c(df_cur, df_opt, df_pes)
col5 <- rep(spec$elev, 3)
col6 <- rep(spec$x, 3)
col7 <- rep(spec$y, 3)

df_spec <- data.frame(species=col1, PC1= col2, PC2 =col3, scenario = col4, elev = col5, x = col6, y = col7)
save(df_spec, file = "../env_df_dolomites_50m_PCA.RData")
load(file = "../../results_pca/env_df_dolomites_50m_PCA.RData")

pc1_cur <- df_spec[df_spec$scenario=="cur",]
pc2_cur <- df_spec[df_spec$scenario=="cur",]

# library(ggplot2)
# library(reshape2)
# plot on same grid, each series colored differently -- 
# good if the series have same scale
levels(df_spec$species) <- c("Campanula morettiana", "Festuca austrodolomitica", 
                             "Gentiana brentae", "Nigritella buschmanniae",
                             "Primula tyrolensis", "Rhizobotrya alpina",
                             "Saxifraga facchinii", "Sempervivum dolomiticum")

FacetEqualWrap <- ggproto(
  "FacetEqualWrap", FacetWrap,
  
  train_scales = function(self, x_scales, y_scales, layout, data, params) {
    
    # doesn't make sense if there is not an x *and* y scale
    if (is.null(x_scales) || is.null(x_scales)) {
      stop("X and Y scales required for facet_equal_wrap")
    }
    
    # regular training of scales
    ggproto_parent(FacetWrap, self)$train_scales(x_scales, y_scales, layout, data, params)
    
    # switched training of scales (x and y and y on x)
    for (layer_data in data) {
      match_id <- match(layer_data$PANEL, layout$PANEL)
      
      x_vars <- intersect(x_scales[[1]]$aesthetics, names(layer_data))
      y_vars <- intersect(y_scales[[1]]$aesthetics, names(layer_data))
      
      SCALE_X <- layout$SCALE_X[match_id]
      ggplot2:::scale_apply(layer_data, y_vars, "train", SCALE_X, x_scales)
      
      SCALE_Y <- layout$SCALE_Y[match_id]
      ggplot2:::scale_apply(layer_data, x_vars, "train", SCALE_Y, y_scales)
    }
    
  }
)

facet_wrap_equal <- function(...) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  ggproto(NULL, FacetEqualWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}

### circle and solid = current, square and dashed = optimistic 2080, triangle and dot-dashed = pessimistic 2080
spec_plot <- ggplot(df_spec, aes(PC1,PC2)) + 
  geom_point(aes(shape=scenario), colour = "grey") + 
  scale_shape_manual(values=c(1, 0, 6))+
  scale_color_grey(start = 0.8, end = 0.2)+ 
  stat_ellipse(mapping = aes(x = PC1,y=PC2, lty=factor(scenario)),
              alpha=0.5, type = "t")+
  scale_linetype_manual(values=c(1,2,4)) +
  facet_wrap_equal(~df_spec$species,ncol=4)+
  theme_bw() +
  theme(legend.position="none")
  


# or plot on different plots
#ggplot(df, aes(time,value)) + geom_line() + facet_grid(series ~ .)
