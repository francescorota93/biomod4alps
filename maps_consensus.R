### sintesi script


setwd("/data/models/bin_geo/") 
d <- list.files(".")
s <- read.table("../endemic_dolo50.txt", head = TRUE, sep = "\t")
species <- c(3,4)
for(i in species){
  sp.names<-levels(factor(s[,1]))[i]
  d2<-list.files(".", pattern = sp.names, full.names = TRUE)
  df <- stack(d2)

# for(i in 1:length(sp)){
al1<-sp[1]
al2<-sp[2]
al3<-sp[3]
al4<-sp[4]
al5<-sp[5]

d2<-dir(".", patter="geo.tif")

######presente############
presGBM <-stack(dir(al1, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
presGLM<-stack(dir(al2, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
presMARS<-stack(dir(al3, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
presMAX<-stack(dir(al4, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
presRF<-stack(dir(al5, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))


p<-stack(presGBM,presGLM,presMARS,presMAX,presRF)
ps<-sum(p)
plot(ps)
name1<-paste0("Campanula_morettiana","_pres_consensus.tif")
writeRaster(ps,name1,overwrite=TRUE)

######pessimistic############
##80
gbm85_80<-stack(dir(al1, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
glm85_80<-stack(dir(al2, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
mars85_80<-stack(dir(al3, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
max85_80<-stack(dir(al4, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
rf85_80<-stack(dir(al5, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))

s85_80<-stack(gbm85_80, glm85_80,mars85_80,max85_80,rf85_80)
m85_80<-sum(s85_80)
plot(m85_80)
name2<-paste0("Campanula_morettiana","_85_80_consensus.tif")
writeRaster(m85_80,name2,overwrite=TRUE)

ps_r<-reclassify(ps, c(0,7,0, 7,15,1))
m85_80_r<-reclassify(m85_80, c(0,37,0, 37,75,2))

m85_80_s<-sum(ps_r,m85_80_r)

writeRaster(m85_80_s,"Campanula_morettiana_85_80_sintesi.tif") 

######optimistic############
##80
gbm45_80<-stack(dir(al1, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
glm45_80<-stack(dir(al2, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
mars45_80<-stack(dir(al3, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
max45_80<-stack(dir(al4, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
rf45_80<-stack(dir(al5, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))


s45_80<-stack(gbm45_80, glm45_80,mars45_80,max45_80,rf45_80)
m45_80<-sum(s45_80)
plot(m45_80)
name2<-paste0("Campanula_morettiana","_45_80_consensus.tif")
writeRaster(m45_80,name2,overwrite=TRUE)

ps_r<-reclassify(ps, c(0,7,0, 7,15,1))
m45_80_r<-reclassify(m45_80, c(0,37,0, 37,75,2))

m45_80_s<-sum(ps_r,m45_80_r)

writeRaster(m45_80_s,"Campanula_morettiana_45_80_sintesi.tif") 