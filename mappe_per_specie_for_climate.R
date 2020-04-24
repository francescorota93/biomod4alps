library(raster)
library(dplyr)
setwd("C:/Users/FRota/Desktop/topoclim/climate/models/")

#setwd("models") 
## CAMPANULA MORETTIANA
d<-as.data.frame(dir(".", full.names=T))

sp <- levels(list(data.frame(do.call('rbind', strsplit(as.character(d[,1]),'./',fixed=TRUE)))[,2])[[1]])
sp <- sp[1:5]
al <- levels(list(data.frame(do.call('rbind', strsplit(as.character(d[,1]),'.',fixed=TRUE)))[,4])[[1]])


# #l<-as.data.frame((strsplit(as.data.frame(d), ".", fixed = TRUE)))
# for(i in 1:length(sp)){
	al1<-sp[1]
	al2<-sp[2]
	al3<-sp[3]
	al4<-sp[4]
	al5<-sp[5]
	
	d2<-dir(".", patter="geo_NAzero.tif")
	
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
	##60
	gbm85_60<-stack(dir(al1, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	glm85_60<-stack(dir(al2, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	mars85_60<-stack(dir(al3, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	max85_60<-stack(dir(al4, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	rf85_60<-stack(dir(al5, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	
	
	s85_60<-stack(gbm85_60, glm85_60,mars85_60,max85_60,rf85_60)
	m85_60<-sum(s85_60)
	plot(m85_60)
	name2<-paste0("Campanula_morettiana","_85_60_consensus.tif")
	writeRaster(m85_60,name2,overwrite=TRUE)
	
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
	m85_60_r<-reclassify(m85_60, c(0,37,0, 37,75,2))
	m85_80_r<-reclassify(m85_80, c(0,37,0, 37,75,2))
	
	m85_60_s<-sum(ps_r,m85_60_r)
	m85_80_s<-sum(ps_r,m85_80_r)

	writeRaster(m85_60_s,"Campanula_morettiana_85_60_sintesi.tif")
	writeRaster(m85_80_s,"Campanula_morettiana_85_80_sintesi.tif") 
	
	
	######optimistic############
	##60
	gbm45_60<-stack(dir(al1, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	glm45_60<-stack(dir(al2, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	mars45_60<-stack(dir(al3, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	max45_60<-stack(dir(al4, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	rf45_60<-stack(dir(al5, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	
	
	s45_60<-stack(gbm45_60, glm45_60,mars45_60,max45_60,rf45_60)
	m45_60<-sum(s45_60)
	plot(m45_60)
	name2<-paste0("Campanula_morettiana","_45_60_consensus.tif")    
	writeRaster(m45_60,name2,overwrite=TRUE)
	
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
	m45_60_r<-reclassify(m45_60, c(0,37,0, 37,75,2))
	m45_80_r<-reclassify(m45_80, c(0,37,0, 37,75,2))
	
	m45_60_s<-sum(ps_r,m45_60_r)
	m45_80_s<-sum(ps_r,m45_80_r)
	
	writeRaster(m45_60_s,"Campanula_morettiana_45_60_sintesi.tif")
	writeRaster(m45_80_s,"Campanula_morettiana_45_80_sintesi.tif") 
	
	
	
# }
	
## RHIZOBOTRYA ALPINA
	library(raster)
	library(dplyr)
	setwd("C:/Users/FRota/Desktop/topoclim/climate/models/")
	
	#setwd("modelli") 
	d<-as.data.frame(dir(".", full.names=T))
	
	sp <- levels(list(data.frame(do.call('rbind', strsplit(as.character(d[,1]),'./',fixed=TRUE)))[,2])[[1]])
	sp <- sp[6:10]
	
	# #l<-as.data.frame((strsplit(as.data.frame(d), ".", fixed = TRUE)))
	# for(i in 1:length(sp)){
	al1<-sp[1]
	al2<-sp[2]
	al3<-sp[3]
	al4<-sp[4]
	al5<-sp[5]
	
	d2<-dir(".", patter=".tif")
	
	######presente############
	presGBM <-stack(dir(al1, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presGLM<-stack(dir(al2, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presMARS<-stack(dir(al3, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presMAX<-stack(dir(al4, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presRF<-stack(dir(al5, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	
	
	p<-stack(presGBM,presGLM,presMARS,presMAX,presRF)
	ps<-sum(p)
	plot(ps)
	name1<-paste0("Rhizobotrya_alpina","_pres_consensus.tif")
	writeRaster(ps,name1,overwrite=TRUE)
	
	######pessimistic############
	##60
	gbm85_60<-stack(dir(al1, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	glm85_60<-stack(dir(al2, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	mars85_60<-stack(dir(al3, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	max85_60<-stack(dir(al4, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	rf85_60<-stack(dir(al5, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	
	
	s85_60<-stack(gbm85_60, glm85_60,mars85_60,max85_60,rf85_60)
	m85_60<-sum(s85_60)
	plot(m85_60)
	name2<-paste0("Rhizobotrya_alpina","_85_60_consensus.tif")    
	writeRaster(m85_60,name2,overwrite=TRUE)
	
	##80
	gbm85_80<-stack(dir(al1, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	glm85_80<-stack(dir(al2, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	mars85_80<-stack(dir(al3, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	max85_80<-stack(dir(al4, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	rf85_80<-stack(dir(al5, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	
	
	s85_80<-stack(gbm85_80, glm85_80,mars85_80,max85_80,rf85_80)
	m85_80<-sum(s85_80)
	plot(m85_80)
	name2<-paste0("Rhizobotrya_alpina","_85_80_consensus.tif")
	writeRaster(m85_80,name2,overwrite=TRUE)
	
	ps_r<-reclassify(ps, c(0,7,0, 7,15,1))
	m85_60_r<-reclassify(m85_60, c(0,37,0, 37,75,2))
	m85_80_r<-reclassify(m85_80, c(0,37,0, 37,75,2))
	
	m85_60_s<-sum(ps_r,m85_60_r)
	m85_80_s<-sum(ps_r,m85_80_r)
	writeRaster(m85_60_s,"Rhizobotrya_alpina_85_60_sintesi.tif")
	writeRaster(m85_80_s,"Rhizobotrya_alpina_85_80_sintesi.tif") 
	
	######optimistic############
	##60
	gbm45_60<-stack(dir(al1, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	glm45_60<-stack(dir(al2, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	mars45_60<-stack(dir(al3, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	max45_60<-stack(dir(al4, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	rf45_60<-stack(dir(al5, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	
	
	s45_60<-stack(gbm45_60, glm45_60,mars45_60,max45_60,rf45_60)
	m45_60<-sum(s45_60)
	plot(m45_60)
	name2<-paste0("Rhizobotrya_alpina","_45_60_consensus.tif")    
	writeRaster(m45_60,name2,overwrite=TRUE)
	
	##80
	gbm45_80<-stack(dir(al1, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	glm45_80<-stack(dir(al2, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	mars45_80<-stack(dir(al3, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	max45_80<-stack(dir(al4, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	rf45_80<-stack(dir(al5, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	
	
	s45_80<-stack(gbm45_80, glm45_80,mars45_80,max45_80,rf45_80)
	m45_80<-sum(s45_80)
	plot(m45_80)
	name2<-paste0("Rhizobotrya_alpina","_45_80_consensus.tif")
	writeRaster(m45_80,name2,overwrite=TRUE)
	
	ps_r<-reclassify(ps, c(0,7,0, 7,15,1))
	m45_60_r<-reclassify(m45_60, c(0,37,0, 37,75,2))
	m45_80_r<-reclassify(m45_80, c(0,37,0, 37,75,2))
	
	m45_60_s<-sum(ps_r,m45_60_r)
	m45_80_s<-sum(ps_r,m45_80_r)
	writeRaster(m45_60_s,"Rhizobotrya_alpina_45_60_sintesi.tif")
	writeRaster(m45_80_s,"Rhizobotrya_alpina_45_80_sintesi.tif") 
	
	## Saxifraga facchinii
	library(raster)
	library(dplyr)
	setwd("C:/Users/FRota/Desktop/topoclim/climate/models/")
	
	#setwd("modelli") 
	d<-as.data.frame(dir(".", full.names=T))
	
	sp <- levels(list(data.frame(do.call('rbind', strsplit(as.character(d[,1]),'./',fixed=TRUE)))[,2])[[1]])
	sp <- sp[11:15]
	
	# #l<-as.data.frame((strsplit(as.data.frame(d), ".", fixed = TRUE)))
	# for(i in 1:length(sp)){
	al1<-sp[1]
	al2<-sp[2]
	al3<-sp[3]
	al4<-sp[4]
	al5<-sp[5]
	
	d2<-dir(".", patter=".tif")
	
	######presente############
	presGBM <-stack(dir(al1, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presGLM<-stack(dir(al2, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presMARS<-stack(dir(al3, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presMAX<-stack(dir(al4, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presRF<-stack(dir(al5, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	
	
	p<-stack(presGBM,presGLM,presMARS,presMAX,presRF)
	ps<-sum(p)
	plot(ps)
	name1<-paste0("Saxifraga_facchinii","_pres_consensus.tif")
	writeRaster(ps,name1,overwrite=TRUE)
	
	######pessimistic############
	##60
	gbm85_60<-stack(dir(al1, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	glm85_60<-stack(dir(al2, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	mars85_60<-stack(dir(al3, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	max85_60<-stack(dir(al4, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	rf85_60<-stack(dir(al5, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	
	
	s85_60<-stack(gbm85_60, glm85_60,mars85_60,max85_60,rf85_60)
	m85_60<-sum(s85_60)
	plot(m85_60)
	name2<-paste0("Saxifraga_facchinii","_85_60_consensus.tif")    
	writeRaster(m85_60,name2,overwrite=TRUE)
	
	##80
	gbm85_80<-stack(dir(al1, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	glm85_80<-stack(dir(al2, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	mars85_80<-stack(dir(al3, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	max85_80<-stack(dir(al4, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	rf85_80<-stack(dir(al5, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	
	
	s85_80<-stack(gbm85_80, glm85_80,mars85_80,max85_80,rf85_80)
	m85_80<-sum(s85_80)
	plot(m85_80)
	name2<-paste0("Saxifraga_facchinii","_85_80_consensus.tif")
	writeRaster(m85_80,name2,overwrite=TRUE)
	
	ps_r<-reclassify(ps, c(0,7,0, 7,15,1))
	m85_60_r<-reclassify(m85_60, c(0,37,0, 37,75,2))
	m85_80_r<-reclassify(m85_80, c(0,37,0, 37,75,2))
	
	m85_60_s<-sum(ps_r,m85_60_r)
	m85_80_s<-sum(ps_r,m85_80_r)
	writeRaster(m85_60_s,"Saxifraga_facchinii_85_60_sintesi.tif")
	writeRaster(m85_80_s,"Saxifraga_facchinii_85_80_sintesi.tif") 

	
	######optimistic############
	##60
	gbm45_60<-stack(dir(al1, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	glm45_60<-stack(dir(al2, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	mars45_60<-stack(dir(al3, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	max45_60<-stack(dir(al4, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	rf45_60<-stack(dir(al5, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	
	
	s45_60<-stack(gbm45_60, glm45_60,mars45_60,max45_60,rf45_60)
	m45_60<-sum(s45_60)
	plot(m45_60)
	name2<-paste0("Saxifraga_facchinii","_45_60_consensus.tif")    
	writeRaster(m45_60,name2,overwrite=TRUE)
	
	##80
	gbm45_80<-stack(dir(al1, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	glm45_80<-stack(dir(al2, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	mars45_80<-stack(dir(al3, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	max45_80<-stack(dir(al4, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	rf45_80<-stack(dir(al5, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	
	
	s45_80<-stack(gbm45_80, glm45_80,mars45_80,max45_80,rf45_80)
	m45_80<-sum(s45_80)
	plot(m45_80)
	name2<-paste0("Saxifraga_facchinii","_45_80_consensus.tif")
	writeRaster(m45_80,name2,overwrite=TRUE)
	
	ps_r<-reclassify(ps, c(0,7,0, 7,15,1))
	m45_60_r<-reclassify(m45_60, c(0,37,0, 37,75,2))
	m45_80_r<-reclassify(m45_80, c(0,37,0, 37,75,2))
	
	m45_60_s<-sum(ps_r,m45_60_r)
	m45_80_s<-sum(ps_r,m45_80_r)
	writeRaster(m45_60_s,"Saxifraga_facchinii_45_60_sintesi.tif")
	writeRaster(m45_80_s,"Saxifraga_facchinii_45_80_sintesi.tif")
	
	
	## Sempervivum_dolomiticum
	library(raster)
	library(dplyr)
	setwd("C:/Users/FRota/Desktop/topoclim/climate/models/")
	
	#setwd("modelli") 
	d<-as.data.frame(dir(".", full.names=T))
	
	sp <- levels(list(data.frame(do.call('rbind', strsplit(as.character(d[,1]),'./',fixed=TRUE)))[,2])[[1]])
	sp <- sp[16:20]
	
	# #l<-as.data.frame((strsplit(as.data.frame(d), ".", fixed = TRUE)))
	# for(i in 1:length(sp)){
	al1<-sp[1]
	al2<-sp[2]
	al3<-sp[3]
	al4<-sp[4]
	al5<-sp[5]
	
	d2<-dir(".", patter=".tif")
	
	######presente############
	presGBM <-stack(dir(al1, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presGLM<-stack(dir(al2, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presMARS<-stack(dir(al3, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presMAX<-stack(dir(al4, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	presRF<-stack(dir(al5, pattern=glob2rx("*current*geo_NAzero.tif*"), full=T))
	
	
	p<-stack(presGBM,presGLM,presMARS,presMAX,presRF)
	ps<-sum(p)
	plot(ps)
	name1<-paste0("Sempervivum_dolomiticum","_pres_consensus.tif")
	writeRaster(ps,name1,overwrite=TRUE)
	
	######pessimistic############
	##60
	gbm85_60<-stack(dir(al1, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	glm85_60<-stack(dir(al2, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	mars85_60<-stack(dir(al3, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	max85_60<-stack(dir(al4, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	rf85_60<-stack(dir(al5, pattern=glob2rx("*85_60*geo_NAzero.tif*"), full=T))
	
	
	s85_60<-stack(gbm85_60, glm85_60,mars85_60,max85_60,rf85_60)
	m85_60<-sum(s85_60)
	plot(m85_60)
	name2<-paste0("Sempervivum_dolomiticum","_85_60_consensus.tif")    
	writeRaster(m85_60,name2,overwrite=TRUE)
	
	##80
	gbm85_80<-stack(dir(al1, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	glm85_80<-stack(dir(al2, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	mars85_80<-stack(dir(al3, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	max85_80<-stack(dir(al4, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	rf85_80<-stack(dir(al5, pattern=glob2rx("*85_80*geo_NAzero.tif*"), full=T))
	
	
	s85_80<-stack(gbm85_80, glm85_80,mars85_80,max85_80,rf85_80)
	m85_80<-sum(s85_80)
	plot(m85_80)
	name2<-paste0("Sempervivum_dolomiticum","_85_80_consensus.tif")
	writeRaster(m85_80,name2,overwrite=TRUE)
	
	ps_r<-reclassify(ps, c(0,7,0, 7,15,1))
	m85_60_r<-reclassify(m85_60, c(0,37,0, 37,75,2))
	m85_80_r<-reclassify(m85_80, c(0,37,0, 37,75,2))
	
	m85_60_s<-sum(ps_r,m85_60_r)
	m85_80_s<-sum(ps_r,m85_80_r)
	writeRaster(m85_60_s,"Sempervivum_dolomiticum_85_60_sintesi.tif")
	writeRaster(m85_80_s,"Sempervivum_dolomiticum_85_80_sintesi.tif") 
	
	
	######optimistic############
	##60
	gbm45_60<-stack(dir(al1, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	glm45_60<-stack(dir(al2, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	mars45_60<-stack(dir(al3, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	max45_60<-stack(dir(al4, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	rf45_60<-stack(dir(al5, pattern=glob2rx("*45_60*geo_NAzero.tif*"), full=T))
	
	
	s45_60<-stack(gbm45_60, glm45_60,mars45_60,max45_60,rf45_60)
	m45_60<-sum(s45_60)
	plot(m45_60)
	name2<-paste0("Sempervivum_dolomiticum","_45_60_consensus.tif")    
	writeRaster(m45_60,name2,overwrite=TRUE)
	
	##80
	gbm45_80<-stack(dir(al1, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	glm45_80<-stack(dir(al2, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	mars45_80<-stack(dir(al3, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	max45_80<-stack(dir(al4, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	rf45_80<-stack(dir(al5, pattern=glob2rx("*45_80*geo_NAzero.tif*"), full=T))
	
	
	s45_80<-stack(gbm45_80, glm45_80,mars45_80,max45_80,rf45_80)
	m45_80<-sum(s45_80)
	plot(m45_80)
	name2<-paste0("Sempervivum_dolomiticum","_45_80_consensus.tif")
	writeRaster(m45_80,name2,overwrite=TRUE)
	
	ps_r<-reclassify(ps, c(0,7,0, 7,15,1))
	m45_60_r<-reclassify(m45_60, c(0,37,0, 37,75,2))
	m45_80_r<-reclassify(m45_80, c(0,37,0, 37,75,2))
	
	m45_60_s<-sum(ps_r,m45_60_r)
	m45_80_s<-sum(ps_r,m45_80_r)
	writeRaster(m45_60_s,"Sempervivum_dolomiticum_45_60_sintesi.tif")
	writeRaster(m45_80_s,"Sempervivum_dolomiticum_45_80_sintesi.tif") 
	
	
	