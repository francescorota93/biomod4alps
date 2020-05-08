library(raster)
#setwd("C:/Users/FRota/Desktop/models_past/bin")

d<-dir("/data/models/bin_geo/", full.names=T)

library(dplyr)
library(tidyr)

all_file <- list.files("/data/models/bin_geo/", pattern = ".tif")
name_curr <- all_file[grepl("current", all_file)]
name <- all_file[!grepl("current", all_file)]


df = t(data.frame(strsplit(name,split = "_")))
colnames(df) <- c("proj","GCM_scenario","year","genre","species","algo","bin","threshold", "extension")
df = as_tibble(df)
df = df %>%
	separate(GCM_scenario, c("GCM", "scenario"), 2)

df_curr = t(data.frame(strsplit(name_curr,split = "_")))
colnames(df_curr) <- c("proj","scenario","genre","species","algo","bin","threshold", "extension")
df_curr = as_tibble(df_curr)
df_curr = df_curr %>% mutate(GCM = NA,year = NA)

df_tot = bind_rows(df,df_curr)

df_tot_split = df_tot %>%
	group_by_at(vars(-GCM,-scenario,-threshold,-year)) %>% 
	group_split()

get_lists = function(x){
	
	x %>% transmute(path = ifelse(scenario=="current",
																paste0(proj,"_",
																			 scenario,"_",genre,"_",
																			 species,"_",algo,"_",
																			 bin,"_",threshold),
																paste0(proj,"_",
																			 GCM,scenario,"_",
																			 year,"_",genre,"_",
																			 species,"_",algo,"_",
																			 bin,"_",threshold))
	) %>% as.list()
	
}

paths = sapply(df_tot_split, get_lists )

paths

df<-data.frame(Species=NA, Algo=NA, GCM=NA,Scenario=NA, Year =NA, Threshold=NA, present_range=NA, range_gain=NA,range_loss=NA, range_change=NA, range_turnover=NA)

### ciclo for per gruppo
for(i in 1:length(paths)){
	d2 <- paths[[i]]
	c<-grep("current",d2)
	o<-grep("45",d2)
	p<-grep("85",d2)
	cur<-stack(d2[c])
	ot<-stack(d2[o])
	ps<-stack(d2[p])
	cr<-as.matrix(cellStats(cur, sum))
	rownames(cr)<-names(cur)
	otr<-as.matrix(cellStats(ot, sum))
	psr<-as.matrix(cellStats(ps, sum))
	m<-rbind(cr, otr,psr)
	s<-stack(ot,ps)
	df1<-data.frame(Species=NA, Algo=NA, GCM=NA,Scenario=NA, Year =NA, Threshold=NA, present_range=NA, range_gain=NA,range_loss=NA, range_change=NA, range_turnover=NA)
	x1_curr <- unlist(strsplit(d2[i], "[_]"))[2]
	if (x1_curr == "current"){
		gn<-unlist(strsplit(d2[i], "[_]"))[3]
		sp<-unlist(strsplit(d2[i], "[_]"))[4] 
		algo<-unlist(strsplit(d2[i], "[_]"))[5]
	} else {
		gn<-unlist(strsplit(d2[i], "[_]"))[4]
		sp<-unlist(strsplit(d2[i], "[_]"))[5] 
		algo<-unlist(strsplit(d2[i], "[_]"))[6]
	}
	sp <- paste0(gn,"_",sp)
	
	for(j in 1:nlayers(s)){
		x_curr <- unlist(strsplit(names(s[[j]]),"_"))[2]
		if (x_curr == "current"){
			gcm <- x_curr
			sce <- x_curr
			y <- x_curr
			tres <- unlist(strsplit(names(s[[j]]),"_"))[7]
		} else {
			gcm<-substr(unlist(strsplit(names(s[[j]]),"_"))[2], 1,2)
			sce<-substr(unlist(strsplit(names(s[[j]]),"_"))[2], 3,4)
			y <-unlist(strsplit(names(s[[j]]),"_"))[3]
			tres <- unlist(strsplit(names(s[[j]]),"_"))[8]
		}
		#Present range
		pr<-cellStats(cur, sum)
		#Range loss
		rl<-cellStats(abs(reclassify((cur+s[[j]]*2), c(1.5,3,0,1,1.5,1))),sum)
		#Range gain
		rg<-cellStats(abs(s[[j]]-cur), sum)
		#Range change C = 100*(RG  RL)/PR
		c<-((rg-rl)*100)/pr
		#Range turnover T = 100*(RL + RG)/(PR + RG)
		t<-((rg+rl)*100)/(pr+rg)
		df2<-data.frame(Species=sp, Algo=algo, GCM=gcm,Scenario=sce, Year = y, Threshold = tres, present_range=pr,range_gain=rg,range_loss=rl, range_change=c, range_turnover=t)
		df1<-rbind(df1,df2)}
	df<-rbind(df,df1)
}

write.table(df, "range_analysis.txt", sep="\t", row.names = FALSE )



###############################################################################
#####################							###################
#####################			SINTESI			###################
#####################							###################
###############################################################################
library(plyr)

t<-read.table("range_analysis.txt", sep="\t", h=T)
t[is.na(t)]<-0

rbi<-ddply(t , .(Species, Scenario), summarize,
					 mean_rc = round(mean(range_change), 3),
					 sd_rc = round(sd(range_change), 3),
					 mean_rt = round(mean(range_turnover), 3),
					 sd_rt = round(sd(range_turnover), 3),
					 mean_rg = round(mean(range_gain), 3),
					 sd_rg = round(sd(range_gain), 3),
					 mean_rl = round(mean(range_loss), 3),
					 sd_rl = round(sd(range_loss), 3),
					 mean_pr = round(mean(present_range), 3),
					 sd_pr = round(sd(present_range), 3)
)
write.table(rbi, "sintesi range analysis.txt", sep="\t")