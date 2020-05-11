setwd("/data/models/")

#### TSS
e_c<-dir("eval_future", full.names=T, pattern="TSS")
df<-data.frame(model=NA,species=NA,algo=NA, eval=NA)

for(i in 1:length(e_c)){
t<-read.table(e_c[i], sep="\t",h=T)
t1<-as.vector(as.matrix(t))
st<-strsplit(e_c[i],"_")[[1]]
algo<-strsplit(st[2],"/")[[1]][2]
sp<-st[4]
algo1<-rep(algo, length(t1))
sp1<-rep(sp, length(t1))
mod<-rep("clim",length(t1))
df1<-data.frame(model=mod,species=sp1,algo=algo1, eval=t1)
df<-rbind(df,df1)
}
df_clim<-df[-1,]

e_c<-dir("eval_topography", full.names=T, pattern="TSS")
df<-data.frame(model=NA,species=NA,algo=NA, eval=NA)

for(i in 1:length(e_c)){
t<-read.table(e_c[i], sep="\t",h=T)
t1<-as.vector(as.matrix(t))
st<-strsplit(e_c[i],"_")[[1]]
algo<-strsplit(st[2],"/")[[1]][2]
sp<-st[4]
algo1<-rep(algo, length(t1))
sp1<-rep(sp, length(t1))
mod<-rep("topo",length(t1))
df1<-data.frame(model=mod,species=sp1,algo=algo1, eval=t1)
df<-rbind(df,df1)
}
df_topo<-df[-1,]


# e_c<-dir("topoclimate/eval_topoclimate/", full.names=T, pattern="TSS")
# df<-data.frame(model=NA,species=NA,algo=NA, eval=NA)
# 
# for(i in 1:length(e_c)){
# t<-read.table(e_c[i], sep="\t",h=T)
# t1<-as.vector(as.matrix(t))
# st<-strsplit(e_c[i],"_")[[1]]
# algo<-strsplit(st[2],"/")[[1]][2]
# sp<-st[4]
# algo1<-rep(algo, length(t1))
# sp1<-rep(sp, length(t1))
# mod<-rep("topoclim",length(t1))
# df1<-data.frame(model=mod,species=sp1,algo=algo1, eval=t1)
# df<-rbind(df,df1)
# }
# df_topoclim<-df[-1,]
# 
# 
df_TSS<-rbind(df_clim,df_topo)
save(df_TSS, file = "df_TSS.RData")

library(ggplot2)
library(PMCMRplus)
myTheme <- theme(
    panel.background = element_rect(fill = "white", colour = "black"), 
    panel.grid.major =  element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black", size=12),
    axis.text.x = element_text(color = "black", size=12, face = "italic"),
)
my_y_title <- expression(paste("Total Sum of Square (TSS)"))
p<-ggplot(data = df_TSS, aes(x=species, y=eval, fill =model)) 
p<-p+geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)
p<-p+scale_fill_manual(values=c("grey100","grey60"))+ labs(y=my_y_title)
p+myTheme + scale_x_discrete(breaks=c("Campanula","Festuca","Gentiana", "Nigritella", "Primula", "Rhizobotrya", "Saxifraga", "Sempervivum"),
    labels=c("Campanula morettiana","Festuca austrodolomitica","Gentiana brentae", "Nigritella bushmanniae", "Primula tyrolensis", "Rhizobotrya alpina", "Saxifraga facchinii", "Sempervivum dolomiticum"),
    guide = guide_axis(n.dodge = 2))

ggsave("results_evaluation/TSS.pdf")

### cambiare specie per ogni
md<-subset(df, species=="Sempervivum")


#kruskal.test(eval~ model, data = md)
rbi<-kwAllPairsNemenyiTest(as.vector(md$eval),as.factor(md$model), dist = "Chisquare")
rbi


write.table(rbi$ p.value,"kw.txt",sep="\t")
ggsave("fig.pdf", dpi=600)

#### ROC
e_c<-dir("eval_future", full.names=T, pattern="ROC")
df<-data.frame(model=NA,species=NA,algo=NA, eval=NA)

for(i in 1:length(e_c)){
    t<-read.table(e_c[i], sep="\t",h=T)
    t1<-as.vector(as.matrix(t))
    st<-strsplit(e_c[i],"_")[[1]]
    algo<-strsplit(st[2],"/")[[1]][2]
    sp<-st[4]
    algo1<-rep(algo, length(t1))
    sp1<-rep(sp, length(t1))
    mod<-rep("clim",length(t1))
    df1<-data.frame(model=mod,species=sp1,algo=algo1, eval=t1)
    df<-rbind(df,df1)
}
df_clim<-df[-1,]

e_c<-dir("eval_topography", full.names=T, pattern="ROC")
df<-data.frame(model=NA,species=NA,algo=NA, eval=NA)

for(i in 1:length(e_c)){
    t<-read.table(e_c[i], sep="\t",h=T)
    t1<-as.vector(as.matrix(t))
    st<-strsplit(e_c[i],"_")[[1]]
    algo<-strsplit(st[2],"/")[[1]][2]
    sp<-st[4]
    algo1<-rep(algo, length(t1))
    sp1<-rep(sp, length(t1))
    mod<-rep("topo",length(t1))
    df1<-data.frame(model=mod,species=sp1,algo=algo1, eval=t1)
    df<-rbind(df,df1)
}
df_topo<-df[-1,]


# e_c<-dir("topoclimate/eval_topoclimate/", full.names=T, pattern="ROC")
# df<-data.frame(model=NA,species=NA,algo=NA, eval=NA)
# 
# for(i in 1:length(e_c)){
# t<-read.table(e_c[i], sep="\t",h=T)
# t1<-as.vector(as.matrix(t))
# st<-strsplit(e_c[i],"_")[[1]]
# algo<-strsplit(st[2],"/")[[1]][2]
# sp<-st[4]
# algo1<-rep(algo, length(t1))
# sp1<-rep(sp, length(t1))
# mod<-rep("topoclim",length(t1))
# df1<-data.frame(model=mod,species=sp1,algo=algo1, eval=t1)
# df<-rbind(df,df1)
# }
# df_topoclim<-df[-1,]
# 
# 
df_ROC<-rbind(df_clim,df_topo)
save(df_ROC, file = "df_ROC.RData")

library(ggplot2)
library(PMCMRplus)
myTheme <- theme(
    panel.background = element_rect(fill = "white", colour = "black"), 
    panel.grid.major =  element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black", size=12),
    axis.text.x = element_text(color = "black", size=12, face = "italic"),
)
my_y_title <- expression(paste("ROC curve"))
p<-ggplot(data = df_ROC, aes(x=species, y=eval, fill =model)) 
p<-p+geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)
p<-p+scale_fill_manual(values=c("grey100","grey60"))+ labs(y=my_y_title)
p+myTheme + scale_x_discrete(breaks=c("Campanula","Festuca","Gentiana", "Nigritella", "Primula", "Rhizobotrya", "Saxifraga", "Sempervivum"),
                             labels=c("Campanula morettiana","Festuca austrodolomitica","Gentiana brentae", "Nigritella bushmanniae", "Primula tyrolensis", "Rhizobotrya alpina", "Saxifraga facchinii", "Sempervivum dolomiticum"),
                             guide = guide_axis(n.dodge = 2))

ggsave("results_evaluation/ROC.pdf")





