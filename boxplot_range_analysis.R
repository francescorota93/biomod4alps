setwd("/data/models/")
t<-read.table("results_range_analysis/range_analysis.txt", sep="\t", h=T)
t[is.na(t)]
t <- na.omit(t)
# levels(t$Species) <- c(levels(t$Species), "Campanula") 
# t$Species[t$Species == "morettiana"] <- "Campanula"
# levels(t$Species) <- c(levels(t$Species), "Rhizobotrya") 
# t$Species[t$Species == "alpina"] <- "Rhizobotrya"
# levels(t$Species) <- c(levels(t$Species), "Saxifraga") 
# t$Species[t$Species == "facchinii"] <- "Saxifraga"
# levels(t$Species) <- c(levels(t$Species), "Sempervivum") 
# t$Species[t$Species == "dolomiticum"] <- "Sempervivum"


library(ggplot2)
library(PMCMRplus)
myTheme <- theme(
	panel.background = element_rect(fill = "white", colour = "black"), 
	panel.grid.major =  element_blank(),
	panel.grid.minor = element_blank(),
	axis.text = element_text(color = "black", size=14),
	axis.text.x = element_text(color = "black", size=14),
)

# p<-ggplot(data = t, aes(x=Species, y=range_change, fill = Algo)) 
# p<-p+geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)
# p<-p+scale_fill_manual(values=c("grey100","grey60", "grey80","grey70","grey90"))
# p+myTheme +  facet_wrap(interaction(t$Scenario, t$year))
# ggsave("range_change.pdf", width = 10, height = 10)

### without algorithms
p<-ggplot(data = t, aes(x=Species, y=range_change)) 
p<-p+geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)
p+myTheme +scale_x_discrete(labels=c("Campanula morettiana","Festuca austrodolomitica","Gentiana brentae", "Nigritella bushmanniae", "Primula tyrolensis", "Rhizobotrya alpina", "Saxifraga facchinii", "Sempervivum dolomiticum"),
guide = guide_axis(n.dodge = 2)) #+  facet_wrap(interaction(t$year))
ggsave("range_change1.pdf", width = 20, height = 10)

p<-ggplot(data = t, aes(x=Species, y=range_turnover, fill = as.factor(Scenario))) 
p<-p+geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)
p<-p+scale_fill_manual(values=c("grey100","grey60"))
p+myTheme +  facet_wrap(interaction(t$year))
ggsave("range_turnover1.pdf", width = 10, height = 10)



p<-ggplot(data = t, aes(x=Species, y= range_turnover, fill = Algo, by = interaction(Scenario))) 
p<-p+geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)
p<-p+scale_fill_manual(values=c("grey100","grey60", "grey80","grey70","grey90"))
p+myTheme +  facet_wrap(t$year)
ggsave("range_turnover.pdf", width = 10, height = 5)

p<-ggplot(data = t, aes(x=Species, y= range_loss, fill = Algo, by = interaction(Scenario))) 
p<-p+geom_boxplot(outlier.fill = NULL, outlier.shape = 1, outlier.size = 1.5)
p<-p+scale_fill_manual(values=c("grey100","grey60", "grey80","grey70","grey90"))
p+myTheme +  facet_wrap(t$year)
ggsave("range_loss.pdf", width = 10, height = 5)

mean(t$range_change)
summary(t)


