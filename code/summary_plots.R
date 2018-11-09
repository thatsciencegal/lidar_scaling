library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(svglite)
library(ggpubr)

#In path
in.path <- "./Data/summary_data"

#Read in the summary csv
lidar_summary <- read.csv("./Data/summary_data/lidar_summary_data_3.csv")

#Turn tile number and resolution into a factor
lidar_summary$tile <- factor(lidar_summary$tile)
lidar_summary$resolution <- factor(lidar_summary$resolution)

#Function to filter out outliers for the plots
outlier_filter <- function(dat, col){
  col <- enquo(col)
  dat %>% 
    filter(!(abs((!!col) - median(!!col))>(2*sd(!!col))))
}

#Filter outliers for each metric (for plotting) 
lidar_summary_maxh <- outlier_filter(lidar_summary, max_height)
lidar_summary_meanh <- outlier_filter(lidar_summary, mean_height)
lidar_summary_rugh <- outlier_filter(lidar_summary, rugosity)
lidar_summary_vert_div <- outlier_filter(lidar_summary, vertical_diversity)
lidar_summary_open <- outlier_filter(lidar_summary, openness)

#Create a theme for the plots
theme_christine <- function (base_size = 12, base_family = "") {
  #theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
  theme(
    ##axis properties
    axis.text.x = element_text(colour = "black", size = 14),
    axis.text.y = element_text(colour = "black", size = 14),
    axis.title.x = element_text(colour = "black", size = 18),
    axis.title.y = element_text(colour = "black", angle=90, size = 24),
    
    ##panel properties
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(size=0),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    
    ##plot background
    plot.background = element_rect(fill="white"),
    
    ##legend properties
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "right",
    legend.title = element_text(colour = "black", size = 16)
    
  )   
}

#Change resolution to numeric, filter out 5x5 m resolution, change resolution back to factors
lidar_summary_maxh$resolution <- as.numeric(levels(lidar_summary_maxh$resolution))[lidar_summary_maxh$resolution]
lidar_summary_maxh2 <- lidar_summary_maxh %>% filter(resolution > 5)
lidar_summary_maxh2$resolution <- as.factor(lidar_summary_maxh2$resolution)

lidar_summary_meanh$resolution <- as.numeric(levels(lidar_summary_meanh$resolution))[lidar_summary_meanh$resolution]
lidar_summary_meanh2 <- lidar_summary_meanh %>% filter(resolution > 5)
lidar_summary_meanh2$resolution <- as.factor(lidar_summary_meanh2$resolution)

lidar_summary_rugh$resolution <- as.numeric(levels(lidar_summary_rugh$resolution))[lidar_summary_rugh$resolution]
lidar_summary_rug2 <- lidar_summary_rugh %>% filter(resolution > 5)
lidar_summary_rug2$resolution <- as.factor(lidar_summary_rug2$resolution)

lidar_summary_vert_div$resolution <- as.numeric(levels(lidar_summary_vert_div$resolution))[lidar_summary_vert_div$resolution]
lidar_summary_vert_div2 <- lidar_summary_vert_div %>% filter(resolution > 5)
lidar_summary_vert_div2$resolution <- as.factor(lidar_summary_vert_div2$resolution)

lidar_summary_open$resolution <- as.numeric(levels(lidar_summary_open$resolution))[lidar_summary_open$resolution]
lidar_summary_open2 <- lidar_summary_open %>% filter(resolution > 5)
lidar_summary_open2$resolution <- as.factor(lidar_summary_open2$resolution)

lidar_summary2 <- lidar_summary %>% filter(resolution > 5)
lidar_summary2$resolution <- as.factor(lidar_summary2$resolution)

#Make the boxplots for each metric
max_height_plot <- ggplot(lidar_summary_maxh2, aes(x=resolution, y=max_height, fill = topo))+#, colour = "#56B4E9"))+
  geom_boxplot()
theme_christine() +
  scale_fill_manual(values = c("#8c8c8c","#f5e626ff"), name = "Topography", 
                    breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  labs(x = "Grid cell length (m)", y = "Maximum height (m)")

mean_height_plot <- ggplot(lidar_summary_meanh2, aes(x=resolution, y=mean_height, fill = topo))+#, colour = "#56B4E9"))+
  geom_boxplot()
theme_christine() +
  scale_fill_manual(values = c("#8c8c8c","#f5e626ff"), name = "Topography", 
                    breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  labs(x = "Grid cell length (m)", y = "Mean height (m)")

rugosity_plot <- ggplot(lidar_summary_rug2, aes(x=resolution, y=rugosity, fill = topo))+#, colour = "#56B4E9"))+
  geom_boxplot()
theme_christine() +
  scale_fill_manual(values = c("#8c8c8c","#f5e626ff"), name = "Topography", 
                    breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  theme(legend.position = "none", axis.title.x = element_blank()) + 
  labs(x = "Grid cell length (m)", y = "Rugosity (m)")

vertical_diversity_plot <- ggplot(lidar_summary_vert_div2, aes(x=resolution, y=vertical_diversity, fill = topo))+#, colour = "#56B4E9"))+
  geom_boxplot()
theme_christine() +
  scale_fill_manual(values = c("#8c8c8c","#f5e626ff"), name = "Topography", 
                    breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  theme(legend.position = "none", axis.title.x = element_blank()) + 
  labs(x = "Grid cell length (m)", y = "Vertical Diversity")

openness_plot <- ggplot(lidar_summary_open2, aes(x=resolution, y=openness, fill = topo))+#, colour = "#56B4E9"))+
  geom_boxplot()
theme_christine() +
  scale_fill_manual(values = c("#8c8c8c","#f5e626ff"), name = "Topography", 
                    breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  theme(legend.position = "none", axis.title.x = element_blank()) + 
  labs(x = "Grid cell length (m)", y = "Canopy Openness (%)")

#Run and save the boxplots
max_height_plot
ggsave("./lidar-scaling/Images/max_height_plot.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

mean_height_plot
ggsave("./lidar-scaling/Images/mean_height_plot.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

rugosity_plot
ggsave("./lidar-scaling/Images/rugosity_plot.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

vertical_diversity_plot
ggsave("./lidar-scaling/Images/vertical_diversity_plot.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

openness_plot
ggsave("./lidar-scaling/Images/openness_plot.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)


##Raster of different metrics for an example plot at a 25x25 m scale
##Load libraries
library(rgdal)
library(raster)
library(sp)
library(viridis)

#Read in the data
dat <- read.csv("./Data/output/filtered_output/normalized_metrics_12_size_25.csv")

#Make a theme for plotting
theme_rast <- function (base_size = 12, base_family = "") {
  #theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
  theme(
    ##axis properties
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks=element_blank(),
    axis.line = element_blank(),
    
    ##panel properties
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    
    ##plot background
    plot.background = element_rect(fill="white"),
    
    ##legend properties
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "right",
    legend.title = element_text(colour = "black", size = 16)
    
  )   
}

#Plot the raster for each metric
maxh_rast<-ggplot(dat)+
  geom_tile(aes(x=X,y=Y,fill=zmax))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill = "Maximum height (m)") +
  theme_rast()

meanh_rast<-ggplot(dat)+
  geom_tile(aes(x=X,y=Y,fill=zmean))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Mean height (m)")+
  theme_rast()

rug_rast<-ggplot(dat)+
  geom_tile(aes(x=X,y=Y,fill=zsd))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Rugosity (m)") + 
  theme_rast()

vdiv_rast<-ggplot(dat)+
  geom_tile(aes(x=X,y=Y,fill=zentropy))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Vertical diversity") +
  theme_rast()

pground_rast<-ggplot(dat)+
  geom_tile(aes(x=X,y=Y,fill=pground))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Canopy openness (%)")+
  theme_rast()

#Run and save the plots
maxh_rast
ggsave("./Images/maxh_rast.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

meanh_rast
ggsave("./Images/meanh_rast.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

rug_rast
ggsave("./Images/rug_rast.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

vdiv_rast
ggsave("./Images/vdiv_rast.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

pground_rast
ggsave("./Images/pground_rast.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)


##Make raster plots for 10x10 m, 50x50 m, and 250x250 m resolutions
dat10 <- read.csv("./Data/output/filtered_output/normalized_metrics_12_size_10.csv")

vdiv10<-ggplot(dat10)+
  geom_tile(aes(x=X,y=Y,fill=zentropy))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Vertical diversity")+
  theme_rast()

dat50 <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/filtered_output/normalized_metrics_12_size_50.csv")

vdiv50<-ggplot(dat50)+
  geom_tile(aes(x=X,y=Y,fill=zentropy))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Vertical diversity") +
  theme_rast()

dat250 <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/filtered_output/normalized_metrics_12_size_250.csv")

vdiv250<-ggplot(dat250)+
  geom_tile(aes(x=X,y=Y,fill=zentropy))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Vertical diversity") +
  theme_rast()

#Histograms of vertical diversity based on FUSION categorization
#Read in the data
vdiv10.dat <- read.csv("./Data/output/bzn_high_topo_6_metrics10_all_returns_strata_stats.csv")
vdiv25.dat <- read.csv("./Data/output/bzn_high_topo_6_metrics25_all_returns_strata_stats.csv")
vdiv50.dat <- read.csv("./Data/output/bzn_high_topo_6_metrics50_all_returns_strata_stats.csv")
vdiv250.dat <- read.csv("./Data/output/bzn_high_topo_6_metrics250_all_returns_strata_stats.csv")

#Isolate the data needed for the analysis
vdiv10.dat<-vdiv10.dat[grepl("total.return.count",names(vdiv10.dat))]
vdiv25.dat<-vdiv25.dat[grepl("total.return.count",names(vdiv25.dat))]
vdiv50.dat<-vdiv50.dat[grepl("total.return.count",names(vdiv50.dat))]
vdiv250.dat<-vdiv250.dat[grepl("total.return.count",names(vdiv250.dat))]

#Take the mean and standard deviation of the total return count per bin
vdiv10.mean <- sapply(vdiv10.dat,mean)
vdiv10.sd <- sapply(vdiv10.dat,sd)
vdiv10.summary <- data.frame("mean"=vdiv10.mean,"sd"=vdiv10.sd, "bin"=seq(1:22))

vdiv25.mean <- sapply(vdiv25.dat,mean)
vdiv25.sd <- sapply(vdiv25.dat,sd)
vdiv25.summary <- data.frame("mean"=vdiv25.mean,"sd"=vdiv25.sd, "bin"=seq(1:22))

vdiv50.mean <- sapply(vdiv50.dat,mean)
vdiv50.sd <- sapply(vdiv50.dat,sd)
vdiv50.summary <- data.frame("mean" = vdiv50.mean, "sd"=vdiv50.sd, "bin"=seq(1:22))

vdiv250.mean <- sapply(vdiv250.dat,mean)
vdiv250.sd <- sapply(vdiv250.dat,sd)
vdiv250.summary <- data.frame("mean"=vdiv250.mean, "sd"=vdiv250.sd, "bin"=seq(1:22))

#Plot and save the data
ggplot(vdiv10.summary,aes(x=bin,y=mean))+
  geom_bar(stat="identity",color="black",fill="#f5e626ff")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,600))+
  scale_x_continuous(expand=c(0,0))+
  coord_flip()+
  labs(y="Mean number of returns", x="Height bin")

ggsave("./Images/height_hist10.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

ggplot(vdiv25.summary,aes(x=bin,y=mean))+
  geom_bar(stat="identity",color="black",fill="#21918dff")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd)) +
  scale_y_continuous(expand=c(0,0)
  )+
  scale_x_continuous(expand=c(0,0))+
  coord_flip()+
  labs(y="Mean number of returns", x="Height bin")

ggsave("./Images/height_hist25.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

ggplot(vdiv50.summary,aes(x=bin,y=mean))+
  geom_bar(stat="identity",color="black", fill="#3b518bff")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,12500))+
  scale_x_continuous(expand=c(0,0))+
  coord_flip()+
  labs(y="Mean number of returns", x="Height bin")
ggsave("./Images/height_hist50.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)


ggplot(vdiv250.summary,aes(x=bin,y=mean))+
  geom_bar(stat="identity",color="black", fill="black")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,250000))+
  scale_x_continuous(expand=c(0,0))+
  coord_flip() +
  labs(y="Mean number of returns", x="Height bin")
ggsave("./Images/height_hist250.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)


##Correlation plots
library(Hmisc)
library(reshape2)

#Read in the data
lidar_summary <- read.csv("./Data/summary_data/lidar_summary_data_3.csv")

#Change resolution into a factor
lidar_summary$tile <- factor(lidar_summary$tile)
lidar_summary$resolution <- factor(lidar_summary$resolution)

#Filter each resolution into a different data frame
lidar_summary_10 <- lidar_summary %>% 
  filter(resolution == 10)

lidar_summary_15 <- lidar_summary %>% 
  filter(resolution == 15)

lidar_summary_25 <- lidar_summary %>% 
  filter(resolution == 25)

lidar_summary_30 <- lidar_summary %>% 
  filter(resolution == 30)

lidar_summary_50 <- lidar_summary %>% 
  filter(resolution == 50)

lidar_summary_60 <- lidar_summary %>% 
  filter(resolution == 60)

lidar_summary_100 <- lidar_summary %>% 
  filter(resolution == 100)

lidar_summary_120 <- lidar_summary %>% 
  filter(resolution == 120)

lidar_summary_250 <- lidar_summary %>% 
  filter(resolution == 250)

lidar_summary_500 <- lidar_summary %>% 
  filter(resolution == 500)

#Filter only the columns needed for the plots
ls10 <- lidar_summary_10[,5:9]
ls15 <- lidar_summary_15[,5:9]
ls25 <- lidar_summary_25[,5:9]
ls30 <- lidar_summary_30[,5:9]
ls50 <- lidar_summary_50[,5:9]
ls60 <- lidar_summary_60[,5:9]
ls100 <- lidar_summary_100[,5:9]
ls120 <- lidar_summary_120[,5:9]
ls250 <- lidar_summary_250[,5:9]
ls500 <- lidar_summary_500[,5:9]

#Rename the columns
cols <- c("Maximum height","Mean height","Rugosity","Vertical diversity","Canopy openness")

colnames(ls10) <- cols
colnames(ls15) <- cols
colnames(ls25) <- cols
colnames(ls30) <- cols
colnames(ls50) <- cols
colnames(ls60) <- cols
colnames(ls100) <- cols
colnames(ls120) <- cols
colnames(ls250) <- cols
colnames(ls500) <- cols

#Run the correlation test
cor.ls10 <- cor(ls10, method="pearson")
cor.ls15 <- cor(ls15, method="pearson")
cor.ls25 <- cor(ls25, method="pearson")
cor.ls30 <- cor(ls30, method="pearson")
cor.ls50 <- cor(ls50, method="pearson")
cor.ls60 <- cor(ls60, method="pearson")
cor.ls100 <- cor(ls100, method="pearson")
cor.ls120 <- cor(ls120, method="pearson")
cor.ls250 <- cor(ls250, method="pearson")
cor.ls500 <- cor(ls500, method="pearson")

#Correlation test in hmisc to get p value
rcorr.ls10 <- rcorr(as.matrix(ls10), type="pearson")
rcorr.ls15 <- rcorr(as.matrix(ls15), type="pearson")
rcorr.ls25 <- rcorr(as.matrix(ls25), type="pearson")
rcorr.ls30 <- rcorr(as.matrix(ls30), type="pearson")
rcorr.ls50 <- rcorr(as.matrix(ls50), type="pearson")
rcorr.ls60 <- rcorr(as.matrix(ls60), type="pearson")
rcorr.ls100 <- rcorr(as.matrix(ls100), type="pearson")
rcorr.ls120 <- rcorr(as.matrix(ls120), type="pearson")
rcorr.ls250 <- rcorr(as.matrix(ls250), type="pearson")
rcorr.ls500 <- rcorr(as.matrix(ls500), type="pearson")

#Melt the data frame to put in correct format
cor.ls10.melt <- melt(cor.ls10)
cor.ls15.melt <- melt(cor.ls15)
cor.ls25.melt <- melt(cor.ls25)
cor.ls30.melt <- melt(cor.ls30)
cor.ls50.melt <- melt(cor.ls50)
cor.ls60.melt <- melt(cor.ls60)
cor.ls100.melt <- melt(cor.ls100)
cor.ls120.melt <- melt(cor.ls120)
cor.ls250.melt <- melt(cor.ls250)
cor.ls500.melt <- melt(cor.ls500)

#Add a column for resolution
cor.ls10.melt$res <- 10
cor.ls15.melt$res <- 15
cor.ls25.melt$res <- 25
cor.ls30.melt$res <- 30
cor.ls50.melt$res <- 50
cor.ls60.melt$res <- 60
cor.ls100.melt$res <- 100
cor.ls120.melt$res <- 120
cor.ls250.melt$res <- 250
cor.ls500.melt$res <- 500

#New data frames to add in p-value
ls10.p <- cbind(cor.ls10.melt,melt(rcorr.ls10[[3]]))
ls15.p <- cbind(cor.ls15.melt,melt(rcorr.ls15[[3]]))
ls25.p <- cbind(cor.ls25.melt,melt(rcorr.ls25[[3]]))
ls30.p <- cbind(cor.ls30.melt,melt(rcorr.ls30[[3]]))
ls50.p <- cbind(cor.ls50.melt,melt(rcorr.ls50[[3]]))
ls60.p <- cbind(cor.ls60.melt,melt(rcorr.ls60[[3]]))
ls100.p <- cbind(cor.ls100.melt,melt(rcorr.ls100[[3]]))
ls120.p <- cbind(cor.ls120.melt,melt(rcorr.ls120[[3]]))
ls250.p <- cbind(cor.ls250.melt,melt(rcorr.ls250[[3]]))
ls500.p <- cbind(cor.ls500.melt,melt(rcorr.ls500[[3]]))

#Put together all of the data
cor.melt <- rbind(cor.ls10.melt,cor.ls15.melt,cor.ls25.melt,cor.ls30.melt,cor.ls50.melt,cor.ls60.melt,cor.ls100.melt,cor.ls120.melt,cor.ls250.melt,cor.ls500.melt)
cor.melt.p <- rbind(ls10.p,ls15.p,ls25.p,ls30.p,ls50.p,ls60.p,ls100.p,ls120.p,ls250.p,ls500.p)

#Rename the columns
names(cor.melt.p)<-c("Var1","Var2","value","res","var3","var4","p-value")

#Select specific columns
cor.melt.p.sel <- select(cor.melt.p, 1,2,3,4,7)

#Filter data to make pairwise comparisons
cor.maxh.meanh <- cor.melt %>% filter(Var1=="Maximum height"&Var2=="Mean height")
cor.maxh.meanh$type <- 1
cor.maxh.rug <- cor.melt %>% filter(Var1=="Maximum height"&Var2=="Rugosity")
cor.maxh.rug$type <- 2
cor.maxh.vdiv <- cor.melt %>% filter(Var1=="Maximum height"&Var2=="Vertical diversity")
cor.maxh.vdiv$type <- 3
cor.maxh.open <- cor.melt %>% filter(Var1=="Maximum height"&Var2=="Canopy openness")
cor.maxh.open$type <- 4
cor.meanh.rug <- cor.melt %>% filter(Var1=="Mean height"&Var2=="Rugosity")
cor.meanh.rug$type <- 5
cor.meanh.vdiv <- cor.melt %>% filter(Var1=="Mean height"&Var2=="Vertical diversity")
cor.meanh.vdiv$type <-6
cor.meanh.open <- cor.melt %>% filter(Var1=="Mean height"&Var2=="Canopy openness")
cor.meanh.open$type <- 7
cor.rug.vdiv <- cor.melt %>% filter(Var1=="Rugosity"&Var2=="Vertical diversity")
cor.rug.vdiv$type <- 8
cor.rug.open <- cor.melt %>% filter(Var1=="Rugosity"&Var2=="Canopy openness")
cor.rug.open$type <- 9
cor.vdiv.open <- cor.melt %>% filter(Var1=="Vertical diversity"&Var2=="Canopy openness")
cor.vdiv.open$type <- 10

#Put the filtered data into a data frame
cor.lines <- rbind(cor.maxh.meanh,cor.maxh.rug,cor.maxh.vdiv,cor.maxh.open,cor.meanh.rug,cor.meanh.vdiv,cor.meanh.open,cor.rug.vdiv,cor.rug.open,cor.vdiv.open)

#Change "type" to a factor
cor.lines$type<-as.factor(cor.lines$type)

#Plot the relationships
ggplot(cor.lines, aes(x=res,y=value,color=type))+
  geom_line(size=1)+
  geom_point()+
  scale_color_viridis(option="cividis",discrete = T, labels=c("Maximum height vs mean height","Maximum height vs rugosity",
                                                              "Maximum height vs vertical diversity", "Maximum height vs canopy openness",
                                                              "Mean height vs rugosity", "Mean height vs vertical diversity",
                                                              "Mean height vs canopy openness", "Rugosity vs vertical diversity",
                                                              "Rugosity vs canopy openness", "Vertical diversity vs canopy openness"))

#Filter out the pairwise comparisons with the p-value data
cor.maxh.meanh.p <- cor.melt.p.sel %>% filter(Var1=="Maximum height"&Var2=="Mean height")
cor.maxh.meanh.p$type <- 1
cor.maxh.rug.p <- cor.melt.p.sel %>% filter(Var1=="Maximum height"&Var2=="Rugosity")
cor.maxh.rug.p$type <- 2
cor.maxh.vdiv.p <- cor.melt.p.sel %>% filter(Var1=="Maximum height"&Var2=="Vertical diversity")
cor.maxh.vdiv.p$type <- 3
cor.maxh.open.p <- cor.melt.p.sel %>% filter(Var1=="Maximum height"&Var2=="Canopy openness")
cor.maxh.open.p$type <- 4
cor.meanh.rug.p <- cor.melt.p.sel %>% filter(Var1=="Mean height"&Var2=="Rugosity")
cor.meanh.rug.p$type <- 5
cor.meanh.vdiv.p <- cor.melt.p.sel %>% filter(Var1=="Mean height"&Var2=="Vertical diversity")
cor.meanh.vdiv.p$type <-6
cor.meanh.open.p <- cor.melt.p.sel %>% filter(Var1=="Mean height"&Var2=="Canopy openness")
cor.meanh.open.p$type <- 7
cor.rug.vdiv.p <- cor.melt.p.sel %>% filter(Var1=="Rugosity"&Var2=="Vertical diversity")
cor.rug.vdiv.p$type <- 8
cor.rug.open.p <- cor.melt.p.sel %>% filter(Var1=="Rugosity"&Var2=="Canopy openness")
cor.rug.open.p$type <- 9
cor.vdiv.open.p <- cor.melt.p.sel %>% filter(Var1=="Vertical diversity"&Var2=="Canopy openness")
cor.vdiv.open.p$type <- 10

#Put the pairwaise data into a data frame
cor.lines.p <- rbind(cor.maxh.meanh.p,cor.maxh.rug.p,cor.maxh.vdiv.p,cor.maxh.open.p,cor.meanh.rug.p,cor.meanh.vdiv.p,
                     cor.meanh.open.p,cor.rug.vdiv.p,cor.rug.open.p,cor.vdiv.open.p)
#Change "type" into a factor
cor.lines.p$type<-as.factor(cor.lines.p$type)
#Change p-value to significant/not significant
cor.lines.p$plogic <- cor.lines.p$`p-value`<=0.05

#Plot the correlation data with information on significance
ggplot(cor.lines.p, aes(x=res,y=value,color=type))+
  geom_line(aes(linetype=type),size=1)+
  geom_point(aes(shape=plogic), size = 4)+
  scale_linetype_manual(values=c("solid","dotted","solid","dotted","solid",
                                 "dotted","solid","dotted","solid","dotted"),
                        labels=c("Maxh vs meanh","Maxh vs rug",
                                 "Maxh vs vdiv", "Maxh vs open",
                                 "Meanh vs rug", "Meanh vs vdiv",
                                 "Meanh vs open", "Rug vs vdiv",
                                 "Rug vs open", "Vdiv vs open"),
                        name = "Correlations")+
  scale_color_manual(values = c("#3b518bff", "#3b518bff", "#21918dff", "#21918dff","#f5e626ff",  
                                "#f5e626ff","#8c8c8c", "#8c8c8c","#000000","#000000"), 
                     labels=c("Maxh vs meanh","Maxh vs rug",
                              "Maxh vs vdiv", "Maxh vs open",
                              "Meanh vs rug", "Meanh vs vdiv",
                              "Meanh vs open", "Rug vs vdiv",
                              "Rug vs open", "Vdiv vs open"),
                     name = "Correlations")+
  scale_shape_manual(values = c(1,16), labels = c("Not sig","Sig"), name = "p-value")+
  labs(x="Cell length (m)",y="Correlation")+
  theme_christine()

#Save the plots
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/correlations2.jpg",plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

##Proportion histograms - create histograms for vertical diversity based on return proportions
#Read in the data
vdiv10.dat <- read.csv("./Data/output/bzn_high_topo_6_metrics10_all_returns_strata_stats.csv")
vdiv25.dat <- read.csv("./Data/output/bzn_high_topo_6_metrics25_all_returns_strata_stats.csv")
vdiv50.dat <- read.csv("./Data/output/bzn_high_topo_6_metrics50_all_returns_strata_stats.csv")
vdiv250.dat<- read.csv("./Data/output/bzn_high_topo_6_metrics250_all_returns_strata_stats.csv")

#Filter just the relevant data out (i.e., proportion of returns)
vdiv10.dat.filt <- vdiv10.dat[grepl("proportion",names(vdiv10.dat))]
vdiv25.dat.filt <- vdiv25.dat[grepl("proportion",names(vdiv25.dat))]
vdiv50.dat.filt <- vdiv50.dat[grepl("proportion",names(vdiv50.dat))]
vdiv250.dat.filt <- vdiv250.dat[grepl("proportion",names(vdiv250.dat))]

#Take the mean for each bin
vdiv10.summary <- data.frame(mean=sapply(vdiv10.dat.filt, mean),sd=sapply(vdiv10.dat.filt,sd),bin=seq(1:22))
vdiv25.summary <- data.frame(mean=sapply(vdiv25.dat.filt, mean),sd=sapply(vdiv25.dat.filt,sd),bin=seq(1:22))
vdiv50.summary <- data.frame(mean=sapply(vdiv50.dat.filt, mean),sd=sapply(vdiv50.dat.filt,sd),bin=seq(1:22))
vdiv250.summary <- data.frame(mean=sapply(vdiv250.dat.filt, mean),sd=sapply(vdiv250.dat.filt,sd),bin=seq(1:22))

#Create a theme for plotting the data
theme_vdiv <- function (base_size = 12, base_family = "") {
  #theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
  theme(
    ##axis properties
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks=element_blank(),
    axis.line = element_blank(),
    
    ##panel properties
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(),
    
    ##plot background
    plot.background = element_rect(fill="white"),
    
    ##legend properties
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "right",
    legend.title = element_text(colour = "black", size = 16)
    
  )   
}

#Plot and save each of the histograms
ggplot(vdiv10.summary, aes(x=bin, y=mean))+
  geom_bar(stat="identity", fill="#f5e626ff",color="black")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd))+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0), limits=c(0,0.35))+scale_x_continuous(expand=c(0,0))+
  labs(x="Height bin", y="Mean proportion of returns")+
  theme_christine()
ggsave("./Images/height_hist_10.jpg",plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

ggplot(vdiv25.summary, aes(x=bin, y=mean))+
  geom_bar(stat="identity", fill="#21918dff",color="black")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd))+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0), limits=c(0,0.35))+scale_x_continuous(expand=c(0,0))+
  labs(x="Height bin", y="Mean proportion of returns")+
  theme_christine()
ggsave("./Images/height_hist_25.jpg",plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

ggplot(vdiv50.summary, aes(x=bin,y=mean))+
  geom_bar(stat="identity", fill="#3b518bff",color="black")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd))+
  coord_flip()+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits=c(0,0.35))+
  labs(x="Height bin", y="Mean proportion of returns") +
  theme_christine()
ggsave("./Images/height_hist_50.jpg",plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)


ggplot(vdiv250.summary,aes(x=bin,y=mean))+
  geom_bar(stat="identity",fill="black",color="black")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd))+
  coord_flip()+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits=c(0,0.35))+
  labs(x="Height bin", y="Mean proportion of returns")+
  theme_christine()
ggsave("./Images/height_hist_250.jpg",plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

##Zone stats density (Not used in paper; used to check on the split between high and low relief standard deviations)
#Read in the data
bzs_zonestat <- read.csv("./Data/gis/bzs_zonestat.csv")
bzn_zonestat <- read.csv("./Data/gis/bzn_zonestat.csv")

#Put the north and south data into one data frame
bz_zonestat<-rbind(bzs_zonestat,bzn_zonestat) 

#Filter by high and low relief
bz_zstat_high<-filter(bz_zonestat, relief=="high") 
bz_zstat_low<-filter(bz_zonestat, relief=="low")

#Take the mean of the mean
mean(bz_zstat_high$X_mean)
mean(bz_zstat_low$X_mean)

#Take the mean standard deviation
mean(bz_zstat_high$X_std)
mean(bz_zstat_low$X_std)

#Plot and save the CDFs of the data
ggplot(data = bz_zonestat, aes(x=X_std,color=relief,fill=relief))+
  geom_density(alpha=0.5)+
  scale_color_manual(values=c("#8c8c8c","#f5e626ff"), 
                     name="Topography",labels=c("High relief","Low relief"))+
  scale_fill_manual(values=c("#8c8c8c","#f5e626ff"),
                    name="Topography",labels=c("High relief","Low relief"))+
  labs(x="Standard deviation of elevation (m)",y="Density")

ggsave("./Images/topo_density.jpg", plot=last_plot(), units="in",width=9,height=6,dpi=600)

#Plot and save the CDFs for maximum elevation for high and low relief
ggplot(data=bz_zonestat, aes(x=X_max,color=relief,fill=relief))+
  geom_density(alpha = 0.5)+
  scale_color_manual(values=c("#8c8c8c","#f5e626ff"), 
                     name="Topography",labels=c("High relief","Low relief"))+
  scale_fill_manual(values=c("#8c8c8c","#f5e626ff"),
                    name="Topography",labels=c("High relief","Low relief"))+
  labs(x="Maximum elevation (m)",y="Density")
ggsave("./Images/elevation_density.jpg", plot=last_plot(), units="in",width=9,height=6,dpi=600)

