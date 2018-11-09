library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(cowplot)

# Remove all from environment
rm(list = ls())

# set data paths
in.path = "./Data/summary_data/"

# list all of the data to be used
file.names <- dir(in.path, pattern = "morans*", full.names = TRUE)

# Split the file names so we can pull out information about high and low relief
splits <- unlist(strsplit(file.names,"[.]"))

splits <- splits[grepl("['/Data*]",splits)]
splits.num <- unlist(strsplit(splits,"[_]"))
splits.num <- splits.num[!grepl("['/D*]",splits.num)]
splits.num <- splits.num[!grepl("[su*]",splits.num)]
splits.res <- splits.num[c(FALSE,TRUE)]
splits.tiles <- splits.num[c(T,F)]

# Create a data frame that stores the file names, tile number, and relief (high/low)
lidar.dat.structure <- cbind(file.names,splits.tiles,splits.res)
lidar.dat.structure <- as.data.frame(lidar.dat.structure)
lidar.dat.structure$splits.tiles <- as.numeric(lidar.dat.structure$splits.tiles)
lidar.dat.structure <- lidar.dat.structure %>% filter(splits.res != "5")
lidar.dat.structure$topo <- NA
lidar.dat.structure$topo[1:135] <- "HIGH"
lidar.dat.structure$topo[136:270] <- "LOW"
lidar.dat.structure$topo[271:405] <- "HIGH"
lidar.dat.structure$topo[406:540] <- "LOW"

##Filter out each resolution from the larger data frame
lidar.10 <- lidar.dat.structure %>% filter(splits.res == 10)
lidar.10$file.names <- as.character(lidar.10$file.names)
lidar.100 <- lidar.dat.structure %>% filter(splits.res == 100)
lidar.100$file.names <- as.character(lidar.100$file.names)
lidar.120 <- lidar.dat.structure %>% filter(splits.res == 120)
lidar.120$file.names <- as.character(lidar.120$file.names)
lidar.15 <- lidar.dat.structure %>% filter(splits.res == 15)
lidar.15$file.names <- as.character(lidar.15$file.names)
lidar.25 <- lidar.dat.structure %>% filter(splits.res == 25)
lidar.25$file.names <- as.character(lidar.25$file.names)
lidar.250 <- lidar.dat.structure %>% filter(splits.res == 250)
lidar.250$file.names <- as.character(lidar.250$file.names)
lidar.30 <- lidar.dat.structure %>% filter(splits.res == 30)
lidar.30$file.names <- as.character(lidar.30$file.names)
lidar.50 <- lidar.dat.structure %>% filter(splits.res == 50)
lidar.50$file.names <- as.character(lidar.50$file.names)
lidar.60 <- lidar.dat.structure %>% filter(splits.res == 60)
lidar.60$file.names <- as.character(lidar.60$file.names)

## Create an empty data to include all of the Moran's I data
dat <- data.frame(X = numeric(), dist = numeric(), zmax.Morans.i = numeric(), zmax.Null.lcl = numeric(), zmax.null.ucl = numeric(), zmax.Pvalue = numeric(),
                  zmean.Morans.i = numeric(), zmean.Null.lcl = numeric(), zmean.Null.ucl = numeric(), zmean.Pvalue = numeric(), zsd.Morans.i = numeric(),
                  zmsd.Null.lcl = numeric(), zsd.Null.ucl = numeric(), zsd.Pvalue = numeric(), zentropy.Morans.i = numeric(), zentropy.Null.lcl = numeric(),
                  zentropy.Null.ucl = numeric(), zentropy.Pvalue = numeric(), pground.Morans.i = numeric(), prground.Null.lcl = numeric(), pground.Null.ucl = numeric(),
                  pground.Pvalue = numeric())

data.bind <- function(x,y){
  #Function to put together Moran's I data and file name, tile number, resolution data
  for(i in 1:nrow(x)){
    new.dat <- read.csv(x$file.names[i])
    y <- rbind(y,new.dat)
  }
  y
}

#Add Moran's I data to file name, tile number, resolution data for each resolution
dat10 <- data.bind(lidar.10,dat)
dat100 <- data.bind(lidar.100,dat)
dat120 <- data.bind(lidar.120,dat)
dat15 <- data.bind(lidar.15,dat)
dat25 <- data.bind(lidar.25,dat)
dat250 <- data.bind(lidar.250,dat)
dat30 <- data.bind(lidar.30,dat)
dat50 <- data.bind(lidar.50,dat)
dat60 <- data.bind(lidar.60,dat)

#Change distance to a factor
dat10$dist <- as.factor(dat10$dist)
dat100$dist <- as.factor(dat100$dist)
dat120$dist <- as.factor(dat120$dist)
dat15$dist <- as.factor(dat15$dist)
dat25$dist <- as.factor(dat25$dist)
dat250$dist <- as.factor(dat250$dist)
dat30$dist <- as.factor(dat30$dist)
dat50$dist <- as.factor(dat50$dist)
dat60$dist <- as.factor(dat60$dist)

#Calculate the mean for each metric in the data frame
mean10 <- mean.calc(dat10)
mean100 <- mean.calc(dat100)
mean120 <- mean.calc(dat120)
mean15 <- mean.calc(dat15)
mean25 <- mean.calc(dat25)
mean250 <- mean.calc(dat250)
mean30 <- mean.calc(dat30)
mean50 <- mean.calc(dat50)
mean60 <- mean.calc(dat60)

numeric.dist <- function(x){
  #Function to change factors to numeric
  as.numeric(levels(x$Group.1))[x$Group.1]
}

#Change distance back to numeric
mean10$dist <- numeric.dist(mean10)
mean100$dist <- numeric.dist(mean100)
mean120$dist <- numeric.dist(mean120)
mean15$dist <- numeric.dist(mean15)
mean25$dist <- numeric.dist(mean25)
mean250$dist <- numeric.dist(mean250)
mean30$dist <- numeric.dist(mean30)
mean50$dist <- numeric.dist(mean50)
mean60$dist <- numeric.dist(mean60)

#Change mark the p-values as significant/non-significant; categorize the confidence envelopes as upper/lower
plot.dat10 <- mean10 %>%
  mutate(p.max = zmax.Pvalue < 0.05) %>% 
  mutate(p.mean = zmean.Pvalue < 0.05) %>% 
  mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
  mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
  mutate(p.openness = pground.Pvalue < 0.05) %>% 
  mutate(zmax.conf = as.factor(ifelse(zmax.Morans.i < zmax.Null.lcl, -1,ifelse(zmax.Morans.i > zmax.Null.ucl, 1, 0)))) %>% 
  mutate(zmean.conf = as.factor(ifelse(zmean.Morans.i < zmean.Null.lcl, -1,ifelse(zmean.Morans.i > zmean.Null.ucl, 1, 0)))) %>% 
  mutate(zsd.conf = as.factor(ifelse(zsd.Morans.i < zmsd.Null.lcl, -1,ifelse(zsd.Morans.i > zsd.Null.ucl, 1, 0)))) %>% 
  mutate(zentropy.conf = as.factor(ifelse(zentropy.Morans.i < zentropy.Null.lcl, -1,ifelse(zentropy.Morans.i > zentropy.Null.ucl, 1, 0)))) %>% 
  mutate(pground.conf = as.factor(ifelse(pground.Morans.i < pground.Null.lcl, -1,ifelse(pground.Morans.i > pground.Null.ucl, 1, 0)))) %>% 
  mutate(res = 10)

plot.dat100 <- mean100 %>%
  mutate(p.max = zmax.Pvalue < 0.05) %>% 
  mutate(p.mean = zmean.Pvalue < 0.05) %>% 
  mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
  mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
  mutate(p.openness = pground.Pvalue < 0.05) %>% 
  mutate(zmax.conf = as.factor(ifelse(zmax.Morans.i < zmax.Null.lcl, -1,ifelse(zmax.Morans.i > zmax.Null.ucl, 1, 0)))) %>% 
  mutate(zmean.conf = as.factor(ifelse(zmean.Morans.i < zmean.Null.lcl, -1,ifelse(zmean.Morans.i > zmean.Null.ucl, 1, 0)))) %>% 
  mutate(zsd.conf = as.factor(ifelse(zsd.Morans.i < zmsd.Null.lcl, -1,ifelse(zsd.Morans.i > zsd.Null.ucl, 1, 0)))) %>% 
  mutate(zentropy.conf = as.factor(ifelse(zentropy.Morans.i < zentropy.Null.lcl, -1,ifelse(zentropy.Morans.i > zentropy.Null.ucl, 1, 0)))) %>% 
  mutate(pground.conf = as.factor(ifelse(pground.Morans.i < pground.Null.lcl, -1,ifelse(pground.Morans.i > pground.Null.ucl, 1, 0)))) %>% 
  mutate(res = 100)

plot.dat25 <- mean25 %>%
  mutate(p.max = zmax.Pvalue < 0.05) %>% 
  mutate(p.mean = zmean.Pvalue < 0.05) %>% 
  mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
  mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
  mutate(p.openness = pground.Pvalue < 0.05) %>% 
  mutate(zmax.conf = as.factor(ifelse(zmax.Morans.i < zmax.Null.lcl, -1,ifelse(zmax.Morans.i > zmax.Null.ucl, 1, 0)))) %>% 
  mutate(zmean.conf = as.factor(ifelse(zmean.Morans.i < zmean.Null.lcl, -1,ifelse(zmean.Morans.i > zmean.Null.ucl, 1, 0)))) %>% 
  mutate(zsd.conf = as.factor(ifelse(zsd.Morans.i < zmsd.Null.lcl, -1,ifelse(zsd.Morans.i > zsd.Null.ucl, 1, 0)))) %>% 
  mutate(zentropy.conf = as.factor(ifelse(zentropy.Morans.i < zentropy.Null.lcl, -1,ifelse(zentropy.Morans.i > zentropy.Null.ucl, 1, 0)))) %>% 
  mutate(pground.conf = as.factor(ifelse(pground.Morans.i < pground.Null.lcl, -1,ifelse(pground.Morans.i > pground.Null.ucl, 1, 0)))) %>% 
  mutate(res = 25)

plot.dat250 <- mean250 %>%
  mutate(p.max = zmax.Pvalue < 0.05) %>% 
  mutate(p.mean = zmean.Pvalue < 0.05) %>% 
  mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
  mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
  mutate(p.openness = pground.Pvalue < 0.05) %>% 
  mutate(zmax.conf = as.factor(ifelse(zmax.Morans.i < zmax.Null.lcl, -1,ifelse(zmax.Morans.i > zmax.Null.ucl, 1, 0)))) %>% 
  mutate(zmean.conf = as.factor(ifelse(zmean.Morans.i < zmean.Null.lcl, -1,ifelse(zmean.Morans.i > zmean.Null.ucl, 1, 0)))) %>% 
  mutate(zsd.conf = as.factor(ifelse(zsd.Morans.i < zmsd.Null.lcl, -1,ifelse(zsd.Morans.i > zsd.Null.ucl, 1, 0)))) %>% 
  mutate(zentropy.conf = as.factor(ifelse(zentropy.Morans.i < zentropy.Null.lcl, -1,ifelse(zentropy.Morans.i > zentropy.Null.ucl, 1, 0)))) %>% 
  mutate(pground.conf = as.factor(ifelse(pground.Morans.i < pground.Null.lcl, -1,ifelse(pground.Morans.i > pground.Null.ucl, 1, 0)))) %>% 
  mutate(res = 250)

plot.dat50 <- mean50 %>%
  mutate(p.max = zmax.Pvalue < 0.05) %>% 
  mutate(p.mean = zmean.Pvalue < 0.05) %>% 
  mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
  mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
  mutate(p.openness = pground.Pvalue < 0.05) %>% 
  mutate(zmax.conf = as.factor(ifelse(zmax.Morans.i < zmax.Null.lcl, -1,ifelse(zmax.Morans.i > zmax.Null.ucl, 1, 0)))) %>% 
  mutate(zmean.conf = as.factor(ifelse(zmean.Morans.i < zmean.Null.lcl, -1,ifelse(zmean.Morans.i > zmean.Null.ucl, 1, 0)))) %>% 
  mutate(zsd.conf = as.factor(ifelse(zsd.Morans.i < zmsd.Null.lcl, -1,ifelse(zsd.Morans.i > zsd.Null.ucl, 1, 0)))) %>% 
  mutate(zentropy.conf = as.factor(ifelse(zentropy.Morans.i < zentropy.Null.lcl, -1,ifelse(zentropy.Morans.i > zentropy.Null.ucl, 1, 0)))) %>% 
  mutate(pground.conf = as.factor(ifelse(pground.Morans.i < pground.Null.lcl, -1,ifelse(pground.Morans.i > pground.Null.ucl, 1, 0)))) %>% 
  mutate(res = 50)

#Keep the data that's going to be plotted
dat.plot.sm <- rbind(plot.dat10.odd,plot.dat100,plot.dat25,plot.dat250,plot.dat50)

#Change resolution into a factor
dat.plot.sm$res <- as.factor(dat.plot.sm$res)

#Create a theme for ggplot
theme_moran <- function (base_size = 12, base_family = "") {
  #theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
  theme(
    ##axis properties
    axis.text.x = element_text(colour = "black", size = 14),
    axis.text.y = element_text(colour = "black", size = 14),
    axis.title.x = element_text(colour = "black", size = 18),
    axis.title.y = element_text(colour = "black", angle=90, size = 18),
    
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

#Plot the Moran's I lines
moran.maxh.env<-ggplot(dat.plot.sm, aes(x=dist, y=zmax.Morans.i, colour=res)) +
  geom_line(aes(group=res), size = 1) +
  geom_point(aes(shape = zmax.conf), size = 3) +
  geom_hline(yintercept = 0) +
  theme_moran()+
  scale_shape_manual(values = c(1,NA,16), labels = c("Outside lower confidence envelope","", "Outside upper confidence envelope"), name = "Significance")+
  scale_color_manual(values = c("#f5e626ff", "#21918dff", "#3b518bff", "#8c8c8c", "#000000"), name = "Grid cell length (m)")+
  theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(x = "Lag distance (m)", y = "Maximum canopy height")+
  annotate(geom="text",x=1000, y=0.63,label="Maximum canopy height", size=5) +
  ylim(-0.25,0.8)

moran.meanh.env<-ggplot(dat.plot.sm, aes(x=dist, y=zmean.Morans.i, colour=res)) +
  geom_line(aes(group=res), size = 1) +
  geom_point(aes(shape = zmean.conf), size = 3) +
  geom_hline(yintercept = 0) +
  theme_moran()+
  scale_shape_manual(values = c(1,NA,16), labels = c(">0.05", "<0.05"), name = "p-value")+
  scale_color_manual(values = c("#f5e626ff", "#21918dff", "#3b518bff", "#8c8c8c", "#000000"), name = "Grid cell length (m)")+
  theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(x = "Lag distance (m)", y = "Mean canopy height")+
  annotate(geom="text",x=1000, y=0.6,label="Mean canopy height", size=5)+
  ylim(-0.25,0.8)

moran.rug.env<-ggplot(dat.plot.sm, aes(x=dist, y=zsd.Morans.i, colour=res)) +
  geom_line(aes(group=res), size = 1) +
  geom_point(aes(shape = zsd.conf), size = 3) +
  geom_hline(yintercept = 0) +
  theme_moran()+
  scale_shape_manual(values = c(1,NA,16), labels = c(">0.05", "<0.05"), name = "p-value")+
  scale_color_manual(values = c("#f5e626ff", "#21918dff", "#3b518bff", "#8c8c8c", "#000000"), name = "Grid cell length (m)")+
  theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(x = "Lag distance (m)", y = "Mean canopy height")+
  annotate(geom="text",x=1000, y=0.54,label="Rugosity", size=5)+
  ylim(-0.25,0.8)

moran.vertdiv.env<-ggplot(dat.plot.sm, aes(x=dist, y=zentropy.Morans.i, colour=res)) +
  geom_line(aes(group=res), size = 1) +
  geom_point(aes(shape = zentropy.conf), size = 3) +
  geom_hline(yintercept = 0) +
  theme_moran()+
  scale_shape_manual(values = c(1,NA,16), labels = c(">0.05", "<0.05"), name = "p-value")+
  scale_color_manual(values = c("#f5e626ff", "#21918dff", "#3b518bff", "#8c8c8c", "#000000"), name = "Grid cell length (m)")+
  theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(x = "Lag distance (m)", y = "Mean canopy height")+
  annotate(geom="text",x=1000, y=0.4,label="Vertical diversity", size=5)+
  ylim(-0.25,0.8)

moran.open.env<-ggplot(dat.plot.sm, aes(x=dist, y=pground.Morans.i, colour=res)) +
  geom_line(aes(group=res), size = 1) +
  geom_point(aes(shape = pground.conf), size = 3) +
  geom_hline(yintercept = 0) +
  theme_moran()+
  scale_shape_manual(values = c(1,NA,16), labels = c(">0.05", "<0.05"), name = "p-value")+
  scale_color_manual(values = c("#f5e626ff", "#21918dff", "#3b518bff", "#8c8c8c", "#000000"), name = "Grid cell length (m)")+
  theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(x = "Lag distance (m)", y = "Mean canopy height")+
  annotate(geom="text",x=1000, y=0.58,label="Canopy openness", size=5)+
  ylim(-0.25,0.8)

#Execute the plots and write to disk
moran.maxh.env
ggsave("./Images/moran_maxh.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

moran.meanh.env
ggsave("./Images/moran_meanh.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

moran.rug.env
ggsave("./Images/moran_rug.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

moran.vertdiv.env
ggsave("./Images/moran_vertdiv.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

moran.open.env
ggsave("./Images/moran_open.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)