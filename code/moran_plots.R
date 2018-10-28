library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(cowplot)

# Remove all from environment
rm(list = ls())

# set data paths
in.path = "D:/Dropbox/Dropbox/lidar-scaling/Data/summary_data/"

# list all of the data to be used
file.names <- dir(in.path, pattern = "morans*", full.names = TRUE)

splits <- unlist(strsplit(file.names,"[.]"))

splits <- splits[grepl("['/Data*]",splits)]
splits.num <- unlist(strsplit(splits,"[_]"))
splits.num <- splits.num[!grepl("['/D*]",splits.num)]
splits.num <- splits.num[!grepl("[su*]",splits.num)]
splits.res <- splits.num[c(FALSE,TRUE)]
splits.tiles <- splits.num[c(T,F)]

lidar.dat.structure <- cbind(file.names,splits.tiles,splits.res)
lidar.dat.structure <- as.data.frame(lidar.dat.structure)
lidar.dat.structure$splits.tiles <- as.numeric(lidar.dat.structure$splits.tiles)
lidar.dat.structure <- lidar.dat.structure %>% filter(splits.res != "5")
lidar.dat.structure$topo <- NA
lidar.dat.structure$topo[1:135] <- "HIGH"
lidar.dat.structure$topo[136:270] <- "LOW"
lidar.dat.structure$topo[271:405] <- "HIGH"
lidar.dat.structure$topo[406:540] <- "LOW"

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

morans.mat <- matrix(ncol = 22, nrow = 9)

dat <- data.frame(X = numeric(), dist = numeric(), zmax.Morans.i = numeric(), zmax.Null.lcl = numeric(), zmax.null.ucl = numeric(), zmax.Pvalue = numeric(),
                  zmean.Morans.i = numeric(), zmean.Null.lcl = numeric(), zmean.Null.ucl = numeric(), zmean.Pvalue = numeric(), zsd.Morans.i = numeric(),
                  zmsd.Null.lcl = numeric(), zsd.Null.ucl = numeric(), zsd.Pvalue = numeric(), zentropy.Morans.i = numeric(), zentropy.Null.lcl = numeric(),
                  zentropy.Null.ucl = numeric(), zentropy.Pvalue = numeric(), pground.Morans.i = numeric(), prground.Null.lcl = numeric(), pground.Null.ucl = numeric(),
                  pground.Pvalue = numeric())

data.bind <- function(x,y){
  for(i in 1:nrow(x)){
  new.dat <- read.csv(x$file.names[i])
  y <- rbind(y,new.dat)
  }
  y
}

dat10 <- data.bind(lidar.10,dat)
dat100 <- data.bind(lidar.100,dat)
dat120 <- data.bind(lidar.120,dat)
dat15 <- data.bind(lidar.15,dat)
dat25 <- data.bind(lidar.25,dat)
dat250 <- data.bind(lidar.250,dat)
dat30 <- data.bind(lidar.30,dat)
dat50 <- data.bind(lidar.50,dat)
dat60 <- data.bind(lidar.60,dat)

dat10$dist <- as.factor(dat10$dist)
dat100$dist <- as.factor(dat100$dist)
dat120$dist <- as.factor(dat120$dist)
dat15$dist <- as.factor(dat15$dist)
dat25$dist <- as.factor(dat25$dist)
dat250$dist <- as.factor(dat250$dist)
dat30$dist <- as.factor(dat30$dist)
dat50$dist <- as.factor(dat50$dist)
dat60$dist <- as.factor(dat60$dist)

mean.calc <- function(x){
  aggregate(x[,3:22],list(x$dist),mean)
}

mean10 <- mean.calc(dat10)
mean100 <- mean.calc(dat100)
mean120 <- mean.calc(dat120)
mean15 <- mean.calc(dat15)
mean25 <- mean.calc(dat25)
mean250 <- mean.calc(dat250)
mean30 <- mean.calc(dat30)
mean50 <- mean.calc(dat50)
mean60 <- mean.calc(dat60)

mean10$dist <- as.numeric(levels(mean10$Group.1))[mean10$Group.1]

numeric.dist <- function(x){
  as.numeric(levels(x$Group.1))[x$Group.1]
}

mean10$dist <- numeric.dist(mean10)
mean100$dist <- numeric.dist(mean100)
mean120$dist <- numeric.dist(mean120)
mean15$dist <- numeric.dist(mean15)
mean25$dist <- numeric.dist(mean25)
mean250$dist <- numeric.dist(mean250)
mean30$dist <- numeric.dist(mean30)
mean50$dist <- numeric.dist(mean50)
mean60$dist <- numeric.dist(mean60)

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

plot.dat10.odd <- plot.dat10[seq(1,nrow(plot.dat10),2),]

# plot.dat10 <- mean10 %>%
#   #filter(dist < 1000*sqrt(2)) %>% 
#   select(Lag.Distance = dist, Max.Height.Morans.I = zmax.Morans.i, Mean.Height.Morans.I = zmean.Morans.i, Rugosity.Morans.I = zsd.Morans.i, 
#          Vertical.Diversity.Morans.I = zentropy.Morans.i, Openness.Morans.I = pground.Morans.i, zmax.Pvalue, zmean.Pvalue, zsd.Pvalue, 
#          zentropy.Pvalue, pground.Pvalue) %>% 
#   mutate(p.max = zmax.Pvalue < 0.05) %>% 
#   mutate(p.mean = zmean.Pvalue < 0.05) %>% 
#   mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
#   mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
#   mutate(p.openness = pground.Pvalue < 0.05) %>% 
#   mutate(res = 10)
# 
# plot.dat10.odd <- plot.dat10[seq(1,nrow(plot.dat10),2),]
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

# plot.dat100 <- mean100 %>%
#   #filter(dist < 1000*sqrt(2)) %>% 
#   select(Lag.Distance = dist, Max.Height.Morans.I = zmax.Morans.i, Mean.Height.Morans.I = zmean.Morans.i, Rugosity.Morans.I = zsd.Morans.i, 
#          Vertical.Diversity.Morans.I = zentropy.Morans.i, Openness.Morans.I = pground.Morans.i, zmax.Pvalue, zmean.Pvalue, zsd.Pvalue, 
#          zentropy.Pvalue, pground.Pvalue) %>% 
#   mutate(p.max = zmax.Pvalue < 0.05) %>% 
#   mutate(p.mean = zmean.Pvalue < 0.05) %>% 
#   mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
#   mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
#   mutate(p.openness = pground.Pvalue < 0.05) %>% 
#   mutate(res = 100)

# plot.dat120 <- mean120 %>%
#   #filter(dist < 1000*sqrt(2)) %>% 
#   select(Lag.Distance = dist, Max.Height.Morans.I = zmax.Morans.i, Mean.Height.Morans.I = zmean.Morans.i, Rugosity.Morans.I = zsd.Morans.i, 
#          Vertical.Diversity.Morans.I = zentropy.Morans.i, Openness.Morans.I = pground.Morans.i, zmax.Pvalue, zmean.Pvalue, zsd.Pvalue, 
#          zentropy.Pvalue, pground.Pvalue) %>% 
#   mutate(p.max = zmax.Pvalue < 0.05) %>% 
#   mutate(p.mean = zmean.Pvalue < 0.05) %>% 
#   mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
#   mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
#   mutate(p.openness = pground.Pvalue < 0.05) %>% 
#   mutate(res = 120)

# plot.dat15 <- mean15 %>%
#   #filter(dist < 1000*sqrt(2)) %>% 
#   select(Lag.Distance = dist, Max.Height.Morans.I = zmax.Morans.i, Mean.Height.Morans.I = zmean.Morans.i, Rugosity.Morans.I = zsd.Morans.i, 
#          Vertical.Diversity.Morans.I = zentropy.Morans.i, Openness.Morans.I = pground.Morans.i, zmax.Pvalue, zmean.Pvalue, zsd.Pvalue, 
#          zentropy.Pvalue, pground.Pvalue) %>% 
#   mutate(p.max = zmax.Pvalue < 0.05) %>% 
#   mutate(p.mean = zmean.Pvalue < 0.05) %>% 
#   mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
#   mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
#   mutate(p.openness = pground.Pvalue < 0.05) %>% 
#   mutate(res = 15)

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

# plot.dat25 <- mean25 %>%
#   #filter(dist < 1000*sqrt(2)) %>% 
#   select(Lag.Distance = dist, Max.Height.Morans.I = zmax.Morans.i, Mean.Height.Morans.I = zmean.Morans.i, Rugosity.Morans.I = zsd.Morans.i, 
#          Vertical.Diversity.Morans.I = zentropy.Morans.i, Openness.Morans.I = pground.Morans.i, zmax.Pvalue, zmean.Pvalue, zsd.Pvalue, 
#          zentropy.Pvalue, pground.Pvalue) %>% 
#   mutate(p.max = zmax.Pvalue < 0.05) %>% 
#   mutate(p.mean = zmean.Pvalue < 0.05) %>% 
#   mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
#   mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
#   mutate(p.openness = pground.Pvalue < 0.05) %>% 
#   mutate(res = 25)

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

# plot.dat30 <- mean30 %>%
#   #filter(dist < 1000*sqrt(2)) %>% 
#   select(Lag.Distance = dist, Max.Height.Morans.I = zmax.Morans.i, Mean.Height.Morans.I = zmean.Morans.i, Rugosity.Morans.I = zsd.Morans.i, 
#          Vertical.Diversity.Morans.I = zentropy.Morans.i, Openness.Morans.I = pground.Morans.i, zmax.Pvalue, zmean.Pvalue, zsd.Pvalue, 
#          zentropy.Pvalue, pground.Pvalue) %>% 
#   mutate(p.max = zmax.Pvalue < 0.05) %>% 
#   mutate(p.mean = zmean.Pvalue < 0.05) %>% 
#   mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
#   mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
#   mutate(p.openness = pground.Pvalue < 0.05) %>% 
#   mutate(res = 30)

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

# plot.dat50 <- mean50 %>%
#   #filter(dist < 1000*sqrt(2)) %>% 
#   select(Lag.Distance = dist, Max.Height.Morans.I = zmax.Morans.i, Mean.Height.Morans.I = zmean.Morans.i, Rugosity.Morans.I = zsd.Morans.i, 
#          Vertical.Diversity.Morans.I = zentropy.Morans.i, Openness.Morans.I = pground.Morans.i, zmax.Pvalue, zmean.Pvalue, zsd.Pvalue, 
#          zentropy.Pvalue, pground.Pvalue) %>% 
#   mutate(p.max = zmax.Pvalue < 0.05) %>% 
#   mutate(p.mean = zmean.Pvalue < 0.05) %>% 
#   mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
#   mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
#   mutate(p.openness = pground.Pvalue < 0.05) %>% 
#   mutate(res = 50)
# 
# plot.dat60 <- mean60 %>%
#   #filter(dist < 1000*sqrt(2)) %>% 
#   select(Lag.Distance = dist, Max.Height.Morans.I = zmax.Morans.i, Mean.Height.Morans.I = zmean.Morans.i, Rugosity.Morans.I = zsd.Morans.i, 
#          Vertical.Diversity.Morans.I = zentropy.Morans.i, Openness.Morans.I = pground.Morans.i, zmax.Pvalue, zmean.Pvalue, zsd.Pvalue, 
#          zentropy.Pvalue, pground.Pvalue) %>% 
#   mutate(p.max = zmax.Pvalue < 0.05) %>% 
#   mutate(p.mean = zmean.Pvalue < 0.05) %>% 
#   mutate(p.rugosity = zsd.Pvalue < 0.05) %>% 
#   mutate(p.vertical.diversity = zentropy.Pvalue < 0.05) %>% 
#   mutate(p.openness = pground.Pvalue < 0.05) %>% 
#   mutate(res = 60)

#plot.dat120.zmax <- mean120 %>%
#   select(dist, Moran = zmax.Morans.i, zmax.Null.lcl, zmax.Null.ucl, p = zmax.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmax.Null.lcl, 1,
#                                  ifelse(Moran > zmax.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 120)
# plot.dat15.zmax <- mean15 %>%
#   select(dist, Moran = zmax.Morans.i, zmax.Null.lcl, zmax.Null.ucl, p = zmax.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmax.Null.lcl, 1,
#                                  ifelse(Moran > zmax.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 15)
# plot.dat25.zmax <- mean25 %>%
#   select(dist, Moran = zmax.Morans.i, zmax.Null.lcl, zmax.Null.ucl, p = zmax.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmax.Null.lcl, 1,
#                                  ifelse(Moran > zmax.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 25)
# plot.dat120.zmax <- mean120 %>%
#   select(dist, Moran = zmax.Morans.i, zmax.Null.lcl, zmax.Null.ucl, p = zmax.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmax.Null.lcl, 1,
#                                  ifelse(Moran > zmax.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 120)
# plot.dat250.zmax <- mean250 %>%
#   select(dist, Moran = zmax.Morans.i, zmax.Null.lcl, zmax.Null.ucl, p = zmax.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmax.Null.lcl, 1,
#                                  ifelse(Moran > zmax.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 250)
# plot.dat30.zmax <- mean30 %>%
#   select(dist, Moran = zmax.Morans.i, zmax.Null.lcl, zmax.Null.ucl, p = zmax.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmax.Null.lcl, 1,
#                                  ifelse(Moran > zmax.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 30)
# plot.dat50.zmax <- mean50 %>%
#   select(dist, Moran = zmax.Morans.i, zmax.Null.lcl, zmax.Null.ucl, p = zmax.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmax.Null.lcl, 1,
#                                  ifelse(Moran > zmax.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 50)
# plot.dat60.zmax <- mean60 %>%
#   select(dist, Moran = zmax.Morans.i, zmax.Null.lcl, zmax.Null.ucl, p = zmax.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmax.Null.lcl, 1,
#                                  ifelse(Moran > zmax.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 60)
# 
# 
# ##########################
# 
# plot.dat100.zmean <- mean100 %>%
#   select(dist, Moran = zmean.Morans.i, zmean.Null.lcl, zmean.Null.ucl, p = zmean.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmean.Null.lcl, 1,
#                                  ifelse(Moran > zmean.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 100)
# plot.dat120.zmean <- mean120 %>%
#   select(dist, Moran = zmean.Morans.i, zmean.Null.lcl, zmean.Null.ucl, p = zmean.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmean.Null.lcl, 1,
#                                  ifelse(Moran > zmean.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 120)
# plot.dat15.zmean <- mean15 %>%
#   select(dist, Moran = zmean.Morans.i, zmean.Null.lcl, zmean.Null.ucl, p = zmean.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmean.Null.lcl, 1,
#                                  ifelse(Moran > zmean.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 15)
# plot.dat25.zmean <- mean25 %>%
#   select(dist, Moran = zmean.Morans.i, zmean.Null.lcl, zmean.Null.ucl, p = zmean.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmean.Null.lcl, 1,
#                                  ifelse(Moran > zmean.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 25)
# plot.dat120.zmean <- mean120 %>%
#   select(dist, Moran = zmean.Morans.i, zmean.Null.lcl, zmean.Null.ucl, p = zmean.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmean.Null.lcl, 1,
#                                  ifelse(Moran > zmean.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 120)
# plot.dat250.zmean <- mean250 %>%
#   select(dist, Moran = zmean.Morans.i, zmean.Null.lcl, zmean.Null.ucl, p = zmean.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmean.Null.lcl, 1,
#                                  ifelse(Moran > zmean.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 250)
# plot.dat30.zmean <- mean30 %>%
#   select(dist, Moran = zmean.Morans.i, zmean.Null.lcl, zmean.Null.ucl, p = zmean.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmean.Null.lcl, 1,
#                                  ifelse(Moran > zmean.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 30)
# plot.dat50.zmean <- mean50 %>%
#   select(dist, Moran = zmean.Morans.i, zmean.Null.lcl, zmean.Null.ucl, p = zmean.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmean.Null.lcl, 1,
#                                  ifelse(Moran > zmean.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 50)
# plot.dat60.zmean <- mean60 %>%
#   select(dist, Moran = zmean.Morans.i, zmean.Null.lcl, zmean.Null.ucl, p = zmean.Pvalue) %>% 
#   mutate(conf = as.factor(ifelse(Moran < zmean.Null.lcl, 1,
#                                  ifelse(Moran > zmean.Null.lcl, 2, 3)))) %>% 
#   mutate(p = p < 0.05) %>% 
#   mutate(res = 60)
# 
# 
# 
# ggplot(plot.dat1,aes(x=dist, y=Moran)) +
#   geom_point(aes(shape = conf, colour = p), size = 4) +
#   geom_line(size = 1) +
#   geom_hline(yintercept = 0) +
#   scale_shape_manual(values=c(0,2,16))
# 
# dat1 <- mean60 %>%
#   select(dist, Moran = zmax.Morans.i, p = zmax.Pvalue) %>%
#   mutate(p = p < 0.05)
# dat1
# 
# ggplot(dat1, aes(x=dist, y=Moran)) +
#   geom_line(size=1) +
#   geom_point(aes(shape = p), size=3) +
#   geom_hline(yintercept = 0) +
#   scale_shape_manual(values = c(1, 16))
# 
# dat2 <- mean50 %>% 
#   select(dist,Moran = zmax.Morans.i, p = zmax.Pvalue) %>% 
#   mutate(p = p < 0.05)
# dat2$dist <- dat2$dist + 100
# 
# dat1$res <- 1
# dat2$res <- 2

dat.all <- bind_rows(plot.dat10.odd,plot.dat100, plot.dat120, plot.dat15, plot.dat25,plot.dat250, plot.dat30, plot.dat50, plot.dat60)

dat.plot.sm <- rbind(plot.dat10.odd,plot.dat100,plot.dat25,plot.dat250,plot.dat50)

dat.plot.sm$res <- as.factor(dat.plot.sm$res)

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", "#CC79A7", "#F0E442", "#b66dff")
cbbPalette.small <- c("#000000","#009E73","#e79f00","#0072B2","#b66dff")

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

# moran.maxh<-ggplot(dat.small, aes(x=Lag.Distance, y=Max.Height.Morans.I, colour=res)) +
#   geom_line(aes(group=res)) +
#   geom_point(aes(shape = p.max), size = 3) +
#   geom_hline(yintercept = 0) +
#   theme_moran()+
#   scale_shape_manual(values = c(1, 16), labels = c(">0.05", "<0.05"), name = "p-value")+
#   scale_color_manual(values=cbbPalette.small, name = "Resolution (m)")+
#   theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
#   labs(x = "Lag distance (m)", y = "Maximum canopy height")+
#   annotate(geom="text",x=1000, y=0.63,label="Maximum canopy height", size=5)

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

# moran.meanh<-ggplot(dat.small, aes(x=Lag.Distance, y=Mean.Height.Morans.I, colour=res)) +
#   geom_line(aes(group=res)) +
#   geom_point(aes(shape = p.mean), size = 3) +
#   geom_hline(yintercept = 0) +
#   theme_moran() +
#   scale_shape_manual(values = c(1, 16)) +
#   scale_color_manual(values=cbbPalette.small, name = "Resolution")+
#   theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
#   labs(x = "Lag distance (m)", y = "Mean canopy height") +
#   annotate(geom="text",x=1000, y=0.6,label="Mean canopy height", size=5)

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

# moran.rug<-ggplot(dat.small, aes(x=Lag.Distance, y=Rugosity.Morans.I, colour=res)) +
#   geom_line(aes(group=res)) +
#   geom_point(aes(shape = p.rugosity), size = 3) +
#   geom_hline(yintercept = 0) +
#   theme_moran() +
#   scale_color_manual(values=cbbPalette.small, name = "Resolution")+
#   theme(legend.position="none", axis.title.x=element_blank(), axis.title.y = element_blank())+
#   scale_shape_manual(values = c(1, 16))+
#   labs(x = "Lag distance (m)", y = "Rugosity") +
#   annotate(geom="text",x=1000, y=0.54,label="Rugosity", size=5)

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

# moran.vert.div <- ggplot(dat.small, aes(x=Lag.Distance, y=Vertical.Diversity.Morans.I, colour=res)) +
#   geom_line(aes(group=res)) +
#   geom_point(aes(shape = p.vertical.diversity), size = 3) +
#   geom_hline(yintercept = 0) +
#   theme_moran() +
#   scale_shape_manual(values = c(1, 16)) +
#   scale_color_manual(values=cbbPalette.small, name = "Resolution")+
#   theme(legend.position="none", axis.title.x = element_blank(),axis.title.y=element_blank())+
#   labs(x = "Lag distance (m)", y = "Vertical diversity") +
#   annotate(geom = "text",x=1000, y=0.4, label = "Vertical diversity", size = 5)

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

# moran.open <- ggplot(dat.small, aes(x=Lag.Distance, y=Openness.Morans.I, colour=res)) +
#   geom_line(aes(group=res)) +
#   geom_point(aes(shape = p.openness), size = 3) +
#   geom_hline(yintercept = 0) +
#   theme_moran()+
#   scale_shape_manual(values = c(1, 16), labels = c("p > 0.05", "p < 0.05"), name = "") +
#   scale_color_manual(values=cbbPalette.small, name = "Resolution")+
#   theme(legend.position = "none",axis.title.x = element_blank(),axis.title.y=element_blank())+
#   labs(x = "Lag distance (m)", y = "Canopy openness") +
#   annotate(geom="text", x=1000, y = 0.55, label = "Canopy openness", size=5)

moran.maxh.env
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/moran_maxh.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

moran.meanh.env
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/moran_meanh.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

moran.rug.env
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/moran_rug.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

moran.vertdiv.env
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/moran_vertdiv.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

moran.open.env
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/moran_open.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)


moran.plots <- ggarrange(moran.maxh.env, moran.meanh.env,moran.rug.env,moran.vertdiv.env,moran.open.env, labels = c("A","B","C","D","E"), 
          label.x = c(0.7,0.7,0.7,0.7,0.7), label.y = c(0.95,0.95,0.95,0.95,0.95),font.label = list(size = 24),ncol = 3, 
          nrow = 2, common.legend = T, legend = "right", align = "h")
annotate_figure(moran.plots,bottom = text_grob("Lag distance (m)", size = 24), left = text_grob("Moran's I", size = 24, rot = 90))

ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/morans.svg", plot = last_plot(), units = "in", width = 30, height = 15, dpi = 600)
# dat.all.mean <- bind_rows(plot.dat100.zmean, plot.dat120.zmean, plot.dat15.zmean, plot.dat25.zmean, plot.dat250.zmean, plot.dat30.zmean, plot.dat50.zmean, plot.dat60.zmean)
# 
# plot(y=correlog.sp$Morans.i,x=correlog.sp$dist, xlab="Lag Distance(m)", ylab="Moran's I", ylim=c(-0.3,0.3)) #ylim provides limit on y-axis between -1 and 1
# abline(h=0)#0 reference
# lines(correlog.sp$dist, correlog.sp$Null.lcl,col = "red")	#add the null lcl to the plot
# lines(correlog.sp$dist, correlog.sp$Null.ucl,col = "red")	#add the null ucl to the plot

x<- c(10,100,1000,120,15,25,250,30,5,50,500,60)
y<-c(0.844260386,0.810354397,0.717166819,0.810976732,0.856148052,0.85564255,0.799213295,0.852621885,0.780498492,0.838694368,0.769891651,0.838437896)
xy<-as.data.frame(cbind(x,y))
