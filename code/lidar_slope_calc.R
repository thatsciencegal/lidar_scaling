##Code to calculate all of the slope-related metrics 
##Written by Christine Swanson
##February 20, 2019

library(raster)
library(spdep)
library(rgdal)
library(tidyverse)

#Import the DEMs
dem_n <- raster("H:/ESRI_DEMs/ground/BE_north.flt")
dem_s <- raster("H:/ESRI_DEMs/ground/BE_South1.flt")

#Mosaic the two dems to make one DEM for the whole area
dem_mosaic <- mosaic(dem_n,dem_s,fun="mean")

#Resample the DEM to the different grid sizes
dem_10<-aggregate(dem_mosaic,10)
dem_25<-aggregate(dem_mosaic,25)
dem_50<-aggregate(dem_mosaic,50)
dem_100<-aggregate(dem_mosaic,100)
dem_250<-aggregate(dem_mosaic,250)
dem_500<-aggregate(dem_mosaic,500)

#Calculate the slope across the different grid sizes
slope_10<-terrain(dem_10,opt="slope",neighbors=8)
slope_25<-terrain(dem_25,opt="slope",neighbors=8)
slope_50<-terrain(dem_50,opt="slope",neighbors=8)
slope_100<-terrain(dem_100,opt="slope",neighbors=8)
slope_250<-terrain(dem_250,opt="slope",neighbors=8)
slope_500<-terrain(dem_500,opt="slope",neighbors=8)

#Read in the shapefile containing the 60 lidar tiles
cells<-readOGR(dsn="C:/Users/starg/Dropbox/lidar-scaling/Data/gis", layer="lidar_polys")

#Extract the slope information for the different lidar tiles
out_slope_10<-mask(slope_10,cells)
out_slope_25<-mask(slope_25,cells)
out_slope_50<-mask(slope_50,cells)
out_slope_100<-mask(slope_100,cells)
out_slope_250<-mask(slope_250,cells)
out_slope_500<-mask(slope_500,cells)

#Create lists of all of the quantified lidar metrics for each resolution
in.path<-"./Data/output/filtered_output"
list_10<-dir(in.path,pattern=".size_10.csv",full.names = T)
list_25<-dir(in.path,pattern=".size_25.csv",full.names = T)
list_50<-dir(in.path,pattern=".size_50.csv",full.names = T)
list_100<-dir(in.path,pattern=".size_100.csv",full.names = T)
list_250<-dir(in.path,pattern=".size_250.csv",full.names = T)
list_500<-dir(in.path,pattern=".size_500.csv",full.names = T)

#Read in the quantified lidar metrics and combine all of the csv's for the same resolution
dat.10<-lapply(1:60,function(i) read.csv(list_10[i]))
dat.10.unl<-dat.10 %>% bind_rows(.id="list_name")
dat.25<-lapply(1:60,function(i) read.csv(list_25[i]))
dat.25.unl<-dat.25 %>% bind_rows(.id="list_name")
dat.50<-lapply(1:60,function(i) read.csv(list_50[i]))
dat.50.unl<-dat.50 %>% bind_rows(.id="list_name")
dat.100<-lapply(1:60,function(i) read.csv(list_100[i]))
dat.100.unl<-dat.100 %>% bind_rows(.id="list_name")
dat.250<-lapply(1:60,function(i) read.csv(list_250[i]))
dat.250.unl<-dat.250 %>% bind_rows(.id="list_name")
dat.500<-lapply(1:59,function(i) read.csv(list_500[i]))
dat.500.unl<-dat.500 %>% bind_rows(.id="list_name")

#Add a column for resolution 
dat.10.unl$Res<-10
dat.25.unl$Res<-25
dat.50.unl$Res<-50
dat.100.unl$Res<-100
dat.250.unl$Res<-250
dat.500.unl$Res<-500

#Create a data frame of XY values for each pixel at each resolution
XY10<-dat.10.unl[,3:4]
XY25<-dat.25.unl[,3:4]
XY50<-dat.50.unl[,3:4]
XY100<-dat.100.unl[,3:4]
XY250<-dat.250.unl[,3:4]
XY500<-dat.500.unl[,3:4]

#Extrac the slope value for each pixel and add a column for slope to the data frame with the quantified lidar
dat.10.ext<-cbind(dat.10.unl,slope=raster::extract(out_slope_10,XY10))
dat.25.ext<-cbind(dat.25.unl,slope=raster::extract(out_slope_25,XY25))
dat.50.ext<-cbind(dat.50.unl,slope=raster::extract(out_slope_50,XY50))
dat.100.ext<-cbind(dat.100.unl,slope=raster::extract(out_slope_100,XY100))
dat.250.ext<-cbind(dat.250.unl,slope=raster::extract(out_slope_250,XY250))
dat.500.ext<-cbind(dat.500.unl,slope=raster::extract(out_slope_500,XY500))

#Combine all of the lidar tiles
dat.all <- rbind(dat.10.ext,dat.25.ext,dat.50.ext,dat.100.ext,dat.250.ext)

#Create custom theme for graphs
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

#Change the resolution from numeric to a factor
dat.all$Res<-as.factor(dat.all$Res)

#Plot maximum height vs slope
maxh.slope<-ggplot(dat.all,aes(x=slope, y=zmax, color=Res))+
  geom_smooth(method='lm',formula=y~x)+
  theme_moran()+
  #theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(x = "Slope", y = "Maximum canopy height (m)")+
  scale_x_continuous(expand=c(0,0), breaks=c(0.25,0.5,0.75,1.0,1.25))+
  scale_color_manual(values = c("#f5e626ff", "#21918dff", "#3b518bff", "#8c8c8c", "#000000", "#481567FF"), name = "Grid cell length (m)")

#Plot mean height vs slope
meanh.slope<-ggplot(dat.all,aes(x=slope, y=zmean, color=Res))+
  geom_smooth(method='lm',formula=y~x)+
  theme_moran()+
  #theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(x = "Slope", y = "Mean canopy height (m)")+
  scale_x_continuous(expand=c(0,0), breaks=c(0.25,0.5,0.75,1.0,1.25))+
  scale_color_manual(values = c("#f5e626ff", "#21918dff", "#3b518bff", "#8c8c8c", "#000000", "#481567FF"), name = "Grid cell length (m)")

#Plot rugosity vs slope
rugosity.slope<-ggplot(dat.all,aes(x=slope, y=zsd, color=Res))+
  geom_smooth(method='lm',formula=y~x)+
  theme_moran()+
  #theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(x = "Slope", y = "Rugosity (m)")+
  scale_x_continuous(expand=c(0,0), breaks=c(0.25,0.5,0.75,1.0,1.25))+
  scale_color_manual(values = c("#f5e626ff", "#21918dff", "#3b518bff", "#8c8c8c", "#000000", "#481567FF"), name = "Grid cell length (m)")

#Plot canopy openness vs slope
open.slope<-ggplot(dat.all,aes(x=slope, y=pground, color=Res))+
  geom_smooth(method='lm',formula=y~x)+
  theme_moran()+
  #theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(x = "Slope", y = "Canopy openness (%)")+
  scale_x_continuous(expand=c(0,0), breaks=c(0.25,0.5,0.75,1.0,1.25))+
  scale_color_manual(values = c("#f5e626ff", "#21918dff", "#3b518bff", "#8c8c8c", "#000000", "#481567FF"), name = "Grid cell length (m)")

#Plot vertical diversity vs slope
vdiv.slope<-ggplot(dat.all,aes(x=slope, y=zentropy, color=Res))+
  geom_smooth(method='lm',formula=y~x)+
  theme_moran()+
  #theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())+
  labs(x = "Slope", y = "Vertical diversity")+
  scale_x_continuous(expand=c(0,0), breaks=c(0.25,0.5,0.75,1.0,1.25))+
  scale_color_manual(values = c("#f5e626ff", "#21918dff", "#3b518bff", "#8c8c8c", "#000000", "#481567FF"), name = "Grid cell length (m)")

#Regression models for slope at different resolutions
summary(lm(dat.10.ext$zmax~dat.10.ext$slope))
summary(lm(dat.25.ext$zmax~dat.25.ext$slope))
summary(lm(dat.50.ext$zmax~dat.50.ext$slope))
summary(lm(dat.100.ext$zmax~dat.100.ext$slope))
summary(lm(dat.250.ext$zmax~dat.250.ext$slope))
summary(lm(dat.500.ext$zmax~dat.500.ext$slope))

summary(lm(dat.10.ext$zmean~dat.10.ext$slope))
summary(lm(dat.25.ext$zmean~dat.25.ext$slope))
summary(lm(dat.50.ext$zmean~dat.50.ext$slope))
summary(lm(dat.100.ext$zmean~dat.100.ext$slope))
summary(lm(dat.250.ext$zmean~dat.250.ext$slope))
summary(lm(dat.500.ext$zmean~dat.500.ext$slope))

summary(lm(dat.10.ext$zsd~dat.10.ext$slope))
summary(lm(dat.25.ext$zsd~dat.25.ext$slope))
summary(lm(dat.50.ext$zsd~dat.50.ext$slope))
summary(lm(dat.100.ext$zsd~dat.100.ext$slope))
summary(lm(dat.250.ext$zsd~dat.250.ext$slope))
summary(lm(dat.500.ext$zsd~dat.500.ext$slope))

summary(lm(dat.10.ext$pground~dat.10.ext$slope))
summary(lm(dat.25.ext$pground~dat.25.ext$slope))
summary(lm(dat.50.ext$pground~dat.50.ext$slope))
summary(lm(dat.100.ext$pground~dat.100.ext$slope))
summary(lm(dat.250.ext$pground~dat.250.ext$slope))
summary(lm(dat.500.ext$pground~dat.500.ext$slope))

summary(lm(dat.10.ext$zentropy~dat.10.ext$slope))
summary(lm(dat.25.ext$zentropy~dat.25.ext$slope))
summary(lm(dat.50.ext$zentropy~dat.50.ext$slope))
summary(lm(dat.100.ext$zentropy~dat.100.ext$slope))
summary(lm(dat.250.ext$zentropy~dat.250.ext$slope))
summary(lm(dat.500.ext$zentropy~dat.500.ext$slope))

#Combine resolutions pairwise for ancova models
dat.10.25 <- rbind(dat.10.ext,dat.25.ext)
dat.10.50 <- rbind(dat.10.ext,dat.50.ext)
dat.10.100 <- rbind(dat.10.ext,dat.100.ext)
dat.10.250 <- rbind(dat.10.ext,dat.250.ext)
dat.10.500 <- rbind(dat.10.ext,dat.500.ext)
dat.25.50 <- rbind(dat.25.ext,dat.50.ext)
dat.25.100 <- rbind(dat.25.ext,dat.100.ext)
dat.25.250 <- rbind(dat.25.ext,dat.250.ext)
dat.25.500 <- rbind(dat.25.ext,dat.500.ext)
dat.50.100 <- rbind(dat.50.ext,dat.100.ext)
dat.50.250 <- rbind(dat.50.ext,dat.250.ext)
dat.50.500 <- rbind(dat.50.ext,dat.500.ext)
dat.100.250 <- rbind(dat.100.ext,dat.250.ext)
dat.100.500 <- rbind(dat.100.ext,dat.500.ext)
dat.250.500 <- rbind(dat.250.ext,dat.500.ext)

#Function to return results of ancova model
do.aov<-function(col1,col2="slope",col3="Res",dat){
  summary(aov(dat[[col1]]~dat[[col2]]+dat[[col3]]))
}

#Ancova for each pairwise comparison 
do.aov(col1="zmax",dat=dat.10.25)
do.aov(col1="zmax",dat=dat.10.50)
do.aov(col1="zmax",dat=dat.10.100)
do.aov(col1="zmax",dat=dat.10.250)
do.aov(col1="zmax",dat=dat.10.500)
do.aov(col1="zmax",dat=dat.25.50)
do.aov(col1="zmax",dat=dat.25.100)
do.aov(col1="zmax",dat=dat.25.250)
do.aov(col1="zmax",dat=dat.25.500)
do.aov(col1="zmax",dat=dat.50.100)
do.aov(col1="zmax",dat=dat.50.250)
do.aov(col1="zmax",dat=dat.50.500)
do.aov(col1="zmax",dat=dat.100.250)
do.aov(col1="zmax",dat=dat.100.500)
do.aov(col1="zmax",dat=dat.250.500)

do.aov(col1="zmean",dat=dat.10.25)
do.aov(col1="zmean",dat=dat.10.50)
do.aov(col1="zmean",dat=dat.10.100)
do.aov(col1="zmean",dat=dat.10.250)
do.aov(col1="zmean",dat=dat.10.500)
do.aov(col1="zmean",dat=dat.25.50)
do.aov(col1="zmean",dat=dat.25.100)
do.aov(col1="zmean",dat=dat.25.250)
do.aov(col1="zmean",dat=dat.25.500)
do.aov(col1="zmean",dat=dat.50.100)
do.aov(col1="zmean",dat=dat.50.250)
do.aov(col1="zmean",dat=dat.50.500)
do.aov(col1="zmean",dat=dat.100.250)
do.aov(col1="zmean",dat=dat.100.500)
do.aov(col1="zmean",dat=dat.250.500)

do.aov(col1="zsd",dat=dat.10.25)
do.aov(col1="zsd",dat=dat.10.50)
do.aov(col1="zsd",dat=dat.10.100)
do.aov(col1="zsd",dat=dat.10.250)
do.aov(col1="zsd",dat=dat.10.500)
do.aov(col1="zsd",dat=dat.25.50)
do.aov(col1="zsd",dat=dat.25.100)
do.aov(col1="zsd",dat=dat.25.250)
do.aov(col1="zsd",dat=dat.25.500)
do.aov(col1="zsd",dat=dat.50.100)
do.aov(col1="zsd",dat=dat.50.250)
do.aov(col1="zsd",dat=dat.50.500)
do.aov(col1="zsd",dat=dat.100.250)
do.aov(col1="zsd",dat=dat.100.500)
do.aov(col1="zsd",dat=dat.250.500)

do.aov(col1="pground",dat=dat.10.25)
do.aov(col1="pground",dat=dat.10.50)
do.aov(col1="pground",dat=dat.10.100)
do.aov(col1="pground",dat=dat.10.250)
do.aov(col1="pground",dat=dat.10.500)
do.aov(col1="pground",dat=dat.25.50)
do.aov(col1="pground",dat=dat.25.100)
do.aov(col1="pground",dat=dat.25.250)
do.aov(col1="pground",dat=dat.25.500)
do.aov(col1="pground",dat=dat.50.100)
do.aov(col1="pground",dat=dat.50.250)
do.aov(col1="pground",dat=dat.50.500)
do.aov(col1="pground",dat=dat.100.250)
do.aov(col1="pground",dat=dat.100.500)
do.aov(col1="pground",dat=dat.250.500)

do.aov(col1="zentropy",dat=dat.10.25)
do.aov(col1="zentropy",dat=dat.10.50)
do.aov(col1="zentropy",dat=dat.10.100)
do.aov(col1="zentropy",dat=dat.10.250)
do.aov(col1="zentropy",dat=dat.10.500)
do.aov(col1="zentropy",dat=dat.25.50)
do.aov(col1="zentropy",dat=dat.25.100)
do.aov(col1="zentropy",dat=dat.25.250)
do.aov(col1="zentropy",dat=dat.25.500)
do.aov(col1="zentropy",dat=dat.50.100)
do.aov(col1="zentropy",dat=dat.50.250)
do.aov(col1="zentropy",dat=dat.50.500)
do.aov(col1="zentropy",dat=dat.100.250)
do.aov(col1="zentropy",dat=dat.100.500)
do.aov(col1="zentropy",dat=dat.250.500)
