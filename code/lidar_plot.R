library(lidR)
library(ggplot2)
library(viridis)

in.path = "D:/Dropbox/Dropbox/lidar-scaling/Data/las_files/merged"
file.names <- dir(in.path, pattern = ".las", full.names = TRUE)

current.file <- file.names[12]
print(current.file)
lidar_in <- readLAS(current.file)
dtm = grid_terrain(lidar_in, res = 1, method = "kriging")
print("DTM finished")
las_norm <- lasnormalize(lidar_in, dtm, copy = T)
las_norm@data[Z  < 0, Z := 0]

cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", "#CC79A7", "#F0E442", "#b66dff")

height.colors(100)
plot(lidar_in, colorPalette = rainbow(256))

grid_metrics3d(las_norm, entropy(Z)) %>% plot(trim = 0.99)
ggsave("bzn_low_topo_site_3.svg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

plot(las_norm, colorPalette = rainbow(256), trim = 0.99)


##Vertical diversity histograms

myMetrics = function(z, by = 1){
  zmax=ifelse(max(z)<70,max(z),70)
  breaks=seq(0,zmax+1,by)
  h=hist(z,breaks=breaks)
}

dtm = grid_terrain(lidar_in, res = 5, method = "knnidw")
print("DTM finished")
#remove ground points
las_norm <- lasnormalize(lidar_in, dtm)
print("Running normalized metrics")
rm(lidar_in)
#normalize all points that are < 0 to 0
las_norm@data[Z  < 0, Z := 0]


#get lower corner lefthand corner of data
minx <- las_norm_head["Min X"]
miny <- las_norm_head["Min Y"]
minx <- as.numeric(minx)
miny <- as.numeric(miny)

#filter data to only include points below 70 m in height
las_filt <- las_norm %>% lasfilter(Z < 70)
rm(las_norm)

X<-grid_metrics(las_norm,myMetrics(Z,by=1),10,start=c(minx,miny))

zmax = max(las_norm@data$z, na.rm=T)
breaks = seq(0, ceiling(zmax/by)*by, by)
h = hist(z, breaks = breaks)
return(list(histogram = list(h)))
