## Effects of Scale on LiDAR interpretation in a neotropical rainforest
## Code written by Christine Swanson
## June 28, 2017

# load appropriate libraries
library(lidR)
library(ggplot2)
library(dplyr)

# set data paths
in.path = "./Data/las_files/merged"

# list all of the lidar files to be used
file.names <- dir(in.path, pattern = ".las", full.names = TRUE)

# vector of grid sizes for lidar metrics analysis
raster_sizes <- c(5, 10, 15, 25, 30, 50, 60, 100, 120, 250, 500, 1000)

# Calculate the lidar metrics
for(i in seq_along(file.names)){
  # read in the current file
  current.file <- file.names[i]
  print(paste0("Working on: ", current.file))
  lidar_in <- readLAS(current.file)
  #calculate the dtm
  dtm = grid_terrain(lidar_in, res = 5, method = "knnidw")
  print("DTM finished")
  #remove ground points
  las_norm <- lasnormalize(lidar_in, dtm, copy=T)
  print("Running normalized metrics")
  rm(lidar_in)
  #normalize all points that are < 0 to 0
  las_norm@data[Z  < 0, Z := 0]
  #get lower corner lefthand corner of data
  minx <- las_norm@header@data["Min X"]
  miny <- las_norm@header@data["Min Y"]
  minx <- as.numeric(minx)
  miny <- as.numeric(miny)
  
  #filter data to only include points below 70 m in height
  las_filt <- las_norm %>% lasfilter(Z < 70)
  rm(las_norm)
  
  for(j in seq_along(raster_sizes)){
    # calculate the lidar metrics for each spatial resolution
    print(paste0("Working on grid size: ", raster_sizes[j]))
    normalized_metrics <- las_filt %>% grid_metrics(.stdmetrics, res = raster_sizes[j], start=c(minx, miny))
    
    # write out the raw data as a csv
    write.csv(normalized_metrics, file=paste0("./Data/output/filtered_output/normalized_metrics_", i, "_size_", raster_sizes[j], ".csv"))
    
  }
  
}