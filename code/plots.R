## Effects of Scale on LiDAR interpretation in a neotropical rainforest
## Code written by Christine Swanson
## June 28, 2017

# load appropriate libraries
library(lidR)
library(ggplot2)
library(dplyr)

# set data paths
in.path = "D:Dropbox/Dropbox/lidar-scaling/Data/las_files/merged"
out.path <- "../Data/output"

# list all of the lidar files to be used
file.names <- dir(in.path, pattern = ".las", full.names = TRUE)

dat <- data.frame(tile = seq(1,60,1), multipoly = "POLYGON", x_min = NA, y_min = NA, min_x = NA, max_y = NA, x_max = NA, y_max = NA, x_max = NA, min_y = NA, x_min = NA, y_min = NA)

# Calculate the lidar metrics
for(i in seq_along(file.names)){
  # read in the current file
  current.file <- file.names[i]
  print(paste0("Working on: ", current.file))
  lidar_in <- readLAS(current.file)

  dat[i,3] <- extent(lidar_in)[1] #minx
  dat[i,4] <- extent(lidar_in)[3] #miny
  dat[i,5] <- extent(lidar_in)[1]
  dat[i,6] <- extent(lidar_in)[4]
  dat[i,7] <- extent(lidar_in)[2]
  dat[i,8] <- extent(lidar_in)[4]
  dat[i,9] <- extent(lidar_in)[2]
  dat[i,10] <- extent(lidar_in)[3]
  dat[i,11] <- extent(lidar_in)[1] #minx
  dat[i,12] <- extent(lidar_in)[3]
}

