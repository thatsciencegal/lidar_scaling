## Effects of Scale on LiDAR interpretation in a neotropical rainforest
## Code written by Christine Swanson
## November 7, 2018

# load appropriate libraries
library(lidR)
library(beepr)

# set data paths
in.path = "./Data/las_files/merged"

# list all of the lidar files to be used
file.names <- dir(in.path, pattern = ".las", full.names = TRUE)

# Calculate the lidar metrics
for(i in seq_along(file.names)){
  # read in the current file
  current.file <- file.names[i]
  print(paste0("Working on: ", current.file))
  lidar_in <- readLAS(current.file)
  #calculate the dtm
  print("Calculating DTM")
  dtm<-grid_terrain(lidar_in,method="knnidw")
  print("normalizing data")
  las.norm<-lasnormalize(lidar_in,dtm,copy=T)
  print("writing file")
  beep_on_error(writeLAS(las.norm,file=paste0("C:/Users/christineswanson/Dropbox/lidar-scaling/Data/las_files/normalized/bz_las",i,".las")),9)
  rm(dtm)
  rm(las.norm)
  rm(lidar_in)
}

beep(8)
