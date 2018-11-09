library(lidR)
library(ggplot2)
library(viridis)

#Set up the input path
in.path = "./Data/las_files/merged"

#Get the file names in the directory
file.names <- dir(in.path, pattern = ".las", full.names = TRUE)

#Plot each of the lidar tiles in the directory
for(i in seq_along(file.names)){
  #Determine current file
  current.file <- file.names[i]
  print(current.file)
  #Read in current file
  lidar_in <- readLAS(current.file)
  #Calculate the DTM
  dtm = grid_terrain(lidar_in, res = 1, method = "kriging")
  print("DTM finished")
  #Normalize the lidar data
  las_norm <- lasnormalize(lidar_in, dtm, copy = T)
  las_norm@data[Z  < 0, Z := 0]
  #Plot the lidar data
  plot(lidar_in, colorPalette = rainbow(256), trim=0.99)
}