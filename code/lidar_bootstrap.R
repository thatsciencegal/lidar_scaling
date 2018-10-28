# load appropriate libraries
library(lidR)
library(abind)

# set data paths
in.path = "../Data/las_files/merged"
out.path <- "../Data/output"

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
  
  #Separate the header info and point cloud data
  lidar_in_head <- lidar_in@header
  lidar_in_data <- lidar_in@data
  
  #Bootstrap the lidar data
  n_iter <- 1000
  lidar_boot <- lidar_in_data
  for(i in 1:n_iter){
    x <- length(lidar_in_data$X)
    bootclasses <- lapply(1:n_iter, function(i)
      lidar_in_data[sample(x, x, replace=T), ])
    bootd <- as.data.frame(bootclasses[i])
    lidar_boot <- abind(lidar_boot, bootd, along = n_iter + 1)
  }
  
  #Extract each bootstrap from the array
  for(i in 1:n_iter){
    assign(paste0("lidar_boot_df_",i),as.data.frame(lidar_boot[,,i]))
  }
  
  #Make a list of the bootstrapped data frames
  
  #For each data frame:
  #calculate the dtm
  dtm = grid_terrain(lidar_in, res = 5, method = "knnidw")
  print("DTM finished")
  #remove ground points
  las_norm <- lasnormalize(lidar_in, dtm)
  print("Running normalized metrics")
  rm(lidar_in)
  #normalize all points to 0
  las_norm@data[Z  < 0, Z := 0]
  minx <- las_norm@header@data["Min X"]
  miny <- las_norm@header@data["Min Y"]
  minx <- as.numeric(minx)
  miny <- as.numeric(miny)
  
  las_filt <- las_norm %>% lasfilter(Z < 70)
  rm(las_norm)
  
  for(j in seq_along(raster_sizes)){
    # calculate the lidar metrics for each spatial resolution
    print(paste0("Working on grid size: ", raster_sizes[j]))
    normalized_metrics <- las_filt %>% grid_metrics(.stdmetrics, res = raster_sizes[j], start=c(minx, miny))
    
    # write out the raw data as a csv
    write.csv(normalized_metrics, file=paste0("../data/output/filtered_output/normalized_metrics_", i, "_size_", raster_sizes[j], ".csv"))
    
  }
  
}