## Effects of scale on LiDAR interpretation in a neotropical rainforest
## Code written by Christine Swanson
## July 10, 2017

# This code is to write out the lidar scaling data to a summary csv

# set data paths
in.path <- "../Data/output/filtered_output"

# list all of the lidar files to be used
output.file.names <- dir(in.path, pattern = ".csv", full.names = TRUE)

# create an empty matrix to store results
lidar.data.matrix <- matrix(ncol = 8, nrow = 720)

lidar.data.names <- c("tile", "resolution", "topo", "max_height", "mean_height", "rugosity", 
                      "vertical_diversity", "openness") 

#Specify the sizes for each raster length
raster_sizes <- c(10, 1000, 120, 15, 250, 30, 5, 500, 60)

for(i in seq_along(output.file.names)){
  # read in the current file
  current.file <- output.file.names[i]
  lidar.data <- read.csv(current.file)
  
  # split the file name into different parts
  file.split <- strsplit(current.file, '[.]')[[1]]
  file.split.numbers <- strsplit(file.split[3], '[_]')[[1]]
  
  if(i <= 180 | i > 360 & i <= 540){
    lidar.data.matrix[i,3] <- "high"
  }  else{
    lidar.data.matrix[i,3] <- "low"
  }
  
  # add data to the analysis csv
  lidar.data.matrix[i,1] <- file.split.numbers[4]
  lidar.data.matrix[i,2] <- file.split.numbers[6]
  lidar.data.matrix[i,4] <- mean(lidar.data$zmax, na.rm=T)
  lidar.data.matrix[i,5] <- mean(lidar.data$zmean, na.rm=T)
  lidar.data.matrix[i,6] <- mean(lidar.data$zsd, na.rm=T)
  lidar.data.matrix[i,7] <- mean(lidar.data$zentropy, na.rm=T)
  lidar.data.matrix[i,8] <- mean(lidar.data$pground, na.rm=T)
}

#Coerce the matrix into a data frame
collated.lidar.data <- data.frame(lidar.data.matrix, stringsAsFactors = FALSE)
colnames(collated.lidar.data) <- lidar.data.names

# Write the data fram as a csv
write.csv(collated.lidar.data, "./Data/summary_data/lidar_summary_data_3.csv")
