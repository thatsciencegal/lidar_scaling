library(pgirmess)

# Remove all from environment
rm(list = ls())

# set data paths
in.path = "../Data/output/filtered_output/"

# list all of the data to be used
file.names <- dir(in.path, pattern = ".csv", full.names = TRUE)

# Create storage dataframe
morans <- as.data.frame(matrix(nrow = 660, ncol = 7))
names(morans) <- c("Tile", "Resolution", "max_h", "mean_h", "rug", "vert_div", "open")

#For loop to calculate Moran's I
for(i in seq_along(file.names)){
  #Read in the data file
  current.file <- file.names[i]
  dat <- read.csv(current.file)
  
  #Split the name of the data file
  file.split <- strsplit(current.file, '[.]')[[1]]
  file.split.numbers <- strsplit(file.split[3], '[_]')[[1]]
  print(paste0("Working on file ", i))
  
  if(file.split.numbers[6]!=1000){
    #Put the tile number and resolution in the data frame
    morans[i,1] <- file.split.numbers[4]
    morans[i,2] <- file.split.numbers[6]
    
    #Calculate the inverse distance
    xy <- cbind(dat$X, dat$Y)
    
    #Calculate Moran's I
    moran.maxh <- correlog(xy, dat$zmax, method="Moran")
    moran.meanh <- correlog(xy, dat$mean, method="Moran")
    moran.rug <- correlog(xy, dat$zsd, method="Moran")
    moran.vert.div <- correlog(xy, dat$zentropy, method="Moran")
    moran.open <- correlog(xy, dat$zpground, method="Moran")
    
    #Store Moran's I data intro data frame
    morans[i,3] <- moran.maxh
    morans[i,4] <- moran.meanh
    morans[i,5] <- moran.rug
    morans[i,6] <- moran.vert.div
    morans[i,7] <- moran.open
  }
}

write.csv(morans, file=paste0("../Data/summary_data/morans_pgirmess.csv"))
