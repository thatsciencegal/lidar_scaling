# Remove all from environment
rm(list = ls())

# Load libraries
library(ape)

# set data paths
in.path = "../Data/output/filtered_output/"

# list all of the data to be used
file.names <- dir(in.path, pattern = ".csv", full.names = TRUE)

# Create storage dataframe
morans <- as.data.frame(matrix(nrow = 660, ncol = 22))
names(morans) <- c("Tile", "Resolution", "max_h_obs","max_h_exp","max_h_sd","max_h_p",
                    "mean_h_obs","mean_h_exp","mean_h_sd","mean_h_p",
                    "rug_obs","rug_exp","rug_sd","rug_p",
                    "vert_div_obs","vert_div_exp","vert_div_sd","vert_div_p",
                    "open_obs","open_exp","open_sd","open_p")

#For loop to calculate Moran's I
for(i in seq_along(file.names)){
  #Read in the data file
  current.file <- file.names[i]
  dat <- read.csv(current.file)
  
  #Split the name of the data file
  file.split <- strsplit(current.file, '[.]')[[1]]
  file.split.numbers <- strsplit(file.split[3], '[_]')[[1]]
  
  if(file.split.numbers[6]!=1000){
  #Put the tile number and resolution in the data frame
  morans[i,1] <- file.split.numbers[4]
  morans[i,2] <- file.split.numbers[6]
  
  #Calculate the inverse distance
  dists <- as.matrix(dist(cbind(dat$X, dat$Y)))
  
  dists.inv <- 1/dists
  diag(dists.inv) <- 0
  
  #Calculate Moran's I
  moran.maxh <- Moran.I(dat$zmax, dists.inv, na.rm=T)
  moran.meanh <- Moran.I(dat$zmean, dists.inv, na.rm=T)
  moran.rug <- Moran.I(dat$zsd, dists.inv, na.rm=T)
  moran.vert.div <- Moran.I(dat$zentropy, dists.inv, na.rm=T)
  moran.open <- Moran.I(dat$pground, dists.inv, na.rm=T)
  
  #Store Moran's I data intro data frame
  morans[i,3] <- moran.maxh$observed
  morans[i,4] <- moran.maxh$expected
  morans[i,5] <- moran.maxh$sd
  morans[i,6] <- moran.maxh$p.value
  morans[i,7] <- moran.meanh$observed
  morans[i,8] <- moran.meanh$expected
  morans[i,9] <- moran.meanh$sd
  morans[i,10] <- moran.meanh$p.value
  morans[i,11] <- moran.rug$observed
  morans[i,12] <- moran.rug$expected
  morans[i,13] <- moran.rug$sd
  morans[i,14] <- moran.rug$p.value
  morans[i,15] <- moran.vert.div$observed
  morans[i,16] <- moran.vert.div$expected
  morans[i,17] <- moran.vert.div$sd
  morans[i,18] <- moran.vert.div$p.value
  morans[i,19] <- moran.open$observed
  morans[i,20] <- moran.open$expected
  morans[i,21] <- moran.open$sd
  morans[i,22] <- moran.open$p.value
  }
}

write.csv(morans, file=paste0("../Data/summary_data/morans.csv"))
