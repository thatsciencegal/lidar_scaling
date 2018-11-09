## Effects of scale on LiDAR interpretation in a neotropical rainforest
## Code written by Christine Swanson
## June 11, 2018

#Load libraries
library(spdep)
library(readr)

# Remove all from environment
rm(list = ls())

# set data paths
in.path = "./Data/output/filtered_output/"

# list all of the data to be used
file.names <- dir(in.path, pattern = ".csv", full.names = TRUE)

#For loop to read in data
for(i in 1:nrow(file.names)){
  #Read in the data file
  current.file <- file.names[i]
  dat <- read.csv(current.file)
  
  #Split the name of the data file to get resolutions for each file
  file.split <- strsplit(current.file, '[.]')[[1]]
  file.split.numbers <- strsplit(file.split[1], '[_]')[[1]]
  print(paste0("Working on file ", i))
  
  #Get coordinates from the dataset
  xy <- cbind(dat$X,dat$Y)
  colnames(xy)=c("x","y")
  
  #Calculate maximum distance (Pythagorean theorem)
  maxdist <- sqrt(2*((max(xy[,1])-min(xy[,1]))^2))
  
  #Create a data frame to store results
  correlog.sp<-data.frame(dist=seq(as.numeric(file.split.numbers[6]),maxdist,by=as.numeric(file.split.numbers[6])),
                          zmax.Morans.i=NA,zmax.Null.lcl=NA,zmax.Null.ucl=NA,zmax.Pvalue=NA,
                          zmean.Morans.i=NA,zmean.Null.lcl=NA,zmean.Null.ucl=NA,zmean.Pvalue=NA,
                          zsd.Morans.i=NA,zmsd.Null.lcl=NA,zsd.Null.ucl=NA,zsd.Pvalue=NA,
                          zentropy.Morans.i=NA,zentropy.Null.lcl=NA,zentropy.Null.ucl=NA,zentropy.Pvalue=NA,
                          pground.Morans.i=NA,pground.Null.lcl=NA,pground.Null.ucl=NA,pground.Pvalue=NA)
  
  #Calculate Moran's I for each lag distance  
  for(i in 1:nrow(correlog.sp)){
    
    print(paste0("Working on Moran's i ", i, " of ", nrow(correlog.sp)))
    d.start<-correlog.sp[i,"dist"]-as.numeric(file.split.numbers[6])
    d.end<-correlog.sp[i,"dist"]
    
    #Calculate the nearest neighbor and weights
    neigh <- dnearneigh(x=xy, d1 =  d.start, d.end, longlat=F)
    wts <- nb2listw(neighbours=neigh, style='W', zero.policy=T)
    
    #Monte Carlo Moran's I simulation of each of the studied metrics
    zmax.mor.i <- moran.mc(x = dat$zmax, listw=wts, nsim=99, alternative="greater", zero.policy = T, na.action=na.omit)
    zmean.mor.i <- moran.mc(x = dat$zmean, listw=wts, nsim=99, alternative="greater", zero.policy = T, na.action = na.omit)
    zsd.mor.i <- moran.mc(x = dat$zsd, listw=wts, nsim=99, alternative="greater", zero.policy = T, na.action = na.omit)
    zentropy.mor.i <- moran.mc(x = dat$zentropy, listw=wts, nsim=99, alternative="greater", zero.policy = T, na.action = na.omit)
    pground.mor.i <- moran.mc(x = dat$pground, listw=wts, nsim=99, alternative="greater", zero.policy = T, na.action = na.omit)
    
    #summarize results from Moran's I
    correlog.sp[i, 1]<-(d.end+d.start)/2              #mean dist
    correlog.sp[i, 2]<-zmax.mor.i$statistic 								#observed moran's i
    correlog.sp[i, 3]<-quantile(zmax.mor.i$res, probs = 0.025,na.rm = TRUE)#lower null envelope	
    correlog.sp[i, 4]<-quantile(zmax.mor.i$res, probs = 0.975,na.rm = TRUE)#upper null envelope
    correlog.sp[i, 5]<-zmean.mor.i$p.value									#p-value for moran's i at that distance category
    correlog.sp[i, 6]<-zmean.mor.i$statistic 								#observed moran's i
    correlog.sp[i, 7]<-quantile(zmean.mor.i$res, probs = 0.025,na.rm = TRUE)#lower null envelope	
    correlog.sp[i, 8]<-quantile(zmean.mor.i$res, probs = 0.975,na.rm = TRUE)#upper null envelope
    correlog.sp[i, 9]<-zmean.mor.i$p.value									#p-value for moran's i at that distance category
    correlog.sp[i, 10]<-zsd.mor.i$statistic 								#observed moran's i
    correlog.sp[i, 11]<-quantile(zsd.mor.i$res, probs = 0.025,na.rm = TRUE)#lower null envelope	
    correlog.sp[i, 12]<-quantile(zsd.mor.i$res, probs = 0.975,na.rm = TRUE)#upper null envelope
    correlog.sp[i, 13]<-zsd.mor.i$p.value									#p-value for moran's i at that distance category
    correlog.sp[i, 14]<-zentropy.mor.i$statistic 								#observed moran's i
    correlog.sp[i, 15]<-quantile(zentropy.mor.i$res, probs = 0.025,na.rm = TRUE)#lower null envelope	
    correlog.sp[i, 16]<-quantile(zentropy.mor.i$res, probs = 0.975,na.rm = TRUE)#upper null envelope
    correlog.sp[i, 17]<-zentropy.mor.i$p.value									#p-value for moran's i at that distance category
    correlog.sp[i, 18]<-pground.mor.i$statistic 								#observed moran's i
    correlog.sp[i, 19]<-quantile(pground.mor.i$res, probs = 0.025,na.rm = TRUE)#lower null envelope	
    correlog.sp[i, 20]<-quantile(pground.mor.i$res, probs = 0.975,na.rm = TRUE)#upper null envelope
    correlog.sp[i, 21]<-pground.mor.i$p.value									#p-value for moran's i at that distance category
    
    print(paste0("Finished with this resolution."))
  }
  write.csv(correlog.sp, file=paste0("./Data/summary_data/morans_summary_", file.split.numbers[4], "_", file.split.numbers[6],".csv"))
  print(paste0("Finished with tile ", file.split.numbers[[4]]))
}

