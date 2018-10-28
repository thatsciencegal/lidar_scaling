library(spdep)
library(readr)

# Remove all from environment
rm(list = ls())

# set data paths
in.path = "/ufrc/bohlman/christineswanson/lidar-scaling/Data/output/filtered_output/"

# list all of the data to be used
file.names <- dir(in.path, pattern = ".csv", full.names = TRUE)

# Create storage dataframe
#morans <- as.data.frame(matrix(nrow = 660, ncol = 7))
#names(morans) <- c("Tile", "Resolution", "max_h", "mean_h", "rug", "vert_div", "open")

#For loop to calculate Moran's I
for(i in 361:405){
  #Read in the data file
  current.file <- file.names[i]
  dat <- read_csv(current.file)
  
  #Split the name of the data file
  file.split <- strsplit(current.file, '[.]')[[1]]
  file.split.numbers <- strsplit(file.split[1], '[_]')[[1]]
  print(paste0("Working on file ", i))
  
  if(file.split.numbers[6]<=250){
    #Put the tile number and resolution in the data frame
    #morans[i,1] <- file.split.numbers[4]
    #morans[i,2] <- file.split.numbers[6]
    
    #Calculate the inverse distance
    xy <- cbind(dat$X,dat$Y)
    colnames(xy)=c("x","y")
    #dist <- as.matrix(dist(xy))
    
    maxdist <- sqrt(2*((max(xy[,1])-min(xy[,1]))^2))
    
    correlog.sp<-data.frame(dist=seq(as.numeric(file.split.numbers[6]),maxdist,by=as.numeric(file.split.numbers[6])),
                            zmax.Morans.i=NA,zmax.Null.lcl=NA,zmax.Null.ucl=NA,zmax.Pvalue=NA,
                            zmean.Morans.i=NA,zmean.Null.lcl=NA,zmean.Null.ucl=NA,zmean.Pvalue=NA,
                            zsd.Morans.i=NA,zmsd.Null.lcl=NA,zsd.Null.ucl=NA,zsd.Pvalue=NA,
                            zentropy.Morans.i=NA,zentropy.Null.lcl=NA,zentropy.Null.ucl=NA,zentropy.Pvalue=NA,
                            pground.Morans.i=NA,pground.Null.lcl=NA,pground.Null.ucl=NA,pground.Pvalue=NA)
    
    for(i in 1:nrow(correlog.sp)){
      
      print(paste0("Working on Moran's i ", i, " of ", nrow(correlog.sp)))
      d.start<-correlog.sp[i,"dist"]-as.numeric(file.split.numbers[6])
      d.end<-sqrt(2*(correlog.sp[i,"dist"])^2)
      
      neigh <- dnearneigh(x=xy, d1=d.start, d.end, longlat=F)
      wts <- nb2listw(neighbours=neigh, style='W', zero.policy=T)
      zmax.mor.i <- moran.mc(x = dat$zmax, listw=wts, nsim=99, alternative="greater", zero.policy = T, na.action=na.omit)
      zmean.mor.i <- moran.mc(x = dat$zmean, listw=wts, nsim=99, alternative="greater", zero.policy = T, na.action = na.omit)
      zsd.mor.i <- moran.mc(x = dat$zsd, listw=wts, nsim=99, alternative="greater", zero.policy = T, na.action = na.omit)
      zentropy.mor.i <- moran.mc(x = dat$zentropy, listw=wts, nsim=99, alternative="greater", zero.policy = T, na.action = na.omit)
      pground.mor.i <- moran.mc(x = dat$pground, listw=wts, nsim=99, alternative="greater", zero.policy = T, na.action = na.omit)
      
      #summarize results from spdep
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
      
    }
    
    # neigh <- dnearneigh(x=xy, d1=0, d2=maxdist)
    # #wts <- nb2listw(neighbours = neigh, style='W', zero.policy = T)
    # 
    # #Calculate Moran's I
    # mormc.maxh <- moran.mc(x = dat$zmax, listw=wts, nsim=99, zero.policy = T)
    # ncf.meanh <- correlog(xy, dat$mean, method="Moran")
    # ncf.rug <- correlog(xy, dat$zsd, method="Moran")
    # ncf.vert.div <- correlog(xy, dat$zentropy, method="Moran")
    # ncf.open <- correlog(xy, dat$zpground, method="Moran")
    # 
    # #Store Moran's I data intro data frame
    # morans[i,3] <- moran.maxh
    # morans[i,4] <- moran.meanh
    # morans[i,5] <- moran.rug
    # morans[i,6] <- moran.vert.div
    # morans[i,7] <- moran.open
    
    print(paste0("Finished with this resolution."))
    write.csv(correlog.sp, file=paste0("/ufrc/bohlman/christineswanson/lidar-scaling/Data/summary_data/morans_summary_", file.split.numbers[4], "_", file.split.numbers[6],".csv"))
  }
  print(paste0("Finished with tile ", file.split.numbers[[4]]))
}