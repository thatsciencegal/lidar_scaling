library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(sp)
library(ggsn)
library(rgdal)

xy<-read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/tiles4.csv")
xy <- xy[,4:ncol(xy)]

xy1<-as.data.frame(lapply(xy,as.double))

points <- list()

for(i in 1:nrow(xy)){
  points[[i]] <- matrix(as.numeric(xy1[i,]),ncol=2,byrow=T)
}

poly<-lapply(points, function (x) Polygon(x))
      

polies<-list()
for (i in 1:60) {polies[[i]] <- Polygons(list(poly[[i]]), ID = i)}
polies.sp <- SpatialPolygons(polies, proj4string = CRS("+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

plot(polies.sp)

topo<-c(rep("high",15), rep("low",15), rep("high",15),rep("low",15))

polys.df <- SpatialPolygonsDataFrame(polies.sp, data.frame(id=1:60, relief = topo))
rel <- rep(polys.df[[2]],each=5)

poly.trans <-  spTransform(polys.df, CRS("+proj=longlat +datum=WGS84"))
poly.box <- bbox(poly.trans)
sq_map <- get_map(location = poly.box, maptype = "satellite", source = "google", zoom = 10)

belize <- readOGR(dsn="D:/Dropbox/Dropbox/lidar-scaling/Data/gis", layer="belize_poly")
belize.trans <- spTransform(belize, CRS("+proj=longlat +datum=WGS84"))
belize.id <- belize.trans[[1]]

poly.map<-ggmap(sq_map)+
  geom_polygon(data = poly.trans, aes(x=long,y=lat, group=id,color=rel), fill = NA, size=1)+
  geom_polygon(data = belize.trans, aes(x=long,y=lat, group = belize.id),fill=NA, size=1)+
  xlim(poly.box[1,1]-0.1, poly.box[1,2]+0.1)+
  ylim(poly.box[2,1]-0.1, poly.box[2,2]+0.1)+
  scale_color_manual(values = c("#3b518bff","#f5e626ff"), name = "Topography", 
                       breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  labs(x = "Longitude", y = "Latitude") +
  scalebar(x.min = poly.box[1,1],x.max=poly.box[1,2],y.min=poly.box[2,1],y.max=poly.box[2,2], dist = 10, model = "WGS84", dd2km = T,
           anchor = c(x = poly.box[1,2]+0.08, y = poly.box[2,1]-0.08), height = 0.01)

north2(poly.map,symbol=10, scale = 0.1)
  
writeOGR(polys.df, "D:/Dropbox/Dropbox/lidar-scaling/Data/gis", "lidar_polys",driver="ESRI Shapefile")
