library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)
library(svglite)
library(ggpubr)

in.path <- "D:/Dropbox/Dropbox/lidar-scaling/Data/summary_data"

lidar_summary <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/summary_data/lidar_summary_data_3.csv")

lidar_summary$tile <- factor(lidar_summary$tile)
lidar_summary$resolution <- factor(lidar_summary$resolution)

lidar_summary_5 <- lidar_summary %>%
  filter(resolution == 5)

lidar_summary_10 <- lidar_summary %>% 
  filter(resolution == 10)

lidar_summary_15 <- lidar_summary %>% 
  filter(resolution == 15)

lidar_summary_25 <- lidar_summary %>% 
  filter(resolution == 25)

lidar_summary_30 <- lidar_summary %>% 
  filter(resolution == 30)

lidar_summary_50 <- lidar_summary %>% 
  filter(resolution == 50)

lidar_summary_60 <- lidar_summary %>% 
  filter(resolution == 60)

lidar_summary_100 <- lidar_summary %>% 
  filter(resolution == 100)

lidar_summary_120 <- lidar_summary %>% 
  filter(resolution == 120)

lidar_summary_250 <- lidar_summary %>% 
  filter(resolution == 250)

lidar_summary_500 <- lidar_summary %>% 
  filter(resolution == 500)

lidar_summary_1000 <- lidar_summary %>% 
  filter(resolution == 1000)

outlier_filter <- function(dat, col){
  col <- enquo(col)
  dat %>% 
    filter(!(abs((!!col) - median(!!col))>(2*sd(!!col))))
}
 
lidar_summary_5_max_height_outlier_filter <- outlier_filter(lidar_summary_5, max_height)
lidar_summary_10_max_height_outlier_filter <- outlier_filter(lidar_summary_10, max_height)
lidar_summary_15_max_height_outlier_filter <- outlier_filter(lidar_summary_15, max_height)
lidar_summary_25_max_height_outlier_filter <- outlier_filter(lidar_summary_25, max_height)
lidar_summary_30_max_height_outlier_filter <- outlier_filter(lidar_summary_30, max_height)
lidar_summary_50_max_height_outlier_filter <- outlier_filter(lidar_summary_50, max_height)
lidar_summary_60_max_height_outlier_filter <- outlier_filter(lidar_summary_60, max_height)
lidar_summary_100_max_height_outlier_filter <- outlier_filter(lidar_summary_100, max_height)
lidar_summary_120_max_height_outlier_filter <- outlier_filter(lidar_summary_120, max_height)
lidar_summary_250_max_height_outlier_filter <- outlier_filter(lidar_summary_250, max_height)
lidar_summary_500_max_height_outlier_filter <- outlier_filter(lidar_summary_500, max_height)
lidar_summary_1000_max_height_outlier_filter <- outlier_filter(lidar_summary_1000, max_height)
 
lidar_max_height_outlier_filter <- rbind(lidar_summary_5_max_height_outlier_filter, lidar_summary_10_max_height_outlier_filter,
                                          lidar_summary_15_max_height_outlier_filter, lidar_summary_25_max_height_outlier_filter,
                                          lidar_summary_30_max_height_outlier_filter, lidar_summary_50_max_height_outlier_filter,
                                          lidar_summary_60_max_height_outlier_filter, lidar_summary_100_max_height_outlier_filter, 
                                          lidar_summary_120_max_height_outlier_filter, lidar_summary_250_max_height_outlier_filter,
                                          lidar_summary_500_max_height_outlier_filter, lidar_summary_1000_max_height_outlier_filter)
                                          
lidar_summary_5_mean_height_outlier_filter <- outlier_filter(lidar_summary_5, mean_height)
lidar_summary_10_mean_height_outlier_filter <- outlier_filter(lidar_summary_10, mean_height)
lidar_summary_15_mean_height_outlier_filter <- outlier_filter(lidar_summary_15, mean_height)
lidar_summary_25_mean_height_outlier_filter <- outlier_filter(lidar_summary_25, mean_height)
lidar_summary_30_mean_height_outlier_filter <- outlier_filter(lidar_summary_30, mean_height)
lidar_summary_50_mean_height_outlier_filter <- outlier_filter(lidar_summary_50, mean_height)
lidar_summary_60_mean_height_outlier_filter <- outlier_filter(lidar_summary_60, mean_height)
lidar_summary_100_mean_height_outlier_filter <- outlier_filter(lidar_summary_100, mean_height)
lidar_summary_120_mean_height_outlier_filter <- outlier_filter(lidar_summary_120, mean_height)
lidar_summary_250_mean_height_outlier_filter <- outlier_filter(lidar_summary_250, mean_height)
lidar_summary_500_mean_height_outlier_filter <- outlier_filter(lidar_summary_500, mean_height)
lidar_summary_1000_mean_height_outlier_filter <- outlier_filter(lidar_summary_1000, mean_height)

lidar_mean_height_outlier_filter <- rbind(lidar_summary_5_mean_height_outlier_filter, lidar_summary_10_mean_height_outlier_filter,
                                          lidar_summary_15_mean_height_outlier_filter, lidar_summary_25_mean_height_outlier_filter,
                                          lidar_summary_30_mean_height_outlier_filter, lidar_summary_50_mean_height_outlier_filter,
                                          lidar_summary_60_mean_height_outlier_filter, lidar_summary_100_mean_height_outlier_filter,
                                          lidar_summary_120_mean_height_outlier_filter, lidar_summary_250_mean_height_outlier_filter,
                                          lidar_summary_500_mean_height_outlier_filter, lidar_summary_1000_mean_height_outlier_filter)

lidar_summary_5_rugosity_outlier_filter <- outlier_filter(lidar_summary_5, rugosity)
lidar_summary_10_rugosity_outlier_filter <- outlier_filter(lidar_summary_10, rugosity)
lidar_summary_15_rugosity_outlier_filter <- outlier_filter(lidar_summary_15, rugosity)
lidar_summary_25_rugosity_outlier_filter <- outlier_filter(lidar_summary_25, rugosity)
lidar_summary_30_rugosity_outlier_filter <- outlier_filter(lidar_summary_30, rugosity)
lidar_summary_50_rugosity_outlier_filter <- outlier_filter(lidar_summary_50, rugosity)
lidar_summary_60_rugosity_outlier_filter <- outlier_filter(lidar_summary_60, rugosity)
lidar_summary_100_rugosity_outlier_filter <- outlier_filter(lidar_summary_100, rugosity)
lidar_summary_120_rugosity_outlier_filter <- outlier_filter(lidar_summary_120, rugosity)
lidar_summary_250_rugosity_outlier_filter <- outlier_filter(lidar_summary_250, rugosity)
lidar_summary_500_rugosity_outlier_filter <- outlier_filter(lidar_summary_500, rugosity)
lidar_summary_1000_rugosity_outlier_filter <- outlier_filter(lidar_summary_1000, rugosity)

lidar_rugosity_outlier_filter <- rbind(lidar_summary_5_rugosity_outlier_filter, lidar_summary_10_rugosity_outlier_filter,
                                           lidar_summary_15_rugosity_outlier_filter, lidar_summary_25_rugosity_outlier_filter,
                                           lidar_summary_30_rugosity_outlier_filter, lidar_summary_50_rugosity_outlier_filter,
                                           lidar_summary_60_rugosity_outlier_filter, lidar_summary_100_rugosity_outlier_filter,
                                           lidar_summary_120_rugosity_outlier_filter, lidar_summary_250_rugosity_outlier_filter,
                                           lidar_summary_500_rugosity_outlier_filter, lidar_summary_1000_rugosity_outlier_filter)

lidar_summary_5_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_5, vertical_diversity)
lidar_summary_10_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_10, vertical_diversity)
lidar_summary_15_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_15, vertical_diversity)
lidar_summary_25_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_25, vertical_diversity)
lidar_summary_30_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_30, vertical_diversity)
lidar_summary_50_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_50, vertical_diversity)
lidar_summary_60_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_60, vertical_diversity)
lidar_summary_100_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_100, vertical_diversity)
lidar_summary_120_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_120, vertical_diversity)
lidar_summary_250_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_250, vertical_diversity)
lidar_summary_500_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_500, vertical_diversity)
lidar_summary_1000_vertical_diversity_outlier_filter <- outlier_filter(lidar_summary_1000, vertical_diversity)

lidar_vertical_diversity_outlier_filter <- rbind(lidar_summary_5_vertical_diversity_outlier_filter, lidar_summary_10_vertical_diversity_outlier_filter,
                                                  lidar_summary_15_vertical_diversity_outlier_filter, lidar_summary_25_vertical_diversity_outlier_filter,
                                                  lidar_summary_30_vertical_diversity_outlier_filter, lidar_summary_50_vertical_diversity_outlier_filter,
                                                  lidar_summary_60_vertical_diversity_outlier_filter, lidar_summary_100_vertical_diversity_outlier_filter,
                                                  lidar_summary_120_vertical_diversity_outlier_filter, lidar_summary_250_vertical_diversity_outlier_filter,
                                                  lidar_summary_500_vertical_diversity_outlier_filter, lidar_summary_1000_vertical_diversity_outlier_filter)

lidar_summary_5_openness_outlier_filter <- outlier_filter(lidar_summary_5, openness)
lidar_summary_10_openness_outlier_filter <- outlier_filter(lidar_summary_10, openness)
lidar_summary_15_openness_outlier_filter <- outlier_filter(lidar_summary_15, openness)
lidar_summary_25_openness_outlier_filter <- outlier_filter(lidar_summary_25, openness)
lidar_summary_30_openness_outlier_filter <- outlier_filter(lidar_summary_30, openness)
lidar_summary_50_openness_outlier_filter <- outlier_filter(lidar_summary_50, openness)
lidar_summary_60_openness_outlier_filter <- outlier_filter(lidar_summary_60, openness)
lidar_summary_100_openness_outlier_filter <- outlier_filter(lidar_summary_100, openness)
lidar_summary_120_openness_outlier_filter <- outlier_filter(lidar_summary_120, openness)
lidar_summary_250_openness_outlier_filter <- outlier_filter(lidar_summary_250, openness)
lidar_summary_500_openness_outlier_filter <- outlier_filter(lidar_summary_500, openness)
lidar_summary_1000_openness_outlier_filter <- outlier_filter(lidar_summary_1000, openness)

lidar_openness_outlier_filter <- rbind(lidar_summary_5_openness_outlier_filter, lidar_summary_10_openness_outlier_filter,
                                        lidar_summary_15_openness_outlier_filter, lidar_summary_25_openness_outlier_filter,
                                        lidar_summary_30_openness_outlier_filter, lidar_summary_50_openness_outlier_filter,
                                        lidar_summary_60_openness_outlier_filter, lidar_summary_100_openness_outlier_filter,
                                        lidar_summary_120_openness_outlier_filter, lidar_summary_250_openness_outlier_filter,
                                        lidar_summary_500_openness_outlier_filter, lidar_summary_1000_openness_outlier_filter)

lidar_summary_maxh <- outlier_filter(lidar_summary, max_height)
lidar_summary_meanh <- outlier_filter(lidar_summary, mean_height)
lidar_summary_rugh <- outlier_filter(lidar_summary, rugosity)
lidar_summary_vert_div <- outlier_filter(lidar_summary, vertical_diversity)
lidar_summary_open <- outlier_filter(lidar_summary, openness)

theme_christine <- function (base_size = 12, base_family = "") {
  #theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      ##axis properties
      axis.text.x = element_text(colour = "black", size = 14),
      axis.text.y = element_text(colour = "black", size = 14),
      axis.title.x = element_text(colour = "black", size = 18),
      axis.title.y = element_text(colour = "black", angle=90, size = 24),
      
      ##panel properties
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(size=0),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      
      ##plot background
      plot.background = element_rect(fill="white"),
      
      ##legend properties
      legend.background = element_rect(fill="white"),
      legend.key = element_blank(),
      legend.text = element_text(colour = "black", size = 16),
      legend.position = "right",
      legend.title = element_text(colour = "black", size = 16)
      
      )   
}
lidar_summary_maxh$resolution <- as.numeric(levels(lidar_summary_maxh$resolution))[lidar_summary_maxh$resolution]
lidar_summary_maxh2 <- lidar_summary_maxh %>% filter(resolution > 5)
lidar_summary_maxh2$resolution <- as.factor(lidar_summary_maxh2$resolution)

lidar_summary_meanh$resolution <- as.numeric(levels(lidar_summary_meanh$resolution))[lidar_summary_meanh$resolution]
lidar_summary_meanh2 <- lidar_summary_meanh %>% filter(resolution > 5)
lidar_summary_meanh2$resolution <- as.factor(lidar_summary_meanh2$resolution)

lidar_summary_rugh$resolution <- as.numeric(levels(lidar_summary_rugh$resolution))[lidar_summary_rugh$resolution]
lidar_summary_rug2 <- lidar_summary_rugh %>% filter(resolution > 5)
lidar_summary_rug2$resolution <- as.factor(lidar_summary_rug2$resolution)

lidar_summary_vert_div$resolution <- as.numeric(levels(lidar_summary_vert_div$resolution))[lidar_summary_vert_div$resolution]
lidar_summary_vert_div2 <- lidar_summary_vert_div %>% filter(resolution > 5)
lidar_summary_vert_div2$resolution <- as.factor(lidar_summary_vert_div2$resolution)

lidar_summary_open$resolution <- as.numeric(levels(lidar_summary_open$resolution))[lidar_summary_open$resolution]
lidar_summary_open2 <- lidar_summary_open %>% filter(resolution > 5)
lidar_summary_open2$resolution <- as.factor(lidar_summary_open2$resolution)

lidar_summary2 <- lidar_summary %>% filter(resolution > 5)
lidar_summary2$resolution <- as.factor(lidar_summary2$resolution)

max_height_plot <- ggplot(lidar_summary_maxh2, aes(x=resolution, y=max_height, fill = topo))+#, colour = "#56B4E9"))+
  geom_boxplot()+#fill = "#E69F00", colour = "black", notch = TRUE) +
  theme_christine() +
  scale_fill_manual(values = c("#8c8c8c","#f5e626ff"), name = "Topography", 
                    breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  #scale_colour_manual(values = c("black")) +
  #guides(fill = "legend", colour = "none") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  labs(x = "Grid cell length (m)", y = "Maximum height (m)")

mean_height_plot <- ggplot(lidar_summary_meanh2, aes(x=resolution, y=mean_height, fill = topo))+#, colour = "#56B4E9"))+
  geom_boxplot()+#fill = "#E69F00", colour = "black", notch = TRUE) +
  theme_christine() +
  scale_fill_manual(values = c("#8c8c8c","#f5e626ff"), name = "Topography", 
                    breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  #scale_colour_manual(values = c("black")) +
  #guides(fill = "legend", colour = "none") +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()) + 
  labs(x = "Grid cell length (m)", y = "Mean height (m)")

rugosity_plot <- ggplot(lidar_summary_rug2, aes(x=resolution, y=rugosity, fill = topo))+#, colour = "#56B4E9"))+
  geom_boxplot()+#fill = "#E69F00", colour = "black", notch = TRUE) +
  theme_christine() +
  scale_fill_manual(values = c("#8c8c8c","#f5e626ff"), name = "Topography", 
                    breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  #scale_colour_manual(values = c("black")) +
  #guides(fill = "legend", colour = "none") +
  theme(legend.position = "none", axis.title.x = element_blank()) + 
  labs(x = "Grid cell length (m)", y = "Rugosity (m)")

vertical_diversity_plot <- ggplot(lidar_summary_vert_div2, aes(x=resolution, y=vertical_diversity, fill = topo))+#, colour = "#56B4E9"))+
  geom_boxplot()+#fill = "#E69F00", colour = "black", notch = TRUE) +
  theme_christine() +
  scale_fill_manual(values = c("#8c8c8c","#f5e626ff"), name = "Topography", 
                    breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  #scale_colour_manual(values = c("black")) +
  #guides(fill = "legend", colour = "none") +
  theme(legend.position = "none", axis.title.x = element_blank()) + 
  labs(x = "Grid cell length (m)", y = "Vertical Diversity")

openness_plot <- ggplot(lidar_summary_open2, aes(x=resolution, y=openness, fill = topo))+#, colour = "#56B4E9"))+
  geom_boxplot()+#fill = "#E69F00", colour = "black", notch = TRUE) +
  theme_christine() +
  scale_fill_manual(values = c("#8c8c8c","#f5e626ff"), name = "Topography", 
                    breaks=c("high", "low"), labels = c("High relief", "Low relief")) +
  #scale_colour_manual(values = c("black")) +
  #guides(fill = "legend", colour = "none") +
  theme(legend.position = "none", axis.title.x = element_blank()) + 
  labs(x = "Grid cell length (m)", y = "Canopy Openness (%)")

boxplots <- ggarrange(max_height_plot, mean_height_plot,rugosity_plot,vertical_diversity_plot,openness_plot,labels = c("A","B","C","D","E"), font.label = list(size = 36),ncol = 3, nrow = 2, common.legend = T, legend = "right", align = "hv")
annotate_figure(boxplots, bottom = text_grob("Grid cell length (m)", size = 24))

ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/summary_boxplots.svg", plot = last_plot(), units = "in", width = 40, height = 20, dpi = 600)

max_height_plot
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/max_height_plot.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

mean_height_plot
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/mean_height_plot.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

rugosity_plot
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/rugosity_plot.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

vertical_diversity_plot
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/vertical_diversity_plot.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

openness_plot
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/openness_plot.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

##Raster of different metrics for an example plot at a 25x25 m scale

library(rgdal)
library(raster)
library(sp)
library(viridis)

dat <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/filtered_output/normalized_metrics_12_size_25.csv")

theme_rast <- function (base_size = 12, base_family = "") {
  #theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
  theme(
    ##axis properties
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks=element_blank(),
    axis.line = element_blank(),
    
    ##panel properties
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    
    ##plot background
    plot.background = element_rect(fill="white"),
    
    ##legend properties
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "right",
    legend.title = element_text(colour = "black", size = 16)
    
  )   
}

maxh_rast<-ggplot(dat)+
  geom_tile(aes(x=X,y=Y,fill=zmax))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill = "Maximum height (m)") +
  theme_rast()

meanh_rast<-ggplot(dat)+
  geom_tile(aes(x=X,y=Y,fill=zmean))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Mean height (m)")+
  theme_rast()

rug_rast<-ggplot(dat)+
  geom_tile(aes(x=X,y=Y,fill=zsd))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Rugosity (m)") + 
  theme_rast()

vdiv_rast<-ggplot(dat)+
  geom_tile(aes(x=X,y=Y,fill=zentropy))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Vertical diversity") +
  theme_rast()

pground_rast<-ggplot(dat)+
  geom_tile(aes(x=X,y=Y,fill=pground))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Canopy openness (%)")+
  theme_rast()

maxh_rast
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/maxh_rast.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

meanh_rast
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/meanh_rast.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

rug_rast
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/rug_rast.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

vdiv_rast
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/vdiv_rast.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

pground_rast
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/pground_rast.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)


rast25 <- ggarrange(maxh_rast, meanh_rast, rug_rast, vdiv_rast,pground_rast,ncol = 3, nrow = 2, common.legend = F, legend = "right", align = "hv")
annotate_figure(rast25)
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/rasters.svg", plot = last_plot(), units = "in", width = 30, height = 15, dpi = 600)


dat10 <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/filtered_output/normalized_metrics_12_size_10.csv")

vdiv10<-ggplot(dat10)+
  geom_tile(aes(x=X,y=Y,fill=zentropy))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Vertical diversity")+
  theme_rast()

dat50 <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/filtered_output/normalized_metrics_12_size_50.csv")

vdiv50<-ggplot(dat50)+
  geom_tile(aes(x=X,y=Y,fill=zentropy))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Vertical diversity") +
  theme_rast()

dat250 <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/filtered_output/normalized_metrics_12_size_250.csv")

vdiv250<-ggplot(dat250)+
  geom_tile(aes(x=X,y=Y,fill=zentropy))+
  scale_fill_viridis(option="cividis",na.value="white", direction = -1)+
  labs(fill="Vertical diversity") +
  theme_rast()

vdiv_rasters <- ggarrange(vdiv10,vdiv50,vdiv250,ncol = 2, nrow = 2, common.legend = F, legend = "right", align = "hv")
annotate_figure(vdiv_rasters)
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/vdiv_rasters.svg", plot = last_plot(), units = "in", width = 8, height = 6, dpi = 600)


#Histograms of vertical diversity based on FUSION categorization

vdiv10.dat <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/bzn_high_topo_6_metrics10_all_returns_strata_stats.csv")
vdiv25.dat <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/bzn_high_topo_6_metrics25_all_returns_strata_stats.csv")
vdiv50.dat <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/bzn_high_topo_6_metrics50_all_returns_strata_stats.csv")
vdiv250.dat <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/bzn_high_topo_6_metrics250_all_returns_strata_stats.csv")

vdiv10.dat<-vdiv10.dat[grepl("total.return.count",names(vdiv10.dat))]
vdiv25.dat<-vdiv25.dat[grepl("total.return.count",names(vdiv25.dat))]
vdiv50.dat<-vdiv50.dat[grepl("total.return.count",names(vdiv50.dat))]
vdiv250.dat<-vdiv250.dat[grepl("total.return.count",names(vdiv250.dat))]

vdiv10.mean <- sapply(vdiv10.dat,mean)
vdiv10.sd <- sapply(vdiv10.dat,sd)
vdiv10.summary <- data.frame("mean"=vdiv10.mean,"sd"=vdiv10.sd, "bin"=seq(1:22))

vdiv25.mean <- sapply(vdiv25.dat,mean)
vdiv25.sd <- sapply(vdiv25.dat,sd)
vdiv25.summary <- data.frame("mean"=vdiv25.mean,"sd"=vdiv25.sd, "bin"=seq(1:22))

vdiv50.mean <- sapply(vdiv50.dat,mean)
vdiv50.sd <- sapply(vdiv50.dat,sd)
vdiv50.summary <- data.frame("mean" = vdiv50.mean, "sd"=vdiv50.sd, "bin"=seq(1:22))

vdiv250.mean <- sapply(vdiv250.dat,mean)
vdiv250.sd <- sapply(vdiv250.dat,sd)
vdiv250.summary <- data.frame("mean"=vdiv250.mean, "sd"=vdiv250.sd, "bin"=seq(1:22))

ggplot(vdiv10.summary,aes(x=bin,y=mean))+
  geom_bar(stat="identity",color="black",fill="#f5e626ff")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,600))+
  scale_x_continuous(expand=c(0,0))+
  coord_flip()+
  labs(y="Mean number of returns", x="Height bin")

ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/height_hist10.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

ggplot(vdiv25.summary,aes(x=bin,y=mean))+
  geom_bar(stat="identity",color="black",fill="#21918dff")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd)) +
  scale_y_continuous(expand=c(0,0)
                     )+
  scale_x_continuous(expand=c(0,0))+
  coord_flip()+
  labs(y="Mean number of returns", x="Height bin")

ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/height_hist25.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

ggplot(vdiv50.summary,aes(x=bin,y=mean))+
  geom_bar(stat="identity",color="black", fill="#3b518bff")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,12500))+
  scale_x_continuous(expand=c(0,0))+
  coord_flip()+
  labs(y="Mean number of returns", x="Height bin")
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/height_hist50.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)


ggplot(vdiv250.summary,aes(x=bin,y=mean))+
  geom_bar(stat="identity",color="black", fill="black")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,250000))+
  scale_x_continuous(expand=c(0,0))+
  coord_flip() +
  labs(y="Mean number of returns", x="Height bin")
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/height_hist250.jpg", plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)


##Correlation plots
library(Hmisc)
library(reshape2)
ls10 <- lidar_summary_10[,5:9]
ls15 <- lidar_summary_15[,5:9]
ls25 <- lidar_summary_25[,5:9]
ls30 <- lidar_summary_30[,5:9]
ls50 <- lidar_summary_50[,5:9]
ls60 <- lidar_summary_60[,5:9]
ls100 <- lidar_summary_100[,5:9]
ls120 <- lidar_summary_120[,5:9]
ls250 <- lidar_summary_250[,5:9]
ls500 <- lidar_summary_500[,5:9]

cols <- c("Maximum height","Mean height","Rugosity","Vertical diversity","Canopy openness")

colnames(ls10) <- cols
colnames(ls15) <- cols
colnames(ls25) <- cols
colnames(ls30) <- cols
colnames(ls50) <- cols
colnames(ls60) <- cols
colnames(ls100) <- cols
colnames(ls120) <- cols
colnames(ls250) <- cols
colnames(ls500) <- cols

cor.ls10 <- cor(ls10, method="pearson")
cor.ls15 <- cor(ls15, method="pearson")
cor.ls25 <- cor(ls25, method="pearson")
cor.ls30 <- cor(ls30, method="pearson")
cor.ls50 <- cor(ls50, method="pearson")
cor.ls60 <- cor(ls60, method="pearson")
cor.ls100 <- cor(ls100, method="pearson")
cor.ls120 <- cor(ls120, method="pearson")
cor.ls250 <- cor(ls250, method="pearson")
cor.ls500 <- cor(ls500, method="pearson")

rcorr.ls10 <- rcorr(as.matrix(ls10), type="pearson")
rcorr.ls15 <- rcorr(as.matrix(ls15), type="pearson")
rcorr.ls25 <- rcorr(as.matrix(ls25), type="pearson")
rcorr.ls30 <- rcorr(as.matrix(ls30), type="pearson")
rcorr.ls50 <- rcorr(as.matrix(ls50), type="pearson")
rcorr.ls60 <- rcorr(as.matrix(ls60), type="pearson")
rcorr.ls100 <- rcorr(as.matrix(ls100), type="pearson")
rcorr.ls120 <- rcorr(as.matrix(ls120), type="pearson")
rcorr.ls250 <- rcorr(as.matrix(ls250), type="pearson")
rcorr.ls500 <- rcorr(as.matrix(ls500), type="pearson")

corrplot(cor.ls10, type="upper", col=cividis(250, direction = -1), tl.col = "black")
corrplot(cor.ls15, type="upper", col=cividis(250, direction = -1), tl.col = "black")
corrplot(cor.ls25, type="upper", col=cividis(250, direction = -1), tl.col = "black")
corrplot(cor.ls30, type="upper", col=cividis(250, direction = -1), tl.col = "black")
corrplot(cor.ls50, type="upper", col=cividis(250, direction = -1), tl.col = "black")
corrplot(cor.ls60, type="upper", col=cividis(250, direction = -1), tl.col = "black")
corrplot(cor.ls100, type="upper", col=cividis(250, direction = -1), tl.col = "black")
corrplot(cor.ls120, type="upper", col=cividis(250, direction = -1), tl.col = "black")
corrplot(cor.ls250, type="upper", col=cividis(250, direction = -1), tl.col = "black")
corrplot(cor.ls500, type="upper", col=cividis(250, direction = -1), tl.col = "black")

cor.ls10.melt <- melt(cor.ls10)
cor.ls15.melt <- melt(cor.ls15)
cor.ls25.melt <- melt(cor.ls25)
cor.ls30.melt <- melt(cor.ls30)
cor.ls50.melt <- melt(cor.ls50)
cor.ls60.melt <- melt(cor.ls60)
cor.ls100.melt <- melt(cor.ls100)
cor.ls120.melt <- melt(cor.ls120)
cor.ls250.melt <- melt(cor.ls250)
cor.ls500.melt <- melt(cor.ls500)

cor.ls10.melt$res <- 10
cor.ls15.melt$res <- 15
cor.ls25.melt$res <- 25
cor.ls30.melt$res <- 30
cor.ls50.melt$res <- 50
cor.ls60.melt$res <- 60
cor.ls100.melt$res <- 100
cor.ls120.melt$res <- 120
cor.ls250.melt$res <- 250
cor.ls500.melt$res <- 500

ls10.p <- cbind(cor.ls10.melt,melt(rcorr.ls10[[3]]))
ls15.p <- cbind(cor.ls15.melt,melt(rcorr.ls15[[3]]))
ls25.p <- cbind(cor.ls25.melt,melt(rcorr.ls25[[3]]))
ls30.p <- cbind(cor.ls30.melt,melt(rcorr.ls30[[3]]))
ls50.p <- cbind(cor.ls50.melt,melt(rcorr.ls50[[3]]))
ls60.p <- cbind(cor.ls60.melt,melt(rcorr.ls60[[3]]))
ls100.p <- cbind(cor.ls100.melt,melt(rcorr.ls100[[3]]))
ls120.p <- cbind(cor.ls120.melt,melt(rcorr.ls120[[3]]))
ls250.p <- cbind(cor.ls250.melt,melt(rcorr.ls250[[3]]))
ls500.p <- cbind(cor.ls500.melt,melt(rcorr.ls500[[3]]))

cor.melt <- rbind(cor.ls10.melt,cor.ls15.melt,cor.ls25.melt,cor.ls30.melt,cor.ls50.melt,cor.ls60.melt,cor.ls100.melt,cor.ls120.melt,cor.ls250.melt,cor.ls500.melt)
cor.melt.p <- rbind(ls10.p,ls15.p,ls25.p,ls30.p,ls50.p,ls60.p,ls100.p,ls120.p,ls250.p,ls500.p)

names(cor.melt.p)<-c("Var1","Var2","value","res","var3","var4","p-value")

cor.melt.p.sel <- select(cor.melt.p, 1,2,3,4,7)

cor.maxh.meanh <- cor.melt %>% filter(Var1=="Maximum height"&Var2=="Mean height")
cor.maxh.meanh$type <- 1
cor.maxh.rug <- cor.melt %>% filter(Var1=="Maximum height"&Var2=="Rugosity")
cor.maxh.rug$type <- 2
cor.maxh.vdiv <- cor.melt %>% filter(Var1=="Maximum height"&Var2=="Vertical diversity")
cor.maxh.vdiv$type <- 3
cor.maxh.open <- cor.melt %>% filter(Var1=="Maximum height"&Var2=="Canopy openness")
cor.maxh.open$type <- 4
cor.meanh.rug <- cor.melt %>% filter(Var1=="Mean height"&Var2=="Rugosity")
cor.meanh.rug$type <- 5
cor.meanh.vdiv <- cor.melt %>% filter(Var1=="Mean height"&Var2=="Vertical diversity")
cor.meanh.vdiv$type <-6
cor.meanh.open <- cor.melt %>% filter(Var1=="Mean height"&Var2=="Canopy openness")
cor.meanh.open$type <- 7
cor.rug.vdiv <- cor.melt %>% filter(Var1=="Rugosity"&Var2=="Vertical diversity")
cor.rug.vdiv$type <- 8
cor.rug.open <- cor.melt %>% filter(Var1=="Rugosity"&Var2=="Canopy openness")
cor.rug.open$type <- 9
cor.vdiv.open <- cor.melt %>% filter(Var1=="Vertical diversity"&Var2=="Canopy openness")
cor.vdiv.open$type <- 10

cor.lines <- rbind(cor.maxh.meanh,cor.maxh.rug,cor.maxh.vdiv,cor.maxh.open,cor.meanh.rug,cor.meanh.vdiv,cor.meanh.open,cor.rug.vdiv,cor.rug.open,cor.vdiv.open)
cor.lines$type<-as.factor(cor.lines$type)
ggplot(cor.lines, aes(x=res,y=value,color=type))+
  geom_line(size=1)+
  geom_point()+
  scale_color_viridis(option="cividis",discrete = T, labels=c("Maximum height vs mean height","Maximum height vs rugosity",
                                                "Maximum height vs vertical diversity", "Maximum height vs canopy openness",
                                                "Mean height vs rugosity", "Mean height vs vertical diversity",
                                                "Mean height vs canopy openness", "Rugosity vs vertical diversity",
                                                "Rugosity vs canopy openness", "Vertical diversity vs canopy openness"))

cor.maxh.meanh.p <- cor.melt.p.sel %>% filter(Var1=="Maximum height"&Var2=="Mean height")
cor.maxh.meanh.p$type <- 1
cor.maxh.rug.p <- cor.melt.p.sel %>% filter(Var1=="Maximum height"&Var2=="Rugosity")
cor.maxh.rug.p$type <- 2
cor.maxh.vdiv.p <- cor.melt.p.sel %>% filter(Var1=="Maximum height"&Var2=="Vertical diversity")
cor.maxh.vdiv.p$type <- 3
cor.maxh.open.p <- cor.melt.p.sel %>% filter(Var1=="Maximum height"&Var2=="Canopy openness")
cor.maxh.open.p$type <- 4
cor.meanh.rug.p <- cor.melt.p.sel %>% filter(Var1=="Mean height"&Var2=="Rugosity")
cor.meanh.rug.p$type <- 5
cor.meanh.vdiv.p <- cor.melt.p.sel %>% filter(Var1=="Mean height"&Var2=="Vertical diversity")
cor.meanh.vdiv.p$type <-6
cor.meanh.open.p <- cor.melt.p.sel %>% filter(Var1=="Mean height"&Var2=="Canopy openness")
cor.meanh.open.p$type <- 7
cor.rug.vdiv.p <- cor.melt.p.sel %>% filter(Var1=="Rugosity"&Var2=="Vertical diversity")
cor.rug.vdiv.p$type <- 8
cor.rug.open.p <- cor.melt.p.sel %>% filter(Var1=="Rugosity"&Var2=="Canopy openness")
cor.rug.open.p$type <- 9
cor.vdiv.open.p <- cor.melt.p.sel %>% filter(Var1=="Vertical diversity"&Var2=="Canopy openness")
cor.vdiv.open.p$type <- 10

cor.lines.p <- rbind(cor.maxh.meanh.p,cor.maxh.rug.p,cor.maxh.vdiv.p,cor.maxh.open.p,cor.meanh.rug.p,cor.meanh.vdiv.p,
                     cor.meanh.open.p,cor.rug.vdiv.p,cor.rug.open.p,cor.vdiv.open.p)
cor.lines.p$type<-as.factor(cor.lines.p$type)
cor.lines.p$plogic <- cor.lines.p$`p-value`<=0.05

ggplot(cor.lines.p, aes(x=res,y=value,color=type))+
  geom_line(aes(linetype=type),size=1)+
  geom_point(aes(shape=plogic), size = 4)+
  scale_linetype_manual(values=c("solid","dotted","solid","dotted","solid",
                                 "dotted","solid","dotted","solid","dotted"),
                        labels=c("Maxh vs meanh","Maxh vs rug",
                                 "Maxh vs vdiv", "Maxh vs open",
                                 "Meanh vs rug", "Meanh vs vdiv",
                                 "Meanh vs open", "Rug vs vdiv",
                                 "Rug vs open", "Vdiv vs open"),
                        name = "Correlations")+
  scale_color_manual(values = c("#3b518bff", "#3b518bff", "#21918dff", "#21918dff","#f5e626ff",  
                                "#f5e626ff","#8c8c8c", "#8c8c8c","#000000","#000000"), 
                     labels=c("Maxh vs meanh","Maxh vs rug",
                              "Maxh vs vdiv", "Maxh vs open",
                              "Meanh vs rug", "Meanh vs vdiv",
                              "Meanh vs open", "Rug vs vdiv",
                              "Rug vs open", "Vdiv vs open"),
                      name = "Correlations")+
  scale_shape_manual(values = c(1,16), labels = c("Not sig","Sig"), name = "p-value")+
  labs(x="Cell length (m)",y="Correlation")+
  theme_christine()
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/correlations2.jpg",plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

##Proportion histograms
vdiv10.dat <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/bzn_high_topo_6_metrics10_all_returns_strata_stats.csv")
vdiv25.dat <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/bzn_high_topo_6_metrics25_all_returns_strata_stats.csv")
vdiv50.dat <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/bzn_high_topo_6_metrics50_all_returns_strata_stats.csv")
vdiv250.dat<- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/output/bzn_high_topo_6_metrics250_all_returns_strata_stats.csv")

vdiv10.dat.filt <- vdiv10.dat[grepl("proportion",names(vdiv10.dat))]
vdiv25.dat.filt <- vdiv25.dat[grepl("proportion",names(vdiv25.dat))]
vdiv50.dat.filt <- vdiv50.dat[grepl("proportion",names(vdiv50.dat))]
vdiv250.dat.filt <- vdiv250.dat[grepl("proportion",names(vdiv250.dat))]

vdiv10.summary <- data.frame(mean=sapply(vdiv10.dat.filt, mean),sd=sapply(vdiv10.dat.filt,sd),bin=seq(1:22))
vdiv25.summary <- data.frame(mean=sapply(vdiv25.dat.filt, mean),sd=sapply(vdiv25.dat.filt,sd),bin=seq(1:22))
vdiv50.summary <- data.frame(mean=sapply(vdiv50.dat.filt, mean),sd=sapply(vdiv50.dat.filt,sd),bin=seq(1:22))
vdiv250.summary <- data.frame(mean=sapply(vdiv250.dat.filt, mean),sd=sapply(vdiv250.dat.filt,sd),bin=seq(1:22))

theme_vdiv <- function (base_size = 12, base_family = "") {
  #theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
  theme(
    ##axis properties
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks=element_blank(),
    axis.line = element_blank(),
    
    ##panel properties
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(),
    
    ##plot background
    plot.background = element_rect(fill="white"),
    
    ##legend properties
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(),
    legend.text = element_text(colour = "black", size = 16),
    legend.position = "right",
    legend.title = element_text(colour = "black", size = 16)
    
  )   
}

ggplot(vdiv10.summary, aes(x=bin, y=mean))+
  geom_bar(stat="identity", fill="#f5e626ff",color="black")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd))+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0), limits=c(0,0.35))+scale_x_continuous(expand=c(0,0))+
  labs(x="Height bin", y="Mean proportion of returns")+
  theme_christine()
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/height_hist_10.jpg",plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

ggplot(vdiv25.summary, aes(x=bin, y=mean))+
  geom_bar(stat="identity", fill="#21918dff",color="black")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd))+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0), limits=c(0,0.35))+scale_x_continuous(expand=c(0,0))+
  labs(x="Height bin", y="Mean proportion of returns")+
  theme_christine()
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/height_hist_25.jpg",plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

ggplot(vdiv50.summary, aes(x=bin,y=mean))+
  geom_bar(stat="identity", fill="#3b518bff",color="black")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd))+
  coord_flip()+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits=c(0,0.35))+
  labs(x="Height bin", y="Mean proportion of returns") +
  theme_christine()
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/height_hist_50.jpg",plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)


ggplot(vdiv250.summary,aes(x=bin,y=mean))+
  geom_bar(stat="identity",fill="black",color="black")+
  geom_errorbar(aes(ymin=mean,ymax=mean+sd))+
  coord_flip()+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits=c(0,0.35))+
  labs(x="Height bin", y="Mean proportion of returns")+
  theme_christine()
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/height_hist_250.jpg",plot = last_plot(), units = "in", width = 9, height = 6, dpi = 600)

##Zone stats density
bzs_zonestat <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/gis/bzs_zonestat.csv")
bzn_zonestat <- read.csv("D:/Dropbox/Dropbox/lidar-scaling/Data/gis/bzn_zonestat.csv")

bz_zonestat<-rbind(bzs_zonestat,bzn_zonestat) 

bz_zstat_high<-filter(bz_zonestat, relief=="high") 
bz_zstat_low<-filter(bz_zonestat, relief=="low")

mean(bz_zstat_high$X_mean)
mean(bz_zstat_low$X_mean)

mean(bz_zstat_high$X_std)
mean(bz_zstat_low$X_std)

ggplot(data = bz_zonestat, aes(x=X_std,color=relief,fill=relief))+
  geom_density(alpha=0.5)+
  scale_color_manual(values=c("#8c8c8c","#f5e626ff"), 
                     name="Topography",labels=c("High relief","Low relief"))+
  scale_fill_manual(values=c("#8c8c8c","#f5e626ff"),
                    name="Topography",labels=c("High relief","Low relief"))+
  labs(x="Standard deviation of elevation (m)",y="Density")

ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/topo_density.jpg", plot=last_plot(), units="in",width=9,height=6,dpi=600)

ggplot(data=bz_zonestat, aes(x=X_max,color=relief,fill=relief))+
  geom_density(alpha = 0.5)+
  scale_color_manual(values=c("#8c8c8c","#f5e626ff"), 
                     name="Topography",labels=c("High relief","Low relief"))+
  scale_fill_manual(values=c("#8c8c8c","#f5e626ff"),
                    name="Topography",labels=c("High relief","Low relief"))+
  labs(x="Maximum elevation (m)",y="Density")
ggsave("D:/Dropbox/Dropbox/lidar-scaling/Images/Plots_For_John/elevation_density.jpg", plot=last_plot(), units="in",width=9,height=6,dpi=600)

