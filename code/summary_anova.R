##ANOVA tests for LiDAR scaling project
##Written by Christine Swanson
##July 18, 2018

#load in library
library(dplyr)
library(car)

#read in summary data
lidar_summary <- read.csv("./Data/summary_data/lidar_summary_data_3.csv")

#Mean and SD of mean canopy height and canopy openness
can.mean <- mean(lidar_summary$mean_height)
can.sd <- sd(lidar_summary$mean_height)

open.mean <- mean(lidar_summary$openness)
open.sd <- sd(lidar_summary$openness)

#set up model names
maxh_res <- lm(lidar_summary$max_height~lidar_summary$resolution)
meanh_res <- lm(lidar_summary$mean_height~lidar_summary$resolution)
rug_res <- lm(lidar_summary$rugosity~lidar_summary$resolution)
vert_div_res <- lm(lidar_summary$vertical_diversity~lidar_summary$resolution)
open_res <- lm(lidar_summary$openness~lidar_summary$resolution)

maxh_topo <- lm(lidar_summary$max_height~lidar_summary$topo)
meanh_topo <- lm(lidar_summary$mean_height~lidar_summary$topo)
rug_topo <- lm(lidar_summary$rugosity~lidar_summary$topo)
vert_div_topo <- lm(lidar_summary$vertical_diversity~lidar_summary$topo)
open_topo <- lm( lidar_summary$openness~lidar_summary$topo)

#Bartlett's test
maxh_res_bart <- bartlett.test(max_height~resolution, lidar_summary) #Significant
meanh_res_bart <- bartlett.test(mean_height~resolution, lidar_summary)
rug_res_bart <- bartlett.test(rugosity~resolution, lidar_summary)
vert_div_res_bart <- bartlett.test(vertical_diversity~resolution, lidar_summary) #Significant
open_res_bart <- bartlett.test(openness~resolution, lidar_summary)

maxh_topo_bart <- bartlett.test(max_height~topo, lidar_summary)
meanh_topo_bart <- bartlett.test(mean_height~topo, lidar_summary) #Significant
rug_topo_bart <- bartlett.test(rugosity~topo, lidar_summary)
vert_div_topo_bart <- bartlett.test(vertical_diversity~topo, lidar_summary) #Significant
open_topo_bart <- bartlett.test(openness~topo, lidar_summary)

#full anova
maxh_res_anova <- Anova(maxh_res, Type = "II", white.adjust=T)
meanh_res_anova <- anova(meanh_res)
rug_res_anova <- anova(rug_res)
vert_div_res_anova <- Anova(vert_div_res, Type = "II", white.adjust = T)
open_res_anova <- anova(open_res)

#topography anova
maxh_topo_anova <- anova(maxh_topo)
meanh_topo_anova <- Anova(meanh_topo, Type = "II", white.adjust = T)
rug_topo_anova <- anova(rug_topo)
vert_div_topo_anova <- Anova(vert_div_topo, Type = "II", white.adjust = T)
open_topo_anova <- anova(open_topo)

#anova summaries
maxh_topo_anova #significant
meanh_topo_anova #significant
rug_topo_anova #marginally significant
vert_div_topo_anova
open_topo_anova

#split data into high and low relief
high_topo <- lidar_summary %>% filter(topo == "high")
low_topo <- lidar_summary %>% filter(topo == "low")

#anova for variables where topography was significant split by relief
maxh_res_high_anova <- anova(lm(high_topo$max_height~high_topo$resolution))
meanh_res_high_anova <- Anova(lm(log(high_topo$mean_height)~high_topo$resolution), Type = "II", white.adjust = T)
rug_res_high_anova <- anova(lm(high_topo$rugosity~high_topo$resolution))

maxh_res_low_anova <- anova(lm(low_topo$max_height~low_topo$resolution))
meanh_res_low_anova <- Anova(lm(low_topo$mean_height~low_topo$resolution), Type = "II", white.adjust = T)
rug_res_low_anova <- anova(lm(low_topo$rugosity~low_topo$resolution))

maxh_res_topo_anova <- anova(lm(lidar_summary$max_height~lidar_summary$topo+lidar_summary$resolution))
meanh_res_topo_anova <- anova(lm(lidar_summary$mean_height~lidar_summary$topo+lidar_summary$resolution))
rug_res_topo_anova <- anova(lm(lidar_summary$rugosity~lidar_summary$topo+lidar_summary$resolution))
#anova summaries
maxh_res_high_anova #Significant
meanh_res_high_anova
rug_res_high_anova #Significant
maxh_res_low_anova #Significant
meanh_res_low_anova 
rug_res_low_anova #Significant
maxh_res_anova #Significant
meanh_res_anova
rug_res_anova #Significant
vert_div_res_anova #Significant
open_res_anova

