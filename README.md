# Scaling lidar-derived rainforest canopy metrics across a Mesoamerican landscape

This file is a guide to the code included in the Github repository.

There are three folders containing code: bat-files, SLURM, and code.

Within bat-files, you can find files used to process lidar data in the US Forest Service's FUSION program. This was used to calculate vertical diversity metrics and ultimately create the vertical diversity histograms shown in the manuscript.

Within the SLURM folder, you will find SLURM for running the Moran's I code in a supercomputer.

The code folder contains all of the lidar analyses done in R.


* Start with the lidar_scaling.R file. This file allows you to run the initial scaling code.
* The summary_csv.R file allows you to summarize all of the lidar_scaling.R output into a single .csv file.
* lidar_morans.R runs all of the Moran's I analyses.
* summary_anova contains the code to do the significance testing.
* summary_plots.R has all of the plotting code, except for the Moran's I plots. Those can be created with the moran_plots.R code. Also, summary_plots.R contains the code to run the correlation analyses and make the corresponding plots.
* las_norm.R is not required to run. It was used to normalize the las files stored in the data repository. This same code can be found at the beginning of the lidar_scaling.R file.
* lidar_plot.R is not required either. It allows you to visualize each lidar tile.
* lidar_slope_calc.R calculates slope and doing the regression analyses related to slope

## Important - lidar_scaling.r

If data was downloaded from the accompanying repository, you must skip all of the normalizing steps in the for loop. The data in the repository have already been normalized.
 
**Remove lines 25-31 and rename "lidar_in" to "las_norm" in line 24** If you do not do this, the code may return incorrect data from the lidar.
**As of 2/21/19, lidR has been updated, and the code is no longer compatible with the new version. Some changes must be made to get the code to run with the newest version of this package.**

Please direct any questions about the code to christineswanson@ufl.edu or @thatsciencegal on Twitter.
