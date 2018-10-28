#!/bin/bash
#SBATCH --job-name=lidar_scaling2
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=christineswanson@ufl.edu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=20gb
#SBATCH --time=10:00:00
#SBATCH --output=lidar_scaling2_%j.log
pwd; hostname; date

module load R/3.4.3
Rscript /ufrc/bohlman//christineswanson/lidar-scaling/code/lidar_spdep2.R
