## First clear your R Memory
rm(list=ls()) 

## load libraries
library(stringr)

## Set the working directory to the directory with your export files from Santiago
setwd("C:/Temp/R_Analysis/Santiago Data Analysis")

## Create an output folder for plots and save corresponding directory as variable
dir.create(file.path(getwd(), "Santiago-R-Plots"), showWarnings = FALSE)

plotdir=(file.path(getwd(),"Santiago-R-Plots"))

## read data 
## read in allSys
props <- read.csv("test/TEST_mysystems_all_R-Export_exampleFiles.csv", TRUE, ",")

## read in selectedSystems
selectedSystems <- read.csv("test/TEST_mysystems_selected_R-Export_exampleFiles.csv", TRUE, ",")

## Add column "selected" with boolean expression checking if system is in selectedSystems
props$selected <- props$ID %in% selectedSystems$ID


## Calculate "accumulated" Recovery and Balance Result
props <- within(props, recovery_ratio_accumulated_balanced_mean <- (recovery_ratio_phosphor_mean + recovery_ratio_nitrogen_mean +
                  recovery_ratio_water_mean + recovery_ratio_totalsolids_mean)/4)
