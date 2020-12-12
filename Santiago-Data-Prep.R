## First clear your R Memory
rm(list=ls()) 

## load libraries
library(stringr)
library(rjson)
library(dplyr)
library(reshape2)


## Set the working directory to the directory with your export files from Santiago
setwd("C:/Temp/R_Analysis/Santiago Data Analysis")

## Create an output folder for plots and save corresponding directory as variable
dir.create(file.path(getwd(), "Santiago-R-Plots"), showWarnings = FALSE)

plotdir=(file.path(getwd(),"Santiago-R-Plots"))

## read data 
  ## read in allSys
  props <- read.csv("test/FullTest_allSys_selected_R-Export.csv", TRUE, ",")
  
  ## read in selectedSystems
  selectedSystems <- read.csv("test/FullTest_selectedSys_selected_R-Export.csv", TRUE, ",")
  
  ## read in and convert TAS
  tas_df <- t(as.data.frame(fromJSON(file = "test/FullTest_TAS_R-Export.json")))
  colnames(tas_df) <- "TAS"

  ## read in and convert TAS Components
  tas_components_list <- fromJSON(file = "test/FullTest_TAS_Components_R-Export.json")
  melted_tas_comp <- melt(tas_components_list)
  colnames(melted_tas_comp) <- c("value", "attribute", "tech")
  tas_components_df <- dcast(melted_tas_comp, tech ~ attribute)
  remove(tas_components_list, melted_tas_comp)
  
  # read out functional group and store them as column
  FG <- toupper(substr(tas_components_df[,"tech"], start = 1, stop = 1))
  tas_components_df <- cbind(tas_components_df, FG)
  remove(FG)
  
  ## Merge Tas and Tas Components
  tas_components_df <- merge(tas_components_df, tas_df, by.x = "tech", by.y = 0)
  
  #create long data frame
  tas_components_df_long = melt(tas_components_df, id=c("tech", "FG"))
  
  
## Add column "selected" with boolean expression checking if system is in selectedSystems
props$selected <- props$ID %in% selectedSystems$ID


## Calculate "accumulated" Recovery and Balance Result
props <- within(props, recovery_ratio_accumulated_balanced_mean <- (recovery_ratio_phosphor_mean + recovery_ratio_nitrogen_mean +
                  recovery_ratio_water_mean + recovery_ratio_totalsolids_mean)/4)

