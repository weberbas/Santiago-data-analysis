## First clear your R Memory
rm(list=ls()) 

## load libraries
library(stringr)
library(rjson)
library(dplyr)
library(reshape2)
library(reshape)

## Set the working directory to the current directory
sourcedir=dirname(rstudioapi::getSourceEditorContext()$path)
setwd=sourcedir

## Insert the runname you used for Santiago
runname = "test"
### define the path to your output folder from this run
rundir<-file.path("../Santiago-runfolder/output", runname)

## read data 
  ## read in allSys
  props <- read.csv(file.path(rundir, paste(runname, "_properties_allSys.csv", sep = "")), sep=",", header=T)
  
  ## read in selectedSystems
  selectedSystems <- read.csv(file.path(rundir, paste(runname, "_selectedSys.csv", sep = "")), sep=",", header=T)
  
  ## read in and convert TAS
  tas_df <- t(as.data.frame(fromJSON(file = file.path(rundir, paste(runname, "_TAS.json", sep = "")))))
  colnames(tas_df) <- "TAS"

  ## read in and convert TAS Components
  tas_components_list <- fromJSON(file = file.path(rundir, paste(runname, "_TAS_Components.json", sep = "")))
  melted_tas_comp <- melt(tas_components_list)
  colnames(melted_tas_comp) <- c("value", "attribute", "tech")
  tas_components_df <- dcast(melted_tas_comp, tech ~ attribute)
  remove(tas_components_list, melted_tas_comp)
  
  # read out functional group and store them as column
  FG <- toupper(substr(tas_components_df[,"tech"], start = 1, stop = 1))
  tas_components_df <- cbind(tas_components_df, FG)
  remove(FG)
  
  ## Merge TAS and TAS Components
  tas_components_df <- merge(tas_components_df, tas_df, by.x = "tech", by.y = 0)
  
  #create long data frame
  tas_components_df_long = melt(tas_components_df, id=c("tech", "FG"))
  
  
## add column "selected" with boolean expression checking if system is in selectedSystems
props$selected <- props$ID %in% selectedSystems$ID


## factor sources for setting order
props$source <- factor(props$source, levels=str_sort(unique(props$source), numeric = TRUE))

### factor template names for setting order
template_c2=str_sort(unique(props$template), numeric = TRUE)
props$template <- factor(props$template,levels=template_c2)
## rename templates with line breaks
for(i in levels(props$template)){
  levels(props$template)[levels(props$template)==i] <- paste(strwrap(i, width=27), collapse=" \n")
}

## make templates short names
temp_nb=c("ST1","ST2","ST3", "ST4", "ST5","ST6","ST9","ST10","ST11", "ST12","ST13","ST14","ST15", "ST16","ST17","ST18","ST19")
props$template_nb <- props$template
for(i in 1:17){
  levels(props$template_nb)[levels(props$template_nb2)==template_c2[i]] <- temp_nb[i]
}
props$template_nb <- factor(props$template_nb,levels=temp_nb)


## add number of SanSys within a each template
n_in_template=NULL
for (template in levels(props$template)) {
  number.of.sys.in.template <- sum(props$template == template, na.rm=TRUE)
  n_in_template = rbind(n_in_template, data.frame(template, number.of.sys.in.template))
}
props$number_in_temp<-NA
for (tem in n_in_template$template) {
  props$number_in_temp[props$template==tem] <- n_in_template$number.of.sys.in.template[n_in_template$template==tem]
}


## calculate "accumulated recoveries"
props <- within(props, recovery_ratio_accumulated_mean <- (recovery_ratio_phosphor_mean + recovery_ratio_nitrogen_mean +
                                                             recovery_ratio_water_mean + recovery_ratio_totalsolids_mean))
props <- within(props, recovery_ratio_accumulated_balanced_mean <- (recovery_ratio_phosphor_mean + recovery_ratio_nitrogen_mean +
                  recovery_ratio_water_mean + recovery_ratio_totalsolids_mean)/4)

props <- within(props, recovery_ratio_accumulated_sd <- (recovery_ratio_phosphor_sd + recovery_ratio_nitrogen_sd +
                                                             recovery_ratio_water_sd + recovery_ratio_totalsolids_sd))
props <- within(props, recovery_ratio_accumulated_balanced_sd <- (recovery_ratio_phosphor_sd + recovery_ratio_nitrogen_sd +
                                                                      recovery_ratio_water_sd + recovery_ratio_totalsolids_sd)/4)

## calculate "accumulated losses"
props <- within(props, phosphor_losses_ratio_accumulated_mean <- lost_phosphor_air.loss_mean + lost_phosphor_soil.loss_mean + lost_phosphor_water.loss_mean)
props <- within(props, nitrogen_losses_ratio_accumulated_mean <- lost_nitrogen_air.loss_mean + lost_nitrogen_soil.loss_mean + lost_nitrogen_water.loss_mean)
props <- within(props, totalsolids_losses_ratio_accumulated_mean <- lost_totalsolids_air.loss_mean + lost_totalsolids_soil.loss_mean + lost_totalsolids_water.loss_mean)
props <- within(props, water_losses_ratio_accumulated_mean <- lost_water_air.loss_mean + lost_water_soil.loss_mean + lost_water_water.loss_mean)

## transform liters to m3
props$entered_water <- props$entered_water/1000
props$recovered_water_mean <- props$recovered_water_mean/1000
props$recovered_water_sd <- props$recovered_water_sd/1000
props$lost_water_air.loss_mean <- props$lost_water_air.loss_mean/1000
props$lost_water_air.loss_sd <- props$lost_water_air.loss_sd/1000
props$lost_water_soil.loss_mean <- props$lost_water_soil.loss_mean/1000
props$lost_water_soil.loss_sd <- props$lost_water_soil.loss_sd/1000
props$lost_water_water.loss_mean <- props$lost_water_water.loss_mean/1000
props$lost_water_water.loss_sd <- props$lost_water_water.loss_sd/1000


## save Props.RData and TAS.RData for future calculations
saveRDS(props, file=(file.path(rundir, paste(runname, "props.Rdata", sep = "_"))))
saveRDS(tas_components_df, file=(file.path(rundir, paste(runname, "tas_props.Rdata", sep = "_"))))
saveRDS(tas_components_df_long, file=(file.path(rundir, paste(runname, "tas_long_props.Rdata", sep = "_"))))

