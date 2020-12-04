# Santiago Data Analysis with R

Welcome to the _Santiago_ Data Analysis _R_-Package.

## General  

This Repository provides a number of plots that can be used with the outputs of _Santiago_ and is updated ongoing. There are three _R_ Scripts:  

* _Santiago-Data-Prep.R_ --> Reads in CSVs and prepares Data Frames
* _Santiago-Data-Helpers.R_ --> Helper functions and variables (colours, labels etc.)
* _Santiago-Data-Plots.R_ --> The actual plots that can be used after running the two scripts above

## Available Plots (2020/Dec/04)
* p3.2 --> Recovery Potentials - A scatter and boxplot per template, colored by source.

## Usage:
### Preparation - Export CSV Files in _Santiago_:
In order to use the _R_-Scripts we need to output files from _Santiago_. A Massflow-Export.CSV from all Systems generated and from the selected Systems (Not used yet). To export the massflow statistics of all Systems and save them as CSV, use the following code:

```Julia
df=properties_dataframe(allSys, 
                        massflow_selection = ["entered | phosphor",
                                                "entered | nitrogen",
                                                "entered | water",
                                                "entered | totalsolids",
                                                "recovered | phosphor | mean",
                                                "recovered | nitrogen | mean",
                                                "recovered | water | mean",
                                                "recovered | totalsolids | mean",
                                                "recovered | phosphor | sd",
                                                "recovered | nitrogen | sd",
                                                "recovered | water | sd",
                                                "recovered | totalsolids | sd",
                                                "recovery_ratio | phosphor | mean",
                                                "recovery_ratio | nitrogen | mean",
                                                "recovery_ratio | water | mean",
                                                "recovery_ratio | totalsolids | mean",
                                                "recovery_ratio | phosphor | sd",
                                                "recovery_ratio | nitrogen | sd",
                                                "recovery_ratio | water | sd",
                                                "recovery_ratio | totalsolids | sd",
                                                "lost | phosphor | air loss | mean",
                                                "lost | nitrogen | air loss | mean",
                                                "lost | water | air loss | mean",
                                                "lost | totalsolids | air loss | mean",
                                                "lost | phosphor | soil loss | mean",
                                                "lost | nitrogen | soil loss | mean",
                                                "lost | water | soil loss | mean",
                                                "lost | totalsolids | soil loss | mean",
                                                "lost | phosphor | water loss | mean",
                                                "lost | nitrogen | water loss | mean",
                                                "lost | water | water loss | mean",
                                                "lost | totalsolids | water loss | mean",
                                                "lost | phosphor | air loss | sd",
                                                "lost | nitrogen | air loss | sd",
                                                "lost | water | air loss | sd",
                                                "lost | totalsolids | air loss | sd",
                                                "lost | phosphor | soil loss | sd",
                                                "lost | nitrogen | soil loss | sd",
                                                "lost | water | soil loss | sd",
                                                "lost | totalsolids | soil loss | sd",
                                                "lost | phosphor | water loss | sd",
                                                "lost | nitrogen | water loss | sd",
                                                "lost | water | water loss | sd",
                                                "lost | totalsolids | water loss | sd"])


import CSV  # the package 'CSV' needs to be installed separately: ] add CSV
CSV.write(joinpath(outputFolder, "$(runName)_allSys_R-Export.csv"), df)
```
Keep in mind, that `outputFolder` and `runName` are defined in previous steps of Santiago, alternatively you can just specify any File name with:  
`CSV.write("My_Name_For_Export_allSys.csv", df)`

The CSV of selected Systems is only used to compare IDs of selected Systems with IDs of all systems and add a column with a boolean=true if this system is a selected System or boolean=false if this system is not one of the selected Systems. We do not need to export the massflow statistics of selected Systems.

Therefore we use:
```Julia
df=properties_dataframe(selectedSys)

import CSV  # the package 'CSV' needs to be installed separately: ] add CSV
CSV.write(joinpath(outputFolder, "$(runName)_selectedSys_R-Export.csv"), df)
```

### Read in Data - Santiago-Data-Prep.R
Make sure you define your directories and file paths right in the Santiago-Data-Prep Script:
```R
## Set the working directory to the directory with your export files from Santiago
setwd("C:/Temp/R_Analysis/R_Files_Modified")

## Create an output folder for plots and save corresponding directory as variable
dir.create(file.path(getwd(), "Santiago-R-Plots"))

plotdir=(file.path(getwd(),"Santiago-R-Plots"))

## read data 
## read in allSys as `props`
props <- read.csv("..._allSys_R-Export.csv", TRUE, ",")

## read in selectedSystems
selectedSystems <- read.csv("...selectedSys_R-Export.csv", TRUE, ",")
```
After this, you should be able to run both of the scripts _Santiago-Data-Prep.R_ and _Santiago-Data-Helpers.R_ (the scripts are also called in the script _Santiago-Data-Plots.R_).

### Plot Data
Once you prepared your data, you can go ahead and run the preimplemented plots in _Santiago-Data-Plots.R_. The plots are calculated and stored as a variable (e.g. "p3.2x") and in a later step exported as PDF. Use `view(p.3.2x)` to view the plot in your Editor. 


This Repository is in the making and constantly updated (check dates for latest version: _Santiago-Data-Preps-YYMMDD.R_)

Have Fun!