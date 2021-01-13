# Santiago Data Analysis with R

Welcome to the _Santiago_ Data Analysis _R_-Package.

## General  

This Repository provides a number of plots that can be used with the outputs of _Santiago_ and is updated ongoing. There are three _R_ Scripts:  

* _Santiago-Data-Prep.R_ --> Reads in CSVs and prepares Data Frames
* _Santiago-Data-Helpers.R_ --> Helper functions and variables (colours, labels etc.)
* _Santiago-Data-Plots.R_ --> The actual plots that can be used after running the two scripts above

### Folder Structure
Wheter you download the R scripts manually or clone them as a git clone, you should save the scripts in a new folder within your folder that you created for Santiago ("Santiago-Sanitation-Systems" not "Santiago.jl"). The folder structure should look as follows:  
_"Santiago-Sanitation-Systems" > "Santiago.jl"_: this is where your Best Practice Runfile.jl is stored and the input/output folder created. 
_"Santiago-Sanitation-Systems" > "Santiago-data-analysis"_: here you should save the R scripts.

By doing so, the script _Santiago-Data-Prep.R_ should automatically find your export files that you calculated with _Santiago.jl_ (keep in mind to adapt the _runName_ to the runName that you used in Santiago). This is very practical if you calculated sanitation alternatives for several different regions (meaning you used a different run name for each of the calculations, e.g. "Woze_mc150" and "Mehalketema_mc150"). By changing the runName in the R script (either "Woze_mc150" or "Mehalketema_mc150"), you can define which set of export files should be analyzed and plotted. 

## Available Plots 
### 1 - Technology Appropriateness Assessment
* p1.1 --> TAS - Plot Appropriateness Profiles per Functional Group
* p1.2 --> TAS - All Technology Appropriatenes Scores
* p1.3 --> TAS - Plot Appropriateness Profiles per Technology 
### 2 - System Appropriateness Assessment
* p2.1 --> Recovery Potentials - SAS, Substance Recoveries and Selected Systems
* p2.2 --> Recovery Potentials - SAS Boxplot per Template colored by Source
### 3 - Resource Recovery
* p3.1 --> Recovery Potentials - A Density plot for recovery ratios
* p3.2 --> Recovery Potentials - A Density plot for recovery ratios, grouped by source
* p3.3 --> Recovery Potentials - A scatter and boxplot per template, colored by source.
* p3.4 --> Recovery Potentials and Losses - Boxplot Grouped by Source 
* p3.5 --> Recovery Potentials - Boxplots for Number of Technologies for every Substance,               grouped by System Templates
* p3.6 --> Recovery Potentials - Boxplot for Number of Technologies for accumulated                   balanced Recovery, grouped by System Templates
* p3.7 --> Standard Deviation against Recovery colored by System Template

## Usage:
### Preparation - Export CSV Files in _Santiago_:
In order to use the _R_-Scripts we need to output files from _Santiago_. A Massflow-Export.CSV from all Systems generated and from the selected Systems. To export the massflow statistics of all Systems and save them as CSV, use the following code:

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
However, if you follow the folder structure and runName structure provided by us, it should not be necessary to specify any file path for R studio, only the _runName_ should be changed accordingly and the R scripts should find your export files from Santiago. 

The CSV of selected Systems is only used to compare IDs of selected Systems with IDs of all systems and add a column with a boolean=true if this system is a selected System or boolean=false if this system is not one of the selected Systems. We do not need to export the massflow statistics of selected Systems.

Therefore we use:
```Julia
df=properties_dataframe(selectedSys)

import CSV  # the package 'CSV' needs to be installed separately: ] add CSV
CSV.write(joinpath(outputFolder, "$(runName)_selectedSys_R-Export.csv"), df)
```

Additionally we read out the TAS and its Components as follows:
```Julia
# Calculate TAS and TAS Components

tas, tas_components = appropriateness(input_tech_file, input_case_file);

# Export TAS for R Analysis

open(joinpath(outputFolder, "$(runName)_TAS_R-Export.json"), "w") do f
    JSON3.write(f, tas)
end

# Export TAS_Components for R Analysis

open(joinpath(outputFolder, "$(runName)_TAS_Components_R-Export.json"), "w") do f
    JSON3.write(f, tas_components)
end

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
After this, you can run the script _Santiago-Data-Prep.R_ that will prepare your data from Santiago and save a .Rdata file in your _Santiago.jl/output/runName"_ folder. This way the data does not have to be prepared every time you open up RStudio and this can prevent conversion errors. 
After that, you can go ahead an run the _Santiago-Data-Plots.R_ script that will call the helpers script (_source(Santiago-Data-Helpers.R)_). Each plot can then be executed individually.

### Plot Data
Once you prepared your data, you can go ahead and run the preimplemented plots in _Santiago-Data-Plots.R_. The plots are calculated and stored as a variable (e.g. "p3.3x") and in a later step exported as PDF. Use `view(p.3.3x)` to view the plot in your Editor. 


This Repository is in the making and constantly updated (check for latest version)

Have Fun!