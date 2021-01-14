# Santiago Data Analysis with R

Welcome to the _Santiago_ Data Analysis _R_-Package version 13.01.2021.

## General  
In this repository you will find a number of scripts that can be used in combination with the newest version of [_Santiago_](https://github.com/santiago-sanitation-systems/Santiago.jl).
The aim is to provide basic support for the analysis of _Santiago_ outputs in R using ggplot2. 
We are conitnousely updating this repository. thus, please download the newest versino frequently and contact us in case of any problem encountered.

## Content
This repository contains three _R_ Scripts:  
* _Santiago-Data-Prep.R_ --> This script helps you to load the data outputs from _Santiago_ and to convert it into a a dataframe for easy handling in _R_. You only need to do this once for new data or when the outputs from your _Santiago_ run have been updated. After using this script. The dataframew are stored as RData and can be loaded directly.
* _Santiago-Data-Helpers.R_ --> This scripts contains helper functions and variables (colours, labels etc.) for plotting. It is called from the third script.
* _Santiago-Data-Plots.R_ --> This script contains the code for the different plots.

## Available Plots 
### 1 - Plotting Technology Appropriateness Scores (TAS)
* p1.1 --> TAS - Boxplot of TAS and criteria scores of all technologies grouped by functional group
* p1.2 --> TAS - Overview on all TAS
* p1.3 --> TAS - Detailes on TAS and criteria scores for all technologies
### 2 - Plotting System Appropriateness Scores (SAS)
* p2.1 --> SAS - Plot of the sytem appropritenss versus the resource recovery potentials and selected systems
* p2.2 --> SAS - Overview of all system appropriatenss scores and selected systems grouped by templates
### 3 - Plotting Resource Recovery (RR) potentials
* p3.1 --> RR - A Density plot for recovery ratios for all four substances: total phosphosrus (TP), total nitrogen (TN), total solids (TS), and water (H2O)
* p3.2 --> RR - A Density plot for recovery ratios, grouped by source
* p3.3 --> RR - A scatter and boxplot per template, colored by source.
* p3.4 --> RR and losses - Boxplot Grouped by Source 
* p3.5 --> RR - Boxplots for length of systems for all substance, grouped by system templates
* p3.6 --> RR - Boxplot for length of systems against accumulated recovery coloured by system template
* p3.7 --> RR and uncertainties - Standard deviation of recovery potentials against the recovery potentials coloured by system template

## Usage:
### Installation
Create on your computer a folder called __santiago-sanitation-systems__. This is ideally also the location of your Santiago-runfolder where you run the _Santiago_ runfile and package and store your _Santiago- input and output data (see below for the recommended folder structure).

### Input data
First of all, you need the required _Santiago_ outputs. These are two csv files and two json files:
* (runName)_allSys_R-Export.csv
* (runName)_selectedSys_R-Export.cs
* (runName)_TAS_R-Export.json
* (runName)_TAS_Components_R-Export.json
How to define the runName and to export these files is explained in the best practice runfile in the [Santiago Wiki](https://github.com/santiago-sanitation-systems/Santiago.jl/wiki).

### Scripts
Then, you need to have all the scripts and path on your computer set correctely. We have defined the script, that they should work without mayor changes if you have the following folder structure somewhere on your computer:
* __santiago-sanitation-systems__ (_this folder you create yourself somewhere_)
 * __Santiago-data-analysis__ (this is the folder you downloaded from github)
   * Santiago-Data-Helpers.R
   * Santiago-Data-Plots.R
   * Santiago-Data-Prep.R
 * __Santiago-runfolder__ (_this folder you have created when you started to work with Santiago_)
   * 3.1-Best-Practice-Runfile.jl (-this file you have downloaded from the wiki-)
   * input (_generated when you run the runfile_)
   * Manifest.toml (_generated when you run the runfile_)
   * output (_generated when you run the runfile_)
     * test (_generated when you run the runfile_)
   * Project.toml (_generated when you run the runfile_)

### Using the data and the scripts
The usage is then as follows:

1) Run once the code in _Santiago-Data-Prep.R_ and store the resulting dataframes as RData files in your runfolder. You only need to do this if you use the data for the first time or if your _Santiago_ output data have been changed (e.g. changes in appropriatness scores). If you data remain the same, you will whenever you start a R session just load the saved Rfiles using the helper script.

2) Use _Santiago-Data-Plots.R_. This file automatically calls the helper file.  The plots are calculated and stored as a variable (e.g. "p3.3x") and in a later step exported as PDF also in the runfolder. Use `view(p.3.3x)` to view the plot in your Editor. 


Have Fun!