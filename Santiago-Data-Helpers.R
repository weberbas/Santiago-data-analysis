# # Helpers for Plots

## Load necessary Libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(gridExtra)
library(grid)
library(reshape2)
library(doBy)
library(randomcoloR)
library(ggrepel)

## Load Props.RData file and TAS files calculated with "Santiago-Data-Prep.R"
props <- readRDS(file.path(getwd(), "output", runname, paste(runname, "props.Rdata", sep = "_")))
tas_components_df <- readRDS(file=(file.path(getwd(), "output", runname, paste(runname, "tas_props.Rdata", sep = "_"))))
tas_components_df_long <- readRDS(file=(file.path(getwd(), "output", runname, paste(runname, "tas_long_props.Rdata", sep = "_"))))


## Create an output folder for plots and save corresponding directory as variable
dir.create(file.path(getwd(), "Santiago-R-Plots"), showWarnings = FALSE)

plotdir=(file.path(getwd(),"Santiago-R-Plots"))


## Define Source Reference
source_cols <- source_labs <- subset(tas_components_df, FG == "U", select = "tech")


## Define Source colors
for (i in 1:nrow(source_cols)) {
  source_cols[i,]<-randomColor()
}
remove(i)

source_cols <- source_cols$tech

## Define Template Colors with randomly assigned colors
template_cols <- template_names <- unique(props$template)

for (i in 1:length(template_cols)) {
    template_cols[i]<-randomColor()
}
remove(i)

## Factor FG from long tas_components dataframe for functional group colors
fgcols <- as.factor(tas_components_df$FG)

## Factor Techs from long tas_components dataframe for functional group colors
techcols <- as.factor(tas_components_df$tech)

## Function for multiple plots with shared ledgend ------------------
  grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}




