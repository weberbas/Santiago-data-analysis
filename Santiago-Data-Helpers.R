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
props <- readRDS(file.path(rundir, paste(runname, "props.Rdata", sep = "_")))
tas_components_df <- readRDS(file=(file.path(rundir, paste(runname, "tas_props.Rdata", sep = "_"))))
tas_components_df_long <- readRDS(file=(file.path(rundir, paste(runname, "tas_long_props.Rdata", sep = "_"))))

## Create an output folder for plots and save corresponding directory as variable
dir.create(file.path(rundir, "Santiago-R-Plots"), showWarnings = FALSE)
plotdir=(file.path(rundir,"Santiago-R-Plots"))

##################################################################
# # # colours and names

## define source colors randomly
source_cols <- levels(props$source)
for (i in 1:length(source_cols)) {
  source_cols[i]<-randomColor()
}
remove(i)
## define source names
source_labs=levels(props$source)
source_names=levels(props$source)

## define template colors with randomly assigned colors
#template_cols <- levels(props$template)
#for (i in 1:length(template_cols)) {
#    template_cols[i]<-randomColor()
#}
#remove(i)

## define template colors manually
template_cols <- c("#25AF87", "#36C196", "#63D6B8", "#EDE720",
              "#F9F04E", "#F9F0A0", "#E83470", "#F74B68",
              "#F7578C", "#F979A3", "#084A99", "#0555A5",
              "#1561BC", "#306CC4", "#467ECE", "#6B98D8",
              "#90B2E2")

# define template names
template_names=levels(props$template)
template_names_short=c("ST1","ST2","ST3","ST4","ST5","ST6","ST9","ST10","ST11","ST12",
                                      "ST13","ST14","ST15","ST16","ST17","ST18","ST19")

## define functional group colour manually
fgcol=c(U="#BF0D0D", C="#FFD400", D="#00AECA", S="#F29100", T="#76B742")


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




