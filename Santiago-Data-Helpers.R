# # Helpers for Plots


## Define Source Reference
source_labs <- c("Cistern.flush" = "Cistern flush", "Pour.flush" = "Pour flush", 
                 "UDDT"="UDDT", "Dry.toilet"="Dry toilet")
## Define Source colors
source_cols <- c("Cistern.flush" = "#306CC4", "Pour.flush" = "#66CCFF", 
                 "Dry.toilet"="#36C196",  "UDDT"="#EDE720")

## Define Template Colors with randomly assigned colors
template_cols <- unique(props$template)

for (i in 1:length(template_cols)) {
    template_cols[i]<-randomColor()
}

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




