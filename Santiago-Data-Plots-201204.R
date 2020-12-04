rm(list=ls()) 

## Load necessary Libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(gridExtra)

source("Santiago-Data-Prep-JF.R")
source("Santiago-Data-Helpers-JF.R")


# # ---- 3.2 Boxplot per template coloured by source - Recovery Potentials ----
# 
## General Labels:
xtxt3.2 <- expression(paste("System template"))
labstxt3.2 <- expression(paste("Source"))
# 
## Preparation: Calculate Median of recovery ratios per template
dataMedian_template_p <- summarise(group_by(props, template), MD = round(median(100*recovery_ratio_phosphor_mean),2))
dataMedian_template_n <- summarise(group_by(props, template), MD = round(median(100*recovery_ratio_nitrogen_mean),2))
dataMedian_template_ts <- summarise(group_by(props, template), MD = round(median(100*recovery_ratio_totalsolids_mean),2))
dataMedian_template_h2o <- summarise(group_by(props, template), MD = round(median(100*recovery_ratio_water_mean),2))
dataMedian_template_h2om <- summarise(group_by(props, template), MD = round(median(recovered_water_mean),2))
dataMedian_template_acc <- summarise(group_by(props, template), MD = round(median(100*recovery_ratio_accumulated_balanced_mean),2))
# 

  ## ---- Phosphor Recovery Ratio per Template ----
  
  p3.2p <- ggplot(data=props, aes(x=template, y=100*recovery_ratio_phosphor_mean))+
            geom_point(aes(color=source), alpha=0.5, size=2, position = position_jitter())+
            scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.2)+
            geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
            geom_text(data = dataMedian_template_p, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
            labs(x=xtxt3.2, y= "Ratio [%]") +
            theme_minimal()+
            guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
            scale_x_discrete(labels = wrap_format(10))+
            theme(axis.text.x = element_text(size=8),
                  legend.position = "right")+
            ggtitle("Phosphorus Recovery")
  
  ## ---- Nitrogen Recovery Ratio per Template ----
  
  p3.2n <- ggplot(data=props, aes(x=template, y=100*recovery_ratio_nitrogen_mean))+
            geom_point(aes(color=source), alpha=0.5, size=2, position = position_jitter())+
            scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.2)+
            geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
            geom_text(data = dataMedian_template_n, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
            labs(x=xtxt3.2, y= "Ratio [%]") +
            theme_minimal()+
            guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
            scale_x_discrete(labels = wrap_format(10))+
            theme(axis.text.x = element_text(size=8),
                  legend.position = "right")+
            ggtitle("Nitrogen Recovery")
  
  ## ---- Total Solids Recovery Ratio per Template ---- 
  
  p3.2ts <- ggplot(data=props, aes(x=template, y=100*recovery_ratio_totalsolids_mean))+
            geom_point(aes(color=source), alpha=0.5, size=2, position = position_jitter())+
            scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.2)+
            geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
            geom_text(data = dataMedian_template_ts, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
            labs(x=xtxt3.2, y= "Ratio [%]") +
            theme_minimal()+
            guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
            scale_x_discrete(labels = wrap_format(10))+
            theme(axis.text.x = element_text(size=8),
                  legend.position = "right")+
            ggtitle("Total Solids Recovery")
  
  ## ---- Water Recovery Ratio per Template ---- 
  
  p3.2h2o <- ggplot(data=props, aes(x=template, y=100*recovery_ratio_water_mean))+
              geom_point(aes(color=source), alpha=0.5, size=2, position = position_jitter())+
              scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.2)+
              geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
              geom_text(data = dataMedian_template_h2o, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
              labs(x=xtxt3.2, y= "Ratio [%]") +
              theme_minimal()+
              guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
              scale_x_discrete(labels = wrap_format(10))+
              theme(axis.text.x = element_text(size=8),
                    legend.position = "right")+
              ggtitle("Water Recovery Ratio")
  
  ## ---- Recovered Water per Template ---- 
  
  p3.2h2om <- ggplot(data=props, aes(x=template, y=recovered_water_mean))+
              geom_point(aes(color=source), alpha=0.5, size=2, position = position_jitter())+
              scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.2)+
              geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
              geom_text(data = dataMedian_template_h2om, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
              labs(x=xtxt3.2, y= "Recovered Amount [kg/year]") +
              theme_minimal()+
              guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
              scale_x_discrete(labels = wrap_format(10))+
              theme(axis.text.x = element_text(size=8),
                    legend.position = "right")+
              ggtitle("Water Recovery Absolute")
  
  ## ---- Accumulated Recovery Ratio per Template ----
  
  p3.2acc <- ggplot(data=props, aes(x=template, y=100*recovery_ratio_accumulated_balanced_mean))+
              geom_point(aes(color=source), alpha=0.5, size=2, position = position_jitter())+
              scale_colour_manual(values=source_cols, labels = source_labs, labstxt3.2)+
              geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
              geom_text(data = dataMedian_template_acc, aes(template, MD, label = MD),size = 3, position = position_dodge(width = 0.8), vjust = 1.5)+
              labs(x=xtxt3.2, y= "Ratio [%]") +
              theme_minimal()+
              guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
              scale_x_discrete(labels = wrap_format(10))+
              theme(axis.text.x = element_text(size=8),
                    legend.position = "right")+
              ggtitle("Accumulated Balanced Recovery")
  
  ## ---- Save Plots as PDF ----

ggsave(file.path(plotdir, "p3_2_template_boxplot_phosphor.pdf"), p3.2p, unit="cm", width=19, height = 24, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p3_2_template_boxplot_nitrogen.pdf"), p3.2n, unit="cm", width=19, height = 24, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p3_2_template_boxplot_totalsolids.pdf"), p3.2ts, unit="cm", width=19, height = 24, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p3_2_template_boxplot_h2o_ratio.pdf"), p3.2h2o, unit="cm", width=19, height = 24, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p3_2_template_boxplot_h2o_mass.pdf"), p3.2h2om, unit="cm", width=19, height = 24, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p3_2_template_boxplot_accumulated.pdf"), p3.2acc, unit="cm", width=19, height = 24, dpi=1000, device="pdf")



# # ---- NEXT PLOT (To be Done) ----