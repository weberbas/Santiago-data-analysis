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

source("Santiago-Data-Prep.R")
source("Santiago-Data-Helpers.R")


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
              labs(x=xtxt3.2, y= "Recovered Amount [kg/year*person]") +
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







# # ---- 1.1.3 Recovery Ratio - Density Plot ----

## General Labels
labstxt1.1.3<- expression(paste("Substance"))

  ## ---- Phosphor Recovery Ratio Density Plot ----

p1.1.3p <- ggplot(props, aes(x=recovery_ratio_phosphor_mean)) + 
              geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
              scale_fill_manual(labstxt1.1.3)+
              theme_minimal()+
              theme(
                    panel.grid =   element_line(colour = "#ECECEC", size=0.25),
                    plot.title = element_text(size = 9, face = "bold"),
                    axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
                    axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
                    strip.text = element_text(size=8, face="bold"),
                    legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
                    legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
              xlim(0,1)+
              labs(x="Ratio [-]", y= "Density") +
              ggtitle("Phosphorus Recovery")

  ## ---- Nitrogen Recovery Ratio Density Plot ----

p1.1.3n <- ggplot(props, aes(x=recovery_ratio_nitrogen_mean)) + 
              geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
              scale_fill_manual(labstxt1.1.3)+
              theme_minimal()+
              theme(
                    panel.grid =   element_line(colour = "#ECECEC", size=0.25),
                    plot.title = element_text(size = 9, face = "bold"),
                    axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
                    axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
                    strip.text = element_text(size=8, face="bold"),
                    legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
                    legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
              xlim(0,1)+
              labs(x="Ratio [-]", y= "Density") +
              ggtitle("Nitrogen Recovery")

  ## ---- Total Solids Recovery Ratio Density Plot ----

p1.1.3ts <- ggplot(props, aes(x=recovery_ratio_totalsolids_mean)) + 
              geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
              scale_fill_manual(labstxt1.1.3)+
              theme_minimal()+
              theme(
                    panel.grid =   element_line(colour = "#ECECEC", size=0.25),
                    plot.title = element_text(size = 9, face = "bold"),
                    axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
                    axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
                    strip.text = element_text(size=8, face="bold"),
                    legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
                    legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
              xlim(0,1)+
              labs(x="Ratio [-]", y= "Density") +
              ggtitle("Total Solids Recovery")

  ## ---- Water Recovery Ratio Density Plot ----

p1.1.3h2o <- ggplot(props, aes(x=recovery_ratio_water_mean)) + 
                geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
                scale_fill_manual(labstxt1.1.3)+
                theme_minimal()+
                theme(
                      panel.grid =   element_line(colour = "#ECECEC", size=0.25),
                      plot.title = element_text(size = 9, face = "bold"),
                      axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
                      axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
                      strip.text = element_text(size=8, face="bold"),
                      legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
                      legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
                xlim(0,1)+
                labs(x="Ratio [-]", y= "Density") +
                ggtitle("Water Recovery Ratio")

  ## ---- Water Recovery Volume Density Plot ----

p1.1.3h2om <- ggplot(props, aes(x=recovered_water_mean)) + 
                geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
                scale_y_sqrt(labels = scientific)+
                scale_fill_manual(labstxt1.1.3)+
                theme_minimal()+
                theme(
                      panel.grid =   element_line(colour = "#ECECEC", size=0.25),
                      plot.title = element_text(size = 9, face = "bold"),
                      axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
                      axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
                      strip.text = element_text(size=8, face="bold"),
                      legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
                      legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
                labs(x=expression(paste("kg/year*person")), y= "density") +
                scale_x_continuous(labels = scientific) +
                ggtitle("Water Recovery Mass")

  ## ---- Accumulated Balanced Recovery Ratio Density Plot ----

p1.1.3acc <- ggplot(props, aes(x=recovery_ratio_accumulated_balanced_mean)) + 
            geom_density(fill = "#6F6F6E", colour = "#6F6F6E")+
            scale_fill_manual(labstxt1.1.3)+
            theme_minimal()+
            theme(
                  panel.grid =   element_line(colour = "#ECECEC", size=0.25),
                  plot.title = element_text(size = 9, face = "bold"),
                  axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
                  axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
                  strip.text = element_text(size=8, face="bold"),
                  legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
                  legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
            xlim(0,1)+
            labs(x="Ratio [-]", y= "Density") +
            ggtitle("Accumulated Balanced Recovery")

  ## ---- Save Plots as PDF ----

ggsave(file.path(plotdir, "p1_1_3_recovery_densityplot_phosphor.pdf"), p1.1.3p, unit="cm", width=19, height = 10, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p1_1_3_recovery_densityplot_nitrogen.pdf"), p1.1.3n, unit="cm", width=19, height = 10, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p1_1_3_recovery_densityplot_totalsolids.pdf"), p1.1.3ts, unit="cm", width=19, height = 10, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p1_1_3_recovery_densityplot_h2o_ratio.pdf"), p1.1.3h2o, unit="cm", width=19, height = 10, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p1_1_3_recovery_densityplot_h2o_mass.pdf"), p1.1.3h2om, unit="cm", width=19, height = 10, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p1_1_3_recovery_densityplot_accumulated.pdf"), p1.1.3acc, unit="cm", width=19, height = 10, dpi=1000, device="pdf")










# # ---- 1.2.1 Recovery Ratio - Density Plot by Source ----------

# General Label
labstxt1.2.1<- expression(paste(""))

  ## ---- Phosphor Recovery Ratio Density Plot by Source ----

p1.2.1p <- ggplot(props, aes(x=recovery_ratio_phosphor_mean)) + 
  geom_density(aes(fill=source), size=0.25, alpha=.6)+
  scale_fill_manual(values = source_cols, labstxt1.2.1)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  guides(color = guide_legend(nrow = 2))+
  theme_minimal()+
  theme(
    panel.grid =   element_line(colour = "#ECECEC", size=0.25),
    plot.title = element_text(size = 9, face = "bold"),
    axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
    axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
    strip.text = element_text(size=8, face="bold"),
    legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
    legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="Ratio [-]", y= "Density") +
  ggtitle("Phosphorus Recovery")

  ## ---- Nitrogen Recovery Ratio Density Plot by Source ----

p1.2.1n <- ggplot(props, aes(x=recovery_ratio_nitrogen_mean)) + 
  geom_density(aes(fill=source),  size=0.25, alpha=.6)+
  scale_fill_manual(values = source_cols,  labstxt1.2.1)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(
    panel.grid =   element_line(colour = "#ECECEC", size=0.25),
    plot.title = element_text(size = 9, face = "bold"),
    axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
    axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
    strip.text = element_text(size=8, face="bold"),
    legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
    legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="Ratio [-]", y= "Density") +
  ggtitle("Nitrogen Recovery")

  ## ---- Total Solids Recovery Ratio Density Plot by Source ----

p1.2.1ts <- ggplot(props, aes(x=recovery_ratio_totalsolids_mean)) + 
  geom_density(aes(fill=source),  size=0.25, alpha=.6)+
  scale_fill_manual(values = source_cols,  labstxt1.2.1)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(
    panel.grid =   element_line(colour = "#ECECEC", size=0.25),
    plot.title = element_text(size = 9, face = "bold"),
    axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
    axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
    strip.text = element_text(size=8, face="bold"),
    legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
    legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="Ratio [-]", y= "Density") +
  ggtitle("Total solids Recovery")

  ## ---- Water Recovery Ratio Density Plot by Source ----

p1.2.1h2o <- ggplot(props, aes(x=recovery_ratio_water_mean)) + 
  geom_density(aes(fill=source), size=0.25,  alpha=.6)+
  scale_fill_manual(values = source_cols,  labstxt1.2.1)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(
    panel.grid =   element_line(colour = "#ECECEC", size=0.25),
    plot.title = element_text(size = 9, face = "bold"),
    axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
    axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
    strip.text = element_text(size=8, face="bold"),
    legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
    legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="Ratio [-]", y= "Density") +
  ggtitle("Water Recovery")

  ## ---- Water Recovery Volume Density Plot by Source ----

p1.2.1h2om <- ggplot(props, aes(x=recovered_water_mean)) + 
  geom_density(aes(fill=source),  size=0.25, alpha=.6)+
  scale_y_sqrt()+
  scale_fill_manual(values = source_cols,  labstxt1.2.1)+
  theme_minimal()+
  theme(
    panel.grid =   element_line(colour = "#ECECEC", size=0.25),
    plot.title = element_text(size = 9, face = "bold"),
    axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
    axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
    strip.text = element_text(size=8, face="bold"),
    legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
    legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,max(props$recovered_water_mean))+
  labs(x=expression(paste("kg/year*person")), y= "Density") +
  ggtitle("Water Recovery")

  ## ---- Accumulated Balanced Recovery Ratio Density Plot by Source ----

p1.2.1acc <- ggplot(props, aes(x=recovery_ratio_accumulated_balanced_mean)) + 
  geom_density(aes(fill=source),  size=0.25, alpha=.6)+
  scale_fill_manual(values = source_cols,  labstxt1.2.1)+
  scale_x_continuous(breaks = seq(0, 1, 0.2))+
  theme_minimal()+
  theme(
    panel.grid =   element_line(colour = "#ECECEC", size=0.25),
    plot.title = element_text(size = 9, face = "bold"),
    axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
    axis.text.x = element_text(size=7, colour="#B1B1B1"), axis.text.y = element_text(size=7, colour="#B1B1B1"),
    strip.text = element_text(size=8, face="bold"),
    legend.text=element_text(size=8, colour="#B1B1B1"), legend.position= "bottom",       
    legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))+
  xlim(0,1)+
  labs(x="Ratio [-]", y= "Density") +
  ggtitle("Accumulated Balanced Recovery")



  ## ---- Save Plots as PDF ----



ggsave(file.path(plotdir, "p1_2_1_recovery_densityplot_phosphor.pdf"), p1.2.1p, unit="cm", width=19, height = 10, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p1_2_1_recovery_densityplot_nitrogen.pdf"), p1.2.1n, unit="cm", width=19, height = 10, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p1_2_1_recovery_densityplot_totalsolids.pdf"), p1.2.1ts, unit="cm", width=19, height = 10, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p1_2_1_recovery_densityplot_h2o_ratio.pdf"), p1.2.1h2o, unit="cm", width=19, height = 10, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p1_2_1_recovery_densityplot_h2o_mass.pdf"), p1.2.1h2om, unit="cm", width=19, height = 10, dpi=1000, device="pdf")
ggsave(file.path(plotdir, "p1_2_1_recovery_densityplot_accumulated.pdf"), p1.2.1acc, unit="cm", width=19, height = 10, dpi=1000, device="pdf")




# # ---- 1.3.3 Recovery Ratio - Boxplot grouped by Sources ----- -----

## Preparation for Plots: Melt dataframe and factor

dt_source_absolute3 <- props[, c("source", "recovery_ratio_phosphor_mean", "lost_phosphor_air.loss_mean", "lost_phosphor_water.loss_mean", "lost_phosphor_soil.loss_mean", 
                                 "recovery_ratio_nitrogen_mean", "lost_nitrogen_air.loss_mean", "lost_nitrogen_water.loss_mean", "lost_nitrogen_soil.loss_mean",
                                 "recovery_ratio_totalsolids_mean", "lost_totalsolids_air.loss_mean", "lost_totalsolids_water.loss_mean", "lost_totalsolids_soil.loss_mean",
                                 "recovered_water_mean", "lost_water_air.loss_mean", "lost_water_water.loss_mean", "lost_water_soil.loss_mean")]
dtmelt_source_absolute3 <- melt(dt_source_absolute3, id=c("source"))
dtmelt_source_absolute3$variable <- factor(dtmelt_source_absolute3$variable , labels = c("TP~recovered~('%')","TP~lost~to~air~('%')","TP~lost~to~water~('%')","TP~lost~to~soil~('%')",
                                                                                         "TN~recovered~('%')","TN~lost~to~air~('%')","TN~lost~to~water~('%')","TN~lost~to~soil~('%')",
                                                                                         "TS~recovered~('%')","TS~lost~to~air~('%')","TS~lost~to~water~('%')","TS~lost~to~soil~('%')",
                                                                                         "H2O~recovered~( kg/y/pers)","H2O~lost~to~air~( kg/y/pers)","H2O~lost~to~water~( kg/y/pers)","H2O~lost~to~soil~( kg/y/pers)"))
## Preparation: Calculate Median

dataMedian <- summarise(group_by(dtmelt_source_absolute3, source, variable), MD = round(median(value),2))

## Preparation: Add General Labels

xtxt1.3.3 <- expression(paste(""))
labstxt1.3.3 <- expression(paste("Source Technology"))

p1.3.3 <- ggplot(data=dtmelt_source_absolute3, aes(x= source, y= value))+
  geom_point(aes(colour= factor(source)), alpha=0.5, size=0.5, position = position_jitter())+
  scale_colour_manual(values = source_cols) +
  geom_boxplot(varwidth= FALSE, alpha=0.5, lwd=0.25, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
  geom_text(data = dataMedian, aes(source, MD, label = MD),size = 2, position = position_dodge(width = 0.8), vjust = -0.75)+
  facet_wrap(~variable, labeller = label_parsed, scale="free_y")+
  labs(color=labstxt1.3.3, x= xtxt1.3.3, y ="substance recoverd/lost")+
  guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
  theme_minimal()+
  theme(
        panel.grid =   element_line(colour = "#ECECEC", size=0.25),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x=element_text(size=7, colour="#B1B1B1"), axis.title.y=element_text(size=7, colour="#B1B1B1"),
        axis.text.x = element_text(size=7, colour="#B1B1B1", angle = 90), axis.text.y = element_text(size=7, colour="#B1B1B1"),
        strip.text = element_text(size=8, face="bold"),
        legend.text=element_text(size=7, colour="#B1B1B1"), legend.position= "bottom",       
        legend.title = element_text(size=9, face = "bold"), legend.key.size = unit(1,"line"))

  ## ---- Save Plot as PDF -----

ggsave(file.path(plotdir, "p1_3_3_sourceboxplot_ratio_h2om.pdf"), p1.3.3, unit="cm", width=19, height = 16, dpi=1000, device="pdf")



# # ---- 4.1 Recovery Ratio - Boxplots for Number of Technologies for every Substance, grouped by System Templates --------


## General Labels

  xtxt4.1 <- expression(paste("Number of technologies in SanSys"))
  labstxt4.1 <- expression(paste("System Template"))
  

  p4.1p <- ggplot(data=props, aes(x=ntechs, y=recovery_ratio_phosphor_mean))+
    geom_point(aes(color=template), position = "jitter", alpha=0.8, size=1.1)+
    scale_colour_manual(values = template_cols, labels = str_wrap(template_names, 25), labstxt4.1)+
    geom_boxplot(aes(group=ntechs), varwidth=TRUE, alpha=0, lwd=0.25)+
    ylim(0,1)+
    labs(x=xtxt4.1, y= "Recovery ratio [-]") +
    theme_minimal()+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    theme(axis.title.x=element_text(size=8), 
          plot.title = element_text(size = 8),
          legend.key.size = unit(1,"line"), 
          legend.position= "bottom")+
    ggtitle("TP Recovery")
  
  
  p4.1n <- ggplot(data=props, aes(x=ntechs, y=recovery_ratio_nitrogen_mean))+
    geom_point(aes(color=template), position = "jitter", alpha=0.8, size=1.1)+
    scale_colour_manual(values = template_cols, labels = str_wrap(template_names, 25), labstxt4.1)+
    geom_boxplot(aes(group=ntechs), varwidth=TRUE, alpha=0, lwd=0.25)+
    ylim(0,1)+
    labs(x=xtxt4.1, y= "Recovery ratio [-]") +
    theme_minimal()+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    theme(axis.title.x=element_text(size=8), 
          plot.title = element_text(size = 8),
          legend.key.size = unit(1,"line"), 
          legend.position= "bottom")+
    ggtitle("TN Recovery")
  
  p4.1ts<- ggplot(data=props, aes(x=ntechs, y=recovery_ratio_totalsolids_mean))+
    geom_point(aes(color=template), position = "jitter", alpha=0.8, size=1.1)+
    scale_colour_manual(values = template_cols, labels = str_wrap(template_names, 25), labstxt4.1)+
    geom_boxplot(aes(group=ntechs), varwidth=TRUE, alpha=0, lwd=0.25)+
    ylim(0,1)+
    labs(x=xtxt4.1, y= "Recovery ratio [-]") +
    theme_minimal()+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    theme(axis.title.x=element_text(size=8), 
          plot.title = element_text(size = 8),
          legend.key.size = unit(1,"line"), 
          legend.position= "bottom")+
    ggtitle("TS Recovery")
  
  p4.1h2o <- ggplot(data=props, aes(x=ntechs, y=recovery_ratio_water_mean))+
    geom_point(aes(color=template), position = "jitter", alpha=0.8, size=1.1)+
    scale_colour_manual(values = template_cols, labels = str_wrap(template_names, 25), labstxt4.1)+
    geom_boxplot(aes(group=ntechs), varwidth=TRUE, alpha=0, lwd=0.25)+
    ylim(0,1)+
    labs(x=xtxt4.1, y= "Recovery ratio [-]") +
    theme_minimal()+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    theme(axis.title.x=element_text(size=8),
          plot.title = element_text(size = 8),
          legend.key.size = unit(1,"line"), 
          legend.position= "bottom")+
    ggtitle("H2O Recovery")
  
  
  p4.1h2om <- ggplot(data=props, aes(x=ntechs, y=recovered_water_mean))+
    geom_point(aes(color=template), position = "jitter", alpha=0.8, size=1.1)+
    geom_boxplot(aes(group=ntechs), varwidth=TRUE, alpha=0, lwd=0.25)+
    scale_colour_manual(values = template_cols, labels = str_wrap(template_names, 25), labstxt4.1)+
    labs(x=xtxt4.1, y= "Recovered water mean [kg/year*person]") +
    theme_minimal()+
    guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)))+
    theme(axis.title.x=element_text(size=8), 
          plot.title = element_text(size = 8),
          legend.key.size = unit(1,"line"), 
          legend.position= "bottom")+
    ggtitle("H2O Recovery")

  ## ---- Create one plot with shared legend from multiple Plots ---- 
  
    p4.1 <- grid_arrange_shared_legend(p4.1p, p4.1n,  p4.1ts, p4.1h2o, nrow=2, ncol=2)
    p4.1_h2om <- grid_arrange_shared_legend(p4.1p, p4.1n,  p4.1ts, p4.1h2om, nrow=2, ncol=2)
  
  ## ---- Save Plots as PDF ----
  
    ggsave(file.path(plotdir, "p4_1_ntech_templates_substance.pdf"), p4.1, unit="cm", width=19, height = 19, dpi=1000, device="pdf")
    ggsave(file.path(plotdir, "p4_1_ntech_templates_substance_h20m.pdf"), p4.1_h2om, unit="cm", width=19, height = 19, dpi=1000, device="pdf")
  
  
  
  
    
# # ---- 4.3.1 Recovery Ratio - Boxplots for Number of Technologies for accumulated Recovery, grouped by System Templates----
    
    ## General Labels
    xtxt4.3.1 <- expression(paste("Length (Number of Technologies within the System)"))
    
    
    p4.3.1 <- ggplot(data=props, aes(x=factor(ntechs), y=recovery_ratio_accumulated_balanced_mean))+
      geom_point(aes(color=template), alpha=0.5, size=1.2, position = position_jitter())+
      scale_colour_manual(values=template_cols, labels = str_wrap(template_names, 25), "System templates")+
      geom_boxplot(aes(group=ntechs), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
      labs(x=xtxt4.3.1, y= "Accumulated Balanced Recovery Ratio [-]") +
      guides(colour = guide_legend(override.aes = list(size=4, alpha= 1), nrow=2))+
      scale_fill_manual(values=template_cols, labels = str_wrap(template_names, 25)) +
      ylim(0,1)+
      theme_minimal()+
      theme(
            plot.title = element_text(size = 9, face = "bold"),
            axis.title.x=element_text(size=7, colour = "#6F6F6E"), 
            axis.title.y=element_text(size=7, colour = "#6F6F6E"),
            axis.text.x = element_text(size=7, colour = "#6F6F6E"), 
            axis.text.y = element_text(size=7, colour = "#6F6F6E"),
            strip.text = element_text(size=8, face="bold"),
            legend.text=element_text(size=8, colour="#B1B1B1"), 
            legend.position= "bottom",       
            legend.title = element_text(size=9, face = "bold"), 
            legend.key.size = unit(1,"line"))
 
    ## ---- Create PDF of Plot ---- 
    ggsave(file.path(plotdir, "p4_3_1_accumultedrecovery_ntech.pdf"), p4.3.1, unit="cm", width=19, height = 10, dpi=1000, device="pdf")

    
    
# # ---- 5.1  Recovery Ratio - SAS, Substance Recoveries & Selected Systems ----
    
    p5.1.p <- ggplot(data=props, aes(x=sysappscore, y= recovery_ratio_phosphor_mean)) +
      geom_point(alpha=0.5, size=1.2, position = position_jitter(), color="#B1B1B1")+
      geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE)+
      scale_fill_manual(values=template_cols, labels = str_wrap(template_names, 25), str_wrap("Templates of Selected Systems", 20)) +
      geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                      arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"), size=2) +
      guides(colour = guide_legend(override.aes = list(size=2, alpha= 1)), fill=guide_legend(nrow=2,byrow=FALSE))+
      theme_minimal() +
      theme(
            panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
            plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
            axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
            axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
            strip.text = element_text(size=8.5, face="bold"),
            legend.position= "bottom",       
            legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
            legend.text=element_text(size=8.5, colour="#6F6F6E"),
            legend.key.size = unit(1,"line"))+
      labs(x = "System Appropriateness Score (SAS)", y="ratio [-]")+
      ggtitle("Phosphorus Recovery")
    
    p5.1.n <- ggplot(data=props, aes(x=sysappscore, y=recovery_ratio_nitrogen_mean )) +
      geom_point(alpha=0.5, size=1.2, position = position_jitter(), color="#B1B1B1")+
      geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE)+
      scale_fill_manual(values=template_cols, labels = str_wrap(template_names, 25), str_wrap("Templates of selected systems", 20)) +
      geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                      arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"),  size=2) +
      guides(colour = guide_legend(override.aes = list(size=2, alpha= 1)), fill=guide_legend(nrow=2,byrow=FALSE))+
      theme_minimal() +
      theme(
            panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
            plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
            axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
            axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
            strip.text = element_text(size=8.5, face="bold"),
            legend.position= "bottom",       
            legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
            legend.text=element_text(size=8.5, colour="#6F6F6E"),
            legend.key.size = unit(1,"line"))+
      labs(x = "System Appropriateness Score (SAS)", y="ratio [-]")+
      ggtitle("Nitrogen Recovery")
    
    p5.1.ts <- ggplot(data=props, aes(x=sysappscore, y=recovery_ratio_totalsolids_mean )) +
      geom_point(alpha=0.5, size=1.2, position = position_jitter(), color="#B1B1B1")+
      geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE)+
      scale_fill_manual(values=template_cols, labels = str_wrap(template_names, 25), str_wrap("Templates of selected systems", 20)) +
      geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                      arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"),  size=2) +
      guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)), fill=guide_legend(nrow=3,byrow=FALSE))+
      theme_minimal() +
      theme(
            panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
            plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
            axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
            axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
            strip.text = element_text(size=8.5, face="bold"),
            legend.position= "bottom",       
            legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
            legend.text=element_text(size=8.5, colour="#6F6F6E"),
            legend.key.size = unit(1,"line"))+
      labs(x = "System Appropriateness Score (SAS)", y="ratio [-]")+
      ggtitle("Total Solids Recovery")
    
    p5.1.h2o <- ggplot(data=props, aes(x=sysappscore, y=recovery_ratio_water_mean )) +
      geom_point(alpha=0.5, size=1.2, position = position_jitter(), color="#B1B1B1")+
      geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE)+
      scale_fill_manual(values=template_cols, labels = str_wrap(template_names, 25), str_wrap("Templates of selected systems", 20)) +
      geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                      arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"),  size=2) +
      guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)), fill=guide_legend(nrow=3,byrow=FALSE))+
      theme_minimal() +
      theme(
            panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
            plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
            axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
            axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
            strip.text = element_text(size=8.5, face="bold"),
            legend.position= "bottom",       
            legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
            legend.text=element_text(size=8.5, colour="#6F6F6E"),
            legend.key.size = unit(1,"line"))+
      labs(x = "System Appropriateness Score (SAS)", y="ratio [-]")+
      ggtitle("Water Recovery Ratio")
    
    p5.1.h2om <- ggplot(data=props, aes(x=sysappscore, y=recovered_water_mean )) +
      geom_point(alpha=0.5, size=1.2, position = position_jitter(), color="#B1B1B1")+
      geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE)+
      scale_fill_manual(values=template_cols, labels = str_wrap(template_names, 25), str_wrap("Templates of selected systems", 20)) +
      geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                      arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"), size=2) +
      guides(colour = guide_legend(override.aes = list(size=4, alpha= 1)), fill=guide_legend(nrow=3,byrow=FALSE))+
      theme_minimal() +
      theme(
            panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
            plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
            axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
            axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
            strip.text = element_text(size=8.5, face="bold"),
            legend.position= "bottom",       
            legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
            legend.text=element_text(size=8.5, colour="#6F6F6E"),
            legend.key.size = unit(1,"line"))+
      labs(x = "System Appropriateness Score (SAS)", y="kg/year*person")+
      ggtitle("Water Recovery Volume")
    
    ## ---- Create one Plot with shared legends from all plots ---- 
    ## Use `p5.1` if you want to plot water recovery ratios
    ## If you want to plot water recovery volumes use `p5.1_h2om`
    #p5.1 <-  grid_arrange_shared_legend(p5.1.p, p5.1.n, p5.1.ts, p5.1.h2o, ncol= 2, nrow=2, position="bottom")
    p5.1_h2om <-  grid_arrange_shared_legend(p5.1.p, p5.1.n, p5.1.ts, p5.1.h2om, ncol= 2, nrow=2, position="bottom")
    
    ## ---- Save Plot as PDF ----
    ggsave(file.path(plotdir, "p5_1_SAS_recovery.pdf"), p5.1_h2om, unit="cm", width=19, height = 14, dpi=1000, device="pdf")

    
    
    
# # ---- 5.3 Recovery Ratio - SAS Boxplot per Template colored by Source ----

    ## Calculate Median of System Appropriateness Score
    dataMedian_sysappscore <- summaryBy(sysappscore ~ template, data = props, FUN = list(median))
    dataMedian_sysappscore$sysappscore.median <- round(dataMedian_sysappscore$sysappscore.median,2)
    
    ## Comment: You might have run this plot a few times to get a position of labels without overlap
    p5.3 <- ggplot(data=props, aes(x=template, y=sysappscore))+
      geom_point(aes(color=source), alpha=0.5, size=1.2, position = position_jitter())+
      scale_colour_manual(values=source_cols, labels = source_labs, "Source", 
                          guide=guide_legend(override.aes = list(size=2, alpha= 1), nrow=4,byrow=FALSE))+
      geom_boxplot(aes(group=template), varwidth= FALSE, alpha=0.5, lwd=0.25, outlier.size = 0.5, fill = "#6F6F6E", colour = "#6F6F6E", width=0.5)+
      geom_text(data = dataMedian_sysappscore, aes(template, sysappscore.median, label = sysappscore.median), size = 3, 
                position = position_dodge(width = 0.2), vjust = -0.75, check_overlap = TRUE)+
      geom_point(data = props[props$selected,], aes(fill= template), size=3.5, shape=21, stroke=0, show.legend = TRUE, alpha=0.7)+
      scale_fill_manual(values=template_cols, labels = str_wrap(template_names, 25), str_wrap("Templates of selected systems", 20), 
                        guide=guide_legend(override.aes = list(size=2, alpha= 1), nrow=4,byrow=FALSE)) +
      geom_text_repel(data = props[props$selected,], aes(label=ID),position = "jitter", segment.size = 0.2, 
                      arrow = arrow(length = unit(0.03, "npc"), type = "open", ends = "last"), size=3) +
      labs(x="Template", y= "SAS") +
      theme_minimal()+
      theme(
            panel.grid =   element_line(colour = "#C5C5C4", size=0.25),
            plot.title = element_text(size = 10, colour = "#1D1D1B", face = "bold"),
            axis.title.x=element_text(size=8.5, colour = "#6F6F6E"),
            axis.title.y=element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.x = element_text(size=8.5, colour = "#6F6F6E"),
            axis.text.y = element_text(size=8.5, colour = "#6F6F6E"),
            strip.text = element_text(size=8.5, face="bold"),
            legend.position= "bottom",       
            legend.title = element_text(size = 9, colour = "#1D1D1B", face = "bold"),
            legend.text=element_text(size=8.5, colour="#6F6F6E"),
            legend.key.size = unit(1,"line"))+
      theme(axis.title.x=element_blank())+
      scale_x_discrete(labels=str_wrap(template_names,18)) +
      ggtitle("system Appropriateness Score (SAS) [-]") 

    ## ---- Save Plot as PDF ----
    ggsave(file.path(plotdir, "p5_3_SAS_template.pdf"), p5.3, unit="cm", width=19, height = 14, dpi=1000, device="pdf")
    
    