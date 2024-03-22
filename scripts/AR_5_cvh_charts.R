# Annual Report Enforcement Chapter: Generating Plots
# Author: Cameron Van Horn
# Contact: cameron.vanhorn@noaa.gov
#          (206) 526-4222

##################################
##### LOAD PACKAGES AND DATA #####
##################################
library(dplyr)
library(ggplot2)
library(ggh4x)
library(googledrive)
library(ggdist)
library(gghalves)
library(extrafont)
library(nmfspalette)
library(ggbeeswarm)

# clear environment
rm(list = ls())

# load data
load(file = 'C:/Users/cameron.vanhorn/Work/AR_2024_Chapter5/data_files/AR_3_rate_output.rdata')

# load fonts
# font_import() # return y or n
loadfonts()
# see list of fonts
# fonts()

#############################################
##### PLOTS OF OCCURENCES PER STATEMENT #####
#############################################
# Not transforming the data, violin plot of statements_combined ----------------
# Set colors
colors <- nmfs_palette('regional')(6)

# Make the plot
OPS_violin_plot <- {
  ggplot(data = statements_combined, 
         aes(x = as.factor(OLE_SYSTEM),
             y = NUMBER_VIOLATIONS,
             color = interaction(OLE_SYSTEM, FIRST_VIOL_YEAR),
             fill = interaction(OLE_SYSTEM, FIRST_VIOL_YEAR))) +
    facet_grid(. ~ FIRST_VIOL_YEAR,
               scales = 'free',) +
    force_panelsizes(cols = c(0.3, 0.5)) +
    geom_violin(width = 1, 
                size = 0.9,
                color = 'black',
                show.legend = F) +
    geom_half_point(side = 'l',
                    range_scale = 0.3,
                    position = position_nudge(x = -0.05),
                    alpha = 0.5,
                    size = 3,
                    show.legend = F) +
    labs(x = 'OLE System',
         y = 'Occurrences per Statement') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          text = element_text(size = 20, family = 'Gill Sans MT')) +
    expand_limits(x = c(0.2, 2)) +
    scale_fill_manual(values = colors[c(1, 2, 3)],
                      guide = 'none') +
    scale_color_manual(values = colors[c(1, 2, 3)],
                       guide = 'none')

}

# View the plot
OPS_violin_plot

# Save the plot
ggsave(filename = 'Plots/OPS_violin_plot.png',
       plot = OPS_violin_plot,
       width = 9,
       height = 8)

# Log transforming the data, violin plot of statements_combined ----------------
# Set colors
colors <- nmfs_palette('regional')(6)

# Make the plot
OPS_violin_plot_log <- {
  ggplot(data = statements_combined,
         aes(x = as.factor(OLE_SYSTEM),
             y = log(NUMBER_VIOLATIONS),
             color = interaction(OLE_SYSTEM, FIRST_VIOL_YEAR),
             fill = interaction(OLE_SYSTEM, FIRST_VIOL_YEAR))) +
    facet_grid(. ~ FIRST_VIOL_YEAR,
               scales = 'free',) +
    force_panelsizes(cols = c(0.3, 0.5)) +
    geom_violin(width = 1, 
                 size = 0.9,
                 color = 'black',
                 show.legend = F) +
    geom_half_point(data = statements_combined %>%
                      filter(log(NUMBER_VIOLATIONS) > 0),
                    side = 'l',
                    range_scale = 0.3,
                    position = position_nudge(x = -0.05),
                    alpha = 0.5,
                    size = 3,
                    show.legend = F) +
    labs(x = 'OLE System',
         y = 'Log Occurrences per Statement') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          text = element_text(size = 20, family = 'Gill Sans MT')) +
    expand_limits(x = c(0.2, 2)) +
    scale_fill_manual(values = colors[c(1, 2, 3)]) +
    scale_color_manual(values = colors[c(1, 2, 3)])
  
}

# View the plot
OPS_violin_plot_log

# Save the plot
ggsave(filename = 'Plots/OPS_violin_plot_log.png',
       plot = OPS_violin_plot_log,
       width = 9,
       height = 8)


# Rain cloud plot of incidents per assignment rates in rate_by_subcat ----------
# Set colors
colors <- nmfs_palette('regional')(6)

# Make the plot
OPS_IPA_rainplot <- {
  ggplot(data = rate_by_subcat,
         aes(x = as.factor(OLE_SYSTEM),
             y = INCIDENTS_PER_ASSIGNMENT,
             fill = interaction(CALENDAR_YEAR, OLE_SYSTEM),
             color = interaction(CALENDAR_YEAR, OLE_SYSTEM))) +
    labs(x = 'OLE System',
         y = 'Occurrences per Assignment') +
    facet_grid(. ~ CALENDAR_YEAR, 
               scales = 'free',) +
    force_panelsizes(cols = c(0.3, 0.5),) +
    stat_halfeye(adjust = .5,
                 width = .3,
                 .width = 0,
                 justification = -0.6,
                 point_color = NA,
                 show.legend = F) +
    geom_boxplot(width = 0.2, 
                 size = 0.9,
                 color = 'black',
                 show.legend = F,
                 alpha = 0.6) +
    geom_half_point(side = 'l', 
                    range_scale = .3, 
                    alpha = .4, 
                    show.legend = F,
                    size = 3) +
    scale_fill_manual(values = colors[c(2, 1, 3)],
                      guide = 'none') +
    scale_color_manual(values = colors[c(2, 1, 3)],
                       guide = 'none') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          text = element_text(size = 20, family = 'Gill Sans MT'))
  
} 

# View the plot
OPS_IPA_rainplot

# Save the plot
ggsave(filename = 'Plots/OPS_IPA_rainplot.png',
       plot = OPS_IPA_rainplot,
       width = 9,
       height = 8)


# Log transformed rain cloud plot of IPA rates in rate_by_subcat ---------------
# Set colors
colors <- nmfs_palette('regional')(6)

# Make the plot
OPS_IPA_rainplot_log <- {
  ggplot(data = rate_by_subcat,
         aes(x = as.factor(OLE_SYSTEM),
             y = log(INCIDENTS_PER_ASSIGNMENT),
             fill = interaction(CALENDAR_YEAR, OLE_SYSTEM),
             color = interaction(CALENDAR_YEAR, OLE_SYSTEM))) +
    labs(x = 'OLE System',
         y = 'Log Occurrences per Assignment') +
    facet_grid(. ~ CALENDAR_YEAR, 
               scales = 'free',) +
    force_panelsizes(cols = c(0.3, 0.5),) +
    stat_halfeye(adjust = .5,
                 width = .3,
                 .width = 0,
                 justification = -0.6,
                 point_color = NA,
                 show.legend = F) +
    geom_boxplot(width = 0.2, 
                 size = 0.9,
                 color = 'black',
                 show.legend = F,
                 alpha = 0.6) +
    geom_half_point(side = 'l', 
                    range_scale = .3, 
                    alpha = .4, 
                    show.legend = F,
                    size = 3) +
    scale_fill_manual(values = colors[c(2, 1, 3)],
                      guide = 'none') +
    scale_color_manual(values = colors[c(2, 1, 3)],
                       guide = 'none') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          text = element_text(size = 20, family = 'Gill Sans MT'))
}

# View the plot
OPS_IPA_rainplot_log

# Save the plot
ggsave(filename = 'Plots/OPS_IPA_rainplot_log.png',
       plot = OPS_IPA_rainplot_log,
       width = 9,
       height = 8)

# Rain cloud plot of incidents per observer rates in rate_by_subcat ------------
# Set colors
colors <- nmfs_palette('regional')(6)

# Make the plot
OPS_IPO_rainplot <- {
  ggplot(data = rate_by_subcat,
         aes(x = as.factor(OLE_SYSTEM),
             y = INCIDENTS_PER_OBSERVER,
             fill = interaction(CALENDAR_YEAR, OLE_SYSTEM),
             color = interaction(CALENDAR_YEAR, OLE_SYSTEM))) +
    labs(x = 'OLE System',
         y = 'Occurrences per Observer') +
    facet_grid(. ~ CALENDAR_YEAR, 
               scales = 'free',) +
    force_panelsizes(cols = c(0.3, 0.5),) +
    stat_halfeye(adjust = .5,
                 width = .3,
                 .width = 0,
                 justification = -0.6,
                 point_color = NA,
                 show.legend = F) +
    geom_boxplot(width = 0.2, 
                 size = 0.9,
                 color = 'black',
                 show.legend = F,
                 alpha = 0.6) +
    geom_half_point(side = 'l', 
                    range_scale = .3, 
                    alpha = .4, 
                    show.legend = F,
                    size = 3) +
    scale_fill_manual(values = colors[c(2, 1, 3)],
                      guide = 'none') +
    scale_color_manual(values = colors[c(2, 1, 3)],
                       guide = 'none') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          text = element_text(size = 20, family = 'Gill Sans MT'))
  
} 

# View the plot
OPS_IPO_rainplot

# Save the plot
ggsave(filename = 'Plots/OPS_IPO_rainplot.png',
       plot = OPS_IPO_rainplot,
       width = 9,
       height = 8)

###############################
##### PLOTS OF SASHI DATA #####
###############################
# Time Series of raw data from statements_combined -----------------------------
# Set colors
colors <- nmfs_palette('regional')(6)

# Make the plot
SASHI_number_ts <- {
  ggplot(data = statements_combined %>%
           filter(OLD_OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL'),
         aes(x = FIRST_VIOLATION_DATE,
             y = NUMBER_VIOLATIONS,
             color = interaction(FIRST_VIOL_YEAR, OLE_SYSTEM))) +
    geom_line(linewidth = 1,
              show.legend = F) +
    labs(x = 'Time',
         y = 'Number of SASHI Violations') +
    theme_bw() +
    theme(text = element_text(size = 20, family = 'Gill Sans MT')) +
    scale_color_manual(values = colors[c(2, 1, 3)])
}
  
# View the plot
SASHI_number_ts

# Save the plot
ggsave(filename = 'Plots/SASHI_number_ts.png',
       plot = SASHI_number_ts,
       width = 10,
       height = 7)

# Rain Cloud Plots of rates from rate_by_subcat --------------------------------
# Set colors
colors <- nmfs_palette('regional')(6)

# Make the plot
SASHI_IPA_boxplot <- {
  ggplot(data = rate_by_subcat %>%
           filter(OLD_OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL'),
         aes(x = as.factor(OLE_SYSTEM),
             y = INCIDENTS_PER_ASSIGNMENT,
             fill = interaction(CALENDAR_YEAR, OLE_SYSTEM),
             color = interaction(CALENDAR_YEAR, OLE_SYSTEM))) +
    labs(x = 'OLE System',
         y = 'SASHI Occurrences per Assignment') +
    facet_grid(. ~ CALENDAR_YEAR, 
               scales = 'free',) +
    force_panelsizes(cols = c(0.3, 0.5),) +
    stat_halfeye(adjust = .5,
                 width = .3,
                 .width = 0,
                 justification = -0.6,
                 point_color = NA,
                 show.legend = F) +
    geom_boxplot(width = 0.2, 
                size = 0.9,
                color = 'black',
                show.legend = F,
                alpha = 0.6) +
    geom_half_point(side = 'l', 
                    range_scale = .3, 
                    alpha = .4, 
                    show.legend = F,
                    size = 3) +
    scale_fill_manual(values = colors[c(2, 1, 3)],
                      guide = 'none') +
    scale_color_manual(values = colors[c(2, 1, 3)],
                       guide = 'none') +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          text = element_text(size = 20, family = 'Gill Sans MT'))
    
} 

# View the plot
SASHI_IPA_boxplot

# Save the plot
ggsave(filename = 'SASHI_IPA_boxplot.png',
       plot = SASHI_IPA_boxplot,
       width = 9,
       height = 8)

# OLD_OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL" for SASHI categories.