# Annual Report Enforcement Chapter: Generating Plots
# Author: Cameron Van Horn
# Contact: cameron.vanhorn@noaa.gov
#          (206) 526-4222

# TODO's:
  # Overplotting dot plots
  # Density of sub categories for OLE PIP only
  # River plots for:
    # OLD OLE CATEGORY -> NEW OLE CATEGORY -> STATEMENT TYPE (NEW DATA ONLY)
      # after filter by New, pivot_wider by statement type
    # STATEMENT TYPE -> OLD OLE CATEGORY (OLD DATA ONLY)

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

#####################
##### ALL PLOTS #####
#####################
# Rain cloud plot of violations from statements_combined -----------------------
# Set colors
colors <- nmfs_palette('regional')(6)

# Make the plot
OPS_number_rainplot <- {
  
  ggplot(data = statements_combined,
         aes(x = factor(OLE_SYSTEM,
                        levels = c('OLD', 'NEW')),
             y = (NUMBER_VIOLATIONS),
             fill = interaction(FIRST_VIOL_YEAR, OLE_SYSTEM),
             color = interaction(FIRST_VIOL_YEAR, OLE_SYSTEM))) +
    labs(x = 'OLE System',
         y = 'Occurrences per Statement') +
    facet_grid(. ~ FIRST_VIOL_YEAR, 
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
    scale_y_log10() +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          text = element_text(size = 20, family = 'Gill Sans MT'))
  
} 

# View the plot
OPS_number_rainplot

# Save the plot
ggsave(filename = 'Plots/OPS_number_rainplot.png',
       plot = OPS_number_rainplot,
       width = 9,
       height = 8)


# Rain cloud plots of OLE Priority IP from rate_by_subcat ----------------------
# Set colors
colors <- nmfs_palette('regional')(6)

# Make the plot
OLEPIP_number_rainplot <- {
  
  ggplot(data = statements_combined %>%
           filter(OLD_OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL'),
         aes(x = factor(OLE_SYSTEM,
                        levels = c('OLD', 'NEW')),
             y = NUMBER_VIOLATIONS,
             fill = interaction(FIRST_VIOL_YEAR, OLE_SYSTEM),
             color = interaction(FIRST_VIOL_YEAR, OLE_SYSTEM))) +
    labs(x = 'OLE System',
         y = 'OLE Priority: Inter-Personal\nOccurrences') +
    facet_grid(. ~ FIRST_VIOL_YEAR, 
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
    scale_y_log10(breaks = c(1:5, 10, 100),
                  limits = c(1, 100)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          text = element_text(size = 20, family = 'Gill Sans MT'))
  
} 

# View the plot
OLEPIP_number_rainplot

# Save the plot
ggsave(filename = 'Plots/OLEPIP_number_rainplot.png',
       plot = OLEPIP_number_rainplot,
       width = 9,
       height = 8)

