# Annual Report Enforcement Chapter: Generating Plots
# Author: Cameron Van Horn
# Contact: cameron.vanhorn@noaa.gov
#          (206) 526-4222

# TODO's:
  # Table of sub category values for OLE PIP only
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
library(ggridges)
library(ggalluvial)
library(ggrepel)
library(ggfittext)

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
         y = 'Occurrences per Statement',
         title = 'All Categories') +
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
                 alpha = 0.6,
                 outliers = F) +
    geom_half_point(side = 'l', 
                    range_scale = .3, 
                    alpha = .25, 
                    show.legend = F,
                    size = 3,
                    pch = 1,
                    transformation = position_jitter(height = 0)) +
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
          plot.title = element_text(hjust = 0.5),
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
         y = 'Occurrences per Statement',
         title = 'OLE Priority: Inter-Personal') +
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
                 alpha = 0.6,
                 outliers = F) +
    geom_half_point(side = 'l', 
                    range_scale = .3, 
                    alpha = .25, 
                    show.legend = F,
                    size = 3,
                    pch = 1,
                    transformation = position_jitter(height = 0)) +
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
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 20, family = 'Gill Sans MT'))
  
} 

# View the plot
OLEPIP_number_rainplot

# Save the plot
ggsave(filename = 'Plots/OLEPIP_number_rainplot.png',
       plot = OLEPIP_number_rainplot,
       width = 9,
       height = 8)


# Ridge plot of sub category densities within OLE Priority IP ------------------
# Set colors
colors <- nmfs_palette('regional')(10)

# Make the plot
OLEPIP_subcat_ridge <- {
  ggplot(data = statements_combined %>%
           filter(OLD_OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL'),
         aes(x = NUMBER_VIOLATIONS,
             y = factor(STATEMENT_TYPE),
             fill = STATEMENT_TYPE)) +
    facet_grid(scales = 'free',
               rows = vars(OLE_SYSTEM)) +
    scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 50, 75, 100),
                       limits = c(0, 100)) +
    scale_y_discrete(expand = c(0.2, 0.2)) +
    scale_color_manual(values = colors, guide = 'none') +
    scale_fill_manual(values = colors, guide = 'none') +
    geom_density_ridges(quantile_lines = T,
                        quantiles = 2,
                        color = 'black',
                        alpha = 0.5) +
    theme_bw() +
    labs(x = 'Occurrences per Statement',
         y = '')
}

# View the plot
OLEPIP_subcat_ridge

# Save the plot
ggsave(filename = 'Plots/OLEPIP_subcat_ridge.png',
       plot = OLEPIP_subcat_ridge,
       width = 16,
       height = 8)


# Alluvial plot of old data ----------------------------------------------------
# flow: STATEMENT_TYPE -> OLD_OLE_CATEGORY (filtered for OLD OLE SYSTEM)

# Set colors
colors <- nmfs_palette('regional')(12)

# define statement types to label outside strata boxes
repel_labels <- rate_by_subcat %>%
  filter(OLE_SYSTEM == 'OLD') %>%
  select(STATEMENT_TYPE, TOTAL_STATEMENTS) %>%
  group_by(STATEMENT_TYPE) %>%
  summarise(across(TOTAL_STATEMENTS, sum)) %>%
  filter(TOTAL_STATEMENTS < 20) %>%
  select(STATEMENT_TYPE)
repel_labels <- as.character(repel_labels$STATEMENT_TYPE)

# break up strings of old ole categories to fit in boxes better
# create a duplicate dataset
rbs_copy <- rate_by_subcat

# view strings to break into lines
unique(rbs_copy$OLD_OLE_CATEGORY)

# break up strings
rbs_copy$OLD_OLE_CATEGORY[
  which(rbs_copy$OLD_OLE_CATEGORY == 'ALL OTHER STATEMENT TYPES')
                          ] <- 'ALL OTHER\nSTATEMENT TYPES'

rbs_copy$OLD_OLE_CATEGORY[
  which(rbs_copy$OLD_OLE_CATEGORY == 'LIMITED ACCESS PROGRAMS')
                          ] <- 'LIMITED ACCESS\nPROGRAMS'

rbs_copy$OLD_OLE_CATEGORY[
  which(rbs_copy$OLD_OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL')
                          ] <- 'OLE PRIORITY:\nINTER-PERSONAL'

rbs_copy$OLD_OLE_CATEGORY[
  which(rbs_copy$OLD_OLE_CATEGORY == 'OLE PRIORITY: SAFETY AND DUTIES')
                          ] <- 'OLE PRIORITY:\nSAFETY AND DUTIES'

rbs_copy$OLD_OLE_CATEGORY[
  which(rbs_copy$OLD_OLE_CATEGORY == 'PROTECTED RESOURCE & PROHIBITED SPECIES')
                          ] <- 'PROTECTED RESOURCE &\nPROHIBITED SPECIES'

# Make the plot
river_oldcat <- {
  
  ggplot(data = rbs_copy %>%
           filter(OLE_SYSTEM == 'OLD'),
         aes(axis1 = STATEMENT_TYPE,
             axis2 = OLD_OLE_CATEGORY,
             y = TOTAL_STATEMENTS)) +
    geom_alluvium(aes(fill = OLD_OLE_CATEGORY),
                  show.legend = F,
                  curve_type = 'sigmoid') +
    geom_stratum() +
    geom_text_repel(aes(label = 
                          ifelse(after_stat(x) == 1 & 
                                   as.character(after_stat(stratum)) %in% 
                                   repel_labels == T, 
                                 as.character(after_stat(stratum)), 
                                 NA)),
                    stat = 'stratum', 
                    size = 5.5,
                    direction = 'y',
                    nudge_x = -0.5,
                    force = 4,
                    family = 'Gill Sans MT') +
    geom_fit_text(aes(label = ifelse(after_stat(x) == 1,
                                     as.character(after_stat(stratum)),
                                     NA)),
                  stat = 'stratum',
                  width = 0.33,
                  min.size = 7,
                  family = 'Gill Sans MT') +
    geom_fit_text(aes(label = ifelse(after_stat(x) == 2,
                                     as.character(after_stat(stratum)),
                                     NA)),
                  stat = 'stratum',
                  width = 0.5,
                  size = 15,
                  family = 'Gill Sans MT') +
    scale_x_discrete(limits = c('Statement Type', 'Old OLE Category'),
                     expand = c(0.15, 0.05)) +
    scale_fill_manual(values = colors[c(1, 3, 5, 6, 7, 9)]) +
    theme_void() 
  
}

# View the plot
river_oldcat

# Save the plot
ggsave(filename = 'Plots/river_subcat.png',
       plot = river_oldcat,
       width = 14,
       height = 10)





