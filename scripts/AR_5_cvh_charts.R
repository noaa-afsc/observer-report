# Annual Report Enforcement Chapter: Generating Plots
# Author: Cameron Van Horn
# Contact: cameron.vanhorn@noaa.gov
#          (206) 526-4222

# TODO's:
  # Filter out statement types in river plots that are under 3 total occurrences []


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
library(scales)
# clear environment
rm(list = ls())

# load data
load(file = 'C:/Users/cameron.vanhorn/Work/AR_2024_Chapter5/data_files/AR_3_rate_output.rdata')

# load fonts
# font_import() # return y or n
loadfonts()
# see list of fonts
# fonts()

########################################
##### CREATING FUNCTIONS FOR PLOTS #####
########################################
# this source explains how to add geom function calls to a ggplot within a fxn
# https://rstats-tips.net/2021/07/11/adding-lines-or-other-geoms-to-a-ggplot-by-calling-a-custom-function/

# Alluvial plots of THREE columns ----------------------------------------------
river3_theme <- function(labels, axis3_text_size = 12, axis3_min_text_size = 10,
                        size_label = 5, axis1_label, axis2_label, axis3_label) {
  list(
    geom_alluvium(aes(fill = STATEMENT_TYPE),
                  show.legend = F,
                  curve_type = 'sigmoid'),
    geom_stratum(width = 0.5),
    annotate(geom = 'text',
             label = axis1_label,
             x = 1,
             y = -7,
             size = 6,
             family = 'Gill Sans MT'),
    annotate(geom = 'text',
             label = axis2_label,
             x = 2,
             y = -7,
             size = 6,
             family = 'Gill Sans MT'),
    annotate(geom = 'text',
             label = axis3_label,
             x = 3,
             y = -7,
             size = 6,
             family = 'Gill Sans MT'),
    geom_fit_text(aes(label = ifelse(after_stat(x) == 1,
                                     as.character(after_stat(stratum)),
                                     NA)),
                  stat = 'stratum',
                  width = 0.5,
                  min.size = 7,
                  size = 18,
                  family = 'Gill Sans MT'),
    geom_fit_text(aes(label = ifelse(after_stat(x) == 2,
                                     as.character(after_stat(stratum)),
                                     NA)),
                  stat = 'stratum',
                  width = 0.5,
                  size = 15,
                  family = 'Gill Sans MT'),
    geom_label_repel(aes(label = 
                           ifelse(after_stat(x) == 3 &
                                    as.character(after_stat(stratum)) %in%
                                    labels,
                                  as.character(after_stat(stratum)),
                                  NA)),
                     stat = 'stratum',
                     size = size_label,
                     direction = 'y',
                     nudge_x = 0.5,
                     force = 3,
                     max.overlaps = 40,
                     family = 'Gill Sans MT'),
    geom_fit_text(aes(label = ifelse(after_stat(x) == 3,
                                     as.character(after_stat(stratum)),
                                     NA)),
                  stat = 'stratum',
                  width = 0.5,
                  size = axis3_text_size,
                  min.size = axis3_min_text_size,
                  family = 'Gill Sans MT'),
    scale_x_discrete(expand = c(0.15, 0.05)),
    theme_void(),
    theme(panel.background = element_rect(fill = 'white'))
  )
}

# Alluvial plots of TWO columns ------------------------------------------------
river2_theme <- function(labels, axis1_text_size = 12, axis1_min_text_size = 10,
                         size_label = 5, axis1_label, axis2_label) {
  list(
    geom_alluvium(aes(fill = STATEMENT_TYPE),
                  show.legend = F,
                  curve_type = 'sigmoid'),
    geom_stratum(width = 0.5),
    annotate(geom = 'text',
             label = axis1_label,
             x = 1,
             y = -15,
             size = 6,
             family = 'Gill Sans MT'),
    annotate(geom = 'text',
             label = axis2_label,
             x = 2,
             y = -15,
             size = 6,
             family = 'Gill Sans MT'),
    geom_fit_text(aes(label = ifelse(after_stat(x) == 1,
                                     as.character(after_stat(stratum)),
                                     NA)),
                  stat = 'stratum',
                  width = 0.5,
                  min.size = axis1_min_text_size,
                  size = axis1_text_size,
                  family = 'Gill Sans MT'),
    geom_fit_text(aes(label = ifelse(after_stat(x) == 2,
                                     as.character(after_stat(stratum)),
                                     NA)),
                  stat = 'stratum',
                  width = 0.5,
                  size = 18,
                  min.size = 7,
                  family = 'Gill Sans MT'),
    geom_label_repel(aes(label = 
                           ifelse(after_stat(x) == 1 &
                                    as.character(after_stat(stratum)) %in%
                                    labels$STATEMENT_TYPE_LABEL,
                                  as.character(after_stat(stratum)),
                                  NA)),
                     stat = 'stratum',
                     size = size_label,
                     direction = 'y',
                     nudge_x = -0.5,
                     force = 3,
                     max.overlaps = 30,
                     family = 'Gill Sans MT'),
    scale_x_discrete(expand = c(0.15, 0.05)),
    theme_void(),
    theme(panel.background = element_rect(fill = 'white'))
  )
}

################################################################
##### CREATING LABELS AND ASSIGNING COLORS FOR RIVER PLOTS #####
################################################################
# Correct spelling -------------------------------------------------------------
statements_combined$STATEMENT_TYPE[which(statements_combined$STATEMENT_TYPE == 'Inadequate Accomodations')] <- 'Inadequate Accommodations'
statements_combined$STATEMENT_TYPE[which(statements_combined$STATEMENT_TYPE == 'Catcher Processer Longline')] <- 'Catcher Processor Longline'

# Create labels for OLD_OLE_CATEGORY -------------------------------------------
# break up strings of old ole categories to fit in boxes better
# view strings to break into lines
unique(statements_combined$OLD_OLE_CATEGORY)

# create new column of labels that match OLD_OLE_CATEGORY
statements_combined$OLD_OLE_CATEGORY_LABEL <- statements_combined$OLD_OLE_CATEGORY
# break up strings
statements_combined$OLD_OLE_CATEGORY_LABEL[
  which(statements_combined$OLD_OLE_CATEGORY == 'ALL OTHER STATEMENT TYPES')
] <- 'ALL OTHER\nSTATEMENT TYPES'

statements_combined$OLD_OLE_CATEGORY_LABEL[
  which(statements_combined$OLD_OLE_CATEGORY == 'LIMITED ACCESS PROGRAMS')
] <- 'LIMITED ACCESS\nPROGRAMS'

statements_combined$OLD_OLE_CATEGORY_LABEL[
  which(statements_combined$OLD_OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL')
] <- 'OLE PRIORITY:\nINTER-PERSONAL'

statements_combined$OLD_OLE_CATEGORY_LABEL[
  which(statements_combined$OLD_OLE_CATEGORY == 'OLE PRIORITY: SAFETY AND DUTIES')
] <- 'OLE PRIORITY:\nSAFETY AND DUTIES'

statements_combined$OLD_OLE_CATEGORY_LABEL[
  which(statements_combined$OLD_OLE_CATEGORY == 'PROTECTED RESOURCE & PROHIBITED SPECIES')
] <- 'PROTECTED RESOURCE &\nPROHIBITED SPECIES'


# create ordered factors for OLD OLE CATEGORY
statements_combined$OLD_OLE_CATEGORY_LABEL <- 
  factor(statements_combined$OLD_OLE_CATEGORY_LABEL, 
         levels = c('OLE PRIORITY:\nINTER-PERSONAL',
                    'OLE PRIORITY:\nSAFETY AND DUTIES',
                    'COAST GUARD',
                    'LIMITED ACCESS\nPROGRAMS',
                    'PROTECTED RESOURCE &\nPROHIBITED SPECIES',
                    'ALL OTHER\nSTATEMENT TYPES'))

# Create labels for NEW_OLE_CATEGORY -------------------------------------------
# define which strings to break into lines
# NEW OLE CATEGORY
unique(statements_combined$NEW_OLE_CATEGORY)

# create new column of labels that match NEW_OLE_CATEGORY
statements_combined$NEW_OLE_CATEGORY_LABEL <- statements_combined$NEW_OLE_CATEGORY

# break up the strings
statements_combined$NEW_OLE_CATEGORY_LABEL[
  which(statements_combined$NEW_OLE_CATEGORY == 'GEAR/EQUIPMENT REQUIREMENTS')
] <- 'GEAR/EQUIPMENT\nREQUIREMENTS'

statements_combined$NEW_OLE_CATEGORY_LABEL[
  which(statements_combined$NEW_OLE_CATEGORY == 'OBSERVER SAFETY AND WORK ENVIRONMENT')
] <- 'OBSERVER SAFETY\nAND\nWORK ENVIRONMENT'

statements_combined$NEW_OLE_CATEGORY_LABEL[
  which(
    statements_combined$NEW_OLE_CATEGORY == 'PERMITS/DOCUMENTS/RECORD KEEPING AND REPORTING'
        )
] <- 'PERMITS, DOCUMENTS,\nRECORD KEEPING\nAND REPORTING'

statements_combined$NEW_OLE_CATEGORY_LABEL[
  which(
    statements_combined$NEW_OLE_CATEGORY == 'PROHIBITED SPECIES/MARINE MAMMALS/SEABIRDS')
] <- 'PROHIBITED SPECIES,\nMARINE MAMMALS,\nSEABIRDS'

statements_combined$NEW_OLE_CATEGORY_LABEL[
  which(statements_combined$NEW_OLE_CATEGORY == 'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION')
] <- 'SAFETY-USCG-FAIL TO CONDUCT DRILLS\nAND/OR SAFETY ORIENTATION'

statements_combined$NEW_OLE_CATEGORY_LABEL[
  which(statements_combined$NEW_OLE_CATEGORY == 'SAFETY-USCG-MARINE CASUALTY')
] <- 'SAFETY-USCG:\nMARINE CASUALTY'

# Create labels for STATEMENT_TYPES --------------------------------------------
# create column of labels that match STATEMENT TYPES (first: NEW system)
statements_combined$STATEMENT_TYPE_LABEL <- statements_combined$STATEMENT_TYPE
sort(unique(statements_combined$STATEMENT_TYPE[which(statements_combined$OLE_SYSTEM == 'NEW')]))

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'DESTRUCTION OF SAMPLE/WORK/PERSONAL EFFECTS')
] <- 'DESTRUCTION OF\nSAMPLE, WORK,\nPERSONAL EFFECTS'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'FOOD AND ACCOMMODATIONS')
] <- 'FOOD AND\nACCOMMODATIONS'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'FORCED TO PERFORM CREW DUTIES')
] <- 'FORCED TO\nPERFORM CREW DUTIES'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'ADMINISTRATIVE RESPONSIBILITIES')
] <- 'ADMINISTRATIVE\nRESPONSIBILITIES'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'BELT AND FLOW OPERATIONS')
] <- 'BELT AND\nFLOW OPERATIONS'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'DISCHARGE OF GARBAGE OR PLASTIC, OR LOSS OF FISHING GEAR')
] <- 'DISCHARGE OF GARBAGE\nOR PLASTIC, OR LOSS\nOF FISHING GEAR'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'MARINE CASUALTY')
] <- 'MARINE\nCASUALTY'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'PROHIBITED SPECIES MISHANDLING')
] <- 'PROHIBITED SPECIES\nMISHANDLING'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'PROHIBITED SPECIES RETENTION')
] <- 'PROHIBITED SPECIES\nRETENTION'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'GENERAL REPORTING REQUIREMENTS')
] <- 'GENERAL REPORTING\nREQUIREMENTS'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'INTIMIDATION/BRIBERY/COERCION')
] <- 'INTIMIDATION, BRIBERY,\nCOERCION'

statements_combined$STATEMENT_TYPE_LABEL[
  which(statements_combined$STATEMENT_TYPE == 'MONITORING THE FLOW OF FISH')
] <- 'MONITORING THE\nFLOW OF FISH'

# define statement types to label outside strata boxes
repel_labels_newcat <- statements_combined %>%
  filter(OLE_SYSTEM == 'NEW') %>%
  select(STATEMENT_TYPE)

TOTAL_STATEMENTS <- vector()
for (i in 1:length(unique(repel_labels_newcat$STATEMENT_TYPE))) {
  x <- as.numeric(colSums(
    repel_labels_newcat == unique(repel_labels_newcat$STATEMENT_TYPE)[i]))
  TOTAL_STATEMENTS <- c(TOTAL_STATEMENTS, x)
}
STATEMENT_TYPE <- unique(repel_labels_newcat$STATEMENT_TYPE)

repel_labels_newcat <- data.frame(STATEMENT_TYPE = STATEMENT_TYPE,
                                  TOTAL_STATEMENTS = TOTAL_STATEMENTS)

repel_labels_newcat <- repel_labels_newcat %>%
  filter(TOTAL_STATEMENTS < 9)


# create ordered factors for NEW OLE CATEGORY
statements_combined$NEW_OLE_CATEGORY_LABEL <- 
  factor(statements_combined$NEW_OLE_CATEGORY_LABEL,
         levels = 
           c('OBSERVER SAFETY\nAND\nWORK ENVIRONMENT',
             'INTERFERENCE WITH DUTIES',
             'MARPOL/OIL SPILL',
             'SAFETY-USCG-EQUIPMENT',
             'SAFETY-USCG-FAIL TO CONDUCT DRILLS\nAND/OR SAFETY ORIENTATION',
             'SAFETY-USCG:\nMARINE CASUALTY',
             'GEAR/EQUIPMENT\nREQUIREMENTS',
             'OPERATIONAL REQUIREMENTS',
             'PROHIBITED SPECIES,\nMARINE MAMMALS,\nSEABIRDS',
             'PERMITS, DOCUMENTS,\nRECORD KEEPING\nAND REPORTING',
             'SUSTAINABLE FISHERIES',
             'CONTRACTOR REQUIREMENTS'))

# Assign colors for old statements ---------------------------------------------
# use the factored level for alluvial plot of old data
old_statements <- {
  c('Disruptive/Bothersome Behavior - Conflict Resolved', #1
    'Harassment-Assault', #2
    'Harassment - Sexual', #3
    'Intimidation, coercion, hostile work environment', #4
    'Interference/Sample Biasing', #5
    'Safety-NMFS', #6
    'MARPOL/Oil Spill', #7
    'Safety-USCG-Equipment', #8
    'Safety-USCG-Fail to Conduct Drills', #9
    'Safety-USCG-Marine Casualty', #10
    'AFA', #11
    'Amendment 80', #12
    'Catcher Processor Longline', #13
    'IFQ Retention', #14
    'Amendment 91 salmon', #15
    'Gulf of Alaska Salmon', #16
    'Halibut Deck Sorting', #17
    'Marine Mammal-Feeding', #18
    'Marine Mammal-Harassment', #19
    'Prohibited Species - Mishandling', #20
    'Prohibited Species - Retaining', #21
    'Sample Bias-Marine Mammals', #22
    'Seabird-Harassment', #23
    'Restricted Access', #24
    'Contractor Problems', #25
    'Failure to Notify', #26
    'Inadequate Accommodations', #27
    'IR/IU', #28
    'Miscellaneous Violations', #29
    'Reasonable Assistance', #30
    'Record Keeping and Reporting' #31
  )
}

# assign colors to corresponding number in old_statements above
old_statement_colors <- {
  c('orangered', #1
    'red2', #2
    'red3', #3
    'tomato', #4
    'orange', #5
    'coral', #6
    'tan4', #7
    'yellow', #8
    'gold', #9
    'khaki', #10
    'green', #11
    'darkgreen', #12
    'greenyellow', #13
    'olivedrab', #14
    'blue', #15
    'cadetblue1', #16
    'royalblue', #17
    'turquoise', #18
    'midnightblue', #19
    'cyan', #20
    'deepskyblue', #21
    'darkturquoise', #22
    'navy', #23
    'blueviolet', #24
    'darkorchid4', #25
    'darkviolet', #26
    'purple4', #27
    'orchid4', #28
    'mediumpurple', #29
    'darkmagenta', #30
    'purple'#31
  )
}

# create column for colors and attach
statements_combined$OLD_STATEMENT_COLOR <- NA
for (i in 1:length(old_statements)) {
  rows <- which(statements_combined$STATEMENT_TYPE == old_statements[i])
  statements_combined$OLD_STATEMENT_COLOR[rows] <- old_statement_colors[i]
}


# Assign colors for new statements (safety) ------------------------------------
# use the factored level for alluvial plot of old data
new_statements_safety <- {
  c('ASSAULT', #1
    'FOOD AND\nACCOMMODATIONS', #2
    'FORCED TO\nPERFORM CREW DUTIES', #3
    'HOSTILE WORK ENVIRONMENT', #4
    'IMPEDIMENT', #5
    'INTIMIDATION, BRIBERY,\nCOERCION', #6
    'SAFETY', #7
    'SEXUAL HARASSMENT', #8
    'ACCESS', #9
    'DESTRUCTION OF\nSAMPLE, WORK,\nPERSONAL EFFECTS', #10
    'NOTIFICATION', #11
    'REASONABLE ASSISTANCE', #12
    'SAMPLING INTERFERENCE', #13
    'EPIRB', #14
    'GENERAL SAFETY EQUIPMENT', #15
    'SURVIVAL CRAFT', #16
    'FAILURE TO CONDUCT DRILLS', #17
    'MARINE\nCASUALTY'# 18
    )
}

# assign colors to corresponding number in new_statements_safety above
new_statement_safety_colors <- {
  c('red', #1
    'tomato', #2
    'indianred', #3
    'firebrick', #4
    'orangered', #5
    'darkred', #6
    'orange', #7
    'red2', #8
    'green', #9
    'blue', #10
    'seagreen', #11
    'turquoise', #12
    'palegreen', #13
    'royalblue', #14
    'yellow', #15
    'gold', #16
    'khaki', #17
    'navy' #18
  )
}

# create column for colors and attach
statements_combined$NEW_STATEMENT_SAFETY_COLOR <- NA
for (i in 1:length(new_statements_safety)) {
  rows <- which(statements_combined$STATEMENT_TYPE_LABEL == new_statements_safety[i])
  statements_combined$NEW_STATEMENT_SAFETY_COLOR[rows] <- new_statement_safety_colors[i]
}

# Assign colors for new statements (non-safety) --------------------------------
# use the factored level for alluvial plot of old data
new_statements_nonsafety <- {
  c('DISCHARGE OF GARBAGE\nOR PLASTIC, OR LOSS\nOF FISHING GEAR', #1
    'DISCHARGE OF OIL', #2
    'BIN MONITORING', #3
    'OBSERVER SAMPLING STATION', #4 
    'SCALES', #5
    'VIDEO MONITORING SYSTEM', #6
    'BELT AND\nFLOW OPERATIONS', #7
    'CATCH WEIGHING', #8
    'CMCP', #9
    'DATA TRANSMISSION', #10
    'MONITORING THE\nFLOW OF FISH', #11
    'OBSERVER COVERAGE', #12 
    'OPERATIONAL LINE', #13
    'TIMELY NOTIFICATION', #14
    'BSAI SALMON BYCATCH', #15
    'GOA SALMON BYCATCH', #16
    'HALIBUT DECK SORTING', #17
    'MARINE MAMMAL', #18
    'PROHIBITED SPECIES\nMISHANDLING', #19
    'PROHIBITED SPECIES\nRETENTION', #20
    'FALSE REPORTING', #21
    'GENERAL REPORTING\nREQUIREMENTS', #22
    'IFQ PERMIT', #23
    'INSPECTION REPORTS', #24
    'UNLAWFUL DISCARD', #25
    'ADMINISTRATIVE\nRESPONSIBILITIES', #26
    'DEPLOYMENT LOGISTICS' #27
    )
}

# assign colors to corresponding number in new_statements_nonsafety above
new_statement_nonsafety_colors <- {
  c('brown', #1
    'chocolate', #2
    'green', #3
    'forestgreen', #4
    'lawngreen', #5
    'darkgreen', #6
    'yellow', #7
    'orange', #8
    'gold', #9
    'orangered', #10
    'darkorange', #11
    'khaki', #12
    'coral', #13
    'goldenrod', #14
    'blue', #15
    'turquoise', #16
    'seagreen', #17
    'navy', #18
    'cyan', #19
    'royalblue4', #20
    'blueviolet', #21
    'darkorchid4', #22
    'purple', #23
    'magenta3', #24
    'turquoise3', #25
    'wheat', #26
    'tan' #27
    )
}

# create column for colors and attach
statements_combined$NEW_STATEMENT_NONSAFETY_COLOR <- NA
for (i in 1:length(new_statements_nonsafety)) {
  rows <- which(statements_combined$STATEMENT_TYPE_LABEL == new_statements_nonsafety[i])
  statements_combined$NEW_STATEMENT_NONSAFETY_COLOR[rows] <- new_statement_nonsafety_colors[i]
}

#####################
##### ALL PLOTS #####
#####################
# Rain cloud plot of violations from statements_combined -----------------------
# Set colors
colors <- nmfs_palette('regional')(6)

# Make the plot
OPS_number_rainplot <- {
  
  ggplot(data = statements_combined,
         aes(x = factor(FIRST_VIOL_YEAR,
                        levels = c(2022, 2023)),
             y = NUMBER_VIOLATIONS,
             fill = interaction(FIRST_VIOL_YEAR, OLE_SYSTEM),
             color = interaction(FIRST_VIOL_YEAR, OLE_SYSTEM))) +
    labs(x = 'Year',
         y = 'Occurrences per Statement',
         title = 'All Categories') +
    facet_grid(. ~ factor(OLE_SYSTEM,
                          levels = c('OLD', 'NEW')), 
               scales = 'free',) +
    force_panelsizes(cols = c(0.5, 0.3),) +
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
    scale_fill_manual(values = colors[c(2, 3, 1)],
                      guide = 'none') +
    scale_color_manual(values = colors[c(2, 3, 1)],
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
         aes(x = factor(FIRST_VIOL_YEAR,
                        levels = c(2022, 2023)),
             y = NUMBER_VIOLATIONS,
             fill = interaction(FIRST_VIOL_YEAR, OLE_SYSTEM),
             color = interaction(FIRST_VIOL_YEAR, OLE_SYSTEM))) +
    labs(x = 'Year',
         y = 'Occurrences per Statement',
         title = 'OLE Priority: Inter-Personal') +
    facet_grid(. ~ factor(OLE_SYSTEM,
                          levels = c('OLD', 'NEW')), 
               scales = 'free',) +
    force_panelsizes(cols = c(0.5, 0.3),) +
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
    scale_fill_manual(values = colors[c(2, 3, 1)],
                      guide = 'none') +
    scale_color_manual(values = colors[c(2, 3, 1)],
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


# Alluvial plot of old data, 2023 ----------------------------------------------
# flow: STATEMENT_TYPE -> OLD_OLE_CATEGORY (filtered for OLD OLE SYSTEM)
# LABELS WITH < 3 STATEMENTS:
  # Contractor Problems, Harassment-Assault, Marine Mammal-Feeding,
  # Marine Mammal-Harassment, Prohibited Species - Retaining, 
  # Sample Bias-Marine Mammals, Seabird-Harassment

# Set labels
river_oldcat_labels_23 <- statements_combined %>%
  filter(OLE_SYSTEM == 'OLD',
         MANUAL_YEAR == 2023) %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 10)

# get colors
old_colors <- {
  statements_combined %>%
    filter(OLE_SYSTEM == 'OLD',
           MANUAL_YEAR == 2023) %>% 
    group_by(STATEMENT_TYPE, OLD_STATEMENT_COLOR) %>%
    summarize() %>%
    ungroup() %>%
    mutate(STATEMENT_TYPE = tolower(STATEMENT_TYPE)) %>%
    arrange(STATEMENT_TYPE)
}

# Make the plot
river_oldcat_23 <- {
  ggplot(data = statements_combined %>%
           filter(OLE_SYSTEM == 'OLD',
                  MANUAL_YEAR == 2023),
         aes(axis1 = factor(STATEMENT_TYPE_LABEL,
                            levels = c('Disruptive/Bothersome Behavior - Conflict Resolved',
                                       'Harassment-Assault', 'Harassment - Sexual',
                                       'Intimidation, coercion, hostile work environment',
                                       'Interference/Sample Biasing', 'Safety-NMFS',
                                       'MARPOL/Oil Spill', 'Safety-USCG-Equipment',
                                       'Safety-USCG-Fail to Conduct Drills',
                                       'Safety-USCG-Marine Casualty', 'AFA',
                                       'Amendment 80', 'Catcher Processor Longline',
                                       'IFQ Retention', 'Amendment 91 salmon',
                                       'Gulf of Alaska Salmon', 
                                       'Halibut Deck Sorting',
                                       'Marine Mammal-Feeding', 
                                       'Marine Mammal-Harassment',
                                       'Prohibited Species - Mishandling',
                                       'Prohibited Species - Retaining',
                                       'Sample Bias-Marine Mammals',
                                       'Seabird-Harassment', 
                                       'Restricted Access',
                                       'Contractor Problems', 
                                       'Failure to Notify', 
                                       'Inadequate Accommodations', 'IR/IU',
                                       'Miscellaneous Violations',
                                       'Reasonable Assistance', 
                                       'Record Keeping and Reporting')),
             axis2 = OLD_OLE_CATEGORY_LABEL)) +
    river2_theme(labels = river_oldcat_labels_23,
                 axis1_label = 'Old Statement Type',
                 axis2_label = 'Old OLE Category',
                 axis1_min_text_size = 9,
                 axis1_text_size = 18) +
    scale_fill_manual(values = old_colors$OLD_STATEMENT_COLOR)
}

# View the plot
river_oldcat_23

# Save the plot
ggsave(filename = 'Plots/river_oldcat_23.png',
       plot = river_oldcat_23,
       width = 14,
       height = 10)


# Alluvial plot of new data, by safety -----------------------------------------
# flow: OLD_OLE_CATEGORY -> NEW_OLE_CATEGORY_STATEMENT_TYPE (filtered for
# NEW OLE SYSTEM)
# LABELS WITH < 3 STATEMENTS:
  # ACCESS, ASSAULT, FORCED TO PERFORM CREW DUTIES, EPIRB

# Set labels
river_newcat_safety_labels <- statements_combined %>%
  filter(OLE_SYSTEM == 'NEW',
         NEW_OLE_CATEGORY %in%
           c('OBSERVER SAFETY AND WORK ENVIRONMENT',
             'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
             'SAFETY-USCG-MARINE CASUALTY',
             'SAFETY-USCG-EQUIPMENT',
             'INTERFERENCE WITH DUTIES')) %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 6) 

# get colors
safety_colors <- {
  statements_combined %>%
    filter(OLE_SYSTEM == 'NEW',
           NEW_OLE_CATEGORY %in%
             c('OBSERVER SAFETY AND WORK ENVIRONMENT',
               'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
               'SAFETY-USCG-MARINE CASUALTY',
               'SAFETY-USCG-EQUIPMENT',
               'INTERFERENCE WITH DUTIES')) %>% 
    group_by(STATEMENT_TYPE_LABEL, NEW_STATEMENT_SAFETY_COLOR) %>%
    summarize() %>%
    ungroup() %>%
    mutate(STATEMENT_TYPE_LABEL = tolower(STATEMENT_TYPE_LABEL)) %>%
    arrange(STATEMENT_TYPE_LABEL)
}

# Make the plot
river_newcat_safety <- {
  ggplot(data = statements_combined %>%
           filter(OLE_SYSTEM == 'NEW',
                  NEW_OLE_CATEGORY %in% 
                    c('OBSERVER SAFETY AND WORK ENVIRONMENT',
                      'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
                      'SAFETY-USCG-MARINE CASUALTY',
                      'SAFETY-USCG-EQUIPMENT',
                      'INTERFERENCE WITH DUTIES')),
         aes(axis1 = OLD_OLE_CATEGORY_LABEL,
             axis2 = NEW_OLE_CATEGORY_LABEL,
             axis3 = factor(STATEMENT_TYPE_LABEL,
                            levels = c('ASSAULT', 'FOOD AND\nACCOMMODATIONS',
                                       'FORCED TO\nPERFORM CREW DUTIES',
                                       'HOSTILE WORK ENVIRONMENT', 'IMPEDIMENT',
                                       'INTIMIDATION, BRIBERY,\nCOERCION',
                                       'SAFETY', 'SEXUAL HARASSMENT', 
                                       'ACCESS', 
                                       'DESTRUCTION OF\nSAMPLE, WORK,\nPERSONAL EFFECTS',
                                       'NOTIFICATION', 'REASONABLE ASSISTANCE', 
                                       'SAMPLING INTERFERENCE', 'EPIRB', 
                                       'GENERAL SAFETY EQUIPMENT', 
                                       'SURVIVAL CRAFT', 
                                       'FAILURE TO CONDUCT DRILLS',
                                       'MARINE\nCASUALTY')))) +
    river3_theme(labels = river_newcat_safety_labels$STATEMENT_TYPE_LABEL,
                 axis1_label = 'Old OLE Category',
                 axis2_label = 'New OLE Category',
                 axis3_label = 'New Statement Type',
                 size_label = 5,
                 axis3_text_size = 18,
                 axis3_min_text_size = 8) +
    scale_fill_manual(values = safety_colors$NEW_STATEMENT_SAFETY_COLOR)
}

# View the plot
river_newcat_safety

# Save the plot
ggsave(filename = 'Plots/river_newcat_safety.png',
       plot = river_newcat_safety,
       width = 20,
       height = 10)


# Alluvial plot of new data, by nonsafety --------------------------------------
# flow: OLD_OLE_CATEGORY -> NEW_OLE_CATEGORY_STATEMENT_TYPE (filtered for
# NEW OLE SYSTEM)
# LABELS WITH < 3 STATEMENTS:
  # ADMINISTRATIVE RESPONSIBILITIES, DATA TRANSMISSION, OBSERVER COVERAGE,
  # OPERATIONAL LINE, TIMELY NOTIFICATION, IFQ PERMIT, INSPECTION REPORTS,
  # HALIBUT DECK SORTING
# Set labels
river_newcat_nonsafety_labels <- statements_combined %>%
  filter(OLE_SYSTEM == 'NEW',
         !NEW_OLE_CATEGORY %in%
           c('OBSERVER SAFETY AND WORK ENVIRONMENT',
             'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
             'SAFETY-USCG-MARINE CASUALTY',
             'SAFETY-USCG-EQUIPMENT',
             'INTERFERENCE WITH DUTIES'),
         !STATEMENT_TYPE_LABEL == 'DISCHARGE OF OIL',
         !STATEMENT_TYPE_LABEL == 'BIN MONITORING',
         !STATEMENT_TYPE_LABEL == 'GOA SALMON BYCATCH') %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 8)

# get colors
nonsafety_colors <- {
  statements_combined %>%
    filter(OLE_SYSTEM == 'NEW',
           !NEW_OLE_CATEGORY %in%
             c('OBSERVER SAFETY AND WORK ENVIRONMENT',
               'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
               'SAFETY-USCG-MARINE CASUALTY',
               'SAFETY-USCG-EQUIPMENT',
               'INTERFERENCE WITH DUTIES')) %>% 
    group_by(STATEMENT_TYPE_LABEL, NEW_STATEMENT_NONSAFETY_COLOR) %>%
    summarize() %>%
    ungroup() %>%
    mutate(STATEMENT_TYPE_LABEL = tolower(STATEMENT_TYPE_LABEL)) %>%
    arrange(STATEMENT_TYPE_LABEL)
}


# Make the plot
river_newcat_nonsafety <- {
  ggplot(data = statements_combined %>%
           filter(OLE_SYSTEM == 'NEW',
                  !NEW_OLE_CATEGORY %in% 
                    c('OBSERVER SAFETY AND WORK ENVIRONMENT',
                      'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
                      'SAFETY-USCG-MARINE CASUALTY',
                      'SAFETY-USCG-EQUIPMENT',
                      'INTERFERENCE WITH DUTIES')),
         aes(axis1 = OLD_OLE_CATEGORY_LABEL,
             axis2 = NEW_OLE_CATEGORY_LABEL,
             axis3 = factor(STATEMENT_TYPE_LABEL,
                            levels = c('DISCHARGE OF GARBAGE\nOR PLASTIC, OR LOSS\nOF FISHING GEAR',
                                       'DISCHARGE OF OIL', 'BIN MONITORING', 
                                       'OBSERVER SAMPLING STATION', 'SCALES',
                                       'VIDEO MONITORING SYSTEM', 
                                       'BELT AND\nFLOW OPERATIONS', 
                                       'CATCH WEIGHING', 'CMCP', 
                                       'DATA TRANSMISSION',
                                       'MONITORING THE\nFLOW OF FISH', 
                                       'OBSERVER COVERAGE', 'OPERATIONAL LINE',
                                       'TIMELY NOTIFICATION', 
                                       'BSAI SALMON BYCATCH', 
                                       'GOA SALMON BYCATCH', 
                                       'HALIBUT DECK SORTING', 'MARINE MAMMAL',
                                       'PROHIBITED SPECIES\nMISHANDLING',
                                       'PROHIBITED SPECIES\nRETENTION',
                                       'FALSE REPORTING', 
                                       'GENERAL REPORTING\nREQUIREMENTS', 
                                       'IFQ PERMIT', 'INSPECTION REPORTS',
                                       'UNLAWFUL DISCARD', 
                                       'ADMINISTRATIVE\nRESPONSIBILITIES',
                                       'DEPLOYMENT LOGISTICS')))) +
    river3_theme(labels = river_newcat_nonsafety_labels$STATEMENT_TYPE_LABEL,
                 axis1_label = 'Old OLE Category',
                 axis2_label = 'New OLE Category',
                 axis3_label = 'New Statement Type',
                 size_label = 5,
                 axis3_text_size = 18,
                 axis3_min_text_size = 10) +
    scale_fill_manual(values = nonsafety_colors$NEW_STATEMENT_NONSAFETY_COLOR)
}

# View the plot
river_newcat_nonsafety

# Save the plot
ggsave(filename = 'Plots/river_newcat_nonsafety.png',
       plot = river_newcat_nonsafety,
       width = 20,
       height = 10)

######################################
##### CREATE TABLES OF PLOT DATA #####
######################################
# Table of upper 1% incidents per statement  -----------------------------------
high_viol <- statements_combined %>% 
               filter(NUMBER_VIOLATIONS >= 
                        quantile(statements_combined$NUMBER_VIOLATIONS, 0.99),
                      FIRST_VIOL_YEAR == 2023)

write.csv(high_viol, 'high_violations_per_statement.csv')

# Table of Occurrences per Statement (aggregated by Statement Type) ------------
ops_proport <- {
  statements_combined %>%
    group_by(NUMBER_VIOLATIONS, OLE_SYSTEM, FIRST_VIOL_YEAR) %>%
    summarize(FREQ = n()) %>%
    group_by(OLE_SYSTEM, FIRST_VIOL_YEAR) %>%
    mutate(TOTAL = sum(FREQ),
           PROPORT_STATEMENTS = FREQ / TOTAL)
}

write.csv(ops_proport, 'ops_proportions.csv')

# Table of Occurrences per Statement (by Statement Type, OLEPIP cats only) -----
olepip_ops_proport <- {
  statements_combined %>%
    filter(OLD_OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL') %>%
    group_by(NUMBER_VIOLATIONS, OLE_SYSTEM, FIRST_VIOL_YEAR) %>%
    summarize(FREQ = n()) %>%
    group_by(OLE_SYSTEM, FIRST_VIOL_YEAR) %>%
    mutate(TOTAL = sum(FREQ),
           PROPORT_STATEMENTS = FREQ / TOTAL)
}

write.csv(olepip_ops_proport, 'ops_olepip_proportions.csv')

# Table of Old OLE System, 2023 data -------------------------------------------
oldole_2023_data <- {
  statements_combined %>%
    filter(OLE_SYSTEM == 'OLD',
           FIRST_VIOL_YEAR == 2023) %>%
    group_by(OLD_OLE_CATEGORY, STATEMENT_TYPE) %>%
    summarize(FREQ = n())
}

write.csv(oldole_2023_data, 'Old_OLE_2023_summary.csv')

# Table of New OLE System, 2023 data -------------------------------------------
newole_2023_data <- {
  statements_combined %>%
    filter(OLE_SYSTEM == 'NEW') %>%
    group_by(NEW_OLE_CATEGORY, STATEMENT_TYPE) %>%
    summarize(FREQ = n())
}

write.csv(newole_2023_data, 'New_OLE_2023_summary.csv')

#####################
##### OLD PLOTS #####
#####################
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

# Set labels
river_oldcat_labels <- statements_combined %>%
  filter(OLE_SYSTEM == 'OLD') %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 26)

# Make the plot
river_oldcat <- {
  ggplot(data = statements_combined %>%
           filter(OLE_SYSTEM == 'OLD'),
         aes(axis1 = STATEMENT_TYPE_LABEL,
             axis2 = OLD_OLE_CATEGORY_LABEL)) +
    river2_theme(labels = river_oldcat_labels,
                 axis1_label = 'Old Statement Type',
                 axis2_label = 'Old OLE Category',
                 axis1_min_text_size = 9,
                 axis1_text_size = 18)
}

# View the plot
river_oldcat

# Save the plot
ggsave(filename = 'Plots/river_oldcat.png',
       plot = river_oldcat,
       width = 14,
       height = 10)


# Alluvial plot of old data, 2022 ----------------------------------------------
# flow: STATEMENT_TYPE -> OLD_OLE_CATEGORY (filtered for OLD OLE SYSTEM)

# Set labels
river_oldcat_labels_22 <- statements_combined %>%
  filter(OLE_SYSTEM == 'OLD',
         MANUAL_YEAR == 2022) %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 22)

# Make the plot
river_oldcat_22 <- {
  ggplot(data = statements_combined %>%
           filter(OLE_SYSTEM == 'OLD',
                  MANUAL_YEAR == 2022),
         aes(axis1 = STATEMENT_TYPE_LABEL,
             axis2 = OLD_OLE_CATEGORY_LABEL)) +
    river2_theme(labels = river_oldcat_labels_22,
                 axis1_label = 'Old Statement Type',
                 axis2_label = 'Old OLE Category',
                 axis1_min_text_size = 9)
}

# View the plot
river_oldcat_22

# Save the plot
ggsave(filename = 'Plots/river_oldcat_22.png',
       plot = river_oldcat_22,
       width = 14,
       height = 10)


# Alluvial plot of all data, only from ALL OTHER STATEMENTS --------------------
# define statements to repel
river_newcat_AOST_labels <- statements_combined %>%
  filter(OLD_OLE_CATEGORY == 'ALL OTHER STATEMENT TYPES',
         OLE_SYSTEM == 'NEW') %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 6)

# Make the plot
river_all_other_statements <- {
  ggplot(data = statements_combined %>%
           filter(OLD_OLE_CATEGORY == 'ALL OTHER STATEMENT TYPES',
                  OLE_SYSTEM == 'NEW'),
         aes(axis1 = OLD_OLE_CATEGORY_LABEL,
             axis2 = NEW_OLE_CATEGORY_LABEL,
             axis3 = STATEMENT_TYPE_LABEL)) +
    river3_theme(labels = river_newcat_AOST_labels$STATEMENT_TYPE_LABEL,
                 axis1_label = 'Old OLE Category',
                 axis2_label = 'New OLE Category',
                 axis3_label = 'New Statement Type')
}

# View the plot
river_all_other_statements

# Save the plot
ggsave(filename = 'Plots/river_all_other_statement_types.png',
       plot = river_all_other_statements,
       width = 20,
       height = 10)

# Alluvial plot of new data ----------------------------------------------------
# flow: OLD_OLE_CATEGORY -> NEW_OLE_CATEGORY_STATEMENT_TYPE (filtered for
# NEW OLE SYSTEM)

# Set labels
river_newcat_labels <- statements_combined %>%
  filter(OLE_SYSTEM == 'NEW') %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 9) 

# Make the plot
river_newcat <- {
  ggplot(data = statements_combined %>%
           filter(OLE_SYSTEM == 'NEW'),
         aes(axis1 = OLD_OLE_CATEGORY_LABEL,
             axis2 = NEW_OLE_CATEGORY_LABEL,
             axis3 = STATEMENT_TYPE_LABEL)) +
    river3_theme(labels = river_newcat_labels$STATEMENT_TYPE_LABEL, 
                 axis1_label = 'Old OLE Category',
                 axis2_label = 'New OLE Category',
                 axis3_label = 'New Statement Type',
                 size_label = 3)
}

# View the plot
river_newcat

# Save the plot
ggsave(filename = 'Plots/river_newcat.png',
       plot = river_newcat,
       width = 20,
       height = 10)


# Facet of rates of occurrences across OLE System/Year and subcat in SASHI -----
### UNFINISHED ###
ggplot(data = rate_by_subcat %>%
         filter(OLD_OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL' |
                  OLD_OLE_CATEGORY == 'OLE PRIORITY: SAFETY AND DUTIES'),
       aes(x = 1,
           y = 1,
           fill = INCIDENTS_PER_1000_DEPLOYED_DAYS)) +
  geom_tile() +
  facet_nested(OLE_SYSTEM + CALENDAR_YEAR ~ OLD_OLE_CATEGORY + STATEMENT_TYPE)














# Rain cloud plot of violations, facet by system -------------------------------
OPS_number_rainplot_system <- {
  
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