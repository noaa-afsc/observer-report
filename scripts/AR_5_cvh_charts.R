# Annual Report Enforcement Chapter: Generating Plots
# Author: Cameron Van Horn
# Contact: cameron.vanhorn@noaa.gov
#          (206) 526-4222

# TODO's:
  # Fix label wraps (ultimately reduces lines) []
  # Separate script for unused materials? []
  # add google drive script for downloading data []
  # add google drive script for outputting plots []
  # delete tables from repo []
  # remove unused libraries []

##################################
##### LOAD PACKAGES AND DATA #####
##################################
# Libraries --------------------------------------------------------------------
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
library(readxl)
library(tidyverse)
library(patchwork)

# Data -------------------------------------------------------------------------
# * chng wd filepath as needed *
rm(list = ls())

# adp_yr is the year of the annual report we are doing this time 
  # (annual_deployment_year)
# NOTE: we need this to ensure we load the CORRECT YEAR.  
  # Each year has it's own directory and Rdata files.
adp_yr <- rstudioapi::showPrompt(title = "ADP YEAR", message = "Enter the ADP YEAR for this analysis:", default = "")

# Set the filepath, change to your own local as needed AND COMMENT OUT FILE PATH
  # OF OTHER RESEARCHERS ATTACHED TO THE PROJECT
# MUST BE OUTSIDE wd, because we cannot have "data" on the GitHub site.
# Rdata_files_path <- paste0("C:/Users/andy.kingham/Work/Analytical Projects/Projects/Statement_redesign/Annual_Report/RData_files/", adp_yr, "/")
Rdata_files_path <- "C:/Users/Cameron.VanHorn/Work/AR_2024_Chapter5/data_files/"


# Pull Rdata file from google drive.
# NOTE: if the google drive file has not changed, the next 2 steps are not 
  # necessary: you can just load from your local.

# Identify the g-drive file to download
# MAKE SURE IT IS CORRECT GOOGLE PATH

# Folder name is below, commented out, because it is slow.as.eff. when executed 
  # this way.
# MUCH faster to use the hard-coded drive ID (see below)

# project_dribble <- googledrive::drive_get(paste0("FMA Analysis Group/FMA OLE Statements Project/FMA OLE Statements AR ch 5 Rdata files/",
#                                                 adp_yr))



## BEGIN UNCOMMENT BELOW IF YOU NEED TO GO GET THE Rdata FILE FROM G-DRIVE

# project_dribble <- googledrive::drive_get(googledrive::as_id("10Qtv5PNIgS9GhmdhSPLOYNgBgn3ykwEA"))
# 
# data_dribble <-
#   drive_ls(project_dribble) %>%
#   filter(name == "AR_3_rate_output.rdata")
# 
# # Download the file from g-drive into local
# drive_download(
#   data_dribble,
#   path = paste0(Rdata_files_path, "AR_3_rate_output.rdata"),
#   overwrite = T
# )

## END UNCOMMENT HERE IF YOU NEED TO GO GET THE Rdata FILE FROM G-DRIVE

load(file = paste0(Rdata_files_path, "AR_3_rate_output.Rdata"))



# load ODDS data from google sheet
# the ORIGINAL spreadsheet can be found at this link:
  # https://docs.google.com/spreadsheets/d/17BsqLarIh-8gqu9W5VWiEvFTjjnzaK-R48TXweXc2ag/edit?usp=sharing
  # here we are using a COPY of this spreadsheet, made 4/17/2024

# to properly load the ODDS data, we need to create the unique dribble ID
  # THIS IS UNIQUE TO EACH USER SINCE THE DATA IS NOT HOUSED IN A SHARED FOLDER
  # EDIT THE FOLLOWING LINE ACCORDING TO WHERE THE DATA IS LOCATED IN YOUR DRIVE
project_dribble <- googledrive::drive_get("2023 Observer Program Annual Report/Chap 5 - Enforcement & Compliance/possible_trips_not_logged_or_logged_incorrectly_copy")

# extract the ID of the file (if there are multiple, select 1 since there should
  # be no difference among the files (I'm not certain though))
id <- as.character(project_dribble$id[1])

# specify this file 
data_dribble <- googledrive::drive_get(googledrive::as_id(id))

# download the file from g-drive into local
drive_download(
  data_dribble,
  path = paste0(Rdata_files_path, "possible_trips_not_logged_or_logged_incorrectly.xlsx"),
  overwrite = T
)

# format the excel sheet to function within R
  # IMPORTANT: comment out the below code and recreate the function call with
  # YOUR SPECIFIC FILE PATH
# the map function allows for multiple sheets to be clumped into one list
odds_data <- excel_sheets(path = 'C:/Users/cameron.vanhorn/Work/AR_2024_Chapter5/data_files/possible_trips_not_logged_or_logged_incorrectly.xlsx') %>%
  map(~read_xlsx(path = 'C:/Users/cameron.vanhorn/Work/AR_2024_Chapter5/data_files/possible_trips_not_logged_or_logged_incorrectly.xlsx',.))

# create dataset for 2023
  # the number corresponds to the sheet # in the spreadsheet
  # 2024 is the first (1) item in the list
odds_2023 <- odds_data[[2]]
# create dataset for 2022
odds_2022 <- odds_data[[3]]

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
    'FORCED TO\nPERFORM CREW DUTIES', #2
    'HOSTILE WORK ENVIRONMENT', #3
    'IMPEDIMENT', #4
    'INTIMIDATION, BRIBERY,\nCOERCION', #5
    'SEXUAL HARASSMENT', #6
    'SAFETY', #7
    'FOOD AND\nACCOMMODATIONS', #8
    'DESTRUCTION OF\nSAMPLE, WORK,\nPERSONAL EFFECTS', #9
    'SAMPLING INTERFERENCE', #10
    'ACCESS', #11
    'NOTIFICATION', #12
    'REASONABLE ASSISTANCE', #13
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
    'red2', #6
    'orange', #7
    'darkred', #8
    'blue', #9
    'green', #10
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
ggsave(filename = '2023_outputs/charts_and_tables/Plots/OPS_number_rainplot.png',
       plot = OPS_number_rainplot,
       width = 9,
       height = 8)


# Rain cloud plots of OLE Priority IP ------------------------------------------
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
ggsave(filename = '2023_outputs/charts_and_tables/Plots/OLEPIP_number_rainplot.png',
       plot = OLEPIP_number_rainplot,
       width = 9,
       height = 8)


# Facet rain cloud plots -------------------------------------------------------
# Wrap the plots
raincloud_facet <- 
  (OPS_number_rainplot + theme(axis.text.x = element_blank(),
                               axis.ticks.x = element_blank())) / 
  (OLEPIP_number_rainplot) +
  plot_layout(axis_titles = 'collect')

# View the wrap
raincloud_facet

# Save the wrap
ggsave(filename = '2023_outputs/charts_and_tables/Plots/OPS_category_facet.png',
       plot = raincloud_facet,
       width = 9,
       height = 14)

# Alluvial plot of old data, 2023 ----------------------------------------------
# flow: STATEMENT_TYPE -> OLD_OLE_CATEGORY (filtered for OLD OLE SYSTEM)

# Set labels
river_oldcat_labels_23 <- {
  statements_combined %>%
  filter(OLE_SYSTEM == 'OLD',
         FIRST_VIOL_YEAR == 2023) %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 10)
}

# get colors
old_colors <- {
  statements_combined %>%
    filter(OLE_SYSTEM == 'OLD',
           FIRST_VIOL_YEAR == 2023) %>% 
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
                  FIRST_VIOL_YEAR == 2023),
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
ggsave(filename = '2023_outputs/charts_and_tables/Plots/river_oldcat_23.png',
       plot = river_oldcat_23,
       width = 14,
       height = 10)


# Alluvial plot of new data, by safety -----------------------------------------
# flow: OLD_OLE_CATEGORY -> NEW_OLE_CATEGORY_STATEMENT_TYPE (filtered for
# NEW OLE SYSTEM)

# Set labels
river_newcat_safety_labels <- {
  statements_combined %>%
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
}

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
                            levels = c('ASSAULT', 
                                       'FORCED TO\nPERFORM CREW DUTIES',
                                       'HOSTILE WORK ENVIRONMENT', 'IMPEDIMENT',
                                       'INTIMIDATION, BRIBERY,\nCOERCION',
                                       'SEXUAL HARASSMENT', 
                                       'SAFETY', 'FOOD AND\nACCOMMODATIONS',
                                       'DESTRUCTION OF\nSAMPLE, WORK,\nPERSONAL EFFECTS',
                                       'SAMPLING INTERFERENCE', 
                                       'ACCESS', 'NOTIFICATION', 
                                       'REASONABLE ASSISTANCE', 'EPIRB', 
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
ggsave(filename = '2023_outputs/charts_and_tables/Plots/river_newcat_safety.png',
       plot = river_newcat_safety,
       width = 20,
       height = 10)


# Alluvial plot of new data, by nonsafety --------------------------------------
# flow: OLD_OLE_CATEGORY -> NEW_OLE_CATEGORY_STATEMENT_TYPE (filtered for
# NEW OLE SYSTEM)

# Set labels
river_newcat_nonsafety_labels <- {
  statements_combined %>%
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
}

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
ggsave(filename = '2023_outputs/charts_and_tables/Plots/river_newcat_nonsafety.png',
       plot = river_newcat_nonsafety,
       width = 20,
       height = 10)

# Pareto plot of ODDS trips not logged / logged incorrectly: 2022 --------------
# Format the data
summarized_odds_2022 <- odds_2022 %>%
  rename(ISSUE_CATEGORY = `Issue Category`) %>%
  group_by(ISSUE_CATEGORY) %>%
  summarize(FREQ = n()) %>%
  arrange(desc(FREQ)) %>%
  mutate(PROPORT_FREQ = FREQ / nrow(odds_2022),
         CUMULATIVE_PROPORT = cumsum(PROPORT_FREQ),
         CUMULATIVE_SUM = cumsum(FREQ),
         ISSUE_CATEGORY = str_wrap(ISSUE_CATEGORY, width = 20),
         YEAR = 2022)


# get colors
colors <- nmfs_palette('oceans')(6)

# Plot the data
pareto_2022 <- {
  ggplot(data = summarized_odds_2022,
         aes(x = factor(ISSUE_CATEGORY,
                        levels = ISSUE_CATEGORY))) +
    labs(x = 'Issue Category',
         y = 'Proportion of All Issues',
         title = 'ODDS Trips Not Logged or Logged Incorrectly (2022)') +
    geom_bar(aes(y = PROPORT_FREQ),
             stat = 'identity',
             fill = colors[1],
             color = colors[6]) +
    geom_text(aes(label = FREQ,
                  y = PROPORT_FREQ),
              vjust = -1,
              family = 'Gill Sans MT',
              size = 5) +
    geom_point(aes(y = CUMULATIVE_PROPORT),
               color = colors[5],
               size = 3) +
    geom_path(aes(y = CUMULATIVE_PROPORT,
                  group = 1),
              color = colors[5],
              lty = 9,
              linewidth = 1) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 20, family = 'Gill Sans MT')) 
}

# View the plot
pareto_2022


# Pareto plot of ODDS trips not logged / logged incorrectly: 2023 --------------
# Format the data
summarized_odds_2023 <- odds_2023 %>%
  rename(ISSUE_CATEGORY = `Issue Category`) %>%
  group_by(ISSUE_CATEGORY) %>%
  summarize(FREQ = n()) %>%
  arrange(desc(FREQ)) %>%
  mutate(PROPORT_FREQ = FREQ / nrow(odds_2023),
         CUMULATIVE_PROPORT = cumsum(PROPORT_FREQ),
         CUMULATIVE_SUM = cumsum(FREQ),
         ISSUE_CATEGORY = str_wrap(ISSUE_CATEGORY, width = 20),
         YEAR = 2023)


# get colors
colors <- nmfs_palette('oceans')(6)

# Plot the data
pareto_2023 <- {
  ggplot(data = summarized_odds_2023,
         aes(x = factor(ISSUE_CATEGORY,
                        levels = ISSUE_CATEGORY))) +
    labs(x = 'Issue Category',
         y = 'Proportion of All Issues',
         title = 'ODDS Trips Not Logged or Logged Incorrectly (2023)') +
    geom_bar(aes(y = PROPORT_FREQ),
             stat = 'identity',
             fill = colors[1],
             color = colors[6]) +
    geom_text(aes(label = FREQ,
                  y = PROPORT_FREQ),
              vjust = -1,
              family = 'Gill Sans MT',
              size = 5) +
    geom_point(aes(y = CUMULATIVE_PROPORT),
               color = colors[5],
               size = 3) +
    geom_path(aes(y = CUMULATIVE_PROPORT,
                  group = 1),
              color = colors[5],
              lty = 9,
              linewidth = 1) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 20, family = 'Gill Sans MT')) 
}

# View the plot
pareto_2023

# Facet Pareto plots by year ---------------------------------------------------
# Wrap the plots
pareto_facet <- 
  (pareto_2022 + facet_grid(. ~ '2022') + ggtitle('')) / 
  (pareto_2023 + facet_grid(. ~ '2023') + ggtitle('')) + 
  plot_layout(axis_titles = 'collect')

# View the wrap
pareto_facet

# Save the wrap
ggsave(filename = '2023_outputs/charts_and_tables/Plots/ODDS_pareto_facet_year.png',
       plot = pareto_facet,
       width = 11,
       height = 13)

# Heat map of ODDS trips not logged / logged incorrectly: year groups ----------
# get colors
colors <- nmfs_palette('oceans')(12)

# Format the data
# first create empty data frame to loop data into
odds_df <- data.frame()

# start from 2 because of incomplete 2024 data, and subtract 1 to avoid using
  # the empty sheet attached at the end
for (i in 2:(length(odds_data) - 1)) {
  data <- odds_data[[i]] %>%
    select(contains('Date'), `Issue Category`)
  odds_df <- bind_rows(odds_df, data)
  print(i)
  rm(data, i)
}

# rename columns, combine date columns, summarize data
odds_df_formatted <- odds_df %>%
  rename(ISSUE_CATEGORY = `Issue Category`) %>%
  mutate(YEAR = as.numeric(format(coalesce(Date, # add columns if necessary
                                           `Date OLE Notified`), '%Y')),
         ISSUE_CATEGORY = replace(ISSUE_CATEGORY, 
                                  ISSUE_CATEGORY == 
                                    'EM Cameras Not Turned on For Selected Trip', 
                                  'Leave without observer'),
         ISSUE_CATEGORY = replace(ISSUE_CATEGORY,
                                  ISSUE_CATEGORY ==
                                    'Fished on a Canceled Observed Trip',
                                  'Fished on a Canceled Trip'),
         ISSUE_CATEGORY = replace(ISSUE_CATEGORY,
                                  ISSUE_CATEGORY ==
                                    'Fished on a canceled selected trip',
                                  'Fished on a Canceled Trip'),
         ISSUE_CATEGORY = replace(ISSUE_CATEGORY,
                                  ISSUE_CATEGORY == 
                                    'Tender trip definition',
                                  'Incorrect Tender'),
         ISSUE_CATEGORY = str_wrap(ISSUE_CATEGORY, width = 20)) %>%
  select(YEAR, ISSUE_CATEGORY) %>%
  group_by(YEAR, ISSUE_CATEGORY) %>%
  summarise(FREQ = n()) %>%
  group_by(YEAR) %>%
  mutate(YEAR_SUM = sum(FREQ),
         PROPORT_FREQ = FREQ / YEAR_SUM) %>%
  filter(YEAR < 2024) %>%
  ungroup()

# Make the plot
odds_heatmap <- {
  ggplot(data = odds_df_formatted %>%
           filter(YEAR > 2015),
         aes(x = 1,
             y = 1)) +
    facet_nested(ISSUE_CATEGORY ~ YEAR,
                 switch = 'y') +
    labs(y = 'Issue Category',
         fill = 'Occurrences',
         x = '') +
    geom_tile(aes(fill = PROPORT_FREQ)) +
    geom_text(aes(label = FREQ),
              color = 'white',
              family = 'Gill Sans MT') +
    theme_minimal() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_gradientn(name = 'Proportion of\nOccurrences\nWithin a Year',
                         colors = rev(colors),
                         limits = c(0, 1.000),
                         breaks = c(0, 0.250, 0.500, 0.750, 1.000)) +
    theme(strip.text.y.left = element_text(angle = 0),
          strip.background = element_rect(fill = 'black'),
          strip.text = element_text(color = 'white'),
          text = element_text(size = 20, family = 'Gill Sans MT'),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(color = 'black',
                                          fill = 'gray'))
}

# View the plot
odds_heatmap

# Save the plot
ggsave(file = '2023_outputs/charts_and_tables/Plots/odds_issues_heatmap.png',
       plot = odds_heatmap,
       width = 14,
       height = 12)

#####################################
##### CONFIDENTIAL DATA REMOVED #####
#####################################
# Alluvial plot of old data, 2023 ----------------------------------------------
# flow: STATEMENT_TYPE -> OLD_OLE_CATEGORY (filtered for OLD OLE SYSTEM)
# LABELS WITH < 3 STATEMENTS:
# Contractor Problems, Harassment-Assault, Marine Mammal-Feeding,
# Marine Mammal-Harassment, Prohibited Species - Retaining, 
# Sample Bias-Marine Mammals, Seabird-Harassment

# Set labels
river_oldcat_labels_noconfid_23 <- {
  statements_combined %>%
  filter(OLE_SYSTEM == 'OLD',
         FIRST_VIOL_YEAR == 2023) %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 10, 
         FREQ > 2)
}

# Set aside names of statements that we are removing
confid_statements_old_23 <- {
  statements_combined %>%
  filter(OLE_SYSTEM == 'OLD',
         FIRST_VIOL_YEAR == 2023) %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 3) %>%
  select(STATEMENT_TYPE_LABEL)
}

# get colors
old_colors_noconfid <- {
  statements_combined %>%
    filter(OLE_SYSTEM == 'OLD',
           FIRST_VIOL_YEAR == 2023,
           !STATEMENT_TYPE_LABEL %in% 
             confid_statements_old_23$STATEMENT_TYPE_LABEL) %>% 
    group_by(STATEMENT_TYPE, OLD_STATEMENT_COLOR) %>%
    summarize() %>%
    ungroup() %>%
    mutate(STATEMENT_TYPE = tolower(STATEMENT_TYPE)) %>%
    arrange(STATEMENT_TYPE)
}

# Make the plot
river_oldcat_noconfid_23 <- {
  ggplot(data = statements_combined %>%
           filter(OLE_SYSTEM == 'OLD',
                  FIRST_VIOL_YEAR == 2023,
                  !STATEMENT_TYPE_LABEL %in%
                    confid_statements_old_23$STATEMENT_TYPE_LABEL),
         aes(axis1 = factor(STATEMENT_TYPE_LABEL,
                            levels = c('Disruptive/Bothersome Behavior - Conflict Resolved',
                                       'Harassment - Sexual',
                                       'Intimidation, coercion, hostile work environment',
                                       'Interference/Sample Biasing', 'Safety-NMFS',
                                       'MARPOL/Oil Spill', 'Safety-USCG-Equipment',
                                       'Safety-USCG-Fail to Conduct Drills',
                                       'Safety-USCG-Marine Casualty', 'AFA',
                                       'Amendment 80', 'Catcher Processor Longline',
                                       'IFQ Retention', 'Amendment 91 salmon',
                                       'Gulf of Alaska Salmon', 
                                       'Halibut Deck Sorting',
                                       'Prohibited Species - Mishandling',
                                       'Restricted Access',
                                       'Failure to Notify', 
                                       'Inadequate Accommodations', 'IR/IU',
                                       'Miscellaneous Violations',
                                       'Reasonable Assistance', 
                                       'Record Keeping and Reporting')),
             axis2 = OLD_OLE_CATEGORY_LABEL)) +
    river2_theme(labels = river_oldcat_labels_noconfid_23,
                 axis1_label = 'Old Statement Type',
                 axis2_label = 'Old OLE Category',
                 axis1_min_text_size = 9,
                 axis1_text_size = 18) +
    scale_fill_manual(values = old_colors_noconfid$OLD_STATEMENT_COLOR)
}

# View the plot
river_oldcat_noconfid_23

# Save the plot
ggsave(filename = 'Plots/river_oldcat_confid_removed_23.png',
       plot = river_oldcat_noconfid_23,
       width = 14,
       height = 10)


# Alluvial plot of new data, by safety -----------------------------------------
# flow: OLD_OLE_CATEGORY -> NEW_OLE_CATEGORY_STATEMENT_TYPE (filtered for
# NEW OLE SYSTEM)
# LABELS WITH < 3 STATEMENTS:
# ACCESS, ASSAULT, FORCED TO PERFORM CREW DUTIES, EPIRB

# Set labels
river_newcat_safety_labels_noconfid <- {
  statements_combined %>%
  filter(OLE_SYSTEM == 'NEW',
         NEW_OLE_CATEGORY %in%
           c('OBSERVER SAFETY AND WORK ENVIRONMENT',
             'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
             'SAFETY-USCG-MARINE CASUALTY',
             'SAFETY-USCG-EQUIPMENT',
             'INTERFERENCE WITH DUTIES')) %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 6,
         FREQ > 2) 
}

# set aside confidential statements that we are removing
confid_statements_safety_23 <- {
  statements_combined %>%
  filter(OLE_SYSTEM == 'NEW',
         NEW_OLE_CATEGORY %in%
           c('OBSERVER SAFETY AND WORK ENVIRONMENT',
             'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
             'SAFETY-USCG-MARINE CASUALTY',
             'SAFETY-USCG-EQUIPMENT',
             'INTERFERENCE WITH DUTIES')) %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 3) %>%
  select(STATEMENT_TYPE_LABEL)
}

# get colors
safety_colors_noconfid <- {
  statements_combined %>%
    filter(OLE_SYSTEM == 'NEW',
           NEW_OLE_CATEGORY %in%
             c('OBSERVER SAFETY AND WORK ENVIRONMENT',
               'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
               'SAFETY-USCG-MARINE CASUALTY',
               'SAFETY-USCG-EQUIPMENT',
               'INTERFERENCE WITH DUTIES'),
           !STATEMENT_TYPE_LABEL %in%
             confid_statements_safety_23$STATEMENT_TYPE_LABEL) %>% 
    group_by(STATEMENT_TYPE_LABEL, NEW_STATEMENT_SAFETY_COLOR) %>%
    summarize() %>%
    ungroup() %>%
    mutate(STATEMENT_TYPE_LABEL = tolower(STATEMENT_TYPE_LABEL)) %>%
    arrange(STATEMENT_TYPE_LABEL)
}

# Make the plot
river_newcat_safety_noconfid <- {
  ggplot(data = statements_combined %>%
           filter(OLE_SYSTEM == 'NEW',
                  NEW_OLE_CATEGORY %in% 
                    c('OBSERVER SAFETY AND WORK ENVIRONMENT',
                      'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
                      'SAFETY-USCG-MARINE CASUALTY',
                      'SAFETY-USCG-EQUIPMENT',
                      'INTERFERENCE WITH DUTIES'),
                  !STATEMENT_TYPE_LABEL %in%
                    confid_statements_safety_23$STATEMENT_TYPE_LABEL),
         aes(axis1 = OLD_OLE_CATEGORY_LABEL,
             axis2 = NEW_OLE_CATEGORY_LABEL,
             axis3 = factor(STATEMENT_TYPE_LABEL,
                            levels = c('HOSTILE WORK ENVIRONMENT', 'IMPEDIMENT',
                                       'INTIMIDATION, BRIBERY,\nCOERCION',
                                       'SEXUAL HARASSMENT', 
                                       'SAFETY', 'FOOD AND\nACCOMMODATIONS',
                                       'DESTRUCTION OF\nSAMPLE, WORK,\nPERSONAL EFFECTS',
                                       'SAMPLING INTERFERENCE', 'NOTIFICATION', 
                                       'REASONABLE ASSISTANCE',
                                       'GENERAL SAFETY EQUIPMENT', 
                                       'SURVIVAL CRAFT', 
                                       'FAILURE TO CONDUCT DRILLS',
                                       'MARINE\nCASUALTY')))) +
    river3_theme(labels = river_newcat_safety_labels_noconfid$STATEMENT_TYPE_LABEL,
                 axis1_label = 'Old OLE Category',
                 axis2_label = 'New OLE Category',
                 axis3_label = 'New Statement Type',
                 size_label = 5,
                 axis3_text_size = 18,
                 axis3_min_text_size = 8) +
    scale_fill_manual(values = safety_colors_noconfid$NEW_STATEMENT_SAFETY_COLOR)
}

# View the plot
river_newcat_safety_noconfid

# Save the plot
ggsave(filename = 'Plots/river_newcat_safety_confid_removed.png',
       plot = river_newcat_safety_noconfid,
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
river_newcat_nonsafety_labels_noconfid <- {
  statements_combined %>%
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
  filter(FREQ < 8,
         FREQ > 2)
}

# set aside confidential statements that we are removing
confid_statements_nonsafety_23 <-  {
  statements_combined %>%
  filter(OLE_SYSTEM == 'NEW',
         !NEW_OLE_CATEGORY %in%
           c('OBSERVER SAFETY AND WORK ENVIRONMENT',
             'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
             'SAFETY-USCG-MARINE CASUALTY',
             'SAFETY-USCG-EQUIPMENT',
             'INTERFERENCE WITH DUTIES')) %>%
  group_by(STATEMENT_TYPE_LABEL) %>%
  summarize(FREQ = n()) %>%
  filter(FREQ < 3) %>%
  select(STATEMENT_TYPE_LABEL)
}

# get colors
nonsafety_colors_noconfid <- {
  statements_combined %>%
    filter(OLE_SYSTEM == 'NEW',
           !NEW_OLE_CATEGORY %in%
             c('OBSERVER SAFETY AND WORK ENVIRONMENT',
               'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
               'SAFETY-USCG-MARINE CASUALTY',
               'SAFETY-USCG-EQUIPMENT',
               'INTERFERENCE WITH DUTIES'),
           !STATEMENT_TYPE_LABEL %in%
             confid_statements_nonsafety_23$STATEMENT_TYPE_LABEL) %>% 
    group_by(STATEMENT_TYPE_LABEL, NEW_STATEMENT_NONSAFETY_COLOR) %>%
    summarize() %>%
    ungroup() %>%
    mutate(STATEMENT_TYPE_LABEL = tolower(STATEMENT_TYPE_LABEL)) %>%
    arrange(STATEMENT_TYPE_LABEL)
}


# Make the plot
river_newcat_nonsafety_noconfid <- {
  ggplot(data = statements_combined %>%
           filter(OLE_SYSTEM == 'NEW',
                  !NEW_OLE_CATEGORY %in% 
                    c('OBSERVER SAFETY AND WORK ENVIRONMENT',
                      'SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION',
                      'SAFETY-USCG-MARINE CASUALTY',
                      'SAFETY-USCG-EQUIPMENT',
                      'INTERFERENCE WITH DUTIES'),
                  !STATEMENT_TYPE_LABEL %in%
                    confid_statements_nonsafety_23$STATEMENT_TYPE_LABEL),
         aes(axis1 = OLD_OLE_CATEGORY_LABEL,
             axis2 = NEW_OLE_CATEGORY_LABEL,
             axis3 = factor(STATEMENT_TYPE_LABEL,
                            levels = c('DISCHARGE OF GARBAGE\nOR PLASTIC, OR LOSS\nOF FISHING GEAR',
                                       'DISCHARGE OF OIL', 'BIN MONITORING', 
                                       'OBSERVER SAMPLING STATION', 'SCALES',
                                       'VIDEO MONITORING SYSTEM', 
                                       'BELT AND\nFLOW OPERATIONS', 
                                       'CATCH WEIGHING', 'CMCP', 
                                       'MONITORING THE\nFLOW OF FISH', 
                                       'BSAI SALMON BYCATCH', 
                                       'GOA SALMON BYCATCH', 'MARINE MAMMAL',
                                       'PROHIBITED SPECIES\nMISHANDLING',
                                       'PROHIBITED SPECIES\nRETENTION',
                                       'FALSE REPORTING', 
                                       'GENERAL REPORTING\nREQUIREMENTS', 
                                       'UNLAWFUL DISCARD', 
                                       'DEPLOYMENT LOGISTICS')))) +
    river3_theme(labels = river_newcat_nonsafety_labels_noconfid$STATEMENT_TYPE_LABEL,
                 axis1_label = 'Old OLE Category',
                 axis2_label = 'New OLE Category',
                 axis3_label = 'New Statement Type',
                 size_label = 5,
                 axis3_text_size = 18,
                 axis3_min_text_size = 10) +
    scale_fill_manual(values = nonsafety_colors_noconfid$NEW_STATEMENT_NONSAFETY_COLOR)
}

# View the plot
river_newcat_nonsafety_noconfid

# Save the plot
ggsave(filename = 'Plots/river_newcat_nonsafety_confid_removed.png',
       plot = river_newcat_nonsafety_noconfid,
       width = 20,
       height = 10)
