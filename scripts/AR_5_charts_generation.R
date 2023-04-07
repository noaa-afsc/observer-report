# Annual Report Enforcement chapter: Charts generation ----------
# Contact Andy Kingham
# 206-526-4212


# load req'd packages ----------------------------------------
# library(plyr)
# library(reshape2)
 library(dplyr)
 library(ggplot2)
# library(tidyr)
# library(lubridate)
# library(data.table)
# library(sqldf)
# library(scales)
# library(devtools)
library(ggh4x)
# library(ggpmisc)

# load the data files. ----------------------------------------
# * chng wd filepath as needed *
rm(list = ls())

# adp_yr is the year of the annual report we are doing this time (annual_deployment_year)
# NOTE: we need this to ensure we load the CORRECT YEAR.  Each year has it's own directory and Rdata files.
adp_yr <- rstudioapi::showPrompt(title = "ADP YEAR", message = "Enter the ADP YEAR for this analysis:", default = "")

load(file = paste0(adp_yr, "_outputs/Rdata_workspaces/AR_4_summary_tables_output.rdata"))

# Define GG's ----------------------------------------

## Heatmap definitions ----------------------------------------

### Heatmap theme elements  ----------------------------------------
fn_heatmap_theme <- function(){ 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust = 1))
}


### Heatmap 1000 days gg-----------------------------------------
fn_heatmap_1000_gg <-
  function(ggcategory, ggtitle){
    ggplot(data = rate_all_groupings_affi_type_for_plots %>%
                     ungroup() %>% 
                     filter(OLE_CATEGORY == ggcategory,
                            CONFI_FLAG == 0,
                            CALENDAR_YEAR == adp_yr), 
           aes(x = '', 
               y = AFFIDAVIT_TYPE)) +
      geom_tile(aes(fill  = INCIDENTS_PER_1000_DEPLOYED_DAYS)) +
      geom_text(aes(label = round(INCIDENTS_PER_1000_DEPLOYED_DAYS, 1)),
                fontface = "bold") +
      facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE) +
      scale_fill_gradient(low  = "blue",
                          high = "yellow",
                          name =  paste("Occurrences",  
                                        "Per 1000", 
                                        "Deployed",
                                        "Days", sep = "\n"))  +
      labs(x = '', y = "Statement Type", title = ggtitle) +
      fn_heatmap_theme() 
    }




### Heatmap Assnmt gg elements -----------------------
fn_heatmap_assnmt_gg <-
  function(ggcategory, ggtitle){
    ggplot(data = rate_all_groupings_affi_type_for_plots %>%
             ungroup() %>% 
             filter(OLE_CATEGORY == ggcategory,
                    CONFI_FLAG == 0,
                    CALENDAR_YEAR == adp_yr), 
           aes(x = '', 
               y = AFFIDAVIT_TYPE)) +
      geom_tile(aes(fill  = INCIDENTS_PER_ASSIGNMENT)) +
      geom_text(aes(label = round(INCIDENTS_PER_ASSIGNMENT, 2)),
                    fontface = "bold") +
      facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE) +
      scale_fill_gradient(low  = "blue",
                          high = "yellow",
                          name =  paste("Occurrences", 
                                        "Per Vessel/Plant", 
                                        "Assignment", sep = "\n")) +
      labs(x = '', y = "Statement Type", title = ggtitle) +
      fn_heatmap_theme() 
  }


### Heatmap ggsSave parameters   ----------------------------------------
fn_save_defs_heatmaps <-
  function(plotname_char, plotname){
    ggsave(filename = paste0(adp_yr,
                             "_outputs/charts_and_tables/heat_maps/hm_", 
                              adp_yr, "_",
                              plotname_char,
                             ".png"
                            ), 
       plot   = plotname,
       height = 8.00,  # mess with these as needed
       width  = 15.6) # mess with these as needed
      }


## Histogram definitions ----------------------------------

### Histogram gg elements ----------------------------------
fn_histog_gg <-
  function(ggcategory, ggtitle){
    ggplot(data = raw_statements %>%
             filter(FIRST_VIOL_YEAR == adp_yr) %>%
             mutate(NUMBER_OF_INCIDENTS = NUMBER_VIOLATIONS,
                    STATEMENT_TYPE = AFFIDAVIT_TYPE) %>%
             filter(OLE_CATEGORY == ggcategory),
           aes(x=NUMBER_OF_INCIDENTS, fill = STATEMENT_TYPE)) +
      geom_histogram(stat     = "bin",
                     binwidth = 1,
                     position = "dodge") +
      facet_wrap (. ~ OLE_CATEGORY, scales="free") +
      labs(x = "# of Ocurrences",
           y = "# of statements",
           fill = "Statement Type",
           title = ggtitle)
  }

### Histogram save parameters --------------------------
fn_save_defs_histog <- 
  function(plotname_char, plotname){
    ggsave(filename = paste0(adp_yr,
                             "_outputs/charts_and_tables/histograms/histog_", 
                             adp_yr, "_",
                             plotname_char,
                             ".png"
                             ), 
           plot   = plotname,
           height = 8.00,  # mess with these as needed
           width  = 15.6) # mess with these as needed
  }




## Barchart gg definitions ----
### barchart theme --------------------------------
fn_barchart_theme <-
  function(){
    theme(axis.text.y  = element_text(size = 7, hjust = 0),
          strip.text.y.left = element_text(size = 8, angle = 0, hjust = 0),
          strip.text.x = element_text(size = 5),
           panel.spacing = unit(0,"lines"),
           strip.background = element_rect(color="grey30", fill="grey90"),
           panel.border = element_rect(color= "black", fill = "NA", size = 0.1), # "grey90"),
           panel.grid = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none")
  }

### barchart facet -------------------------
fn_barchart_facet <-
  function(){
    facet_grid(AFFIDAVIT_TYPE ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
               scales = "free",
               switch= "both",
               # bleed = FALSE,
               labeller = label_wrap_gen(),
               shrink = TRUE)
  }

### barchart labels -----------------------------------
fn_barchart_labs <-
  function(rate_type){
    labs(x = 'Vessel Type ~ Gear Type ~ Geographic Region ~ Management Program',
         fill = '', # paste("Coverage Type ~","\n", "Observer Role"),
         y = paste("Occurrences per ", rate_type, "\n") # Added a space for right y axis.
         #    title = "OLE PRIORITY: SAFETY AND DUTIES Statements -   Occurrences per 1000 Deployed Days"
    ) }

### barchart 1000 gg definitions --------------------------
fn_barchart_1000_gg <-
  function(ggcategory, rate_type){
    ggplot(data = rate_all_groupings_affi_type_for_plots %>%
             ungroup() %>% 
             mutate(FILL = factor(paste(COVERAGE_TYPE, 'COVERAGE', sep= ' '))
             ) %>%
             filter(CALENDAR_YEAR == adp_yr,
                    CONFI_FLAG == 0,
                    OLE_CATEGORY == ggcategory), 
           aes('',
               # keeping this here COMMENTED OUT, because it is a useful way of grouping as well, but not used.  
               # x = paste(VESSEL_TYPE, 
               #           if_else(is.na(GEAR_TYPE), '   ', ' ~ '), 
               #           if_else(is.na(GEAR_TYPE), '   ', as.character(GEAR_TYPE)), 
               #           ' ~ ', MANAGEMENT_PROGRAM_CODE, 
               #           ' ~ ', NMFS_REGION, 
               #           sep = ''),
               y = INCIDENTS_PER_1000_DEPLOYED_DAYS,
               fill = FILL) ) +
      geom_col(position  = "dodge") +
      geom_text(aes(label = round(INCIDENTS_PER_1000_DEPLOYED_DAYS, 1)),
                position = position_dodge(width = 1),
                fontface = "bold", size = 2,
               vjust="inward",
               hjust="inward") +
      # + facet_nested(AFFIDAVIT_TYPE ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
      #              scales = "free",
      #              switch="both",
      #              # bleed = FALSE,
      #              labeller = label_wrap_gen())
      fn_barchart_facet() +
      scale_y_continuous(position="right",
                         labels = function(x) ifelse(x == 0, "", x)) + #https://stackoverflow.com/questions/56771267/remove-trailing-zeros-in-ggplot-axis 
      # for more control of how much space, you could use, limits = function(y){c(min(y), max(y)*1.1)})
      fn_barchart_labs(rate_type = "1000 Deployed Days") +
      scale_fill_manual(values =c("#F8766D", "#00BFC4")) +
      # + theme_bw()
      fn_barchart_theme()
  }

### barchart assnmt gg definitions --------------------------
fn_barchart_assnmt_gg <-
  function(ggcategory){
    ggplot(data = rate_all_groupings_affi_type_for_plots %>%
             ungroup() %>% 
             mutate(FILL = factor(paste(COVERAGE_TYPE, 'COVERAGE', sep= ' '))
             ) %>%
             filter(CALENDAR_YEAR == adp_yr,
                    CONFI_FLAG == 0,
                    OLE_CATEGORY == ggcategory), 
           aes('',
               # keeping this here COMMENTED OUT, because it is a useful way of grouping as well, but not used.  
               # x = paste(VESSEL_TYPE, 
               #           if_else(is.na(GEAR_TYPE), '   ', ' ~ '), 
               #           if_else(is.na(GEAR_TYPE), '   ', as.character(GEAR_TYPE)), 
               #           ' ~ ', MANAGEMENT_PROGRAM_CODE, 
               #           ' ~ ', NMFS_REGION, 
               #           sep = ''),
               y = INCIDENTS_PER_ASSIGNMENT,
               fill = FILL) ) +
      geom_col(position  = "dodge") +
      geom_text(aes(label = round(INCIDENTS_PER_ASSIGNMENT, 2)),
                # nudge_y = 2,
                position = position_dodge(width = 1),
                #  vjust = 0.3,
                fontface = "bold",
                size = 2,
                vjust="inward",
                hjust="inward") +
      # + facet_nested(AFFIDAVIT_TYPE ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
      #              scales = "free",
      #              switch="both",
      #              # bleed = FALSE,
      #              labeller = label_wrap_gen())
      fn_barchart_facet() +
      scale_y_continuous(position="right",
                         labels = function(x) ifelse(x == 0, "", x)) + #https://stackoverflow.com/questions/56771267/remove-trailing-zeros-in-ggplot-axis 
      fn_barchart_labs(rate_type = "Vessel/Plant Assignment") +
      scale_fill_manual(values =c("#F8766D", "#00BFC4")) +
      # + theme_bw()
      fn_barchart_theme()
  }

### barchart ggsave parameters ---------------------------------
fn_save_defs_barchart <-
  function(plotname_char, plotname){
    ggsave(filename = paste0(adp_yr,
                             "_outputs/charts_and_tables/barcharts/bar_chart_", 
                             adp_yr, "_",
                             plotname_char,
                             ".pdf"
                             ), 
           plot   = plotname,
           height = 5.7,  # mess with these as needed
           width  = 9, 
           units = "in") # mess with these as needed
  }

# Heat Maps ----------------------------------------
# There are 4 Charts for EACH OLE_CATEGORY, for a total of 24 charts: 
# 1. rate per 1000 deployed days
# 2. rate per 90 deployed days
# 3. rate per cruise
# 4. rate per vessel/plant assignment


## SAFETY AND DUTIES ----------------------------------------

### Incidents Per 1000 Deployed Days ----------------------------------------
incis_per_1000_days_work_env_by_affi_type <-
  fn_heatmap_1000_gg(
    ggcategory = 'OLE PRIORITY: SAFETY AND DUTIES',
    ggtitle    = "Statement Category Group OLE PRIORITY: SAFETY AND DUTIES"
              )

incis_per_1000_days_work_env_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_1000_days_work_env_by_affi_type",
                      plotname = incis_per_1000_days_work_env_by_affi_type)

### Incidents Per 90 Deployed Days ----------------------------------------
# COMMENTED OUT, not used.

# incis_per_90_days_work_env_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'OLE PRIORITY: SAFETY AND DUTIES',
#                    CALENDAR_YEAR == adp_yr), 
#           aes(x = '',
#               y    = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_90_DEPLOYED_DAYS))
#   + geom_text(aes(label = round(INCIDENTS_PER_90_DEPLOYED_DAYS, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE)
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group OLE PRIORITY: SAFETY AND DUTIES")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per 90", 
#                                       "Deployed",
#                                       "Days", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_90_days_work_env_by_affi_type

# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                          adp_yr, "_incis_per_90_days_work_env_by_affi_type.png", 
#                          sep = ''), 
#        plot = incis_per_90_days_work_env_by_affi_type,
#        height=7.00, 
#        width=15.6)





### Incidents Per CRUISE ----------------------------------------
# COMMENTED OUT, not used.

# incis_per_cruise_work_env_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'OLE PRIORITY: SAFETY AND DUTIES',
#                    CALENDAR_YEAR == adp_yr),  
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_CRUISE))
#   + geom_text(aes(label = round(INCIDENTS_PER_CRUISE, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE)
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group OLE PRIORITY: SAFETY AND DUTIES")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per Cruise", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_cruise_work_env_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                         adp_yr, 
#                         "_incis_per_cruise_work_env_by_affi_type.png", 
#                         sep = '') , 
#        plot = incis_per_cruise_work_env_by_affi_type,
#        height=7.00, width=15.6)





### Incidents Per ASSIGNMENT ----------------------------------------
incis_per_assnmt_work_env_by_affi_type <-
  fn_heatmap_assnmt_gg(
    ggcategory = 'OLE PRIORITY: SAFETY AND DUTIES',
    ggtitle = "Statement Category Group OLE PRIORITY: SAFETY AND DUTIES"
    )

incis_per_assnmt_work_env_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_assnmt_work_env_by_affi_type",
                      plotname = incis_per_assnmt_work_env_by_affi_type)


















## OLE PRIORITY: INTER-PERSONAL ----------------------------------------

### Incidents Per 1000 Deployed Days ----------------------------------------
incis_per_1000_days_interpersonal_by_affi_type <-
  fn_heatmap_1000_gg(
    ggcategory = 'OLE PRIORITY: INTER-PERSONAL',  
    ggtitle = "Statement Category Group OLE PRIORITY: INTER-PERSONAL"
  )

incis_per_1000_days_interpersonal_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_1000_days_interpersonal_by_affi_type",
                      plotname = incis_per_1000_days_interpersonal_by_affi_type)



### Incidents Per 90 Deployed Days ----------------------------------------
# COMMENTING OUT, not used.

# incis_per_90_days_interpersonal_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL',
#                    CALENDAR_YEAR == adp_yr), 
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_90_DEPLOYED_DAYS))
#   + geom_text(aes(label = round(INCIDENTS_PER_90_DEPLOYED_DAYS, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
#                  scales = "free_y")
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group OLE PRIORITY: INTER-PERSONAL")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per 90", 
#                                       "Deployed",
#                                       "Days", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_90_days_interpersonal_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_",
#                         adp_yr,
#                         "_incis_per_90_days_interpersonal_by_affi_type.png",
#                         sep = ''), 
#        plot = incis_per_90_days_interpersonal_by_affi_type,
#        height=7.00, width=15.6)





### Incidents Per CRUISE ----------------------------------------
# COMMENTING OUT, not used.

# incis_per_cruise_interpersonal_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'OLE PRIORITY: INTER-PERSONAL',
#                    CALENDAR_YEAR == adp_yr), 
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_CRUISE))
#   + geom_text(aes(label = round(INCIDENTS_PER_CRUISE, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
#                  scales = "free_y")
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group OLE PRIORITY: INTER-PERSONAL")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per Cruise",
#                                       sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_cruise_interpersonal_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                         adp_yr, 
#                         "_incis_per_cruise_interpersonal_by_affi_type.png", 
#                         sep = ''), 
#        plot = incis_per_cruise_interpersonal_by_affi_type,
#        height=7.00, width=15.6)





### Incidents Per ASSIGNMENT ----------------------------------------
incis_per_assnmt_interpersonal_by_affi_type <-
  fn_heatmap_assnmt_gg(
    ggcategory = 'OLE PRIORITY: INTER-PERSONAL',  
    ggtitle = "Statement Category Group OLE PRIORITY: INTER-PERSONAL"
  )

incis_per_assnmt_interpersonal_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_assnmt_interpersonal_by_affi_type",
                      plotname = incis_per_assnmt_interpersonal_by_affi_type)







## OLE_CATEGORY = LIMITED ACCESS PROGRAMS ----------------------------------------

### Incidents Per 1000 Deployed Days ----------------------------------------
incis_per_1000_days_lapp_by_affi_type <-
  fn_heatmap_1000_gg(
    ggcategory =  'LIMITED ACCESS PROGRAMS',  
    ggtitle = "Statement Category Group LIMITED ACCESS PROGRAMS"
  )

incis_per_1000_days_lapp_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_1000_days_lapp_by_affi_type",
                      plotname = incis_per_1000_days_lapp_by_affi_type)




### Incidents Per 90 Deployed Days ----------------------------------------
# COMMENTING OUT, not used.

# incis_per_90_days_lapp_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'LIMITED ACCESS PROGRAMS',
#                    CALENDAR_YEAR == adp_yr), 
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_90_DEPLOYED_DAYS))
#   + geom_text(aes(label = round(INCIDENTS_PER_90_DEPLOYED_DAYS, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
#                  scales = "free_y")
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group LIMITED ACCESS PROGRAMS")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per 90", 
#                                       "Deployed",
#                                       "Days", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_90_days_lapp_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                         adp_yr, 
#                         "_incis_per_90_days_lapp_by_affi_type.png", 
#                         sep = ''), 
#        plot = incis_per_90_days_lapp_by_affi_type,
#        height=7.00, width=15.6)






### Incidents Per CRUISE ----------------------------------------
# COMMENTING OUT, not used.

# incis_per_cruise_lapp_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'LIMITED ACCESS PROGRAMS',
#                    CALENDAR_YEAR == adp_yr), 
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_CRUISE))
#   + geom_text(aes(label = round(INCIDENTS_PER_CRUISE, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
#                  scales = "free_y")
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group LIMITED ACCESS PROGRAMS")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per Cruise", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_cruise_lapp_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                          adp_yr, 
#                          "_incis_per_cruise_lapp_by_affi_type.png", 
#                         sep = ''), 
#        plot = incis_per_cruise_lapp_by_affi_type,
#        height=7.00, width=15.6)






### Incidents Per ASSIGNMENT ----------------------------------------
incis_per_assnmt_lapp_by_affi_type <-
  fn_heatmap_assnmt_gg(
    ggcategory =  'LIMITED ACCESS PROGRAMS',  
    ggtitle = "Statement Category Group LIMITED ACCESS PROGRAMS"
  )

incis_per_assnmt_lapp_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_assnmt_lapp_by_affi_type",
                      plotname = incis_per_assnmt_lapp_by_affi_type)

  
 




## PROTECTED RESOURCE & PROHIBITED SPECIES ----------------------------------------

### Incidents Per 1000 Deployed Days ----------------------------------------
incis_per_1000_days_prohib_by_affi_type <-
  fn_heatmap_1000_gg(
    ggcategory =  'PROTECTED RESOURCE & PROHIBITED SPECIES',  
    ggtitle = "Statement Category Group  PROTECTED RESOURCE & PROHIBITED SPECIES"
  )

incis_per_1000_days_prohib_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_1000_days_prohib_by_affi_type",
                      plotname = incis_per_1000_days_prohib_by_affi_type)




### Incidents Per 90 Deployed Days ----------------------------------------
# COMMENTING OUT, not used.

# incis_per_90_days_prohib_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'PROTECTED RESOURCE & PROHIBITED SPECIES',
#                    CALENDAR_YEAR == adp_yr), 
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_90_DEPLOYED_DAYS))
#   + geom_text(aes(label = round(INCIDENTS_PER_90_DEPLOYED_DAYS, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
#                  scales = "free_y")
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group PROTECTED RESOURCE & PROHIBITED SPECIES")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per 90", 
#                                       "Deployed",
#                                       "Days", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_90_days_prohib_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                         adp_yr, 
#                         "_incis_per_90_days_prohib_by_affi_type.png", 
#                         sep = ''), 
#        plot = incis_per_90_days_prohib_by_affi_type,
#        height=7.00, width=15.6)






### Incidents Per CRUISE ----------------------------------------
# COMMENTING OUT, not used.

# incis_per_cruise_prohib_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'PROTECTED RESOURCE & PROHIBITED SPECIES',
#                    CALENDAR_YEAR == adp_yr),  
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_CRUISE))
#   + geom_text(aes(label = round(INCIDENTS_PER_CRUISE, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
#                  scales = "free_y")
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group PROTECTED RESOURCE & PROHIBITED SPECIES")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per Cruise", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_cruise_prohib_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                         adp_yr, 
#                         "_incis_per_cruise_prohib_by_affi_type.png", 
#                         sep = ''), 
#        plot = incis_per_cruise_prohib_by_affi_type,
#        height=7.00, width=15.6)






### Incidents Per ASSIGNMENT ----------------------------------------
incis_per_assnmt_prohib_by_affi_type <-
  fn_heatmap_assnmt_gg(
    ggcategory =  'PROTECTED RESOURCE & PROHIBITED SPECIES',  
    ggtitle = "Statement Category Group  PROTECTED RESOURCE & PROHIBITED SPECIES"
  )

incis_per_assnmt_prohib_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_assnmt_prohib_by_affi_type",
                      plotname = incis_per_assnmt_prohib_by_affi_type)






## OLE_CATEGORY = COAST GUARD ----------------------------------------

### Incidents Per 1000 Deployed Days ----------------------------------------
incis_per_1000_days_cg_by_affi_type <-
  fn_heatmap_1000_gg(
    ggcategory =  'COAST GUARD',  
    ggtitle    = "Statement Category Group COAST GUARD"
       )

incis_per_1000_days_cg_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_1000_days_cg_by_affi_type",
                      plotname = incis_per_1000_days_cg_by_affi_type)


### Incidents Per 90 Deployed Days ----------------------------------------
# COMMENTING OUT, not used.

# incis_per_90_days_cg_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'COAST GUARD',
#                    CALENDAR_YEAR == adp_yr),  
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_90_DEPLOYED_DAYS))
#   + geom_text(aes(label = round(INCIDENTS_PER_90_DEPLOYED_DAYS, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
#                  scales = "free_y")
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group COAST GUARD")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per 90", 
#                                       "Deployed",
#                                       "Days", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_90_days_cg_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                         adp_yr, 
#                         "_incis_per_90_days_cg_by_affi_type.png", 
#                         sep = '') , 
#        plot = incis_per_90_days_cg_by_affi_type,
#        height=7.00, width=15.6)






### Incidents Per CRUISE ----------------------------------------
# COMMENTING OUT, not used.

# incis_per_cruise_cg_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'COAST GUARD',
#                    CALENDAR_YEAR == adp_yr), 
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_CRUISE))
#   + geom_text(aes(label = round(INCIDENTS_PER_CRUISE, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
#                  scales = "free_y")
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group COAST GUARD")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per Cruise", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_cruise_cg_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                         adp_yr, 
#                         "_incis_per_cruise_cg_by_affi_type.png", 
#                         sep = ''), 
#        plot = incis_per_cruise_cg_by_affi_type,
#        height=7.00, width=15.6)






### Incidents Per ASSIGNMENT ----------------------------------------
incis_per_assnmt_cg_by_affi_type <-
  fn_heatmap_assnmt_gg(
    ggcategory =  'COAST GUARD',  
    ggtitle    = "Statement Category Group COAST GUARD"
  )

incis_per_assnmt_cg_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_assnmt_cg_by_affi_type",
                      plotname = incis_per_assnmt_cg_by_affi_type)





## ALL OTHER STATEMENT TYPES ----------------------------------------

### Incidents Per 1000 Deployed Days ----------------------------------------
incis_per_1000_days_other_by_affi_type <-
  fn_heatmap_1000_gg(
    ggcategory =  'ALL OTHER STATEMENT TYPES',  
    ggtitle    = "Statement Category Group ALL OTHER STATEMENT TYPES"
  )

incis_per_1000_days_other_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_1000_days_other_by_affi_type",
                      plotname = incis_per_1000_days_other_by_affi_type)



### Incidents Per 90 Deployed Days ----------------------------------------
# COMMENTING OUT, not used.

# incis_per_90_days_other_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'ALL OTHER STATEMENT TYPES',
#                    CALENDAR_YEAR == adp_yr), 
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_90_DEPLOYED_DAYS))
#   + geom_text(aes(label = round(INCIDENTS_PER_90_DEPLOYED_DAYS, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
#                  scales = "free_y")
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group ALL OTHER STATEMENT TYPES")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per 90", 
#                                       "Deployed",
#                                       "Days", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_90_days_other_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                         adp_yr, 
#                         "_incis_per_90_days_other_by_affi_type.png", 
#                         sep = '') , 
#        plot = incis_per_90_days_other_by_affi_type,
#        height=7.00, width=15.6)






### Incidents Per CRUISE ----------------------------------------
# COMMENTING OUT, not used.

# incis_per_cruise_other_by_affi_type <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>%
#             filter(OLE_CATEGORY == 'ALL OTHER STATEMENT TYPES',
#                    CALENDAR_YEAR == adp_yr),  
#           aes(x = '',
#               y = AFFIDAVIT_TYPE ) 
#   )
#   + geom_tile(aes(fill = INCIDENTS_PER_CRUISE))
#   + geom_text(aes(label = round(INCIDENTS_PER_CRUISE, 2)),
#               fontface = "bold")
#   + facet_nested(COVERAGE_TYPE  ~ VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE,
#                  scales = "free_y")
#   + labs( x = '',
#           y = "Statement Type",
#           title = "Statement Category Group ALL OTHER STATEMENT TYPES")
#   + scale_fill_gradient(low  = "blue",
#                         high = "yellow",
#                         name =  paste("Occurrences",  
#                                       "Per Cruise", sep = "\n"))
#   + theme(axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1))
#   
#   )
# 
# incis_per_cruise_other_by_affi_type
# 
# ggsave(filename = paste("charts_and_tables/heat_maps/hm_", 
#                         adp_yr, 
#                         "_incis_per_cruise_other_by_affi_type.png", 
#                         sep = ''), 
#        plot = incis_per_cruise_other_by_affi_type,
#        height=7.00, width=15.6)






### Incidents Per ASSIGNMENT ----------------------------------------
incis_per_assnmt_other_by_affi_type <-
  fn_heatmap_assnmt_gg(
    ggcategory =  'ALL OTHER STATEMENT TYPES',  
    ggtitle    = "Statement Category Group ALL OTHER STATEMENT TYPES"
  )

incis_per_assnmt_other_by_affi_type
fn_save_defs_heatmaps(plotname_char = "incis_per_assnmt_other_by_affi_type",
                      plotname = incis_per_assnmt_other_by_affi_type)






# Histograms (Incis per Statement) ----------------------------------------

## all other types ----------------------------------------
incis_per_statement_histog_all_other_types <-
  fn_histog_gg(ggcategory = 'ALL OTHER STATEMENT TYPES',
               ggtitle = 'Statement Category Group ALL OTHER STATEMENT TYPES') 

incis_per_statement_histog_all_other_types
fn_save_defs_histog(plotname_char = "incis_per_statement_histog_all_other_types",
                    plotname = incis_per_statement_histog_all_other_types)





## Coast Guard ----------------------------------------
incis_per_statement_histog_cg <-
  fn_histog_gg(ggcategory = 'COAST GUARD',
               ggtitle = 'Statement Category Group COAST GUARD') 

incis_per_statement_histog_cg
fn_save_defs_histog(plotname_char = "incis_per_statement_histog_cg",
                    plotname = incis_per_statement_histog_cg)






## LAPP ----------------------------------------
incis_per_statement_histog_LAPP <-
  fn_histog_gg(ggcategory = 'LIMITED ACCESS PROGRAMS',
               ggtitle = 'Statement Category Group LIMITED ACCESS PROGRAMS') 

incis_per_statement_histog_LAPP
fn_save_defs_histog(plotname_char = "incis_per_statement_histog_LAPP",
                    plotname = incis_per_statement_histog_LAPP)



## InterPersonal ----------------------------------------
incis_per_statement_histog_Interpersonal <-
  fn_histog_gg(ggcategory = 'OLE PRIORITY: INTER-PERSONAL',
               ggtitle = 'Statement Category Group OLE PRIORITY: INTER-PERSONAL') 

incis_per_statement_histog_Interpersonal
fn_save_defs_histog(plotname_char = "incis_per_statement_histog_Interpersonal",
                    plotname = incis_per_statement_histog_Interpersonal)
 


## Safety and Duties ----------------------------------------
incis_per_statement_histog_Safety <-
  fn_histog_gg(ggcategory = 'OLE PRIORITY: SAFETY AND DUTIES',
               ggtitle = 'Statement Category Group OLE PRIORITY: SAFETY AND DUTIES') 

incis_per_statement_histog_Safety
fn_save_defs_histog(plotname_char = "incis_per_statement_histog_Safety",
                    plotname = incis_per_statement_histog_Safety)



## Prohib ----------------------------------------
incis_per_statement_histog_prohib <-
  fn_histog_gg(ggcategory = 'PROTECTED RESOURCE & PROHIBITED SPECIES',
               ggtitle = 'Statement Category Group PROTECTED RESOURCE & PROHIBITED SPECIES') 

incis_per_statement_histog_prohib
fn_save_defs_histog(plotname_char = "incis_per_statement_histog_prohib",
                    plotname = incis_per_statement_histog_prohib)






## All Categs Incis Per Statement ----------------------------------------
# THIS IS ALSO A KEEPER
# UPDATE: NOT USED.

incis_per_statement_histog_all <-
  (ggplot(raw_statements %>%
            filter(FIRST_VIOL_YEAR == adp_yr) %>%
            mutate(NUMBER_OF_INCIDENTS = NUMBER_VIOLATIONS,
                   STATEMENT_TYPE = AFFIDAVIT_TYPE) 
          #         %>%
          #          filter(OLE_CATEGORY == 'PROTECTED RESOURCE & PROHIBITED SPECIES') 
          ,
          aes(x=NUMBER_OF_INCIDENTS # 
              #   fill = STATEMENT_TYPE
          ))
   + geom_histogram(stat     = "bin",
                    binwidth = 1,
                    position = "stack",
                    fill = "#F8766D" )
   + facet_wrap (.~ OLE_CATEGORY, scales="free")
   + labs(x = "# of Ocurrences",
          y = "# of statements",
          fill = "Statement Type")
  )

incis_per_statement_histog_all
ggsave(paste("charts_and_tables/histograms/histog_", 
             adp_yr, 
             "_incis_per_statement_all.png", 
             sep = '') , 
       height=10.00, width=15.6)






## R&R only ----------------------------------------
incis_per_statement_histog_RandR_only <-
  (ggplot(days_statements_all_groupings_raw %>%
            mutate(NUMBER_OF_INCIDENTS = NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1,
                   STATEMENT_TYPE = AFFIDAVIT_TYPE) %>%
            filter(CALENDAR_YEAR == adp_yr,
                   !is.na(AFFIDAVIT_ID),
                   AFFIDAVIT_TYPE == 'Record Keeping and Reporting',
                   # VESSEL_TYPE == 'CV',
                   # GEAR_TYPE == 'POT',
                   #  MANAGEMENT_PROGRAM_CODE == 'IFQ'
            ),
          aes(x=NUMBER_OF_INCIDENTS, fill = factor(paste(COVERAGE_TYPE, ' COVERAGE', sep = '')) 
          )
  )
  + geom_histogram(stat     = "bin",
                   binwidth = 1,
                   position = "stack")
  # + facet_grid(VESSEL_TYPE +  GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION ~ . , scales = "free")  
  + scale_x_continuous(breaks = seq(0,100, by=5) # labels = scales::number_format(accuracy = 1) #, position="right"
  )
  + labs(x = "# of Occurrences",
         y = "# of statements",
         title = paste("Record Keeping & Reporting Occurrences per Statement ",
                       adp_yr,
                       sep = ''),
         fill = '')
  
  )

incis_per_statement_histog_RandR_only
ggsave(paste("charts_and_tables/histograms/histog_", 
             adp_yr, 
             "_incis_per_statement_RandR_only.png", 
             sep = '') , 
       height=10.00, width=15.6)




## all categs_facet ----------------------------------------
# THIS ONE IS A KEEPER

incis_per_statement_all_facet <-
(ggplot(days_statements_all_groupings_raw %>%
          mutate(NUMBER_OF_INCIDENTS = NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1,
                 STATEMENT_TYPE = AFFIDAVIT_TYPE) %>%
          filter(CALENDAR_YEAR == adp_yr,
                 OLE_CATEGORY == 'PROTECTED RESOURCE & PROHIBITED SPECIES',
                 # VESSEL_TYPE == 'PLANT',
                 COVERAGE_TYPE == 'FULL',
                 #MANAGEMENT_PROGRAM_CODE == 'AFA'
          ),
        aes(x=NUMBER_OF_INCIDENTS,
            fill = paste(VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, sep= '~')))
 + geom_histogram(stat     = "bin",
                  binwidth = 1,
                  position = "stack")
 + facet_grid(AFFIDAVIT_TYPE ~., 
              scales = "free", 
              switch = 'y',
              labeller = label_wrap_gen())
 + scale_y_continuous(labels = scales::number_format(accuracy = 0.1), position="right")  # Put the y-axis labels on the right
 + labs(x = "Number of Occurrences",
        y = "Number of Statements",
        fill = paste("Vessel Type","Gear Type", "Management Program","Geographic Region", sep = ' ~ \n'), 
        title = "PROTECTED RESOURCE & PROHIBITED SPECIES: Occurrences Per Statement")
 + theme(strip.text.y = element_text(angle = 180),
 )
)

incis_per_statement_all_facet
ggsave(paste("charts_and_tables/histograms/histog_", 
             adp_yr, 
             "_incis_per_statement_all_facet.png", 
             sep = '') , 
       height=10.00, width=15.6)



# BOXPLOT incis per statement ----------------------------------------
(ggplot(days_statements_all_groupings_raw %>%
          mutate(NUMBER_OF_INCIDENTS = NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1,
                 STATEMENT_TYPE = AFFIDAVIT_TYPE) %>%
          filter(CALENDAR_YEAR == adp_yr,
                 OLE_CATEGORY == 'ALL OTHER STATEMENT TYPES',
                 # VESSEL_TYPE == 'PLANT',
                 # COVERAGE_TYPE == 'FULL',
                 # MANAGEMENT_PROGRAM_CODE == 'AFA'
          ),
        aes(x = '', # paste(VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, sep= '~'),
            y = NUMBER_OF_INCIDENTS))
 + geom_boxplot(position = "dodge")
 + facet_grid(AFFIDAVIT_TYPE ~ VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION,
              scales = "free", 
              switch = 'both',
              labeller = label_wrap_gen())
 + scale_y_continuous(position="right")  # Put the y-axis labels on the right
 + labs( x = "Vessel Type ~ Gear Type ~ Management Program ~ Geographic Region",
         y = "Occurrences Per Statement",
         title = "ALL OTHER STATEMENT TYPES")
 + theme(axis.text.x  = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     hjust = 1),
         axis.text.y  = element_text(hjust = 0),
         strip.text.y = element_text(angle = 180)
 )
)













# BARCHARTS ----------------------------------------

## Prohibs 1000_days ----------------------------------------
incis_per_1000_days_prohib_bar <-
 fn_barchart_1000_gg(ggcategory = 'PROTECTED RESOURCE & PROHIBITED SPECIES')

incis_per_1000_days_prohib_bar
fn_save_defs_barchart(plotname_char = "incis_per_1000_days_prohib_bar",
                      plotname = incis_per_1000_days_prohib_bar)

## CG 1000 days ----------------------------------------
incis_per_1000_days_CG_bar <-
  fn_barchart_1000_gg(ggcategory = 'COAST GUARD')

incis_per_1000_days_CG_bar
fn_save_defs_barchart(plotname_char = "incis_per_1000_days_CG_bar",
                      plotname = incis_per_1000_days_CG_bar)

## Other types 1000 Days ----------------------------------------
incis_per_1000_days_other_bar <-
  fn_barchart_1000_gg(ggcategory = 'ALL OTHER STATEMENT TYPES') +
  theme(strip.text.x = element_text(size = 4)) #TODO - this is a hack.

incis_per_1000_days_other_bar
fn_save_defs_barchart(plotname_char = "incis_per_1000_days_other_bar",
                      plotname = incis_per_1000_days_other_bar)
  
## LAPP 1000 days ----------------------------------------
incis_per_1000_days_LAPP_bar <-
  fn_barchart_1000_gg(ggcategory = 'LIMITED ACCESS PROGRAMS')

incis_per_1000_days_LAPP_bar
fn_save_defs_barchart(plotname_char = "incis_per_1000_days_LAPP_bar",
                      plotname = incis_per_1000_days_LAPP_bar)

# Safety 1000 days ----------------------------------------
incis_per_1000_days_SAFETY_bar <-
  fn_barchart_1000_gg(ggcategory = 'OLE PRIORITY: SAFETY AND DUTIES')

incis_per_1000_days_SAFETY_bar
fn_save_defs_barchart(plotname_char = "incis_per_1000_days_SAFETY_bar",
                      plotname = incis_per_1000_days_SAFETY_bar)

## Inter-personal 1000 days ----------------------------------------
incis_per_1000_days_interpersonal_bar <-
  fn_barchart_1000_gg(ggcategory = 'OLE PRIORITY: INTER-PERSONAL')

incis_per_1000_days_interpersonal_bar
fn_save_defs_barchart(plotname_char = "incis_per_1000_days_interpersonal_bar",
                      plotname = incis_per_1000_days_interpersonal_bar)

## Inter Personal Assnmt ----------------------------------------
incis_per_assnmt_interpersonal_bar <-
  fn_barchart_assnmt_gg(ggcategory = 'OLE PRIORITY: INTER-PERSONAL')

incis_per_assnmt_interpersonal_bar
fn_save_defs_barchart(plotname_char = "incis_per_assnmt_interpersonal_bar",
                      plotname = incis_per_assnmt_interpersonal_bar)

# STOP HERE --------------------------



# barcharts by Year ----------------------------------------
# COMMENTED OUT, not used.

## Safety 1000 days  ----------------------------------------
# safety_barchart_year <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>% 
#             mutate(FILL        = factor(CALENDAR_YEAR) # factor(paste(COVERAGE_TYPE, 'COVERAGE', sep= ' ')) # factor(YEAR)
#             ) %>%
#             filter(# CALENDAR_YEAR == 2020, 
#               OLE_CATEGORY == 'OLE PRIORITY: SAFETY AND DUTIES'), 
#           aes('',
#               # x = paste(VESSEL_TYPE, 
#               #           if_else(is.na(GEAR_TYPE), '   ', ' ~ '), 
#               #           if_else(is.na(GEAR_TYPE), '   ', as.character(GEAR_TYPE)), 
#               #           ' ~ ', MANAGEMENT_PROGRAM_CODE, 
#               #           ' ~ ', NMFS_REGION, 
#               #           sep = ''),
#               #  x = MANAGEMENT_PROGRAM_CODE,
#               y = STATEMENTS_PER_1000_DEPLOYED_DAYS,
#               fill = FILL
#           ) 
#   )
#   + geom_col(position  = "dodge")
#   + geom_text(aes(label = round(STATEMENTS_PER_1000_DEPLOYED_DAYS, 1)),
#               # nudge_y = 2,
#               position = position_dodge(width = 1),
#               #  vjust = 0.3,
#               fontface = "bold")
#   # + facet_nested(AFFIDAVIT_TYPE ~ VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION, 
#   #              scales = "free",
#   #              switch="both",
#   #              bleed = FALSE,
#   #              labeller = label_wrap_gen())
#   + facet_grid(AFFIDAVIT_TYPE ~ COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE, 
#                scales = "free",
#                switch="both",
#                # bleed = FALSE,
#                labeller = label_wrap_gen())
#   + scale_y_continuous(position="right")  # Put the y-axis labels on the right
#   
#   + labs( x = 'Coverage Type ~ Vessel Type ~ Gear Type ~ Geographic Region ~ Management Program',
#           fill = '', # paste("Coverage Type ~","\n", "Observer Role"),
#           y = "# Occurrences per 1000 Deployed Days" # ,
#           #    title = "OLE PRIORITY: SAFETY AND DUTIES Statements -   Occurrences per 1000 Deployed Days"
#   )
#   + scale_fill_manual(values =c("#F8766D", "#00BFC4"))
#   # + theme_bw()
#   + theme(axis.text.y  = element_text(hjust = 0),
#           strip.text.y.left = element_text(angle = 0, hjust = 0),
#           # panel.spacing=unit(1,"lines"),
#           # strip.background=element_rect(color="grey30", fill="grey90"),
#           # panel.border=element_rect(color= "black"), # "grey90"),
#           axis.ticks.x=element_blank(),
#           legend.position="top"
#   )
#   
#   )
# 
# 
# 
# 
# safety_barchart_year
# 
# ggsave(paste("charts_and_tables/barcharts/bar_chart_", 
#              adp_yr, 
#              "_incis_per_1000_days_safety_by_affi_type_year_comprson.png", 
#              sep = '') , 
#        height=8.00, width=15.6)
# 
# 
# 
# 


## LAPP 1000 days ----------------------------------------
# (ggplot(rate_all_groupings_affi_type_for_plots %>%
#           ungroup() %>% 
#           mutate(FILL        = factor(CALENDAR_YEAR) # factor(paste(COVERAGE_TYPE, 'COVERAGE', sep= ' '))
#           ) %>%
#           filter(# CALENDAR_YEAR == 2020, 
#             OLE_CATEGORY == 'LIMITED ACCESS PROGRAMS'), 
#         aes('',
#             # x = paste(VESSEL_TYPE, 
#             #           if_else(is.na(GEAR_TYPE), '   ', ' ~ '), 
#             #           if_else(is.na(GEAR_TYPE), '   ', as.character(GEAR_TYPE)), 
#             #           ' ~ ', MANAGEMENT_PROGRAM_CODE, 
#             #           ' ~ ', NMFS_REGION, 
#             #           sep = ''),
#             y = INCIDENTS_PER_1000_DEPLOYED_DAYS,
#             fill = FILL
#         ) 
# )
#   + geom_col(position  = "dodge")
#   + geom_text(aes(label = round(INCIDENTS_PER_1000_DEPLOYED_DAYS, 1)),
#               # nudge_y = 2,
#               position = position_dodge(width = 1),
#               #  vjust = 0.3,
#               fontface = "bold")
#   # + facet_nested(AFFIDAVIT_TYPE ~ VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION, 
#   #              scales = "free",
#   #              switch="both",
#   #              bleed = FALSE,
#   #              labeller = label_wrap_gen())
#   + facet_grid(AFFIDAVIT_TYPE ~ COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE, 
#                scales = "free",
#                switch="both",
#                # bleed = FALSE,
#                labeller = label_wrap_gen())
#   + scale_y_continuous(position="right")  # Put the y-axis labels on the right
#   
#   + labs( x = 'Coverage Type ~ Vessel Type ~ Gear Type ~ Geographic Region ~ Management Program',
#           fill = '', # paste("Coverage Type ~","\n", "Observer Role"),
#           y = "# Occurrences per 1000 Deployed Days" # ,
#           #    title = "OLE PRIORITY: SAFETY AND DUTIES Statements -   Occurrences per 1000 Deployed Days"
#   )
#   + scale_fill_manual(values =c("#F8766D", "#00BFC4"))
#   # + theme_bw()
#   + theme(axis.text.y  = element_text(hjust = 0),
#           strip.text.y.left = element_text(angle = 0, hjust = 0),
#           # panel.spacing=unit(1,"lines"),
#           # strip.background=element_rect(color="grey30", fill="grey90"),
#           # panel.border=element_rect(color= "black"), # "grey90"),
#           axis.ticks.x=element_blank(),
#           legend.position="top"
#   )
#   
# )
# ggsave(paste("charts_and_tables/barcharts/bar_chart_", 
#              adp_yr, 
#              "_incis_per_1000_days_LAPP_by_affi_type_year_comprson.png", 
#              sep = '') , 
#        height=8.00, width=15.6)
# 
# 



## Prohib 1000 days ----------------------------------------
# (ggplot(rate_all_groupings_affi_type_for_plots %>%
#           ungroup() %>% 
#           mutate(FILL        = factor(CALENDAR_YEAR) # factor(paste(COVERAGE_TYPE, 'COVERAGE', sep= ' '))
#           ) %>%
#           filter(# CALENDAR_YEAR == 2020, 
#             OLE_CATEGORY == 'PROTECTED RESOURCE & PROHIBITED SPECIES'), 
#         aes('',
#             # x = paste(VESSEL_TYPE, 
#             #           if_else(is.na(GEAR_TYPE), '   ', ' ~ '), 
#             #           if_else(is.na(GEAR_TYPE), '   ', as.character(GEAR_TYPE)), 
#             #           ' ~ ', MANAGEMENT_PROGRAM_CODE, 
#             #           ' ~ ', NMFS_REGION, 
#             #           sep = ''),
#             y = INCIDENTS_PER_1000_DEPLOYED_DAYS,
#             fill = FILL
#         ) 
# )
#   + geom_col(position  = "dodge")
#   + geom_text(aes(label = round(INCIDENTS_PER_1000_DEPLOYED_DAYS, 1)),
#               # nudge_y = 2,
#               position = position_dodge(width = 1),
#               #  vjust = 0.3,
#               fontface = "bold")
#   # + facet_nested(AFFIDAVIT_TYPE ~ VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION, 
#   #              scales = "free",
#   #              switch="both",
#   #              bleed = FALSE,
#   #              labeller = label_wrap_gen())
#   + facet_grid(AFFIDAVIT_TYPE ~ COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + NMFS_REGION + MANAGEMENT_PROGRAM_CODE, 
#                scales = "free",
#                switch="both",
#                # bleed = FALSE,
#                labeller = label_wrap_gen())
#   + scale_y_continuous(position="right")  # Put the y-axis labels on the right
#   
#   + labs( x = 'Coverage Type ~ Vessel Type ~ Gear Type ~ Geographic Region ~ Management Program',
#           fill = '', # paste("Coverage Type ~","\n", "Observer Role"),
#           y = "# Occurrences per 1000 Deployed Days" # ,
#           #    title = "OLE PRIORITY: SAFETY AND DUTIES Statements -   Occurrences per 1000 Deployed Days"
#   )
#   + scale_fill_manual(values =c("#F8766D", "#00BFC4"))
#   # + theme_bw()
#   + theme(axis.text.y  = element_text(hjust = 0),
#           strip.text.y.left = element_text(angle = 0, hjust = 0),
#           # panel.spacing=unit(1,"lines"),
#           # strip.background=element_rect(color="grey30", fill="grey90"),
#           # panel.border=element_rect(color= "black"), # "grey90"),
#           axis.ticks.x=element_blank(),
#           legend.position="top"
#   )
#   
# )
# ggsave(paste("charts_and_tables/barcharts/bar_chart_", 
#              adp_yr, 
#              "_incis_per_1000_days_PROHIB_by_affi_type_year_comprson.png", 
#              sep = '') , 
#        height=8.00, width=15.6)
# 
# 
# 
# 
# 
# 
# 
# 






































#  LINE plots_by_year ----------------------------------------
# COMMENTED OUT, not used.

## Safety 1000 days ----------------------------------------

# safety_lineplot_year <-
#   (ggplot(rate_all_groupings_affi_type_for_plots %>%
#             ungroup() %>% 
#             full_join(cnt_dep_days_by_factor_group) %>%
#             mutate(INCIDENTS_PER_1000_DEPLOYED_DAYS = if_else(is.na(INCIDENTS_PER_1000_DEPLOYED_DAYS), 0, INCIDENTS_PER_1000_DEPLOYED_DAYS),
#                    FILL        = factor(paste(COVERAGE_TYPE, 'COVERAGE' , sep= ' '))
#             ) %>%
#             filter(# CALENDAR_YEAR == adp_yr, 
#                    OLE_CATEGORY == 'OLE PRIORITY: SAFETY AND DUTIES'), 
#           aes('',
#               x = CALENDAR_YEAR,
#               #x = AFFIDAVIT_TYPE,
#               # x = paste(VESSEL_TYPE, 
#               #           if_else(is.na(GEAR_TYPE), '   ', ' ~ '), 
#               #           if_else(is.na(GEAR_TYPE), '   ', as.character(GEAR_TYPE)), 
#               #           ' ~ ', MANAGEMENT_PROGRAM_CODE, 
#               #           ' ~ ', NMFS_REGION, 
#               #           sep = ''),
#               #  x = MANAGEMENT_PROGRAM_CODE,
#               y = INCIDENTS_PER_1000_DEPLOYED_DAYS,
#               color = paste(MANAGEMENT_PROGRAM_CODE, NMFS_REGION, sep = '~')
#           ) 
#   )
#   + geom_line()
#   # + geom_text(aes(label = round(INCIDENTS_PER_1000_DEPLOYED_DAYS, 1)),
#   #             # nudge_y = 2,
#   #             position = position_dodge(width = 1),
#   #             #  vjust = 0.3,
#   #             fontface = "bold")
#   + facet_nested(AFFIDAVIT_TYPE ~ COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION,
#                  scales = "free",
#                  switch="y",
#                  bleed = FALSE,
#                  labeller = label_wrap_gen())
#   # + facet_grid(AFFIDAVIT_TYPE ~ VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION, 
#   #              #   scales = "free",
#   #              switch="y",
#   #              # bleed = FALSE,
#   #              labeller = label_wrap_gen())
#   + scale_y_continuous(position="right")  # Put the y-axis labels on the right
#   # + coord_cartesian(ylim = c(0, 50))
#   
#   + labs( x = 'YEAR', #Vessel Type ~ Gear Type ~ Management Program ~ Geographic Region',
#           color = 'MGT PGM ~ Geog. Region', # paste("Coverage Type ~","\n", "Observer Role"),
#           y = "# Occurrences per 1000 Deployed Days" # ,
#           #    title = "OLE PRIORITY: SAFETY AND DUTIES Statements -   Occurrences per 1000 Deployed Days"
#   )
#   #  + scale_fill_manual(values =c("#F8766D", "#00BFC4"))
#   + scale_x_continuous(breaks = c(adp_yr))
#   # + theme_bw()
#   + theme(axis.text.y  = element_text(hjust = 0),
#           strip.text.y.left = element_text(angle = 0, hjust = 0),
#           # panel.spacing=unit(1,"lines"),
#           # strip.background=element_rect(color="grey30", fill="grey90"),
#           # panel.border=element_rect(color= "black"), # "grey90"),
#           # axis.ticks.x=element_blank(),
#           #legend.position="top"
#           axis.text.x = element_text(angle = 90, 
#                                      vjust = 0.5, 
#                                      hjust = 1)
#   )
#   
#   )
# 
# safety_lineplot_year
# 
# ggsave(paste("charts_and_tables/barcharts/line_chart_", 
#              adp_yr, 
#              "_incis_per_1000_days_safety_by_affi_type_year_comprson_", 
#              Sys.Date(), ".png", 
#              sep = '') , 
#        height=8.00, width=15.6)



