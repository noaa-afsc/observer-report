
# Annual Report Enforcement chapter: Table generation --------------------------
# Contact Andy Kingham
# 206-526-4212


# load req'd packages -----------------------------------

library(plyr)
library(reshape2)
library(tidyverse)
library(data.table)
library(sqldf)
library(devtools)


# load the data files. --------------------------------------------------------
rm(list = ls())


# Set the .Rdata file we will load
file_3_name <- "AR_3_rate_output.Rdata"

# Assign the address of the Annual Report Project in the Shared Gdrive
AnnRpt_EnfChp_dribble <- gdrive_set_dribble("Projects/Annual Report OLE chapter 5/2024_data")

# Pull Rdata file from google drive. This will update your local if the GDrive version is newer,
# otherwise it will load your local
gdrive_download(file_3_name, AnnRpt_EnfChp_dribble)

load(file = file_3_name)


# Write .csv of the priority rate but first remove confidentail (total_statements < 3)
write.csv(file =  paste0("tables/tbl_",
                         adp_yr,
                         "_rate_by_subcat_priority.csv"
                          ),
          x    = rate_by_subcat_priority %>%
                   filter(TOTAL_STATEMENTS >= 3)
          )





###############

# Make summary table of just the STATEMENTS and INCIDENTS for each factor grouping, all statement categories aggregated.
# Useful to make because it is used in multiple places later.
cnt_incis_by_factor_group_all_categs <-
  rate_all_groupings_ole_category %>%
  group_by(CALENDAR_YEAR, OLE_SYSTEM, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, 
           MANAGEMENT_PROGRAM_CODE, NMFS_REGION, TOTAL_DAYS, TOTAL_CRUISES, TOTAL_OBSERVERS, 
           DISTINCT_OBSERVER_ASSIGNMENTS, CONFI_FLAG
  ) %>% 
  summarize(TOTAL_STATEMENTS = sum(if_else(is.na(TOTAL_STATEMENTS), 0, TOTAL_STATEMENTS)), # There are 0 statements if it is NA for the category.
            TOTAL_INCIDENTS  = sum(if_else(is.na(TOTAL_INCIDENTS), 0, TOTAL_INCIDENTS)) # There are 0 incidents if it is NA for the category.
  ) %>%
  mutate(FACTOR_GROUP = paste(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, sep = '')
  ) %>%
  ungroup()

######################

# Make a CAST of these rates, for tables in the report.
# Scripts exist below for per assignment, per 1000 days, per cruise, and per 90 days.

# CASTed table per 1000 days
rate_all_groupings_ole_category_cast_1000 <- 
  merge(reshape2::dcast(data = rate_all_groupings_ole_category  %>% 
                          filter(!is.na(OLD_OLE_CATEGORY)),  # for the report, ONLY show factor groups that had statement occurrences.
                        formula   = CALENDAR_YEAR + OLE_SYSTEM + COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION + CONFI_FLAG ~ OLD_OLE_CATEGORY,
                        value.var = "INCIDENTS_PER_1000_DEPLOYED_DAYS"),
        cnt_incis_by_factor_group_all_categs,
        all=TRUE)




# CASTed table per assignment
rate_all_groupings_ole_category_cast_per_assnmt <- 
  merge(
    reshape2::dcast(data      = rate_all_groupings_ole_category
                    %>% filter(!is.na(OLD_OLE_CATEGORY)), 
                    formula   = CALENDAR_YEAR + OLE_SYSTEM + COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION + CONFI_FLAG ~ OLD_OLE_CATEGORY,
                    value.var = "INCIDENTS_PER_ASSIGNMENT") ,
    cnt_incis_by_factor_group_all_categs, all=TRUE)





# merge the per_assignment and per_1000 days CASTed tables into one WIDE table, for the report.
# NOTE THAT THIS IS A 2-step script, must be run together.

rate_all_groupings_ole_category_all <-
  rate_all_groupings_ole_category_cast_1000 %>%
  inner_join(rate_all_groupings_ole_category_cast_per_assnmt %>%
               select(CALENDAR_YEAR, OLE_SYSTEM, FACTOR_GROUP,
                      `OLE PRIORITY: INTER-PERSONAL per ASSNMT` = `OLE PRIORITY: INTER-PERSONAL`)
  ) 

# next step, ensures that ALL factor groups are shown, even if there are 0's for all statement headers, in the current year.
rate_all_groupings_ole_category_all <-
  rate_all_groupings_ole_category_all %>%
  distinct(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, FACTOR_GROUP) %>%
  left_join(rate_all_groupings_ole_category_all %>%
              filter(CALENDAR_YEAR == adp_yr)) %>%
  mutate(CALENDAR_YEAR = ifelse(is.na(CALENDAR_YEAR), adp_yr, CALENDAR_YEAR)) # stick the current year for any that do not exist this year (NA's)


# save this summary table as CSV for output to a report
# THIS BECOMES THE FINAL PRODUCT FOR TABLE 4.2
# Use XL to format for final output:
#    # Remove rows where  CONFI_FLAG = 1, for confidentiality.
#        # (these are left here, so the the annual report team can review them, but should be omitted from the final product to protect observers)
#    # change NA's to 0's for each OLE_CATEGORY where there is NA. (Much easier in XL using ctrl-F)
#    # remove unneeded columns, if any (?)
#    # change borders, merge cells, change column names (remove underscores, update names), other visual cleanup stuff.

write.csv(file = paste0("tables/tbl_",
                        adp_yr,
                        "_rate_ole_category.csv"
),
x    = rate_all_groupings_ole_category_all %>% 
  filter(CONFI_FLAG == 0) %>%
  select(`Coverage Type` = COVERAGE_TYPE, 
         `Vessel Type`   = VESSEL_TYPE, 
         `Gear Type`     = GEAR_TYPE, 
         `Management Program` = MANAGEMENT_PROGRAM_CODE, 
         `NMFS Region`   = NMFS_REGION, 
         `Confi Flag`    = CONFI_FLAG,
         `OLE System`    = OLE_SYSTEM,
         `Vessel/Plant Assignments` = DISTINCT_OBSERVER_ASSIGNMENTS,
         `Deployed Days` = TOTAL_DAYS,
         `Statements (all categories)`  = TOTAL_STATEMENTS, 
         `Occurrences (all categories)` = TOTAL_INCIDENTS,
         `OLE PRIORITY: INTER-PERSONAL per ASSNMT`,
         `OLE PRIORITY: INTER-PERSONAL`,
         `OLE PRIORITY: SAFETY AND DUTIES`,
         `COAST GUARD`,
         `LIMITED ACCESS PROGRAMS`,
         `PROTECTED RESOURCE & PROHIBITED SPECIES`,
         `ALL OTHER STATEMENT TYPES`
  )
)





#####################
# use the CASTED tables to calculate YOY change
# Take the CURRENT YEAR's data as the MAIN OUTPUT, and use LAST YEAR's DATA to calculate % change YOY.

# COMMENTING THIS OUT FOR NOW, not used.

# rate_all_groupings_ole_category_YOY <-
#   rate_all_groupings_ole_category_all %>%
#     left_join(rate_all_groupings_ole_category_all %>%
#                 filter(CALENDAR_YEAR == as.numeric(adp_yr)-1) %>%
#                 select(FACTOR_GROUP,
#                        TOTAL_DAYS_PREV_YR = TOTAL_DAYS,
#                        DISTINCT_OBSERVER_ASSIGNMENTS_PREV_YR = DISTINCT_OBSERVER_ASSIGNMENTS,
#                        TOTAL_STATEMENTS_PREV_YR = TOTAL_STATEMENTS,
#                        TOTAL_INCIDENTS_PREV_YR = TOTAL_INCIDENTS,
#                        OPIP_PER_ASNMT_PREV_YR = `OLE PRIORITY: INTER-PERSONAL per ASSNMT`,
#                        OPIP_PER_1000_PREV_YR  = `OLE PRIORITY: INTER-PERSONAL`,
#                        OPSD_PER_1000_PREV_YR  = `OLE PRIORITY: SAFETY AND DUTIES`,
#                        CG_PER_1000_DAYS_PREV_YR    = `COAST GUARD`,
#                        PRPS_PER_1000_DAYS_PREV_YR  = `PROTECTED RESOURCE & PROHIBITED SPECIES`,
#                        LAPP_PER_1000_DAYS_PREV_YR  = `LIMITED ACCESS PROGRAMS`,
#                        AOST_PER_1000_DAYS_PREV_YR  = `ALL OTHER STATEMENT TYPES`)
#                 ) %>%
#     mutate(OPIP_ASSNMT_YOY_CHNG = (OPIP_PER_ASNMT_PREV_YR - `OLE PRIORITY: INTER-PERSONAL per ASSNMT`)/OPIP_PER_ASNMT_PREV_YR,
#            OPIP_1000_YOY_CHNG = (OPIP_PER_1000_PREV_YR - `OLE PRIORITY: INTER-PERSONAL`)/OPIP_PER_1000_PREV_YR,
#            CG_1000_YOY_CHNG = (CG_PER_1000_DAYS_PREV_YR - `COAST GUARD`)/CG_PER_1000_DAYS_PREV_YR,
#            LAPP_1000_YOY_CHNG = (LAPP_PER_1000_DAYS_PREV_YR - `LIMITED ACCESS PROGRAMS`)/LAPP_PER_1000_DAYS_PREV_YR,
#            PRPS_1000_YOY_CHNG = (PRPS_PER_1000_DAYS_PREV_YR - `PROTECTED RESOURCE & PROHIBITED SPECIES`)/PRPS_PER_1000_DAYS_PREV_YR,
#            AOST_1000_YOY_CHNG = (AOST_PER_1000_DAYS_PREV_YR - `ALL OTHER STATEMENT TYPES`)/AOST_PER_1000_DAYS_PREV_YR
#           ) %>%
#     select(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, CONFI_FLAG,
#            DISTINCT_OBSERVER_ASSIGNMENTS, TOTAL_DAYS, TOTAL_CRUISES, TOTAL_OBSERVERS,
#            TOTAL_STATEMENTS, TOTAL_INCIDENTS,
#            `OLE PRIORITY: INTER-PERSONAL per ASSNMT`, OPIP_ASSNMT_YOY_CHNG, 
#            `OLE PRIORITY: INTER-PERSONAL`, OPIP_1000_YOY_CHNG,
#            `COAST GUARD`, CG_1000_YOY_CHNG,
#            `LIMITED ACCESS PROGRAMS`, LAPP_1000_YOY_CHNG,
#            `PROTECTED RESOURCE & PROHIBITED SPECIES`, PRPS_1000_YOY_CHNG,
#            `ALL OTHER STATEMENT TYPES`, AOST_1000_YOY_CHNG
#           )
# 











#####################################
# Summarize by AFFI_TYPE, with NO FACTORS.
# THis is table 3 in the report.
#####################################

# NOT USED FOR 2023
# 
# ######################
# # First, count the factor groups that are associated with each statement category
# # NOTE: filters out CONFI_FLAG factor groups.
# factor_groups_by_cat <-
#   rate_all_groupings_affi_type %>%
#   filter(CONFI_FLAG == 0) %>%
#   group_by(CALENDAR_YEAR, OLE_CATEGORY, AFFIDAVIT_TYPE) %>%
#   summarize(N_FACTOR_GROUPS = n_distinct(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)) %>%
#   ungroup() %>%
#   inner_join(rate_all_groupings_affi_type %>%
#                filter(CONFI_FLAG == 0) %>%
#                group_by(CALENDAR_YEAR) %>%
#                summarize(N_TOTAL_FACTOR_GROUPS = n_distinct(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)) %>%
#                ungroup()
#              ) %>%
#   mutate(PROPORT_FACTOR_GRPS_WITH_INCIS = N_FACTOR_GROUPS/N_TOTAL_FACTOR_GROUPS)
# 
# 
# 
# 
# 
# 
# 
# ####################
# # start by doing this for the current year
# summ_by_affi_type_current_yr <-
#   raw_statements %>%
#   distinct(OLE_CATEGORY, AFFIDAVIT_TYPE) %>% # have to start here and then use a LEFT JOIN back, to ensure we get ALL cateogories, even those that may not be represented in the current year.
#   left_join(raw_statements %>%  # summarize current year statements
#               filter(FIRST_VIOL_YEAR == as.numeric(adp_yr)) %>%
#               group_by(AFFIDAVIT_TYPE) %>%
#               summarize(N_STATEMENTS = n(),
#                         N_INCIDENTS        = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS)),
#                         MEAN_INCIS_PER     = mean(NUMBER_VIOLATIONS),
#                         MEDIAN_INCIS_PER   = median(NUMBER_VIOLATIONS),
#                         MIN_INCIS_PER      = min(NUMBER_VIOLATIONS),
#                         MAX_INCIS_PER      = max(NUMBER_VIOLATIONS),
#                         QUANT_25_INCIS_PER = quantile(NUMBER_VIOLATIONS, 0.25),
#                         QUANT_75_INCIS_PER = quantile(NUMBER_VIOLATIONS, 0.75),
#                         STDEV_INCIS_PER    = sd(NUMBER_VIOLATIONS)
#                         ) %>%
#               ungroup()
#             ) %>%
#   # tack on the current year RATE without factor groupings
#   left_join(rate_no_groupings %>%
#               filter(CALENDAR_YEAR == as.numeric(adp_yr)) %>%
#               select(AFFIDAVIT_TYPE, RATE_PER_ASSNMT, RATE_PER_1000_DAYS)
#            ) %>%
#   # tack on the current year 0 FACTOR GROUPS
#   left_join(factor_groups_by_cat %>%
#               filter(CALENDAR_YEAR == as.numeric(adp_yr)) %>%
#               select(AFFIDAVIT_TYPE, PROPORT_FACTOR_GRPS_WITH_INCIS)
#             )
# 
# 
# 
# ####################
# # Now do the same thing for the PREVIOUS year, except we don't need the statistics.
# # this is for calculating YOY change.
# 
#   summ_by_affi_type_prev_yr <-
#     raw_statements %>%
#     distinct(OLE_CATEGORY, AFFIDAVIT_TYPE) %>% # have to start here and then use a LEFT JOIN back, to ensure we get ALL cateogories, even those that may not be represented in the current year.
#     left_join(raw_statements %>%  # summarize current year statements
#                 filter(FIRST_VIOL_YEAR == as.numeric(adp_yr)-1) %>%
#                 group_by(AFFIDAVIT_TYPE) %>%
#                 summarize(N_STATEMENTS_PREV_YR = n(),
#                           N_INCIDENTS_PREV_YR  = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS))
#                           ) %>%
#                 ungroup() 
#               ) %>%
#     # tack on the current year RATE without factor groupings
#     left_join(rate_no_groupings %>%
#                 filter(CALENDAR_YEAR == as.numeric(adp_yr)-1) %>%
#                 select(AFFIDAVIT_TYPE, 
#                        RATE_PER_ASSNMT_PREV_YR    = RATE_PER_ASSNMT, 
#                        RATE_PER_1000_DAYS_PREV_YR = RATE_PER_1000_DAYS)
#               )  %>%
#     # tack on the current year 0 FACTOR GROUPS
#   left_join(factor_groups_by_cat %>%
#               filter(CALENDAR_YEAR == as.numeric(adp_yr)-1) %>%
#               select(AFFIDAVIT_TYPE, 
#                      PROPORT_FACTOR_GRPS_WITH_INCIS_PREV_YR = PROPORT_FACTOR_GRPS_WITH_INCIS)
#   )
#   
#   
# ######################
# # stick both years together, and formt the ouput into a .CSV for the report
# ######################
#   
# summ_by_type_both_years <-
#   # replace the NA's with 0's, because if they are NA, that means there were 0 statements/incidents
#     merge(summ_by_affi_type_current_yr,
#           summ_by_affi_type_prev_yr,
#           all=TRUE) %>%
#     # Calculate YOY change
#     mutate(N_STATEMENTS_YOY_CHG = (N_STATEMENTS - N_STATEMENTS_PREV_YR)/N_STATEMENTS_PREV_YR,
#            N_INCIDENTS_YOY_CHG  = (N_INCIDENTS  - N_INCIDENTS_PREV_YR) /N_INCIDENTS_PREV_YR,
#            INCI_RATE_ASSMT_YOY_CHG  = (RATE_PER_ASSNMT - RATE_PER_ASSNMT_PREV_YR)/RATE_PER_ASSNMT_PREV_YR,
#            INCI_RATE_1000_YOY_CHG   = (RATE_PER_1000_DAYS   - RATE_PER_1000_DAYS_PREV_YR)/RATE_PER_1000_DAYS_PREV_YR,
#            # Note that this next one is a percent change of a percent, so it is just a straight subtraction!!
#            PROPORT_WITH_INCIS_YOY_CHG   =  PROPORT_FACTOR_GRPS_WITH_INCIS - PROPORT_FACTOR_GRPS_WITH_INCIS_PREV_YR,
#            
#            # Next change the NA's in %_YOY_CHANGE columns to either -100% or 100%, depending on which year has the NA.  
#            # If LAST YEAR has the NA and this year does NOT, then make it 100% change YOY; 
#            # if THIS YEAR has an NA and LAST YEAR does NOT, make it -100% change YOY.
#            N_STATEMENTS_YOY_CHG = ifelse(is.na(N_STATEMENTS) & !is.na(N_STATEMENTS_PREV_YR), -1, N_STATEMENTS_YOY_CHG),
#            N_STATEMENTS_YOY_CHG = ifelse(is.na(N_STATEMENTS_PREV_YR) & !is.na(N_STATEMENTS), 1, N_STATEMENTS_YOY_CHG),
#            N_INCIDENTS_YOY_CHG = ifelse(is.na(N_INCIDENTS) & !is.na(N_INCIDENTS_PREV_YR), -1, N_INCIDENTS_YOY_CHG),
#            N_INCIDENTS_YOY_CHG = ifelse(is.na(N_INCIDENTS_PREV_YR) & !is.na(N_INCIDENTS), 1, N_INCIDENTS_YOY_CHG),
#            INCI_RATE_ASSMT_YOY_CHG = ifelse(is.na(RATE_PER_ASSNMT) & !is.na(RATE_PER_ASSNMT_PREV_YR), -1, INCI_RATE_ASSMT_YOY_CHG),
#            INCI_RATE_ASSMT_YOY_CHG = ifelse(is.na(RATE_PER_ASSNMT_PREV_YR) & !is.na(RATE_PER_ASSNMT), 1, INCI_RATE_ASSMT_YOY_CHG),
#            INCI_RATE_1000_YOY_CHG = ifelse(is.na(RATE_PER_1000_DAYS) & !is.na(RATE_PER_1000_DAYS_PREV_YR), -1, INCI_RATE_1000_YOY_CHG),
#            INCI_RATE_1000_YOY_CHG = ifelse(is.na(RATE_PER_1000_DAYS_PREV_YR) & !is.na(RATE_PER_1000_DAYS), 1, INCI_RATE_1000_YOY_CHG),
#            PROPORT_WITH_INCIS_YOY_CHG = ifelse(is.na(PROPORT_FACTOR_GRPS_WITH_INCIS) & !is.na(PROPORT_FACTOR_GRPS_WITH_INCIS_PREV_YR), -1, PROPORT_WITH_INCIS_YOY_CHG),
#            PROPORT_WITH_INCIS_YOY_CHG = ifelse(is.na(PROPORT_FACTOR_GRPS_WITH_INCIS_PREV_YR) & !is.na(PROPORT_FACTOR_GRPS_WITH_INCIS), 1, PROPORT_WITH_INCIS_YOY_CHG),
#            
#            # replace the NA's in the main statements/incidents value columns with 0's, because if those are NA, that means there were 0 statements/incidents
#            N_STATEMENTS = ifelse(is.na(N_STATEMENTS), 0, N_STATEMENTS),
#            N_INCIDENTS = ifelse(is.na(N_INCIDENTS), 0, N_INCIDENTS),
#            N_STATEMENTS_PREV_YR = ifelse(is.na(N_STATEMENTS_PREV_YR), 0, N_STATEMENTS_PREV_YR),
#            N_INCIDENTS_PREV_YR = ifelse(is.na(N_INCIDENTS_PREV_YR), 0, N_INCIDENTS_PREV_YR),
#            RATE_PER_ASSNMT = ifelse(is.na(RATE_PER_ASSNMT), 0, RATE_PER_ASSNMT),
#            RATE_PER_ASSNMT_PREV_YR = ifelse(is.na(RATE_PER_ASSNMT_PREV_YR), 0, RATE_PER_ASSNMT_PREV_YR),
#            RATE_PER_1000_DAYS = ifelse(is.na(RATE_PER_1000_DAYS), 0, RATE_PER_1000_DAYS),
#            RATE_PER_1000_DAYS_PREV_YR = ifelse(is.na(RATE_PER_1000_DAYS_PREV_YR), 0, RATE_PER_1000_DAYS_PREV_YR),
#            PROPORT_FACTOR_GRPS_WITH_INCIS = ifelse(is.na(PROPORT_FACTOR_GRPS_WITH_INCIS), 0, PROPORT_FACTOR_GRPS_WITH_INCIS),
#            PROPORT_FACTOR_GRPS_WITH_INCIS_PREV_YR = ifelse(is.na(PROPORT_FACTOR_GRPS_WITH_INCIS_PREV_YR), 0, PROPORT_FACTOR_GRPS_WITH_INCIS_PREV_YR),
#            
#            )
#   
#     
# # Write the CSV file and rename some of the columns for cleaner reporting
# # Still need to do some basic formatting in XL: 
# #   # re-order OLE_CATEGORY
# #   # remove spaces, 
# #   # make column headers not caps, 
# #   # add grid lines, etc)
# 
# write.csv(file =  paste0("tables/tbl_",
#                          adp_yr,
#                          "_summ_incis_by_type.csv"
#                          ),
#           x    = summ_by_type_both_years %>%
#                   mutate(OLE_CATEGORY = factor(OLE_CATEGORY, # need to re-order the levels for the final output table
#                                                levels = c('OLE PRIORITY: INTER-PERSONAL',
#                                                           'OLE PRIORITY: SAFETY AND DUTIES',
#                                                           'COAST GUARD',
#                                                           'LIMITED ACCESS PROGRAMS',
#                                                           'PROTECTED RESOURCE & PROHIBITED SPECIES',
#                                                           'ALL OTHER STATEMENT TYPES')) ) %>%
#                   select(OLE_CATEGORY, 
#                          STATEMENT_TYPE   = AFFIDAVIT_TYPE, 
#                          TOTAL_STATEMENTS = N_STATEMENTS,
#                          N_STATEMENTS_YOY_CHG,
#                          TOTAL_OCCURRRENCES  = N_INCIDENTS,
#                          N_INCIDENTS_YOY_CHG,
#                          OCCURRENCES_PER_VESSEL_PLANT_ASSIGNMENT = RATE_PER_ASSNMT,
#                          INCI_RATE_ASSMT_YOY_CHG,
#                          OCCURRRENCES_PER_1000_DAYS = RATE_PER_1000_DAYS,
#                          INCI_RATE_1000_YOY_CHG,
#                          PROPORT_FACTOR_GRPS_WITH_INCIS,
#                          PROPORT_WITH_INCIS_YOY_CHG,
#                          MEAN_INCIS_PER, MEDIAN_INCIS_PER, MIN_INCIS_PER, MAX_INCIS_PER,
#                          QUANT_25_INCIS_PER, QUANT_75_INCIS_PER)
#                          
#             )
# 
# 




###############

# Make summary table of just the STATEMENTS and INCIDENTS for each factor grouping, all statement categories aggregated.
# Useful to make because it is used in multiple places later.
cnt_incis_by_factor_group_all_new_categs <-
  rate_all_groupings_new_ole_category %>%
  group_by(CALENDAR_YEAR, OLE_SYSTEM, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, 
           MANAGEMENT_PROGRAM_CODE, NMFS_REGION, TOTAL_DAYS, TOTAL_CRUISES, TOTAL_OBSERVERS, 
           DISTINCT_OBSERVER_ASSIGNMENTS, CONFI_FLAG
  ) %>% 
  summarize(TOTAL_STATEMENTS = sum(if_else(is.na(TOTAL_STATEMENTS), 0, TOTAL_STATEMENTS)), # There are 0 statements if it is NA for the category.
            TOTAL_INCIDENTS  = sum(if_else(is.na(TOTAL_INCIDENTS), 0, TOTAL_INCIDENTS)) # There are 0 incidents if it is NA for the category.
  ) %>%
  mutate(FACTOR_GROUP = paste(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, sep = '')
  ) %>%
  ungroup()

######################

# Make a CAST of these rates, for tables in the report.
# Scripts exist below for per assignment, per 1000 days, per cruise, and per 90 days.

# CASTed table per 1000 days
rate_all_groupings_new_ole_category_cast_1000 <- 
  merge(reshape2::dcast(data = rate_all_groupings_new_ole_category  %>% 
                          filter(!is.na(NEW_OLE_CATEGORY)),  # for the report, ONLY show factor groups that had statement occurrences.
                        formula   = CALENDAR_YEAR + OLE_SYSTEM + COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION + CONFI_FLAG ~ NEW_OLE_CATEGORY,
                        value.var = "INCIDENTS_PER_1000_DEPLOYED_DAYS"),
        cnt_incis_by_factor_group_all_new_categs,
        all=TRUE) %>%
  filter(OLE_SYSTEM == 'NEW')




# CASTed table per assignment
rate_all_groupings_new_ole_category_cast_per_assnmt <- 
  merge(
    reshape2::dcast(data      = rate_all_groupings_new_ole_category
                    %>% filter(!is.na(NEW_OLE_CATEGORY)), 
                    formula   = CALENDAR_YEAR + OLE_SYSTEM + COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION + CONFI_FLAG ~ NEW_OLE_CATEGORY,
                    value.var = "INCIDENTS_PER_ASSIGNMENT") ,
    cnt_incis_by_factor_group_all_new_categs, all=TRUE) %>%
  filter(OLE_SYSTEM == 'NEW')





# merge the per_assignment and per_1000 days CASTed tables into one WIDE table, for the report.
# NOTE THAT THIS IS A 2-step script, must be run together.

rate_all_groupings_new_ole_category_all <-
  rate_all_groupings_new_ole_category_cast_1000 %>%
  inner_join(rate_all_groupings_new_ole_category_cast_per_assnmt %>%
               select(CALENDAR_YEAR, OLE_SYSTEM, FACTOR_GROUP,
                      `OBSERVER SAFETY AND WORK ENVIRONMENT per ASSNMT` = `OBSERVER SAFETY AND WORK ENVIRONMENT`)
  ) 

# next step, ensures that ALL factor groups are shown, even if there are 0's for all statement headers, in the current year.
rate_all_groupings_new_ole_category_all <-
  rate_all_groupings_new_ole_category_all %>%
  distinct(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, FACTOR_GROUP) %>%
  left_join(rate_all_groupings_new_ole_category_all %>%
              filter(CALENDAR_YEAR == adp_yr)) %>%
  mutate(CALENDAR_YEAR = ifelse(is.na(CALENDAR_YEAR), adp_yr, CALENDAR_YEAR)) # stick the current year for any that do not exist this year (NA's)


# save this summary table as CSV for output to a report
# THIS BECOMES THE FINAL PRODUCT FOR TABLE 4.2
# Use XL to format for final output:
#    # Remove rows where  CONFI_FLAG = 1, for confidentiality.
#        # (these are left here, so the the annual report team can review them, but should be omitted from the final product to protect observers)
#    # change NA's to 0's for each OLE_CATEGORY where there is NA. (Much easier in XL using ctrl-F)
#    # remove unneeded columns, if any (?)
#    # change borders, merge cells, change column names (remove underscores, update names), other visual cleanup stuff.

write.csv(file = paste0("tables/tbl_",
                        adp_yr,
                        "_rate_new_ole_category.csv"
),
x    = rate_all_groupings_new_ole_category_all %>%
  filter(CONFI_FLAG == 0) %>%
  select(`Coverage Type` = COVERAGE_TYPE, 
         `Vessel Type`   = VESSEL_TYPE, 
         `Gear Type`     = GEAR_TYPE, 
         `Management Program` = MANAGEMENT_PROGRAM_CODE, 
         `NMFS Region`   = NMFS_REGION, 
         `Confi Flag`    = CONFI_FLAG,
         `OLE System`    = OLE_SYSTEM,
         `Vessel/Plant Assignments` = DISTINCT_OBSERVER_ASSIGNMENTS,
         `Deployed Days` = TOTAL_DAYS,
         `Statements (all categories)`  = TOTAL_STATEMENTS, 
         `Occurrences (all categories)` = TOTAL_INCIDENTS,
         `OBSERVER SAFETY AND WORK ENVIRONMENT per ASSNMT`,
         `OBSERVER SAFETY AND WORK ENVIRONMENT`,
         `SAFETY-USCG-MARINE CASUALTY`,
         `SAFETY-USCG-FAIL TO CONDUCT DRILLS AND/OR SAFETY ORIENTATION`,
         `SAFETY-USCG-EQUIPMENT`,
         `SUSTAINABLE FISHERIES`,
         `PROHIBITED SPECIES/MARINE MAMMALS/SEABIRDS`,
         `INTERFERENCE WITH DUTIES`,
         `OPERATIONAL REQUIREMENTS`,
         `GEAR/EQUIPMENT REQUIREMENTS`,
         `PERMITS/DOCUMENTS/RECORD KEEPING AND REPORTING`,
         `MARPOL/OIL SPILL`
  )
)



##################
# Save Output -------------------------------------------------------------

file_4_name <- "AR_4_summary_tables_output.Rdata"

save(list = ls(),
     file = file_4_name)

gdrive_upload(file_4_name, AnnRpt_EnfChp_dribble)
