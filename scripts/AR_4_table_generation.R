
# Annual Report Enforcement chapter: Table generation --------------------------
# Contact Andy Kingham
# 206-526-4212

# Load packages ----
library(plyr)
library(reshape2)
library(tidyverse)
library(data.table)
library(sqldf)
library(devtools)
library(FMAtools)

# Load data ----
rm(list = ls())

# Set the .Rdata file we will load
file_3_name <- "AR_3_rate_output.Rdata"

# Assign the address of the Annual Report Project in the Shared Gdrive
AnnRpt_EnfChp_dribble <- gdrive_set_dribble("Projects/Annual Report OLE chapter 5/2024_data")

# Pull Rdata file from google drive. This will update your local if the GDrive version is newer,
#  otherwise it will load your local
gdrive_download(file_3_name, AnnRpt_EnfChp_dribble)
load(file = file_3_name)

# Write .csv of the priority rate but first remove confidential (total_statements < 3)
write.csv(file =  paste0("tables/tbl_",
                         adp_yr,
                         "_subcat_units_rate_priority.csv"
                          ),
          x    = subcat_units_rate_priority # %>%
                  # filter(TOTAL_STATEMENTS >= 3)
          )

# Write .csv of the priority rate but first remove confidential (total_statements < 3)
write.csv(file =  paste0("tables/tbl_",
                         adp_yr,
                         "_subcat_units_rate.csv"
),
x    = subcat_units_rate # %>%
# filter(TOTAL_STATEMENTS >= 3)
)

### BELOW IS STILL 2023 AR CODE
# It is not relevant for the new rates.
## TODO: update as/if needed for 2024.
# ADK 20250312

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

#'`================================================================================`
# Summary table ----

library(flextable)
library(officer)

# Set table output default settings
set_flextable_defaults(font.family = "Times New Roman", font.size = 11)

# Summary of total observer statements per reporting category with total numbers
#  of various factors for comparisons of total effort

#'*----------------------------------------------------------*
## Wide format ----

# Total observed for each unit type
summary_units <- subcat_units_rate %>%
  filter(CALENDAR_YEAR == adp_yr) %>%
  distinct(INCIDENT_UNIT, TOTAL_UNITS) %>%
  mutate(INCIDENT_UNIT = fct_recode(factor(INCIDENT_UNIT),
                                    "Deployments" = "DEPL",
                                    "Hauls" = "HAUL",
                                    "Days" = "DAYS",
                                    "Trips" = "TRIP",
                                    "Offloads" = "OFFL",
                                    "Samples" = "SAMP",
                                    "Marine Mammal Interactions" = "MARM")) %>%
  # Number of unique vessels/plants that were observed
  bind_rows(assignments_dates_cr_perm %>%
              filter(CALENDAR_YEAR == adp_yr) %>%
              group_by(VESSEL_OR_PLANT) %>%
              summarize(TOTAL_UNITS = n_distinct(PERMIT)) %>%
              ungroup() %>%
              mutate(VESSEL_OR_PLANT = ifelse(VESSEL_OR_PLANT == "V", "Vessels", "Plants")) %>%
              rename(INCIDENT_UNIT = VESSEL_OR_PLANT)) %>%
  pivot_wider(names_from = INCIDENT_UNIT, values_from = TOTAL_UNITS) %>%
  # Add total statements/category
  cross_join(subcat_units_rate %>%
               filter(CALENDAR_YEAR == adp_yr) %>%
               group_by(CATEGORY) %>%
               summarize(`# of Statements` = sum(N_STATEMENTS)), .) %>%
  mutate(CATEGORY = str_to_title(CATEGORY),
         CATEGORY = case_when(str_detect(CATEGORY, "Uscg-Equipment") ~
                                "Safety-USCG: Equipment",
                              str_detect(CATEGORY, "Uscg-Fail") ~
                                "Safety-USCG: Fail to Conduct Drills and/or Safety Orientation",
                              str_detect(CATEGORY, "Uscg-Marine") ~
                                "Safety-USCG: Marine Casualty",
                              str_detect(CATEGORY, "Observer Safety") ~
                                "Observer Safety and Work Environment",
                              str_detect(CATEGORY, "Permits") ~
                                "Permits/Documents/Record Keeping and Reporting",
                              str_detect(CATEGORY, "Marpol") ~
                                "MARPOL/Oil Spill",
                              str_detect(CATEGORY, "Interference") ~
                                "Interference with Duties",
                              TRUE ~ CATEGORY)) %>%
  relocate(Vessels, Plants, .after = `# of Statements`) %>%
  relocate(Hauls, Samples, `Marine Mammal Interactions`, .after = Offloads)

# Create flextable object
sum_tab <- flextable(summary_units) %>%
  bold(bold = TRUE, part = "header") %>%
  #merge_v(j = c(3:ncol(summary_units))) %>%
  flextable::surround(i = ~CATEGORY %like% "Fisheries", border.bottom = fp_border(width = 1)) %>%
  flextable::surround(part = "header", border.top = fp_border(width = 1), border.bottom = fp_border(width = 1)) %>%
  valign(part = "header", valign = "bottom") %>%
  height(i = c(1:nrow(summary_units)), part = "body", height = 0.42, unit = "in") %>%
  hrule(part = "body", rule = "exact") %>%
  vline(j = 2) %>%
  width(j = 1, 2.19, unit = "in") %>%
  width(j = 2, 0.88, unit = "in") %>%
  width(j = 3, 0.63, unit = "in") %>%
  width(j = 4, 0.56, unit = "in") %>%
  width(j = 5, 1, unit = "in") %>%
  width(j = 6, 0.63, unit = "in") %>%
  width(j = 7, 0.56, unit = "in") %>%
  width(j = 8, 0.75, unit = "in") %>%
  width(j = 9, 0.63, unit = "in") %>%
  width(j = 10, 0.69, unit = "in") %>%
  width(j = 11, 0.98, unit = "in")

# Save to DOCX
save_as_docx(sum_tab,
             path = paste0("confidential_figures/tbl_", adp_yr, "_statement_summary.docx"),
             pr_section = officer::prop_section(page_size = page_size(orient = "landscape"),
                                                page_margins = page_mar(left = 0.25, right = 0.25)))

#'*----------------------------------------------------------*
## Block format ----
summary_units_2 <- subcat_units_rate %>%
  filter(CALENDAR_YEAR == adp_yr) %>%
  group_by(CATEGORY) %>%
  summarize(`# of Statements` = sum(N_STATEMENTS)) %>%
  mutate(CATEGORY = str_to_title(CATEGORY),
         CATEGORY = case_when(str_detect(CATEGORY, "Uscg-Equipment") ~
                                "Safety-USCG: Equipment",
                              str_detect(CATEGORY, "Uscg-Fail") ~
                                "Safety-USCG: Fail to Conduct Drills and/or Safety Orientation",
                              str_detect(CATEGORY, "Uscg-Marine") ~
                                "Safety-USCG: Marine Casualty",
                              str_detect(CATEGORY, "Observer Safety") ~
                                "Observer Safety and Work Environment",
                              str_detect(CATEGORY, "Permits") ~
                                "Permits/Documents/Record Keeping and Reporting",
                              str_detect(CATEGORY, "Marpol") ~
                                "MARPOL/Oil Spill",
                              str_detect(CATEGORY, "Interference") ~
                                "Interference with Duties",
                              TRUE ~ CATEGORY)) %>%
  {
    # Capture the row count from the incoming data (before filtering and transformations)
    nrow_before <- nrow(.)
    
    transformed_data <- subcat_units_rate %>%
      filter(CALENDAR_YEAR == adp_yr) %>%
      distinct(INCIDENT_UNIT, TOTAL_UNITS) %>%
      bind_rows(
        # Combine with assignments_dates_cr_perm after filtering and summarizing
        assignments_dates_cr_perm %>%
          filter(CALENDAR_YEAR == adp_yr) %>%
          group_by(VESSEL_OR_PLANT) %>%
          summarize(TOTAL_UNITS = n_distinct(PERMIT)) %>%
          ungroup() %>%
          mutate(VESSEL_OR_PLANT = ifelse(VESSEL_OR_PLANT == "V", "Vessels", "Plants")) %>%
          rename(INCIDENT_UNIT = VESSEL_OR_PLANT)
      )
    # Capture the row count after transformation
    nrow_after <- nrow(transformed_data)
    # Pad with NA rows to match the original row count
    final_data <- transformed_data %>%
      bind_rows(
        data.frame(INCIDENT_UNIT = rep(NA, nrow_before - nrow_after))
      ) %>% 
      rename(`Unit Type` = INCIDENT_UNIT,
             `Total Units` = TOTAL_UNITS) %>%
      mutate(`Unit Type` = fct_recode(factor(`Unit Type`),
                                      "Deployments" = "DEPL",
                                      "Hauls" = "HAUL",
                                      "Days" = "DAYS",
                                      "Trips" = "TRIP",
                                      "Offloads" = "OFFL",
                                      "Samples" = "SAMP",
                                      "Marine Mammal Interactions" = "MARM"),
             `Unit Type` = fct_relevel(`Unit Type`,
                                       "Deployments", "Vessels", "Plants", "Days",
                                       "Trips", "Offloads", "Hauls", "Samples",
                                       "Marine Mammal Interactions")) %>%
      arrange(`Unit Type`)
    # Now combine the incoming data (.) with the transformed data
    final_data <- cbind(., final_data)
  }

# Create flextable object
sum_tab2 <- flextable(summary_units_2) %>%
  bold(bold = TRUE, part = "header") %>%
  flextable::surround(i = ~CATEGORY %like% "Fisheries", border.bottom = fp_border(width = 1)) %>%
  flextable::surround(part = "header", border.top = fp_border(width = 1), border.bottom = fp_border(width = 1)) %>%
  valign(part = "header", valign = "bottom") %>%
  valign(part = "body", valign = "center") %>%
  height(i = c(1:nrow(summary_units_2)), part = "body", height = 0.27, unit = "in") %>%
  hrule(part = "body", rule = "exact") %>%
  vline(j = c(2, 4)) %>%
  vline_left() %>%
  width(j = 1, 4.08, unit = "in") %>%
  width(j = 2, 0.88, unit = "in") %>%
  width(j = 3, 1.96, unit = "in") %>%
  width(j = 4, 0.69, unit = "in")

# Save to DOCX
save_as_docx(sum_tab2,
             path = paste0("confidential_figures/tbl_", adp_yr, "_statement_summary_2.docx"))

#'`================================================================================`

# Placeholder until Andy is done in AR_2 & AR_3, then make sure this is in there
subcat_units_rate_priority <-
  subcat_units_rate %>%
  filter(SUBCATEGORY %in% c("ACCESS", "ASSAULT", "BIN MONITORING",
                            "DESTRUCTION OF SAMPLE/WORK/PERSONAL EFFECTS",
                            "FOOD AND ACCOMMODATIONS", "FORCED TO PERFORM CREW DUTIES",
                            "HOSTILE WORK ENVIRONMENT", "IMPEDIMENT",
                            "INTIMIDATION/BRIBERY/COERCION", "MARINE CASUALTY",
                            "NOTIFICATION", "OBSERVER SAMPLING STATION",
                            "REASONABLE ASSISTANCE", "SAFETY",
                            "SAMPLING INTERFERENCE", "SCALES", "SEXUAL ASSAULT",
                            "SEXUAL HARASSMENT", "VIDEO MONITORING SYSTEM",
                            "VMS REQUIREMENTS")) %>%
  mutate(CAT_COMBO = ifelse(str_detect(CATEGORY, "USCG|SAFETY"),
                            "OBSERVER SAFETY AND WORK ENVIRONMENT / USCG-SAFETY", CATEGORY))

#'`================================================================================`
# Heatmaps ----

library(patchwork)

# Because of the way ggplot handles faceting, each category will need to be plotted
#  separately and then combined using patchwork so that tiles are the same size across
#  panels.

# Plots will need to be adjusted depending on what categories/subcategories are included
#  For example, not all category/subcategory combinations contain the same units.
#  In these cases, you'll need to adjust "incident_units" in the "process_data()" fxn
#  when creating the plots so that they match between all categories that will be
#  plotted together.

#'*----------------------------------------------------------*
## Inputs ----

# Factor order for plotting
fact_order <- c("DEPLOY", "DAYS", "TRIP", "OFFLOAD", "HAUL", "SAMPLE", "MAR MAM")

# Rate
rate_x <- 100

#'*----------------------------------------------------------*
## Functions ----

# Plot function
#  The create_plot function is designed to create a customizable faceted heatmap-style
#  plot in ggplot2. It is flexible in terms of handling facet labels, legend placement,
#  and formatting based on the user's input.
create_plot <- function(data, txt_width, type = c("top", "mid", "bottom")) {
  # txt_width = width in characters for facet row labels before being wrapped
  # top:    include facet labels for columns but no legend
  # mid:    no facet labels for columns or legend
  # bottom: no facet labels for columns, legend on right
  
  type <- match.arg(type)  # Ensure valid input for type
  
  plot <- ggplot(data) +
    geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)),
                  fill = .data[[paste0("RATE_X_", rate_x, "_plot")]])) +
    geom_text(aes(x = 1, y = SUBCATEGORY,
                  label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
              color = "white", size = 4) +
    facet_grid(cols = vars(INCIDENT_UNIT),
               rows = vars(CAT_COMBO),
               labeller = labeller(CAT_COMBO = label_wrap_gen(width = txt_width)),
               scales = "free_y") +
    theme_bw() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 8),
        panel.grid = element_blank(),
        plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm"),
        panel.spacing.x = unit(0.1, "cm")
      )
    
    # Add theme customization based on the type
    if (type == "top") {
      plot + theme(
        legend.position = "none"
      )
    } else if (type == "mid") {
      plot + theme(
        strip.text.x = element_blank(), 
        legend.position = "none"
      )
    } else if (type == "bottom") {
      plot + theme(
        legend.title = element_text(size = 8),
        strip.text.x = element_blank(),
        legend.text = element_text(size = 8)
      )
    }
}

# Define common color scale with global limits
#  The color_scale function is used to apply a customized color gradient scale
#  to a numeric variable, typically a rate. It provides a navy-to-red gradient,
#  adjusts the scale based on the data's range and a user-defined upper limit (max),
#  and customizes the appearance of the color scale's legend.
color_scale <- function(data, max) {
  # max: set appropriate "threshold_" object
  
  color_scale <- scale_fill_gradient(
    low = "navy", 
    high = "red",
    na.value = "transparent", 
    name = paste0("Rate (× ", rate_x, ")"),
    limits = c(min(data[[paste0("RATE_X_", rate_x)]], na.rm = TRUE),
               max(max, na.rm = TRUE)),
    # Legend (currently not used)
    breaks = data.frame(n = seq(1, max(data[[paste0("RATE_X_", rate_x)]]) + 25, by = 25)) %>%
      mutate(n = ifelse(n > 1, n - 1, n)) %>% unlist(use.names = FALSE),
    guide = guide_colorbar(
      order = 2,
      title.hjust = 0.8,
      ticks = FALSE,
      barheight = unit(1.33, "in"),
      barwidth = unit(0.33, "in")
    )
  )
}

# Process data for individual plots
#  The process_data function processes a dataset by:
#  1) Filtering rows based on a search string that identifies a specific category
#     in the CAT_COMBO column.
#  2) Adding missing incident units for visualization, ensuring that all required
#     INCIDENT_UNIT types are present in the dataset.
#  3) Adjusting the factor levels of the INCIDENT_UNIT column to a predefined
#     order for consistent plotting.
process_data <- function(data, search_string, incident_units) {
  # search_string:  unique string that identifies a specific category.
  #       enter as a string, e.g., "SPECIES"
  # incident_units: missing unit types to be added for plotting.
  #       enter as: c("UNIT_TYPE_1", "UNIT_TYPE_2", "ETC.")
  
  data %>%
    filter(str_detect(CAT_COMBO, search_string)) %>%
    bind_rows(.,
              tibble(CAT_COMBO = pull(., CAT_COMBO)[1],
                     SUBCATEGORY = pull(., SUBCATEGORY)[1],
                     INCIDENT_UNIT = incident_units)) %>%
    mutate(INCIDENT_UNIT = factor(INCIDENT_UNIT, levels = fact_order))
}

# Set threshold value where all colors >= will be the "hottest" color
#  Threshold is needed when there are large outliers
#  The function will check the maximum value within the data and if it is 2×
#  greater or more than the next largest value, it will set a threshold
#  value = 1.55× the second largest value 
calculate_threshold <- function(df, return_name) {
  # return_name: the object name you want the value to be returned as
  
  # Compute threshold priority
  threshold <- df %>%
    filter(CALENDAR_YEAR == adp_yr) %>%
    select(RATE) %>%
    filter(!is.na(RATE)) %>%
    unlist() %>%
    { 
      max_rate <- max(.)
      second_largest_rate <- sort(., decreasing = TRUE)[2]
      ifelse(max_rate < 2 * second_largest_rate,
             max_rate * rate_x,
             1.55 * second_largest_rate * rate_x)
    }
  # Dynamically create the variable name in the parent environment
  assign(return_name, threshold, envir = parent.frame())
}

#'*----------------------------------------------------------*
## Check data values ----
# Priority subcategories
ggplot(filter(subcat_units_rate_priority, CALENDAR_YEAR == adp_yr),
       aes(x = SUBCATEGORY, y = RATE * rate_x)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

calculate_threshold(subcat_units_rate_priority, "threshold_priority")

# Non-priority subcategories
ggplot(filter(subcat_units_rate,
              !(SUBCATEGORY %in% subcat_units_rate_priority$SUBCATEGORY) &
                CALENDAR_YEAR == adp_yr),
       aes(x = SUBCATEGORY, y = RATE * rate_x)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

calculate_threshold(filter(subcat_units_rate,
                           !(SUBCATEGORY %in% subcat_units_rate_priority$SUBCATEGORY)),
                    "threshold_other")

#'*----------------------------------------------------------*
## Prepare data ----
# Priority subcategories
subcat_priority <- subcat_units_rate_priority %>%
  filter(CALENDAR_YEAR == adp_yr) %>%
  mutate(!! paste0("RATE_X_", rate_x) := RATE * rate_x,
         # Create dummy column for color scale
         !! paste0("RATE_X_", rate_x, "_plot") := pmin(RATE, threshold_priority/rate_x) * rate_x,
         INCIDENT_UNIT = case_when(INCIDENT_UNIT == "OFFL" ~ "OFFLOAD",
                                   INCIDENT_UNIT == "SAMP" ~ "SAMPLE",
                                   INCIDENT_UNIT == "DEPL" ~ "DEPLOY",
                                   INCIDENT_UNIT == "MARM" ~ "MAR MAM",
                                   TRUE ~ INCIDENT_UNIT),
         CAT_COMBO = ifelse(str_detect(CATEGORY, "GEAR"), "GEAR / EQUIPMENT REQUIREMENTS",
                            CAT_COMBO)) %>%
  select(CAT_COMBO, SUBCATEGORY, INCIDENT_UNIT, paste0("RATE_X_", rate_x),
         paste0("RATE_X_", rate_x, "_plot"))

# Non-priority subcategories
subcat_other <- subcat_units_rate %>%
  filter(CALENDAR_YEAR == adp_yr,
         !(SUBCATEGORY %in% subcat_priority$SUBCATEGORY)) %>%
  mutate(CAT_COMBO = case_when(str_detect(CATEGORY, "SAFETY-USCG|MARPOL") ~
                                 "SAFETY-USCG / MARPOL / OIL SPILL",
                               str_detect(CATEGORY, "PERMITS/DOCUMENTS|OPERATIONAL|GEAR") ~
                                 "GEAR / EQUIPMENT / OPERATIONAL REQUIREMENTS / PERMITS / DOCUMENTS / RECORD KEEPING AND REPORTING",
                               str_detect(CATEGORY, "SUSTAINABLE|SPECIES") ~
                                 "PROHIBITED SPECIES / MARINE MAMMALS / SEABIRDS / SUSTAINABLE FISHERIES",
                               TRUE ~ CATEGORY),
         INCIDENT_UNIT = case_when(INCIDENT_UNIT == "OFFL" ~ "OFFLOAD",
                                   INCIDENT_UNIT == "SAMP" ~ "SAMPLE",
                                   INCIDENT_UNIT == "DEPL" ~ "DEPLOY",
                                   INCIDENT_UNIT == "MARM" ~ "MAR MAM",
                                   TRUE ~ INCIDENT_UNIT),
         !! paste0("RATE_X_", rate_x) := RATE * rate_x,
         # Create dummy column for color scale
         !! paste0("RATE_X_", rate_x, "_plot") := pmin(RATE, threshold_other/rate_x) * rate_x) %>%
  select(CAT_COMBO, SUBCATEGORY, INCIDENT_UNIT, paste0("RATE_X_", rate_x),
         paste0("RATE_X_", rate_x, "_plot"))

#'*----------------------------------------------------------*
## Priority categories plot ----

# Step 1: Create individual plots for each CATEGORY
# Plot for CATEGORY_1
plot1 <- create_plot(process_data(subcat_priority, "GEAR",
                                  c("OFFLOAD", "SAMPLE")),
                     txt_width = 17,
                     "top") +
  color_scale(subcat_priority, max = threshold_priority)

# plot for CATEGORY_2
plot2 <- create_plot(process_data(subcat_priority, "INTERFERENCE",
                                  c("TRIP")),
                     txt_width = 15,
                     "mid") +
  color_scale(subcat_priority, max = threshold_priority)

# Plot for CATEGORY_3
plot3 <- create_plot(process_data(subcat_priority, "USCG",
                                  c("HAUL", "OFFLOAD", "SAMPLE")),
                     txt_width = 37,
                     "mid") +
  color_scale(subcat_priority, max = threshold_priority)

# Step 2: Combine the plots using patchwork
priority_subcat_plot <- plot1 / plot2 / plot3 + plot_layout(
  heights = subcat_priority %>%
    group_by(CAT_COMBO) %>%
    summarize(n = n_distinct(SUBCATEGORY)) %>%
    .[["n"]]) +
  theme(
    axis.title.y = element_text(size = 10,
                                hjust = 1.2, # Likely have to play around with this to get it centered
                                margin = margin(r = 20),
                                angle = 90)) +
  labs(y = "SUBCATEGORY")

# Step 3: Display and save the combined plot
priority_subcat_plot

ggsave(paste0("confidential_figures/fig_", adp_yr, "_priority_subcat_plot.jpg"),
       device = "jpeg",
       width = 9,
       height = 6,
       units = "in",
       dpi = 300)

#'*----------------------------------------------------------*
### ALT: Priority categories plot ----
# This still needs to be cleaned up a lot, but here is what the data look like
# Wanted you to at least have something while I'm away CG 20250328

library(ggh4x)

color_scale_factor <- function(data, max) {
  # max: set appropriate "threshold_" object
  
  color_scale <- scale_fill_gradient(
    low = "navy", 
    high = "red",
    na.value = "transparent", 
    name = paste0("Rate (× ", rate_x, ")"),
    limits = c(min(data[[paste0("RATE_X_", rate_x)]], na.rm = TRUE),
               max(max, na.rm = TRUE))
  )
}

create_factor_plot <- function(data, txt_width, type = c("top", "mid", "bottom")) {
  # txt_width = width in characters for facet row labels before being wrapped
  # top:    include facet labels for columns but no legend
  # mid:    no facet labels for columns or legend
  # bottom: no facet labels for columns, legend on right
  
  type <- match.arg(type)  # Ensure valid input for type
  
  plot <- ggplot(data) +
    geom_tile(aes(x = VESSEL_TYPE, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)),
                  fill = .data[[paste0("RATE_X_", rate_x, "_plot")]])) +
    geom_text(aes(x = VESSEL_TYPE, y = SUBCATEGORY,
                  label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
              color = "white", size = 4) +
    facet_nested(rows = vars(CAT_COMBO),
                 cols = vars(INCIDENT_UNIT, COVERAGE_TYPE, NMFS_REGION),
                 labeller = labeller(CAT_COMBO = label_wrap_gen(width = txt_width)),
                 scales = "free_y") +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 8, color = "black"),
      axis.title.y = element_blank(),
      strip.text = element_text(size = 8),
      panel.grid = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      panel.spacing.x = unit(0.1, "cm"),
      legend.position = "none",
      axis.text.x = element_text(angle = 90, hjust= 0, vjust = 0.5)
    )
  
  # # Add theme customization based on the type
  # if (type == "top") {
  #   plot + theme(
  #     axis.text.x = element_blank(),
  #     legend.position = "none"
  #   )
  # } else if (type == "mid") {
  #   plot + theme(
  #     axis.text.x = element_blank(),
  #     strip.text.x = element_blank(), 
  #     legend.position = "none"
  #   )
  # } else if (type == "bottom") {
  #   plot + theme(
  #     strip.text.x = element_blank(),
  #     legend.position = "none"
  #   )
  # }
}

subcat_units_rate_for_factors_priority <-
  subcat_units_rate_for_factors %>%
  filter(SUBCATEGORY %in% c("ACCESS", "ASSAULT", "BIN MONITORING",
                            "DESTRUCTION OF SAMPLE/WORK/PERSONAL EFFECTS",
                            "FOOD AND ACCOMMODATIONS", "FORCED TO PERFORM CREW DUTIES",
                            "HOSTILE WORK ENVIRONMENT", "IMPEDIMENT",
                            "INTIMIDATION/BRIBERY/COERCION", "MARINE CASUALTY",
                            "NOTIFICATION", "OBSERVER SAMPLING STATION",
                            "REASONABLE ASSISTANCE", "SAFETY",
                            "SAMPLING INTERFERENCE", "SCALES", "SEXUAL ASSAULT",
                            "SEXUAL HARASSMENT", "VIDEO MONITORING SYSTEM",
                            "VMS REQUIREMENTS")) %>%
  mutate(CAT_COMBO = ifelse(str_detect(CATEGORY, "USCG|SAFETY"),
                            "OBSERVER SAFETY AND WORK ENVIRONMENT / USCG-SAFETY", CATEGORY))

calculate_threshold(subcat_units_rate_for_factors_priority, "threshold_factor_priority")

subcat_factors_priority <- subcat_units_rate_for_factors_priority %>%
  filter(CALENDAR_YEAR == adp_yr) %>%
  mutate(!! paste0("RATE_X_", rate_x) := RATE * rate_x,
         # Create dummy column for color scale
         !! paste0("RATE_X_", rate_x, "_plot") := pmin(RATE, threshold_factor_priority/rate_x) * rate_x,
         INCIDENT_UNIT = case_when(INCIDENT_UNIT == "OFFL" ~ "OFFLOAD",
                                   INCIDENT_UNIT == "SAMP" ~ "SAMPLE",
                                   INCIDENT_UNIT == "DEPL" ~ "DEPLOY",
                                   INCIDENT_UNIT == "MARM" ~ "MAR MAM",
                                   TRUE ~ INCIDENT_UNIT),
         CAT_COMBO = ifelse(str_detect(CATEGORY, "GEAR"), "GEAR / EQUIPMENT REQUIREMENTS",
                            CAT_COMBO)) %>%
  select(CAT_COMBO, SUBCATEGORY, INCIDENT_UNIT, paste0("RATE_X_", rate_x),
         paste0("RATE_X_", rate_x, "_plot"), COVERAGE_TYPE, VESSEL_TYPE, NMFS_REGION) %>%
  filter(!is.na(NMFS_REGION))

create_factor_plot(subcat_factors_priority,
                   txt_width = 17) +
  color_scale_factor(subcat_factors_priority, max = threshold_factor_priority)

#'*----------------------------------------------------------*
## Non-priority categories plot ----

# Step 1: Create individual plots for each CATEGORY
# Plot for CATEGORY_1
plot1 <- create_plot(process_data(subcat_other, "CONTRACTOR",
                                  c("HAUL", "MAR MAM", "OFFLOAD", "SAMPLE", "TRIP")),
                     txt_width = 15,
                     "top") +
  color_scale(subcat_other, max = threshold_other)

# Plot for CATEGORY_2
plot2 <- create_plot(process_data(subcat_other, "OPERATIONAL REQ",
                                  c("DEPLOY", "MAR MAM")),
                     txt_width = 55,
                     "mid") +
  color_scale(subcat_other, max = threshold_other)

# Plot for CATEGORY_3
plot3 <- create_plot(process_data(subcat_other, "SPECIES",
                                  c("DAYS", "DEPLOY", "SAMPLE")),
                     txt_width = 37,
                     "mid") +
  color_scale(subcat_other, max = threshold_other)

# Plot for CATEGORY_4
plot4 <- create_plot(process_data(subcat_other, "USCG",
                                  c("DEPLOY", "HAUL", "MAR MAM", "OFFLOAD", "SAMPLE")),
                     txt_width = 19,
                     "mid") +
  color_scale(subcat_other, max = threshold_other)

# Step 2: Combine the plots using patchwork
non_priority_subcat_plot <- plot1 / plot2 / plot3 / plot4 + plot_layout(
  heights = subcat_other %>%
    group_by(CAT_COMBO) %>%
    summarize(n = n_distinct(SUBCATEGORY)) %>%
    .[["n"]]) +
  theme(
    axis.title.y = element_text(size = 10,
                                hjust = 17, # Likely have to play around with this to get it centered
                                margin = margin(r = 20),
                                angle = 90)) +
  labs(y = "SUBCATEGORY")

# Step 3: Display and save the combined plot
non_priority_subcat_plot

ggsave(paste0("confidential_figures/fig_", adp_yr, "_non_priority_subcat_plot.jpg"),
       device = "jpeg",
       width = 10,
       height = 9.5,
       units = "in",
       dpi = 300)

#'*----------------------------------------------------------*
### ALT: Non-priority categories plot ----
# This can be cleaned up a lot, but I just wanted to get it in here since I'll be away CG 20250328

plot1 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "DEP"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm"),
    panel.spacing.x = unit(0.1, "cm")
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot2 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "DAY"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm"),
    panel.spacing.x = unit(0.1, "cm")
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot3 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "TRIP"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm"),
    panel.spacing.x = unit(0.1, "cm")
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot4 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "OFF"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm"),
    panel.spacing.x = unit(0.1, "cm")
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot5 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "HAUL"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm"),
    panel.spacing.x = unit(0.1, "cm")
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot6 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "SAM"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm"),
    panel.spacing.x = unit(0.1, "cm")
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot7 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "MAR"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm"),
    panel.spacing.x = unit(0.1, "cm")
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

non_priority_subcat_plot <- plot1 / plot2 / plot3 / plot4 / plot5 / plot6 / plot7 + plot_layout(
  heights = subcat_other %>%
    mutate(INCIDENT_UNIT = factor(INCIDENT_UNIT, levels = fact_order)) %>%
    group_by(INCIDENT_UNIT) %>%
    summarize(n = n_distinct(SUBCATEGORY)) %>%
    .[["n"]]) +
  theme(
    axis.title.y = element_text(size = 10,
                                hjust = 17, # Likely have to play around with this to get it centered
                                margin = margin(r = 20),
                                angle = 90)) +
  labs(y = "SUBCATEGORY")

non_priority_subcat_plot

ggsave(paste0("confidential_figures/fig_", adp_yr, "_non_priority_subcat_plot_long.jpg"),
       device = "jpeg",
       width = 7,
       height = 10.5,
       units = "in",
       dpi = 300)


# Clear unneeded objects
rm(color_scale, create_plot, process_data, fact_order, rate_x,
   plot1, plot2, plot3, plot4, subcat_other, subcat_priority, threshold_other,
   threshold_priority, calculate_threshold)

# Save Output -------------------------------------------------------------

file_4_name <- "AR_4_summary_tables_output.Rdata"

save(list = ls(),
     file = file_4_name)

gdrive_upload(file_4_name, AnnRpt_EnfChp_dribble)
