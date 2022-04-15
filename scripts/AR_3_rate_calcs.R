
####################
# Annual Report Enforcement chapter: Rate calculations
# Contact Andy Kingham
# 206-526-4212

library(plyr)
library(reshape2)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(sqldf)
library(devtools)


# load the data files.
# * chng wd filepath as needed *
rm(list = ls())
load(file = "scripts/AR_rolling_join_output.rdata")



###############
# Summarize the days for each factor combination.


# Count deployed days.
# Frst count the SUM TOTAL of days for cruise/permit/all factors.
# This double-counts days where multiple factors occur, and is used to WEIGHT the statements in later step: WIEGHTING METHOD 1.
# Next count DISTINCT days for each cruise/permit ONLY.  Used for WIEGHTING METHOD 2.  Update: this method is not used.
# Finally count DISTINCT days for each cruise/permit/factor combination

cnt_dep_days_all_groupings <- 
  merge(assnmts_days_all_groupings %>%
          group_by(CALENDAR_YEAR, CRUISE, PERMIT, OBSERVER_SEQ) %>%
          summarize(TOTAL_FACTOR_DAYS_CR_PERM              = length(DEPLOYED_DATE),
                    TOTAL_DISTINCT_DAYS_CR_PERM            = length(unique(DEPLOYED_DATE)),
                    TOTAL_FACTOR_FSHRY_DATA_DAYS_CR_PERM   = length(DEPLOYED_DATE[FISHERY_DATA_BOOL == 'Y']),
                    TOTAL_DISTINCT_FSHRY_DATA_DAYS_CR_PERM = length(unique(DEPLOYED_DATE[FISHERY_DATA_BOOL == 'Y']))
          ) %>%
          ungroup() ,
        assnmts_days_all_groupings %>% 
          group_by(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION) %>%
          summarize(FACTOR_DAYS = n_distinct(DEPLOYED_DATE),
                    FACTOR_FSHRY_DATA_DAYS = length(unique(DEPLOYED_DATE[FISHERY_DATA_BOOL == 'Y'])) ),
        all = TRUE) %>%
  mutate(FACTOR_WEIGHT_MTHD_1 = FACTOR_DAYS/TOTAL_FACTOR_DAYS_CR_PERM,       # Method 1 for WEIGHTING the statements will apportion the number of instances to the number of days in that factor category.
         FACTOR_WEIGHT_MTHD_2 = FACTOR_DAYS/TOTAL_DISTINCT_DAYS_CR_PERM,     # Method 2 for WEIGHTING the statements will apportion the number of instances to the number of distinct days in that factor category.
         FACTOR_WEIGHT_MTHD_3 = FACTOR_FSHRY_DATA_DAYS/TOTAL_FACTOR_FSHRY_DATA_DAYS_CR_PERM,       # Method 3 for WEIGHTING the statements will apportion the number of instances to the number of FISHERY_DATA days in that factor category.
         FACTOR_WEIGHT_MTHD_4 = FACTOR_FSHRY_DATA_DAYS/TOTAL_DISTINCT_FSHRY_DATA_DAYS_CR_PERM) %>% # Method 4 for WEIGHTING the statements will apportion the number of instances to the number of DISTINCT FISHERY DATA DAYS in that factor category.
  select(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, FACTOR_DAYS, TOTAL_FACTOR_DAYS_CR_PERM, TOTAL_DISTINCT_DAYS_CR_PERM, FACTOR_WEIGHT_MTHD_1, FACTOR_WEIGHT_MTHD_2, FACTOR_WEIGHT_MTHD_3, FACTOR_WEIGHT_MTHD_4)





######################
# Finally!!!!
# Calculate the rates.  
# There are THREE rates that are calculated.
#     # The FIRST is for each individual statement category (affi_type) and each unique factor grouping
#     # The SECOND, is for the higher-level, OLE_CATEGORY and each unique factor grouping.
#     # The THIRD is with NO factors (all aggregated).


# First rate: by affi_type, for each factor combination.
rate_all_groupings_affi_type <-
  # First, summarize for each factor combination and get the total days etc.  These are the DENOMINATORS of the rates.
  cnt_dep_days_all_groupings %>% 
  group_by(CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION) %>%
  summarize(TOTAL_DAYS      = sum(FACTOR_DAYS), 
            TOTAL_OBSERVERS = n_distinct(OBSERVER_SEQ),
            TOTAL_CRUISES   = n_distinct(CRUISE),
            DISTINCT_OBSERVER_ASSIGNMENTS = n_distinct(CRUISE, PERMIT) ) %>%
  ungroup() %>%
  # Next, join raw_statements to the dep_days DF on cruise/permit, and sum to get the WEIGHTED number_of_incidents and number_of_statements. This is the NUMERATOR of the rates.
  left_join(cnt_dep_days_all_groupings %>% # LEFT join ensures all days where NO statements were written are counted!! Critical to accurate rate calc.
              inner_join(raw_statements %>%
                           mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR)) %>%  
              group_by (CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, OLE_CATEGORY, AFFIDAVIT_TYPE) %>%
              summarize(TOTAL_STATEMENTS = sum(if_else(is.na(AFFIDAVIT_ID), 0, FACTOR_WEIGHT_MTHD_1)),
                        TOTAL_INCIDENTS  = sum(if_else(is.na(AFFIDAVIT_ID), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1)) ) %>%
              ungroup() )  %>%
  #Last, calculate the rates.
  mutate(CONFI_FLAG = ifelse(DISTINCT_OBSERVER_ASSIGNMENTS < 3, 1, 0), # for confidentiality, flag any factor combinations with < 3 cruise/permit assignments, these will be removed later.
         AFFIS_PER_DAY         = TOTAL_STATEMENTS/TOTAL_DAYS,
         INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/TOTAL_DAYS,
         STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/TOTAL_DAYS)*1000,
         INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/TOTAL_DAYS)*1000,
         STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/TOTAL_DAYS)*90,
         INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/TOTAL_DAYS)*90,
         STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/TOTAL_OBSERVERS,
         INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/TOTAL_OBSERVERS,
         STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/TOTAL_CRUISES,
         INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/TOTAL_CRUISES,
         STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/DISTINCT_OBSERVER_ASSIGNMENTS,
         INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/DISTINCT_OBSERVER_ASSIGNMENTS
  )


# make a separate df for plots, all this does is it re-orders the factors, so it looks better.
rate_all_groupings_affi_type_for_plots <-
  rate_all_groupings_affi_type %>% 
  mutate(VESSEL_TYPE = factor(VESSEL_TYPE, levels = c('PLANT', 'CP/MS', 'CV')),
         GEAR_TYPE   = factor(if_else(is.na(GEAR_TYPE), ' ', GEAR_TYPE),   levels = c('NPT', 'PTR', 'HAL', 'POT', ' ')),
         NMFS_REGION = factor(if_else(is.na(NMFS_REGION), ' ', NMFS_REGION),   levels = c('BSAI', 'GOA', ' '))
  )

# output this for OLE to examine.  It includes confidential data so not for public use.
write.csv(file = paste("charts_and_tables/tables/tbl_", 
                       adp_yr, 
                       "_rate_all_groupings_by_affi_type_", 
                       Sys.Date(), ".csv", 
                       sep = ''),
          x    = rate_all_groupings_affi_type_for_plots)









############
# Make summary table of just the DEPLOYED DAYS for each factor grouping.
cnt_dep_days_by_factor_group <-
  rate_all_groupings_affi_type %>%
  distinct(CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, TOTAL_DAYS, TOTAL_CRUISES, TOTAL_OBSERVERS, DISTINCT_OBSERVER_ASSIGNMENTS, CONFI_FLAG
  ) %>%  
  mutate(FACTOR_GROUP = paste(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, sep = ''))





####################
# Make a DF that simply joins statements and cnt_dep_days.  Not really used in analysis, but useful for referencing.  
days_statements_all_groupings_raw  <-
  cnt_dep_days_all_groupings %>%
  left_join(raw_statements %>% mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR)) %>%
  filter(paste(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, sep = '') %in% cnt_dep_days_by_factor_group$FACTOR_GROUP
  ) %>%
  mutate(VESSEL_TYPE = factor(VESSEL_TYPE, levels = c('PLANT', 'CP/MS', 'CV')),
         GEAR_TYPE   = factor(if_else(is.na(GEAR_TYPE), ' ', GEAR_TYPE),   levels = c('NPT', 'PTR', 'HAL', 'POT', ' ')),
         NMFS_REGION = factor(if_else(is.na(NMFS_REGION), ' ', NMFS_REGION),   levels = c('BSAI', 'GOA', ' '))
  )















######################
# Second rate: by OLE_CATEOGRY, for each factor combination.

rate_all_groupings_ole_category <- 
  # First, summarize for each factor combination and get the total days etc.  These are the DENOMINATORS of the rates.
  cnt_dep_days_all_groupings %>% 
  group_by(CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION) %>%
  summarize(TOTAL_DAYS      = sum(FACTOR_DAYS), 
            TOTAL_OBSERVERS = n_distinct(OBSERVER_SEQ),
            TOTAL_CRUISES   = n_distinct(CRUISE),
            DISTINCT_OBSERVER_ASSIGNMENTS = n_distinct(CRUISE, PERMIT) ) %>%
  #  filter(DISTINCT_OBSERVER_ASSIGNMENTS >= 3) ) %>%  # for confidentiality, remove any factor combinations with < 3 cruise/permit assignments.
  ungroup() %>%
  # Next, join raw_statements to the dep_days DF on cruise/permit, and sum to get the WEIGHTED number_of_incidents and number_of_statements. This is the NUMERATOR of the rates.
  left_join(cnt_dep_days_all_groupings %>% # LEFT join ensures all days where NO statements were written are counted!! Critical to accurate rate calc.
              inner_join(
                raw_statements %>% 
                  mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR,  # need to make column names match
                         OLE_CATEGORY  = factor(OLE_CATEGORY, # need to re-order the levels for the final output table
                                                levels = c('OLE PRIORITY: INTER-PERSONAL',
                                                           'OLE PRIORITY: SAFETY AND DUTIES',
                                                           'COAST GUARD',
                                                           'LIMITED ACCESS PROGRAMS',
                                                           'PROTECTED RESOURCE & PROHIBITED SPECIES',
                                                           'ALL OTHER STATEMENT TYPES'))
                         )
                         ) %>%  
              group_by (CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, OLE_CATEGORY) %>%
              summarize(TOTAL_STATEMENTS = sum(if_else(is.na(AFFIDAVIT_ID), 0, FACTOR_WEIGHT_MTHD_1)),
                        TOTAL_INCIDENTS  = sum(if_else(is.na(AFFIDAVIT_ID), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1)) ) %>%
              ungroup() ) %>%
  # last, calculate the rates.
  mutate(CONFI_FLAG = ifelse(DISTINCT_OBSERVER_ASSIGNMENTS < 3, 1, 0), # for confidentiality, flag any factor combinations with < 3 cruise/permit assignments, these will be removed later.
         AFFIS_PER_DAY         = TOTAL_STATEMENTS/TOTAL_DAYS,
         INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/TOTAL_DAYS,
         STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/TOTAL_DAYS)*1000,
         INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/TOTAL_DAYS)*1000,
         STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/TOTAL_DAYS)*90,
         INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/TOTAL_DAYS)*90,
         STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/TOTAL_OBSERVERS,
         INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/TOTAL_OBSERVERS,
         STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/TOTAL_CRUISES,
         INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/TOTAL_CRUISES,
         STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/DISTINCT_OBSERVER_ASSIGNMENTS,
         INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/DISTINCT_OBSERVER_ASSIGNMENTS
  )





#############################
#  THIRD rate: no factor groupings.

# First, summarize ALL days and assignments, with no factors.  Use for reference and to 
# calculate rate with NO factors.
depl_days_all_summ <-
  assignments_dates_cr_perm %>%
  group_by(CALENDAR_YEAR) %>%
  summarize(DAYS = n_distinct(CRUISE, PERMIT, DEPLOYED_DATE),
            ASSIGNMENTS = n_distinct(CRUISE, PERMIT),
            CRUISES = n_distinct(CRUISE),
            OBSERVERS = n_distinct(OBSERVER_SEQ))



# calculate rate for NO factors (all aggregated)
rate_no_groupings <-
  raw_statements %>%
  group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, OLE_CATEGORY, AFFIDAVIT_TYPE) %>%
  summarize(N_OCCURRENCES = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS))) %>%
  ungroup() %>%
  full_join(depl_days_all_summ) %>%
  mutate(RATE_PER_1000_DAYS = (N_OCCURRENCES/DAYS)*1000,
         RATE_PER_ASSNMT    = (N_OCCURRENCES/ASSIGNMENTS))



##################
# Save Output -------------------------------------------------------------
#Requires the folder 'scripts'
save(list = ls(),
     file = paste0("scripts/", "AR_rate_output.rdata"))


