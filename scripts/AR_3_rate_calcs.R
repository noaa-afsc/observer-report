
####################
# Annual Report Enforcement chapter: Rate calculations
# Contact Andy Kingham
# 206-526-4212

# Set up environment -----------------------------------------------------------

# Load pkgs
if(!require("plyr"))        install.packages("plyr",        repos='http://cran.us.r-project.org')
if(!require("reshape2"))    install.packages("reshape2",    repos='http://cran.us.r-project.org')
if(!require("dplyr"))       install.packages("dplyr",       repos='http://cran.us.r-project.org')
if(!require("tidyr"))       install.packages("tidyr",       repos='http://cran.us.r-project.org')
if(!require("lubridate"))   install.packages("lubridate",   repos='http://cran.us.r-project.org')
if(!require("data.table"))  install.packages("data.table",  repos='http://cran.us.r-project.org')
if(!require("sqldf"))       install.packages("sqldf",       repos='http://cran.us.r-project.org')
if(!require("devtools"))    install.packages("devtools",    repos='http://cran.us.r-project.org')
if(!require("googledrive")) install.packages("googledrive", repos='http://cran.us.r-project.org')



# load the data files.----------------------------------------------------------
rm(list = ls())

# adp_yr is the year of the annual report we are doing this time (annual_deployment_year)
# NOTE: we need this to ensure we load the CORRECT YEAR.  Each year has it's own directory and Rdata files.
adp_yr <- rstudioapi::showPrompt(title = "ADP YEAR", message = "Enter the ADP YEAR for this analysis:", default = "")

file_2_name  <- "AR_2_rolling_join_output.Rdata"


# Pull Rdata file from google drive.
# NOTE: if the google drive file has not changed, the next 2 steps are not necessary: you can just load from your local.

# Identify the g-drive file to download
# MAKE SURE IT IS CORRECT GOOGLE PATH

# Folder name is below, commented out, because it is slow.as.eff. when executed this way.
# MUCH faster to use the hard-coded drive ID (see below)

# project_dribble <- googledrive::drive_get(paste0("FMA Analysis Group/FMA OLE Statements Project/FMA OLE Statements AR ch 5 Rdata files/",
#                                                 adp_yr))



## BEGIN UNCOMMENT HERE IF YOU NEED TO GO GET THE Rdata FILE FROM G-DRIVE
################

# project_dribble <- googledrive::drive_get(googledrive::as_id("10Qtv5PNIgS9GhmdhSPLOYNgBgn3ykwEA"))
# 
# data_dribble <- 
#   drive_ls(project_dribble) %>%
#   filter(name == file_2_name)
# 
# # Download the file from g-drive into local
# drive_download(
#   data_dribble,
#   path = file_2_name,
#   overwrite = T
# )

################
## END UNCOMMENT HERE IF YOU NEED TO GO GET THE Rdata FILE FROM G-DRIVE



load(file = file_2_name)




###############
# Combine all statements from the NEW and OLD systems into on data frame
statements_combined <-rbind(statements_raw_new, statements_raw_old)


# Summarize the days for each factor combination.

# Count deployed days.
# Frst count the SUM TOTAL of days for cruise/permit/all factors.
# This double-counts days where multiple factors occur, and is used to WEIGHT the statements in later step: WIEGHTING METHOD 1.
# Next count DISTINCT days for each cruise/permit ONLY.  Used for WIEGHTING METHOD 2.  Update: this method is not used.
# Finally count DISTINCT days for each cruise/permit/factor combination
# Weighting methods 3 and 4 are based on fishery-data-collection days as the denominator, rather than all deployment days.
# Weighting methods 3 and 4 are not currently used.


cnt_dep_days_all_groupings <- 
  merge(assnmts_days_all_groupings %>%
          group_by(CALENDAR_YEAR, OLE_SYSTEM, CRUISE, PERMIT, OBSERVER_SEQ) %>%
          summarize(TOTAL_FACTOR_DAYS_CR_PERM              = length(DEPLOYED_DATE),
                    TOTAL_DISTINCT_DAYS_CR_PERM            = length(unique(DEPLOYED_DATE)),
                    TOTAL_FACTOR_FSHRY_DATA_DAYS_CR_PERM   = length(DEPLOYED_DATE[FISHERY_DATA_BOOL == 'Y']),
                    TOTAL_DISTINCT_FSHRY_DATA_DAYS_CR_PERM = length(unique(DEPLOYED_DATE[FISHERY_DATA_BOOL == 'Y']))
          ) %>%
          ungroup() ,
        assnmts_days_all_groupings %>% 
          group_by(CALENDAR_YEAR, OLE_SYSTEM, OBSERVER_SEQ, CRUISE, PERMIT, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION) %>%
          summarize(FACTOR_DAYS = n_distinct(DEPLOYED_DATE),
                    FACTOR_FSHRY_DATA_DAYS = length(unique(DEPLOYED_DATE[FISHERY_DATA_BOOL == 'Y'])) ) %>%
          ungroup(),
        all = TRUE) %>%
  mutate(FACTOR_WEIGHT_MTHD_1 = FACTOR_DAYS/TOTAL_FACTOR_DAYS_CR_PERM,       # Method 1 for WEIGHTING the statements will apportion the number of instances to the number of days in that factor category.
         FACTOR_WEIGHT_MTHD_2 = FACTOR_DAYS/TOTAL_DISTINCT_DAYS_CR_PERM,     # Method 2 for WEIGHTING the statements will apportion the number of instances to the number of distinct days in that factor category.
         FACTOR_WEIGHT_MTHD_3 = FACTOR_FSHRY_DATA_DAYS/TOTAL_FACTOR_FSHRY_DATA_DAYS_CR_PERM,       # Method 3 for WEIGHTING the statements will apportion the number of instances to the number of FISHERY_DATA days in that factor category.
         FACTOR_WEIGHT_MTHD_4 = FACTOR_FSHRY_DATA_DAYS/TOTAL_DISTINCT_FSHRY_DATA_DAYS_CR_PERM) %>% # Method 4 for WEIGHTING the statements will apportion the number of instances to the number of DISTINCT FISHERY DATA DAYS in that factor category.
  select(CALENDAR_YEAR,  OLE_SYSTEM, OBSERVER_SEQ, CRUISE, PERMIT, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, FACTOR_DAYS, TOTAL_FACTOR_DAYS_CR_PERM, TOTAL_DISTINCT_DAYS_CR_PERM, FACTOR_WEIGHT_MTHD_1, FACTOR_WEIGHT_MTHD_2, FACTOR_WEIGHT_MTHD_3, FACTOR_WEIGHT_MTHD_4)





# Next, summarize for each factor combination and get the total days etc.  These are the DENOMINATORS of the rates.
depl_days_summ_by_factor <-
  cnt_dep_days_all_groupings %>% 
  group_by(CALENDAR_YEAR, OLE_SYSTEM, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION) %>%
  summarize(TOTAL_DAYS      = sum(FACTOR_DAYS), 
            TOTAL_OBSERVERS = n_distinct(OBSERVER_SEQ),
            TOTAL_CRUISES   = n_distinct(CRUISE),
            DISTINCT_OBSERVER_ASSIGNMENTS = n_distinct(CRUISE, PERMIT) ) %>%
  ungroup()


######################
# Finally!!!!
# Calculate the rates.  
# There are FOUR rates that are calculated.
#     # The FIRST is for each individual statement subcategory (SUBCAT) and each unique factor grouping
#     # The SECOND, is for the higher-level, OLD_OLE_CATEGORY and each unique factor grouping.
#     # The THIRD is with NO factors (all aggregated), by SUBCAT.
#     # The FOURTH is with NO factors (all aggregated), by OLD_OLE_CATEGORY.



# First rate: by subcat, for each factor combination.
rate_all_groupings_subcat <-
  depl_days_summ_by_factor %>%  #denominator, see above
  # Next, join raw_statements to the dep_days DF on cruise/permit, and sum to get the WEIGHTED number_of_incidents and number_of_statements. This is the NUMERATOR of the rates.
  left_join(cnt_dep_days_all_groupings %>% # LEFT join ensures all days where NO statements were written are counted!! Critical to accurate rate calc.
              inner_join(statements_combined %>%
                           mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR,
                                  PERMIT = as.numeric(PERMIT))) %>%  
              group_by (CALENDAR_YEAR, OLE_SYSTEM, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, OLD_OLE_CATEGORY, STATEMENT_TYPE) %>%
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


# make a separate df for plots, all this does is it re-orders the factors and 
# put spaces in for NA, so it looks better in tables and figures.
rate_all_groupings_subcat_for_plots <-
  rate_all_groupings_subcat %>% 
  mutate(VESSEL_TYPE = factor(VESSEL_TYPE, levels = c('PLANT', 'CP/MS', 'CV')),
         GEAR_TYPE   = factor(if_else(is.na(GEAR_TYPE), ' ', GEAR_TYPE),   levels = c('NPT', 'PTR', 'HAL', 'POT', ' ')),
         NMFS_REGION = factor(if_else(is.na(NMFS_REGION), ' ', NMFS_REGION),   levels = c('BSAI', 'GOA', ' '))
  )

# output this for OLE to examine.  It includes confidential data so not for public use.
#UPDATE: filtering out CONFI_FLAG rows
write.csv(file = paste0(adp_yr, "_outputs/charts_and_tables/tables/tbl_", 
                        adp_yr, 
                        "rate_all_groupings_subcat_for_plots"
),
x    = rate_all_groupings_subcat_for_plots %>%
  filter (CONFI_FLAG == 0 | is.na(CONFI_FLAG))
)





######################
# Second rate: by OLD_OLE_CATEGORY, for each factor combination.

rate_all_groupings_ole_category <- 
  depl_days_summ_by_factor %>%
  # Next, join raw_statements to the dep_days DF on cruise/permit, and sum to get the WEIGHTED number_of_incidents and number_of_statements. This is the NUMERATOR of the rates.
  left_join(cnt_dep_days_all_groupings %>% # LEFT join ensures all days where NO statements were written are counted!! Critical to accurate rate calc.
              inner_join(statements_combined %>% 
                           mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR,
                                  PERMIT = as.numeric(PERMIT)) # need to make column names match
              ) %>%  
              group_by (CALENDAR_YEAR, OLE_SYSTEM, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, OLD_OLE_CATEGORY) %>%
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





######################
# Third rate: by NEW_OLE_CATEGORY, for each factor combination.

rate_all_groupings_new_ole_category <- 
  depl_days_summ_by_factor %>%
  # Next, join raw_statements to the dep_days DF on cruise/permit, and sum to get the WEIGHTED number_of_incidents and number_of_statements. This is the NUMERATOR of the rates.
  left_join(cnt_dep_days_all_groupings %>% # LEFT join ensures all days where NO statements were written are counted!! Critical to accurate rate calc.
              inner_join(statements_combined %>% 
                           mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR,
                                  PERMIT = as.numeric(PERMIT)) # need to make column names match
              ) %>%  
              group_by (CALENDAR_YEAR, OLE_SYSTEM, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, NEW_OLE_CATEGORY) %>%
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









# Summarize ALL days and assignments
# Include an extra row that SINGLE COUNTS for 2023
# This is really just for display and clarity, becuase any cruise that overlapped 
# the cutoff date of 07/28/2023, those deployment days, assignments, etc will be counted for 
# BOTH the OLD and NEW SYSTEM, in this kind of summary.
# Note that all inner_joins for rates will IGNORE this row so it is not needed for the other rates.
depl_days_summ_all <-
  rbind( assignments_dates_cr_perm %>%
           group_by(CALENDAR_YEAR, 
                    OLE_SYSTEM    = if_else(CALENDAR_YEAR == adp_yr, 'COMBINED', 'OLD')
           ) %>%
           summarize(DAYS         = n_distinct(CRUISE, PERMIT, DEPLOYED_DATE),
                     ASSIGNMENTS  = n_distinct(CRUISE, PERMIT),
                     CRUISES      = n_distinct(CRUISE),
                     OBSERVERS    = n_distinct(OBSERVER_SEQ)) %>%
           ungroup(),
         assignments_dates_cr_perm %>%
           group_by(CALENDAR_YEAR, OLE_SYSTEM ) %>%
           summarize(DAYS        = n_distinct(CRUISE, PERMIT, DEPLOYED_DATE),
                     ASSIGNMENTS = n_distinct(CRUISE, PERMIT),
                     CRUISES     = n_distinct(CRUISE),
                     OBSERVERS   = n_distinct(OBSERVER_SEQ)
           ) %>%
           ungroup()
  ) %>%
  distinct()

# calculate rate by subcat for NO GROUPINGS
rate_by_subcat <-
  statements_combined %>%
  group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, OLE_SYSTEM,
           OLD_OLE_CATEGORY, STATEMENT_TYPE) %>%
  summarize(TOTAL_INCIDENTS  = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS)),
            TOTAL_STATEMENTS = n()
  ) %>%
  ungroup() %>%
  inner_join(depl_days_summ_all) %>%
  mutate(AFFIS_PER_DAY         = TOTAL_STATEMENTS/DAYS,
         INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/DAYS,
         STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*1000,
         INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*1000,
         STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*90,
         INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*90,
         STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/OBSERVERS,
         INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/OBSERVERS,
         STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/CRUISES,
         INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/CRUISES,
         STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/ASSIGNMENTS,
         INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/ASSIGNMENTS
  )


# munge it for cleaner plotting. 
# filter for OLE_PRIORITY types only, 
# and combine subcats that are the SAME for old and new systems.
rate_by_subcat_priority <-
  rate_by_subcat %>%
  ungroup() %>% 
  filter(OLD_OLE_CATEGORY %in% c('OLE PRIORITY: SAFETY AND DUTIES',
                                 'OLE PRIORITY: INTER-PERSONAL')
         | OLD_OLE_CATEGORY ==  'COAST GUARD' & STATEMENT_TYPE %in% c('Safety-USCG-Marine Casualty',
                                                                      'MARINE CASUALTY') 
  ) %>%
  mutate(OLE_SYSTEM       = factor(OLE_SYSTEM, levels = c('OLD', 'NEW')),
         OLD_OLE_CATEGORY = gsub("OLE PRIORITY: ","", OLD_OLE_CATEGORY),
         OLD_OLE_CATEGORY = paste0(OLD_OLE_CATEGORY, ' categories'),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment - Sexual',
                                    'SEXUAL HARASSMENT',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment-Assault',
                                    'ASSAULT',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-NMFS',
                                    'SAFETY',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Interference/Sample Biasing',
                                    'SAMPLING INTERFERENCE',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-USCG-Marine Casualty',
                                    'MARINE CASUALTY',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = toupper(STATEMENT_TYPE)
         
  )


# calculate rate by OLD_OLE_CATEGORY for NO GROUPINGS
rate_by_old_ole_category <-
  statements_combined %>%
  group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, OLE_SYSTEM,
           OLD_OLE_CATEGORY) %>%
  summarize(TOTAL_INCIDENTS  = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS)),
            TOTAL_STATEMENTS = n()
  ) %>%
  ungroup() %>%
  inner_join(depl_days_summ_all) %>%
  mutate(AFFIS_PER_DAY         = TOTAL_STATEMENTS/DAYS,
         INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/DAYS,
         STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*1000,
         INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*1000,
         STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*90,
         INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*90,
         STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/OBSERVERS,
         INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/OBSERVERS,
         STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/CRUISES,
         INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/CRUISES,
         STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/ASSIGNMENTS,
         INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/ASSIGNMENTS
  )





# calculate rate by NEW_OLE_CATEGORY for NO GROUPINGS
# (2023 NEW data only)
rate_by_new_ole_category <-
  statements_combined %>%
  group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, OLE_SYSTEM,
           NEW_OLE_CATEGORY) %>%
  summarize(TOTAL_INCIDENTS  = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS)),
            TOTAL_STATEMENTS = n()
  ) %>%
  ungroup() %>%
  inner_join(depl_days_summ_all) %>%
  mutate(AFFIS_PER_DAY         = TOTAL_STATEMENTS/DAYS,
         INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/DAYS,
         STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*1000,
         INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*1000,
         STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*90,
         INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*90,
         STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/OBSERVERS,
         INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/OBSERVERS,
         STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/CRUISES,
         INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/CRUISES,
         STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/ASSIGNMENTS,
         INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/ASSIGNMENTS
  )




# calculate the rate of occurrences per assnmt and per deployed day for EACH CRUISE.
# Can be used in distribution plots???
rate_by_subcat_cruise <-
  merge(assignments_dates_cr_perm %>%
          group_by(CRUISE, 
                   CALENDAR_YEAR, 
                   OLE_SYSTEM
          )  %>%
          summarize(DEPLOYED_DAYS = n_distinct(DEPLOYED_DATE),
                    ASSIGNMENTS   = n_distinct(PERMIT)
          ) %>%
          ungroup(),
        statements_combined %>%
          distinct(CALENDAR_YEAR = FIRST_VIOL_YEAR,
                   OLE_SYSTEM,
                   OLD_OLE_CATEGORY, 
                   STATEMENT_TYPE) 
        , all = TRUE ) %>%
  left_join(statements_combined %>%
              group_by(CRUISE, 
                       CALENDAR_YEAR = FIRST_VIOL_YEAR,
                       OLE_SYSTEM,
                       OLD_OLE_CATEGORY, 
                       STATEMENT_TYPE
              ) %>%
              summarize(STATEMENTS  = n_distinct(AFFIDAVIT_ID),
                        OCCURRENCES = sum(NUMBER_VIOLATIONS)
              ) %>%
              ungroup()
  ) %>%
  mutate(STATEMENTS                   = if_else(is.na(STATEMENTS),  0, STATEMENTS),  
         OCCURRENCES                  = if_else(is.na(OCCURRENCES), 0, OCCURRENCES), 
         STATEMENTS_PER_DEPLOYED_DAY  = STATEMENTS/DEPLOYED_DAYS, 
         STATEMENTS_PER_1000_DEPLOYED_DAYS  = (STATEMENTS/DEPLOYED_DAYS)*1000,
         STATEMENTS_PER_ASSIGNMENT    = STATEMENTS/ASSIGNMENTS,
         OCCURRENCES_PER_DEPLOYED_DAY = OCCURRENCES/DEPLOYED_DAYS,
         OCCURRENCES_PER_1000_DEPLOYED_DAYS  = (OCCURRENCES/DEPLOYED_DAYS)*1000,
         OCCURRENCES_PER_ASSIGNMENT   = OCCURRENCES/ASSIGNMENTS) %>%
  select(CALENDAR_YEAR,
         OLE_SYSTEM,
         OLD_OLE_CATEGORY, 
         STATEMENT_TYPE,
         CRUISE,
         DEPLOYED_DAYS,
         ASSIGNMENTS,
         STATEMENTS,
         OCCURRENCES,
         STATEMENTS,  
         OCCURRENCES, 
         STATEMENTS_PER_DEPLOYED_DAY,
         STATEMENTS_PER_ASSIGNMENT,
         OCCURRENCES_PER_DEPLOYED_DAY,
         OCCURRENCES_PER_ASSIGNMENT)


# munge it for cleaner plotting. Combine subcats that are the SAME for old and new systems.
rate_by_subcat_cruise_priority <-
  rate_by_subcat_cruise  %>%
  filter(OLD_OLE_CATEGORY %in% c('OLE PRIORITY: SAFETY AND DUTIES',
                                 'OLE PRIORITY: INTER-PERSONAL',
                                 'COAST GUARD')
  ) %>%
  mutate(OLE_SYSTEM       = factor(OLE_SYSTEM, levels = c('OLD', 'NEW')),
         OLD_OLE_CATEGORY = gsub("OLE PRIORITY: ","", OLD_OLE_CATEGORY),
         OLD_OLE_CATEGORY = paste0(OLD_OLE_CATEGORY, ' categories'),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment - Sexual',
                                    'SEXUAL HARASSMENT',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment-Assault',
                                    'ASSAULT',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-NMFS',
                                    'SAFETY',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Interference/Sample Biasing',
                                    'SAMPLING INTERFERENCE',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-USCG-Marine Casualty',
                                    'MARINE CASUALTY',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = toupper(STATEMENT_TYPE)
         
  )





occur_per_stmt <-
  statements_combined %>%
  mutate(OLE_SYSTEM       = factor(OLE_SYSTEM, levels = c('OLD', 'NEW')),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment - Sexual',
                                    'SEXUAL HARASSMENT',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment-Assault',
                                    'ASSAULT',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-NMFS',
                                    'SAFETY',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Interference/Sample Biasing',
                                    'SAMPLING INTERFERENCE',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-USCG-Marine Casualty',
                                    'MARINE CASUALTY',
                                    STATEMENT_TYPE),
         STATEMENT_TYPE   = toupper(STATEMENT_TYPE)
  ) %>%
  group_by(FIRST_VIOL_YEAR, OLE_SYSTEM, OLD_OLE_CATEGORY, STATEMENT_TYPE) %>%
  summarize(OCCUR_PER_STMT = mean(NUMBER_VIOLATIONS)
  ) # %>%
# filter(STATEMENT_TYPE %in% c('MARINE CASUALTY', 'SAFETY', 'ASSAULT','SEXUAL HARASSMENT'))  





##################
# Save Output -------------------------------------------------------------

file_3_name <- "AR_3_rate_output.Rdata"

save(list = ls(),
     file = file_3_name)



# upload the .Rdata file to g-drive
googledrive::drive_upload(
  media     = file_3_name,
  name      = file_3_name,
  path      = project_dribble,
  overwrite = T
) 



