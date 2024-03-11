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
# * chng wd filepath as needed *
rm(list = ls())

# adp_yr is the year of the annual report we are doing this time (annual_deployment_year)
# NOTE: we need this to ensure we load the CORRECT YEAR.  Each year has it's own directory and Rdata files.
adp_yr <- rstudioapi::showPrompt(title = "ADP YEAR", message = "Enter the ADP YEAR for this analysis:", default = "")

# Set the filepath, change to your own local as needed
# MUST BE OUTSIDE wd, because we cannot have "data" on the GitHub site.
Rdata_files_path <- paste0("C:/Users/andy.kingham/Work/Analytical Projects/Projects/Statement_redesign/Annual_Report/RData_files/", adp_yr, "/")


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
#   filter(name == "AR_2_rolling_join_output.Rdata")
# 
# # Download the file from g-drive into local
# drive_download(
#   data_dribble,
#   path = paste0(Rdata_files_path, "AR_2_rolling_join_output.Rdata"),
#   overwrite = T
# )

################
## END UNCOMMENT HERE IF YOU NEED TO GO GET THE Rdata FILE FROM G-DRIVE



load(file = paste0(Rdata_files_path, "AR_2_rolling_join_output.Rdata"))



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
rate_all_groupings_subcat_NEW_OLE_SYSTEM <-
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
              inner_join(raw_statements_new %>%
                           mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR,
                                  PERMIT = as.numeric(PERMIT))) %>%  
              group_by (CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, CATEGORY, SUBCATEGORY) %>%
              summarize(TOTAL_STATEMENTS = sum(if_else(is.na(OLE_OBS_STATEMENT_SEQ), 0, FACTOR_WEIGHT_MTHD_1)),
                        TOTAL_INCIDENTS  = sum(if_else(is.na(OLE_OBS_STATEMENT_SEQ), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1)) ) %>%
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
rate_all_groupings_subcat_for_plots_NEW_OLE_SYSTEM <-
  rate_all_groupings_subcat_NEW_OLE_SYSTEM %>% 
  mutate(VESSEL_TYPE = factor(VESSEL_TYPE, levels = c('PLANT', 'CP/MS', 'CV')),
         GEAR_TYPE   = factor(if_else(is.na(GEAR_TYPE), ' ', GEAR_TYPE),   levels = c('NPT', 'PTR', 'HAL', 'POT', ' ')),
         NMFS_REGION = factor(if_else(is.na(NMFS_REGION), ' ', NMFS_REGION),   levels = c('BSAI', 'GOA', ' '))
  )

# output this for OLE to examine.  It includes confidential data so not for public use.
write.csv(file = paste0(adp_yr, "_outputs/NEW_OLE_SYSTEM_data_outputs/charts_and_tables/tables/tbl_", 
                        adp_yr, 
                        "_rate_all_groupings_subcat_for_plots_NEW_OLE_SYSTEM.csv"
),
x    = rate_all_groupings_subcat_for_plots_NEW_OLE_SYSTEM)









############
# Make summary table of just the DEPLOYED DAYS for each factor grouping.
cnt_dep_days_by_factor_group_NEW_OLE_SYSTEM <-
  rate_all_groupings_subcat_NEW_OLE_SYSTEM %>%
  distinct(CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, TOTAL_DAYS, TOTAL_CRUISES, TOTAL_OBSERVERS, DISTINCT_OBSERVER_ASSIGNMENTS, CONFI_FLAG
  ) %>%  
  mutate(FACTOR_GROUP = paste(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, sep = ''))





####################
# Make a DF that simply joins statements and cnt_dep_days.  Not really used in analysis, but useful for referencing.  
days_statements_all_groupings_raw_NEW_OLE_SYSTEM  <-
  cnt_dep_days_all_groupings %>%
  left_join(raw_statements_new %>% 
              mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR,
                     PERMIT = as.numeric(PERMIT))) %>%
  filter(paste0(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, 
                MANAGEMENT_PROGRAM_CODE, NMFS_REGION
                ) %in% cnt_dep_days_by_factor_group_NEW_OLE_SYSTEM$FACTOR_GROUP
        ) %>%
  mutate(VESSEL_TYPE = factor(VESSEL_TYPE, levels = c('PLANT', 'CP/MS', 'CV')),
         GEAR_TYPE   = factor(if_else(is.na(GEAR_TYPE), ' ', GEAR_TYPE),   levels = c('NPT', 'PTR', 'HAL', 'POT', ' ')),
         NMFS_REGION = factor(if_else(is.na(NMFS_REGION), ' ', NMFS_REGION),   levels = c('BSAI', 'GOA', ' '))
  )















######################
# Second rate: by CATEOGRY, for each factor combination.

rate_all_groupings_category_NEW_OLE_SYSTEM <- 
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
                raw_statements_new %>% 
                  mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR,
                         PERMIT = as.numeric(PERMIT)) # need to make column names match
                        ) %>%  
              group_by (CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, CATEGORY) %>%
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
rate_no_groupings_NEW_OLE_SYSTEM <-
  raw_statements_new %>%
  group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, CATEGORY, SUBCATEGORY) %>%
  summarize(N_OCCURRENCES = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS))) %>%
  ungroup() %>%
  full_join(depl_days_all_summ) %>%
  mutate(RATE_PER_1000_DAYS = (N_OCCURRENCES/DAYS)*1000,
         RATE_PER_ASSNMT    = (N_OCCURRENCES/ASSIGNMENTS))



##################
# Save Output -------------------------------------------------------------
save(list = ls(),
     file = paste0(Rdata_files_path, "AR_3.1_rate_output_NEW_OLE_SYSTEM.rdata"))



# upload the .Rdata file to g-drive
googledrive::drive_upload(
  media     = paste0(Rdata_files_path, "AR_3.1_rate_output_NEW_OLE_SYSTEM.rdata"),
  name      = "AR_3.1_rate_output_NEW_OLE_SYSTEM.rdata",
  path      = project_dribble
) 




