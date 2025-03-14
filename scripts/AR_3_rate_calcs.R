
####################
# Annual Report Enforcement chapter: Rate calculations
# Contact Andy Kingham
# 206-526-4212

# Set up environment -----------------------------------------------------------

# Load pkgs
if(!require("plyr"))        install.packages("plyr",        repos='http://cran.us.r-project.org')
if(!require("reshape2"))    install.packages("reshape2",    repos='http://cran.us.r-project.org')
if(!require("tidyverse"))   install.packages("tidyverse",       repos='http://cran.us.r-project.org')
if(!require("data.table"))  install.packages("data.table",  repos='http://cran.us.r-project.org')
if(!require("sqldf"))       install.packages("sqldf",       repos='http://cran.us.r-project.org')
if(!require("devtools"))    install.packages("devtools",    repos='http://cran.us.r-project.org')
if(!require("FMAtools"))    devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")

# load the data files.----------------------------------------------------------
rm(list = ls())

# Set the .Rdata file we will load
file_2_name  <- "AR_2_rolling_join_output.Rdata"

# Assign the address of the Annual Report Project in the Shared Gdrive
AnnRpt_EnfChp_dribble <- gdrive_set_dribble("Projects/Annual Report OLE chapter 5/2024_data")

# Pull Rdata file from google drive. This will update your local if the GDrive version is newer,
# otherwise it will load your local
gdrive_download(file_2_name, AnnRpt_EnfChp_dribble)

load(file = file_2_name)




# Some high-level summary tables of the new statements data.
# long format: reg-level summary
summ_regs_units <-
 df_obs_statements %>% 
  filter(!is.na(OLE_OBS_STATEMENT_UNIT_SEQ)) %>%
  group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, CATEGORY, SUBCATEGORY, OLE_REGULATION_SEQ, REG_SUMMARY, INCIDENT_UNIT) %>%
  summarise(N_STATEMENTS = n_distinct(OLE_OBS_STATEMENT_SEQ),
            N_UNITS_REPORTED = n_distinct(OLE_OBS_STATEMENT_UNIT_SEQ),
            .groups = "drop")

# wide format: reg-level summary.  Not used
# summ_regs_units <-
#   df_obs_statements %>%
#   group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, OLE_REGULATION_SEQ, REG_SUMMARY, CATEGORY, SUBCATEGORY) %>%
#   summarize(N_DISTINCT_CRUISES_REPORTED      = n_distinct(CRUISE)
#             ,   N_DISTINCT_VESSPLANTS_REPORTED   = n_distinct(PERMIT[!is.na(PERMIT) & as.numeric(PERMIT) != 0])
#             ,   N_HAULS           = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'HAUL' & !is.na(INCIDENT_UNIT)])
#             ,   N_OFFLOADS        = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'OFFL' & !is.na(INCIDENT_UNIT)]) 
#             ,   N_TRIPS           = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'TRIP' & !is.na(INCIDENT_UNIT)])
#             ,   N_DAYS            = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'DAYS' & !is.na(INCIDENT_UNIT)])
#             ,   N_DEPL            = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'DEPL' & !is.na(INCIDENT_UNIT)])
#             ,   N_SAMPLES         = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'SAMP' & !is.na(INCIDENT_UNIT)])
#             ,   N_MARINE_MAMMALS  = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'MARM' & !is.na(INCIDENT_UNIT)])
#             ,   N_UNIT_ISSUES     = n_distinct(OLE_OBS_STATEMENT_DETAIL_SEQ[!is.na(UNIT_ISSUE)])
#             ,   .groups = "drop" 
#   )


# Units recorded, high-level
summ_reg_unit_types <-
  df_obs_statements %>%
  group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, OLE_REGULATION_SEQ, REG_SUMMARY, CATEGORY, SUBCATEGORY) %>%
  distinct(INCIDENT_UNIT) %>%
  ungroup()


summ_units_used <-
  df_obs_statements %>%
  filter(!is.na(OLE_OBS_STATEMENT_UNIT_SEQ)) %>%
  group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, INCIDENT_UNIT) %>%
  summarise(N_UNITS_REPORTED = n_distinct(OLE_OBS_STATEMENT_UNIT_SEQ),
            .groups = "drop" )



# Subcat-level summary.  Long format
summ_subcat_units <-
  df_obs_statements %>%
  filter(!is.na(OLE_OBS_STATEMENT_UNIT_SEQ)) %>% # TODO: do something about these. 
  # Possibly find them from the unit_issue and update the units data???? For now, just filtering them out.
  group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, CATEGORY, SUBCATEGORY, INCIDENT_UNIT) %>%
  summarise(N_STATEMENTS = n_distinct(OLE_OBS_STATEMENT_SEQ),
            DISTINCT_REGS_SELECTED = n_distinct(OLE_REGULATION_SEQ),
            N_REG_SELECTIONS       = n_distinct(OLE_OBS_STATEMENT_DETAIL_SEQ),
            N_UNITS_REPORTED       = n_distinct(OLE_OBS_STATEMENT_UNIT_SEQ),
            .groups = "drop" )


# Subcat-level summary # Wide format: not used.
# summ_subcat_units <-
#   df_obs_statements %>%
#   group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, CATEGORY, SUBCATEGORY) %>%
#   summarize(N_DISTINCT_CRUISES_REPORTED      = n_distinct(CRUISE)
#             ,   N_DISTINCT_VESSPLANTS_REPORTED   = n_distinct(PERMIT[!is.na(PERMIT) & as.numeric(PERMIT) != 0])
#             ,   N_DISTINCT_REGS_REPORTED         = n_distinct(OLE_REGULATION_SEQ)
#             ,   N_HAULS           = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'HAUL' & !is.na(INCIDENT_UNIT)])
#             ,   N_OFFLOADS        = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'OFFL' & !is.na(INCIDENT_UNIT)]) 
#             ,   N_TRIPS           = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'TRIP' & !is.na(INCIDENT_UNIT)])
#             ,   N_DAYS            = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'DAYS' & !is.na(INCIDENT_UNIT)])
#             ,   N_DEPL            = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'DEPL' & !is.na(INCIDENT_UNIT)])
#             ,   N_SAMPLES         = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'SAMP' & !is.na(INCIDENT_UNIT)])
#             ,   N_MARINE_MAMMALS  = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'MARM' & !is.na(INCIDENT_UNIT)])
#             ,   N_UNIT_ISSUES     = n_distinct(OLE_OBS_STATEMENT_DETAIL_SEQ[!is.na(UNIT_ISSUE)])
#             ,   .groups = "drop" 
#   )




###########
# Unit summaries
##########

# TODO: determine what factors to group by for 2024
# For now, just grouping by CALENDAR_YEAR
# ADK 20250312

# First make a wide table, that is more descriptive, with some more columns, we probably won't use those, but that's OK.
summ_units <-
  # DEPL and DAYS units
  assignments_dates_cr_perm %>%
     group_by(CALENDAR_YEAR) %>%
     summarize(DAYS         = n_distinct(CRUISE, PERMIT, DEPLOYED_DATE),
               ASSIGNMENTS  = n_distinct(CRUISE, PERMIT),
               CRUISES      = n_distinct(CRUISE),
               OBSERVERS    = n_distinct(OBSERVER_SEQ),
               .groups = "drop"
               ) %>%
  # TRIPS
  inner_join(
    df_obs_trips %>%
      mutate(CALENDAR_YEAR = ifelse(year(TRIP_START_DATE) == adp_yr-2, 
                                    year(TRIP_END_DATE),
                                    ifelse(year(TRIP_END_DATE) == adp_yr+1, 
                                                   year(TRIP_START_DATE),
                                                   year(TRIP_END_DATE))
                                            )
             ) %>%
      group_by(CALENDAR_YEAR) %>%
      summarise(TOTAL_TRIPS   = n_distinct(CRUISE, PERMIT, TRIP_SEQ),
                NONFISH_TRIPS = length(TRIP_SEQ[DID_FISHING_OCCUR_FLAG == 'N']),
                FISH_TRIPS    = length(TRIP_SEQ[DID_FISHING_OCCUR_FLAG == 'Y']),
                .groups = "drop")
          ) %>%
  
  # OFFLOADS
  inner_join(
    df_obs_offloads %>%
      group_by(CALENDAR_YEAR = year(LANDING_DATE)) %>%
      summarise(OFFLOADS = n_distinct(CRUISE, PERMIT, OFFLOAD_SEQ),
                VESSEL_OBS_OFFLOADS = length(OFFLOAD_SEQ[VESSEL_OR_PLANT == 'V']),
                PLANT_OBS_OFFLOADS  = length(OFFLOAD_SEQ[VESSEL_OR_PLANT == 'P']),
                .groups = "drop")
          ) %>%
  
  # HAULS
  inner_join(
    hauls %>%
     group_by(CALENDAR_YEAR) %>%
     summarise(HAULS = n_distinct(CRUISE, PERMIT, HAUL_SEQ),
               .groups = "drop")
      ) %>% 
  
  # SAMPLES
  inner_join(
    df_obs_samples %>%
      group_by(CALENDAR_YEAR) %>%
      summarise(TOTAL_SAMPLES     = n_distinct(CRUISE, PERMIT, SAMPLE_SEQ),
                PARENT_SAMPLES    = length(SAMPLE_SEQ[SUBSAMPLE_FLAG == 'N']),
                SUBSAMPLES        = length(SAMPLE_SEQ[SUBSAMPLE_FLAG   == 'Y']),
                DECKSORT_SAMPLES  = length(SAMPLE_SEQ[DECKSORT_FLAG    == 'Y']),
                PRESORTED_SAMPLES = length(SAMPLE_SEQ[PRESORTED_FLAG   == 'Y']),
                .groups = "drop")
      ) %>%

  # MARINE MAMMALS
  inner_join(
    df_obs_marm %>%
      group_by(CALENDAR_YEAR = year(INTERACTION_DATE)) %>%
      summarise(MAMMAL_INTERACTIONS = n_distinct(CRUISE, PERMIT, MAMMAL_SEQ))
           )
                

# Next MELT this table into LONG format, and use that actual unit names
units_melt <-
  melt(data = summ_units %>%
                mutate(DEPL = CRUISES,
                       TRIP = TOTAL_TRIPS,
                       OFFL = OFFLOADS,
                       HAUL = HAULS,
                       SAMP = TOTAL_SAMPLES,
                       MARM = MAMMAL_INTERACTIONS), 
       id.vars       = c("CALENDAR_YEAR"),
       measure.vars  = c("DAYS", "DEPL", "TRIP","OFFL", "HAUL", "SAMP", "MARM"
                         ),
       value.name    = 'TOTAL_UNITS',
       variable.name = "INCIDENT_UNIT") 

#########
# Rates for Units
#########

# REG-level rate
reg_units_rate <-
  summ_regs_units %>%
  left_join(units_melt) %>%
  full_join(summ_units) %>%
  mutate(RATE = N_UNITS_REPORTED/TOTAL_UNITS,
         RATE_PER_1000_UNITS = RATE*1000,
         UNITS_PER_OBSERVER   =  N_UNITS_REPORTED/OBSERVERS,
         UNITS_PER_CRUISE     =  N_UNITS_REPORTED/CRUISES,
         UNITS_PER_ASSIGNMENT =  N_UNITS_REPORTED/ASSIGNMENTS,
         UNITS_PER_DAY        =  N_UNITS_REPORTED/DAYS,
         UNITS_PER_1000_DAYS  = (N_UNITS_REPORTED/DAYS)*1000
          ) %>%
  select(CALENDAR_YEAR, CATEGORY, SUBCATEGORY,
         OLE_REGULATION_SEQ, REG_SUMMARY,
         INCIDENT_UNIT, N_UNITS_REPORTED, TOTAL_UNITS,
         RATE, RATE_PER_1000_UNITS,
         OBSERVERS, UNITS_PER_OBSERVER,
         CRUISES, UNITS_PER_CRUISE,
         ASSIGNMENTS, UNITS_PER_ASSIGNMENT,
         DAYS, UNITS_PER_DAY, UNITS_PER_1000_DAYS
         )
  
  
# SUBCATEGORY-level rate
subcat_units_rate <-
  summ_subcat_units %>%
  left_join(units_melt) %>%
  full_join(summ_units) %>%
  mutate(RATE = N_UNITS_REPORTED/TOTAL_UNITS,
         RATE_X_1000 = RATE*1000,
         UNITS_PER_OBSERVER   =  N_UNITS_REPORTED/OBSERVERS,
         UNITS_PER_CRUISE     =  N_UNITS_REPORTED/CRUISES,
         UNITS_PER_ASSIGNMENT =  N_UNITS_REPORTED/ASSIGNMENTS,
         UNITS_PER_DAY        =  N_UNITS_REPORTED/DAYS,
         UNITS_PER_1000_DAYS  = (N_UNITS_REPORTED/DAYS)*1000
  ) %>%
  select(CALENDAR_YEAR, CATEGORY, SUBCATEGORY,
         INCIDENT_UNIT, DISTINCT_REGS_SELECTED, N_REG_SELECTIONS, 
         N_UNITS_REPORTED, TOTAL_UNITS,
         RATE, RATE_X_1000,
         OBSERVERS, UNITS_PER_OBSERVER,
         CRUISES, UNITS_PER_CRUISE,
         ASSIGNMENTS, UNITS_PER_ASSIGNMENT,
         DAYS, UNITS_PER_DAY, UNITS_PER_1000_DAYS
  )


# Above table, but FILTERED For OLE_PRIORITY types
subcat_units_rate_priority <-
  subcat_units_rate %>%
  filter(SUBCATEGORY %in% c('REASONABLE ASSISTANCE', 'SAMPLING INTERFERENCE',
                            'DESTRUCTION OF SAMPLE/WORK/PERSONAL EFFECTS',
                            'SEXUAL ASSAULT',
                            'SAFETY',
                            'ASSAULT',
                            'SEXUAL HARASSMENT',
                            'INTIMIDATION/BRIBERY/COERCION',
                            'IMPEDIMENT',
                            'HOSTILE WORK ENVIRONMENT'))







########################
#########################
# Summarize the DAYS for each factor combination.

# Count deployed days.
# Frst count the SUM TOTAL of days for cruise/permit/all factors.
# This double-counts days where multiple factors occur, and is used to WEIGHT the statements in later step: WIEGHTING METHOD 1.
# Next count DISTINCT days for each cruise/permit ONLY.  Used for WIEGHTING METHOD 2.  Update: this method is not used.
# Finally count DISTINCT days for each cruise/permit/factor combination
# Weighting methods 3 and 4 are based on fishery-data-collection days as the denominator, rather than all deployment days.
# Weighting methods 3 and 4 are not currently used.

# NOTE: this may not be used for 2024 depending on what factors are used.

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
          group_by(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION) %>%
          summarize(FACTOR_DAYS = n_distinct(DEPLOYED_DATE),
                    FACTOR_FSHRY_DATA_DAYS = length(unique(DEPLOYED_DATE[FISHERY_DATA_BOOL == 'Y'])) ) %>%
          ungroup(),
        all = TRUE) %>%
  mutate(FACTOR_WEIGHT_MTHD_1 = FACTOR_DAYS/TOTAL_FACTOR_DAYS_CR_PERM,       # Method 1 for WEIGHTING the statements will apportion the number of instances to the number of days in that factor category.
         FACTOR_WEIGHT_MTHD_2 = FACTOR_DAYS/TOTAL_DISTINCT_DAYS_CR_PERM,     # Method 2 for WEIGHTING the statements will apportion the number of instances to the number of distinct days in that factor category.
         FACTOR_WEIGHT_MTHD_3 = FACTOR_FSHRY_DATA_DAYS/TOTAL_FACTOR_FSHRY_DATA_DAYS_CR_PERM,       # Method 3 for WEIGHTING the statements will apportion the number of instances to the number of FISHERY_DATA days in that factor category.
         FACTOR_WEIGHT_MTHD_4 = FACTOR_FSHRY_DATA_DAYS/TOTAL_DISTINCT_FSHRY_DATA_DAYS_CR_PERM) %>% # Method 4 for WEIGHTING the statements will apportion the number of instances to the number of DISTINCT FISHERY DATA DAYS in that factor category.
  select(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION, FACTOR_DAYS, TOTAL_FACTOR_DAYS_CR_PERM, TOTAL_DISTINCT_DAYS_CR_PERM, FACTOR_WEIGHT_MTHD_1, FACTOR_WEIGHT_MTHD_2, FACTOR_WEIGHT_MTHD_3, FACTOR_WEIGHT_MTHD_4)





# Next, summarize for each factor combination and get the total days etc.  These are the DENOMINATORS of the rates.
depl_days_summ_by_factor <-
  cnt_dep_days_all_groupings %>% 
  group_by(CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION) %>%
  summarize(TOTAL_DAYS      = sum(FACTOR_DAYS), 
            TOTAL_OBSERVERS = n_distinct(OBSERVER_SEQ),
            TOTAL_CRUISES   = n_distinct(CRUISE),
            DISTINCT_OBSERVER_ASSIGNMENTS = n_distinct(CRUISE, PERMIT),
            .groups = "drop")








# ###############
# # OLD CODE, COMMENTING OUT FOR 2024
# 
# ######################
# # Finally!!!!
# # Calculate the rates.  
# # There are FOUR rates that are calculated.
# #     # The FIRST is for each individual statement subcategory (SUBCAT) and each unique factor grouping
# #     # The SECOND, is for the higher-level, OLD_OLE_CATEGORY and each unique factor grouping.
# #     # The THIRD is with NO factors (all aggregated), by SUBCAT.
# #     # The FOURTH is with NO factors (all aggregated), by OLD_OLE_CATEGORY.
# 
# # First rate: by subcat, for each factor combination.
# rate_all_groupings_subcat <-
#   depl_days_summ_by_factor %>%  #denominator, see above
#   # Next, join raw_statements to the dep_days DF on cruise/permit, and sum to get the WEIGHTED number_of_incidents and number_of_statements. This is the NUMERATOR of the rates.
#   left_join(cnt_dep_days_all_groupings %>% # LEFT join ensures all days where NO statements were written are counted!! Critical to accurate rate calc.
#               inner_join(df_statements_raw %>%
#                            mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR,
#                                   PERMIT = as.numeric(PERMIT))) %>%  
#               group_by (CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION, SUBCATEGORY) %>%
#               summarize(TOTAL_STATEMENTS = sum(if_else(is.na(OLE_OBS_STATEMENT_SEQ), 0, FACTOR_WEIGHT_MTHD_1)),
#                         TOTAL_INCIDENTS  = sum(if_else(is.na(OLE_OBS_STATEMENT_SEQ), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1)) ) %>%
#               ungroup() )  %>%
#   #Last, calculate the rates.
#   mutate(CONFI_FLAG = ifelse(DISTINCT_OBSERVER_ASSIGNMENTS < 3, 1, 0), # for confidentiality, flag any factor combinations with < 3 cruise/permit assignments, these will be removed later.
#          AFFIS_PER_DAY         = TOTAL_STATEMENTS/TOTAL_DAYS,
#          INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/TOTAL_DAYS,
#          STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/TOTAL_DAYS)*1000,
#          INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/TOTAL_DAYS)*1000,
#          STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/TOTAL_DAYS)*90,
#          INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/TOTAL_DAYS)*90,
#          STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/TOTAL_OBSERVERS,
#          INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/TOTAL_OBSERVERS,
#          STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/TOTAL_CRUISES,
#          INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/TOTAL_CRUISES,
#          STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/DISTINCT_OBSERVER_ASSIGNMENTS,
#          INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/DISTINCT_OBSERVER_ASSIGNMENTS
#   )
# 
# 
# # make a separate df for plots, all this does is it re-orders the factors and 
# # put spaces in for NA, so it looks better in tables and figures.
# rate_all_groupings_subcat_for_plots <-
#   rate_all_groupings_subcat %>% 
#   mutate(VESSEL_TYPE = factor(VESSEL_TYPE, levels = c('PLANT', 'CP/MS', 'CV')),
#          GEAR_TYPE   = factor(if_else(is.na(GEAR_TYPE), ' ', GEAR_TYPE),   levels = c('NPT', 'PTR', 'HAL', 'POT', ' ')),
#          NMFS_REGION = factor(if_else(is.na(NMFS_REGION), ' ', NMFS_REGION),   levels = c('BSAI', 'GOA', ' '))
#   )
# 
# # output this for OLE to examine.  It includes confidential data so not for public use.
# #UPDATE: filtering out CONFI_FLAG rows
# write.csv(file = paste0("tables/tbl_", 
#                         adp_yr, 
#                         "rate_all_groupings_subcat_for_plots"
# ),
# x    = rate_all_groupings_subcat_for_plots %>%
#   filter (CONFI_FLAG == 0 | is.na(CONFI_FLAG))
# )
# 
# 
# 
# 
# 
# ######################
# # Second rate: by OLD_OLE_CATEGORY, for each factor combination.
# 
# rate_all_groupings_ole_category <- 
#   depl_days_summ_by_factor %>%
#   # Next, join raw_statements to the dep_days DF on cruise/permit, and sum to get the WEIGHTED number_of_incidents and number_of_statements. This is the NUMERATOR of the rates.
#   left_join(cnt_dep_days_all_groupings %>% # LEFT join ensures all days where NO statements were written are counted!! Critical to accurate rate calc.
#               inner_join(statements_combined %>% 
#                            mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR,
#                                   PERMIT = as.numeric(PERMIT)) # need to make column names match
#               ) %>%  
#               group_by (CALENDAR_YEAR, OLE_SYSTEM, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, OLD_OLE_CATEGORY) %>%
#               summarize(TOTAL_STATEMENTS = sum(if_else(is.na(AFFIDAVIT_ID), 0, FACTOR_WEIGHT_MTHD_1)),
#                         TOTAL_INCIDENTS  = sum(if_else(is.na(AFFIDAVIT_ID), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1)) ) %>%
#               ungroup() ) %>%
#   # last, calculate the rates.
#   mutate(CONFI_FLAG = ifelse(DISTINCT_OBSERVER_ASSIGNMENTS < 3, 1, 0), # for confidentiality, flag any factor combinations with < 3 cruise/permit assignments, these will be removed later.
#          AFFIS_PER_DAY         = TOTAL_STATEMENTS/TOTAL_DAYS,
#          INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/TOTAL_DAYS,
#          STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/TOTAL_DAYS)*1000,
#          INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/TOTAL_DAYS)*1000,
#          STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/TOTAL_DAYS)*90,
#          INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/TOTAL_DAYS)*90,
#          STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/TOTAL_OBSERVERS,
#          INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/TOTAL_OBSERVERS,
#          STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/TOTAL_CRUISES,
#          INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/TOTAL_CRUISES,
#          STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/DISTINCT_OBSERVER_ASSIGNMENTS,
#          INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/DISTINCT_OBSERVER_ASSIGNMENTS
#   )
# 
# 
# 
# 
# 
# ######################
# # Third rate: by NEW_OLE_CATEGORY, for each factor combination.
# 
# rate_all_groupings_new_ole_category <- 
#   depl_days_summ_by_factor %>%
#   # Next, join raw_statements to the dep_days DF on cruise/permit, and sum to get the WEIGHTED number_of_incidents and number_of_statements. This is the NUMERATOR of the rates.
#   left_join(cnt_dep_days_all_groupings %>% # LEFT join ensures all days where NO statements were written are counted!! Critical to accurate rate calc.
#               inner_join(statements_combined %>% 
#                            mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR,
#                                   PERMIT = as.numeric(PERMIT)) # need to make column names match
#               ) %>%  
#               group_by (CALENDAR_YEAR, OLE_SYSTEM, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, NEW_OLE_CATEGORY) %>%
#               summarize(TOTAL_STATEMENTS = sum(if_else(is.na(AFFIDAVIT_ID), 0, FACTOR_WEIGHT_MTHD_1)),
#                         TOTAL_INCIDENTS  = sum(if_else(is.na(AFFIDAVIT_ID), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1)) ) %>%
#               ungroup() ) %>%
#   # last, calculate the rates.
#   mutate(CONFI_FLAG = ifelse(DISTINCT_OBSERVER_ASSIGNMENTS < 3, 1, 0), # for confidentiality, flag any factor combinations with < 3 cruise/permit assignments, these will be removed later.
#          AFFIS_PER_DAY         = TOTAL_STATEMENTS/TOTAL_DAYS,
#          INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/TOTAL_DAYS,
#          STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/TOTAL_DAYS)*1000,
#          INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/TOTAL_DAYS)*1000,
#          STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/TOTAL_DAYS)*90,
#          INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/TOTAL_DAYS)*90,
#          STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/TOTAL_OBSERVERS,
#          INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/TOTAL_OBSERVERS,
#          STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/TOTAL_CRUISES,
#          INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/TOTAL_CRUISES,
#          STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/DISTINCT_OBSERVER_ASSIGNMENTS,
#          INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/DISTINCT_OBSERVER_ASSIGNMENTS
#   )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Summarize ALL days and assignments
# # Include an extra row that SINGLE COUNTS for 2023
# # This is really just for display and clarity, becuase any cruise that overlapped 
# # the cutoff date of 07/28/2023, those deployment days, assignments, etc will be counted for 
# # BOTH the OLD and NEW SYSTEM, in this kind of summary.
# # Note that all inner_joins for rates will IGNORE this row so it is not needed for the other rates.
# depl_days_summ_all <-
#   rbind( assignments_dates_cr_perm %>%
#            group_by(CALENDAR_YEAR, 
#                     OLE_SYSTEM    = if_else(CALENDAR_YEAR == adp_yr, 'COMBINED', 'OLD')
#            ) %>%
#            summarize(DAYS         = n_distinct(CRUISE, PERMIT, DEPLOYED_DATE),
#                      ASSIGNMENTS  = n_distinct(CRUISE, PERMIT),
#                      CRUISES      = n_distinct(CRUISE),
#                      OBSERVERS    = n_distinct(OBSERVER_SEQ)) %>%
#            ungroup(),
#          assignments_dates_cr_perm %>%
#            group_by(CALENDAR_YEAR, OLE_SYSTEM ) %>%
#            summarize(DAYS        = n_distinct(CRUISE, PERMIT, DEPLOYED_DATE),
#                      ASSIGNMENTS = n_distinct(CRUISE, PERMIT),
#                      CRUISES     = n_distinct(CRUISE),
#                      OBSERVERS   = n_distinct(OBSERVER_SEQ)
#            ) %>%
#            ungroup()
#   ) %>%
#   distinct()
# 
# # calculate rate by subcat for NO GROUPINGS
# rate_by_subcat <-
#   statements_combined %>%
#   group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, OLE_SYSTEM,
#            OLD_OLE_CATEGORY, STATEMENT_TYPE) %>%
#   summarize(TOTAL_INCIDENTS  = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS)),
#             TOTAL_STATEMENTS = n()
#   ) %>%
#   ungroup() %>%
#   inner_join(depl_days_summ_all) %>%
#   mutate(AFFIS_PER_DAY         = TOTAL_STATEMENTS/DAYS,
#          INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/DAYS,
#          STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*1000,
#          INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*1000,
#          STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*90,
#          INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*90,
#          STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/OBSERVERS,
#          INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/OBSERVERS,
#          STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/CRUISES,
#          INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/CRUISES,
#          STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/ASSIGNMENTS,
#          INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/ASSIGNMENTS
#   )
# 
# 
# # munge it for cleaner plotting. 
# # filter for OLE_PRIORITY types only, 
# # and combine subcats that are the SAME for old and new systems.
# rate_by_subcat_priority <-
#   rate_by_subcat %>%
#   ungroup() %>% 
#   filter(OLD_OLE_CATEGORY %in% c('OLE PRIORITY: SAFETY AND DUTIES',
#                                  'OLE PRIORITY: INTER-PERSONAL')
#          | OLD_OLE_CATEGORY ==  'COAST GUARD' & STATEMENT_TYPE %in% c('Safety-USCG-Marine Casualty',
#                                                                       'MARINE CASUALTY') 
#   ) %>%
#   mutate(OLE_SYSTEM       = factor(OLE_SYSTEM, levels = c('OLD', 'NEW')),
#          OLD_OLE_CATEGORY = gsub("OLE PRIORITY: ","", OLD_OLE_CATEGORY),
#          OLD_OLE_CATEGORY = paste0(OLD_OLE_CATEGORY, ' categories'),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment - Sexual',
#                                     'SEXUAL HARASSMENT',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment-Assault',
#                                     'ASSAULT',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-NMFS',
#                                     'SAFETY',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Interference/Sample Biasing',
#                                     'SAMPLING INTERFERENCE',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-USCG-Marine Casualty',
#                                     'MARINE CASUALTY',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = toupper(STATEMENT_TYPE)
#          
#   )
# 
# 
# # calculate rate by OLD_OLE_CATEGORY for NO GROUPINGS
# rate_by_old_ole_category <-
#   statements_combined %>%
#   group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, OLE_SYSTEM,
#            OLD_OLE_CATEGORY) %>%
#   summarize(TOTAL_INCIDENTS  = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS)),
#             TOTAL_STATEMENTS = n()
#   ) %>%
#   ungroup() %>%
#   inner_join(depl_days_summ_all) %>%
#   mutate(AFFIS_PER_DAY         = TOTAL_STATEMENTS/DAYS,
#          INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/DAYS,
#          STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*1000,
#          INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*1000,
#          STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*90,
#          INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*90,
#          STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/OBSERVERS,
#          INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/OBSERVERS,
#          STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/CRUISES,
#          INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/CRUISES,
#          STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/ASSIGNMENTS,
#          INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/ASSIGNMENTS
#   )
# 
# 
# 
# 
# 
# # calculate rate by NEW_OLE_CATEGORY for NO GROUPINGS
# # (2023 NEW data only)
# rate_by_new_ole_category <-
#   statements_combined %>%
#   group_by(CALENDAR_YEAR = FIRST_VIOL_YEAR, OLE_SYSTEM,
#            NEW_OLE_CATEGORY) %>%
#   summarize(TOTAL_INCIDENTS  = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS)),
#             TOTAL_STATEMENTS = n()
#   ) %>%
#   ungroup() %>%
#   inner_join(depl_days_summ_all) %>%
#   mutate(AFFIS_PER_DAY         = TOTAL_STATEMENTS/DAYS,
#          INCIDENTS_PER_DAY     = TOTAL_INCIDENTS/DAYS,
#          STATEMENTS_PER_1000_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*1000,
#          INCIDENTS_PER_1000_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*1000,
#          STATEMENTS_PER_90_DEPLOYED_DAYS  = (TOTAL_STATEMENTS/DAYS)*90,
#          INCIDENTS_PER_90_DEPLOYED_DAYS   = (TOTAL_INCIDENTS/DAYS)*90,
#          STATEMENTS_PER_OBSERVER     = TOTAL_STATEMENTS/OBSERVERS,
#          INCIDENTS_PER_OBSERVER      = TOTAL_INCIDENTS/OBSERVERS,
#          STATEMENTS_PER_CRUISE       = TOTAL_STATEMENTS/CRUISES,
#          INCIDENTS_PER_CRUISE        = TOTAL_INCIDENTS/CRUISES,
#          STATEMENTS_PER_ASSIGNMENT   = TOTAL_STATEMENTS/ASSIGNMENTS,
#          INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/ASSIGNMENTS
#   )
# 
# 
# 
# 
# # calculate the rate of occurrences per assnmt and per deployed day for EACH CRUISE.
# # Can be used in distribution plots???
# rate_by_subcat_cruise <-
#   merge(assignments_dates_cr_perm %>%
#           group_by(CRUISE, 
#                    CALENDAR_YEAR, 
#                    OLE_SYSTEM
#           )  %>%
#           summarize(DEPLOYED_DAYS = n_distinct(DEPLOYED_DATE),
#                     ASSIGNMENTS   = n_distinct(PERMIT)
#           ) %>%
#           ungroup(),
#         statements_combined %>%
#           distinct(CALENDAR_YEAR = FIRST_VIOL_YEAR,
#                    OLE_SYSTEM,
#                    OLD_OLE_CATEGORY, 
#                    STATEMENT_TYPE) 
#         , all = TRUE ) %>%
#   left_join(statements_combined %>%
#               group_by(CRUISE, 
#                        CALENDAR_YEAR = FIRST_VIOL_YEAR,
#                        OLE_SYSTEM,
#                        OLD_OLE_CATEGORY, 
#                        STATEMENT_TYPE
#               ) %>%
#               summarize(STATEMENTS  = n_distinct(AFFIDAVIT_ID),
#                         OCCURRENCES = sum(NUMBER_VIOLATIONS)
#               ) %>%
#               ungroup()
#   ) %>%
#   mutate(STATEMENTS                   = if_else(is.na(STATEMENTS),  0, STATEMENTS),  
#          OCCURRENCES                  = if_else(is.na(OCCURRENCES), 0, OCCURRENCES), 
#          STATEMENTS_PER_DEPLOYED_DAY  = STATEMENTS/DEPLOYED_DAYS, 
#          STATEMENTS_PER_1000_DEPLOYED_DAYS  = (STATEMENTS/DEPLOYED_DAYS)*1000,
#          STATEMENTS_PER_ASSIGNMENT    = STATEMENTS/ASSIGNMENTS,
#          OCCURRENCES_PER_DEPLOYED_DAY = OCCURRENCES/DEPLOYED_DAYS,
#          OCCURRENCES_PER_1000_DEPLOYED_DAYS  = (OCCURRENCES/DEPLOYED_DAYS)*1000,
#          OCCURRENCES_PER_ASSIGNMENT   = OCCURRENCES/ASSIGNMENTS) %>%
#   select(CALENDAR_YEAR,
#          OLE_SYSTEM,
#          OLD_OLE_CATEGORY, 
#          STATEMENT_TYPE,
#          CRUISE,
#          DEPLOYED_DAYS,
#          ASSIGNMENTS,
#          STATEMENTS,
#          OCCURRENCES,
#          STATEMENTS,  
#          OCCURRENCES, 
#          STATEMENTS_PER_DEPLOYED_DAY,
#          STATEMENTS_PER_ASSIGNMENT,
#          OCCURRENCES_PER_DEPLOYED_DAY,
#          OCCURRENCES_PER_ASSIGNMENT)
# 
# 
# # munge it for cleaner plotting. Combine subcats that are the SAME for old and new systems.
# rate_by_subcat_cruise_priority <-
#   rate_by_subcat_cruise  %>%
#   filter(OLD_OLE_CATEGORY %in% c('OLE PRIORITY: SAFETY AND DUTIES',
#                                  'OLE PRIORITY: INTER-PERSONAL',
#                                  'COAST GUARD')
#   ) %>%
#   mutate(OLE_SYSTEM       = factor(OLE_SYSTEM, levels = c('OLD', 'NEW')),
#          OLD_OLE_CATEGORY = gsub("OLE PRIORITY: ","", OLD_OLE_CATEGORY),
#          OLD_OLE_CATEGORY = paste0(OLD_OLE_CATEGORY, ' categories'),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment - Sexual',
#                                     'SEXUAL HARASSMENT',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment-Assault',
#                                     'ASSAULT',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-NMFS',
#                                     'SAFETY',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Interference/Sample Biasing',
#                                     'SAMPLING INTERFERENCE',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-USCG-Marine Casualty',
#                                     'MARINE CASUALTY',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = toupper(STATEMENT_TYPE)
#          
#   )
# 
# 
# 
# 
# 
# occur_per_stmt <-
#   statements_combined %>%
#   mutate(OLE_SYSTEM       = factor(OLE_SYSTEM, levels = c('OLD', 'NEW')),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment - Sexual',
#                                     'SEXUAL HARASSMENT',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Harassment-Assault',
#                                     'ASSAULT',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-NMFS',
#                                     'SAFETY',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Interference/Sample Biasing',
#                                     'SAMPLING INTERFERENCE',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = if_else(STATEMENT_TYPE == 'Safety-USCG-Marine Casualty',
#                                     'MARINE CASUALTY',
#                                     STATEMENT_TYPE),
#          STATEMENT_TYPE   = toupper(STATEMENT_TYPE)
#   ) %>%
#   group_by(FIRST_VIOL_YEAR, OLE_SYSTEM, OLD_OLE_CATEGORY, STATEMENT_TYPE) %>%
#   summarize(OCCUR_PER_STMT = mean(NUMBER_VIOLATIONS)
#   ) # %>%
# # filter(STATEMENT_TYPE %in% c('MARINE CASUALTY', 'SAFETY', 'ASSAULT','SEXUAL HARASSMENT'))  





##################
# Save Output -------------------------------------------------------------

file_3_name <- "AR_3_rate_output.Rdata"

save(list = ls()[!(ls() == 'channel')],
     file = file_3_name)

gdrive_upload(file_3_name, AnnRpt_EnfChp_dribble)
