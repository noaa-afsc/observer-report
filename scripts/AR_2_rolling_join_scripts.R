####################
# Annual Report Enforcement chapter: rolling Joins and Rate Calculations
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

# adp_yr is the year of the annual report we are doing this time (annual_deployment_year)
# NOTE: we need this to ensure we load the CORRECT YEAR.  Each year has it's own directory and Rdata files.
adp_yr <- rstudioapi::showPrompt(title = "ADP YEAR", message = "Enter the ADP YEAR for this analysis:", default = "")


load(file = paste0(adp_yr, "_outputs/Rdata_workspaces/", "AR_1_Statements_data.rdata"))


#First, get all the factors by joining the ASSIGNMENTS data to the haul data.
# This gets each factor for every date that has a value in haul data for that date.
# Joining by permit and date ensures that all factors are joined even for 2nd observers, for each vessel and date.


#################
# COMBINE factors, so each COMBINATION is unique.

#First, get all the factors by joining the ASSIGNMENTS data to the haul data.
# This gets each factor for every date that has a value in haul data for that date.
# Joining by permit and date ensures that all factors are joined even for 2nd observers, for each vessel and date.

assnmts_days_all_groupings <-   
  assignments_dates_cr_perm %>%
  filter(CALENDAR_YEAR <= as.numeric(adp_yr) & CALENDAR_YEAR >= as.numeric(adp_yr) -1 ) %>%
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, VESSEL_OR_PLANT, COVERAGE_TYPE, SPECIAL_DEPLOYMENT_CODE) %>%  # remove duplicates; rows are unique by cruise, permit, date, and factor combination.
  left_join(sqldf('SELECT DISTINCT PERMIT, HAUL_DATE as DEPLOYED_DATE,
                          VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION
                     FROM hauls
                    UNION ALL
                   SELECT DISTINCT PROCESSOR_PERMIT_ID as PERMIT,
                          LANDING_DATE as DEPLOYED_DATE,
                          null AS VESSEL_TYPE, null as GEAR_TYPE, 
                          MANAGEMENT_PROGRAM_CODE, NMFS_REGION
                    FROM df_em_efp_offloads' ) %>%
              mutate(DEPLOYED_DATE = as.POSIXct(DEPLOYED_DATE, origin = "1970-01-01", tz = 'GMT')
              )) %>%  
  # step 2: hardcode missing values.
  # 1. get plants
  # 2. get cases where there is no hauls in the table, but we have deployment dates for those vessels. Hardcode the factor values for those cruise/permit.
  # This may be due to:
  # a.) the vessel not leaving the dock for whatever reason, but the obs was deployed.  
  # b.) the data were deleted.  
  # c.) the observer was ill and collected no data.  
  # NOTE: these permits are determined in later steps, AFTER the rolling joins have been completed, but it is better to enter them now.
  # The process is, after determining which are still NULL after the rolling joins, go investigate those from the raw data, and hardcode them     
  mutate(VESSEL_TYPE = ifelse(VESSEL_OR_PLANT == 'P', 'PLANT', as.character(VESSEL_TYPE)),  #for plant days, call them all PLANT.
         GEAR_TYPE       = ifelse(VESSEL_OR_PLANT == 'P' & is.na(GEAR_TYPE),  ' ', as.character(GEAR_TYPE)), # set plants = ' ', there is no GT for them, and using a space makes the plots nicer..
         
         ####### commented out extra stuff that is no longer needed!!!
         
         # NMFS_REGION     = ifelse(VESSEL_OR_PLANT == 'P' & lubridate::year(DEPLOYED_DATE) <  2020 & is.na(NMFS_REGION),  'BSAI', as.character(NMFS_REGION)), # set plants = BSAI for all years before the EM EFP, since they are only in BSAI until this point.
         # NMFS_REGION     = ifelse(is.na(NMFS_REGION) & PERMIT %in% c(34707) & lubridate::year(DEPLOYED_DATE) < 2020, 'BSAI', as.character(NMFS_REGION)), # The darn GOLDEN HARVEST ALASKA - ADAK (34707) was a PLANT, and participated in an EFP in 2019. It is in the BSAI. 
         # NMFS_REGION     = ifelse(VESSEL_OR_PLANT == 'P' & lubridate::year(DEPLOYED_DATE) >= 2020 & is.na(NMFS_REGION) &   PERMIT %in% c(4078, 5320, 5358, 5306, 5310, 5323, 34707),  'BSAI', as.character(NMFS_REGION)), # set this HARD-CODED set of plants = BSAI for all years SINCE the EM EFP. NOTE: also includes ADAK (34707), since it is BSAI.
         # NMFS_REGION     = ifelse(VESSEL_OR_PLANT == 'P' & lubridate::year(DEPLOYED_DATE) >= 2020 & is.na(NMFS_REGION) & !(PERMIT %in% c(4078, 5320, 5358, 5306, 5310, 5323, 34707)),  'GOA', as.character(NMFS_REGION)), # set plants = GOA for all plants NOT in this HARD-CODED set of plants for all years SINCE the EM EFP.  
         # MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(34707) & lubridate::year(DEPLOYED_DATE) < 2020, 'EFP', as.character(MANAGEMENT_PROGRAM_CODE)), # The darn GOLDEN HARVEST ALASKA - ADAK (34707) was a PLANT, and participated in an EFP in 2019.  Since all other plant days PRIOR to 2020 will be labeled as 'AFA' (in a later step), this must be done now, to ensure these days are not included in that AFA-PLANT rate. 
         # MANAGEMENT_PROGRAM_CODE = if_else(VESSEL_OR_PLANT == 'P'  & lubridate::year(DEPLOYED_DATE) <  2020 & is.na(MANAGEMENT_PROGRAM_CODE), 'AFA', MANAGEMENT_PROGRAM_CODE),  # Plants are AFA prior to 2020 EM EFP
         # MANAGEMENT_PROGRAM_CODE = if_else(VESSEL_OR_PLANT == 'P'  & lubridate::year(DEPLOYED_DATE) >= 2020 & is.na(MANAGEMENT_PROGRAM_CODE) &   PERMIT %in% c(5320, 5358, 5306, 5305, 5310, 5323),  'AFA', MANAGEMENT_PROGRAM_CODE),  # set this HARD-CODED set of plants = AFA for all years SINCE the EM EFP Plants are AFA after 2020 unless designated as EFP in the special deployment code.
         # MANAGEMENT_PROGRAM_CODE = if_else(VESSEL_OR_PLANT == 'P'  & lubridate::year(DEPLOYED_DATE) >= 2020 & is.na(MANAGEMENT_PROGRAM_CODE) & !(PERMIT %in% c(5320, 5358, 5306, 5305, 5310, 5323)), 'OA', MANAGEMENT_PROGRAM_CODE),  # set plants = OA for all plants NOT in this HARD-CODED set of plants for all years SINCE the EM EFP.
  ) %>%
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)





######################################

# Get missing dates: Non-fishing days, etc.  
# Use data.table's ROLLING JOIN feature to get the 'nearest-neighbor' of date match within the permit. 
# This effectively fills in the dates with no value for the factor, by getting the value from the closest date within the cruise/permit.

# have to do this TWICE.  was only doing by permit at first, and it was rolling on days where the vessel fished a different vessel type!!
# SO: do it by cruise + permit FIRST.  That way, get the vessel type for the cruise/permit, if it exists. Only go outside the cruise,
# if we can't find it from the cruise.

# DO this for each factor. 

# First, VESSEL_TYPE

# first set up dt_t as dummy data.table, so I can use rolling join    
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE) 
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_v <- dt_hauls[, .(CALENDAR_YEAR, VESSEL_TYPE, CRUISE = if_else(SAMPLED_BY_CRUISE != CRUISE & SAMPLED_BY_CRUISE != 0, SAMPLED_BY_CRUISE, CRUISE), PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_v, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_hauls_v <- unique(dt_hauls_v)  # removing duplicates
dt_hauls_v[, VESSEL_TYPE := as.character(VESSEL_TYPE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character
assnmts_days_all_groupings <- dt_hauls_v[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_V := ifelse(!is.na(i.VESSEL_TYPE), i.VESSEL_TYPE, VESSEL_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE
assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(VESSEL_TYPE = FINAL_V) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE,VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)

#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$VESSEL_TYPE, exclude = FALSE)

# Now do it AGAIN, for the last few missing rows, but just on PERMIT and CALENDAR_YEAR this time.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_v <- dt_hauls[, .(CALENDAR_YEAR, VESSEL_TYPE, PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_v, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_hauls_v <- unique(dt_hauls_v)  # removing duplicates
dt_hauls_v[, VESSEL_TYPE := as.character(VESSEL_TYPE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_v[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_V := ifelse(!is.na(i.VESSEL_TYPE), i.VESSEL_TYPE, VESSEL_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(VESSEL_TYPE = FINAL_V) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE,VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR,  assnmts_days_all_groupings$VESSEL_TYPE, exclude = FALSE)



# Now do it ONE LAST TIME, for the VERY last few missing rows, but just on PERMIT without CALENDAR_YEAR this time.
# For these last few cases, there are not haul data to match up for the permit and year.  For example, 
# the observer was ill and collected NO data, and was the only observer deployed to that permit for the entire year. 
# For these, get the info from other years and cruises for the same permit.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_v <- dt_hauls[, .(VESSEL_TYPE, PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_v, PERMIT, DEPLOYED_DATE)
dt_hauls_v <- unique(dt_hauls_v)  # removing duplicates
dt_hauls_v[, VESSEL_TYPE := as.character(VESSEL_TYPE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_v[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_V := ifelse(!is.na(i.VESSEL_TYPE), i.VESSEL_TYPE, VESSEL_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(VESSEL_TYPE = FINAL_V #, 
         
         # commenting out extra stuff that is NO LONGER NEEDED!!!
         
         # VESSEL_TYPE = ifelse(PERMIT %in% c(5744, 3736, 5777, 17103) & is.na(VESSEL_TYPE), 'CV', as.character(VESSEL_TYPE)), # These permits do not have any fishery data at all in the time series.  Hardcode them as CV, from previous years data (I did some exploration).
         # VESSEL_TYPE = ifelse(PERMIT == 3533  & CALENDAR_YEAR == 2019 & is.na(VESSEL_TYPE), 'CV', as.character(VESSEL_TYPE)), # The Highliner had an observer deployed in 2019, but she was seasick and collected no data, so no hauls to match. So no vessel type associated from haul data.  They are CV.
         # VESSEL_TYPE = ifelse(PERMIT == 34732 & CALENDAR_YEAR == 2021 & is.na(VESSEL_TYPE), 'CP/MS', as.character(VESSEL_TYPE)) # The North Star is a new vessel and there is only data in HAUL after 10/1/21, so not available in this data.  It is a CP/MS.
  ) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE,VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR,  assnmts_days_all_groupings$VESSEL_TYPE, exclude = FALSE)

test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE VESSEL_TYPE is null')

#############################



















#############################
#############################

# Next, for GEAR_TYPE.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_g <- dt_hauls[, .(CALENDAR_YEAR, GEAR_TYPE, CRUISE = if_else(SAMPLED_BY_CRUISE != CRUISE & SAMPLED_BY_CRUISE != 0, SAMPLED_BY_CRUISE, CRUISE), PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_g, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_hauls_g <- unique(dt_hauls_g)  # removing duplicates
dt_hauls_g[, GEAR_TYPE := as.character(GEAR_TYPE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_g[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_G := ifelse(!is.na(i.GEAR_TYPE), i.GEAR_TYPE, GEAR_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(GEAR_TYPE = FINAL_G ) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)

#Check for NA's.  If none, then the rolling join worked correctly.

table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$GEAR_TYPE, exclude = FALSE)


# Do it again, this time with just permit.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_g <- dt_hauls[, .(CALENDAR_YEAR, GEAR_TYPE, PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_g, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_hauls_g <- unique(dt_hauls_g)  # removing duplicates
dt_hauls_g[, GEAR_TYPE := as.character(GEAR_TYPE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_g[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_G := ifelse(!is.na(i.GEAR_TYPE), i.GEAR_TYPE, GEAR_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(GEAR_TYPE = FINAL_G) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$GEAR_TYPE, exclude = FALSE)



# Now do it ONE LAST TIME, for the VERY last few missing rows, but just on PERMIT without CALENDAR_YEAR this time.
# For these last few cases, there are not haul data to match up for the permit and year..  For example, 
# the observer was ill and collected NO data, and was the only observer deployed to that permit for the entire year. 
# For these, get the info from other years and cruises for the same permit.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_g <- dt_hauls[, .(GEAR_TYPE, PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_g, PERMIT, DEPLOYED_DATE)
dt_hauls_g <- unique(dt_hauls_g)  # removing duplicates
dt_hauls_g[, GEAR_TYPE := as.character(GEAR_TYPE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_g[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_G := ifelse(!is.na(i.GEAR_TYPE), i.GEAR_TYPE, GEAR_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(GEAR_TYPE = FINAL_G #,
         
         # commenting out extra stuff that is NO LONGER NEEDED!!!
         
         # GEAR_TYPE = ifelse(PERMIT %in% c(5744, 3736, 5777, 17103) & is.na(GEAR_TYPE), 'HAL', as.character(GEAR_TYPE)), # These permits do not have any data at all in the time series.  Hardcode them as HAL, from previous years data (I did some exploration).
         # GEAR_TYPE   = ifelse(PERMIT %in% c(3533) & is.na(GEAR_TYPE) & CALENDAR_YEAR  == 2019, 'HAL', as.character(GEAR_TYPE)), # The darn Highliner DOES have deployment days in 2019, but no hauls, so no gear_type associated from haul data.  They are Longliner, per the OBS trip data.
         # GEAR_TYPE   = ifelse(PERMIT == 34732 & CALENDAR_YEAR == 2021 & is.na(GEAR_TYPE), 'NPT', as.character(GEAR_TYPE)) # The North Star is a new vessel and there is only data in HAUL after 10/1/21, so not available in this data.  It is NPT.
  ) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$GEAR_TYPE, exclude = FALSE)

test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE GEAR_TYPE is null')










#######################

# Next, MANAGEMENT_PROGRAM_CODE.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_m <- dt_hauls[, .(CALENDAR_YEAR, MANAGEMENT_PROGRAM_CODE, CRUISE = if_else(SAMPLED_BY_CRUISE != CRUISE & SAMPLED_BY_CRUISE != 0, SAMPLED_BY_CRUISE, CRUISE), PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_m, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_hauls_m <- unique(dt_hauls_m)  # removing duplicates
dt_hauls_m[, MANAGEMENT_PROGRAM_CODE := as.character(MANAGEMENT_PROGRAM_CODE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_m[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_M := ifelse(!is.na(i.MANAGEMENT_PROGRAM_CODE), i.MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_CODE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(MANAGEMENT_PROGRAM_CODE = FINAL_M
  ) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)

#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)



# Do it again with just PERMIT this time.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_m <- dt_hauls[, .(CALENDAR_YEAR, MANAGEMENT_PROGRAM_CODE, PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_m, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_hauls_m <- unique(dt_hauls_m)  # removing duplicates
dt_hauls_m[, MANAGEMENT_PROGRAM_CODE := as.character(MANAGEMENT_PROGRAM_CODE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_m[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_M := ifelse(!is.na(i.MANAGEMENT_PROGRAM_CODE), i.MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_CODE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(MANAGEMENT_PROGRAM_CODE = FINAL_M) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)




# Do it again for the remaining PLANT data (from df_em__efp_offloads)
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_em_offloads <- copy(df_em_efp_offloads)
setDT(dt_em_offloads)
dt_em_offloads_m <- dt_em_offloads[, .(CALENDAR_YEAR, MANAGEMENT_PROGRAM_CODE, PERMIT = PROCESSOR_PERMIT_ID, DEPLOYED_DATE = LANDING_DATE)]   # removing unneeded columns
setkey(dt_em_offloads_m, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_em_offloads_m <- unique(dt_em_offloads_m)  # removing duplicates
dt_em_offloads_m[, MANAGEMENT_PROGRAM_CODE := as.character(MANAGEMENT_PROGRAM_CODE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_em_offloads_m[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_M := ifelse(!is.na(i.MANAGEMENT_PROGRAM_CODE), i.MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_CODE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(MANAGEMENT_PROGRAM_CODE = FINAL_M) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


# Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)


# Do it again for the remaining PLANT data (from df_em__efp_offloads), with ONLY PERMIT (without CALENDAR_YEAR)
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, PERMIT, DEPLOYED_DATE)         
dt_em_offloads <- copy(df_em_efp_offloads)
setDT(dt_em_offloads)
dt_em_offloads_m <- dt_em_offloads[, .(MANAGEMENT_PROGRAM_CODE, PERMIT = PROCESSOR_PERMIT_ID, DEPLOYED_DATE = LANDING_DATE)]   # removing unneeded columns
setkey(dt_em_offloads_m, PERMIT, DEPLOYED_DATE)
dt_em_offloads_m <- unique(dt_em_offloads_m)  # removing duplicates
dt_em_offloads_m[, MANAGEMENT_PROGRAM_CODE := as.character(MANAGEMENT_PROGRAM_CODE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_em_offloads_m[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_M := ifelse(!is.na(i.MANAGEMENT_PROGRAM_CODE), i.MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_CODE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(MANAGEMENT_PROGRAM_CODE = FINAL_M) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


# Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)





# # Now do it ONE LAST TIME, for the VERY last few missing rows, but just on PERMIT without CALENDAR_YEAR this time.
# For these last few cases, there are not haul data to match up for the permit and year..  For example, 
# the observer was ILL and collected NO data, and was the only observer deployed to that permit for the entire year. 
# For these, get the info from other years and cruises for the same permit.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_m <- dt_hauls[, .( MANAGEMENT_PROGRAM_CODE, PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_m, PERMIT, DEPLOYED_DATE)
dt_hauls_m <- unique(dt_hauls_m)  # removing duplicates
dt_hauls_m[, MANAGEMENT_PROGRAM_CODE := as.character(MANAGEMENT_PROGRAM_CODE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_m[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_M := ifelse(!is.na(i.MANAGEMENT_PROGRAM_CODE), i.MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_CODE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(MANAGEMENT_PROGRAM_CODE = FINAL_M #,
         
         # commenting out extra stuff that is NO LONGER NEEDED!!!
         
         # MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(5744, 3736, 5777), 'IFQ', as.character(MANAGEMENT_PROGRAM_CODE)), # These permits do not have any data at all in the time series.  Hardcode them as IFQ, from previous years data (I did some exploration).
         # MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(3533) & CALENDAR_YEAR == 2019, 'OA', as.character(MANAGEMENT_PROGRAM_CODE)), # The darn Highliner (3533) DO have deployment days in 2019, but no hauls could be matched up, so no MGM code can be associated from haul data.  They are OA, per the Landings data.
         # MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(17103), 'OA', as.character(MANAGEMENT_PROGRAM_CODE)), # The darn Highliner (3533) DO have deployment days in 2019, but no hauls could be matched up, so no MGM code can be associated from haul data.  They are OA, per the Landings data.
         # MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(993, 32333)& CALENDAR_YEAR == 2019, 'OA', as.character(MANAGEMENT_PROGRAM_CODE)), # The darn Cape Kiwanda (1235), Hickory Wind (993), and Claire Oceana (32333) have 'N/A' in the obs_haul table for certain dates, and they are not being handled in the rolling join that follows for some reason, even after the previous line that takes care of 'N/A'.  TODO, debug this to eliminate hard-codeing.  For now, these days are all 'OA', per the Landings data. 
         # MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(34732) & CALENDAR_YEAR == 2021, 'A80', as.character(MANAGEMENT_PROGRAM_CODE)), # The darn North Star (33732) has no hauls in the data until 10/1/21, so could not be matched up, so no MGM code can be associated from haul data.  They are A80, per the newer data.
  ) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)

test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE MANAGEMENT_PROGRAM_CODE is null')












#######################

# Next, for NMFS_REGION.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_n <- dt_hauls[, .(CALENDAR_YEAR, NMFS_REGION, CRUISE = if_else(CRUISE != SAMPLED_BY_CRUISE & SAMPLED_BY_CRUISE !=0, SAMPLED_BY_CRUISE, CRUISE), PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_n, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_hauls_n <- unique(dt_hauls_n)  # removing duplicates
dt_hauls_n[, NMFS_REGION := as.character(NMFS_REGION)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_n[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_N := ifelse(!is.na(i.NMFS_REGION), i.NMFS_REGION, NMFS_REGION)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(NMFS_REGION = FINAL_N) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)



# Do it again with just PERMIT and CALENDAR_YEAR this time.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_n <- dt_hauls[, .(CALENDAR_YEAR, NMFS_REGION, PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_n, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_hauls_n <- unique(dt_hauls_n)  # removing duplicates
dt_hauls_n[, NMFS_REGION := as.character(NMFS_REGION)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_n[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_N := ifelse(!is.na(i.NMFS_REGION), i.NMFS_REGION, NMFS_REGION)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(NMFS_REGION = FINAL_N) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)


# Do it again for the remaining PLANT data (from df_em__efp_offloads)
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_em_offloads <- df_em_efp_offloads
setDT(dt_em_offloads)
dt_em_offloads_nr <- dt_em_offloads[, .(CALENDAR_YEAR, NMFS_REGION, PERMIT = PROCESSOR_PERMIT_ID, DEPLOYED_DATE = LANDING_DATE)]   # removing unneeded columns
setkey(dt_em_offloads_nr, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_em_offloads_nr <- unique(dt_em_offloads_nr)  # removing duplicates
dt_em_offloads_nr[, NMFS_REGION := as.character(NMFS_REGION)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_em_offloads_nr[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_NR := ifelse(!is.na(i.NMFS_REGION), i.NMFS_REGION, NMFS_REGION)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(NMFS_REGION = FINAL_NR) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


# Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)
table(assnmts_days_all_groupings$VESSEL_TYPE, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)


# Do it again for the remaining PLANT data (from df_em__efp_offloads), with ONLY PERMIT (without CALENDAR_YEAR)
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, PERMIT, DEPLOYED_DATE)         
dt_em_offloads <- df_em_efp_offloads
setDT(dt_em_offloads)
dt_em_offloads_nr <- dt_em_offloads[, .(NMFS_REGION, PERMIT = PROCESSOR_PERMIT_ID, DEPLOYED_DATE = LANDING_DATE)]   # removing unneeded columns
setkey(dt_em_offloads_nr, PERMIT, DEPLOYED_DATE)
dt_em_offloads_nr <- unique(dt_em_offloads_nr)  # removing duplicates
dt_em_offloads_nr[, NMFS_REGION := as.character(NMFS_REGION)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_em_offloads_nr[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_NR := ifelse(!is.na(i.NMFS_REGION), i.NMFS_REGION, NMFS_REGION)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(NMFS_REGION = FINAL_NR) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


# Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)
table(assnmts_days_all_groupings$VESSEL_TYPE, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)








# # Now do it ONE LAST TIME, for the VERY last few missing rows, but just on PERMIT without CALENDAR_YEAR this time.
# For these last few cases, there are not haul data to match up for the permit and year..  For example, 
# the observer was ILL and collected NO data, and was the only observer deployed to that permit for the entire year. 
# For these, get the info from other years and cruises for the same permit.
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_n <- dt_hauls[, .(NMFS_REGION, PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_n, PERMIT, DEPLOYED_DATE)
dt_hauls_n <- unique(dt_hauls_n)  # removing duplicates
dt_hauls_n[, NMFS_REGION := as.character(NMFS_REGION)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_n[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_N := ifelse(!is.na(i.NMFS_REGION), i.NMFS_REGION, NMFS_REGION)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(NMFS_REGION = FINAL_N #,
         
         # commenting out extra stuff that is NO LONGER NEEDED!!!
         
         # NMFS_REGION = ifelse(is.na(NMFS_REGION) & PERMIT %in% c(5744, 3736, 5777, 17103),  'GOA', as.character(NMFS_REGION)), # hard-code all of these as GOA.  If they are still null at this point, then they have no data for the year in question.  So use the data from other years.  After investigating, these permits are all GOA.
         # NMFS_REGION = ifelse(is.na(NMFS_REGION) & PERMIT %in% c(200, 17058, 438, 1137, 1946, 2836, 32333),  'GOA', as.character(NMFS_REGION)), # hard-code these ones as GOA.  It is still null at this point, so we went to the landing for these trips and it was GOA.
         # NMFS_REGION = ifelse(is.na(NMFS_REGION) & VESSEL_TYPE == 'PLANT' & PERMIT %in% c(5306, 5310, 5358, 5320, 4078, 5323, 35448),  'BSAI', as.character(NMFS_REGION)), # hard-code these plants as BSAI, they are all AFA BSAI plants.  If unable to find a join, we know these ones.
         # NMFS_REGION = ifelse(is.na(NMFS_REGION) & VESSEL_TYPE == 'PLANT' & PERMIT %in% c(5342, 5392, 27990, 5305, 30883, 28695, 29502, 27989, 5294, 7021, 5299, 29550, 31740, 35011, 35479, 35457),  'GOA', as.character(NMFS_REGION)), # hard-code these plants as GOA, they are all GOA Open Access plants.  If unable to find a join, we know these ones.
         # NMFS_REGION = ifelse(PERMIT == 3533 & is.na(NMFS_REGION) & CALENDAR_YEAR == 2019,   'GOA', as.character(NMFS_REGION)), # The darn Highliner DOES have deployment days in 2019, but no hauls, so no nmfs_region associated from haul data.  It is GOA (the trip left fro KODIAK); hardcode this.
         # NMFS_REGION = ifelse(PERMIT == 34732 & is.na(NMFS_REGION) & CALENDAR_YEAR == 2021, 'BSAI', as.character(NMFS_REGION)) # The darn North Star has no data in haul until 10.1.21, so no data to match.  They are BSAI per the newer data.
  ) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)
table(assnmts_days_all_groupings$VESSEL_TYPE, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)

# find the nulls and investigate them.
nvl_test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE NMFS_REGION is null')

#####################


# Check for NA's in any other column.  Because, there shouldn't be any if the rolling join worked correctly.
# If there are, go investigate these permits in landings data etc to figure out what the factor should be.  
# Then, go back to the first step and hardcode those permits.


nvl_test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings 
          WHERE NMFS_REGION is null
             OR MANAGEMENT_PROGRAM_CODE is null
             OR GEAR_TYPE is null
             OR VESSEL_TYPE is null')









#####################

# Get FISHERY DATA DAYS and stick a 'Y/N' flag onto each deployed date (Y if fishery data exist for the date; N if no fishery data for date)
assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  left_join(df_fishery_dates %>%
              distinct(CRUISE, PERMIT = as.numeric(PERMIT), DEPLOYED_DATE = HAUL_OFFLOAD_DATE, 
                       CNT_HAULS_ALL_FOR_DATE, CNT_HAULS_SAMPLED_FOR_DATE,
                       TOTAL_HAUL_WEIGHTS_FOR_DATE_MT,
                       CNT_OFFLOADS_ALL_FOR_DATE, TOTAL_OFFLOAD_WEIGHTS_FOR_DATE_MT,
                       FISHERY_DATA_BOOL)
  ) %>%
  mutate(FISHERY_DATA_BOOL = ifelse(is.na(FISHERY_DATA_BOOL), 'N', FISHERY_DATA_BOOL))

#####################




##################
# remove unneeded temporary objects.
rm(dt_em_offloads_m, dt_em_offloads_nr, dt_em_offloads, dt_hauls, dt_hauls_g, dt_hauls_m,
   dt_hauls_n, dt_hauls_v, nvl_test_df, test_df, dt_t)





##################
# Save Output -------------------------------------------------------------
#Requires the folder "(adp_yr)_outputs/Rdata_workspaces/" to be in the working directory.
save(list = ls(),
     file = paste0(adp_yr, "_outputs/Rdata_workspaces/", "AR_2_rolling_join_output.rdata"))

