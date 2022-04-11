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

setwd("~/Analytical Projects/Projects/Statement_redesign/2022_annual_report_2021_report")
load(file = "scripts/AR_Statements_data.rdata")


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
  filter(CALENDAR_YEAR < 2022) %>%
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
         NMFS_REGION     = ifelse(VESSEL_OR_PLANT == 'P' & lubridate::year(DEPLOYED_DATE) <  2020 & is.na(NMFS_REGION),  'BSAI', as.character(NMFS_REGION)), # set plants = BSAI for all years before the EM EFP, since they are only in BSAI until this point.
         NMFS_REGION     = ifelse(is.na(NMFS_REGION) & PERMIT %in% c(34707) & lubridate::year(DEPLOYED_DATE) < 2020, 'BSAI', as.character(NMFS_REGION)), # The darn GOLDEN HARVEST ALASKA - ADAK (34707) was a PLANT, and participated in an EFP in 2019. It is in the BSAI. 
         # NMFS_REGION     = ifelse(VESSEL_OR_PLANT == 'P' & lubridate::year(DEPLOYED_DATE) >= 2020 & is.na(NMFS_REGION) &   PERMIT %in% c(4078, 5320, 5358, 5306, 5310, 5323, 34707),  'BSAI', as.character(NMFS_REGION)), # set this HARD-CODED set of plants = BSAI for all years SINCE the EM EFP. NOTE: also includes ADAK (34707), since it is BSAI.
         # NMFS_REGION     = ifelse(VESSEL_OR_PLANT == 'P' & lubridate::year(DEPLOYED_DATE) >= 2020 & is.na(NMFS_REGION) & !(PERMIT %in% c(4078, 5320, 5358, 5306, 5310, 5323, 34707)),  'GOA', as.character(NMFS_REGION)), # set plants = GOA for all plants NOT in this HARD-CODED set of plants for all years SINCE the EM EFP.  
         MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(34707) & lubridate::year(DEPLOYED_DATE) < 2020, 'EFP', as.character(MANAGEMENT_PROGRAM_CODE)), # The darn GOLDEN HARVEST ALASKA - ADAK (34707) was a PLANT, and participated in an EFP in 2019.  Since all other plant days PRIOR to 2020 will be labeled as 'AFA' (in a later step), this must be done now, to ensure these days are not included in that AFA-PLANT rate. 
         MANAGEMENT_PROGRAM_CODE = if_else(VESSEL_OR_PLANT == 'P'  & lubridate::year(DEPLOYED_DATE) <  2020 & is.na(MANAGEMENT_PROGRAM_CODE), 'AFA', MANAGEMENT_PROGRAM_CODE),  # Plants are AFA prior to 2020 EM EFP
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
  mutate(VESSEL_TYPE = FINAL_V, 
         VESSEL_TYPE = ifelse(PERMIT %in% c(5744, 3736, 5777, 17103) & is.na(VESSEL_TYPE), 'CV', as.character(VESSEL_TYPE)), # These permits do not have any fishery data at all in the time series.  Hardcode them as CV, from previous years data (I did some exploration).
         VESSEL_TYPE = ifelse(PERMIT == 3533  & CALENDAR_YEAR == 2019 & is.na(VESSEL_TYPE), 'CV', as.character(VESSEL_TYPE)), # The Highliner had an observer deployed in 2019, but she was seasick and collected no data, so no hauls to match. So no vessel type associated from haul data.  They are CV.
         VESSEL_TYPE = ifelse(PERMIT == 34732 & CALENDAR_YEAR == 2021 & is.na(VESSEL_TYPE), 'CP/MS', as.character(VESSEL_TYPE)) # The North Star is a new vessel and there is only data in HAUL after 10/1/21, so not available in this data.  It is a CP/MS.
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
  mutate(GEAR_TYPE = FINAL_G,
         GEAR_TYPE = ifelse(PERMIT %in% c(5744, 3736, 5777, 17103) & is.na(GEAR_TYPE), 'HAL', as.character(GEAR_TYPE)), # These permits do not have any data at all in the time series.  Hardcode them as HAL, from previous years data (I did some exploration).
         GEAR_TYPE   = ifelse(PERMIT %in% c(3533) & is.na(GEAR_TYPE) & CALENDAR_YEAR  == 2019, 'HAL', as.character(GEAR_TYPE)), # The darn Highliner DOES have deployment days in 2019, but no hauls, so no gear_type associated from haul data.  They are Longliner, per the OBS trip data.
         GEAR_TYPE   = ifelse(PERMIT == 34732 & CALENDAR_YEAR == 2021 & is.na(GEAR_TYPE), 'NPT', as.character(GEAR_TYPE)) # The North Star is a new vessel and there is only data in HAUL after 10/1/21, so not available in this data.  It is NPT.
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
  mutate(MANAGEMENT_PROGRAM_CODE = FINAL_M,
         MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(5744, 3736, 5777), 'IFQ', as.character(MANAGEMENT_PROGRAM_CODE)), # These permits do not have any data at all in the time series.  Hardcode them as IFQ, from previous years data (I did some exploration).
         MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(3533) & CALENDAR_YEAR == 2019, 'OA', as.character(MANAGEMENT_PROGRAM_CODE)), # The darn Highliner (3533) DO have deployment days in 2019, but no hauls could be matched up, so no MGM code can be associated from haul data.  They are OA, per the Landings data.
         MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(17103), 'OA', as.character(MANAGEMENT_PROGRAM_CODE)), # The darn Highliner (3533) DO have deployment days in 2019, but no hauls could be matched up, so no MGM code can be associated from haul data.  They are OA, per the Landings data.
         MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(993, 32333)& CALENDAR_YEAR == 2019, 'OA', as.character(MANAGEMENT_PROGRAM_CODE)), # The darn Cape Kiwanda (1235), Hickory Wind (993), and Claire Oceana (32333) have 'N/A' in the obs_haul table for certain dates, and they are not being handled in the rolling join that follows for some reason, even after the previous line that takes care of 'N/A'.  TODO, debug this to eliminate hard-codeing.  For now, these days are all 'OA', per the Landings data. 
         MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT %in% c(34732) & CALENDAR_YEAR == 2021, 'A80', as.character(MANAGEMENT_PROGRAM_CODE)), # The darn North Star (33732) has no hauls in the data until 10/1/21, so could not be matched up, so no MGM code can be associated from haul data.  They are A80, per the newer data.
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
  mutate(NMFS_REGION = FINAL_N,
         NMFS_REGION = ifelse(is.na(NMFS_REGION) & PERMIT %in% c(5744, 3736, 5777, 17103),  'GOA', as.character(NMFS_REGION)), # hard-code all of these as GOA.  If they are still null at this point, then they have no data for the year in question.  So use the data from other years.  After investigating, these permits are all GOA.
         # NMFS_REGION = ifelse(is.na(NMFS_REGION) & PERMIT %in% c(200, 17058, 438, 1137, 1946, 2836, 32333),  'GOA', as.character(NMFS_REGION)), # hard-code these ones as GOA.  It is still null at this point, so we went to the landing for these trips and it was GOA.
         NMFS_REGION = ifelse(is.na(NMFS_REGION) & VESSEL_TYPE == 'PLANT' & PERMIT %in% c(5306, 5310, 5358, 5320, 4078, 5323, 35448),  'BSAI', as.character(NMFS_REGION)), # hard-code these plants as BSAI, they are all AFA BSAI plants.  If unable to find a join, we know these ones.
         NMFS_REGION = ifelse(is.na(NMFS_REGION) & VESSEL_TYPE == 'PLANT' & PERMIT %in% c(5342, 5392, 27990, 5305, 30883, 28695, 29502, 27989, 5294, 7021, 5299, 29550, 31740, 35011, 35479, 35457),  'GOA', as.character(NMFS_REGION)), # hard-code these plants as GOA, they are all GOA Open Access plants.  If unable to find a join, we know these ones.
         NMFS_REGION = ifelse(PERMIT == 3533 & is.na(NMFS_REGION) & CALENDAR_YEAR == 2019,   'GOA', as.character(NMFS_REGION)), # The darn Highliner DOES have deployment days in 2019, but no hauls, so no nmfs_region associated from haul data.  It is GOA (the trip left fro KODIAK); hardcode this.
         NMFS_REGION = ifelse(PERMIT == 34732 & is.na(NMFS_REGION) & CALENDAR_YEAR == 2021, 'BSAI', as.character(NMFS_REGION)) # The darn North Star has no data in haul until 10.1.21, so no data to match.  They are BSAI per the newer data.
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

###############



















###############
# Now, get the days for each factor combination.


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
# Calculate the rates.  First, by affi_type, for each factor combination.

rate_all_groupings_affi_type <- 
  cnt_dep_days_all_groupings %>%
  # First, join to raw_statements on cruise/permit, and sum to get the WEIGHTED number_of_incidents and number_of_statements. This is the NUMERATOR of the rates.
  left_join(raw_statements %>% mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR)) %>%  # LEFT join ensures all days where NO statements were written are counted!! Critical to accurate rate calc.
  group_by (CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, OLE_CATEGORY, AFFIDAVIT_TYPE) %>%
  summarize(TOTAL_STATEMENTS = sum(if_else(is.na(AFFIDAVIT_ID), 0, FACTOR_WEIGHT_MTHD_1)),
            TOTAL_INCIDENTS  = sum(if_else(is.na(AFFIDAVIT_ID), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1)) ) %>%
  ungroup() %>%
  #Next, join back to the cnt_dep_days on the FACTOR combinations, and get the total days etc.  These are the DENOMINATORS of the rates.
  inner_join(cnt_dep_days_all_groupings %>% 
               group_by(CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION) %>%
               summarize(TOTAL_DAYS      = sum(FACTOR_DAYS), 
                         TOTAL_OBSERVERS = n_distinct(OBSERVER_SEQ),
                         TOTAL_CRUISES   = n_distinct(CRUISE),
                         DISTINCT_OBSERVER_ASSIGNMENTS = n_distinct(CRUISE, PERMIT) ) %>%
               filter(DISTINCT_OBSERVER_ASSIGNMENTS >= 3) ) %>%  # for confidentiality, remove any factor combinations with < 3 cruise/permit assignments.
  #Finally!!!!!  Calculate the rates.
  mutate(AFFIS_PER_DAY         = TOTAL_STATEMENTS/TOTAL_DAYS,
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





# Make summary table of just the DEPLOYED DAYS for each factor grouping.
cnt_dep_days_by_factor_group <-
  rate_all_groupings_affi_type %>%
  distinct(CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, TOTAL_DAYS, TOTAL_CRUISES, TOTAL_OBSERVERS, DISTINCT_OBSERVER_ASSIGNMENTS
  ) %>%  
  mutate(FACTOR_GROUP = paste(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, sep = ''))
# save this summary table as CSV for output to a report





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

# Rate  by grouping ole category
rate_all_groupings_ole_category <- 
  cnt_dep_days_all_groupings %>%
  # First, join to raw_statements on cruise/permit, and sum to get the WEIGHTED number_of_incidents and number_of_statements. This is the NUMERATOR of the rates.
  left_join(raw_statements %>% mutate(CALENDAR_YEAR = FIRST_VIOL_YEAR)) %>%  # LEFT join ensures all days where NO statements were written are counted!! Critical to accurate rate calc.
  group_by (CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, OLE_CATEGORY) %>%
  summarize(TOTAL_STATEMENTS = sum(if_else(is.na(AFFIDAVIT_ID), 0, FACTOR_WEIGHT_MTHD_1)),
            TOTAL_INCIDENTS  = sum(if_else(is.na(AFFIDAVIT_ID), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1)) ) %>%
  ungroup() %>%
  #Next, join back to the cnt_dep_days on the FACTOR combinations, and get the total days etc.  These are the DENOMINATORS of the rates.
  inner_join(cnt_dep_days_all_groupings %>% 
               group_by(CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION) %>%
               summarize(TOTAL_DAYS      = sum(FACTOR_DAYS), 
                         TOTAL_OBSERVERS = n_distinct(OBSERVER_SEQ),
                         TOTAL_CRUISES   = n_distinct(CRUISE),
                         DISTINCT_OBSERVER_ASSIGNMENTS = n_distinct(CRUISE, PERMIT) ) %>%
               filter(DISTINCT_OBSERVER_ASSIGNMENTS >= 3) ) %>%  # for confidentiality, remove any factor combinations with < 3 cruise/permit assignments.
  #Finally!!!!!  Calculate the rates.
  mutate(AFFIS_PER_DAY         = TOTAL_STATEMENTS/TOTAL_DAYS,
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
         INCIDENTS_PER_ASSIGNMENT    = TOTAL_INCIDENTS/DISTINCT_OBSERVER_ASSIGNMENTS )



# Make summary table of just the DEPLOYED DAYS for each factor grouping.
cnt_dep_days_by_factor_group_ole_cat <-
  rate_all_groupings_ole_category %>%
  group_by(CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, TOTAL_DAYS, TOTAL_CRUISES, TOTAL_OBSERVERS, DISTINCT_OBSERVER_ASSIGNMENTS
  ) %>% 
  summarize(TOTAL_STATEMENTS = sum(if_else(is.na(TOTAL_STATEMENTS), 0, TOTAL_STATEMENTS)),
            TOTAL_INCIDENTS  = sum(if_else(is.na(TOTAL_INCIDENTS), 0, TOTAL_INCIDENTS))
  ) %>%
  mutate(FACTOR_GROUP = paste(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION, sep = ''))
# save this summary table as CSV for output to a report
write.csv(file = paste("charts_and_tables/tables/tbl_num_dep_days_by_factor_group_", Sys.Date(), ".csv", sep = ''),
          x    = cnt_dep_days_by_factor_group)






######################

# Make a CAST of these rates, for tables in the report.
rate_all_groupings_ole_category_cast_1000 <- 
  merge(
    reshape2::dcast(data      = rate_all_groupings_ole_category
                    %>% filter(!is.na(OLE_CATEGORY)), 
                    formula   = CALENDAR_YEAR + COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION ~ OLE_CATEGORY,
                    value.var = "INCIDENTS_PER_1000_DEPLOYED_DAYS"),
    cnt_dep_days_by_factor_group_ole_cat, all=TRUE)
write.csv(file = paste("charts_and_tables/tables/tbl_rate_all_cast_new_grouping_1000_days_", Sys.Date(), ".csv", sep = ''),
          x    = rate_all_groupings_ole_category_cast_1000)





rate_all_groupings_ole_category_cast_90 <- 
  merge(
    reshape2::dcast(data = rate_all_groupings_ole_category  %>%
                      filter(!is.na(OLE_CATEGORY)), 
                    formula   = CALENDAR_YEAR + COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION ~ OLE_CATEGORY,
                    value.var = "INCIDENTS_PER_90_DEPLOYED_DAYS"),
    cnt_dep_days_by_factor_group_ole_cat, all=TRUE)
write.csv(file = paste("charts_and_tables/tables/tbl_rate_all_cast_new_grouping_90_days_", Sys.Date(), ".csv", sep = ''),
          x    = rate_all_groupings_ole_category_cast_90)





rate_all_groupings_ole_category_cast_per_cruise <- 
  merge(
    reshape2::dcast(data      = rate_all_groupings_ole_category
                    %>% filter(!is.na(OLE_CATEGORY)), 
                    formula   = CALENDAR_YEAR + COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION ~ OLE_CATEGORY,
                    value.var = "INCIDENTS_PER_CRUISE"),
    cnt_dep_days_by_factor_group_ole_cat, all=TRUE)




rate_all_groupings_ole_category_cast_per_assnmt <- 
  merge(
    reshape2::dcast(data      = rate_all_groupings_ole_category
                    %>% filter(!is.na(OLE_CATEGORY)), 
                    formula   = CALENDAR_YEAR + COVERAGE_TYPE + VESSEL_TYPE + GEAR_TYPE + MANAGEMENT_PROGRAM_CODE + NMFS_REGION ~ OLE_CATEGORY,
                    value.var = "INCIDENTS_PER_ASSIGNMENT") ,
    cnt_dep_days_by_factor_group_ole_cat, all=TRUE)
write.csv(file = paste("charts_and_tables/tables/tbl_rate_all_groupings_ole_category_cast_per_assnmt_", Sys.Date(), ".csv", sep = ''),
          x    = rate_all_groupings_ole_category_cast_per_assnmt)




# merge all of these CASTed tables into one WIDE table, for the report.
rate_all_groupings_ole_category_cast_all <-
  merge(merge(rate_all_groupings_ole_category_cast_1000 %>%
                rename(ALL_OTHER_STATEMENT_TYPES_1000_DEPLOYED_DAYS = 'ALL OTHER STATEMENT TYPES',
                       COAST_GUARD_1000_DEPLOYED_DAYS = 'COAST GUARD',
                       LIMITED_ACCESS_PROGRAMS_1000_DEPLOYED_DAYS = 'LIMITED ACCESS PROGRAMS',
                       OLE_PRIORITY_INTER_PERSONAL_1000_DEPLOYED_DAYS = 'OLE PRIORITY: INTER-PERSONAL',
                       OLE_PRIORITY_SAFETY_AND_DUTIES_1000_DEPLOYED_DAYS = 'OLE PRIORITY: SAFETY AND DUTIES',
                       PROTECTED_RESOURCE_AND_PROHIBITED_SPECIES_1000_DEPLOYED_DAYS = 'PROTECTED RESOURCE & PROHIBITED SPECIES') ,
              rate_all_groupings_ole_category_cast_90 %>%
                rename(ALL_OTHER_STATEMENT_TYPES_90_DEPLOYED_DAYS = 'ALL OTHER STATEMENT TYPES',
                       COAST_GUARD_90_DEPLOYED_DAYS = 'COAST GUARD',
                       LIMITED_ACCESS_PROGRAMS_90_DEPLOYED_DAYS = 'LIMITED ACCESS PROGRAMS',
                       OLE_PRIORITY_INTER_PERSONAL_90_DEPLOYED_DAYS = 'OLE PRIORITY: INTER-PERSONAL',
                       OLE_PRIORITY_SAFETY_AND_DUTIES_90_DEPLOYED_DAYS = 'OLE PRIORITY: SAFETY AND DUTIES',
                       PROTECTED_RESOURCE_AND_PROHIBITED_SPECIES_90_DEPLOYED_DAYS = 'PROTECTED RESOURCE & PROHIBITED SPECIES')  ,
              all = TRUE),
        merge(rate_all_groupings_ole_category_cast_per_cruise %>%
                rename(ALL_OTHER_STATEMENT_TYPES_CRUISE = 'ALL OTHER STATEMENT TYPES',
                       COAST_GUARD_CRUISE = 'COAST GUARD',
                       LIMITED_ACCESS_PROGRAMS_CRUISE = 'LIMITED ACCESS PROGRAMS',
                       OLE_PRIORITY_INTER_PERSONAL_CRUISE= 'OLE PRIORITY: INTER-PERSONAL',
                       OLE_PRIORITY_SAFETY_AND_DUTIES_CRUISE = 'OLE PRIORITY: SAFETY AND DUTIES',
                       PROTECTED_RESOURCE_AND_PROHIBITED_SPECIES_CRUISE = 'PROTECTED RESOURCE & PROHIBITED SPECIES') ,
              rate_all_groupings_ole_category_cast_per_assnmt %>%
                rename(ALL_OTHER_STATEMENT_TYPES_ASSIGNMENT = 'ALL OTHER STATEMENT TYPES',
                       COAST_GUARD_ASSIGNMENT = 'COAST GUARD',
                       LIMITED_ACCESS_PROGRAMS_ASSIGNMENT = 'LIMITED ACCESS PROGRAMS',
                       OLE_PRIORITY_INTER_PERSONAL_ASSIGNMENT = 'OLE PRIORITY: INTER-PERSONAL',
                       OLE_PRIORITY_SAFETY_AND_DUTIES_ASSIGNMENT = 'OLE PRIORITY: SAFETY AND DUTIES',
                       PROTECTED_RESOURCE_AND_PROHIBITED_SPECIES_ASSIGNMENT = 'PROTECTED RESOURCE & PROHIBITED SPECIES') ,
              all = TRUE),
        all = TRUE)

# save this summary table as CSV for output to a report
write.csv(file = paste("charts_and_tables/tables/tbl_rate_all_cast_new_grouping_", Sys.Date(), ".csv", sep = ''),
          x    = rate_all_groupings_ole_category_cast_all)
















#############################
# Summarize ALL days and assignments, with no factors.
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
  summarize(N_OCCURRENCES = sum(NUMBER_VIOLATIONS)) %>%
  ungroup() %>%
  full_join(depl_days_all_summ) %>%
  mutate(RATE_PER_1000_DAYS = (N_OCCURRENCES/DAYS)*1000,
         RATE_PER_ASSNMT    = (N_OCCURRENCES/ASSIGNMENTS)
  ) %>%
  select(-CRUISES, -OBSERVERS, -DAYS, -ASSIGNMENTS)
















###############################

# Need to get the raw number of statements and number of incidents in each catagory, for the report.
sum_by_type <-
  raw_statements %>%
  filter(FIRST_VIOL_YEAR == adp_yr) %>%
  group_by(OLE_CATEGORY, AFFIDAVIT_TYPE  #, FIRST_VIOL_YEAR
  ) %>%
  summarize(N_STATEMENTS = n(),
            N_INCIDENTS        = sum(if_else(is.na(NUMBER_VIOLATIONS), 0, NUMBER_VIOLATIONS)),
            MEAN_INCIS_PER     = mean(NUMBER_VIOLATIONS),
            MEDIAN_INCIS_PER   = median(NUMBER_VIOLATIONS),
            MIN_INCIS_PER      = min(NUMBER_VIOLATIONS),
            MAX_INCIS_PER      = max(NUMBER_VIOLATIONS),
            QUANT_25_INCIS_PER = quantile(NUMBER_VIOLATIONS, 0.25),
            QUANT_75_INCIS_PER = quantile(NUMBER_VIOLATIONS, 0.75),
            STDEV_INCIS_PER    = sd(NUMBER_VIOLATIONS)
  )

write.csv(file = paste("charts_and_tables/tables/tbl_", 
                       adp_yr, 
                       "_sum_statements_incis_by_type_", 
                       Sys.Date(), ".csv", 
                       sep = ''),
          x    = sum_by_type)











sum_by_type_both_years <-
  raw_statements %>%
  distinct(OLE_CATEGORY, AFFIDAVIT_TYPE) %>%
  left_join(raw_statements %>%
              filter(FIRST_VIOL_YEAR %in% c(as.numeric(adp_yr), as.numeric(adp_yr)-1)) %>%
              group_by(OLE_CATEGORY, AFFIDAVIT_TYPE, FIRST_VIOL_YEAR
              ) %>%
              summarize(N_STATEMENTS = n(),
                        N_INCIDENTS        = sum(if_else(is.na(NUMBER_VIOLATIONS) | NUMBER_VIOLATIONS == 0, 1, NUMBER_VIOLATIONS)),
                        MEAN_INCIS_PER     = mean(NUMBER_VIOLATIONS),
                        MEDIAN_INCIS_PER   = median(NUMBER_VIOLATIONS),
                        MIN_INCIS_PER      = min(NUMBER_VIOLATIONS),
                        MAX_INCIS_PER      = max(NUMBER_VIOLATIONS),
                        QUANT_25_INCIS_PER = quantile(NUMBER_VIOLATIONS, 0.25),
                        QUANT_75_INCIS_PER = quantile(NUMBER_VIOLATIONS, 0.75),
                        STDEV_INCIS_PER    = sd(NUMBER_VIOLATIONS)
              )
            )










# need to get the factor groups that have 0's for each statement category, for the report.
zeros_by_cat <-
  merge(
    days_statements_all_groupings_raw %>%
      filter(CALENDAR_YEAR == adp_yr) %>%
      distinct(OLE_CATEGORY, AFFIDAVIT_TYPE),
    days_statements_all_groupings_raw  %>%
      filter(CALENDAR_YEAR == adp_yr) %>% 
      distinct(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION), 
    all = TRUE ) %>%
  filter (!is.na(AFFIDAVIT_TYPE)) %>%
  left_join(days_statements_all_groupings_raw %>%
              filter(CALENDAR_YEAR == adp_yr) ) %>%
  group_by(OLE_CATEGORY, AFFIDAVIT_TYPE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION) %>%
  summarize(SUM_INCIDS = sum(if_else(is.na(AFFIDAVIT_ID), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1))) %>%
  filter(SUM_INCIDS <= 0.00000000) %>%
  ungroup() %>%
  group_by(OLE_CATEGORY, AFFIDAVIT_TYPE) %>%
  summarize(NUM_CATEGS_WITH_0_INCIDS = n())






write.csv(file = paste("charts_and_tables/tables/tbl_", 
                       adp_yr, 
                       "_zero_factors_by_type_", 
                       Sys.Date(), ".csv", 
                       sep = ''),
          x    = zeros_by_cat)







# add zeros to sum_by_type
sum_by_type_plus_zeros <- 
  merge(sum_by_type , zeros_by_cat, all = TRUE) %>%
  mutate(OLE_CATEGORY = factor(OLE_CATEGORY, levels = c('OLE PRIORITY: INTER-PERSONAL', 'OLE PRIORITY: SAFETY AND DUTIES', 'COAST GUARD',
                                                        'LIMITED ACCESS PROGRAMS', 'PROTECTED RESOURCE & PROHIBITED SPECIES', 'ALL OTHER STATEMENT TYPES')))


write.csv(file = paste("charts_and_tables/tables/tbl_", 
                       adp_yr, 
                       "_sum_statements_incis_plus_zeros_by_type_", 
                       Sys.Date(), ".csv", 
                       sep = ''),
          x    = sum_by_type_plus_zeros)



factor_grp_summ <-
  assnmts_days_all_groupings %>%
  group_by(CALENDAR_YEAR, COVERAGE_TYPE, GEAR_TYPE, VESSEL_TYPE, NMFS_REGION, MANAGEMENT_PROGRAM_CODE) %>%
  summarize(N_ASSNMTS = n_distinct(CRUISE, PERMIT),
            N_CRUISES = n_distinct(CRUISE),
            N_OBSERVERS = n_distinct(OBSERVER_SEQ),
            N_DAYS = n_distinct(CRUISE, PERMIT, DEPLOYED_DATE))


proport_cats_with_incis <-
  factor_grp_summ %>%
  filter(N_ASSNMTS > 2) %>%
  left_join(days_statements_all_groupings_raw %>%
              group_by(CALENDAR_YEAR, OLE_CATEGORY, AFFIDAVIT_TYPE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION) %>%
              summarize(SUM_INCIDS = sum(if_else(is.na(AFFIDAVIT_ID), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1)))
            ) %>%
  ungroup() %>%
  group_by(CALENDAR_YEAR, OLE_CATEGORY, AFFIDAVIT_TYPE) %>%
  summarize(N_FACTOR_GRPS_WITH_INCIDS = n_distinct(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION)) %>%
  ungroup() %>%
  full_join(factor_grp_summ %>%
              filter(N_ASSNMTS > 2) %>%
              group_by(CALENDAR_YEAR) %>%
              summarize(N_TOTAL_FACTOR_GRPS = n()) ) %>%
  mutate(PORPORT_GRPS_WITH_INCIDS = N_FACTOR_GRPS_WITH_INCIDS/N_TOTAL_FACTOR_GRPS) %>%
  filter(!is.na(OLE_CATEGORY))
         
test5 <-
  rate_all_groupings_affi_type %>% 
    filter(CALENDAR_YEAR == 2021) %>%
    distinct(MANAGEMENT_PROGRAM_CODE)



###############################
###############################


# Need to get the raw number of statements and number of incidents in each catagory, for the report
# PREVIOUS YEAR
#########################.
sum_by_type_prev_year <-
  raw_statements %>%
  filter(FIRST_VIOL_YEAR == as.numeric(adp_yr)-1) %>%
  group_by(OLE_CATEGORY, AFFIDAVIT_TYPE  #, FIRST_VIOL_YEAR
  ) %>%
  summarize(N_STATEMENTS = n(),
            N_INCIDENTS        = sum(if_else(is.na(NUMBER_VIOLATIONS), 0, NUMBER_VIOLATIONS)),
            MEAN_INCIS_PER     = mean(NUMBER_VIOLATIONS),
            MEDIAN_INCIS_PER   = median(NUMBER_VIOLATIONS),
            MIN_INCIS_PER      = min(NUMBER_VIOLATIONS),
            MAX_INCIS_PER      = max(NUMBER_VIOLATIONS),
            QUANT_25_INCIS_PER = quantile(NUMBER_VIOLATIONS, 0.25),
            QUANT_75_INCIS_PER = quantile(NUMBER_VIOLATIONS, 0.75),
            STDEV_INCIS_PER    = sd(NUMBER_VIOLATIONS)
  )

write.csv(file = paste("charts_and_tables/tables/tbl_", 
                       adp_yr, 
                       "_sum_statements_incis_by_type_previous_year_", 
                       Sys.Date(), ".csv", 
                       sep = ''),
          x    = sum_by_type_prev_year)




# need to get the factor groups that have 0's for each statement category, for the report.
zeros_by_cat_prev_year <-
  merge(
    days_statements_all_groupings_raw %>%
      filter(CALENDAR_YEAR == as.numeric(adp_yr)-1) %>%
      distinct(OLE_CATEGORY, AFFIDAVIT_TYPE),
    days_statements_all_groupings_raw  %>%
      filter(CALENDAR_YEAR == as.numeric(adp_yr)-1) %>% 
      distinct(COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION), 
    all = TRUE ) %>%
  filter (!is.na(AFFIDAVIT_TYPE)) %>%
  left_join(days_statements_all_groupings_raw %>%
              filter(CALENDAR_YEAR == as.numeric(adp_yr)-1) ) %>%
  group_by(OLE_CATEGORY, AFFIDAVIT_TYPE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION) %>%
  summarize(SUM_INCIDS = sum(if_else(is.na(AFFIDAVIT_ID), 0, NUMBER_VIOLATIONS*FACTOR_WEIGHT_MTHD_1))) %>%
  filter(SUM_INCIDS == 0) %>%
  ungroup() %>%
  group_by(OLE_CATEGORY, AFFIDAVIT_TYPE) %>%
  summarize(NUM_CATEGS_WITH_0_INCIDS = n())


write.csv(file = paste("charts_and_tables/tables/tbl_", 
                       adp_yr, 
                       "_zero_factors_by_type_previous_year_", 
                       Sys.Date(), ".csv", 
                       sep = ''),
          x    = zeros_by_cat_prev_year)






####################
sum_by_type_prev_year_plus_zeros <- 
  merge(sum_by_type_prev_year , zeros_by_cat_prev_year, all = TRUE) %>%
  mutate(OLE_CATEGORY = factor(OLE_CATEGORY, levels = c('OLE PRIORITY: INTER-PERSONAL', 'OLE PRIORITY: SAFETY AND DUTIES', 'COAST GUARD',
                                                        'LIMITED ACCESS PROGRAMS', 'PROTECTED RESOURCE & PROHIBITED SPECIES', 'ALL OTHER STATEMENT TYPES')))


write.csv(file = paste("charts_and_tables/tables/tbl_", 
                       adp_yr, 
                       "_sum_statements_incis_by_type_plus_zeros_previous_year_", 
                       Sys.Date(), ".csv", 
                       sep = ''),
          x    = sum_by_type_prev_year_plus_zeros)


















#######################
test <- 
  sqldf("SELECT DISTINCT OLE_CATEGORY, AFFIDAVIT_TYPE
           FROM rate_all_groupings_affi_type
          ORDER BY OLE_CATEGORY, AFFIDAVIT_TYPE") 


test2 <-
  sqldf("SELECT * FROM
             (SELECT CALENDAR_YEAR,
                     COVERAGE_TYPE,
                     VESSEL_TYPE,
                     GEAR_TYPE,
                     MANAGEMENT_PROGRAM_CODE,
                     NMFS_REGION,
                     count(distinct cruise||permit) AS N_ASSNMTS
                FROM assnmts_days_all_groupings
               GROUP BY CALENDAR_YEAR,
                     COVERAGE_TYPE,
                     VESSEL_TYPE,
                     GEAR_TYPE,
                     MANAGEMENT_PROGRAM_CODE,
                     NMFS_REGION)
          WHERE N_ASSNMTS >= 3
        ORDER BY CALENDAR_YEAR")



test_3 <-
  cnt_dep_days_by_factor_group %>%
  group_by(CALENDAR_YEAR) %>%
  summarize(N_FACTOR_GROUPS = n_distinct(COVERAGE_TYPE,
                                         VESSEL_TYPE,
                                         GEAR_TYPE,
                                         MANAGEMENT_PROGRAM_CODE,
                                         NMFS_REGION))

##################
# Save Output -------------------------------------------------------------
#Requires the folder 'scripts'
save(list = ls(),
     file = paste0("scripts/", "AR_summary_scripts.rdata"))























