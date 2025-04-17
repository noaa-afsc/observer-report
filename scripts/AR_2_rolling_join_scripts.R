#################### #
# Annual Report Enforcement chapter: rolling Joins and Rate Calculations
# Contact Andy Kingham
# 206-526-4212


# Set up environment -----------------------------------------------------------

# Load pkgs
if(!require("plyr"))        install.packages("plyr",        repos='http://cran.us.r-project.org')
if(!require("reshape2"))    install.packages("reshape2",    repos='http://cran.us.r-project.org')
if(!require("tidyverse"))   install.packages("tidyverse",       repos='http://cran.us.r-project.org')
if(!require("data.table"))  install.packages("data.table",  repos='http://cran.us.r-project.org')
if(!require("sqldf"))       install.packages("sqldf",       repos='http://cran.us.r-project.org')
if(!require("devtools"))    install.packages("devtools",   repos='http://cran.us.r-project.org')
if(!require("FMAtools"))    devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")

# Clear everything first
rm(list = ls())

# Set the .Rdata file we will load
file_1_name  <- "AR_1_Statements_data.Rdata"

# Assign the address of the Annual Report Project in the Shared Gdrive
AnnRpt_EnfChp_dribble <- gdrive_set_dribble("Projects/Annual Report OLE chapter 5/2024_data")

# Pull Rdata file from google drive. This will update your local if the GDrive version is newer,
# otherwise it will load your local
gdrive_download(file_1_name, AnnRpt_EnfChp_dribble)

# load it from local into R
load(file = file_1_name)


# Begin Data Munge Section ------------------------------------------------------

#First, get all the factors by joining the ASSIGNMENTS data to the haul data.
# This gets each factor for every date that has a value in haul data for that date.
# Joining by permit and date ensures that all factors are joined even for 2nd observers, for each vessel and date.

# NEW for AR 2024: we are now looking at multiple unit types for the denominator of the rates.
# This original method of calculating factors for each DEPLOYMENT DAY, is still the method being used 
# to calculate the DAYS unit type denominator.
# Other unit type denominators (e.g., HAULS, TRIPS, OFFLOADS, DEPLOYMENTS, etc) will be calculated separately, at the END of this script.
# ADK 20250311

################# #
# COMBINE factors, so each COMBINATION is unique. ------------

#First, get all the factors by joining the ASSIGNMENTS data to the haul data for vessel observers and the offload data for plant observers.
# This gets each factor for every date that has a value in haul data or offload data for that date.
# Joining by permit and date ensures that all factors are joined even for 2nd observers, for each vessel or plant, and date.

assnmts_days_all_groupings <-   
  assignments_dates_cr_perm %>%
  filter(CALENDAR_YEAR <= as.numeric(adp_yr) & CALENDAR_YEAR >= as.numeric(adp_yr) -1 ) %>%
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME, CRUISE, PERMIT, DEPLOYED_DATE, VESSEL_OR_PLANT, COVERAGE_TYPE) %>%  # remove duplicates; rows are unique by cruise, permit, date, and factor combination.
  left_join(rbind(hauls %>%
                    # exact match(es) for VESSEL days where hauls exist:
                    distinct(PERMIT,
                             DEPLOYED_DATE = HAUL_DATE,
                             VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, NMFS_REGION,
                             TRAWL_EM = NA),
                  # exact match(es) for plant days where offloads exist:
                  # This step is just for PLANTS, so we have to filter out the CP's
                  # because those show up as "PROCESSORS" in E-landings.
                  # Also: don't get the VESSEL_TYPE or GEAR_TYPE for PLANTS; those will just be set to null. 
                  df_elandings_raw %>%
                    filter(VESSEL_TYPE != 'CP/MS') %>%
                    distinct(PERMIT = PROCESSOR_PERMIT_ID,
                             DEPLOYED_DATE = LANDING_DATE, # for plant data, ONLY use the ACTUAL landing date.
                             VESSEL_TYPE = NA, GEAR_TYPE = NA,
                             MANAGEMENT_PROGRAM_CODE, NMFS_REGION,
                             TRAWL_EM = ifelse(MANAGEMENT_PROGRAM_MODIFIER == 'TEM', 'Y', 'N'))
                   ),
            relationship = "many-to-many"
            ) %>%
  mutate(VESSEL_TYPE = ifelse(VESSEL_OR_PLANT == 'P', 'PLANT', VESSEL_TYPE),
         GEAR_TYPE   = ifelse(VESSEL_OR_PLANT == 'P', ' ', GEAR_TYPE) # using a space here makes the plots nicer.
         ) %>%
  # next, join back to the expanded landings data for VESSEL data, to fill in the missing factors
  # for any VESSEL days that do not have haul data, but have a "day" in the offload data, we can get the factors from that.
  # Use dummy column names to prevent joining on the FACTOR columns; 
  # Then only use the DUMMY if the FACTOR is null after the previous 2 steps.
  # join on ALL dates from fishing_start_date thru landing_date
  left_join(df_elandings_expand %>%
               filter(VESSEL_TYPE == 'CV') %>%
               distinct(PERMIT        = VESSEL_ID,
                        DEPLOYED_DATE, 
                        DUMMY_VESSEL_TYPE = VESSEL_TYPE, 
                        DUMMY_GEAR_TYPE   = GEAR_TYPE,
                        DUMMY_MANAGEMENT_PROGRAM_CODE = MANAGEMENT_PROGRAM_CODE, 
                        DUMMY_NMFS_REGION = NMFS_REGION),
            relationship = "many-to-many") %>%
  mutate(VESSEL_TYPE = ifelse(is.na(VESSEL_TYPE), DUMMY_VESSEL_TYPE, VESSEL_TYPE),
         GEAR_TYPE   = ifelse(is.na(GEAR_TYPE),   DUMMY_GEAR_TYPE, GEAR_TYPE),
         MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE), DUMMY_MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_CODE),
         NMFS_REGION = ifelse(is.na(NMFS_REGION), DUMMY_NMFS_REGION, NMFS_REGION)
          ) %>%
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)



#Check for NA's
table(assnmts_days_all_groupings$VESSEL_TYPE, exclude = FALSE)







###################################### #

# Get missing dates: Non-fishing days, etc. ----------  
# Use data.table's ROLLING JOIN feature to get the 'nearest-neighbor' of date match within the permit. 
# This effectively fills in the dates with no value for the factor, by getting the value from the closest date within the cruise/permit.

# have to do this SEVERAL TIMES.  Starting with the MOST DIRECT first.  Then going outward to LESS DIRECT (more roll) from there.
#   First, on cruise/permit, looking at HAULS. 
#   Second, on cruise/permit, looking at OFFLOADS.
#   Third, on just permit, looking at HAULS.
#   Fourth, on just permit, looking at OFFLOADS.
#
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
dt_hauls_v[, VESSEL_TYPE := as.character(VESSEL_TYPE)]  # coercing to character instead of factor

assnmts_days_all_groupings <- dt_hauls_v[dt_t, roll="nearest"] # now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_V := ifelse(!is.na(i.VESSEL_TYPE), i.VESSEL_TYPE, VESSEL_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE
assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(VESSEL_TYPE = FINAL_V) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$VESSEL_TYPE, exclude = FALSE)



# Next use the offload data, to fill in gaps for non-fishing days, etc 
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE) 
dt_offloads <- copy(df_elandings_expand)
setDT(dt_offloads)
dt_offloads_v <- dt_offloads[, .(CALENDAR_YEAR, VESSEL_TYPE, PERMIT = VESSEL_ID, DEPLOYED_DATE)]   # removing unneeded columns
setkey(dt_offloads_v, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_offloads_v <- unique(dt_offloads_v)  # removing duplicates
dt_offloads_v[, VESSEL_TYPE := as.character(VESSEL_TYPE)]  # coercing to character instead of factor
assnmts_days_all_groupings <- dt_offloads_v[dt_t, roll="nearest"] # now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_V := ifelse(!is.na(i.VESSEL_TYPE), i.VESSEL_TYPE, VESSEL_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE
assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(VESSEL_TYPE = FINAL_V) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE,VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's
 table(assnmts_days_all_groupings$VESSEL_TYPE, exclude = FALSE)
 
test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE VESSEL_TYPE is null')

############################# #



















############################# #
############################# #

# Next, for GEAR_TYPE. -----------------
# First, using HAULS
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)         
dt_hauls <- copy(hauls)
setDT(dt_hauls)
dt_hauls_g <- dt_hauls[, .(CALENDAR_YEAR, GEAR_TYPE, CRUISE = if_else(SAMPLED_BY_CRUISE != CRUISE & SAMPLED_BY_CRUISE != 0, SAMPLED_BY_CRUISE, CRUISE), PERMIT, DEPLOYED_DATE = HAUL_DATE)]   # removing unneeded columns
setkey(dt_hauls_g, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_hauls_g <- unique(dt_hauls_g)  # removing duplicates
dt_hauls_g[, GEAR_TYPE := as.character(GEAR_TYPE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_hauls_g[dt_t, roll="nearest"] # now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_G := ifelse(!is.na(i.GEAR_TYPE), i.GEAR_TYPE, GEAR_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(GEAR_TYPE = FINAL_G ) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$GEAR_TYPE, exclude = FALSE)





# Next for Gear_type:
# using OFFLOADS
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_elandings_expand)
setDT(dt_offloads)
dt_offloads_g <- dt_offloads[, .(CALENDAR_YEAR, GEAR_TYPE, PERMIT = VESSEL_ID, DEPLOYED_DATE)]   # removing unneeded columns
setkey(dt_offloads_g, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_offloads_g <- unique(dt_offloads_g)  # removing duplicates
dt_offloads_g[, GEAR_TYPE := as.character(GEAR_TYPE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings <- dt_offloads_g[dt_t, roll="nearest"] # now join the dates using a rolling join.
assnmts_days_all_groupings[, FINAL_G := ifelse(!is.na(i.GEAR_TYPE), i.GEAR_TYPE, GEAR_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings <-
  assnmts_days_all_groupings %>%
  mutate(GEAR_TYPE = FINAL_G ) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$GEAR_TYPE, exclude = FALSE)

test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE GEAR_TYPE is null')





####################### #

# Next, for NMFS_REGION. --------------
# For VESSELS, from the HAUL data
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)



# NMFS_REGION
# For VESSELS, from the OFFLOAD data
dt_t <- copy(assnmts_days_all_groupings %>% filter(VESSEL_TYPE != 'PLANT') )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_elandings_expand)
setDT(dt_offloads)
dt_offloads_n <- dt_offloads[, .(CALENDAR_YEAR, NMFS_REGION, PERMIT = VESSEL_ID, DEPLOYED_DATE)]   # removing unneeded columns
setkey(dt_offloads_n, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_offloads_n <- unique(dt_offloads_n)  # removing duplicates
dt_offloads_n[, NMFS_REGION := as.character(NMFS_REGION)]  # coercing to character instead of factor

assnmts_days_all_groupings_vessel_temp <- dt_offloads_n[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings_vessel_temp[, FINAL_N := ifelse(!is.na(i.NMFS_REGION), i.NMFS_REGION, NMFS_REGION)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings_vessel_temp <-
  assnmts_days_all_groupings_vessel_temp %>%
  mutate(NMFS_REGION = FINAL_N) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings_vessel_temp$CALENDAR_YEAR, assnmts_days_all_groupings_vessel_temp$NMFS_REGION, exclude = FALSE)




# NMFS_REGION
# For PLANTS, from the OFFLOAD data
dt_t <- copy(assnmts_days_all_groupings %>% filter(VESSEL_TYPE == 'PLANT') )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_elandings_raw)
setDT(dt_offloads)
dt_offloads_n <- dt_offloads[, .(CALENDAR_YEAR, NMFS_REGION, PERMIT = PROCESSOR_PERMIT_ID, DEPLOYED_DATE = LANDING_DATE)]   # removing unneeded columns; for PLANTS, only use LANDING_DATE (not the expanded dates)
setkey(dt_offloads_n, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_offloads_n <- unique(dt_offloads_n)  # removing duplicates
dt_offloads_n[, NMFS_REGION := as.character(NMFS_REGION)]  # coercing to character instead of factor

assnmts_days_all_groupings_plant_temp <- dt_offloads_n[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings_plant_temp[, FINAL_N := ifelse(!is.na(i.NMFS_REGION), i.NMFS_REGION, NMFS_REGION)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings_plant_temp <-
  assnmts_days_all_groupings_plant_temp %>%
  mutate(NMFS_REGION = FINAL_N) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings_plant_temp$CALENDAR_YEAR, assnmts_days_all_groupings_plant_temp$NMFS_REGION, exclude = FALSE)



# Bind the VESSEL and PLANT df's together
assnmts_days_all_groupings <-
  rbind(assnmts_days_all_groupings_plant_temp,
        assnmts_days_all_groupings_vessel_temp
  )

rm(assnmts_days_all_groupings_plant_temp,
   assnmts_days_all_groupings_vessel_temp)



#Check for NA's
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)
table(assnmts_days_all_groupings$GEAR_TYPE, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)
table(assnmts_days_all_groupings$VESSEL_TYPE, assnmts_days_all_groupings$NMFS_REGION,   exclude = FALSE)



test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE NMFS_REGION is null')






####################### #

# Next, MANAGEMENT_PROGRAM_CODE. ---------------
# First, using HAULS
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)




# Using Offloads
# VESSELS
dt_t <- copy(assnmts_days_all_groupings %>% filter(VESSEL_TYPE != 'PLANT') )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_elandings_expand)
setDT(dt_offloads)
dt_offloads_m <- dt_offloads[, .(CALENDAR_YEAR, MANAGEMENT_PROGRAM_CODE, PERMIT = VESSEL_ID, DEPLOYED_DATE)]   # removing unneeded columns
setkey(dt_offloads_m, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_offloads_m <- unique(dt_offloads_m)  # removing duplicates
dt_offloads_m[, MANAGEMENT_PROGRAM_CODE := as.character(MANAGEMENT_PROGRAM_CODE)]  # coercing to character instead of factor

assnmts_days_all_groupings_vessel_temp <- dt_offloads_m[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings_vessel_temp[, FINAL_M := ifelse(!is.na(i.MANAGEMENT_PROGRAM_CODE), i.MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_CODE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings_vessel_temp <-
  assnmts_days_all_groupings_vessel_temp %>%
  mutate(MANAGEMENT_PROGRAM_CODE = FINAL_M
  ) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings_vessel_temp$CALENDAR_YEAR, assnmts_days_all_groupings_vessel_temp$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)



# Using Offloads
# PLANTS
dt_t <- copy(assnmts_days_all_groupings %>% filter(VESSEL_TYPE == 'PLANT') )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_elandings_raw)
setDT(dt_offloads)
# Only use the actual LANDING_DATE for plant data;
dt_offloads_m <- dt_offloads[, .(CALENDAR_YEAR, MANAGEMENT_PROGRAM_CODE, PERMIT = PROCESSOR_PERMIT_ID, DEPLOYED_DATE = LANDING_DATE)]   # removing unneeded columns
setkey(dt_offloads_m, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_offloads_m <- unique(dt_offloads_m)  # removing duplicates
dt_offloads_m[, MANAGEMENT_PROGRAM_CODE := as.character(MANAGEMENT_PROGRAM_CODE)]  # coercing to character instead of factor

assnmts_days_all_groupings_plant_temp <- dt_offloads_m[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings_plant_temp[, FINAL_M := ifelse(!is.na(MANAGEMENT_PROGRAM_CODE), i.MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_CODE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings_plant_temp <-
  assnmts_days_all_groupings_plant_temp %>%
  mutate(MANAGEMENT_PROGRAM_CODE = FINAL_M) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's
table(assnmts_days_all_groupings_plant_temp$CALENDAR_YEAR, assnmts_days_all_groupings_plant_temp$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)




# Do it again for the remaining PLANT data (from df_offloads), with ONLY PERMIT (without CALENDAR_YEAR)
dt_t <- copy(assnmts_days_all_groupings_plant_temp )
setDT(dt_t)
setkey(dt_t, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_elandings_raw)
setDT(dt_offloads)
dt_offloads_m <- dt_offloads[, .(MANAGEMENT_PROGRAM_CODE, PERMIT = PROCESSOR_PERMIT_ID, DEPLOYED_DATE = LANDING_DATE)]   # removing unneeded columns
setkey(dt_offloads_m, PERMIT, DEPLOYED_DATE)
dt_offloads_m <- unique(dt_offloads_m)  # removing duplicates
dt_offloads_m[, MANAGEMENT_PROGRAM_CODE := as.character(MANAGEMENT_PROGRAM_CODE)]  # coercing to character instead of factor, because dt_vs$VESSEL_TYPE is character

assnmts_days_all_groupings_plant_temp <- dt_offloads_m[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings_plant_temp[, FINAL_M := ifelse(!is.na(i.MANAGEMENT_PROGRAM_CODE), i.MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_CODE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings_plant_temp <-
  assnmts_days_all_groupings_plant_temp %>%
  mutate(MANAGEMENT_PROGRAM_CODE = FINAL_M) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)


# Check for NA's.
table(assnmts_days_all_groupings_plant_temp$CALENDAR_YEAR, assnmts_days_all_groupings_plant_temp$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)


# Bind the vessel and plant rolling joints together.
assnmts_days_all_groupings <-
  rbind(assnmts_days_all_groupings_plant_temp,
        assnmts_days_all_groupings_vessel_temp) %>%
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME,  CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)

test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE MANAGEMENT_PROGRAM_CODE is null')


rm(assnmts_days_all_groupings_plant_temp, 
   assnmts_days_all_groupings_vessel_temp)











##################### #


# Check for NA's in any other column.  Because, there shouldn't be any if the rolling join worked correctly.
# If there are, go investigate these permits in landings data etc to figure out what the factor should be.  
# Then, go back to the first step and hardcode those permits.


nvl_test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings 
          WHERE NMFS_REGION is null
             OR MANAGEMENT_PROGRAM_CODE is null
             OR GEAR_TYPE is null
             OR VESSEL_TYPE is null')









#####################  #

# Get FISHERY DATA DAYS ----------
# and stick a 'Y/N' flag onto each deployed date (Y if fishery data exist for the date; N if no fishery data for date)
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






################## #
# remove unneeded temporary objects. ---------
rm(dt_offloads_m, dt_offloads_n, dt_offloads_g, dt_offloads_v, dt_offloads, dt_hauls, dt_hauls_g, dt_hauls_m,
   dt_hauls_n, dt_hauls_v, nvl_test_df, test_df, dt_t)




############################ #
# Calculate the same factor summaries for the other UNIT types. ----------
# ADK 20250311

# HAULS unit
hauls_with_factors <-
  hauls %>%
    # uses the LEAD_CRUISE only (this is DATA_CRUISE in the statements data)
    # TODO: consider this further.  Do we need to get for 2nd observers...??? not sure at this point. ADK 20250325
    distinct(CRUISE, PERMIT, HAUL_SEQ, TRIP_SEQ, CALENDAR_YEAR, DEPLOYED_DATE = HAUL_DATE, 
             SAMPLED_BY_CRUISE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM = NA,  NMFS_REGION) %>%
    # have to join to assignments to get coverage_type for the DEPLOYED_DATE, by matching that to the HAUL_DATE
    inner_join(assignments_dates_cr_perm %>%
                distinct(OBSERVER_NAME, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE,
                         CALENDAR_YEAR, COVERAGE_TYPE)
              ) %>%
    distinct()
             


# OFFLOADS unit
offloads_with_factors <-
  # have to join to assignments to get observer_seq, coverage_type for the DEPLOYED_DATE, then can match that to the LANDING_DATE
  assignments_dates_cr_perm %>%
    distinct(OBSERVER_NAME, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE,
             CALENDAR_YEAR, COVERAGE_TYPE, VESSEL_OR_PLANT) %>%
    inner_join(df_obs_offloads %>%
                 mutate(PERMIT = as.numeric(PERMIT),
                        PLANT_PERMIT = as.numeric(PLANT_PERMIT)
                        ) %>%
                 inner_join(rbind(df_elandings_raw %>%
                                   mutate(PERMIT = VESSEL_ID,
                                          VESSEL_OR_PLANT = 'V'),
                                   df_elandings_raw %>%
                                   mutate(PERMIT = PROCESSOR_PERMIT_ID,
                                          VESSEL_OR_PLANT = 'P',
                                          VESSEL_TYPE = 'PLANT')  # for plant observers, remove the vessel_type from the fish ticket data and replace with PLANT
                                ) %>%
                            mutate(TRAWL_EM = ifelse(MANAGEMENT_PROGRAM_CODE == 'TEM','Y', NA)) %>%
                            distinct(REPORT_ID, DEPLOYED_DATE = LANDING_DATE,
                                     FISHING_START_DATE, FISHING_DAYS, VESSEL_TYPE, GEAR_TYPE,
                                     MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION, VESSEL_OR_PLANT 
                                     )  ) )


# TRIPS unit
#   Uses the assnmts_days_all_groupings to get the FACTORS,
#   because we need to include non-fishing trips. Because there are statements written with NF trips as the selected unit!
#   Then there are still some trips that do not join with deployment days.
#   (11 days in 2024 ).  This is likely bad data, need to investigate.
#   Since we are only getting the factors at the trip level we use a rolling join 
#   to fill in these gaps

trips_with_factors <-
  # start with exact join to get haul-based factors for all fishing trips
  # then have to join to assignments to get factors for the DEPLOYED_DATE, then can match that to the TRIP_START_DATE or TRIP_END_DATE
  obs_trips_expand %>%
    mutate(PERMIT = as.numeric(PERMIT)) %>%
    left_join(assnmts_days_all_groupings %>%
                distinct(CALENDAR_YEAR, OBSERVER_SEQ, OBSERVER_NAME, CRUISE, PERMIT, DEPLOYED_DATE, 
                         COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION),
              relationship = "many-to-many") # there are often multiple factors for a cruise/permit/date


#Check for NA's
table(trips_with_factors$CALENDAR_YEAR, trips_with_factors$VESSEL_TYPE, exclude = FALSE)
table(trips_with_factors$CALENDAR_YEAR, trips_with_factors$GEAR_TYPE, exclude = FALSE)
table(trips_with_factors$CALENDAR_YEAR, trips_with_factors$COVERAGE_TYPE, exclude = FALSE)
table(trips_with_factors$CALENDAR_YEAR, trips_with_factors$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)
table(trips_with_factors$CALENDAR_YEAR, trips_with_factors$NMFS_REGION, exclude = FALSE)





# Next do rolling join scripts  for TRIP factors

# First, for VESSEL TYPE
# uses the EXPANDED trips data.frame so we can match on EACH DAY OF THE TRIP.
# Then will collapse using distinct() later so that there is only one row x factor combination for each trip.
dt_t     <- setkey(setDT(copy(trips_with_factors)), 
                   CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_assnmts <- unique(setkey(setDT(copy(assnmts_days_all_groupings)), 
                          CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE))
dt_assnmts_t <- dt_assnmts[, .(CALENDAR_YEAR, VESSEL_TYPE, CRUISE, PERMIT, DEPLOYED_DATE)]   # removing unneeded columns
setkey(dt_assnmts_t, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_assnmts_t <- unique(dt_assnmts_t)  # removing duplicates
dt_assnmts_t[, VESSEL_TYPE := as.character(VESSEL_TYPE)]  # coercing to character instead of factor

trips_with_factors <- dt_assnmts_t[dt_t, roll="nearest"] # now join the dates using a rolling join.
trips_with_factors[, FINAL_T := ifelse(!is.na(i.VESSEL_TYPE), i.VESSEL_TYPE, VESSEL_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE
trips_with_factors <-
  trips_with_factors %>%
  mutate(VESSEL_TYPE = FINAL_T)


#Check for NA's
table(trips_with_factors$CALENDAR_YEAR, trips_with_factors$VESSEL_TYPE, exclude = FALSE)




# Next, for GEAR TYPE
dt_t     <- setkey(setDT(copy(trips_with_factors)), 
                   CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_assnmts <- unique(setkey(setDT(copy(assnmts_days_all_groupings)), 
                            CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE))
dt_assnmts_t <- dt_assnmts[, .(CALENDAR_YEAR, GEAR_TYPE, CRUISE, PERMIT, DEPLOYED_DATE)]   # removing unneeded columns
setkey(dt_assnmts_t, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_assnmts_t <- unique(dt_assnmts_t)  # removing duplicates
dt_assnmts_t[, GEAR_TYPE := as.character(GEAR_TYPE)]  # coercing to character instead of factor

trips_with_factors <- dt_assnmts_t[dt_t, roll="nearest"] # now join the dates using a rolling join.
trips_with_factors[, FINAL_T := ifelse(!is.na(i.GEAR_TYPE), i.GEAR_TYPE, GEAR_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE
trips_with_factors <-
  trips_with_factors %>%
  mutate(GEAR_TYPE = FINAL_T) 

#Check for NA's
table(trips_with_factors$CALENDAR_YEAR, trips_with_factors$GEAR_TYPE, exclude = FALSE)






# Next, for COVERAGE_TYPE
dt_t     <- setkey(setDT(copy(trips_with_factors)), 
                   CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_assnmts <- unique(setkey(setDT(copy(assnmts_days_all_groupings)), 
                            CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE))
dt_assnmts_t <- dt_assnmts[, .(CALENDAR_YEAR, COVERAGE_TYPE, CRUISE, PERMIT, DEPLOYED_DATE)]   # removing unneeded columns
setkey(dt_assnmts_t, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_assnmts_t <- unique(dt_assnmts_t)  # removing duplicates
dt_assnmts_t[, COVERAGE_TYPE := as.character(COVERAGE_TYPE)]  # coercing to character instead of factor

trips_with_factors <- dt_assnmts_t[dt_t, roll="nearest"] # now join the dates using a rolling join.
trips_with_factors[, FINAL_T := ifelse(!is.na(i.COVERAGE_TYPE), i.COVERAGE_TYPE, COVERAGE_TYPE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE
trips_with_factors <-
  trips_with_factors %>%
  mutate(COVERAGE_TYPE = FINAL_T) 

#Check for NA's
table(trips_with_factors$CALENDAR_YEAR, trips_with_factors$COVERAGE_TYPE, exclude = FALSE)





# Next, for MANAGEMENT_PROGRAM_CODE
dt_t     <- setkey(setDT(copy(trips_with_factors)), 
                   CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_assnmts <- unique(setkey(setDT(copy(assnmts_days_all_groupings)), 
                            CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE))
dt_assnmts_t <- dt_assnmts[, .(CALENDAR_YEAR, MANAGEMENT_PROGRAM_CODE, CRUISE, PERMIT, DEPLOYED_DATE)]   # removing unneeded columns
setkey(dt_assnmts_t, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_assnmts_t <- unique(dt_assnmts_t)  # removing duplicates
dt_assnmts_t[, MANAGEMENT_PROGRAM_CODE := as.character(MANAGEMENT_PROGRAM_CODE)]  # coercing to character instead of factor

trips_with_factors <- dt_assnmts_t[dt_t, roll="nearest"] # now join the dates using a rolling join.
trips_with_factors[, FINAL_T := ifelse(!is.na(i.MANAGEMENT_PROGRAM_CODE), i.MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_CODE)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE
trips_with_factors <-
  trips_with_factors %>%
  mutate(MANAGEMENT_PROGRAM_CODE = FINAL_T)

#Check for NA's
table(trips_with_factors$CALENDAR_YEAR, trips_with_factors$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)









# Next, for NMFS_REGION
dt_t     <- setkey(setDT(copy(trips_with_factors)), 
                   CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_assnmts <- unique(setkey(setDT(copy(assnmts_days_all_groupings)), 
                            CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE))
dt_assnmts_t <- dt_assnmts[, .(CALENDAR_YEAR, NMFS_REGION, CRUISE, PERMIT, DEPLOYED_DATE)]   # removing unneeded columns
setkey(dt_assnmts_t, CALENDAR_YEAR, CRUISE, PERMIT, DEPLOYED_DATE)
dt_assnmts_t <- unique(dt_assnmts_t)  # removing duplicates
dt_assnmts_t[, NMFS_REGION := as.character(NMFS_REGION)]  # coercing to character instead of factor

trips_with_factors <- dt_assnmts_t[dt_t, roll="nearest"] # now join the dates using a rolling join.
trips_with_factors[, FINAL_T := ifelse(!is.na(i.NMFS_REGION), i.NMFS_REGION, NMFS_REGION)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE
trips_with_factors <-
  trips_with_factors %>%
  mutate(NMFS_REGION = FINAL_T) %>% 
  # LAST STEP, get rid of duplicate days.
  # for the TRIP data we only need one row per trip x factor combination! (not one for each day on the trip)
  distinct(CALENDAR_YEAR, OBSERVER_SEQ,  OBSERVER_NAME, CRUISE, PERMIT, TRIP_SEQ, TRIP_DESCRIPTION, TRIP_START_DATE, TRIP_END_DATE,
           EMBARKED_PORT_CODE, EMBARKED_PORT, DISEMBARKED_PORT_CODE, DISEMBARKED_PORT,
           DID_FISHING_OCCUR_FLAG, FISH_IN_HOLD_AT_START_FLAG, TRIP_DAYS,
           COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION
  )



#Check for NA's
table(trips_with_factors$CALENDAR_YEAR, trips_with_factors$NMFS_REGION, exclude = FALSE)



################## #
# remove unneeded temporary objects. ----------
rm(dt_t, dt_assnmts, dt_assnmts_t)













   

#####################  #
# SAMPLES
samp_with_factors <-
  df_obs_samples %>%
  mutate(PERMIT = as.numeric(PERMIT),
         ) %>%
  inner_join(rbind(hauls_with_factors %>%
                     distinct(OBSERVER_NAME, OBSERVER_SEQ, CALENDAR_YEAR, CRUISE, PERMIT, 
                              HAUL_SEQ, OFFLOAD_SEQ = NA,
                              COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, NMFS_REGION,
                              MANAGEMENT_PROGRAM_CODE, TRAWL_EM),
                   offloads_with_factors %>%
                     distinct(OBSERVER_NAME, OBSERVER_SEQ, CALENDAR_YEAR, CRUISE, PERMIT, 
                              HAUL_SEQ = NA, OFFLOAD_SEQ,
                              COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, NMFS_REGION,
                              MANAGEMENT_PROGRAM_CODE, TRAWL_EM)
             ),
             relationship = "many-to-many") %>%
  distinct()


# MARINE MAMMALS (MARM)
marm_with_factors <-
  df_obs_marm %>%
   mutate(PERMIT = as.numeric(PERMIT)) %>%
   inner_join(rbind(hauls_with_factors %>%
                        distinct(OBSERVER_NAME, OBSERVER_SEQ, CALENDAR_YEAR, CRUISE, PERMIT, 
                                 HAUL_SEQ, TRIP_SEQ = NA, OFFLOAD_SEQ = NA,
                                 COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, NMFS_REGION,
                                 MANAGEMENT_PROGRAM_CODE, TRAWL_EM),
                    
                    offloads_with_factors %>%
                        distinct(OBSERVER_NAME, OBSERVER_SEQ, CALENDAR_YEAR, CRUISE, PERMIT, 
                                 HAUL_SEQ = NA, TRIP_SEQ = NA, OFFLOAD_SEQ,
                                 COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, NMFS_REGION,
                                 MANAGEMENT_PROGRAM_CODE, TRAWL_EM),
                    
                    trips_with_factors %>%
                        distinct(OBSERVER_NAME, OBSERVER_SEQ, CALENDAR_YEAR, CRUISE, PERMIT, 
                                 HAUL_SEQ = NA, TRIP_SEQ, OFFLOAD_SEQ = NA,
                                 COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, NMFS_REGION,
                                 MANAGEMENT_PROGRAM_CODE, TRAWL_EM)
                    ),
              relationship = "many-to-many") %>%
  distinct()
   




################## #
# Save Output and upload to G-drive -------------------------------------------------------------

file_2_name  <- "AR_2_rolling_join_output.Rdata"

save(list = ls()[!(ls() == 'channel')],
     file = file_2_name)

gdrive_upload(file_2_name, AnnRpt_EnfChp_dribble)
