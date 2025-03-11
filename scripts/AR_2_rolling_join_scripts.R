####################
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


#################
# COMBINE factors, so each COMBINATION is unique.

#First, get all the factors by joining the ASSIGNMENTS data to the haul data for vessel observers and the offload data for plant observers.
# This gets each factor for every date that has a value in haul data or offload data for that date.
# Joining by permit and date ensures that all factors are joined even for 2nd observers, for each vessel or plant, and date.

assnmts_days_all_groupings <-   
  assignments_dates_cr_perm %>%
  filter(CALENDAR_YEAR <= as.numeric(adp_yr) & CALENDAR_YEAR >= as.numeric(adp_yr) -1 ) %>%
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, VESSEL_OR_PLANT, COVERAGE_TYPE) %>%  # remove duplicates; rows are unique by cruise, permit, date, and factor combination.
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
                  df_offloads %>%
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
  # next, join back to the offloads for VESSEL data, to fill in the missing factors
  # for any VESSEL days that do not have haul data, but have a "day" in the offload data, we can get the factors from that.
  # Use dummy column names to prevent joining on the FACTOR columns; 
  # Then only use the DUMMY if the FACTOR is null after the previous 2 steps.
  left_join(df_offloads %>%
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)



#Check for NA's
table(assnmts_days_all_groupings$VESSEL_TYPE, exclude = FALSE)







######################################

# Get missing dates: Non-fishing days, etc.  
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$VESSEL_TYPE, exclude = FALSE)



# Next use the offload data, to fill in gaps for non-fishing days, etc 
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE) 
dt_offloads <- copy(df_offloads)
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE,VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's
 table(assnmts_days_all_groupings$VESSEL_TYPE, exclude = FALSE)
 
test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE VESSEL_TYPE is null')

#############################



















#############################
#############################

# Next, for GEAR_TYPE.
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$GEAR_TYPE, exclude = FALSE)





# Next for Gear_type:
# using OFFLOADS
dt_t <- copy(assnmts_days_all_groupings )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_offloads)
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$GEAR_TYPE, exclude = FALSE)

test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE GEAR_TYPE is null')









#######################

# Next, for NMFS_REGION.
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)



# NMFS_REGION
# For VESSELS, from the OFFLOAD data
dt_t <- copy(assnmts_days_all_groupings %>% filter(VESSEL_TYPE != 'PLANT') )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_offloads)
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings_vessel_temp$CALENDAR_YEAR, assnmts_days_all_groupings_vessel_temp$NMFS_REGION, exclude = FALSE)







# NMFS_REGION
# For PLANTS, from the OFFLOAD data
dt_t <- copy(assnmts_days_all_groupings %>% filter(VESSEL_TYPE == 'PLANT') )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_offloads)
setDT(dt_offloads)
dt_offloads_n <- dt_offloads[, .(CALENDAR_YEAR, NMFS_REGION, PERMIT = PROCESSOR_PERMIT_ID, DEPLOYED_DATE = LANDING_DATE)]   # removing unneeded columns
setkey(dt_offloads_n, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)
dt_offloads_n <- unique(dt_offloads_n)  # removing duplicates
dt_offloads_n[, NMFS_REGION := as.character(NMFS_REGION)]  # coercing to character instead of factor

assnmts_days_all_groupings_plant_temp <- dt_offloads_n[dt_t, roll="nearest"] #now join the dates using a rolling join.
assnmts_days_all_groupings_plant_temp[, FINAL_N := ifelse(!is.na(i.NMFS_REGION), i.NMFS_REGION, NMFS_REGION)]  # Finally, use the actual join date if it exists; if not, use the rolling join date instead.  Using i.VESSEL_TYPE if present, otherwise using VESSEL_TYPE

assnmts_days_all_groupings_plant_temp <-
  assnmts_days_all_groupings_plant_temp %>%
  mutate(NMFS_REGION = FINAL_N) %>% 
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings_plant_temp$CALENDAR_YEAR, assnmts_days_all_groupings_plant_temp$NMFS_REGION, exclude = FALSE)



# Bind the VESSEL and PLANT df's together
assnmts_days_all_groupings <-
  rbind(assnmts_days_all_groupings_plant_temp,
        assnmts_days_all_groupings_vessel_temp
  )



#Check for NA's
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)
table(assnmts_days_all_groupings$GEAR_TYPE, assnmts_days_all_groupings$NMFS_REGION, exclude = FALSE)
table(assnmts_days_all_groupings$VESSEL_TYPE, assnmts_days_all_groupings$NMFS_REGION,   exclude = FALSE)



test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE NMFS_REGION is null')






#######################

# Next, MANAGEMENT_PROGRAM_CODE.
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)




# Using Offloads
# VESSELS
dt_t <- copy(assnmts_days_all_groupings %>% filter(VESSEL_TYPE != 'PLANT') )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_offloads)
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings_vessel_temp$CALENDAR_YEAR, assnmts_days_all_groupings_vessel_temp$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)



# Using Offloads
# PLANTS
dt_t <- copy(assnmts_days_all_groupings %>% filter(VESSEL_TYPE == 'PLANT') )
setDT(dt_t)
setkey(dt_t, CALENDAR_YEAR, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_offloads)
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)

#Check for NA's
table(assnmts_days_all_groupings_plant_temp$CALENDAR_YEAR, assnmts_days_all_groupings_plant_temp$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)




# Do it again for the remaining PLANT data (from df_offloads), with ONLY PERMIT (without CALENDAR_YEAR)
dt_t <- copy(assnmts_days_all_groupings_plant_temp )
setDT(dt_t)
setkey(dt_t, PERMIT, DEPLOYED_DATE)         
dt_offloads <- copy(df_offloads)
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
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)





# Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings_plant_temp$CALENDAR_YEAR, assnmts_days_all_groupings_plant_temp$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)


# Bind the vessel and plant rolling joints together.
assnmts_days_all_groupings <-
  rbind(assnmts_days_all_groupings_plant_temp %>%
          # hardcode the last of the missing values.  Do the required digging into the data..
          mutate(MANAGEMENT_PROGRAM_CODE = ifelse(is.na(MANAGEMENT_PROGRAM_CODE) & PERMIT == 5305, # Sand Point.  For whatever reason, these days aren't joining.  Likekly a date/time issue.
                                                 'OA',
                                                  as.character(MANAGEMENT_PROGRAM_CODE))
                 ),
        assnmts_days_all_groupings_vessel_temp) %>%
  distinct(CALENDAR_YEAR, OBSERVER_SEQ, CRUISE, PERMIT, DEPLOYED_DATE, COVERAGE_TYPE, VESSEL_TYPE, GEAR_TYPE, MANAGEMENT_PROGRAM_CODE, TRAWL_EM, NMFS_REGION)


#Check for NA's.  If none, then the rolling join worked correctly.
table(assnmts_days_all_groupings$CALENDAR_YEAR, assnmts_days_all_groupings$MANAGEMENT_PROGRAM_CODE, exclude = FALSE)

test_df <- 
  sqldf('SELECT * FROM assnmts_days_all_groupings WHERE MANAGEMENT_PROGRAM_CODE is null')


rm(assnmts_days_all_groupings_plant_temp, 
   assnmts_days_all_groupings_vessel_temp)











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
rm(dt_offloads_m, dt_offloads_n, dt_offloads_g, dt_offloads_v, dt_offloads, dt_hauls, dt_hauls_g, dt_hauls_m,
   dt_hauls_n, dt_hauls_v, nvl_test_df, test_df, dt_t)





##################
# Save Output and upload to G-drive -------------------------------------------------------------

file_2_name  <- "AR_2_rolling_join_output.Rdata"

save(list = ls()[!(ls() == 'channel')],
     file = file_2_name)

gdrive_upload(file_2_name, AnnRpt_EnfChp_dribble)
