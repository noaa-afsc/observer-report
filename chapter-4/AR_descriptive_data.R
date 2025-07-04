
# -------------------------------------------------------------------------------------------------------------------- #
#
# Program:AR_descriptive_data.Rmd                                         
# Project:Observer Program Annual Report Descriptive Chapter                                      
# Location: S:\Observer Program Annual Report\2024_Annual_Report\Chapt4_Descriptive_Info 
#      or: H:\Observer Program\Annual Report Local GIT Project\Descriptive Info
#
# Objectives:                                                                  
# - Query and perform some data clean-up for the descriptive chapter (Ch.4) of the Observer Program Annual Report
# - Generate catch table summaries that are posted to the AKRO website 
#   (https://www.fisheries.noaa.gov/alaska/fisheries-observers/observed-catch-tables-north-pacific-observer-program)
#
# Inputs:      
# Normal locations:
#  - S:\Observer Program Annual Report\Descriptive_Chapter\AR_descriptive_helper.r 
#      - fig.theme  (libraries, ggplot themes, etc.)
#  - AKRO Database
#      - akfish_sf.valhalla_scratch
#      - akfish_report.transaction_fact
#      - akfish_report.transaction_fact_log
#      - akfish_report.catch_report 
#      - akfish_report.catch_report_source 
#      - akfish_report.species_group 
#      - akfish_report.flag 
#      - akfish_report.mortality_rate 
#  - S:\Observer Program Annual Report\2024Annual_Report\Chap4_Descriptive_Info\2013_2023_catchtables.csv 
#  - H:\Observer Program\Annual Report Local GIT Project\Descriptive Info\Files for 2024 Annual Report\work.offload.rdata
#
# Output:    
# Normal locations:
#  - S:\Observer Program Annual Report\YEAR_Annual_Report\Chapt4_Descriptive_Info\AR_descriptive_YEAR_data.Rdata
#     - catch_tables - 2013-YEAR catch tables for posting to the AKRO website
#     - previous_catch_tables - previous year's version of the catch tables (2013-(YEAR-1))
#     - VALHALLA - the original Valhalla data
#     - warehouse_data - the mortality rates that were applied to the PSC data in the CAS run used in Valhalla's creation 
#     - addl_catch_table - summary of total catch of groundfish, directed halibut, and PSC halibut as observed or not observed
#     - work_data - Valhalla dataset following some clean-up and addition of DMRs for halibut PSC
#  - S:\Observer Program Annual Report\2024_Annual_Report\Chapt4_Descriptive_Info\2014_YEAR_catchtables.csv
#
# -------------------------------------------------------------------------------------------------------------------- #


## User Inputs -----------------------------------------------------------------------------------------------------

#Source helper file (libraries, ggplot themes, etc.)
source("AR_descriptive_helper.r")

#Create a generalized YEAR object that corresponds to the Annual Report year
YEAR <- 2024

# Set up ROracle connection for database calling information from R environment:
channel_cas <- dbConnect(drv = dbDriver('Oracle'), 
                         username =paste(Sys.getenv('CASid')), 
                         password =paste(Sys.getenv('CASpw')), 
                         dbname = Sys.getenv("myCASConnStr"))


## Load Data --------------------------------------------------------------------------------------------------------

# Query Valhalla data directly from the database for the Annual Report:
#valhalla_query <- paste0("select * from akfish_sf.valhalla where adp = ", YEAR)
#valhalla_data <- dbGetQuery(channel_cas, valhalla_query) 


# When Valhalla data aren't currently in the database.  Load .RData file instead:
load("G://FMGROUP//CADQ_library//observer_annual_reports_code//Valhalla Data//2024//2025-04-15cas_valhalla.RData")
valhalla_data <- valhalla  

# A specialized dataset of Trawl EM tendered trips that identifies which deliveries (by REPORT_ID) had salmon counts done shoreside:
load("H://Observer Program//Annual Report Local GIT Project//Descriptive Info//Files for 2024 Annual Report//work.offload.rdata")


## Valhalla data transformations  ----------------------------------------------------------------------------------- 


# Perform some data transformations:
prep_data <- valhalla_data %>% 
  # Create an CV_FULL flag for the CVs in full coverage (formerly called RPP):
  # (as they are in full coverage as opposed to other CVs that are in partial coverage
  #  previously only used for RPP... now use for PCTC and AFA CVs)
  mutate(CV_FULL = case_when((MANAGEMENT_PROGRAM_CODE == 'RPP'  & PROCESSING_SECTOR == 'S') ~ 'RPP',
                             (MANAGEMENT_PROGRAM_CODE == 'PCTC' & PROCESSING_SECTOR == 'S') ~ 'PCTC', 
                             (MANAGEMENT_PROGRAM_CODE == 'AFA'  & PROCESSING_SECTOR == 'S') ~ 'AFA', 
                              .default = '  ')) %>%
  # Identify the retained catch (source table = 'Y') as R and discards (source table = 'N') as D:
  mutate(RETAINED = ifelse(SOURCE_TABLE == 'Y', 'R', 'D')) %>% 
  # Aggregate the NPT and PTR trawl gears:
  mutate(COMBINED_GEAR = ifelse(AGENCY_GEAR_CODE %in% c('NPT', 'PTR'), 'TRW', AGENCY_GEAR_CODE)) %>% 
  # Aggregate species groups the way the Observer Program Annual Report has in the past:
  mutate(SPECIES_GROUP = ifelse(FMP=='GOA' & SPECIES_GROUP_CODE %in% c('REXS', 'FSOL', 'ARTH', 'DFL4'),'DFL4', SPECIES_GROUP_CODE)) %>% 
  mutate(SPECIES_GROUP = ifelse(FMP=='GOA' & SPECIES_GROUP_CODE %in% c('SQID', 'OCTP', 'SCLP', 'AMCK'), 'OTHR', SPECIES_GROUP)) %>% 
  mutate(SPECIES_GROUP = ifelse(FMP=='GOA' & SPECIES_GROUP_CODE %in% c('DUSK', 'REYE', 'THDS', 'POPA', 'DEM1', 'NORK', 'SRKR'), 'ROCK', SPECIES_GROUP)) %>% 
  mutate(SPECIES_GROUP = ifelse(FMP=='GOA' & SPECIES_GROUP_CODE %in% c('BSKT', 'USKT', 'LSKT'), 'USKT', SPECIES_GROUP)) %>% 
  mutate(SPECIES_GROUP = ifelse(FMP=='BSAI' & SPECIES_GROUP_CODE %in% c('AKPL', 'FLO5', 'YSOL', 'RSOL', 'FSOL'), 'FLAT', SPECIES_GROUP)) %>% 
  mutate(SPECIES_GROUP = ifelse(FMP=='BSAI' & SPECIES_GROUP_CODE %in% c('SQID', 'OCTP', 'SCLP'), 'OTHR', SPECIES_GROUP)) %>% 
  mutate(SPECIES_GROUP = ifelse(FMP=='BSAI' & SPECIES_GROUP_CODE %in% c('SRKR', 'POPA', 'NORK', 'REYE'), 'ROCK', SPECIES_GROUP)) %>% 
  mutate(SPECIES_GROUP = ifelse(FMP=='BSAI' & SPECIES_GROUP_CODE %in% c('KMKA', 'ARTH', 'GTRB'), 'TURB', SPECIES_GROUP)) %>%
  # Add a vessel length category for Ch.4 Table 4-1:
  mutate(VESSEL_LENGTH_CATEGORY = ifelse(as.numeric(LENGTH_OVERALL) <40, 'LT40', NA)) %>% 
  mutate(VESSEL_LENGTH_CATEGORY = ifelse((as.numeric(LENGTH_OVERALL) >= 40 & as.numeric(LENGTH_OVERALL) <57.5), 'BT40_57', VESSEL_LENGTH_CATEGORY)) %>% 
  mutate(VESSEL_LENGTH_CATEGORY = ifelse(as.numeric(LENGTH_OVERALL) >= 57.5, 'GT58', VESSEL_LENGTH_CATEGORY)) 



# Corrections to LENGTH_OVERALL and VESSEL_LENGTH_CATEGORY  ------------------------------------------------ 

# In recent years, a comparisons of the total vessel counts from Table 4-2 to Table 3-5 have identified discrepancies.  They have been a 
# result of vessel length changes mid-year on our side and where fishing occurred when the vessel was attributed to the different 
# vessel lengths and categories.  That's fine... it is just nice to footnote it in Table 4-2. 

# Make 3 changes here:
#    1) do a preemptive check for if there are vessels with fishing under different length categories
#    2) in 2024 there is a vessel (33391) where a vessel length change was made (36 ft to 43 ft) but a typo was entered (343 ft instead of 43 ft) 
#       and fishing occurred before 343 ft was corrected to 43 ft. In Valhalla, the vessel is attributed to 3 different length categories over the course
#       of the year.  Correct 343 to 43 so it is only in 2 categories.
#    3) in 2024 these is a vessel (33881) that changed length on 7/19/2024.  Trips before that date are ZERO coverage and after that date are OB_FIXED_GOA

# Make a vessel length and vessel length category correction for 2024:
prep_data <- prep_data %>% 
  mutate(LENGTH_OVERALL = case_when((VESSEL_ID == 33391 & LENGTH_OVERALL == 343) ~ 43,
                                    !(VESSEL_ID == 33391 & LENGTH_OVERALL == 343) ~ LENGTH_OVERALL),
         LENGTH_OVERALL = case_when((VESSEL_ID == 33881 & TRIP_TARGET_DATE >= as.Date("2024-07-19")) ~ 45,
                                    !(VESSEL_ID == 33881 & TRIP_TARGET_DATE >= as.Date("2024-07-19")) ~ LENGTH_OVERALL), 
         VESSEL_LENGTH_CATEGORY = case_when((VESSEL_ID == 33391 & VESSEL_LENGTH_CATEGORY == 'GT58') ~ 'BT40_57',
                                            !(VESSEL_ID == 33391 & VESSEL_LENGTH_CATEGORY == 'GT58') ~ VESSEL_LENGTH_CATEGORY),
         VESSEL_LENGTH_CATEGORY = case_when((VESSEL_ID == 33881 & TRIP_TARGET_DATE >= as.Date("2024-07-19")) ~ 'BT40_57',
                                            !(VESSEL_ID == 33881 & TRIP_TARGET_DATE >= as.Date("2024-07-19")) ~ VESSEL_LENGTH_CATEGORY))  
  

      # Identify vessels with more than 1 vessel length category:
      vessel_length_counts <- prep_data %>% 
        group_by(VESSEL_ID) %>% 
        summarize(length_count = n_distinct(VESSEL_LENGTH_CATEGORY)) %>%  
        filter(length_count > 1)
      
      vessel_length_counts   # NOTE: This count of vessels will need to be referenced in a footnote to Table 4-2.  Not currently coded.  Will have to be hand edited.
      
      # Look to see When those vessel length categories valid:
      length_changes <- prep_data %>% 
        filter(VESSEL_ID %in% vessel_length_counts$VESSEL_ID) %>% 
        group_by(VESSEL_ID, LENGTH_OVERALL, VESSEL_LENGTH_CATEGORY) %>% 
        summarize(start = min(TRIP_TARGET_DATE),
                  end = max(TRIP_TARGET_DATE), .groups = 'drop') %>% 
        arrange(VESSEL_ID, start)
      
      length_changes

      # Evaluate if these changes are valid or if corrections are needed.

      
# Corrections to strata ---------------------------------------------------------------------------- 

# Hardcode the following STRATA changes here:
#  1. The 2024 ADP indicates that NO vessels are participating in the EM Innovation Project in 2024. So no strata changes
#     for this. (https://www.fisheries.noaa.gov/s3//2023-11/Final-2024-ADP.pdf) 
#  2. The Trawl EM EFP strata are already split into BSAI and GOA strata... so this code no longer needed in 2024
#  3. In 2024, vessel 33881 changed from ZERO (<40) to OBS_FIXED_GOA mid-year

table(prep_data$STRATA)
# 4/28 counts:
#EM_FIXED_BSAI  EM_FIXED_GOA   EM_TRW_BSAI    EM_TRW_GOA          FULL OB_FIXED_BSAI  OB_FIXED_GOA   OB_TRW_BSAI    OB_TRW_GOA          ZERO 
#         4584         66192         49177         15067        834396         19207        127089          1187         13666         76310 

# Create an ORIGINAL_STRATA value and changes some of the STRATA values for the EM Research Zero pool and EM TRW EFP:
prep_data <- prep_data %>% 
#  rename(ORIGINAL_STRATA = STRATA) %>% 
#  mutate(STRATA = #ifelse(VESSEL_ID %in% c('5029', '1472'), 'ZERO_EM_RESEARCH',  # removed for 2021: , '3759' ,'2844'
#                         ifelse(ORIGINAL_STRATA == 'EM_TRW_EFP' & FMP == 'BSAI', 'EM_TRW_EFP_FULL', 
#                                ifelse(ORIGINAL_STRATA == 'EM_TRW_EFP' & FMP == 'GOA',  'EM_TRW_EFP_PART', ORIGINAL_STRATA)))#)
   mutate(STRATA = case_when((VESSEL_ID == 33881 & TRIP_TARGET_DATE >= as.Date("2024-07-19")) ~ 'OB_FIXED_GOA',
                             !(VESSEL_ID == 33881 & TRIP_TARGET_DATE >= as.Date("2024-07-19")) ~ STRATA))

table(prep_data$STRATA)
# EM_FIXED_BSAI  EM_FIXED_GOA   EM_TRW_BSAI    EM_TRW_GOA          FULL OB_FIXED_BSAI  OB_FIXED_GOA   OB_TRW_BSAI    OB_TRW_GOA          ZERO 
#          4584         66192         49177         15067        834396         19207        127182          1187         13666         76217 

      
      

# Back calculate halibut PSC estimates using mortality rates -------------------------------------------------------------

# From the annual report:
#       "DMRs are not applied to raw observer data prior to expansion to the entire fishery. Therefore, in order
#       to present observed and unobserved catch, the data in this chapter are presented without DMRs. As
#       such, these data represent total catch - not total mortality; it is important to recognize that not all of the
#       halibut that were discarded would have died."  (2016 Annual Report, p.87)

# In the data warehouse the species_weight column (and therefore Valhalla's weight_posted column) have had 
# the discard mortality rate applied to halibut PSC. (NOTE: wastage in the halibut fishery has had a DMR of 1 applied which is the 
# same as NOT having a DMR applied)

# In order to remain consistent with previous versions of the annual report, the Ch. 4 catch tables need to include
# halibut PSC estimates rather than halibut mortality: 
#     - For 2017 PSC estimates were added to a version of Valhalla from v_cas_psc_estimate (in Cathy_valhalla_3_14_17.RData).  
#     - For 2018 onward the mortality rates from the warehouse are needed to back calculate the estimates from the mortality
#       (NOTE: you cannot simply use the DMRs in akfish.core_halibut_mortality_rate because some discards occur through deck sorting
#              and have their own haul or vessel specific DMR applied)


# Identify the Valhalla run date - so its CAS run can be identified and halibut mortality rates obtained:
valhalla_run_date <- valhalla_data %>% 
  mutate(RUNDATE = format(RUN_DATE, '%d-%b-%Y')) %>% 
  distinct(RUNDATE)

valhalla_run_date
valhalla_run_date$RUNDATE <- '15-APR-2025'
  
  
# Using the run date from Valhalla, query the data warehouse to get the CAS run used in Valhalla's creation 
# and get the mortality rates that were applied to the PSC:
warehouse_query <- paste0("WITH cas_run AS
(-- Identify the CAS run that was likely used to create Valhalla (a.k.a. the last successful CAS run before the Valhalla run date)
SELECT * FROM 
(-- Get all the CAS runs for the year that happened before the creation of Valhalla:
SELECT *
FROM akfish_report.transaction_fact_log a
WHERE a.year = ", YEAR, 
" AND a.status = 'SUCCESS'
AND a.end_time <= '", valhalla_run_date$RUNDATE, 
"' ORDER BY a.end_time desc
)
WHERE rownum = 1 
)

SELECT distinct crs.catch_report_source_pk, -- identified as ca_reference_key in 2019 Valhalla dataset
cr.data_source_type_code,
crs.catch_report_type_code,
fmp.area_code AS fmp,
sg.species_group_code, 
txn.mortality_rate,
r.value AS source_table 
FROM akfish_report.transaction_fact txn
JOIN akfish_report.transaction_fact_log tfl ON txn.transaction_fact_log_pk = tfl.transaction_fact_log_pk  
JOIN cas_run cas ON tfl.cas_run_log_pk = cas.cas_run_log_pk 
JOIN akfish_report.catch_report cr ON txn.catch_report_pk = cr.catch_report_pk 
JOIN akfish_report.catch_report_source crs ON cr.catch_report_source_pk = crs.catch_report_source_pk 
JOIN akfish_report.species_group sg ON txn.species_group_pk = sg.species_group_pk 
JOIN akfish_report.flag r ON txn.retained_flag_pk = r.flag_pk 
JOIN akfish_report.area fmp ON txn.fmp_area_pk = fmp.area_pk
WHERE tfl.year = ", YEAR,
" AND sg.species_group_code = 'HLBT'
AND r.value = 'N' -- discarded")

warehouse_data <- dbGetQuery(channel_cas, warehouse_query) 


# The Valhalla data have the old primary key column name for catch reports (ca_reference_key) and the data warehouse data
# have the new primary key (catch_report_source_pk). Rename it, for compatibility sake:

warehouse_data <- warehouse_data %>% 
  rename(CA_REFERENCE_KEY = CATCH_REPORT_SOURCE_PK)


# Merge the datasets so the DMR can be used to back calculate the estimate:
apply_dmr <- prep_data %>% 
  left_join(warehouse_data, 
            by = c("CA_REFERENCE_KEY", "SPECIES_GROUP_CODE", "SOURCE_TABLE", "DATA_SOURCE_TYPE_CODE", "CATCH_REPORT_TYPE_CODE", "FMP")) %>% 
  # Calculate the estimate from the DMR and the mortality for PSC:
  mutate(HALIBUT_WEIGHT_NOMORT = ifelse(!is.na(MORTALITY_RATE), as.numeric(WEIGHT_POSTED)/as.numeric(MORTALITY_RATE), NA))


# ---- Review halibut data and the new halibut_weight_nomort field just to make sure I understand which to use for the Ch4 catch tables:
        halibut <- apply_dmr[apply_dmr$SPECIES_GROUP == 'HLBT',]

        # Halibut records without a HALIBUT_WEIGHT_NOMORT value are SUPPOSED TO BE retained catch:   Use weight_posted for these records.
        retained.halibut <- halibut[is.na(halibut$HALIBUT_WEIGHT_NOMORT),]
        table(retained.halibut$RETAINED)  # these results should ALL be R!!  If not, investigate! 

        # Halibut records with a HALIBUT_WEIGHT_NOMORT value are discarded catch:  Use halibut_weight_nomort for these records.
        discarded.halibut <- halibut[!is.na(halibut$HALIBUT_WEIGHT_NOMORT),]
        table(discarded.halibut$RETAINED)

        discarded.halibut <- discarded.halibut %>% 
          mutate(difference = as.numeric(WEIGHT_POSTED) - HALIBUT_WEIGHT_NOMORT)

        # Verify that when discarded halibut is wastage, there is no difference between the weight posted and the weight 
        # with no DMRs applied:
        table(discarded.halibut[discarded.halibut$GROUNDFISH_FLAG=='Y',]$difference)
# ---------------------------------------------------------------------------------------------------------------------------- #

        
# Specify that retained halibut (and all other species groups) should use weight_posted whereas discarded halibut (PSC and wastage)
# should use the halibut_weight_nomort field for the Ch.4 catch tables:
apply_dmr <- apply_dmr %>% 
   mutate(catch_table_weight = ifelse((SPECIES_GROUP == 'HLBT' & RETAINED == 'D'), 
                                      HALIBUT_WEIGHT_NOMORT, as.numeric(WEIGHT_POSTED)))

# Identify the working dataset:        
work_data <- apply_dmr         


# Corrections to OBSERVED_FLAG  ---------------------------------------------------------------------------- 

table(work_data$OBSERVED_FLAG)
#     N      Y 
#279916 926959 

# Hardcode the following changes to 3 trips here (2020 remnant... none so far for 2021, 2022, or 2023):
#prep_data <- prep_data %>% 
#  rename(ORIGINAL_OBSERVED_FLAG = OBSERVED_FLAG) %>% 
#  mutate(OBSERVED_FLAG = ifelse(TRIP_ID %in% c('28207362.0', '28207273.0', '28207833.0') & ORIGINAL_OBSERVED_FLAG == 'N', 'Y', ORIGINAL_OBSERVED_FLAG))


# Address how deployment occurs for Trawl EM -- by delivery -- for shoreside sampling.  This is new code for 2024, drafted based on 
# code provided by Geoff Mayhew (trawl_em_report_id_obs_fix.R)

#' `work.offload` has a column, `OBS_SALMON_CNT_FLAG`, which we create from the shoreside observer data to determine
#' whether the REPORT_ID was monitored by a shoreside observer. We want to use this to replace the existing 
#' `OBSERVED_FLAG`, which we understand will be flipped to 'Y' if ANY REPORT_ID within a CV's tender trip was monitored.
#' We also have the `T_REPORT_ID` column, which we use as an identifier that groups up all REPORT_IDs in the same
#' shoreside delivery by a tender vessel. That is, `OBS_SALMON_CNT` is either 'Y' or 'N' for each T_REPORT_ID to tell us
#' which tender delivery was monitored, and then `OBSERVED_FLAG` is fine to use for any other remaining records which 
#' were non-tendered trips.

# Keep the original column order
val.col <- colnames(work_data)

# Get unique REPORT_ID, T_REPORT_ID, and OBS_SALMON_CNT_FLAG
report_id.obs <- work.offload %>% 
  distinct(REPORT_ID, T_REPORT_ID, OBS_SALMON_CNT_FLAG)

# Merge then into Valhalla (currently as work_data). I made a new object called valhalla.new for the sake of comparison, but is not needed
valhalla.new <- merge(work_data, report_id.obs, by = "REPORT_ID", all = T)

valhalla.new %>% filter(OBS_SALMON_CNT_FLAG != OBSERVED_FLAG)


# Re-code `TRIP_ID` and `OBSERVED_FLAG` to take into account deliveries in the TEM strata: 
revised_work_data <- valhalla.new %>%
  mutate(TRIP_ID = case_when(!STRATA %in% c('EM_TRW_BSAI', 'EM_TRW_GOA') ~ as.character(TRIP_ID),
                              STRATA %in% c('EM_TRW_BSAI', 'EM_TRW_GOA') & is.na(T_REPORT_ID) ~ as.character(REPORT_ID),   # When there isn't an Offload ID, use the Report ID instead of the Trip ID
                              STRATA %in% c('EM_TRW_BSAI', 'EM_TRW_GOA') & !is.na(T_REPORT_ID) ~ T_REPORT_ID),             # When there IS an Offload ID use it.   
         OBSERVED_FLAG = case_when(is.na(OBS_SALMON_CNT_FLAG) ~ OBSERVED_FLAG,
                                  !is.na(OBS_SALMON_CNT_FLAG) ~ OBS_SALMON_CNT_FLAG)) %>%
  select(all_of(val.col)) 


# Had 690 'TRIP_IDs' before, now at 806. That is, we have 690 CV trips and 806 shoreside deliveries
work_data %>% filter(STRATA == "EM_TRW_GOA") %>% summarize(length(unique(TRIP_ID)))
revised_work_data %>% filter(STRATA == "EM_TRW_GOA") %>% summarize(length(unique(TRIP_ID)))

# Had 1,712 'TRIP_IDs' before, now at 1,725. That is, we have 1,712 CV trips and 1,725 shoreside deliveries
work_data %>% filter(STRATA == "EM_TRW_BSAI") %>% summarize(length(unique(TRIP_ID)))
revised_work_data %>% filter(STRATA == "EM_TRW_BSAI") %>% summarize(length(unique(TRIP_ID)))


# By monitoring status: 33.7% of trips, 36.0% of deliveries.
work_data %>% filter(STRATA == "EM_TRW_GOA") %>% group_by(OBSERVED_FLAG) %>% summarize(Trips = length(unique(TRIP_ID))) 
revised_work_data %>% filter(STRATA == "EM_TRW_GOA") %>% group_by(OBSERVED_FLAG) %>% summarize(Deliveries = length(unique(TRIP_ID)))




# Summarize Catch for Catch Tables --------------------------------------------------------------------------------


# ------- Summarize Total Catch of Groundfish, Directed Halibut, and PSC halibut (observed + not observed): --------- #
calc_total <- revised_work_data %>% 
  # refine to groundfish (which includes directed halibut) OR psc halibut:
  filter(GROUNDFISH_FLAG == 'Y' | (PSC_FLAG == 'Y' & SPECIES_GROUP_CODE == 'HLBT')) %>% 
  group_by(FMP, PROCESSING_SECTOR, CV_FULL, AGENCY_GEAR_CODE, SPECIES_GROUP, RETAINED) %>% 
  summarise(TONS = sum(catch_table_weight), .groups = 'drop') %>% 
  # add 'Total' as a filler value in the used for estimation flag:
  mutate(OBSERVED_FLAG = 'Total') %>% 
  data.frame()

# ------- Summarize Catch of Groundfish, Directed Halibut, and PSC halibut as observed or not observed --------- #
observed <- revised_work_data %>% 
  # refine to groundfish (which includes directed halibut) OR psc halibut:
  filter(GROUNDFISH_FLAG == 'Y' | (PSC_FLAG == 'Y' & SPECIES_GROUP_CODE == 'HLBT')) %>% 
  mutate(OBSERVED_FLAG = case_when(OBSERVED_FLAG == 'Y' ~ 'Observed',
                                   OBSERVED_FLAG == 'N' ~ 'Unobserved')) %>% 
  group_by(FMP, PROCESSING_SECTOR, CV_FULL, OBSERVED_FLAG, AGENCY_GEAR_CODE, SPECIES_GROUP, RETAINED) %>% 
  summarise(TONS = sum(catch_table_weight), .groups = 'drop') %>% 
  data.frame()


# Combine the observed catch and total catch summaries:
with_totals <- rbind(observed, calc_total[,c(1,2,3,5,6,7,8,4)])


# Create a new catch table time series -------------------------------------------------------------------------- 

# Read in previous catch table:
previous_catch_table <- read.csv(paste0("2013_", YEAR-1, "_catchtables.csv"))

previous_catch_table <- previous_catch_table %>% 
  rename(OBSERVED_FLAG = OBS_FOR_EST,
         CV_FULL = RPP)

# Format the new year's catch table data:
addl_catch_table <- with_totals %>% 
  # Remove unobserved catch:
  filter(OBSERVED_FLAG %in% c('Observed', 'Total')) %>% 
  # Add a year column:
  mutate(YEAR = YEAR) %>%
  # Arrange the columns so it matches previous catch tables:
  select(YEAR, FMP, PROCESSING_SECTOR, AGENCY_GEAR_CODE, CV_FULL, SPECIES_GROUP, RETAINED, OBSERVED_FLAG, TONS)


# Combine the previous catch table with the new year's catch table:
catch_tables <- rbind(previous_catch_table, addl_catch_table)


# Format new catch table time series for web -------------------------------------------------

export_format <- catch_tables %>% 
  # Provide some 'pretty' translations:
  mutate(SECTOR = case_when((CV_FULL == 'RPP' & PROCESSING_SECTOR == 'S') ~ 'Catcher Vessel: Rockfish Program',
                            (CV_FULL == 'AFA' & PROCESSING_SECTOR == 'S') ~ 'Catcher Vessel: AFA',
                            (CV_FULL == 'PCTC'& PROCESSING_SECTOR == 'S') ~ 'Catcher Vessel: PCTC',
                             PROCESSING_SECTOR == 'CP' ~ 'Catcher/Processor',
                             PROCESSING_SECTOR == 'M' ~ 'Mothership', 
                             .default = 'Catcher Vessel')) %>%
  mutate(GEAR = ifelse(AGENCY_GEAR_CODE == 'HAL', 'Hook and Line', NA)) %>% 
  mutate(GEAR = ifelse(AGENCY_GEAR_CODE == 'NPT', 'Nonpelagic Trawl', GEAR)) %>% 
  mutate(GEAR = ifelse(AGENCY_GEAR_CODE == 'PTR', 'Pelagic Trawl', GEAR)) %>% 
  mutate(GEAR = ifelse(AGENCY_GEAR_CODE %in% c('JIG', 'POT'), str_to_sentence(AGENCY_GEAR_CODE), GEAR)) %>% 
  mutate(DISPOSITION = ifelse(RETAINED == 'R', 'Retained', NA)) %>% 
  mutate(DISPOSITION = ifelse(RETAINED == 'D', 'Discarded', DISPOSITION)) %>% 
  mutate(MONITORED_OR_TOTAL = ifelse(YEAR > 2017 & OBSERVED_FLAG == 'Observed', 'Monitored', OBSERVED_FLAG)) %>%
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'AMCK', 'Atka Mackerel', NA)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'DFL4', 'Deep-water Flatfish (GOA)', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'FLAT', 'Flatfish (BSAI)', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'HLBT', 'Pacific Halibut', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'OTHR', 'Other Groundfish', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'PCOD', 'Pacific Cod', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'PLCK', 'Walleye Pollock', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'ROCK', 'Rockfish', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'SABL', 'Sablefish (Black Cod)', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'SFL1', 'Shallow-water Flatfish (GOA)', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'TURB', 'Turbot', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'USKT', 'Skates', SPECIES_GROUP_NAME)) %>% 
  mutate(SPECIES_GROUP_NAME = ifelse(SPECIES_GROUP == 'USRK', 'Sharks', SPECIES_GROUP_NAME)) %>% 
  # Aggregate across the new translations:
  group_by(YEAR, FMP, SECTOR, GEAR, SPECIES_GROUP_NAME, DISPOSITION, MONITORED_OR_TOTAL) %>% 
  summarize(METRIC_TONS = sum(TONS), .groups = 'drop') %>%
  # Sort:
  arrange(YEAR, FMP, SECTOR, GEAR, SPECIES_GROUP_NAME, DISPOSITION, MONITORED_OR_TOTAL)


# Export new 2013-YEAR catch tables as a raw csv:
#write.csv(catch_tables,paste0("2013_", YEAR, "_catchtables.csv"), row.names = FALSE)

# Export new formatted 2013-YEAR catch tables as csv:
#write.csv(export_format,paste0("2013_", YEAR, "_catchtables_formatted.csv"), row.names = FALSE)


# Clean up workspace and save RData file --------------------------------------------------------------


# Clean up workspace (removes everything EXCEPT the objects listed) and save RData
rm(list= ls()[!(ls() %in% c('YEAR', 'valhalla_data', 'warehouse_data', 'work_data', 'revised_work_data', 'previous_catch_table', 
                             'addl_catch_table', 'catch_tables'))])

save(YEAR, valhalla_data, warehouse_data, work_data, revised_work_data, previous_catch_table, addl_catch_table, catch_tables, 
     file = paste0("AR_descriptive_", YEAR, "_data.RData"))
