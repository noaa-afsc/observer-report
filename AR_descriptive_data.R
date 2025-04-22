
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


## Load Valhalla Data ---------------------------------------------------------------------------------------------

# Query Valhalla data directly from the database for the Annual Report:
#valhalla_query <- paste0("select * from akfish_sf.valhalla where adp = ", YEAR)
#valhalla_data <- dbGetQuery(channel_cas, valhalla_query) 


# When Valhalla data aren't currently in the database.  Load .RData file instead:
load("G://FMGROUP//CADQ_library//observer_annual_reports_code//Valhalla Data//2024//2025-04-03cas_valhalla.RData")
valhalla_data <- valhalla  


## Valhalla data transformations  ----------------------------------------------------------------------------------- 


# Perform some data transformations:
prep_data <- valhalla_data %>% 
  # Create an RPP management program flag for the CVs:
  # (as they are in full coverage as opposed to other CVs that are in partial coverage)
  mutate(RPP = ifelse(MANAGEMENT_PROGRAM_CODE=='RPP' & PROCESSING_SECTOR=='S', 'RPP', ' ')) %>% 
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
  mutate(VESSEL_LENGTH_CATEGORY = ifelse(as.numeric(LENGTH_OVERALL) >= 57.5, 'GT58', VESSEL_LENGTH_CATEGORY)) %>% 
  # Add a flag indicating if the observed data are used in CAS for estimation or not:
  #    (In the past, this flag would have distinguished the EM_POT data from other observed trips)
  mutate(OBS_FOR_EST = ifelse(OBSERVED_FLAG == 'Y', 'Observed', 'Not Observed'))



# Corrections to strata ---------------------------------------------------------------------------- 

# Hardcode the following STRATA changes here:
#  1. The 2024 ADP indicates that NO vessels are participating in the EM Innovation Project in 2024. So no strata changes
#     for this. (https://www.fisheries.noaa.gov/s3//2023-11/Final-2024-ADP.pdf) 
#  2. The Trawl EM EFP strata are already split into BSAI and GOA strata... so this code no longer needed in 2024

table(prep_data$STRATA)
# 4/11 counts:
#EM_FIXED_BSAI  EM_FIXED_GOA   EM_TRW_BSAI    EM_TRW_GOA          FULL OB_FIXED_BSAI  OB_FIXED_GOA   OB_TRW_BSAI    OB_TRW_GOA          ZERO 
#         4595         66181         49177         15067        834396         19186        127110          1187         13666         76555 

# Create an ORIGINAL_STRATA value and changes some of the STRATA values for the EM Research Zero pool and EM TRW EFP:
#prep_data <- prep_data %>% 
#  rename(ORIGINAL_STRATA = STRATA) %>% 
#  mutate(STRATA = #ifelse(VESSEL_ID %in% c('5029', '1472'), 'ZERO_EM_RESEARCH',  # removed for 2021: , '3759' ,'2844'
#                         ifelse(ORIGINAL_STRATA == 'EM_TRW_EFP' & FMP == 'BSAI', 'EM_TRW_EFP_FULL', 
#                                ifelse(ORIGINAL_STRATA == 'EM_TRW_EFP' & FMP == 'GOA',  'EM_TRW_EFP_PART', ORIGINAL_STRATA)))#)


#table(prep_data$ORIGINAL_STRATA, prep_data$STRATA)



# Corrections to OBSERVED_FLAG  ---------------------------------------------------------------------------- 

table(prep_data$OBSERVED_FLAG)
#     N      Y 
#280161 926959

# Hardcode the following changes to 3 trips here (2020 remnant... none so far for 2021):
#prep_data <- prep_data %>% 
#  rename(ORIGINAL_OBSERVED_FLAG = OBSERVED_FLAG) %>% 
#  mutate(OBSERVED_FLAG = ifelse(TRIP_ID %in% c('28207362.0', '28207273.0', '28207833.0') & ORIGINAL_OBSERVED_FLAG == 'N', 'Y', ORIGINAL_OBSERVED_FLAG))

#table(prep_data$OBSERVED_FLAG)


#table(prep_data$ORIGINAL_OBSERVED_FLAG, prep_data$OBSERVED_FLAG)




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
valhalla_run_date$RUNDATE <- '04-APR-2025'
  
  
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



# Summarize Catch for Catch Tables --------------------------------------------------------------------------------


# ------- Summarize Total Catch of Groundfish, Directed Halibut, and PSC halibut (observed + not observed): --------- #
calc_total <- work_data %>% 
  # refine to groundfish (which includes directed halibut) OR psc halibut:
  filter(GROUNDFISH_FLAG == 'Y' | (PSC_FLAG == 'Y' & SPECIES_GROUP_CODE == 'HLBT')) %>% 
  group_by(FMP, PROCESSING_SECTOR, RPP, AGENCY_GEAR_CODE, SPECIES_GROUP, RETAINED) %>% 
  summarise(TONS = sum(catch_table_weight), .groups = 'drop') %>% 
  # add 'Total' as a filler value in the used for estimation flag:
  mutate(OBS_FOR_EST = 'Total') %>% 
  #mutate(OBSERVED_FLAG = 'Total') %>% 
  data.frame()

# ------- Summarize Catch of Groundfish, Directed Halibut, and PSC halibut as observed or not observed --------- #
#           in the context of whether or not the observer data would be used for catch estimation                #
observed <- work_data %>% 
  # refine to groundfish (which includes directed halibut) OR psc halibut:
  filter(GROUNDFISH_FLAG == 'Y' | (PSC_FLAG == 'Y' & SPECIES_GROUP_CODE == 'HLBT')) %>% 
  group_by(FMP, PROCESSING_SECTOR, RPP, OBS_FOR_EST, AGENCY_GEAR_CODE, SPECIES_GROUP, RETAINED) %>% 
  #group_by(FMP, PROCESSING_SECTOR, RPP, OBSERVED_FLAG, AGENCY_GEAR_CODE, SPECIES_GROUP, RETAINED) %>% 
  summarise(TONS = sum(catch_table_weight), .groups = 'drop') %>% 
  data.frame()


# Combine the observed catch and total catch summaries:
with_totals <- rbind(observed, calc_total[,c(1,2,3,5,6,7,8,4)])


# Create a new catch table time series -------------------------------------------------------------------------- 

# Read in previous catch table:
previous_catch_table <- read.csv(paste0("2013_", YEAR-1, "_catchtables.csv"))

# Format the new year's catch table data:
addl_catch_table <- with_totals %>% 
  # Remove unobserved catch:
  filter(OBS_FOR_EST %in% c('Observed', 'Total')) %>% 
  # Add a year column:
  mutate(YEAR = YEAR) %>%
  # Arrange the columns so it matches previous catch tables:
  select(YEAR, FMP, PROCESSING_SECTOR, AGENCY_GEAR_CODE, RPP, SPECIES_GROUP, RETAINED, OBS_FOR_EST, TONS)


# Combine the previous catch table with the new year's catch table:
catch_tables <- rbind(previous_catch_table, addl_catch_table)


# Format new catch table time series for web -------------------------------------------------

export_format <- catch_tables %>% 
  # Provide some 'pretty' translations:
  mutate(SECTOR = ifelse(RPP == 'RPP' & PROCESSING_SECTOR == 'S', 'Catcher Vessel: Rockfish Program', NA)) %>% 
  mutate(SECTOR = ifelse(RPP != 'RPP' & PROCESSING_SECTOR == 'S', 'Catcher Vessel', SECTOR)) %>% 
  mutate(SECTOR = ifelse(PROCESSING_SECTOR == 'CP', 'Catcher/Processor', SECTOR)) %>% 
  mutate(SECTOR = ifelse(PROCESSING_SECTOR == 'M', 'Mothership', SECTOR)) %>% 
  mutate(GEAR = ifelse(AGENCY_GEAR_CODE == 'HAL', 'Hook and Line', NA)) %>% 
  mutate(GEAR = ifelse(AGENCY_GEAR_CODE == 'NPT', 'Nonpelagic Trawl', GEAR)) %>% 
  mutate(GEAR = ifelse(AGENCY_GEAR_CODE == 'PTR', 'Pelagic Trawl', GEAR)) %>% 
  mutate(GEAR = ifelse(AGENCY_GEAR_CODE %in% c('JIG', 'POT'), str_to_sentence(AGENCY_GEAR_CODE), GEAR)) %>% 
  mutate(DISPOSITION = ifelse(RETAINED == 'R', 'Retained', NA)) %>% 
  mutate(DISPOSITION = ifelse(RETAINED == 'D', 'Discarded', DISPOSITION)) %>% 
  mutate(MONITORED_OR_TOTAL = ifelse(YEAR > 2017 & OBS_FOR_EST == 'Observed', 'Monitored', OBS_FOR_EST)) %>%
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


# Read in TEM Video Review Data --------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------------#
# The 2023 Council motion says:
#   "Future reports include data on the amount of catch monitored by electronic monitoring (EM) similar to data on 
#     observed catch"
#
# My notes from June that says: this is referring to a table that summarizes catch in the TEM strata that was monitored by:
#   1) sampled shoreside
#   2) video was on for complaince review
#   3) video was actually reviewed for complance
# 
# ------------------------------------------------------------------------------------------------------------------------------ #

# This identifies when video review has been matched to logbooks:
# tem_video_query <- paste0("SELECT
#                           ps.year_pk AS year,
#                           ps.landing_report_id,
#                           'Y' AS video_review,
#                           'Y' AS video_review_logbook,
#                           ps.upload_account
#                           FROM
#                           akfish_report.v_trawl_em_pacstates_received ps
#                           WHERE
#                           ps.year_pk = 2023
#                           AND ps.expire_date IS NULL -- get current records  
#                           AND ps.report_type_code = 'TRIP' -- means video review?
#                           
#                           UNION ALL
#                           
#                           SELECT
#                           sw.year_pk AS year,
#                           sw.landing_report_id,
#                           'Y' AS video_review,
#                           'Y' AS video_review_logbook,
#                           sw.upload_account
#                           FROM
#                           akfish_report.v_trawl_em_saltwater_received sw
#                           WHERE sw.year_pk = 2023
#                           AND sw.expire_date IS NULL
#                           AND sw.report_type_code = 'TRIP'")
# tem_video <- dbGetQuery(channel_cas, tem_video_query) 
# 
# 
# # Identify video review that doesn't match to logbooks:
# tem_video_query2 <- paste0("SELECT distinct 
#                            pso.year_pk AS year,
#                            pso.landing_report_id,
#                            'Y' AS video_review,
#                            'Y' AS video_review_nologbook,
#                            pso.upload_account
#                            FROM
#                            akfish_report.v_trawl_em_pacstates_outstanding pso
#                            WHERE pso.year_pk = 2023
#                            AND pso.expire_date IS NULL
#                            AND pso.status = 'MISSING LOGBOOK'
#                            
#                            UNION ALL
#                            
#                            SELECT distinct 
#                            swo.year_pk AS year,
#                            swo.landing_report_id,
#                            'Y' AS video_review,
#                            'Y' AS video_review_nologbook,
#                            swo.upload_account
#                            FROM
#                            akfish_report.v_trawl_em_saltwater_outstanding swo
#                            WHERE swo.year_pk = 2023
#                            AND swo.expire_date IS NULL
#                            AND swo.status = 'MISSING LOGBOOK'")
# 
# tem_video_nologbook <- dbGetQuery(channel_cas, tem_video_query2) 
# 
# 
# 
# # Identifies logbooks that don't have matching video review:
# tem_novideo_query <- paste0("SELECT distinct
#                             pso.year_pk AS year,
#                             pso.landing_report_id,
#                             'N' AS video_review,
#                             pso.upload_account
#                             FROM
#                             akfish_report.v_trawl_em_pacstates_outstanding pso
#                             WHERE pso.year_pk = 2023
#                             AND pso.expire_date IS NULL
#                             AND pso.status = 'MISSING VIDEO REVIEW'
#                             
#                             UNION ALL
#                             
#                             SELECT distinct 
#                             swo.year_pk AS year,
#                             swo.landing_report_id,
#                             'N' AS video_review,
#                             swo.upload_account
#                             FROM
#                             akfish_report.v_trawl_em_saltwater_outstanding swo
#                             WHERE swo.year_pk = 2023
#                             AND swo.expire_date IS NULL
#                             AND swo.status = 'MISSING VIDEO REVIEW'")
# tem_novideo <- dbGetQuery(channel_cas, tem_novideo_query) 
# 
# 
# # Add Video review information to Valhalla dataset:
# valhalla_tem <- work_data %>% 
#   filter(ORIGINAL_STRATA == 'EM_TRW_EFP') %>% 
#   # refine to groundfish (which includes directed halibut) OR psc halibut:
#   filter(GROUNDFISH_FLAG == 'Y' | (PSC_FLAG == 'Y' & SPECIES_GROUP_CODE == 'HLBT')) %>% 
#   # Rename the Observed Flag the Shoreside sampling flag:
#   rename(SHORESIDE_SAMPLING = OBSERVED_FLAG) %>% 
#   # Append info on hard drives that have been reviewed (and have matching logbook data):
#   left_join(tem_video, by = c("REPORT_ID" = "LANDING_REPORT_ID")) %>% 
#   # Append info on hard drives that have been reviewed (but DON'T have matching logbook data):
#   left_join(tem_video_nologbook, by = c("REPORT_ID" = "LANDING_REPORT_ID")) %>% 
#   # Append info on video review that is still outstanding:
#   #left_join(tem_novideo, by = c("REPORT_ID" = "LANDING_REPORT_ID")) %>%
#   # Consolidate some of the fields:
#   mutate(preVIDEO_REVIEW = ifelse(is.na(VIDEO_REVIEW.x), VIDEO_REVIEW.y, VIDEO_REVIEW.x),
#          VIDEO_REVIEW = ifelse(is.na(preVIDEO_REVIEW), 'N', 'Y'),  
#          preVIDEO_REVIEWER = ifelse(is.na(UPLOAD_ACCOUNT.x), UPLOAD_ACCOUNT.y, UPLOAD_ACCOUNT.x),
#          VIDEO_REVIEWER = ifelse(is.na(preVIDEO_REVIEWER), 'n/a', preVIDEO_REVIEWER),
#          VIDEO_REVIEW_LB = ifelse(is.na(VIDEO_REVIEW_LOGBOOK), 'N', VIDEO_REVIEW_LOGBOOK),
#          VIDEO_REVIEW_NOLB = ifelse(is.na(VIDEO_REVIEW_NOLOGBOOK), 'N', VIDEO_REVIEW_NOLOGBOOK)) %>% 
#   # drop some fields:
#   select(-c(ends_with(".x"), ends_with(".y"))) %>% 
#   data.frame()


# Clean up workspace and save RData file --------------------------------------------------------------


# Clean up workspace (removes everything EXCEPT the objects listed) and save RData
rm(list= ls()[!(ls() %in% c('YEAR', 'valhalla_data', 'warehouse_data', 'work_data', 'previous_catch_table', 
                             'addl_catch_table', 'catch_tables'))])

save(YEAR, valhalla_data, warehouse_data, work_data, previous_catch_table, addl_catch_table, catch_tables, 
     file = paste0("AR_descriptive_", YEAR, "_data.RData"))
