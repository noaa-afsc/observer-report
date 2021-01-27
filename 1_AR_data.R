# Get packages and user-defined functions ---------------------------------
source("functions/3_helper.R")

# User inputs -------------------------------------------------------------

# Random number seed
set.seed(052870)

# Report year (year that fishing and observing took place)
year <- 2020 

# The user's physical location when running this code (used to pull data from the closest database)
location <- toupper(getPass('What is your current physical location? (Juneau or Seattle)'))

# ADP inputs --------------------------------------------------------------

# Loads two objects: efrt and efrt_adpyear
# efrt contains 3 years of effort data used for the ADP
# efrt_adpyear contains the effort predictions for each domain and the 1 year of trips used to simulate effort
load("data/effort_prediction.rdata")

# Remove the efrt object
rm(efrt)

# ADP outputs -------------------------------------------------------------
adp_out <- readRDS("data/fin_a2020_i5000_s12345.rds")

# Get data ----------------------------------------------------------------
channel_afsc  <- channel.fxn(location)
channel_akro  <- channel.fxn(location, db="AKRO") # Hit cancel unless sitting in Juneau and pulling Valhalla.

# * Valhalla ----
# Pull in this report year's Valhalla
load("data/2021-01-25CAS_VALHALLA.RData")
work.data <- VALHALLA
rm(VALHALLA)

#Summary of coverage by strata and processing sector
#This is a check to make sure no entries look wonky
table(work.data$COVERAGE_TYPE, work.data$STRATA, work.data$PROCESSING_SECTOR, useNA='always')

# Data check for observed vessels under 40 ft., should be zero rows
work.data %>% 
filter(LENGTH_OVERALL < 40 & OBSERVED_FLAG == "Y") %>% 
select(VESSEL_ID, TRIP_ID, REPORT_ID, COVERAGE_TYPE, STRATA, OBSERVED_FLAG, TRIP_TARGET_CODE, REPORTING_AREA_CODE, LANDING_DATE, LENGTH_OVERALL) %>% 
distinct() %>% 
arrange(VESSEL_ID)

# Change date format to eliminate times and match what is in the database
work.data <- mutate(work.data, TRIP_TARGET_DATE = as.Date(TRIP_TARGET_DATE), LANDING_DATE = as.Date(LANDING_DATE))

# Add ADP year to all TRIP_IDs if there are duplicates
if(nrow(select(work.data, TRIP_ID, ADP) %>% 
        distinct() %>% 
        group_by(TRIP_ID) %>% 
        filter(n()>1) %>% 
        data.frame()) > 0){
  work.data$TRIP_ID <- paste(work.data$ADP, work.data$TRIP_ID, sep = ".")}

# b. Salmon dockside monitoring ----

# Queries
script <- paste0("
SELECT o.landing_report_id AS REPORT_ID,
CASE WHEN oco.employer_code = 'AIS' THEN 'PARTIAL' ELSE 'FULL' END AS OBS_COVERAGE_TYPE
FROM atl_offload o
JOIN atl_salmon s
ON o.cruise = s.cruise
AND o.permit = s.permit
AND o.offload_seq = s.offload_seq
JOIN ols_observer_cruise ocr
ON ocr.cruise = o.cruise
JOIN ols_observer_contract oco
ON oco.contract_number = ocr.contract_number
WHERE o.delivery_end_date BETWEEN '01-JAN-", year, "' AND '31-DEC-", year, "'
ORDER BY OBS_COVERAGE_TYPE, REPORT_ID")

#The following query returns all landing ID's for offloads monitored for salmon all sectors.
salmon.landings.obs <- dbGetQuery(channel_afsc, script)

# Data checks and clean up

#Number of offloads monitored for salmon by Observer Coverage Type (Full vs Partial)
salmon.landings.obs  %>%  group_by(OBS_COVERAGE_TYPE) %>% summarise(n=n())

# c. ODDS ----

# Queries
script <- paste("
SELECT
norpac.ODDS_RANDOM_NUMBER.getControlPct(pl.original_embark_date, vtr.trip_rate_strata)  as ODDS_SELECTION_PCT,
EXTRACT(YEAR FROM pl.original_embark_date) AS YEAR,      
vtr.sample_plan_seq,
--vtr.gear_type_code, --Change #1 for 2018 Annual Report
-- vtr.tender_trip_flag, --Change #2 for 2018 Annual Report
-- vtr.trip_rate_strata, 
vtr.description AS STRATA,
lov.akr_vessel_id AS VESSEL_ID,
lov.vessel_seq,
pl.trip_plan_log_seq,
pl.gear_type_code, --Change #3 for 2018 Annual Report
pl.tender_trip_flag, --Change #4 for 2018 Annual Report
pl.original_embark_date,
pl.planned_embark_date,
pl.trip_status_code,
pl.trip_obs_code,
pl.trip_selected_obs,
pl.inherit_obs_trip_seq,
mon.random_number_used,
pl.trip_plan_number,
-- This line converts the CLOB format of this field into VARCHAR

dbms_lob.substr(tw.request_comment,4000) AS WAIVER_DESCRIPTION,
(SELECT ws.description 
FROM odds_lov_waiver_status ws
WHERE tw.waiver_status_seq = ws.waiver_status_seq) AS WAIVER_TYPE
                
FROM      
norpac.odds_vessel_to_rate_strata vtr ,   
norpac.atl_lov_vessel             lov,
norpac.odds_trip_plan_log         pl   
-- monitor record doesnt exist for BSAI or early EM ( dont need random number for 100% or 0% selection)
                
LEFT OUTER JOIN
norpac.odds_monitor mon   
ON pl.trip_plan_log_seq  =  mon.trip_plan_log_seq 
                AND mon.random_number_used IS NOT NULL 
                
LEFT OUTER JOIN
norpac.odds_trip_waiver tw    
ON pl.trip_plan_log_seq  =  tw.trip_plan_log_seq 
WHERE pl.sample_plan_seq = vtr.sample_plan_seq
AND NVL(pl.gear_type_code, 0) =  NVL( vtr.gear_type_code ,  NVL(pl.gear_type_code, 0))                  
AND NVL(pl.tender_trip_flag, 0) = NVL( vtr.tender_trip_flag , NVL(pl.tender_trip_flag, 0))               
AND pl.vessel_seq = lov.vessel_seq 
AND EXTRACT(YEAR FROM pl.original_embark_date) =", year)

odds.dat <- dbGetQuery(channel_afsc, script)


# Data checks and clean up

# Check for duplicates - should be no records (= 0)
sum(duplicated(odds.dat$TRIP_PLAN_LOG_SEQ)) 

# Database check - Should be no records
length(odds.dat[!is.na(odds.dat$TRIP_SELECTED_OBS) & odds.dat$TRIP_STATUS_CODE=="CN"]) 

# This confirms the check
table(odds.dat$TRIP_SELECTED_OBS, odds.dat$TRIP_STATUS_CODE, useNA='always')  

# Summary of trip dispositions and observer assignments 
# Sample plan sequences: 11 = gear + tender, 13 = EM selected for review, 
# 98 = EM not selected for review where IFQ fishing was to occur in more than one NMFS area.
# Trip status codes: CS	= Cancel by System, PD = Pending, CN = Cancelled, CP = Completed, CC = Cancel Cascaded
table(odds.dat$TRIP_OBS_CODE, odds.dat$TRIP_STATUS_CODE, odds.dat$SAMPLE_PLAN_SEQ, useNA='ifany')

# The trip_selected_obs = "Y" when sample_plan_seq = 98 means that coverage was requested (due 
# to fishing occuring in more than one area) but the random number generated was larger than the 
# programmed rate, and so the video was not selected for review. Since these trips aren't truly
# monitored, make trip_selected_obs = "N". Also change strata to include gear_type_code = 6 (pot)
# and gear_type_code = 8 (longline).
odds.dat <- odds.dat %>% 
            mutate(TRIP_SELECTED_OBS=ifelse(SAMPLE_PLAN_SEQ==98, "N", TRIP_SELECTED_OBS),
                   STRATA=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==6 & TENDER_TRIP_FLAG=="N", paste0(STRATA, " - POT - No Tender"), STRATA),
                   STRATA=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==6 & TENDER_TRIP_FLAG=="Y", paste0(STRATA, " - POT - Tender"), STRATA),
                   STRATA=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==8 & TENDER_TRIP_FLAG=="N", paste0(STRATA, " - HAL - No Tender"), STRATA),
                   STRATA=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==8 & TENDER_TRIP_FLAG=="Y", paste0(STRATA, " - HAL - Tender"), STRATA))

# d. EM ----
script <- paste0("SELECT * from em_pac_review.EM_TRIP
                  WHERE EXTRACT(YEAR FROM TRIP_END_DATE_TIME) = ", year)

EM.data <- dbGetQuery(channel_afsc, script)


# In the EM data the field EM_VESSEL means either ADFG NUMBER OR COAST GUARD NUMBER
# Get the translation to permit
script <- paste0("SELECT DISTINCT et.vessel_id as EM_VESSEL_ID,
                 a.permit as VESSEL_ID,
                 a.adfg_number,
                 a.coast_guard_number
                 FROM em_pac_review.EM_TRIP et
                 LEFT JOIN norpac.atl_lov_vessel a
                 ON ltrim(et.vessel_id, 0) = a.adfg_number  
                 -- ltrim removes preceding zeros in the former to match to the latter
                 OR ltrim(et.vessel_id , 0)   = a.coast_guard_number")

transform.EM.data.vessel <- dbGetQuery(channel_afsc, script)


#Fix EM.data VESSEL_ID
EM.data <- rename(EM.data, PS_VESSEL_ID = VESSEL_ID)

EM.data <- merge(EM.data, transform.EM.data.vessel, 
                 by.x = "PS_VESSEL_ID",
                 by.y = "EM_VESSEL_ID", all.x = TRUE)

rm(transform.EM.data.vessel)

# Get gear type for EM data
script <- paste0("SELECT * from em_pac_review.EM_FISHING_EVENT
                  WHERE EXTRACT(YEAR FROM END_DATE_TIME) = ", year)

EM.gear <- dbGetQuery(channel_afsc, script)


#Select data for this report year and recode gear type to those used by CAS
EM.gear <- select(EM.gear, TRIP_NUMBER, GEAR_TYPE_ID) %>% 
           distinct() %>% 
           arrange(TRIP_NUMBER) %>% 
           mutate(AGENCY_GEAR_CODE=ifelse(GEAR_TYPE_ID==6 | GEAR_TYPE_ID==7, "HAL", "NA"),
                  AGENCY_GEAR_CODE=ifelse(GEAR_TYPE_ID==10 | GEAR_TYPE_ID==11, "POT", AGENCY_GEAR_CODE)) %>%
           select(TRIP_NUMBER, AGENCY_GEAR_CODE) %>%  
           distinct()

# Join gear types to EM data
EM.data <- left_join(EM.data, EM.gear, by="TRIP_NUMBER")

# Are there NAs in EM.data$AGENCY_GEAR_CODE?
EM.data %>% group_by(AGENCY_GEAR_CODE) %>% summarise(n=n_distinct(TRIP_NUMBER))

# Isolate VESSEL_IDs for NAs in EM.data$AGENCY_GEAR_CODE
gear_na_vessels <- filter(EM.data, is.na(AGENCY_GEAR_CODE)) %>% distinct(VESSEL_ID) %>% unlist() %>% as.vector()

# Isolate VESSEL_IDs with NAs in EM.data$AGENCY_GEAR_CODE 
# that logged trips of only one gear type in ODDS
single_gear_nas <- filter(odds.dat, VESSEL_ID %in% gear_na_vessels) %>% 
                   distinct(VESSEL_ID, GEAR_TYPE_CODE,STRATA) %>% 
                   arrange(VESSEL_ID, GEAR_TYPE_CODE) %>% 
                   group_by(VESSEL_ID) %>% 
                   filter(uniqueN(GEAR_TYPE_CODE) == 1) %>% 
                   mutate(AGENCY_GEAR_CODE=ifelse(GEAR_TYPE_CODE==8, "HAL", NA)) %>% 
                   mutate(AGENCY_GEAR_CODE=ifelse(GEAR_TYPE_CODE==6, "POT", AGENCY_GEAR_CODE)) %>% 
                   distinct(VESSEL_ID, AGENCY_GEAR_CODE)

# Isolate VESSEL_IDs with NAs in EM.data$AGENCY_GEAR_CODE 
# that logged trips of more than one gear type in ODDS
multiple_gear_nas <- filter(odds.dat, VESSEL_ID %in% gear_na_vessels) %>% 
                     distinct(VESSEL_ID, GEAR_TYPE_CODE) %>% 
                     arrange(VESSEL_ID, GEAR_TYPE_CODE) %>% 
                     group_by(VESSEL_ID) %>% 
                     filter(uniqueN(GEAR_TYPE_CODE) > 1)

# Compare ODDS to EM.data for VESSEL_IDs with NAs in EM.data$AGENCY_GEAR_CODE
# to determine the most likely AGENCY_GEAR_CODE
filter(odds.dat, VESSEL_ID %in% multiple_gear_nas$VESSEL_ID) %>% 
  distinct(VESSEL_ID, PLANNED_EMBARK_DATE, GEAR_TYPE_CODE, STRATA) %>% 
  arrange(VESSEL_ID, PLANNED_EMBARK_DATE, GEAR_TYPE_CODE, STRATA)

filter(EM.data, VESSEL_ID %in% multiple_gear_nas$VESSEL_ID) %>% 
  distinct(TRIP_NUMBER, VESSEL_ID, TRIP_START_DATE_TIME, AGENCY_GEAR_CODE) %>% 
  arrange(VESSEL_ID, TRIP_START_DATE_TIME)

# Fix NAs in AGENCY_GEAR_CODE for EM.data
EM.data <- EM.data %>% 
           mutate(AGENCY_GEAR_CODE=ifelse(VESSEL_ID %in% single_gear_nas$VESSEL_ID[AGENCY_GEAR_CODE=="HAL"] & is.na(AGENCY_GEAR_CODE), "HAL", AGENCY_GEAR_CODE)) %>% 
           mutate(AGENCY_GEAR_CODE=ifelse(VESSEL_ID %in% single_gear_nas$VESSEL_ID[AGENCY_GEAR_CODE=="POT"] & is.na(AGENCY_GEAR_CODE), "POT", AGENCY_GEAR_CODE)) %>% 
           mutate(AGENCY_GEAR_CODE = ifelse(TRIP_NUMBER %in% c("19_POLARSTAR04.03", "19_MARILYNJ01.01", "19_ALEUTIANISLE08.01"), "POT", AGENCY_GEAR_CODE))


# The following query will provide a list of em selectecd trips and if they have been reviewed or not
# Query will only include trips in completed or pending status and will not include compliance trips.
# This query will also show the declared gear type and if reviewed, will show the em_reviewed_gear_type_code
# This query will also show when the HD was received by PSFMC and when the EM reviewed data was exported and sent to AFSC
# This query will also show the actual em trip start date and time and actual em trip end date and time which comes from the data on the HD.

# Important note, this query is setup to pull a specific year of data.  That year is based on when the ODDS declared trip start date is.  
# To change the year find the declared year in the below sub-query (2 places) and change it to the year wanted.

# Also an important note, if an EM reviewed trip used multiple gear types on a trip (ie.  pot and longline) there will be 2 records in the output.

script <- paste(
  "select all_data.*, em_rev_gear.em_gear_code, 
  hd_data.date_hd_received_by_psmfc,
  hd_data.date_exported_to_afsc,
  hd_data.actual_em_trip_start_date_time,
  hd_data.actual_em_trip_end_date_time from (
  select 
  
  case
  when em_reviewed.trip_plan_log_seq is not null
  then 'YES'
  else 'NO'
  End as EM_DATA_REVIEWED,   
  em_reviewed.trip_number as em_reviewed_trip_number, 
  em_selected.*
  from    
  
  ---- from ODDS get the list of selected EM trips that are in completed or pending status
  ---- and are not compliance monitoring trips
  ( select distinct
  
  e.odds_trip_number,
  d.name as vessel_name,
  d.adfg_number as vessel_adfg_number,
  trunc(e.date_logged) as date_logged,
  e.GEAR_TYPE as gear_type_logged,
  trunc(e.declared_leave_date) as declared_trip_start_date,
  e.declared_port_of_departure as declared_trip_start_port,
  trunc(e.declared_return_date) as declared_trip_end_date,
  e.declared_plant_offloading_to as declared_trip_end_port,
  trip_status,
  i.user_reqst_coverage
  
  from norpac.odds_provider a,                   
  norpac.odds_em_vessel_request f,
  norpac.odds_eligible_opt_strata g,
  norpac.odds_vessel_sample_plan c,
  norpac.atl_lov_vessel d,
  norpac.odds_logged_trip_summary_v e,
  norpac.odds_em_request_status h,
  norpac.odds_monitor i
  where f.eligible_opt_seq = g.eligible_opt_seq
  and g.vessel_seq = d.vessel_seq
  and c.vessel_seq = d.vessel_seq
  and d.permit = e.akr_vessel_id
  and e.odds_trip_number = i.trip_plan_log_seq
  and e.year = h.status_year   --- need this to make sure we don't get duplicates
  and e.observer_status_code <> 'NO'
  and e.year = ", year,"
  and h.status_year = ", year,"
  and i.sample_plan_seq in (13))em_selected
  
  left join
  
  ----get the em reviewed trip number for those em trips that have been reviwed and where AFSC has the data
  
  (select a.trip_plan_log_seq, a.trip_number 
  from em_pac_review.em_trip a
  )em_reviewed
  
  on em_selected.ODDS_TRIP_NUMBER = em_reviewed.trip_plan_log_seq  
  
  
  AND em_selected.trip_status in ('COMPLETED', 'PENDING')  
  
  order by  em_selected.odds_trip_number asc)all_data
  
  left join 
  
  ---- the below query will get the em gear code from the em data
  
  (select em_rev_gear.* from 
  (select em_gear_code.trip_number,
  em_gear_code.gear_type_code as em_gear_code
  from
  (select a.trip_number, b.gear_type_code
  from EM_PAC_REVIEW.EM_FISHING_EVENT a,
  em_pac_review.em_gear_type_lov b
  where a.gear_Type_id = b.gear_type_id
  group by a.trip_number, b.gear_type_code)em_gear_code
  group by em_gear_code.trip_number, em_gear_code.gear_type_code)em_rev_gear)em_rev_gear
  
  on em_rev_gear.trip_number = all_data.em_reviewed_trip_number
  
  left join 
  
  --- the below query will get the when the HD was received by psmfc, when it was exported to AFSC and the em trip start date and time and trip end date and time
  
  (select a.trip_number, 
  b.date_received_by_psmfc as date_hd_received_by_psmfc,
  a.file_import_date as date_exported_to_afsc, 
  c.trip_plan_log_seq, 
  c.trip_start_date_time as actual_em_trip_start_date_time, 
  c.trip_end_date_time as actual_em_trip_end_date_time,
  d.planned_embark_date as declared_embark_date,
  d.planned_disembark_date as declared_end_date
  from em_pac_review.em_trip_hard_drive a,
  em_pac_review.em_hard_drive b,
  em_pac_review.em_trip c,
  norpac.odds_trip_plan_log d
  where a.HARD_DRIVE_NUMBER = b.HARD_DRIVE_NUMBER
  and c.TRIP_PLAN_LOG_SEQ = d.TRIP_PLAN_LOG_SEQ
  and a.trip_number = c.trip_number)hd_data
  
  on hd_data.trip_number = all_data.em_reviewed_trip_number"
)

EM.review <- dbGetQuery(channel_afsc, script)
  

# Flip pending trips to completed if they have data reviewed
# For clarification, see email from Glenn Campbell on 3/11/20
EM.review$TRIP_STATUS[EM.review$EM_DATA_REVIEWED == "YES"] <- "COMPLETED"

# e. Shapefiles ----
## Load land and NMFS stat area shapefiles 
## (".." points the directory back one level)
shp_land      <- st_read("../shapefiles/P3_AK_All.shp")
shp_nmfs      <- st_read("../shapefiles/NMFS_Zones_Clean.shp")
shp_centroids <- st_read("../shapefiles/NMFS_Area_Centroid.shp")

# III. Rename strata ----

# Rename strata for brevity and consistency
to_draw <- mutate(to_draw, STRATA_NEW = recode(STRATA_NEW, 
                                                   "HAL" = "HAL",
                                                   "POT" = "POT - No Tender",
                                                   "POT_TENDER" = "POT - Tender",
                                                   "TRW_TENDER" = "TRW - Tender",
                                                   "TRW" = "TRW - No Tender"))

draw_from <- mutate(draw_from, STRATA_NEW = recode(STRATA_NEW, 
                                             "HAL" = "HAL",
                                             "POT" = "POT - No Tender",
                                             "POT_TENDER" = "POT - Tender",
                                             "TRW_TENDER" = "TRW - Tender",
                                             "TRW" = "TRW - No Tender"))

sample_pops_out <- mutate(sample_pops_out, STRATA_NEW = recode(STRATA_NEW, 
                                                   "HAL" = "HAL",
                                                   "POT" = "POT - No Tender",
                                                   "POT_TENDER" = "POT - Tender",
                                                   "TRW_TENDER" = "TRW - Tender",
                                                   "TRW" = "TRW - No Tender"))

odds.dat <- mutate(odds.dat, STRATA = recode(STRATA, 
                                             "Declared Gear & Tender Delivery - Pot No  Tender Delivery" = "POT - No Tender",    
                                             "Declared Gear & Tender Delivery - Pot   Tender Delivery" = "POT - Tender",      
                                             "Declared Gear & Tender Delivery - Trawl  No Tender Delivery" = "TRW - No Tender",  
                                             "EM Declared Gear & Tender - Longline No Tender Delivery" = "EM HAL",      
                                             "Declared Gear & Tender Delivery - Longline No Tender Delivery" = "HAL",
                                             "EM Declared Gear & Tender - Pot gear - No Tender Delivery" = "EM POT",    
                                             "Declared Gear & Tender Delivery - Trawl  Tender Delivery" = "TRW - Tender",    
                                             "EM Compliance Monitoring - Fishing IFQ mulitple Areas - HAL - No Tender" = "EM Compliance HAL",
                                             "Declared Gear & Tender Delivery - Longline  Tender Delivery" = "HAL",
                                             "EM Declared Gear & Tender -  Pot gear Tender delivery" = "EM POT",
                                             "EM Declared Gear & Tender - Longline Tender Delivery" = "EM HAL",
                                             "EM Compliance Monitoring - Fishing IFQ mulitple Areas - POT - No Tender" = "EM Compliance POT"))

work.data <- mutate(work.data, STRATA=ifelse(STRATA=="EM_TenP", "EM_POT", STRATA))

work.data <- mutate(work.data, STRATA = recode(STRATA, 
                                               "HAL" = "HAL",
                                               "TRW" = "TRW - No Tender",
                                               "FULL" = "FULL", 
                                               "ZERO" = "ZERO", 
                                               "POT" = "POT - No Tender",
                                               "TenTR" = "TRW - Tender", 
                                               "EM_POT" = "EM POT",
                                               "TenP" = "POT - Tender",
                                               "EM_HAL" = "EM HAL"))

# Add the Predator (2844) to the EMResearch list used in the 2019 ADP (per Farron's email on 12/2/2018)
work.data <- work.data %>% 
             mutate(STRATA = ifelse(VESSEL_ID %in% c("5029", "3759", "1472", "2844"), "Zero EM Research", STRATA))

# Look up table for strata in partial coverage category

## Observed and EM partial coverage strata
partial <- data.frame(STRATA = c("HAL", "POT - No Tender", "POT - Tender", "TRW - No Tender", "TRW - Tender", "EM HAL", "EM POT"), 
                      Rate = c(0.1771, 0.1543, 0.1611, 0.2370, 0.2712, 0.3000, 0.3000),
                      descriptions = c("hook-and-line gear", "non-tendered pot gear", "tendered pot gear", "non-tendered trawl gear", "tendered trawl gear", "hook-and-line gear with electronic monitoring", "pot gear with electronic monitoring")) %>% 
           mutate(formatted_strat = paste0("*", STRATA, "*"),
                  txt = paste0(formatC(round(Rate*100, 2), format='f', digits=2), '% in the ', formatted_strat, ' stratum'))


# IV. Save ----

# Remove any remaining unwanted objects and save data
rm(location, channel_afsc, channel_akro)

# Save (save.session saves loaded packages in addition to objects)
save.image(file = "2_AR_data.Rdata")
