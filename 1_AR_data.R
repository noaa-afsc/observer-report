# Setup -------------------------------------------------------------------

# Get packages and user-defined functions
source("3_helper.R")

# Random number seed
set.seed(052870)

# Report year (year that fishing and observing took place)
year <- 2024

# The user's physical location when running this code (used to pull data from the closest database)
location <- toupper(getPass('What is your current physical location? (Juneau or Seattle)'))

# Establish database connections
channel_afsc  <- channel.fxn(location)
channel_akro  <- channel.fxn(location, db="AKRO") # Hit cancel unless sitting in Juneau and pulling Valhalla.

# Get data ----------------------------------------------------------------

# To make this script run, ensure that the following files are within a folder titled 'data' within the main repo:
# effort_prediction.rdata (created in the most recent final ADP project)
# fin_a2020_i5000_s12345.rds (created in the most recent final ADP project)
# These files can be found here: https://drive.google.com/drive/u/0/folders/1Mf628Jvb_TaeL2zN2wdSbiZ8h62YbS3R

# * ADP inputs ----

# Loads two objects: efrt and efrt_adpyear
# efrt contains 3 years of effort data used for the ADP
# efrt_adpyear contains the effort predictions for each domain and the 1 year of trips used to simulate effort
load("data/effort_prediction.rdata")

# Remove the efrt object
rm(efrt)

# * ADP outputs ----

# Output from the days paid function, which sets the budget
load("data/dp_res.rdata")

# Output from simulations
adp_out <- readRDS("data/fin_a2020_i5000_s12345.rds")

# * Valhalla ----
# Pull in this report year's Valhalla
# The code that creates Valhalla is maintained separately from this project
script <- paste0("select * 
                  from loki.akr_valhalla_scratch_v")

work.data <- dbGetQuery(channel_afsc, script)

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

# Check for TRIP_IDs that are repeated across ADP years
if(nrow(select(work.data, TRIP_ID, ADP) %>% 
        distinct() %>% 
        group_by(TRIP_ID) %>% 
        filter(n()>1) %>% 
        data.frame()) > 0){

# If there are duplicate TRIP_IDs across ADP years, add ADP year to the front of *all* TRIP_IDs
work.data$TRIP_ID <- paste(work.data$ADP, work.data$TRIP_ID, sep = ".")}

# * Salmon dockside monitoring ----

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

# * ODDS ----

# Queries
script <- paste("
  SELECT 
    -- Retrieve a trip's selection pecentage based on the trip's original declared embark date and stratum
    odds.ODDS_RANDOM_NUMBER.getControlPct(a.original_embark_date, b.strata) as ODDS_SELECTION_PCT,     

    a.trip_plan_log_seq, a.trip_status_code, a.vessel_seq, EXTRACT(YEAR FROM a.original_embark_date) AS YEAR,
    a.original_embark_date, a.planned_embark_date, a.tender_trip_flag, a.trip_plan_number,
    b.trip_stratas_seq, b.trip_monitor_code, b.trip_selected,  b.random_number_used, b.strata AS STRATA_CODE, 
    b.inherit_trip_seq,
    c.group_code, c.description AS STRATUM_DESCRIPTION,
    d.release_comment,
    e.description AS RELEASE_STATUS_DESCRIPTION,
    f.akr_vessel_id AS VESSEL_ID,
    g.gear_type_code
   
  FROM odds.odds_trip_plan_log a
    LEFT JOIN odds.odds_trip_stratas b
      ON a.trip_plan_log_seq = b.trip_plan_log_seq 
    LEFT JOIN odds.odds_strata c
      ON b.strata = c.strata
    LEFT JOIN odds.odds_strata_release d
      ON b.trip_stratas_seq = d.trip_stratas_seq
    LEFT JOIN odds.odds_lov_release_status e
      ON d.release_status_seq = e.release_status_seq
    LEFT JOIN norpac.atl_lov_vessel f
      ON a.vessel_seq = f.vessel_seq
    LEFT JOIN odds.odds_trip_gear g
      ON a.trip_plan_log_seq = g.trip_plan_log_seq
  WHERE EXTRACT(YEAR FROM a.original_embark_date) IN (", paste(year + -2:-1, collapse = ","), ")
    -- [2023 Annual report Only]
    -- Exclude a 2024 trip with original embark date in 2023 that makes the ODDS_RANDOM_NUMBER package throw an error.
    AND a.trip_plan_log_seq != 202328923             
")

odds.dat <- dbGetQuery(channel_afsc, script)

# Data checks and clean up

# Check for duplicates - should be no records (= 0)
sum(duplicated(odds.dat$TRIP_PLAN_LOG_SEQ)) 

# Database check - Should be no records where TRIP_SELECTED is NA and trip was cancelled
nrow(odds.dat[!is.na(odds.dat$TRIP_SELECTED) & odds.dat$TRIP_STATUS_CODE == "CN", ]) 

# This confirms the check
table(odds.dat$TRIP_SELECTED, odds.dat$TRIP_STATUS_CODE, useNA = 'always')  

#'*==============================================*
#' FIXME * I FAIL THIS CHECK *

if(F){
  
  
  # Try pulling data since 2020 for comparison
  since_2020 <- paste(
  "
    SELECT 
    
      -- Retrieve a trip's selection pecentage based on the trip's original declared embark date and stratum
      odds.ODDS_RANDOM_NUMBER.getControlPct(a.original_embark_date, b.strata) as ODDS_SELECTION_PCT,     
      
      a.trip_plan_log_seq, a.trip_status_code, a.vessel_seq, EXTRACT(YEAR FROM a.original_embark_date) AS YEAR,
      a.original_embark_date, a.planned_embark_date, a.tender_trip_flag,
      b.trip_stratas_seq, b.trip_monitor_code, b.trip_selected,  b.random_number_used, b.strata, b.inherit_trip_seq,
      c.group_code, c.gear_typecode, c.description AS STRATUM_DESCRIPTION,
      d.release_comment,
      e.description AS RELEASE_STATUS_DESCRIPTION,
      f.akr_vessel_id AS VESSEL_ID
     
    FROM odds.odds_trip_plan_log a
      LEFT JOIN odds.odds_trip_stratas b
        ON a.trip_plan_log_seq = b.trip_plan_log_seq 
      LEFT JOIN odds.odds_strata c
        ON b.strata = c.strata
      LEFT JOIN odds.odds_strata_release d
        ON b.trip_stratas_seq = d.trip_stratas_seq
      LEFT JOIN odds.odds_lov_release_status e
        ON d.release_status_seq = e.release_status_seq
      LEFT JOIN norpac.atl_lov_vessel f
        ON a.vessel_seq = f.vessel_seq
    WHERE EXTRACT(YEAR FROM a.original_embark_date) IN (", paste(year + -4:-1, collapse = ","), ")
      -- Just to make the code run for now until the package is fixed to deal with 2023 trip that should be in 2024
      AND a.trip_plan_log_seq != 202328923             
    "
  )
  
  test <- setDT(dbGetQuery(channel_afsc, since_2020))
  
  # To look at TRIP_STATUS_CODE == CN cases
  data.table::dcast(
    test[, .N, keyby = .(YEAR, TRIP_SELECTED, TRIP_STATUS_CODE, TRIP_MONITOR_CODE)],
    TRIP_STATUS_CODE + TRIP_SELECTED + TRIP_MONITOR_CODE ~ YEAR, value.var = "N", fill = 0
  )[TRIP_STATUS_CODE == "CN"]
  # In the past, all CN trips had NA for TRIP_SELECTED. They still might have had OA or TRIP_MONITOR_CODE?
  
  #' TRIP_PLAN_LOG_SEQ = 145603 was logged *2022*-01-22, original embark of *2022*-02-22, cancelled *2023*-01-02
  what <- paste(test[YEAR == 2022 & TRIP_STATUS_CODE == "CN" & !is.na(TRIP_SELECTED), TRIP_PLAN_LOG_SEQ], collapse = ",")
  what <- setDT(dbGetQuery(channel_afsc, paste(
    "SELECT * FROM odds.odds_trip_plan_log WHERE TRIP_PLAN_LOG_SEQ IN(", what, ")"
  )))
  range(what[, CANCEL_DATE_TIME])  
  #' *ALL of these trips were cancelled on or after Dec 14 2022, the day ODDS 3.0 went live!*
  #' *Is it our wish to make ODDS 3.0 match the old data, re-work our analyses, or make the old match the new?*
}
#'*==============================================*

# Summary of trip dispositions and observer assignments 
# GROUP_CODE: 10 = at-sea observer, 13 = fixed gear EM, 14 = EM TRW EFP
# Trip status codes: CS	= Cancel by System, PD = Pending, CN = Cancelled, CP = Completed, CC = Cancel Cascaded
table(odds.dat$TRIP_MONITOR_CODE, odds.dat$TRIP_STATUS_CODE, odds.dat$GROUP_CODE, useNA = 'ifany')

# The TRIP_SELECTED = "Y" when STRATA_CODE = 96 or 98 means that coverage was requested (due 
# to fishing occurring in more than one area) but the random number generated was larger than the 
# programmed rate, and so the video was not selected for review. Since these trips aren't truly
# monitored, make TRIP_SELECTED = "N". 96 is used for at-sea observer compliance trips and 98 is used
# for at-sea fixed gear EM trips.
odds.dat <- mutate(odds.dat, TRIP_SELECTED = ifelse(STRATA_CODE %in% c(96, 98), "N", TRIP_SELECTED))

# Translate GROUP_CODE, STRATA_CODE, and GEAR_TYPE_CODE into STRATA
odds.dat %>% select(GROUP_CODE, STRATA_CODE, GEAR_TYPE_CODE, STRATUM_DESCRIPTION) %>% distinct() %>% arrange(GROUP_CODE, STRATA_CODE)
odds.dat <- mutate(odds.dat, STRATA = paste0(
  # Tag on "compliance" if the trip was a multi-area IFQ trip
  ifelse(STRATA_CODE %in% c(96, 98), "Compliance ", ""),
  case_when(
    GROUP_CODE == 10 ~ case_match(GEAR_TYPE_CODE, 3 ~ "OB TRW", 6 ~ "OB POT", 8 ~ "OB HAL"),
    GROUP_CODE == 13 ~ case_match(GEAR_TYPE_CODE, 6 ~ "EM POT", 8 ~ "EM HAL"),
    GROUP_CODE == 14 ~ "EM TRW EFP"
  )
))
odds.dat %>% distinct(STRATA) %>% arrange(STRATA)
if(any(is.na(odds.dat$STRATA))) stop("Some `STRATA` are not yet defined!")

# Lookup table for strata in partial coverage category
partial <- odds.dat %>%
  filter(!(STRATA %like% "Compliance")) %>% 
  group_by(YEAR, STRATA, GEAR_TYPE_CODE) %>%
  distinct(Rate = ODDS_SELECTION_PCT / 100 ) %>%
  ungroup() %>%
  mutate(GEAR = case_match(GEAR_TYPE_CODE, 3 ~ "Trawl", 6 ~ "Pot", 8 ~ "Hook-and-line")) %>%
  mutate(Rate = ifelse(STRATA == "EM TRW EFP", 0.3333, Rate)) %>%
  distinct(YEAR, STRATA, Rate, GEAR) %>%
  mutate(formatted_strat = paste0("*", STRATA, "*"))
partial %>% pivot_wider(names_from = YEAR, values_from = Rate)

# * EM ----
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
                   distinct(VESSEL_ID, GEAR_TYPE_CODE,DESCRIPTION) %>% 
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
  distinct(VESSEL_ID, PLANNED_EMBARK_DATE, GEAR_TYPE_CODE, DESCRIPTION) %>% 
  arrange(VESSEL_ID, PLANNED_EMBARK_DATE, GEAR_TYPE_CODE, DESCRIPTION)

filter(EM.data, VESSEL_ID %in% multiple_gear_nas$VESSEL_ID) %>% 
  distinct(TRIP_NUMBER, VESSEL_ID, TRIP_START_DATE_TIME, AGENCY_GEAR_CODE) %>% 
  arrange(VESSEL_ID, TRIP_START_DATE_TIME)

# Fix NAs in AGENCY_GEAR_CODE for EM.data
EM.data <- EM.data %>% 
           # NAs for vessels that (based on ODDS) fished only one gear in the report year can be assumed to be that gear in the EM data
           mutate(AGENCY_GEAR_CODE=ifelse(VESSEL_ID %in% single_gear_nas$VESSEL_ID[AGENCY_GEAR_CODE=="HAL"] & is.na(AGENCY_GEAR_CODE), "HAL", AGENCY_GEAR_CODE)) %>% 
           mutate(AGENCY_GEAR_CODE=ifelse(VESSEL_ID %in% single_gear_nas$VESSEL_ID[AGENCY_GEAR_CODE=="POT"] & is.na(AGENCY_GEAR_CODE), "POT", AGENCY_GEAR_CODE)) %>% 
           # NAs for vessels that (based on ODDS) fished multiple gears in the report year are recoded manually according to the comparison made immediately above
           mutate(AGENCY_GEAR_CODE = ifelse(TRIP_NUMBER  == "20_POLARSTAR03.02", "HAL", AGENCY_GEAR_CODE))

# The following query will provide a list of em selectecd trips and if they have been reviewed or not
# Query will only include trips in completed or pending status and will not include compliance trips.
# This query will also show the declared gear type and if reviewed, will show the em_reviewed_gear_type_code
# This query will also show when the HD was received by PSFMC and when the EM reviewed data was exported and sent to AFSC
# This query will also show the actual em trip start date and time and actual em trip end date and time which comes from the data on the HD.

# Important note: if an EM reviewed trip used multiple gear types on a trip (ie.  pot and longline) there will be 2 records in the output.

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

# Fixed-gear EM research
em_research <- dbGetQuery(channel_afsc, paste("select distinct adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status
                                              from loki.em_vessels_by_adp
                                              where sample_plan_seq_desc = 'Electronic Monitoring -  research not logged '
                                              and adp =", year))

# * Shapefiles ----
## Load land and NMFS stat area shapefiles 
## (".." points the directory back one level)
shp_land      <- st_read("../shapefiles/P3_AK_All.shp")
shp_nmfs      <- st_read("../shapefiles/NMFS_Zones_Clean.shp")
shp_centroids <- st_read("../shapefiles/NMFS_Area_Centroid.shp")

# Format strata names -----------------------------------------------------

# Format strata names in Valhalla
#' TODO *Do this for observer strata as well, adding "OB "? *
work.data <- mutate(work.data, STRATA = recode(STRATA,
                    "EM_POT" = "EM POT",
                    "EM_HAL" = "EM HAL",
                    "EM_TRW_EFP" = "EM TRW EFP"))

# Identify trips by EM research vessels
work.data <- work.data %>% 
             mutate(STRATA = ifelse(VESSEL_ID %in% em_research$VESSEL_ID, "Zero EM Research", STRATA))

# Save --------------------------------------------------------------------

# Remove any remaining unwanted objects and save data
rm(location, channel_afsc, channel_akro)

# Save
save.image(file = "2_AR_data.Rdata")
