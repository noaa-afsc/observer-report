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
  WHERE EXTRACT(YEAR FROM a.original_embark_date) IN (", paste(year + -2:-1, collapse = ","), ")
    AND a.trip_plan_log_seq != 202328923              -- Just to make the code run for now until the package is fixed
"
)

#' *TODO* this code doesn't work, will need to get old code working again
#' `error with year = 2020` *Could not find selection rate for strata - date*

odds.dat <- dbGetQuery(channel_afsc, script)

# Data checks and clean up

# Check for duplicates - should be no records (= 0)
sum(duplicated(odds.dat$TRIP_PLAN_LOG_SEQ)) 

# Database check - Should be no records
length(odds.dat[!is.na(odds.dat$TRIP_SELECTED_OBS) & odds.dat$TRIP_STATUS_CODE=="CN"]) 

# This confirms the check
table(odds.dat$TRIP_SELECTED_OBS, odds.dat$TRIP_STATUS_CODE, useNA='always')  

# Summary of trip dispositions and observer assignments 
# Sample plan sequences: 11 = gear + tender, 13 = EM selected for review, 14 = EM EFP
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
                   DESCRIPTION=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==6 & TENDER_TRIP_FLAG=="N", paste0(DESCRIPTION, " - POT - No Tender"), DESCRIPTION),
                   DESCRIPTION=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==6 & TENDER_TRIP_FLAG=="Y", paste0(DESCRIPTION, " - POT - Tender"), DESCRIPTION),
                   DESCRIPTION=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==8 & TENDER_TRIP_FLAG=="N", paste0(DESCRIPTION, " - HAL - No Tender"), DESCRIPTION),
                   DESCRIPTION=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==8 & TENDER_TRIP_FLAG=="Y", paste0(DESCRIPTION, " - HAL - Tender"), DESCRIPTION))

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

# Translate ODDS sample plans into strata
odds.dat <- mutate(odds.dat, STRATA = recode(DESCRIPTION, 
                                             "Declared Gear & Tender Delivery - Pot No  Tender Delivery" = "POT",    
                                             "Declared Gear & Tender Delivery - Pot   Tender Delivery" = "POT",      
                                             "Declared Gear & Tender Delivery - Trawl  No Tender Delivery" = "TRW",  
                                             "EM Declared Gear & Tender - Longline No Tender Delivery" = "EM HAL",      
                                             "Declared Gear & Tender Delivery - Longline No Tender Delivery" = "HAL",
                                             "EM Declared Gear & Tender - Pot gear - No Tender Delivery" = "EM POT",    
                                             "Declared Gear & Tender Delivery - Trawl  Tender Delivery" = "TRW",    
                                             "EM Compliance Monitoring - Fishing IFQ mulitple Areas - HAL - No Tender" = "EM Compliance HAL",
                                             "Declared Gear & Tender Delivery - Longline  Tender Delivery" = "HAL",
                                             "EM Declared Gear & Tender -  Pot gear Tender delivery" = "EM POT",
                                             "EM Declared Gear & Tender - Longline Tender Delivery" = "EM HAL",
                                             "EM Compliance Monitoring - Fishing IFQ mulitple Areas - POT - No Tender" = "EM Compliance POT",
                                             "EM Exempt fish Permit - Trawl - No Tender Delivery" = "EM TRW EFP",
                                             "EM Exempt Fish Permit -  Trawl - Tender Delivery" = "EM TRW EFP"))

# Format strata names in Valhalla
work.data <- mutate(work.data, STRATA = recode(STRATA,
                    "EM_POT" = "EM POT",
                    "EM_HAL" = "EM HAL",
                    "EM_TRW_EFP" = "EM TRW EFP"))

# Identify trips by EM research vessels
work.data <- work.data %>% 
             mutate(STRATA = ifelse(VESSEL_ID %in% em_research$VESSEL_ID, "Zero EM Research", STRATA))

# Lookup table for strata in partial coverage category
partial_desc <- data.table(STRATA = c("HAL", "POT", "TRW", "EM HAL", "EM POT", "EM TRW EFP"), 
                      descriptions = c("hook-and-line gear", "pot gear", "trawl gear", "hook-and-line gear with electronic monitoring", "pot gear with electronic monitoring", "trawl gear with electronic monitoring")) 
# Pull selection rates from norpac.odds_pct_trip_select table
partial <- setDT(dbGetQuery(channel_afsc, paste(
  "
  SELECT DISTINCT 
    a.percent / 100 as rate, a.effective_date, 
    b.sample_plan_seq, 
    c.description AS GEAR,
    d.description AS SAMPLE_PLAN
  FROM norpac.odds_pct_trip_select a
    JOIN norpac.odds_vessel_to_rate_strata b
      ON a.trip_rate_strata = b.trip_rate_strata
    JOIN norpac.atl_lov_gear_type c
      ON b.gear_type_code = c.gear_type_code
    JOIN norpac.odds_lov_sample_plan d
      ON b.sample_plan_seq = d.sample_plan_seq
  WHERE EXTRACT(YEAR FROM a.effective_date) = ", year, "
  "
)))

partial[, GEAR := ifelse(GEAR %like% "Pot", "POT", ifelse(GEAR %like% "Longline", "HAL", ifelse(GEAR %like% "Trawl", "TRW", GEAR)))] # Simplify gear types
partial[, STRATA := ifelse(           # Define strata based on sample plan and gear type
  SAMPLE_PLAN %like% "Electronic Monitoring", paste("EM", GEAR, sep=" "), ifelse(
    SAMPLE_PLAN %like% "EM EFP" & GEAR == "TRW", "EM TRW EFP", ifelse(
      SAMPLE_PLAN %like% "Gear Type", GEAR, NA)))]
partial <- unique(partial[, .(Effective_Date = as.Date(EFFECTIVE_DATE), STRATA, Rate = RATE)])[order(Effective_Date, STRATA)]  # Run unique on simplified gear and sample plans
partial[STRATA == "EM TRW EFP", Rate := 0.3000]   # Make the expected rate for partial coverage EM TRW EFP equal to the shoreside monitoring rate (not in ODDS)  
partial[, descriptions := partial_desc[partial, descriptions, on=.(STRATA)]]    # Merge descriptions in 
partial[, formatted_strat := paste0("*", STRATA, "*")]                          # Create formatted_strata column
partial[, txt := paste0(formatC(round(Rate * 100, 2), format='f', digits=2), '% in the ', formatted_strat, ' stratum')]    # Create txt column that combines Rate and formatted_strata
dcast(partial, STRATA ~ Effective_Date, value.var="Rate")   # Note that if the rates change for some strata and not others, 'NA' is returned

# Save --------------------------------------------------------------------

# Remove any remaining unwanted objects and save data
rm(location, channel_afsc, channel_akro)

# Save
save.image(file = "2_AR_data.Rdata")
