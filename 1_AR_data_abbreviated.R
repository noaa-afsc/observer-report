
# Setup -------------------------------------------------------------------

# Get packages and user-defined functions
source("3_helper.R")

# Random number seed
set.seed(052870)

# Report year (year that fishing and observing took place)
year <- 2021 

# The user's physical location when running this code (used to pull data from the closest database)
location <- toupper(getPass('What is your current physical location? (Juneau or Seattle)'))

# Establish database connections
channel_afsc  <- channel.fxn(location)
channel_akro  <- channel.fxn(location, db="AKRO") # Hit cancel unless sitting in Juneau and pulling Valhalla.

# Get data ----------------------------------------------------------------



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


# * ODDS ----

# Queries
script <- paste("
SELECT
norpac.ODDS_RANDOM_NUMBER.getControlPct(pl.original_embark_date, vtr.trip_rate_strata)  as ODDS_SELECTION_PCT,
EXTRACT(YEAR FROM pl.original_embark_date) AS YEAR,      
vtr.sample_plan_seq,
--vtr.gear_type_code, --Change #1 for 2018 Annual Report
-- vtr.tender_trip_flag, --Change #2 for 2018 Annual Report
-- vtr.trip_rate_strata, 
vtr.description,
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


# Get gear type for EM data
script <- paste0("SELECT * from em_pac_review.EM_FISHING_EVENT
                  WHERE EXTRACT(YEAR FROM END_DATE_TIME) = ", year)

EM.gear <- dbGetQuery(channel_afsc, script)



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


# Save --------------------------------------------------------------------

# Remove any remaining unwanted objects and save data
rm(location, channel_afsc, channel_akro)

# Save
#save.image(file = "2_AR_data_abbreviated.Rdata")
save.image(file = "G:\\FMGROUP\\Observer Program Annual Report\\2021_Annual_Report\\2_AR_raw_afsc_data.RData")
