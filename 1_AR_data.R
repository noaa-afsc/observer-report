# Setup -------------------------------------------------------------------

# Get packages and user-defined functions
source("3_helper.R")

# Get list of all items in helper that can be excluded from output of 2_AR.data.rdata
helper_objects <- ls()

# Random number seed
set.seed(052870)

# Report year (year that fishing and observing took place)
year <- 2024

# Establish database connection
channel_afsc  <- eval(parse(text = Sys.getenv('channel_afsc')))

# Get data ----------------------------------------------------------------

# Assign the address of the Annual Report Project in the Shared Gdrive
AnnRpt_DepChp_dribble <- gdrive_set_dribble("Projects/AnnRpt-Deployment-Chapter")

# * Observer costs ----

#' Get actual costs from spreadsheet provided by FMA (make sure spreadsheet is up-to-date!). 
#' Initially, download the following spreadsheet as a `.csv` format file to the `source_data/` folder. 
#' Upload an `.rdata` copy to the [FMA shared Gdrive].
#' [https://docs.google.com/spreadsheets/d/1KGmZNo7uVuB6FCVRZd4UV2PROa4Jl7-J/edit?usp=sharing&ouid=112928343270640187258&rtpof=true&sd=true]
if(FALSE) {
  FMA_Days_Paid <- read.csv("source_data/FMA Days Paid.xlsx - Days_Paid.csv")
  save(FMA_Days_Paid, file = "source_data/FMA_Days_Paid.rdata")
  gdrive_upload("source_data/FMA_Days_Paid.rdata", AnnRpt_DepChp_dribble)
}

gdrive_download("source_data/FMA_Days_Paid.rdata", AnnRpt_DepChp_dribble)
load("source_data/FMA_Days_Paid.rdata")
days_paid <- filter(FMA_Days_Paid, Calendar == year)

# * ADP outputs ----

# Assign the address of the ADP outputs in the Shared Gdrive
ADP_Output_dribble <- gdrive_set_dribble("Projects/ADP/Output")
ADP_Tables_dribble <- gdrive_set_dribble("Projects/ADP/Output/2024 ADP")

# Initially:
#' `ADP/results/final_adp_2024_results.rdata`
#' shared Gdrive in `Projects/ADP/Output` folder as `final_adp_2024_results.rdata`
if(FALSE) gdrive_upload("source_data/final_adp_2024_results.rdata", ADP_Output_dribble)
if(FALSE) gdrive_upload("source_data/final_adp_tables_and_figures_2024.rdata", ADP_Tables_dribble)

# 2024 Final ADP Outputs
gdrive_download("source_data/final_adp_2024_results.rdata", ADP_Output_dribble)
gdrive_download("source_data/final_adp_tables_and_figures_2024.rdata", ADP_Tables_dribble)
gdrive_download("source_data/costs_boot_lst_2024AR.rdata", ADP_Tables_dribble)
load("source_data/final_adp_2024_results.rdata")
load("source_data/final_adp_tables_and_figures_2024.rdata")  # table_b3_flex contains predicted days by stratum
load("source_data/costs_boot_lst_2024AR.rdata")              # simulated observer days and costs

## Predicted monitored days by stratum
predicted <- table_b3_flex$body$dataset[15:29,] %>%
  # Isolate partial coverage strata
  filter(Pool %in% c("At-sea Observer", "Fixed-gear EM", "Trawl EM (EFP)") & Stratum != "Total") %>%
  mutate(STRATA = case_when(Pool == "At-sea Observer" & Stratum == "Fixed-gear BSAI" ~ "OB FIXED BSAI", 
                            Pool == "At-sea Observer" & Stratum == "Fixed-gear GOA" ~ "OB FIXED GOA",
                            Pool == "At-sea Observer" & Stratum == "Trawl BSAI" ~ "OB TRW BSAI",
                            Pool == "At-sea Observer" & Stratum == "Trawl GOA" ~ "OB TRW GOA",
                            Pool == "Fixed-gear EM" & Stratum == "Fixed-gear BSAI" ~ "EM FIXED BSAI",
                            Pool == "Fixed-gear EM" & Stratum == "Fixed-gear GOA" ~ "EM FIXED GOA",
                            Pool == "Trawl EM (EFP)" & Stratum == "Trawl GOA" ~ "EM TRW GOA (EFP)")) %>%
  rename(pred_days = d) %>%
  select(STRATA, pred_days)

# Budget(s) used in the ADP
bud_scen_lst <- unlist(budget_lst)

## Distribution of simulated observed days and costs (at-sea observers only)

# Convert matrices to data.tables and flatten list to a single table
sim_costs_dt <- rbindlist(lapply(
  lapply(costs_boot_lst, "[[", "SIM_COSTS" ), function(x) as.data.table(t(x))
), idcol = "SIM_ITER")
# Format columns and add identifier for ODDS iteration
col_nms <- colnames(sim_costs_dt)
sim_costs_dt[, (col_nms) := lapply(.SD, as.numeric), .SDcols = col_nms]
sim_costs_dt[, ODDS_ITER := seq_len(.N), by = .(SIM_ITER)]
# Formatted result for the AR
bud_tbl <- sim_costs_dt[, .(SIM_ITER, ODDS_ITER, ADP_D = OB_DAYS, ADP_C = OB_TOTAL)]

# * Valhalla ----

# Create a copy of Valhalla named 'work.data' that will be manipulated
gdrive_download("source_data/2025-01-14cas_valhalla.Rdata", AnnRpt_DepChp_dribble)
load("source_data/2025-01-14cas_valhalla.Rdata")
work.data <- valhalla[, PERMIT := as.character(PERMIT)][]
rm(valhalla)

#Summary of coverage by strata and processing sector
#This is a check to make sure no entries look wonky
table(work.data$COVERAGE_TYPE, work.data$STRATA, work.data$PROCESSING_SECTOR, useNA = 'always')

# Data check for observed vessels under 40 ft., should be zero rows
work.data %>% 
filter(LENGTH_OVERALL < 40 & OBSERVED_FLAG == "Y") %>% 
select(VESSEL_ID, TRIP_ID, REPORT_ID, COVERAGE_TYPE, STRATA, OBSERVED_FLAG, TRIP_TARGET_CODE, REPORTING_AREA_CODE, LANDING_DATE, LENGTH_OVERALL) %>% 
distinct() %>% 
arrange(VESSEL_ID)

# Change date format to eliminate times and match what is in the database
work.data <- mutate(work.data, TRIP_TARGET_DATE = as.Date(TRIP_TARGET_DATE), LANDING_DATE = as.Date(LANDING_DATE))

# Check for TRIP_IDs that are repeated across ADP years
if( nrow(unique(work.data[, .(TRIP_ID, ADP)])[, .N, keyby = .(TRIP_ID)][N > 1]) ){
  message("Some TRIP_IDs are repated across ADP years")
  print(unique(work.data[, .(TRIP_ID, ADP)])[, .N, keyby = .(TRIP_ID)][N > 1])
}

# Format strata names
work.data <- work.data %>%
  mutate(STRATA = recode(
    STRATA,
    "EM_FIXED_BSAI" = "EM FIXED BSAI",
    "EM_FIXED_GOA" = "EM FIXED GOA",
    "EM_TRW_BSAI" = "EM TRW BSAI (EFP)",
    "EM_TRW_GOA" = "EM TRW GOA (EFP)",
    "OB_FIXED_BSAI" = "OB FIXED BSAI",
    "OB_FIXED_GOA" = "OB FIXED GOA",
    "OB_TRW_BSAI" = "OB TRW BSAI",
    "OB_TRW_GOA" = "OB TRW GOA"))

#' [NOTE] `work.data` may be modified later in the `EM` section of this script to re-assign STRATA for any EM research
#' vessels active for this year.

# * Observed days ----

#' Match up ODDS/OLS records with VALHALLA records to get stratum-specific estimates of total at-sea observer days.

source("functions/model_trip_duration.R")

#' Initialize: Perform the matching of ODDS+OLS with VALHALLA, and model using the simplest model possible. The results
#' of the matching are automatically assigned to the `mod_dat` object in the global environment.
td_mod0 <- model_trip_duration(work.data, use_mod = "DAYS ~ RAW", channel = channel_afsc)

#' Yearly totals
mod_dat[, .(DAYS = sum(DAYS)), keyby = .(ADP)]
#' It appears some observers were assigned to non-observer strata
mod_dat[, .(DAYS = sum(DAYS)), keyby = .(ADP, STRATA)]

#' Scraping all the non-observer strata matches to their observed strata counterparts. FMA is charged for sea
#' days regardless of the observer was supposed to monitor these trips or not.
mod_dat_copy <- copy(mod_dat)
mod_dat_copy[STRATA == "ZERO", STRATA := "OB FIXED GOA"]
mod_dat_copy[, .(DAYS = sum(DAYS)), keyby = .(ADP, STRATA)]

# Stratum-specific totals
obs_act_days <- mod_dat_copy[, .(act_days = sum(DAYS)), keyby = .(ADP, STRATA)]
obs_act_days <- mutate(obs_act_days, ADP = as.numeric(as.character(ADP)))
if(!any(obs_act_days$STRATA %like% "OB ")) stop("You still have non-observer strata in 'obs_act_days'")

rm(td_mod0, mod_dat, mod_dat_copy, model_trip_duration)

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

# The following query returns all landing ID's for offloads monitored for salmon all sectors.
salmon.landings.obs <- dbGetQuery(channel_afsc, script)

# * ODDS ----

# Queries
script <- paste("
  SELECT 
    d.percent as ODDS_SELECTION_PCT,     
    a.trip_plan_log_seq, a.trip_status_code, a.vessel_seq, EXTRACT(YEAR FROM a.original_embark_date) AS YEAR,
    a.original_embark_date, a.planned_embark_date, a.tender_trip_flag, a.trip_plan_number,
    b.trip_stratas_seq, b.trip_monitor_code, b.trip_selected,  b.random_number_used, b.strata AS STRATA_CODE, 
    -- inherit_trip_seq corresponds to the trip_stratas_seq that was cancelled, not trip_plan_log_seq before ODDS 3.0!
    b.inherit_trip_seq,          
    c.group_code, c.description AS STRATUM_DESCRIPTION,
    e.release_comment,
    f.description AS RELEASE_STATUS_DESCRIPTION,
    g.akr_vessel_id AS VESSEL_ID,
    h.gear_type_code
   
  FROM odds.odds_trip_plan_log a
    LEFT JOIN odds.odds_trip_stratas b
      ON a.trip_plan_log_seq = b.trip_plan_log_seq 
    LEFT JOIN odds.odds_strata c
      ON b.strata = c.strata
    LEFT JOIN odds.odds_strata_rates d
      ON b.strata = d.strata
    LEFT JOIN odds.odds_strata_release e
      ON b.trip_stratas_seq = e.trip_stratas_seq
    LEFT JOIN odds.odds_lov_release_status f
      ON e.release_status_seq = f.release_status_seq
    LEFT JOIN norpac.atl_lov_vessel g
      ON a.vessel_seq = g.vessel_seq
    LEFT JOIN odds.odds_trip_gear h
      ON a.trip_plan_log_seq = h.trip_plan_log_seq
  WHERE EXTRACT(YEAR FROM a.original_embark_date) =", year,"
  AND EXTRACT(YEAR FROM d.effective_date) =", year)

odds.dat <- dbGetQuery(channel_afsc, script)

# Summary of trip dispositions and observer assignments 
# GROUP_CODE: 10:11 = at-sea observer, 13 = Fixed-gear EM, 14 = Trawl EM
# TRIP_STATUS_CODE: 
#   CS = Cancel by System 
#   PD = Pending
#   CN = Cancelled 
#   CP = Completed 
#   CC = Cancel Cascaded
#   CR = Cancel Replaced (introduced with ODDS 3.0 in 2023)
table(odds.dat$TRIP_MONITOR_CODE, odds.dat$TRIP_STATUS_CODE, odds.dat$GROUP_CODE, useNA = 'ifany')

#' The TRIP_SELECTED = "Y" when STRATA_CODE = 96 or 98 means that coverage was requested (due to fishing occurring in 
#' more than one area) but the random number generated was larger than the programmed rate, and so the video was not 
#' selected for review. Since these trips aren't truly monitored, make TRIP_SELECTED = "N". 96 is used for at-sea 
#' observer compliance trips and 98 is used for at-sea fixed gear EM trips.
odds.dat <- mutate(odds.dat, TRIP_SELECTED = ifelse(STRATA_CODE %in% c(96, 98), "N", TRIP_SELECTED))

# Translate GROUP_CODE, STRATA_CODE, and GEAR_TYPE_CODE into STRATA
odds.dat <- mutate(odds.dat, STRATA = paste0(
  # Tag on "compliance" if the trip was a multi-area IFQ trip
  ifelse(STRATA_CODE %in% c(96, 98), "Compliance ", ""),
  case_when(
    STRATUM_DESCRIPTION == "EM EFP - Trawl No Tender" ~ "EM TRW GOA (EFP)",
    STRATUM_DESCRIPTION == "EM EFP - Trawl Tender Delivery" ~ "EM TRW GOA (EFP)",
    STRATUM_DESCRIPTION == "EM Fixed Gear - BSAI" ~ "EM FIXED BSAI",
    STRATUM_DESCRIPTION == "EM Fixed Gear - GOA" ~ "EM FIXED GOA",
    STRATUM_DESCRIPTION == "Fixed Gear - BSAI" ~ "OB FIXED BSAI",
    STRATUM_DESCRIPTION == "Fixed Gear - GOA" ~ "OB FIXED GOA",
    STRATUM_DESCRIPTION == "Trawl Gear - BSAI" ~ "OB TRW BSAI",
    STRATUM_DESCRIPTION == "Trawl Gear - GOA" ~ "OB TRW GOA",
    .default = "Unknown"
  )
))
odds.dat %>% distinct(STRATA) %>% arrange(STRATA)
if(any(odds.dat$STRATA == "Unknown")) stop("Some `STRATA` are not yet defined!")

# Create a lookup table for strata in partial coverage category
partial <- odds.dat %>%
  filter(!(STRATA %like% "Compliance")) %>% 
  group_by(YEAR, STRATA, GEAR_TYPE_CODE) %>%
  distinct(Rate = ODDS_SELECTION_PCT / 100 ) %>%
  ungroup() %>%
  mutate(GEAR = case_match(GEAR_TYPE_CODE, 3 ~ "Trawl", 6 ~ "Pot", 8 ~ "Hook-and-line")) %>%
  mutate(Rate = ifelse(STRATA == "EM TRW GOA (EFP)", 0.3333, Rate)) %>%
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

# Fix EM.data VESSEL_ID
EM.data <- rename(EM.data, PS_VESSEL_ID = VESSEL_ID)

EM.data <- merge(EM.data, transform.EM.data.vessel, 
                 by.x = "PS_VESSEL_ID",
                 by.y = "EM_VESSEL_ID", all.x = TRUE)

rm(transform.EM.data.vessel)

# Get gear type for EM data
script <- paste0("SELECT * from em_pac_review.EM_FISHING_EVENT
                  WHERE EXTRACT(YEAR FROM END_DATE_TIME) = ", year)

EM.gear <- dbGetQuery(channel_afsc, script)

# Select data for this report year and recode gear type to those used by CAS
EM.gear <- 
  select(EM.gear, TRIP_NUMBER, GEAR_TYPE_ID) %>% 
  distinct() %>% 
  arrange(TRIP_NUMBER) %>% 
  mutate(AGENCY_GEAR_CODE = case_when(GEAR_TYPE_ID %in% c(6, 7) ~ "HAL",
                                      GEAR_TYPE_ID %in% c(10, 11, 16, 17, 18, 19) ~ "POT")) %>%
  select(TRIP_NUMBER, AGENCY_GEAR_CODE) %>%  
  distinct()

# Join gear types to EM data
EM.data <- left_join(EM.data, EM.gear, by = "TRIP_NUMBER")

# Are there NAs in EM.data$AGENCY_GEAR_CODE?
EM.data %>% group_by(AGENCY_GEAR_CODE) %>% summarise(n = n_distinct(TRIP_NUMBER))

# Isolate VESSEL_IDs for NAs in EM.data$AGENCY_GEAR_CODE
gear_na_vessels <- filter(EM.data, is.na(AGENCY_GEAR_CODE)) %>% distinct(VESSEL_ID) %>% unlist() %>% as.vector()

# Isolate VESSEL_IDs with NAs in EM.data$AGENCY_GEAR_CODE 
# that logged trips of only one gear type in ODDS
single_gear_nas <- 
  filter(odds.dat, VESSEL_ID %in% gear_na_vessels) %>% 
  distinct(VESSEL_ID, GEAR_TYPE_CODE, STRATUM_DESCRIPTION) %>% 
  arrange(VESSEL_ID, GEAR_TYPE_CODE) %>% 
  group_by(VESSEL_ID) %>% 
  filter(uniqueN(GEAR_TYPE_CODE) == 1) %>% 
  mutate(AGENCY_GEAR_CODE = ifelse(GEAR_TYPE_CODE == 8, "HAL", NA)) %>% 
  mutate(AGENCY_GEAR_CODE = ifelse(GEAR_TYPE_CODE == 6, "POT", AGENCY_GEAR_CODE)) %>% 
  distinct(VESSEL_ID, AGENCY_GEAR_CODE, STRATUM_DESCRIPTION)

# Isolate VESSEL_IDs with NAs in EM.data$AGENCY_GEAR_CODE 
# that logged trips of more than one gear type in ODDS
multiple_gear_nas <- 
  filter(odds.dat, VESSEL_ID %in% gear_na_vessels) %>% 
  distinct(VESSEL_ID, GEAR_TYPE_CODE) %>% 
  arrange(VESSEL_ID, GEAR_TYPE_CODE) %>% 
  group_by(VESSEL_ID) %>% 
  filter(uniqueN(GEAR_TYPE_CODE) > 1)

# Compare ODDS to EM.data for VESSEL_IDs with NAs in EM.data$AGENCY_GEAR_CODE
# to determine the most likely AGENCY_GEAR_CODE
filter(odds.dat, VESSEL_ID %in% multiple_gear_nas$VESSEL_ID) %>% 
  distinct(VESSEL_ID, PLANNED_EMBARK_DATE, GEAR_TYPE_CODE, STRATUM_DESCRIPTION) %>% 
  arrange(VESSEL_ID, PLANNED_EMBARK_DATE, GEAR_TYPE_CODE, STRATUM_DESCRIPTION)

filter(EM.data, VESSEL_ID %in% multiple_gear_nas$VESSEL_ID) %>% 
  distinct(TRIP_NUMBER, VESSEL_ID, TRIP_START_DATE_TIME, AGENCY_GEAR_CODE) %>% 
  arrange(VESSEL_ID, TRIP_START_DATE_TIME)

#' *Fix NAs in AGENCY_GEAR_CODE for EM.data*
#' *TODO We should make the query more robust, filter out non-fishing trips and when gear is NA but ODDS has it as trawl.*
EM.data <- 
  EM.data %>% 
  # NAs for vessels that (based on ODDS) fished only one gear in the report year can be assumed to be that gear in the EM data
  mutate(AGENCY_GEAR_CODE = ifelse(VESSEL_ID %in% single_gear_nas$VESSEL_ID[AGENCY_GEAR_CODE == "HAL"] & is.na(AGENCY_GEAR_CODE), "HAL", AGENCY_GEAR_CODE)) %>% 
  mutate(AGENCY_GEAR_CODE = ifelse(VESSEL_ID %in% single_gear_nas$VESSEL_ID[AGENCY_GEAR_CODE == "POT"] & is.na(AGENCY_GEAR_CODE), "POT", AGENCY_GEAR_CODE))

# The following query will provide a list of EM selected trips and if they have been reviewed or not
# Query will only include trips in completed or pending status and will not include compliance trips.
# This query will also show the declared gear type and if reviewed, will show the em_reviewed_gear_type_code
# This query will also show when the HD was received by PSFMC and when the EM reviewed data was exported and sent to AFSC
# This query will also show the actual EM trip start date and time and actual EM trip end date and time which comes from the data on the HD.

# Important note: if an EM reviewed trip used multiple gear types on a trip (i.e.,  pot and longline) there will be 2 records in the output.
if(F){ 
  # FIXME: When performing the data timeliness evaluations for the 2024 ADP, 
  # it was discovered that some of the columns below don't mean what they sound like. 
  # Even if the columns are accurate, this query would need to be translated to the ODDS schema. 
  # Consider switching to 2024 ADP data timeliness queries.
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
    
    ----get the EM reviewed trip number for those em trips that have been reviwed and where AFSC has the data
    
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
    
    --- the below query will get the when the HD was received by PSMFC, when it was exported to AFSC and the em trip start date and time and trip end date and time
    
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

}

# Fixed-gear EM research
em_research <- dbGetQuery(channel_afsc, paste(" select distinct adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status
                                              from loki.em_vessels_by_adp
                                              where sample_plan_seq_desc = 'Electronic Monitoring -  research not logged '
                                              and adp =", year))

# Identify trips by EM research vessels
work.data <- work.data %>% 
  mutate(STRATA = ifelse(VESSEL_ID %in% em_research$VESSEL_ID, "Zero EM Research", STRATA))

# * Shapefiles ----
# Initial upload to Shared Gdrive
if(FALSE) gdrive_upload("source_data/ak_shp.rdata", AnnRpt_DepChp_dribble)
## Load land and NMFS stat area shapefiles 
gdrive_download("source_data/ak_shp.rdata", AnnRpt_DepChp_dribble)
(load(("source_data/ak_shp.rdata")))

# * EM trawl offloads ----
em_trw_offload <- work.data %>%
  filter(STRATA %in% c("EM TRW BSAI (EFP)", "EM TRW GOA (EFP)")) %>%
  select(TRIP_ID, REPORT_ID, VESSEL_ID, STRATA, MANAGEMENT_PROGRAM_CODE, AGENCY_GEAR_CODE, COVERAGE_TYPE,
         LANDING_DATE, OBSERVED_FLAG, PORT_CODE, FMP, TENDER, TENDER_VESSEL_ADFG_NUMBER) %>%
  distinct() %>%
  mutate(TENDER_VESSEL_ADFG_NUMBER = as.integer(TENDER_VESSEL_ADFG_NUMBER))

# Double check no duplicate report_ids
nrow(em_trw_offload %>% select(REPORT_ID) %>% distinct()) - nrow(em_trw_offload)

# Check for BSAI tendering
nrow(filter(em_trw_offload, STRATA == "EM TRW BSAI (EFP)" & TENDER == "Y"))

# Check gear type and IFP deliveries
nrow(filter(em_trw_offload, AGENCY_GEAR_CODE == "NPT" | PORT_CODE == "IFP"))

# Get eLandings data for these fish tickets to add TENDER_OFFLOAD_DATE which is needed to match
#  observer data so we can determine which landings observers marked as monitored
script <- paste(
  "SELECT report_id, vessel_id, tender_vessel_adfg_number, tender_offload_date, processor_name
   FROM norpac_views.atl_landing_id
   WHERE year = ", year
)

vessels <- dbGetQuery(channel_afsc, paste(
  "SELECT adfg_number, permit, name
   FROM norpac.atl_lov_vessel"
  )) %>%
  mutate(PERMIT = as.integer(PERMIT), ADFG_NUMBER = as.integer(ADFG_NUMBER))

eland.offload <- dbGetQuery(channel_afsc, script) %>%
  filter(REPORT_ID %in% em_trw_offload$REPORT_ID) %>%
  mutate(TENDER_OFFLOAD_DATE = as.Date(TENDER_OFFLOAD_DATE),
         TENDER_VESSEL_ADFG_NUMBER = as.integer(TENDER_VESSEL_ADFG_NUMBER)) %>%
  left_join(vessels, by = join_by(VESSEL_ID == PERMIT)) %>%
  rename(CV_NAME = NAME, CV_ID = VESSEL_ID) %>%
  select(-ADFG_NUMBER) %>%
  left_join(vessels, by = join_by(TENDER_VESSEL_ADFG_NUMBER == ADFG_NUMBER)) %>%
  rename(TENDER_NAME = NAME, TENDER_ID = PERMIT) %>%
  mutate(T_REPORT_ID = case_when(!is.na(TENDER_OFFLOAD_DATE) ~ paste0(TENDER_VESSEL_ADFG_NUMBER, TENDER_OFFLOAD_DATE)))

# Make sure we have the same number of records
nrow(eland.offload) - nrow(em_trw_offload)

# and the same REPORT_IDs
nrow(anti_join(eland.offload, em_trw_offload, by = join_by(REPORT_ID)))

# Combine VALHALLA with eLandings
work.eland <- em_trw_offload %>%
  left_join(eland.offload, by = join_by(REPORT_ID, VESSEL_ID == CV_ID, TENDER_VESSEL_ADFG_NUMBER)) %>%
  rename(CV_ID = VESSEL_ID)

# Look at missing offloads from one data source to the other
#  How many offloads were marked as tendered in VALHALLA that do not have an assigned tender vessel or
#  TENDER_OFFLOAD_DATE from eLandings
nrow(work.eland %>% filter(TENDER == "Y" & is.na(T_REPORT_ID)))

# Compare observer monitoring records to VALHALLA monitoring records
script <- paste(
  "SELECT o.landing_report_id AS report_id, o.cruise, o.permit AS processor_permit_id,
      v.permit, v.name, o.delivery_end_date, o.offload_number, v.adfg_number,
      o.delivered_weight, o.lb_kg, o.offload_to_tender_flag,
      CASE WHEN EXISTS (SELECT 1 FROM norpac.atl_salmon WHERE cruise = o.cruise AND permit = o.permit AND offload_seq = o.offload_seq)
      THEN 'Y' ELSE 'N' END as obs_salmon_cnt_flag --Identifies where salmon counts were done
      FROM norpac.atl_offload o
  LEFT JOIN norpac.atl_lov_vessel v
      ON o.delivery_vessel_adfg = v.adfg_number
  LEFT JOIN norpac.atl_lov_plant p
      ON o.permit = p.permit
  WHERE EXTRACT(YEAR FROM o.delivery_end_date) = ", year
)

obs.offload <- dbGetQuery(channel_afsc, script) %>%
  mutate(ADFG_NUMBER = as.integer(ADFG_NUMBER),
         PERMIT = as.integer(PERMIT),
         T_REPORT_ID = NA,
         DELIVERY_END_DATE = as.Date(DELIVERY_END_DATE))

# Observer tender offloads
obs.tender <- obs.offload %>%
  filter(ADFG_NUMBER %in% work.eland$TENDER_VESSEL_ADFG_NUMBER & !is.na(ADFG_NUMBER)) %>%
  filter(is.na(REPORT_ID)) %>%
  select(-OFFLOAD_NUMBER, -REPORT_ID) %>% # Looks like cruise 26929 has a duplicate haul for tender 5178/59109
  mutate(T_REPORT_ID = paste0(ADFG_NUMBER, DELIVERY_END_DATE)) %>%
  rename(TENDER_ID = PERMIT,
         TENDER_NAME = NAME,
         TENDER_VESSEL_ADFG_NUMBER = ADFG_NUMBER,
         TENDER_OFFLOAD_DATE = DELIVERY_END_DATE) %>%
  distinct()

val.tender <- filter(work.eland, !is.na(T_REPORT_ID))

tender.link <- val.tender %>%
  left_join(obs.tender, by = join_by(TENDER_ID, TENDER_NAME, T_REPORT_ID, TENDER_OFFLOAD_DATE,
                                     TENDER_VESSEL_ADFG_NUMBER))

# Many-to-manys
#filter(obs.tender, TENDER_VESSEL_ADFG_NUMBER == 59109 & T_REPORT_ID == "591092024-09-18") # Duplicate record?

# Observed non-tender offloads
obs.cv <- obs.offload %>%
  filter(REPORT_ID %in% work.eland$REPORT_ID) %>%
  filter(!is.na(REPORT_ID)) %>%
  select(-OFFLOAD_NUMBER)

val.cv <- filter(work.eland, is.na(T_REPORT_ID))

cv.link <- val.cv %>%
  left_join(obs.cv, by = join_by(REPORT_ID, T_REPORT_ID)) %>%
  select(-PERMIT, -NAME, -ADFG_NUMBER, -DELIVERY_END_DATE) %>%
  mutate(TENDER_OFFLOAD_DATE = as.Date(TENDER_OFFLOAD_DATE))
  
# Combine tenders and CVs
work.link <- tender.link %>%
  rbind(cv.link) %>%
  relocate(TRIP_ID, REPORT_ID, T_REPORT_ID, CV_ID, CV_NAME, TENDER_ID, TENDER_VESSEL_ADFG_NUMBER, TENDER_NAME,
           LANDING_DATE, TENDER_OFFLOAD_DATE, PORT_CODE, TENDER, OFFLOAD_TO_TENDER_FLAG, OBSERVED_FLAG,
           OBS_SALMON_CNT_FLAG)

# Clean up workspace
rm(vessels, val.tender, val.cv, tender.link, cv.link, work.eland, obs.tender, obs.cv, obs.offload, em_trw_offload,
   eland.offload)

# Evaluate differences between what VALHALLA says is observed compared to what observer records say
#'* Remove when finalizing code *
work.obs.cv <- filter(work.link, is.na(T_REPORT_ID))
nrow(filter(work.obs.cv, (OBSERVED_FLAG == "Y" & OBS_SALMON_CNT_FLAG == "N") |
              (OBSERVED_FLAG == "N" & OBS_SALMON_CNT_FLAG == "Y")))

cv.dups <- filter(work.obs.cv, (OBSERVED_FLAG == "Y" & OBS_SALMON_CNT_FLAG == "N") |
                    (OBSERVED_FLAG == "N" & OBS_SALMON_CNT_FLAG == "Y"))

work.obs.tender <- filter(work.link, !is.na(T_REPORT_ID))
nrow(filter(work.obs.tender, (OBSERVED_FLAG == "Y" & OBS_SALMON_CNT_FLAG == "N") |
              (OBSERVED_FLAG == "N" & OBS_SALMON_CNT_FLAG == "Y")))

tender.dups <- filter(work.obs.tender, (OBSERVED_FLAG == "Y" & OBS_SALMON_CNT_FLAG == "N") |
                        (OBSERVED_FLAG == "N" & OBS_SALMON_CNT_FLAG == "Y"))

work.dups.cv <- filter(work.link, REPORT_ID %in% cv.dups$REPORT_ID)
work.dups.tender <- filter(work.link, T_REPORT_ID %in% tender.dups$T_REPORT_ID)

work.dups <- rbind(work.dups.cv, work.dups.tender)

vessel.issues <- filter(work.link, CV_ID %in% work.dups$CV_ID)

# Save --------------------------------------------------------------------

# Remove any remaining unwanted objects and save data
rm(helper_objects, channel_afsc, ADP_Output_dribble)

# Save
save.image(file = "2_AR_data.Rdata")
gdrive_upload("2_AR_data.Rdata", AnnRpt_DepChp_dribble)
