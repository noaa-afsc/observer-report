# Setup -------------------------------------------------------------------

# Get packages and user-defined functions
source("3_helper.R")
# Get list of all items in helper that can be excluded from output of 2_AR.data.rdata
helper_objects <- ls()

# Random number seed
set.seed(052870)

# Report year (year that fishing and observing took place)
year <- 2023

# The user's physical location when running this code (used to pull data from the closest database)
location <- toupper(getPass('What is your current physical location? (Juneau or Seattle)'))

# Establish database connections
channel_afsc  <- channel.fxn(location, db = "AFSC")

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
#' [ver1] *was used for the 2023 Annual Report*
gdrive_download("source_data/FMA_Days_Paid.rdata", AnnRpt_DepChp_dribble)
load("source_data/FMA_Days_Paid.rdata")

#' *2023 AR* create separate days paid 
days_paid.2022 <- filter(FMA_Days_Paid, Calendar == 2022)
days_paid.2023 <- filter(FMA_Days_Paid, Calendar == 2023)

# * ADP outputs ----

# Assign the address of the ADP outputs in the Shared Gdrive
ADP_Output_dribble <- gdrive_set_dribble("Projects/ADP/Output")

# Initially:
#' `2022_final_adp_repository/data/2022-FINAL-ADP_2021-11-17_i1000-o1000-seed12345.RData`, originally saved at
#' [https://drive.google.com/file/d/1nEQNXV4s0bJJb8lGha7AKsnubItdCU8H/view?usp=drive_link], renamed and uploaded to the
#' shared Gdrive in `Projects/ADP/Output/` folder as `2022_Final_ADP_output.rdata`
if(FALSE) gdrive_upload("source_data/2022_Final_ADP_Output.rdata", ADP_Output_dribble)

#' `2023_final_adp_repository/data/2023-FINAL-ADP_2022-11-17_i1000-o1000-seed12345`, originally saved at
#' [https://drive.google.com/file/d/1Sb9gwpRnG67Azikc6BY1WMfd2cP_NNfR/view?usp=drive_link], renamed and uploaded to the 
#' shared Gdrive in `Projects/ADP/Output` folder as `2023_Final_ADP_output.rdata`
if(FALSE) gdrive_upload("source_data/2023_Final_ADP_Output.rdata", ADP_Output_dribble)

# 2022 Final ADP Outputs
gdrive_download("source_data/2022_Final_ADP_Output.rdata", ADP_Output_dribble)
final_adp_vec.2022 <- (load("source_data/2022_Final_ADP_Output.rdata"))
Nnd_tbl.2022 <- mutate(Nnd_tbl, YEAR = 2022)
adj_tbl.2022 <- adj_tbl
bud_scen_lst.2022 <- bud_scen_lst
bud_tbl.2022 <- bud_tbl
rm(list = c(final_adp_vec.2022, "final_adp_vec.2022"))

# 2023 Final ADP Outputs
gdrive_download("source_data/2023_Final_ADP_Output.rdata", ADP_Output_dribble)
final_adp_vec.2023 <-(load("source_data/2023_Final_ADP_Output.rdata"))
Nnd_tbl.2023 <- mutate(Nnd_tbl, YEAR = 2023)
adj_tbl.2023 <- adj_tbl
bud_scen_lst.2023 <- bud_scen_lst
bud_tbl.2023 <- bud_tbl
rm(list = c(final_adp_vec.2023, "final_adp_vec.2023"))

# Format and calculate predicted days from ADP
Nnd_tbl <- rbind(Nnd_tbl.2022, Nnd_tbl.2023)

predicted <- Nnd_tbl[
][, lapply(.SD, mean), keyby = .(YEAR, POOL, STRATA), .SDcols = c("Nh", "nh", "dh")] %>%
  # Remove zero stratum
  filter(POOL != "ZE") %>% 
  mutate(STRATA = case_when(STRATA == "EM_PLK" ~ "EM TRW EFP", #' *2023 EM TRW EFP number off from ADP report value for some reason*
                            STRATA == "TRW" & POOL == "OB" ~ "OB TRW",
                            STRATA == "POT" & POOL == "OB" ~ "OB POT",
                            STRATA == "HAL" & POOL == "OB" ~ "OB HAL",
                            STRATA == "EM_POT" ~ "EM POT",
                            STRATA == "EM_HAL" ~ "EM HAL",
                            TRUE ~ STRATA)) %>%
  rename(pred_days = dh) %>%
  select(!c(POOL, Nh, nh))

#' *2023 fix: changed EM TRW EFP to value presented in 2023 ADP*
#' *Calculated value was using 30% (what is set in the sampling plan), but ADP used 33% (what is done in practice)*
predicted[8, 3] <- 486

# * Valhalla ----

#' Initial upload of [2022] Valhalla to the Shared Gdrive. Querying the latest year, 2022, from `loki.valhalla`.
#' (run date of 2023-04-10 15:15:15 PDT)
if(FALSE){
  valhalla.2022 <- setDT(dbGetQuery(channel_afsc, paste("SELECT * FROM loki.akr_valhalla WHERE adp = 2022")))
  valhalla.2022[, TRIP_TARGET_DATE := as.Date(TRIP_TARGET_DATE)]
  valhalla.2022[, LANDING_DATE := as.Date(LANDING_DATE)]
  save(valhalla.2022, file = "source_data/2023-04-10cas_valhalla.Rdata")
  gdrive_upload("source_data/2023-04-10cas_valhalla.Rdata", AnnRpt_DepChp_dribble)
}
gdrive_download("source_data/2023-04-10cas_valhalla.Rdata", AnnRpt_DepChp_dribble)
(load("source_data/2023-04-10cas_valhalla.Rdata"))

#' Initial upload of [2023] Valhalla to the Shared Gdrive. Originally obtained from:
#' [https://drive.google.com/drive/u/0/folders/1JK0EJDBByn7GyfDt2q96ZBjvjYuhezOF]
if(FALSE) gdrive_upload("source_data/2024-04-15cas_valhalla.Rdata", AnnRpt_DepChp_dribble)
gdrive_download("source_data/2024-04-15cas_valhalla.Rdata", AnnRpt_DepChp_dribble)
(load("source_data/2024-04-15cas_valhalla.Rdata"))

#' Create a copy of Valhalla named 'work.data' that will be manipulated
work.data <- rbind(valhalla, valhalla.2022)
rm(valhalla, valhalla.2022); gc()

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
#' *2023 AR* Addressing trips that spanned years
unique(work.data[TRIP_ID == 1593889, .(ADP, TRIP_ID, TRIP_TARGET_DATE)])[order(TRIP_TARGET_DATE)]
unique(work.data[TRIP_ID == 1615161, .(ADP, TRIP_ID, TRIP_TARGET_DATE)])[order(TRIP_TARGET_DATE)]
# Will assign `ADP` to 2023 for these trips
work.data[TRIP_ID %in% c(1593889, 1615161), ADP := 2023]

# Format strata names
work.data <- work.data %>%
  mutate(STRATA = recode(
    STRATA,
    "POT" = "OB POT",
    "HAL" = "OB HAL",
    "TRW" = "OB TRW",
    "EM_POT" = "EM POT",
    "EM_HAL" = "EM HAL",
    "EM_TRW_EFP" = "EM TRW EFP"))

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

#' *2023 AR* Scraping all the non-observer strata matches to their observed strata counterparts. FMA is charged for sea
#' days regardless of the observer was supposed to monitor these trips or not.
mod_dat_copy <- copy(mod_dat)
mod_dat_copy[
][STRATA == "EM TRW EFP" | STRATA == "TRW", STRATA := "OB TRW"
][STRATA == "EM HAL" | STRATA == "HAL", STRATA := "OB HAL"
][STRATA == "EM POT" | STRATA == "POT", STRATA := "OB POT"
][STRATA == "ZERO", STRATA := "OB HAL"]
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
WHERE o.delivery_end_date BETWEEN '01-JAN-", year - 1, "' AND '31-DEC-", year, "' -- for 2023 AR, need mulitple years
ORDER BY OBS_COVERAGE_TYPE, REPORT_ID")

# The following query returns all landing ID's for offloads monitored for salmon all sectors.
salmon.landings.obs <- dbGetQuery(channel_afsc, script)

# Data checks and clean up

# Number of offloads monitored for salmon by Observer Coverage Type (Full vs Partial)
salmon.landings.obs  %>%  group_by(OBS_COVERAGE_TYPE) %>% summarise(n = n())

# * ODDS ----

# Queries
#' *For 2023 AR, pulling 2 prior years instead of 1 to back-fill analyses that were skipped in 2022 AR*
script <- paste("
  SELECT 
    -- Retrieve a trip's selection pecentage based on the trip's original declared embark date and stratum
    odds.ODDS_RANDOM_NUMBER.getControlPct(a.original_embark_date, b.strata) as ODDS_SELECTION_PCT,     

    a.trip_plan_log_seq, a.trip_status_code, a.vessel_seq, EXTRACT(YEAR FROM a.original_embark_date) AS YEAR,
    a.original_embark_date, a.planned_embark_date, a.tender_trip_flag, a.trip_plan_number,
    b.trip_stratas_seq, b.trip_monitor_code, b.trip_selected,  b.random_number_used, b.strata AS STRATA_CODE, 
    -- inherit_trip_seq corresponds to the trip_stratas_seq that was cancelled, not trip_plan_log_seq before ODDS 3.0!
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
  WHERE EXTRACT(YEAR FROM a.original_embark_date) IN (", paste(year + -1:0, collapse = ","), ")
    -- [2023 Annual report Only]
    -- Exclude a 2024 trip with original embark date in 2023 that makes the ODDS_RANDOM_NUMBER package throw an error.
    AND a.trip_plan_log_seq != 202328923             
")

odds.dat <- dbGetQuery(channel_afsc, script)

# Data checks and clean up

# Check for duplicates - should be no records (= 0)
if(sum(duplicated(odds.dat$TRIP_PLAN_LOG_SEQ))) stop("Some 'TRIP_PLAN_LOG_SEQ' are duplicated!")

#'*====================================================================================================================*
#' FIXME `ODDS 3.0 is not creating records in odds.odds_strata_release for trips auto-released by the three-in-a-row`
#' `rule. Andy Kingham has been notified to remedy this, but we will hard-code these 2 identified instances here`

setDT(odds.dat)[TRIP_PLAN_LOG_SEQ %in% c(202322990, 202317623), ':=' (
  RELEASE_COMMENT = "Three Observerd Trips Release",
  RELEASE_STATUS_DESCRIPTION = "Three in Row Release"
)]
setDF(odds.dat)
#'*====================================================================================================================*

# Summary of trip dispositions and observer assignments 
# GROUP_CODE: 10:11 = at-sea observer, 13 = Fixed-gear EM, 14 = Trawl EM
# TRIP_STATUS_CODE: 
#   CS = Cancel by System 
#   PD = Pending
#   CN = Cancelled 
#   CP = Completed 
#   CC = Cancel Cascaded (discontinued with ODDS 3.0 in 2023)
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
    GROUP_CODE %in% 10:11 ~ case_match(GEAR_TYPE_CODE, 3 ~ "OB TRW", 6 ~ "OB POT", 8 ~ "OB HAL"),
    GROUP_CODE == 13 ~ case_match(GEAR_TYPE_CODE, 6 ~ "EM POT", 8 ~ "EM HAL"),
    GROUP_CODE == 14 ~ "EM TRW EFP",
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
  mutate(Rate = ifelse(STRATA == "EM TRW EFP", 0.3333, Rate)) %>%
  distinct(YEAR, STRATA, Rate, GEAR) %>%
  mutate(formatted_strat = paste0("*", STRATA, "*"))
partial %>% pivot_wider(names_from = YEAR, values_from = Rate)

# * EM ----
script <- paste0("SELECT * from em_pac_review.EM_TRIP
                  WHERE EXTRACT(YEAR FROM TRIP_END_DATE_TIME) IN(", paste0(year + -1:0, collapse = ","), ")")

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
                  WHERE EXTRACT(YEAR FROM END_DATE_TIME) IN(", paste0(year + -1:0, collapse = ","), ")")

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
#' *2023: one vessel has trawl gear declared, but no trawl codes in EM.gear - this trip seems to be coming from EM.data*
#' *2023: one vessel has a trip marked as NON_FISHING_TRIP = Y*
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
  mutate(AGENCY_GEAR_CODE = ifelse(VESSEL_ID %in% single_gear_nas$VESSEL_ID[AGENCY_GEAR_CODE == "POT"] & is.na(AGENCY_GEAR_CODE), "POT", AGENCY_GEAR_CODE)) %>% 
  # NAs for vessels that (based on ODDS) fished multiple gears in the report year are recoded manually according to the comparison made immediately above
  mutate(AGENCY_GEAR_CODE = ifelse(TRIP_NUMBER == "22_SUMNERSTRAIT01.01", "POT", AGENCY_GEAR_CODE)) %>%
  # NAs for vessels that (based on ODDS and PSMFC EM.data) did not actually fish in the EM strata 
  #' *2023 AR* Excluding records from a vessel that apparently fished trawl, not fixed gear 
  filter(VESSEL_ID  != "422") %>%
  #' *2023 AR* Another that had it's trip marked as non-fishing
  filter(TRIP_NUMBER != "22_BLACKPEARL07.01")

# The following query will provide a list of EM selected trips and if they have been reviewed or not
# Query will only include trips in completed or pending status and will not include compliance trips.
# This query will also show the declared gear type and if reviewed, will show the em_reviewed_gear_type_code
# This query will also show when the HD was received by PSFMC and when the EM reviewed data was exported and sent to AFSC
# This query will also show the actual EM trip start date and time and actual EM trip end date and time which comes from the data on the HD.

# Important note: if an EM reviewed trip used multiple gear types on a trip (i.e.,  pot and longline) there will be 2 records in the output.

#'*========== Geoff and Christian get the error: [Oracle][ODBC][Ora]ORA-01031: insufficient privileges*
#'*Skipping this section for now*
if(F){
  
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
                                              and adp IN(", paste(year + -1:0, collapse = ","), ")" ))

# Identify trips by EM research vessels
work.data <- work.data %>% 
  mutate(STRATA = ifelse(VESSEL_ID %in% em_research$VESSEL_ID, "Zero EM Research", STRATA))

# * Shapefiles ----
# Initial upload to Shared Gdrive
if(FALSE) gdrive_upload("source_data/ak_shp.rdata", AnnRpt_DepChp_dribble)
## Load land and NMFS stat area shapefiles 
gdrive_download("source_data/ak_shp.rdata", AnnRpt_DepChp_dribble)
(load(("source_data/ak_shp.rdata")))

# Save --------------------------------------------------------------------

# Remove any remaining unwanted objects and save data
rm(helper_objects, location, channel_afsc, ADP_Output_dribble)

# Save
save.image(file = "2_AR_data.Rdata")
gdrive_upload("2_AR_data.Rdata", AnnRpt_DepChp_dribble)
