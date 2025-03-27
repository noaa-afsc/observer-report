# Setup -------------------------------------------------------------------

# Get packages and user-defined functions
source("3_helper.R")

# Report year (year that fishing and observing took place)
year <- 2024

# Establish database connection
channel_akro <- eval(parse(text = Sys.getenv("channel_cas")))
channel_afsc  <- eval(parse(text = Sys.getenv('channel_afsc')))

# Get data ----------------------------------------------------------------

# Assign the address of the Annual Report Project in the Shared Gdrive
data_dribble <- gdrive_set_dribble("Data")

# * Observer costs ----

#' Get actual costs from spreadsheet provided by FMA (make sure spreadsheet is up-to-date!). 
#' Initially, download the following spreadsheet as a `.csv` format file to the `source_data/` folder. 
#' Upload an `.rdata` copy to the [FMA shared Gdrive].
#' [https://docs.google.com/spreadsheets/d/1KGmZNo7uVuB6FCVRZd4UV2PROa4Jl7-J/edit?usp=sharing&ouid=112928343270640187258&rtpof=true&sd=true]
if(FALSE) {
  FMA_Days_Paid <- read.csv("source_data/FMA Days Paid.xlsx - Days_Paid.csv")
  save(FMA_Days_Paid, file = "source_data/FMA_Days_Paid.rdata")
  gdrive_upload("source_data/FMA_Days_Paid.rdata", data_dribble)
}

gdrive_download("source_data/FMA_Days_Paid.rdata", data_dribble)
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
gdrive_download("source_data/valhalla.Rdata", data_dribble)
load("source_data/valhalla.Rdata")
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
    odds.ODDS_RANDOM_NUMBER.getControlPct(a.original_embark_date, b.strata) as ODDS_SELECTION_PCT, 
    a.trip_plan_log_seq, a.trip_status_code, a.vessel_seq, EXTRACT(YEAR FROM a.original_embark_date) AS YEAR,
    a.original_embark_date, a.planned_embark_date, a.tender_trip_flag, a.trip_plan_number,
    b.trip_stratas_seq, b.trip_monitor_code, b.trip_selected,  b.random_number_used, b.strata AS STRATA_CODE, 
    b.user_reqst_coverage, b.inherit_trip_seq, 
    c.group_code, c.description AS STRATUM_DESCRIPTION,
    e.release_comment,
    f.description AS RELEASE_STATUS_DESCRIPTION,
    g.akr_vessel_id AS VESSEL_ID,
    h.gear_type_code,
    i.fish_area_mneum AS FISH_AREA
   
  FROM odds.odds_trip_plan_log a
    LEFT JOIN odds.odds_trip_stratas b
      ON a.trip_plan_log_seq = b.trip_plan_log_seq 
    LEFT JOIN odds.odds_strata c
      ON b.strata = c.strata
    LEFT JOIN odds.odds_strata_release e
      ON b.trip_stratas_seq = e.trip_stratas_seq
    LEFT JOIN odds.odds_lov_release_status f
      ON e.release_status_seq = f.release_status_seq
    LEFT JOIN norpac.atl_lov_vessel g
      ON a.vessel_seq = g.vessel_seq
    LEFT JOIN odds.odds_trip_gear h
      ON a.trip_plan_log_seq = h.trip_plan_log_seq
    LEFT JOIN odds.odds_trip_fish_area i
      ON a.trip_plan_log_seq = i.trip_plan_log_seq
  WHERE EXTRACT(YEAR FROM a.original_embark_date) =", year)

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

#' STRATA_CODE 96, 97, 196, and 197 is used for at-sea observer compliance trips and 98/198 is used for at-sea fixed 
#' gear EM trips. Trips are placed in these strata when USER_REQST_COVERAGE = 'Y' only when RANDOM_NUMBER_USED does not
#' result in selection. We need to recode these trips in their original strata, and we will use the USER_REQST_COVERAGE 
#' flag to identify which trips did not under truly random selection.
#' USER_REQST_COVERAGE is 'Y' only when fixed gear trips are logged with MULTIPLE_AREA = 'Y' and either IFQ_FLAG or CDQ_FLAG
#' = 'Y' AND when the user accepts monitoring when prompted, stating they my plan on catching more tonnage than any of
#' their area-specific quotas.

# Translate GROUP_CODE, STRATA_CODE, and GEAR_TYPE_CODE into STRATA
odds.dat <- mutate(odds.dat, STRATA = case_when(
    STRATA_CODE %in% c(96, 97, 196, 197) & FISH_AREA == "BSAI" ~ "OB FIXED BSAI",
    STRATA_CODE %in% c(96, 97, 196, 197) & FISH_AREA == "GOA" ~ "OB FIXED GOA",
    STRATA_CODE %in% c(98, 198) & FISH_AREA == "BSAI" ~ "EM FIXED BSAI",
    STRATA_CODE %in% c(98, 198) & FISH_AREA == "GOA" ~ "EM FIXED GOA",
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
)
odds.dat %>% distinct(STRATA) %>% arrange(STRATA)
if(any(odds.dat$STRATA %like% "Unknown")) stop("Some `STRATA` are not yet defined!")

# Create a lookup table for strata in partial coverage category
#' \TODO from GMM: I don't think we need `GEAR`. 4_AR tends to have to exclude it before joins or else the fixed gear strata 
#' result in multiple matches
partial <- odds.dat %>%
  # Exclude the compliance strata
  filter(is.na(USER_REQST_COVERAGE) | USER_REQST_COVERAGE == "N" ) %>% 
  group_by(YEAR, STRATA, GEAR_TYPE_CODE) %>%
  distinct(Rate = ODDS_SELECTION_PCT / 100 ) %>%
  ungroup() %>%
  mutate(
    GEAR = case_match(GEAR_TYPE_CODE, 3 ~ "Trawl", 6 ~ "Pot", 8 ~ "Hook-and-line"),
    Rate = ifelse(STRATA == "EM TRW GOA (EFP)", 0.3333, Rate),
    formatted_strat = paste0("*", STRATA, "*")) %>%
  distinct(YEAR, STRATA, Rate, GEAR, formatted_strat) 
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

# * Data timeliness ----
em_trip_end <- dbGetQuery(channel_afsc, paste("
select distinct extract(year from t.trip_start_date_time) year, t.trip_plan_log_seq odds_trip_number,
trunc(max(t.trip_end_date_time)) as trip_end
from em_pac_review.em_trip t
where extract(year from t.trip_start_date_time) =", year, "
group by extract(year from t.trip_start_date_time), t.trip_plan_log_seq
order by t.trip_plan_log_seq"))

em_data_available <- dbGetQuery(channel_akro, paste("
select h.ODDS_TRIP_NUMBER, 
-- sampling strata information
case when ss.sampling_strata_code in ('F', 'EM_TrawlFullCoverage') then 'FULL' else 'PARTIAL' end as coverage_type,
ss.sampling_strata_code as strata,
trunc(min(r.EFFECTIVE_DATE)) data_available
from AKFISH_REPORT.CATCH_REPORT_SOURCE r
join AKFISH_REPORT.CATCH_REPORT_SPECIES_FACT f on r.CATCH_REPORT_SOURCE_PK = f.CATCH_REPORT_SOURCE_PK
join AKFISH_REPORT.EM_HAUL h on f.EM_HAUL_PK = h.EM_HAUL_PK
join AKFISH_REPORT.CALENDAR_DATE c on f.REPORT_DATE_PK = c.CALENDAR_DATE_PK
join akfish_report.sampling_strata ss on ss.sampling_strata_pk = f.sampling_strata_pk
where r.year_pk >=", year-1, "
and r.CATCH_REPORT_TYPE_CODE = 'EM'
and ss.sampling_strata_code != 'N/A'
group by h.ODDS_TRIP_NUMBER,
case when ss.sampling_strata_code in ('F', 'EM_TrawlFullCoverage') then 'FULL' else 'PARTIAL' end,
ss.sampling_strata_code
order by h.ODDS_TRIP_NUMBER"))

ob_trips <- dbGetQuery(channel_afsc, paste("
select extract(year from t.start_date) as year, t.cruise, t.permit, t.trip_seq, t.end_date as trip_end
from norpac.atl_fma_trip t
join norpac.ols_observer_cruise cru on cru.cruise = t.cruise
join norpac.ols_observer_contract con on con.contract_number = cru.contract_number
where extract(year from t.start_date) =", year, "
and t.did_fishing_occur_flag = 'Y'"))

ob_hauls <- dbGetQuery(channel_akro, paste("
select h.cruise, to_char(v.vessel_id) as permit, h.trip_seq,
-- sampling strata information
case when ss.sampling_strata_code in ('F', 'EM_TrawlFullCoverage') then 'FULL' else 'PARTIAL' end as coverage_type,
ss.sampling_strata_code as strata,
trunc(min(r.EFFECTIVE_DATE)) data_available
from AKFISH_REPORT.CATCH_REPORT_SOURCE r
join AKFISH_REPORT.CATCH_REPORT_SPECIES_FACT f on r.CATCH_REPORT_SOURCE_PK = f.CATCH_REPORT_SOURCE_PK
join AKFISH_REPORT.OBSERVER_HAUL h on f.OBSERVER_HAUL_PK = h.OBSERVER_HAUL_PK
join AKFISH_REPORT.CALENDAR_DATE c on f.REPORT_DATE_PK = c.CALENDAR_DATE_PK
join akfish_report.sampling_strata ss on ss.sampling_strata_pk = f.sampling_strata_pk
join akfish_report.vessel v on v.vessel_pk = f.vessel_pk
where r.year_pk >=", year-1, "
and r.CATCH_REPORT_TYPE_CODE = 'OBS'
and ss.sampling_strata_code != 'N/A'
group by h.cruise, v.vessel_id, h.trip_seq,
case when ss.sampling_strata_code in ('F', 'EM_TrawlFullCoverage') then 'FULL' else 'PARTIAL' end,
ss.sampling_strata_code"))

afsc_offloads <- dbGetQuery(channel_afsc, paste("
select distinct extract(year from delivery_end_date) as year, to_char(offload_join) offload_join, delivery_end_date, date_of_entry
from obsint.offload_em_trawl_received_v
where extract(year from delivery_end_date) =", year))

akro_offloads <- dbGetQuery(channel_akro, paste("
with j as (
    -- sub query to only include active offloads
    select distinct offload_join
    from akfish_report.catch_report_source
    where expire_date is null
    and catch_report_type_code = 'EM_OFFLOAD'
    and year_pk =", year, "
    ), q as (
    select to_char(r.offload_join) offload_join,
    -- sampling strata information
    case when ss.sampling_strata_code in ('F', 'EM_TrawlFullCoverage') then 'FULL' else 'PARTIAL' end as coverage_type,
    ss.sampling_strata_code as strata
    from akfish_report.catch_report_source r
    join akfish_report.catch_report_species_fact f on f.catch_report_source_pk = r.catch_report_source_pk
    join akfish_report.sampling_strata ss on ss.sampling_strata_pk = f.sampling_strata_pk
    where r.catch_report_type_code = 'EM_OFFLOAD'
      and r.year_pk =", year, "
      and r.offload_join in (select * from j)
      and ss.sampling_strata_code != 'N/A'
) select distinct * from q"))

em_data_timeliness <- em_trip_end %>% 
  inner_join(
    em_data_available, 
    by = "ODDS_TRIP_NUMBER"
  ) %>% 
  select(YEAR, TRIP_NUMBER = ODDS_TRIP_NUMBER, COVERAGE_TYPE, STRATA, TRIP_END, DATA_AVAILABLE) %>% 
  mutate(TRIP_END = as.Date(TRIP_END),
         DATA_AVAILABLE = as.Date(DATA_AVAILABLE),
         data_timeliness = as.numeric(difftime(DATA_AVAILABLE, TRIP_END, units = "days")))

ob_data_timeliness <- ob_trips %>% 
  inner_join(
    ob_hauls,
    by = c("CRUISE", "PERMIT", "TRIP_SEQ")
  ) %>% 
  mutate(TRIP_NUMBER = paste0(CRUISE, PERMIT, TRIP_SEQ), 
         TRIP_END = as.Date(TRIP_END),
         DATA_AVAILABLE = as.Date(DATA_AVAILABLE)) %>% 
  select(YEAR, TRIP_NUMBER, COVERAGE_TYPE, STRATA, TRIP_END, DATA_AVAILABLE) %>%  
  mutate(data_timeliness = as.numeric(difftime(DATA_AVAILABLE, TRIP_END, units = "days")))

offload_data_timeliness <- afsc_offloads %>% 
  inner_join(
    akro_offloads, 
    by = "OFFLOAD_JOIN"
  ) %>% 
  select(YEAR, TRIP_NUMBER = OFFLOAD_JOIN, COVERAGE_TYPE, STRATA, TRIP_END = DELIVERY_END_DATE, DATA_AVAILABLE = DATE_OF_ENTRY) %>% 
  mutate(TRIP_END = as.Date(TRIP_END),
         DATA_AVAILABLE = as.Date(DATA_AVAILABLE),
         data_timeliness = as.numeric(difftime(DATA_AVAILABLE, TRIP_END, units = "days")) + 1)

data_timeliness <- rbind(em_data_timeliness, ob_data_timeliness, offload_data_timeliness) %>% 
  mutate(STRATA = recode(STRATA, 
                         'EM_H' = 'EM HAL',
                         'EM_P' = 'EM POT',
                         'EM_TrawlFullCoverage' = 'EM TRW (EFP)',
                         'EM_TrawlPartialCoverage' = 'EM TRW (EFP)',
                         'EM_TrawlPartialCoverageTen' = 'EM TRW (EFP)',
                         'F' = 'FULL',
                         'H' = 'HAL',
                         'N' = 'ZERO',
                         'N/A' = 'NA',
                         'P' = 'POT',
                         'TR' = 'TRW',
                         'EM_FG_BSAI' = 'EM FIXED BSAI',
                         'EM_FG_GOA' = 'EM FIXED GOA',
                         'EM_TR_BSAI' = 'EM TRW BSAI (EFP)',
                         'EM_TR_GOA' = 'EM TRW GOA (EFP)',
                         'FG_BSAI' = 'OB FIXED BSAI',
                         'FG_GOA' = 'OB FIXED GOA',
                         'TR_BSAI' = 'OB TRW BSAI',
                         'TR_GOA' = 'OB TRW GOA')) %>% 
  filter(STRATA %in% c("EM FIXED BSAI", "EM FIXED GOA", "EM TRW BSAI (EFP)", "EM TRW GOA (EFP)", "OB FIXED BSAI", "OB FIXED GOA", "OB TRW BSAI", "OB TRW GOA")) %>% 
  mutate(data_timeliness = ifelse(data_timeliness < 0, 0, data_timeliness)) %>% 
  group_by(TRIP_NUMBER) %>% 
  filter(n_distinct(COVERAGE_TYPE) == 1 & n_distinct(STRATA) == 1) %>% 
  ungroup()

# * EM research ----
em_research <- dbGetQuery(channel_afsc, paste(" select distinct adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status
                                              from loki.em_vessels_by_adp
                                              where sample_plan_seq_desc = 'Electronic Monitoring -  research not logged '
                                              and adp =", year))

# Identify trips by EM research vessels
work.data <- work.data %>% 
  mutate(STRATA = ifelse(VESSEL_ID %in% em_research$VESSEL_ID, "Zero EM Research", STRATA))

# * Shapefiles ----
# Initial upload to Shared Gdrive
if(FALSE) gdrive_upload("source_data/ak_shp.rdata", data_dribble)
## Load land and NMFS stat area shapefiles 
gdrive_download("source_data/ak_shp.rdata", data_dribble)
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
  mutate(PERMIT = as.integer(PERMIT),
         ADFG_NUMBER = as.integer(ADFG_NUMBER))

eland.offload <- dbGetQuery(channel_afsc, script) %>%
  filter(REPORT_ID %in% em_trw_offload$REPORT_ID) %>%
  mutate(TENDER_OFFLOAD_DATE = as.Date(TENDER_OFFLOAD_DATE),
         TENDER_VESSEL_ADFG_NUMBER = as.integer(TENDER_VESSEL_ADFG_NUMBER)) %>%
  left_join(vessels, by = join_by(VESSEL_ID == PERMIT)) %>%
  rename(CV_NAME = NAME, CV_ID = VESSEL_ID) %>%
  select(-ADFG_NUMBER) %>%
  left_join(vessels, by = join_by(TENDER_VESSEL_ADFG_NUMBER == ADFG_NUMBER)) %>%
  rename(TENDER_NAME = NAME, TENDER_ID = PERMIT) %>%
  # Create T_REPORT_ID to identify every CV offload to a tender vessel
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
#' \TODO *2024 AR* Remove this check once VALHALLA trip assignment code is updated and verified to be assigning trips correctly

# Get observer offload data and combine with VALHALLA using eLandings data as the linkage
script <- paste(
  "SELECT o.landing_report_id AS report_id, o.cruise, o.permit AS processor_permit_id,
      v.permit, v.name, o.delivery_end_date, o.offload_number, v.adfg_number,
      o.delivered_weight, o.lb_kg, o.offload_to_tender_flag,
      CASE WHEN EXISTS (
        SELECT 1 FROM norpac.atl_salmon 
        WHERE cruise = o.cruise AND permit = o.permit AND offload_seq = o.offload_seq)
      THEN 'Y' ELSE 'N' END as obs_salmon_cnt_flag --Identifies where salmon counts were done
   FROM norpac.atl_offload o
   LEFT JOIN norpac.atl_lov_vessel v
      ON o.delivery_vessel_adfg = v.adfg_number
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
  # For tender offloads, Observers do not report the landing report IDs
  filter(is.na(REPORT_ID)) %>%
  select(-OFFLOAD_NUMBER, -REPORT_ID) %>% # OFFLOAD_NUMBER is useful for finding any duplicates
  # Create T_REPORT_ID to identify every CV delivery to a tender vessel
  mutate(T_REPORT_ID = paste0(ADFG_NUMBER, DELIVERY_END_DATE)) %>%
  rename(TENDER_ID = PERMIT,
         TENDER_NAME = NAME,
         TENDER_VESSEL_ADFG_NUMBER = ADFG_NUMBER,
         TENDER_OFFLOAD_DATE = DELIVERY_END_DATE) %>%
  distinct()

val.tender <- filter(work.eland, !is.na(T_REPORT_ID))

# Join tender offloads in Valhalla/eLandings with those in NORPAC
tender.link <- val.tender %>%
  left_join(obs.tender, by = join_by(TENDER_ID, TENDER_NAME, T_REPORT_ID, TENDER_OFFLOAD_DATE,
                                     TENDER_VESSEL_ADFG_NUMBER))

# Observer non-tender offloads
obs.cv <- obs.offload %>%
  filter(REPORT_ID %in% work.eland$REPORT_ID) %>%
  filter(!is.na(REPORT_ID)) %>%
  select(-OFFLOAD_NUMBER)

val.cv <- filter(work.eland, is.na(T_REPORT_ID))

# Join CV offloads in Valhalla/eLandings with those in NORPAC
cv.link <- val.cv %>%
  left_join(obs.cv, by = join_by(REPORT_ID, T_REPORT_ID)) %>%
  select(-PERMIT, -NAME, -ADFG_NUMBER, -DELIVERY_END_DATE) %>%
  mutate(TENDER_OFFLOAD_DATE = as.Date(TENDER_OFFLOAD_DATE))
  
# Combine tenders and CVs to create final dataframe
work.offload <- tender.link %>%
  rbind(cv.link) %>%
  relocate(TRIP_ID, REPORT_ID, T_REPORT_ID, CV_ID, CV_NAME, TENDER_ID, TENDER_VESSEL_ADFG_NUMBER, TENDER_NAME,
           LANDING_DATE, TENDER_OFFLOAD_DATE, PORT_CODE, TENDER, OFFLOAD_TO_TENDER_FLAG, OBSERVED_FLAG,
           OBS_SALMON_CNT_FLAG) %>%
  #'* 2024 AR only: * REPORT_ID missing from observer data, need to hardcode that it wasn't monitored
  #'                  VALHALLA says it was an unmonitored offload and it wasn't a tendered delivery
  mutate(across(c(OBS_SALMON_CNT_FLAG, OFFLOAD_TO_TENDER_FLAG), ~ ifelse(REPORT_ID == 9543781, "N", .x)))

# Check to make sure all VALHALLA REPORT_IDs have a match in observer data
if (any(is.na(work.offload$OBS_SALMON_CNT_FLAG))) {
  cat("\033[31mREPORT_IDs exist that are not in observer data\033[39m\n")
} else {
  cat("\033[32mAll REPORT_IDs present in observer data\033[39m\n")
}

# Check port assignments to see if there are mismatches between where a processor is actually located
#  and what was reported in VALHALLA
#  If there are any mismatches, the processor_port object will be created and these can then be checked
#  using VMS and observer deployment data
# Output will provide the offloads in question and a list of where the observer was stationed on those dates

#'* 2024 AR only: * These 4 tender deliveries were at AKU. CVs offloaded to the tenders at SPT, but
#'                  tenders did not deliver to SPT, instead they traveled directly to AKU and delivered
#'                  unsorted catch where observers were able to monitor these deliveries (info from Joel K. on Mar 24).
#'                  Phil G. working on getting these records updated on AKRO end to reflect AKU as the port.
#'* 2025 AR note: * In 2024, CVs are offloading catch at the plant in SPT where it is sorted and monitored by
#'                  observers. Some of these offloads are then being pumped onto a tender to be taken to
#'                  AKU for processing. Observers should not be monitoring/recording these deliveries at AKU
processor_port <- dbGetQuery(channel_afsc,
                             paste("SELECT d.permit, p.name AS plant_port
                                    FROM norpac.atl_lov_plant d
                                    LEFT JOIN norpac.atl_lov_port_code p
                                      ON d.port_code = p.port_code")) %>%
  right_join(work.offload, by = join_by(PERMIT == PROCESSOR_PERMIT_ID)) %>%
  mutate(PLANT_PORT = case_when(PLANT_PORT == "Akutan" ~ "AKU",
                                PLANT_PORT == "Dutch Harbor" ~ "DUT",
                                PLANT_PORT == "False Pass" ~ "FSP",
                                PLANT_PORT == "Kodiak" ~ "KOD",
                                PLANT_PORT == "Sand Point" ~ "SPT",
                                PROCESSOR_NAME == "NORTHERN VICTOR" ~ "DUT")) %>%
  select(CRUISE, REPORT_ID, T_REPORT_ID, PLANT_PORT, PORT_CODE, PROCESSOR_NAME, PERMIT,
         CV_ID, CV_NAME, TENDER_ID, TENDER_NAME, STRATA, LANDING_DATE, TENDER_OFFLOAD_DATE) %>%
  filter(PLANT_PORT != PORT_CODE)
  
if (nrow(processor_port) > 0) {
  print(processor_port)
  cat("\033[31mPLANT_PORT (port_code based on permit) and PORT_CODE 
      (valhalla) mismatch for some EM offloads\033[39m\n") # red
  
  # Check port using the plant observer assignments
  obs_port <- dbGetQuery(channel_afsc,
                         paste("SELECT dl.deployed_date, vp.permit, vp.cruise, p.name
                FROM norpac.ols_vessel_plant vp 
                JOIN TABLE(norpac.ole_statement_factors_pkg.get_deployed_dates_list_vp_seq(vp.vessel_plant_seq))  dl
                  ON vp.vessel_plant_seq = dl.vessel_plant_seq
                LEFT JOIN norpac.atl_lov_plant p
                  ON vp.permit = p.permit
                WHERE vp.cruise  IN (",
                               paste(processor_port %>%
                                       select(CRUISE) %>%
                                       distinct() %>%
                                       unlist(use.names = FALSE), collapse = ",") ,")")) %>%
    mutate(DEPLOYED_DATE = as.Date(DEPLOYED_DATE)) %>%
    filter(DEPLOYED_DATE %in% coalesce(processor_port$TENDER_OFFLOAD_DATE, processor_port$LANDING_DATE))
  
  print(obs_port)
  cat("\033[31mObserver plant assignments for these deliveries\033[39m\n") # red
  rm(obs_port)
} else {
  cat("\033[32mEM offload ports match\033[39m\n") # green
}

#'* 2024 AR only: * Replace PORT_CODE with PLANT_PORT for the records with a mismatch
work.offload <- work.offload %>%
  left_join(processor_port %>% select(REPORT_ID, PLANT_PORT), by = "REPORT_ID") %>%
  mutate(PORT_CODE = ifelse(!is.na(PLANT_PORT), PLANT_PORT, PORT_CODE)) %>%
  select(-PLANT_PORT)

# Clean up workspace
rm(vessels, val.tender, val.cv, tender.link, cv.link, work.eland, obs.tender, obs.cv, obs.offload, em_trw_offload,
   eland.offload, script)

# Evaluate differences between what VALHALLA says is observed compared to what observer records say
#'* Remove code below when finalizing code *
#' This code creates objects so we can see where there are mismatches between what is flagged as observed in
#' VALHALLA versus what NORPAC says was monitored. Once we update VALHALLA logic or decide on another method
#' for assigning observed flag to EM Trawl offloads we don't need these checks anymore.
work.obs.cv <- filter(work.offload, is.na(T_REPORT_ID))
nrow(filter(work.obs.cv, (OBSERVED_FLAG == "Y" & OBS_SALMON_CNT_FLAG == "N") |
              (OBSERVED_FLAG == "N" & OBS_SALMON_CNT_FLAG == "Y")))

cv.dups <- filter(work.obs.cv, (OBSERVED_FLAG == "Y" & OBS_SALMON_CNT_FLAG == "N") |
                    (OBSERVED_FLAG == "N" & OBS_SALMON_CNT_FLAG == "Y"))

work.obs.tender <- filter(work.offload, !is.na(T_REPORT_ID))
nrow(filter(work.obs.tender, (OBSERVED_FLAG == "Y" & OBS_SALMON_CNT_FLAG == "N") |
              (OBSERVED_FLAG == "N" & OBS_SALMON_CNT_FLAG == "Y")))

tender.dups <- filter(work.obs.tender, (OBSERVED_FLAG == "Y" & OBS_SALMON_CNT_FLAG == "N") |
                        (OBSERVED_FLAG == "N" & OBS_SALMON_CNT_FLAG == "Y"))

work.dups.cv <- filter(work.offload, REPORT_ID %in% cv.dups$REPORT_ID)
work.dups.tender <- filter(work.offload, T_REPORT_ID %in% tender.dups$T_REPORT_ID)

work.dups <- rbind(work.dups.cv, work.dups.tender)

vessel.issues <- filter(work.offload, CV_ID %in% work.dups$CV_ID)

rm(work.obs.cv, cv.dups, work.obs.tender, tender.dups, work.dups.cv, work.dups.tender, work.dups, vessel.issues)

# Save --------------------------------------------------------------------

save(year, days_paid, obs_act_days, predicted, bud_scen_lst, bud_tbl, work.data, partial, salmon.landings.obs, 
     odds.dat, EM.data, data_timeliness, shp_centroids, shp_land, shp_nmfs, work.offload,
     file = "2_AR_data.Rdata")
gdrive_upload("2_AR_data.Rdata", gdrive_set_dribble("Projects/AnnRpt-Deployment-Chapter"))
