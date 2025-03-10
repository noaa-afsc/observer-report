# Title:                      Statement Annual Report Analytics, v2025.1
# Author:                     Andy Kingham
# Contact:                    Andy.kingham@noaa.gov
# Create date:                20210329
###############################################################################
###############################################################################
# This is the same as V2 of the same name, except it removes OBSERVER_ROLE as a factor,
# updates GDrive functions, changes NORPAC access to use .Renvirons,
# removes query for old statements database call, and streamlines trawl EM offload query.

##########
# Set up the environment
##########

if(!require("odbc"))        install.packages("odbc",        repos='http://cran.us.r-project.org')
if(!require("plyr"))        install.packages("plyr",        repos='http://cran.us.r-project.org')
if(!require("tidyverse"))   install.packages("tidyverse",   repos='http://cran.us.r-project.org')
if(!require("devtools"))     install.packages("devtools",   repos='http://cran.us.r-project.org')
if(!require("FMAtools"))     devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")

###############################################################################
###############################################################################

# clean up first
rm(list = ls())

#Open data channel
#####################


# Define odbc connection to NORPAC
channel  <- eval(parse(text = Sys.getenv('channel_afsc')))

# Assign the address of the Annual Report Project in the Shared Gdrive
AnnRpt_EnfChp_dribble <- gdrive_set_dribble("Projects/Annual Report OLE chapter 5/2024_data")

# adp_yr is the year of the annual report we are doing this time (annual_deployment_year)
adp_yr <- rstudioapi::showPrompt(title = "ADP YEAR", message = "Enter the ADP YEAR for this analysis:", default = "")

# first_date is the first violation date for statements.  Must be the current year, PLUS the previous year.
# Generally this is 01-JAN of the year PRIOR to the ADP year..
first_date <- rstudioapi::showPrompt(title = "First Date",
                                     message ="Enter the first date for statements in this analysis.  Must be the current year, PLUS the previous year.  Generally this is 01-JAN of the year PRIOR to the ADP year.  (Use DD-MON-RR format):",
                                     default = "")

# last_date is the Last calendar day of the annual report year, generally 31-DEC of the most recent year.
last_date  <- rstudioapi::showPrompt(title = "Last Date", 
                                     message = "Enter the last date for statements in this analysis.  This is the Last calendar day of the annual report year, generally 31-DEC of the most recent year. (DD-MON-RR format):", 
                                     default = "")


# Get min and max manual_year cruise numbers.  This is only used in the next step after this one, nowhere else.
manual_year_cruises <-
  dbGetQuery(channel,
             "SELECT max(cruise) as max_cruise,
                     min(cruise) AS min_cruise,
                     manual_year
                FROM (SELECT cruise, 
                             norpac.ole_statement_factors_pkg.GetManualYearForCruise(ocr.cruise) AS manual_year
                        FROM ols_observer_cruise ocr) a
               WHERE manual_year is not null --this PL/SQL function does not handle cruises prior to 1999 so these are filtered out.          
               GROUP BY manual_year
               ")


#Set the first CRUISE NUMBER for the analysis
#IMPORTANT!!!!

# first_cruise must be several cruise numbers prior to the first cruise for the year prior to this annual report year.
# Use the results from the previous query, if needed.
# The RULE OF THUMB IS: subtract 200 from the FIRST manual_year cruise for the year PRIOR to the annual report year.
# Bottom line is you need MORE than the manual_year, because cruises deploy across the calendar year break.
# They will be filtered explicitly on DEPLOYED_DATE in a later step, this is just to ensure ALL that are needed are returned.
first_cruise <- manual_year_cruises$MIN_CRUISE[manual_year_cruises$MANUAL_YEAR == as.numeric(adp_yr)-1] - 200


########################
#SQL Queries
####################


###################
# Statements
####################

df_statements_raw  <-
  dbGetQuery(
    channel,
    paste0("SELECT norpac.ole_statement_factors_pkg.getManualYearForCruise(os.cruise) AS manual_year 
               ,   os.ole_obs_statement_seq
               ,   osd.ole_obs_statement_detail_seq
               ,   oc.category
               ,   os.cruise
               ,   os.permit
               ,   os.witness_flag
               ,   os.victim_name
               ,   os.agent
               ,   os.case_number
               ,   os.enforcement_comments
               ,   acs.status_value  AS case_status
               ,   lsc.subcategory
               ,   lr.ole_regulation_seq
               ,   lr.description    AS reg_description
               ,   lr.summary        AS reg_summary
               ,   osu.ole_obs_statement_unit_seq
               ,   lu.incident_unit
               ,   lu.definition AS unit_description
               ,   osu.answer
               ,   osd.unit_issue 
               ,   osu.data_cruise
               ,   os.comments AS statement_body
               ,   CASE WHEN os.permit = 0 OR os.permit is null 
                        THEN 'N'
                        ELSE decode(norpac.ole_statement_factors_pkg.wasCruiseAssndToVssPlnt_TF(
                                              in_cruise => os.cruise, 
                                              in_permit => os.permit),
                                    'T', 'Y', 'F', 'N')               
                    END AS was_assigned_to_permit_flag
               ,	 CASE WHEN os.permit = 0 OR os.permit is null 
                        THEN 'No Permit'
                        ELSE v.name
                    END AS vessel_plant       
                         
              FROM norpac.OLE_OBS_STATEMENT os
              JOIN norpac.ole_lov_category oc             ON oc.ole_category_seq = os.ole_category_seq
              JOIN norpac.ole_obs_statement_detail osd    ON osd.ole_obs_statement_seq = os.ole_obs_statement_seq
              JOIN norpac.ole_lov_regulation lr           ON lr.ole_regulation_seq = osd.ole_regulation_seq
              JOIN norpac.ole_lov_subcategory lsc         ON lsc.ole_subcategory_seq = lr.ole_subcategory_seq
              LEFT JOIN affidavit_case_status acs         ON acs.status = os.affidavit_case_status    
              LEFT JOIN norpac.ole_obs_statement_unit osu ON osu.ole_obs_statement_detail_seq = osd.ole_obs_statement_detail_seq      
              LEFT JOIN norpac.ole_lov_incident_unit lu   ON osu.ole_incident_unit_seq = lu.ole_incident_unit_seq
              LEFT JOIN norpac.atl_vessplant_v v          ON v.permit = os.permit      
           ")) 


#################
# First violation date
  # We DO need this in 2024, because this is how we determine if a statement belongs in the report for the year or not.
  # Making this a separate DF, because it was proving to be VERY slow in the processing of the above query.
  # Then will merge and filter in the next step.
  
  ## TODO #
  # There are 30 NAs in first_violation_date.  There was a lotta code in the original script
  # to go get data for cases where first_violation_date is null for 2023.
  # See last years script for 2023 data that need to be hard-coded.
  # leaving all this stuff out for now!! 
  # TODO: 
  #   1) ADD these back in for 2023  using dplyr instead of SQL;
  #   2) go sleuth these for the 2024 data and add those hard-codes.
  # ADK
df_first_viol_date <-
dbGetQuery(
  channel,
  "SELECT ole_obs_statement_seq,
          trunc(norpac.OLE_STATEMENT_PKG.get_first_violation_date(ole_obs_statement_seq)) AS first_violation_date
     FROM norpac.ole_obs_statement") %>%
  mutate(FIRST_VIOL_YEAR = year(FIRST_VIOLATION_DATE) )  
  

df_statements_raw <-
  df_statements_raw %>%
  left_join(df_first_viol_date) %>%
  filter(FIRST_VIOL_YEAR >= as.numeric(adp_yr)-1)


 
  
# Some summary tables of the new statements data.
#TODO: move this to a different script.  Jsut getting it up here for now, it's 5PM on a Friday, LOL

summ_regs_units <-
df_statements_raw %>%
  group_by(OLE_REGULATION_SEQ, REG_DESCRIPTION, REG_SUMMARY, CATEGORY, SUBCATEGORY) %>%
  summarize(N_DISTINCT_CRUISES_REPORTED      = n_distinct(CRUISE)
            ,   N_DISTINCT_VESSPLANTS_REPORTED   = n_distinct(PERMIT[!is.na(PERMIT) & as.numeric(PERMIT) != 0])
            ,   N_HAULS           = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'HAUL' & !is.na(INCIDENT_UNIT)])
            ,   N_OFFLOADS        = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'OFFL' & !is.na(INCIDENT_UNIT)]) 
            ,   N_TRIPS           = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'TRIP' & !is.na(INCIDENT_UNIT)])
            ,   N_DAYS            = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'DAYS' & !is.na(INCIDENT_UNIT)])
            ,   N_DEPL            = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'DEPL' & !is.na(INCIDENT_UNIT)])
            ,   N_SAMPLES         = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'SAMP' & !is.na(INCIDENT_UNIT)])
            ,   N_MARINE_MAMMALS  = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'MARM' & !is.na(INCIDENT_UNIT)])
            ,   N_UNIT_ISSUES     = n_distinct(OLE_OBS_STATEMENT_DETAIL_SEQ[!is.na(UNIT_ISSUE)])
            ,   .groups = "drop" 
  )

summ_reg_unit_types <-
  df_statements_raw %>%
  group_by(OLE_REGULATION_SEQ, REG_DESCRIPTION, REG_SUMMARY, CATEGORY, SUBCATEGORY) %>%
  distinct(INCIDENT_UNIT)           




summ_subcat_units <-
  df_statements_raw %>%
  group_by(CATEGORY, SUBCATEGORY) %>%
  summarize(N_DISTINCT_CRUISES_REPORTED      = n_distinct(CRUISE)
            ,   N_DISTINCT_VESSPLANTS_REPORTED   = n_distinct(PERMIT[!is.na(PERMIT) & as.numeric(PERMIT) != 0])
            ,   N_DISTINCT_REGS_REPORTED         = n_distinct(OLE_REGULATION_SEQ)
            ,   N_HAULS           = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'HAUL' & !is.na(INCIDENT_UNIT)])
            ,   N_OFFLOADS        = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'OFFL' & !is.na(INCIDENT_UNIT)]) 
            ,   N_TRIPS           = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'TRIP' & !is.na(INCIDENT_UNIT)])
            ,   N_DAYS            = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'DAYS' & !is.na(INCIDENT_UNIT)])
            ,   N_DEPL            = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'DEPL' & !is.na(INCIDENT_UNIT)])
            ,   N_SAMPLES         = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'SAMP' & !is.na(INCIDENT_UNIT)])
            ,   N_MARINE_MAMMALS  = length(OLE_OBS_STATEMENT_DETAIL_SEQ[INCIDENT_UNIT == 'MARM' & !is.na(INCIDENT_UNIT)])
            ,   N_UNIT_ISSUES     = n_distinct(OLE_OBS_STATEMENT_DETAIL_SEQ[!is.na(UNIT_ISSUE)])
            ,   .groups = "drop" 
  )











###################
# deployed_dates for each cruise/permit SQL query

# Go get cruise/permit assignment data
# Returns a row for each cruise, permit, and date the observer was assigned. 
# Also includes coverage_type, deployment_type, and trip_code as factors.

assignments_dates_cr_perm <-
  dbGetQuery(
    channel,
    paste0("SELECT distinct *
              FROM
              (SELECT norpac.ols_obsrvr.getObserverNamebyCruise(vp.cruise) observer_name, 
                      oco.observer_seq,
                      dl.cruise,
                      to_number(dl.permit) permit,
                      dl.trip_code,
                      DECODE(vp.special_deployment_seq, 1, 'EFP', 2, 'MIX', vp.special_deployment_seq) AS special_deployment_code,
                      (SELECT upper(regexp_substr(trip_description, '[^ ]*'))
                         FROM ols_lov_trip_codes
                        WHERE trip_code = vp.trip_code) AS observer_role, 
                      dl.vessel_plant_seq,
                      nvl(ds.debrief_start_date, ds.scheduled_debrief_date) AS debrief_start_date,
                      ds.debrief_end_date,
                      decode(dl.coverage_type_code, 'F', 'FULL', 'P', 'PARTIAL') as coverage_type,
                      dl.coverage_type_code,
                      trunc(dl.deployed_date) deployed_date,
                      extract(year FROM dl.deployed_date) AS calendar_year,
                      CASE WHEN extract(month FROM deployed_date) IN (10,11,12)
                          THEN extract(year FROM deployed_date) + 1
                          ELSE extract(year FROM deployed_date) 
                      END  AS fiscal_year,
                      v.name as vessel_or_plant_name, 
                      v.type as vessel_or_plant,
                      v.length as vessel_length
                FROM ols_vessel_plant vp
                JOIN norpac.atl_vessplant_v v   ON v.permit = vp.permit
                JOIN ols_observer_cruise ocr    ON ocr.cruise = vp.cruise
                JOIN ols_observer_contract oco  ON oco.contract_number = ocr.contract_number
                JOIN ols_debriefing_schedule ds ON ds.cruise = ocr.cruise
                JOIN TABLE(norpac.ole_statement_factors_pkg.get_deployed_dates_list_vp_seq(vp.vessel_plant_seq))  dl
                  ON vp.vessel_plant_seq = dl.vessel_plant_seq  
               WHERE vp.cruise > ", first_cruise,
           " AND ols_cruise.isFMAcruiseTF(vp.cruise) = 'T')  --eliminate HAKE cruises.
              WHERE DEPLOYED_DATE BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR') 
          "))


################
#NOTE: this one takes awhile, ~10 minutes.
df_fishery_dates <-
  dbGetQuery(channel,
             paste0(
               "SELECT fd.*
                FROM ols_vessel_plant vp
                JOIN TABLE(norpac.ole_statement_factors_pkg.get_fshry_data_dates_list(vp.cruise, vp.permit)) fd 
                  ON vp.cruise = fd.cruise
                 AND vp.permit = fd.permit 
               WHERE vp.cruise > ", 
               first_cruise))


###################
# hauls
# From atl_haul
hauls <-
  dbGetQuery(channel,
             paste0("SELECT * FROM
                       (SELECT CASE WHEN extract(month FROM h.retrv_date_time) IN (10,11,12) 
                                    THEN extract(year  FROM h.retrv_date_time) +1
                                    ELSE extract(year  FROM h.retrv_date_time) 
                                END AS fiscal_year,
                               extract(year  FROM h.retrv_date_time) as calendar_year,
                               h.cruise,
                               h.sampled_by AS sampled_by_cruise,
                               to_number(h.permit) AS permit,
                               h.haul_seq,
                               trunc(h.retrv_date_time) haul_date,
                               h.nmfs_area,
                               CASE WHEN rv.nmfs_region like '%GOA%' 
                                      THEN 'GOA'
                                    WHEN rv.nmfs_region like '%AI%' OR rv.nmfs_region like '%BS%'
                                      THEN 'BSAI'
                                    ELSE nmfs_region
                                END nmfs_region,
                               h.gear_type_code,
                               DECODE(h.gear_type_code, 8, 'HAL', 6, 'POT', 2, 'PTR', 1, 'NPT') AS gear_type,
                               h.vessel_type AS vessel_type_code,
                               CASE WHEN h.vessel_type in (1,2,4) THEN 'CP/MS'
                                    WHEN h.vessel_type in (3,5,6) THEN 'CV'
                                END AS vessel_type,
                               CASE WHEN h.cdq_code IS NOT NULL           THEN 'CDQ'
                                    WHEN h.indiv_fishing_quota_flag = 'Y' THEN 'IFQ'
                                    ELSE CASE WHEN h2.management_program_code = 'N/A' THEN null
                                              ELSE h2.management_program_code
                                          END   
                                END AS management_program_code 
                          FROM norpac.atl_haul h
                          JOIN norpac.atl_nmfs_area_v rv  
                            ON h.nmfs_area = rv.nmfs_area
                          LEFT OUTER JOIN norpac_views.akr_obs_haul h2
                            ON h.cruise   = h2.cruise
                           AND to_number(h.permit)   = h2.vessel_id
                           AND h.haul_seq = h2.haul_seq
                         WHERE trunc(h.retrv_date_time) >= (to_date('", first_date, "', 'DD-MON-RR') - 730)  --Get WAAAAY more than I need, 2 years more, to get data from other years, IF I need it for the rolling joins.
                          AND h.haul_purpose_code <> 'HAK')
                      WHERE management_program_code is not null"
                   ))




df_offloads <- 
  dbGetQuery(channel,
             paste0(
               "SELECT distinct report_id,
                     CASE WHEN extract(month FROM landing_date) IN (10,11,12) 
                                THEN extract(year  FROM landing_date) +1
                                ELSE extract(year  FROM landing_date) 
                            END AS fiscal_year,
                     trunc(landing_date) AS landing_date,
                     extract(year FROM landing_date) AS calendar_year,       
                     vessel_id, processor_permit_id, management_program_code,
                     decode(fmp_area_code, 'N/A', null, fmp_area_code) nmfs_region,
                     management_program_modifier ---Trawl EM identifier
                FROM norpac_views.atl_landing_mgm_id
                WHERE landing_date >= (to_date('", first_date, "', 'DD-MON-RR') -730) --get WAAAY more than I need, to ensure I have other years for the rolling join, if I need it.
               "))  


df_em_efp_offloads <- df_offloads %>%
  filter(MANAGEMENT_PROGRAM_MODIFIER == "TEM") %>%
  # have to change 'EXP' to 'AFA' for the 6 or so records that were recorded as such, it is messing things up.  They are AFA and that is a mistake some processors made!!
  mutate(MANAGEMENT_PROGRAM_CODE = ifelse(MANAGEMENT_PROGRAM_CODE == 'EXP', 'AFA', MANAGEMENT_PROGRAM_CODE))


# Save Output -------------------------------------------------------------

file_1_name  <- "AR_1_Statements_data.Rdata"

# save the file to the wd (but don't save the "channel object, no point as it will expire anyway)
save(list = ls()[!(ls() == 'channel')], 
     file = file_1_name)

gdrive_upload(file_1_name, AnnRpt_EnfChp_dribble)
