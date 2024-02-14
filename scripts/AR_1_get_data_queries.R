# Title:                      Statement Annual Report Analytics, v2022.1
# Author:                     Andy Kingham
# Contact:                    Andy.kingham@noaa.gov
# Create date:                20210329
###############################################################################
###############################################################################
# This is the same as V2 of the same name, except it removes OBSERVER_ROLE as a factor.

##########
# Set up the environment
##########

if(!require("odbc"))        install.packages("odbc",        repos='http://cran.us.r-project.org')
if(!require("plyr"))        install.packages("plyr",        repos='http://cran.us.r-project.org')
if(!require("dplyr"))       install.packages("dplyr",       repos='http://cran.us.r-project.org')
if(!require("tidyr"))       install.packages("tidyr",       repos='http://cran.us.r-project.org')
if(!require("lubridate"))   install.packages("lubridate",   repos='http://cran.us.r-project.org')
if(!require("ggplot2"))     install.packages("ggplot2",     repos='http://cran.us.r-project.org')
if(!require("scales"))      install.packages("scales",      repos='http://cran.us.r-project.org')
if(!require("googledrive")) install.packages("googledrive", repos='http://cran.us.r-project.org')


###############################################################################
###############################################################################

# clean up first
rm(list = ls())

#Open data channel
#####################


# Define odbc connection to NORPAC

channel <- dbConnect(odbc::odbc(),"AFSC",
                     UID    = rstudioapi::askForPassword("Enter your NORPAC Username: "),
                     PWD    = rstudioapi::askForPassword("Enter your NORPAC Password: "))

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
first_cruise <- rstudioapi::showPrompt(title = "First Cruise", 
                                       message = "Enter the first CRUISE number for statements in this query. NOTE: use a SMALLER cruise than you need, to ensure ALL cruises for the ADP year are selected.  Cruises will be filtered based on specific dates at a later step.",
                                       default = "")
########################
#SQL Queries
####################


###################
# Statements
# Amend as needed to get statements for the current year.
raw_statements <-
  dbGetQuery(channel,
             paste("
              SELECT AFFIDAVIT_ID,
                     (select species 
                        from MANAGEMENT_TARGET_FISHERIES 
                                 where code = aff.mgmt_fish_code) as MGMT_FISH_CODE,
                     FIRST_VIOLATION_DATE,
                     nvl(NUMBER_VIOLATIONS, 1) AS NUMBER_VIOLATIONS, --replace nulls with 1.  If this is an issue in a given year, this produces the lowest possible estimate.
                     aff.COMMENTS,
                     (select status_value 
                        from affidavit_case_status
                       where status = aff.affidavit_case_status) as AFFIDAVIT_CASE_STATUS,
                     AFFIDAVIT_DATE,
                     AFFIDAVIT_RECEIVED,
                     CASE_NUMBER,
                     (SELECT AFFIDAVIT_SUBJECT
                        FROM affidavit_subject
                       WHERE affidavit_type = aff.affidavit_type) AFFIDAVIT_TYPE,

                    --stick the ole_category on there.  Categories come from the Annual Report.
                    
                    CASE WHEN aff.affidavit_type IN ('XX', 'F','E','VV') 
                            THEN 'OLE PRIORITY: INTER-PERSONAL'
                         WHEN aff.affidavit_type IN ('DD','QQ','H','G','GG') 
                            THEN 'OLE PRIORITY: SAFETY AND DUTIES'
                         WHEN aff.affidavit_type IN ('SS','ZZ','WW','M','L','EE','K','I','J','BB','X','CC','P','Q','O','FF','N')
                            THEN 'PROTECTED RESOURCE & PROHIBITED SPECIES'
                         WHEN aff.affidavit_type IN ('W','MM','T','U','Z','KK','NN','LL') 
                            THEN 'ALL OTHER STATEMENT TYPES'
                         WHEN aff.affidavit_type IN ('V','HH','II','JJ')
                            THEN 'COAST GUARD'
                         WHEN aff.affidavit_type IN ('OO','PP','TT','RR','R','UU')
                            THEN 'LIMITED ACCESS PROGRAMS'
                         ELSE 'UNKNOWN'   
                       END AS ole_category,
                     AGENT,
                     CAPTAIN_NAME,
                     TRIPS_JOIN,
                     AFFIDAVIT_FORWARDED_DATE,
                     (select location_text 
                        from affidavit_forwarding_locations
                       where location_code = aff.forwarded_location) as FORWARDED_LOCATION,
                     ENFORCEMENT_COMMENTS,
                     FORWARDED_AED_DATE,
                     aff.VESSEL_PLANT_SEQ,
                     to_number(aff.PERMIT) PERMIT,
                     aff.CRUISE,
                     nvl(extract(YEAR FROM aff.first_violation_date), extract(YEAR FROM aff.create_date)) affi_year,
                     extract(YEAR FROM aff.first_violation_date) first_viol_year,
                     norpac.ole_statement_factors_pkg.getManualYearForCruise(aff.cruise) AS manual_year
                FROM OBSERVER_AFFIDAVITS aff
                LEFT OUTER JOIN ols_observer_cruise ocr
                  ON ocr.cruise = aff.cruise
                LEFT OUTER JOIN ols_observer_contract oco
                  ON oco.contract_number = ocr.contract_number
                LEFT OUTER JOIN ols_lov_employer emp
                  ON emp.employer_code = oco.employer_code
                LEFT OUTER JOIN ols_observer o
                  ON o.observer_seq = oco.observer_seq
                LEFT OUTER JOIN ols_vessel_plant vp
                  ON aff.vessel_plant_seq = vp.vessel_plant_seq   
                LEFT OUTER JOIN ols_debriefed_vessplant dvp
                  ON dvp.vessel_plant_seq = vp.vessel_plant_seq
                LEFT OUTER JOIN ols_debriefing_schedule ds
                  ON ds.schedule_seq = dvp.schedule_seq
                LEFT OUTER JOIN ols_lov_staff s
                  ON s.staff_id = ds.debriefer_staff_id             
                  WHERE aff.first_violation_date BETWEEN 
                      to_date('", first_date, "', 'DD-MON-RR') AND
                      to_date('", last_date, "', 'DD-MON-RR')
                    AND oco.contract_status in ('C', 'E')    --only get cruises that are already debriefed.
                 AND oco.hake_flag = 'N'                  --eliminate HAKE cruises 
                 ORDER BY affidavit_id desc",
                   sep = ''))


###################
# deployed_dates for each cruise/permit SQL query

# Go get cruise/permit assignment data
# Returns a row for each cruise, permit, and date the observer was assigned. 
# Also includes coverage_type, deployment_type, and trip_code as factors.

assignments_dates_cr_perm <-
  dbGetQuery(channel,paste("
            SELECT distinct *
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
                JOIN norpac.atl_vessplant_v v
                  ON v.permit = vp.permit
                JOIN ols_observer_cruise ocr
                  ON ocr.cruise = vp.cruise
                JOIN ols_observer_contract oco
                  ON oco.contract_number = ocr.contract_number
                JOIN TABLE(norpac.ole_statement_factors_pkg.get_deployed_dates_list_vp_seq(vp.vessel_plant_seq))  dl
                  ON vp.vessel_plant_seq = dl.vessel_plant_seq  
               WHERE vp.cruise > ", first_cruise,
                           " AND ols_cruise.isFMAcruiseTF(vp.cruise) = 'T')  --eliminate HAKE cruises.
              WHERE DEPLOYED_DATE BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR') 
         ", sep = ''))


################
df_fishery_dates <-
  dbGetQuery(channel,
             paste(
               "SELECT fd.*
                FROM ols_vessel_plant vp
                JOIN TABLE(norpac.ole_statement_factors_pkg.get_fshry_data_dates_list(vp.cruise, vp.permit)) fd 
                  ON vp.cruise = fd.cruise
                 AND vp.permit = fd.permit 
               WHERE vp.cruise > ", first_cruise, sep = ''))




###################
# hauls
# From atl_haul
hauls <-
  dbGetQuery(channel,
             paste("SELECT * FROM
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
                          LEFT OUTER JOIN norpac_views.akr_obs_haul_mv h2
                            ON h.cruise   = h2.cruise
                           AND to_number(h.permit)   = h2.vessel_id
                           AND h.haul_seq = h2.haul_seq
                         WHERE trunc(h.retrv_date_time) >= (to_date('", first_date, "', 'DD-MON-RR') - 730)  --Get WAAAAY more than I need, 2 years more, to get data from other years, IF I need it for the rolling joins.
                          AND h.haul_purpose_code <> 'HAK')
                      WHERE management_program_code is not null", 
                   sep = ''))




df_offloads <- 
  dbGetQuery(channel,
             paste(
               "SELECT distinct report_id,
                     CASE WHEN extract(month FROM landing_date) IN (10,11,12) 
                                THEN extract(year  FROM landing_date) +1
                                ELSE extract(year  FROM landing_date) 
                            END AS fiscal_year,
                     trunc(landing_date) AS landing_date,
                     extract(year  FROM landing_date) AS calendar_year,       
                     vessel_id, processor_permit_id, management_program_code,
                     decode(fmp_area_code, 'N/A', null, fmp_area_code) nmfs_region
                FROM norpac_views.atl_landing_mgm_id_mv
               WHERE landing_date >= (to_date('", first_date, "', 'DD-MON-RR') -730) --get WAAAY more than I need, to ensure I have other years for the rolling join, if I need it.
               ",
               sep = ''))  






df_em_efp_offloads <-
  dbGetQuery(channel,
             "SELECT DISTINCT ed.*,
                     CASE WHEN extract(month FROM ed.landing_date) IN (10,11,12) 
                                THEN extract(year  FROM ed.landing_date) +1
                                ELSE extract(year  FROM ed.landing_date) 
                            END AS fiscal_year,
                     extract(year FROM ed.landing_date) AS calendar_year,       
                     nvl(decode(fmp_area_code, 'N/A', null, fmp_area_code),
                         CASE WHEN GOA_POLLOCK_OFFLOAD = 'NO'
                              THEN 'BSAI'
                              WHEN GOA_POLLOCK_OFFLOAD = 'YES'
                              THEN 'GOA'
                          END) AS nmfs_region
                FROM norpac_views.atl_landing_mgm_id_mv o
                JOIN norpac_views.em_efp_trawl_deliveries ed
                  ON o.report_id = ed.report_id
             ")  %>%
  # have to change 'EXP' to 'AFA' for the 6 or so records that were recorded as such, it is messing things up.  They are AFA and that is a mistake some processors made!!
  mutate(MANAGEMENT_PROGRAM_CODE = ifelse(MANAGEMENT_PROGRAM_CODE == 'EXP', 'AFA', MANAGEMENT_PROGRAM_CODE))    



# Save Output -------------------------------------------------------------
# MUST SAVE OUTSIDE wd, because we cannot have "data" on the GitHub site.
# UPdate to your local filepath as needed.

Rdata_files_path <- "C:/Users/andy.kingham/Work/Analytical Projects/Projects/Statement_redesign/Annual_Report/RData_files/2023/"

save(list = c("raw_statements", "assignments_dates_cr_perm", "hauls", "df_offloads", "df_em_efp_offloads", "df_fishery_dates", "first_cruise", "first_date", "last_date", "adp_yr", "Rdata_files_path"), 
     file = paste0(Rdata_files_path, "AR_1_OLD_DATA_Statements_data.Rdata"))


#' Authorize the googledrive package to access your NOAA Gdrive. Authenticating via this browser should be a one-time
#' thing. Future calls to googledrive functions will prompt you to simply select your NOAA google account as the one 
#' you want to re-authorize
#' 
#' Commenting out, UNCOMMENT if needed.
# googledrive::drive_auth()

# Assign google drive location
# MAKE SURE IT IS CORRECT GOOGLE PATH
project_dribble <- googledrive::drive_get("FMA Analysis Group/FMA OLE Statements Project/FMA OLE Statements AR ch 5 Rdata files/")

# upload the .Rdata file to g-drive
googledrive::drive_upload(
  media     = paste0(Rdata_files_path, "AR_1_OLD_DATA_Statements_data.Rdata"),     #' *The local filepath to the file you want to upload*
  path      = project_dribble,                        #' *The dribble object of the Gdrive folder you want to upload to*
  name      = "AR_1_OLD_DATA_Statements_data.Rdata",  #' *Optional. Assignes your uploaded object with a different file name.*,
  overwrite = T                                       #' *A control for overwriting existing Gdrive files*.
) 

