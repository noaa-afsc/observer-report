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
#'* LOAD FMAtools here to enable our GDrive functions * - also requires devtools
#'* Could just load tidyverse after plyr to get dplyr, tidyr, and lubridate
#'  Can remove googledrive package if we use FMAtools

if(!require("odbc"))        install.packages("odbc",        repos='http://cran.us.r-project.org')
if(!require("plyr"))        install.packages("plyr",        repos='http://cran.us.r-project.org')
if(!require("tidyverse"))   install.packages("tidyverse",   repos='http://cran.us.r-project.org')
# if(!require("dplyr"))       install.packages("dplyr",       repos='http://cran.us.r-project.org')
# if(!require("tidyr"))       install.packages("tidyr",       repos='http://cran.us.r-project.org')
# if(!require("lubridate"))   install.packages("lubridate",   repos='http://cran.us.r-project.org')
# if(!require("googledrive")) install.packages("googledrive", repos='http://cran.us.r-project.org')
if(!require("devtools"))     install.packages("devtools",   repos='http://cran.us.r-project.org')
if(!require("FMAtools"))     devtools::install_github("Alaska-Fisheries-Monitoring-Analytics/FMAtools")

###############################################################################
###############################################################################

# clean up first
rm(list = ls())

#Open data channel
#####################


# Define odbc connection to NORPAC
# channel <- dbConnect(odbc::odbc(),"AFSC",
#                      UID    = rstudioapi::askForPassword("Enter your NORPAC Username: "),
#                      PWD    = rstudioapi::askForPassword("Enter your NORPAC Password: "))
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
# Amend as needed to get statements for the current year.
#'* THIS CAN LIKELY GO * statements_raw_old  <-
#   dbGetQuery(
#     channel,
#     paste("
#           SELECT affidavit_id
#              ,   mtt.species as mgmt_fish_code
#              ,   first_violation_date
#              ,   nvl(number_violations, 1) AS number_violations --replace nulls with 1.  If this is an issue in a given year, this produces the lowest possible estimate.  (Has not been an issue since 2019)
#              ,   aff.comments AS statement_body   
#              ,   case_number
#              ,   acs.status_value AS case_status
#              ,   asu.affidavit_subject AS statement_type   
# 
#                  --stick the ole_category on there.  Categories come from the Annual Report.
#                 
#              ,  CASE WHEN aff.affidavit_type IN ('XX', 'F','E','VV') 
#                          THEN 'OLE PRIORITY: INTER-PERSONAL'
#                       WHEN aff.affidavit_type IN ('DD','QQ','H','G','GG') 
#                          THEN 'OLE PRIORITY: SAFETY AND DUTIES'
#                       WHEN aff.affidavit_type IN ('SS','ZZ','WW','M','L','EE','K','I','J','BB','X','CC','P','Q','O','FF','N')
#                          THEN 'PROTECTED RESOURCE & PROHIBITED SPECIES'
#                       WHEN aff.affidavit_type IN ('W','MM','T','U','Z','KK','NN','LL') 
#                          THEN 'ALL OTHER STATEMENT TYPES'
#                       WHEN aff.affidavit_type IN ('V','HH','II','JJ')
#                          THEN 'COAST GUARD'
#                       WHEN aff.affidavit_type IN ('OO','PP','TT','RR','R','UU')
#                          THEN 'LIMITED ACCESS PROGRAMS'
#                       ELSE 'UNKNOWN'   
#                   END AS old_ole_category
#              ,  agent
#              ,  enforcement_comments
#              ,  to_number(aff.permit) AS permit
#              ,  v.name AS vessel_plant
#              ,  aff.cruise
#              ,  norpac.ole_statement_factors_pkg.getManualYearForCruise(aff.cruise) AS manual_year
#             FROM observer_affidavits aff
#             LEFT JOIN management_target_fisheries mtt  ON mtt.code = aff.mgmt_fish_code 
#             LEFT JOIN affidavit_case_status acs        ON acs.status = aff.affidavit_case_status
#             LEFT JOIN affidavit_subject asu            ON asu.affidavit_type = aff.affidavit_type
#             LEFT JOIN ols_observer_cruise ocr          ON ocr.cruise = aff.cruise
#             LEFT JOIN ols_observer_contract oco        ON oco.contract_number = ocr.contract_number
#             LEFT JOIN ols_vessel_plant vp              ON aff.vessel_plant_seq = vp.vessel_plant_seq   
#             LEFT JOIN norpac.atl_vessplant_v v         ON v.permit = vp.permit  
#             LEFT JOIN ols_debriefed_vessplant dvp      ON dvp.vessel_plant_seq = vp.vessel_plant_seq  
#            WHERE aff.first_violation_date BETWEEN 
#                    to_date('", first_date, "', 'DD-MON-RR') AND
#                    to_date('", last_date, "',  'DD-MON-RR')
#              AND oco.contract_status in ('C', 'E')    --only get cruises that are already debriefed.
#              AND oco.hake_flag = 'N'                  --eliminate HAKE cruises 
#           ",
#           sep = '')) %>%
#   mutate(FIRST_VIOL_YEAR  = year(FIRST_VIOLATION_DATE),
#          OLE_SYSTEM       = 'OLD',
#          NEW_OLE_CATEGORY = NA,
#          WITNESS_FLAG     = NA,
#          VICTIM_NAME      = NA,
#          UNITS            = NA,
#          WAS_ASSIGNED_TO_PERMIT_FLAG = 'Y')
# 
# 
# 



#################
# statements NEW DATA mid-2023 and on
####################

statements_raw_new <-
  dbGetQuery(
    channel,
    paste0("
WITH stmts_base AS (
 SELECT os.ole_obs_statement_seq,  oc.category AS new_ole_category,
        os.cruise, os.permit, os.witness_flag,  os.victim_name,
        os.agent, os.case_number,  os.enforcement_comments,
        acs.status_value  AS case_status,
        lsc.subcategory AS statement_type,
        listagg(distinct ot.ole_category, ', ') AS old_ole_category,
        listagg(distinct lu.incident_unit, ', ') WITHIN GROUP (order by lsc.subcategory) AS units,
        count(osu.ole_obs_statement_unit_seq) AS number_violations
   FROM norpac.OLE_OBS_STATEMENT os
   JOIN norpac.ole_lov_category oc             ON oc.ole_category_seq = os.ole_category_seq
   JOIN norpac.ole_obs_statement_detail osd    ON osd.ole_obs_statement_seq = os.ole_obs_statement_seq
   JOIN norpac.ole_lov_regulation lr ON lr.ole_regulation_seq = osd.ole_regulation_seq
   JOIN norpac.ole_lov_subcategory lsc         ON lsc.ole_subcategory_seq = lr.ole_subcategory_seq
   LEFT JOIN affidavit_case_status acs         ON acs.status = os.affidavit_case_status    
   LEFT JOIN norpac.OLE_OBS_STATEMENT_UNIT osu ON osu.ole_obs_statement_detail_seq = osd.ole_obs_statement_detail_seq      
   LEFT JOIN norpac.OLE_LOV_INCIDENT_UNIT lu   ON osu.ole_incident_unit_seq = lu.ole_incident_unit_seq
   LEFT JOIN norpac.ole_transform ot           ON osd.ole_regulation_seq = ot.ole_regulation_seq
   GROUP BY os.ole_obs_statement_seq,  oc.category,
        os.cruise, os.permit, os.witness_flag, os.victim_name,
        os.agent, os.case_number, os.enforcement_comments, acs.status_value, lsc.subcategory       
  )
 SELECT a.ole_obs_statement_seq AS affidavit_id
     ,  null AS mgmt_fish_code
     ,  a.new_ole_category   
     ,	nvl(norpac.OLE_STATEMENT_PKG.get_first_violation_date(a.ole_obs_statement_seq),              
        --2nd step in the above 'nvl' fxn is to fill in the GAPS of statements that are missing units 
        --(i.e, they have a UNIT_ISSUE) by reviewing th text fields from the statement and then hard-coding.  
        --ADK went thru and mined the text on 20240314
        --Note: this will need to be done again for any that are missing in subsequent years.            
        CASE WHEN a.ole_obs_statement_seq = 30153 THEN to_date('08/19/2023', 'mm/dd/yyyy')  --directly from the statement and/or unit_issue text.
             WHEN a.ole_obs_statement_seq = 30152 THEN 
                   (SELECT trunc(min(retrv_date_time)) --first haul of trip 2, from the statement and/or unit_issue text.
                      FROM norpac.atl_haul h JOIN norpac.atl_fma_trip t 
                        ON t.cruise = h.cruise AND t.permit = h.permit AND t.trip_seq = h.trip_seq
                      WHERE t.cruise in ((select * from table(norpac.ole_factor_pkg.get_data_cruise(a.cruise, a.permit))))
                        AND t.permit = a.permit
                        AND t.trip_number = 2
                    )
             WHEN a.ole_obs_statement_seq = 30236 THEN to_date('08/15/2023', 'mm/dd/yyyy')  --directly from the statement and/or unit_issue text
             WHEN a.ole_obs_statement_seq = 30238 THEN 
                   (SELECT min(delivery_end_date) 
                      FROM norpac.atl_offload WHERE cruise = 26324 
                       AND permit = 5306
                       AND offload_number = 12)  --offload 12, from the statement and/or unit_issue text
             WHEN a.ole_obs_statement_seq = 30290 THEN to_date('10/10/2023', 'mm/dd/yyyy')  --directly from the unit_issue text.
             WHEN a.ole_obs_statement_seq = 30358 THEN   
                   (SELECT trunc(min(retrv_date_time)) --haul 599, which was deleted.  From the statement and/or unit_issue text.
                      FROM norpac.atl_haul h
                     WHERE h.haul_number = 599)
             WHEN a.ole_obs_statement_seq = 30364 THEN  
                   (SELECT min(delivery_end_date) FROM norpac.atl_offload
                     WHERE cruise = a.cruise AND plant_seq = 21) --first offload for at this processor.  From the statement and/or unit_issue text.
             WHEN a.ole_obs_statement_seq = 30395 THEN
                   (SELECT min(embark_date) FROM norpac.ols_vessel_plant
                     WHERE cruise = a.cruise AND permit = 5306)  --first embark for the plant.
             WHEN a.ole_obs_statement_seq = 30460 THEN to_date('02/03/2024', 'mm/dd/yyyy') --from the statement and/or unit_issue text.
             WHEN a.ole_obs_statement_seq = 30463 THEN to_date('02/22/2024', 'mm/dd/yyyy')  --from the statement and/or unit_issue text.             
             WHEN a.ole_obs_statement_seq = 30027 THEN
                   (SELECT trunc(min(retrv_date_time)) --first haul.  Not defined in statement or unit_issue text.
                      FROM norpac.atl_haul h  
                     WHERE h.cruise in ((select * from table(norpac.ole_factor_pkg.get_data_cruise(a.cruise, a.permit))))
                       AND h.permit = a.permit
                    )
                 --if any are still left,  use the FIRST EMBARK DATE for the vessel_plant.
                 
             ELSE nvl((SELECT min(embark_date) FROM norpac.ols_vessel_plant
                        WHERE cruise = a.cruise AND permit = a.permit
                       ),
                       --if any are STILL left, use the FIRST EMBARK DATE for the cruise overall.
                       --Note: no statements ever went this far in the CASE statement for 2023, so it is just a fallback.
                      (SELECT min(embark_date) FROM norpac.ols_vessel_plant
                        WHERE cruise = a.cruise) 
                      )  
         END           
             ) AS first_violation_date
         ,	a.number_violations
         ,	a.units  
         ,  a.statement_type
         ,  a.case_status
         ,  a.agent
         ,  a.case_number     
         ,  a.enforcement_comments
         ,	CASE WHEN a.permit = 0 OR a.permit is null 
                 THEN 'No Permit'
                 ELSE v.name
             END AS vessel_plant
         ,  CASE WHEN a.permit = 0 OR a.permit is null 
                 THEN 'N'
                 ELSE decode(norpac.ole_statement_factors_pkg.wasCruiseAssndToVssPlnt_TF(
                                             in_cruise => a.cruise, 
                                             in_permit => a.permit),
                             'T', 'Y', 'F', 'N')               
             END AS was_assigned_to_permit_flag           
         ,	a.cruise AS cruise
         ,	to_number(a.permit) AS permit
         ,	(SELECT comments FROM norpac.ole_obs_statement WHERE ole_obs_statement_seq = a.ole_obs_statement_seq) AS statement_body
         ,  a.old_ole_category  
         ,  norpac.ole_statement_factors_pkg.getManualYearForCruise(a.cruise) AS manual_year     
         ,  a.witness_flag
         ,  a.victim_name
    FROM stmts_base a
    LEFT JOIN norpac.atl_vessplant_v v          ON v.permit = a.permit
    LEFT JOIN norpac.OLS_OBSERVER_CRUISE ocr    ON ocr.cruise = a.cruise
    LEFT JOIN norpac.OLS_OBSERVER_CONTRACT oco  ON oco.contract_number = ocr.contract_number
    LEFT JOIN norpac.OLS_DEBRIEFING_SCHEDULE ds ON ds.cruise = ocr.cruise
   WHERE trunc(norpac.OLE_STATEMENT_PKG.get_first_violation_date(a.ole_obs_statement_seq)) BETWEEN 
               to_date('", first_date, "', 'DD-MON-RR') AND
               to_date('", last_date,  "', 'DD-MON-RR')
           --    to_date(:in_first_date, 'DD-MON-RR') AND
           --    to_date(:in_last_date,  'DD-MON-RR')
     AND oco.contract_status in ('C', 'E')    --only get cruises that are already debriefed.
     AND oco.hake_flag = 'N'                  --eliminate HAKE cruises
   ")) %>%
  mutate(OLE_SYSTEM = 'NEW',
         FIRST_VIOL_YEAR = year(FIRST_VIOLATION_DATE)
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
          ")) %>%
  mutate(OLE_SYSTEM = if_else(CALENDAR_YEAR == 2023 &
                                DEBRIEF_START_DATE >= as.POSIXct(x = '2023/07/27', format = "%Y/%m/%d") |
                                CALENDAR_YEAR == 2024,
                              'NEW',  'OLD')
  )


################
#NOTE: this one takes awhile, ~10 minutes.
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
                          LEFT OUTER JOIN norpac_views.akr_obs_haul h2
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
                     extract(year FROM landing_date) AS calendar_year,       
                     vessel_id, processor_permit_id, management_program_code,
                     decode(fmp_area_code, 'N/A', null, fmp_area_code) nmfs_region,
                     management_program_modifier ---Trawl EM identifier
                FROM norpac_views.atl_landing_mgm_id
                WHERE landing_date >= (to_date('", first_date, "', 'DD-MON-RR') -730) --get WAAAY more than I need, to ensure I have other years for the rolling join, if I need it.
               ",
               sep = ''))  


df_em_efp_offloads <- df_offloads %>%
  filter(MANAGEMENT_PROGRAM_MODIFIER == "TEM") %>%
  # have to change 'EXP' to 'AFA' for the 6 or so records that were recorded as such, it is messing things up.  They are AFA and that is a mistake some processors made!!
  mutate(MANAGEMENT_PROGRAM_CODE = ifelse(MANAGEMENT_PROGRAM_CODE == 'EXP', 'AFA', MANAGEMENT_PROGRAM_CODE))

# df_em_efp_offloads <-
#   dbGetQuery(channel,
#              "SELECT DISTINCT ed.*,
#                      CASE WHEN extract(month FROM ed.landing_date) IN (10,11,12) 
#                                 THEN extract(year  FROM ed.landing_date) +1
#                                 ELSE extract(year  FROM ed.landing_date) 
#                             END AS fiscal_year,
#                      extract(year FROM ed.landing_date) AS calendar_year,       
#                      nvl(decode(fmp_area_code, 'N/A', null, fmp_area_code),
#                          CASE WHEN GOA_POLLOCK_OFFLOAD = 'NO'
#                               THEN 'BSAI'
#                               WHEN GOA_POLLOCK_OFFLOAD = 'YES'
#                               THEN 'GOA'
#                           END) AS nmfs_region
#                 FROM norpac_views.atl_landing_mgm_id o
#                 JOIN norpac_views.em_efp_trawl_deliveries ed ---THIS IS A BAD TABLE TO USE, SHOULD USE norpac_views.atl_landing_mgm_id and sort by MANAGEMENT_PROGRAM_MODIFIER == TEM
#                   ON o.report_id = ed.report_id
#              ")  %>%
#   # have to change 'EXP' to 'AFA' for the 6 or so records that were recorded as such, it is messing things up.  They are AFA and that is a mistake some processors made!!
#   mutate(MANAGEMENT_PROGRAM_CODE = ifelse(MANAGEMENT_PROGRAM_CODE == 'EXP', 'AFA', MANAGEMENT_PROGRAM_CODE))    


# Save Output -------------------------------------------------------------
#'* UPDATE THIS TO USE NEW GDrive FUNCTIONS FROM FMAtools *
# Commenting out drive_auth(), UNCOMMENT if needed.
#  Authorizes the googledrive package to access your NOAA Gdrive. Authenticating via this browser should be a one-time
# thing. Future calls to googledrive functions will prompt you to simply select your NOAA google account as the one 
# you want to re-authorize

# googledrive::drive_auth()

# Assign google drive location
# MAKE SURE IT IS CORRECT GOOGLE PATH
# Folder name is below, commented out, because it is slow.as.eff. when executed this way.
# MUCH faster to use the hard-coded drive ID (see below)

# project_dribble <- googledrive::drive_get(paste0("FMA Analysis Group/FMA OLE Statements Project/FMA OLE Statements AR ch 5 Rdata files/",
#                                                 adp_yr))

# project_dribble <- googledrive::drive_get(googledrive::as_id("10Qtv5PNIgS9GhmdhSPLOYNgBgn3ykwEA"))

file_1_name  <- "AR_1_Statements_data.Rdata"

# save the file to the wd (but don't save the "channel object, no point as it will expire anyway)
save(list = ls()[!(ls() == 'channel')], 
     file = file_1_name)

# # upload the .Rdata file to g-drive
# googledrive::drive_upload(
#   media     = file_1_name,       #' *The local filepath to the file you want to upload*
#   path      = project_dribble,   #' *The dribble object of the Gdrive folder you want to upload to*
#   name      = file_1_name,       #' *Optional. Assignes your uploaded object with a different file name.*,
#   overwrite = T                  #' *A control for overwriting existing Gdrive files*.
# ) 

gdrive_upload(file_1_name, AnnRpt_EnfChp_dribble)
