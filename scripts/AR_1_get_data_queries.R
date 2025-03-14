# Title:                      Statement Annual Report Analytics, v2025.1
# Author:                     Andy Kingham
# Contact:                    Andy.kingham@noaa.gov
# Create date:                20210329
###############################################################################
###############################################################################
# This is the same as V2 of the same name, except it removes OBSERVER_ROLE as a factor,
# updates GDrive functions, changes NORPAC access to use .Renvirons,
# removes query for old statements database call, and streamlines offload query.

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
adp_yr <- as.numeric(rstudioapi::showPrompt(title = "ADP YEAR", message = "Enter the ADP YEAR for this analysis:", default = ""))

# first_date is the first violation date for statements.  Must be the current year, PLUS the previous year.
# Set this to 01-JAN of the year PRIOR to the ADP year.
first_date <- paste0('01-JAN-', (adp_yr-1)-2000)

# last_date is the Last calendar day of the annual report year,
# Set this 31-DEC of the ADP year.
last_date  <- paste0('31-DEC-', adp_yr-2000)


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
first_cruise <- manual_year_cruises$MIN_CRUISE[manual_year_cruises$MANUAL_YEAR == adp_yr-1] - 200


########################
#SQL Queries
####################


###################
# Statements
####################

# Changing this to be MULTIPLE queries, and do some of the JOINS in dplyr (much faster this way in R, DOH)
df_obs_statement_parent <-
  dbGetQuery(
    # Parent statement records
    channel,
    "SELECT norpac.ole_statement_factors_pkg.getManualYearForCruise(os.cruise) AS manual_year 
        ,   os.ole_obs_statement_seq
        ,   oc.category
        ,   os.cruise
        ,   os.permit
        ,   os.witness_flag
        ,   os.victim_name
        ,   os.agent
        ,   os.case_number
        ,   os.enforcement_comments
        ,   acs.status_value  AS case_status
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
      LEFT JOIN affidavit_case_status acs         ON acs.status = os.affidavit_case_status 
      LEFT JOIN norpac.atl_vessplant_v v          ON v.permit = os.permit      
   ")


# Statement details
df_obs_statement_details <-
  dbGetQuery(
    channel,
    "SELECT osd.ole_obs_statement_seq
       ,    osd.ole_obs_statement_detail_seq
       ,    lsc.subcategory
       ,    lr.ole_regulation_seq
       ,    lr.description    AS reg_description
       ,    lr.summary        AS reg_summary
       ,    osd.unit_issue 
      FROM norpac.ole_obs_statement_detail osd
      JOIN norpac.ole_lov_regulation lr           ON lr.ole_regulation_seq = osd.ole_regulation_seq
      JOIN norpac.ole_lov_subcategory lsc         ON lsc.ole_subcategory_seq = lr.ole_subcategory_seq
    ")


# UNITS for the statements
df_obs_statement_units <-  
  dbGetQuery(
    channel,
    "SELECT osu.ole_obs_statement_detail_seq
        ,   osu.ole_obs_statement_unit_seq
        ,   lu.incident_unit
        ,   lu.definition AS unit_description
        ,   osu.answer
        ,   osu.data_cruise
      FROM norpac.ole_obs_statement_unit osu      
      LEFT JOIN norpac.ole_lov_incident_unit lu   ON osu.ole_incident_unit_seq = lu.ole_incident_unit_seq
      ")


#################
# First violation date for the statements
  # We DO need this in 2024, because this is how we determine if a statement belongs in the report for the year or not.
  # Making this a separate DF, because it was proving to be VERY slow in the processing of the above query.
  # Then will merge and filter in the next step.

df_first_viol_date <-
  dbGetQuery(
    channel,
    "SELECT ole_obs_statement_seq,
            nvl(trunc(norpac.OLE_STATEMENT_PKG.get_first_violation_date(ole_obs_statement_seq)),
                
                -- There are 30 NAs in first_violation_date.  These are because there is no UNIT 
                -- from which to pull the date (that's what the above fxn does).
                -- So the 2nd half of the nvl() fxn accounts for this:
                -- for any of these, hard-code them: we sleuthed them out based on the UNIT ISSUE comment and/or the text of the statement.
                -- NOTE: Most are exact. Some are inexact, but that is OK, 
                -- because at this point, really this is only being used to determine which YEAR the statement belongs in.
                
                CASE WHEN ole_obs_statement_seq = 30027 THEN trunc(to_date('07-JUN-23', 'DD-MON-RR'))  --obs was unsure of date; using first depl date for permit
                     WHEN ole_obs_statement_seq = 30152 THEN trunc(to_date('24-SEP-23', 'DD-MON-RR'))  --obs says trip 2; using first day of trip 2.
                     WHEN ole_obs_statement_seq = 30153 THEN trunc(to_date('19-AUG-23', 'DD-MON-RR'))  --obs says the date in the unit issue text.
                     WHEN ole_obs_statement_seq = 30201 THEN trunc(to_date('25-SEP-23', 'DD-MON-RR'))  --obs says unsure which haul; using first haul on trip.
                     WHEN ole_obs_statement_seq = 30236 THEN trunc(to_date('15-AUG-23', 'DD-MON-RR'))  --obs says the date in the unit issue text.
                     
                     WHEN ole_obs_statement_seq = 30238 THEN trunc(to_date('05-SEP-23', 'DD-MON-RR'))  --offload 12, per the unit issue text.
                     WHEN ole_obs_statement_seq = 30290 THEN trunc(to_date('10-OCT-23', 'DD-MON-RR'))  --landing_report_id = 9530822, per the statement text
                     WHEN ole_obs_statement_seq = 30336 THEN trunc(to_date('17-SEP-23', 'DD-MON-RR'))  --Obs unsure of date; using first day of first trip.
                     WHEN ole_obs_statement_seq = 30358 THEN trunc(to_date('19-SEP-23', 'DD-MON-RR'))  --haul 599 *NOTE: should probably add an actual UNIT for this one
                     WHEN ole_obs_statement_seq = 30364 THEN trunc(to_date('07-SEP-23', 'DD-MON-RR'))  --first offload at this processor, per the unit issue.
                     
                     WHEN ole_obs_statement_seq = 30395 THEN trunc(to_date('28-AUG-23', 'DD-MON-RR'))  --Obs unsure of date; using first deployment date at this plant
                     WHEN ole_obs_statement_seq = 30460 THEN trunc(to_date('04-FEB-24', 'DD-MON-RR'))  --Offload 1 for this vessel in the observers data, per the unit issue text.
                     WHEN ole_obs_statement_seq = 30463 THEN trunc(to_date('22-FEB-24', 'DD-MON-RR'))  --landing_report_id = 9547842, per thethe unit issue
                     WHEN ole_obs_statement_seq = 30627 THEN trunc(to_date('20-JAN-24', 'DD-MON-RR'))  --obs unsure of haul.  Using date of first haul sampled by this observer
                     WHEN ole_obs_statement_seq = 30686 THEN trunc(to_date('27-FEB-24', 'DD-MON-RR'))  --offload 1 for this observer.
                     
                     WHEN ole_obs_statement_seq = 30728 THEN trunc(to_date('15-APR-24', 'DD-MON-RR'))  --from the unit_issue text
                     WHEN ole_obs_statement_seq = 30765 THEN trunc(to_date('10-MAY-24', 'DD-MON-RR'))  --Obs is unsure of dates.  Using first depl date on trip due to nature of violation.
                     WHEN ole_obs_statement_seq = 30766 THEN trunc(to_date('13-MAY-24', 'DD-MON-RR'))  --Obs is unsure of dates.  Using first haul sampled by this observer.
                     WHEN ole_obs_statement_seq = 30844 THEN trunc(to_date('30-JUN-24', 'DD-MON-RR'))  --using first deployment date due to the nature of statement
                     WHEN ole_obs_statement_seq = 30896 THEN trunc(to_date('02-SEP-24', 'DD-MON-RR'))  --using LAST date of deployment: unit_issue text says 'towards the end of the contract'
                     
                     WHEN ole_obs_statement_seq = 30930 THEN trunc(to_date('03-JUL-24', 'DD-MON-RR'))  --using first haul sampled by this observer, due to nature of the statement.
                     WHEN ole_obs_statement_seq = 30985 THEN trunc(to_date('20-JUL-24', 'DD-MON-RR'))  --using first deployment date due to the nature of this statement
                     WHEN ole_obs_statement_seq = 30986 THEN trunc(to_date('17-SEP-24', 'DD-MON-RR'))  --offload for the ENDURANCE for this cruise from the unit_issue..
                     WHEN ole_obs_statement_seq = 31007 THEN trunc(to_date('25-AUG-24', 'DD-MON-RR'))  --first haul sampled by this observer due to nature of the statement
                     WHEN ole_obs_statement_seq = 31016 THEN trunc(to_date('15-SEP-24', 'DD-MON-RR'))  --offload for trip 2 on this vessel at this plant, per the unit_issue text.
                     
                     WHEN ole_obs_statement_seq = 31017 THEN trunc(to_date('08-SEP-24', 'DD-MON-RR'))  --offload for trip 2 on this vessel at this plant, per the unit_issue text.
                     WHEN ole_obs_statement_seq = 31020 THEN trunc(to_date('29-JUL-24', 'DD-MON-RR'))  --first deployment date of this assignment
                     WHEN ole_obs_statement_seq = 31089 THEN trunc(to_date('21-OCT-24', 'DD-MON-RR'))  --from the unit_issue text
                     WHEN ole_obs_statement_seq = 31134 THEN trunc(to_date('15-SEP-24', 'DD-MON-RR'))  --using the first deployment date, due to the nature of the statement.
                     WHEN ole_obs_statement_seq = 31137 THEN trunc(to_date('15-SEP-24', 'DD-MON-RR'))  --using the first deployment date, due to the nature of the statement.
                     
                     ELSE null END   
                ) AS first_violation_date
       FROM norpac.ole_obs_statement os
       --don't forget to FILTER OUT HAKE STATEMENTS!  do it here
      WHERE norpac.ols_cruise.isFMAcruiseTF(os.cruise) = 'T' 
    ") %>%
  mutate(FIRST_VIOL_YEAR = year(FIRST_VIOLATION_DATE) )  %>%  
  # remove extra rows NOT from the last 2 years
  filter(FIRST_VIOL_YEAR >= adp_yr-1,
         FIRST_VIOL_YEAR <= adp_yr)




# Put it all together into on LONG dataframe; this is what will be used in the analysis.
# (The intermediate df's above, are just for building this one, and can be used as reference)
df_obs_statements <-
  df_obs_statement_parent %>%
  inner_join(df_first_viol_date) %>%  #inner_join ensures NO extra statements are included in the final list, e.g. hake or other years.
  left_join(df_obs_statement_details) %>%  # outer_join ensures we still include any that do NOT have detail record
  left_join(df_obs_statement_units) # outer_join ensures we still include any that do NOT have unit record (there are several of these)


 
  


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
  dbGetQuery(
    channel,
     paste0(
       "SELECT fd.*
          FROM ols_vessel_plant vp
          JOIN TABLE(norpac.ole_statement_factors_pkg.get_fshry_data_dates_list(vp.cruise, vp.permit)) fd 
            ON vp.cruise = fd.cruise
           AND vp.permit = fd.permit 
         WHERE vp.cruise > ", first_cruise,
         " AND trunc(haul_offload_date) BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
       "))


###################
# hauls
# From atl_haul
hauls <-
  dbGetQuery(
    channel,
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
               WHERE h.cruise >= ", first_cruise, 
               " AND h.haul_purpose_code <> 'HAK'
               )
            WHERE management_program_code is not null
              AND trunc(haul_date) BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
           "))



# Deliveries from Elandings - see view SQL for details
# Use to get FACTORS from offload data

df_elandings_raw <- 
  dbGetQuery(
    channel,
     "SELECT * FROM loki.deliveries_factors_last_2_yrs
     
       -- TODO: getting this ERROR when using the first_date and last_date parameters in this query:
       -- 'Error: Not compatible with requested type: [type=character; target=logical].'
       -- not sure WTF.  Hardcoded the dates for now; need to trace and fix this.
       
       WHERE trunc(landing_date) BETWEEN to_date('01-JAN-23', 'DD-MON-RR') AND to_date('31-DEC-24', 'DD-MON-RR')
      ") 


df_elandings_expand <-
  df_elandings_raw %>%
    group_by(REPORT_ID, FISCAL_YEAR, CALENDAR_YEAR, FISHING_DAYS, 
             VESSEL_ID, PROCESSOR_PERMIT_ID, GEAR_TYPE, VESSEL_TYPE,
             MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_MODIFIER, NMFS_REGION) %>%
    expand(DEPLOYED_DATE = seq(min(FISHING_START_DATE), 
                               max(LANDING_DATE), 
                               by = '1 day')
           ) %>%
    ungroup %>%
    inner_join(df_elandings_raw) %>% # have to get fishing_start_date and landing_date columns back into the df (this is a hack LOL)
    select(REPORT_ID, FISCAL_YEAR, CALENDAR_YEAR, DEPLOYED_DATE, FISHING_START_DATE, LANDING_DATE, FISHING_DAYS, VESSEL_ID, PROCESSOR_PERMIT_ID,
           GEAR_TYPE, VESSEL_TYPE, MANAGEMENT_PROGRAM_CODE, MANAGEMENT_PROGRAM_MODIFIER, NMFS_REGION) %>%
     # have to change 'EXP' to 'AFA' for the 6 or so Trawl EM records that were recorded as such, it is messing things up.  They are AFA and that is a mistake some processors made!!
    mutate(MANAGEMENT_PROGRAM_CODE = ifelse(MANAGEMENT_PROGRAM_CODE == 'EXP' & MANAGEMENT_PROGRAM_MODIFIER == 'TEM', 
                                           'AFA',
                                            MANAGEMENT_PROGRAM_CODE)  )

# Observer-recorded offloads
# Adding this because we need it for the OFFLOAD unit now.
# only get the ones we need (no need to get MORE), because this is NOT used for rolling joins
df_obs_offloads <-
  dbGetQuery(
    channel,
    paste0("SELECT cruise, permit, offload_seq, landing_report_id AS report_id, trunc(delivery_end_date) AS landing_date,
                   CASE WHEN cruise_plant_seq is null THEN 'V' ELSE 'P' END AS vessel_or_plant,
                   norpac.ole_statement_pkg.get_unit_description(
                             p_data_cruise   => cruise,
                             p_permit        => permit,
                             p_answer        => to_char(offload_seq),
                             p_incident_unit => 'OFFL'
                             )  AS offload_description         
              FROM norpac.atl_offload
             WHERE cruise >= ", first_cruise,
             " AND trunc(delivery_end_date) BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
           "))



# Observer-recorded trips
df_obs_trips <-
  dbGetQuery(
    channel,
    paste0("SELECT cruise, permit, trip_seq,
                   norpac.ole_statement_pkg.get_unit_description(
                             p_data_cruise   => cruise,
                             p_permit        => permit,
                             p_answer        => to_char(trip_seq),
                             p_incident_unit => 'TRIP'
                             )  AS trip_description,    
                   trunc(start_date) as trip_start_date,
                   trunc(end_date) AS trip_end_date,
                   embarked_port_code, 
                  (SELECT name FROM norpac.atl_lov_port_code WHERE port_code = embarked_port_code) AS embarked_port,
                   disembarked_port_code, 
                  (SELECT name FROM norpac.atl_lov_port_code WHERE port_code = disembarked_port_code) AS disembarked_port,
                   did_fishing_occur_flag, fish_in_hold_at_start_flag
              FROM norpac.atl_fma_trip
             WHERE cruise >= ", first_cruise,
           " AND norpac.ols_cruise.isFMAcruiseTF(cruise) = 'T'
             AND (trunc(start_date) BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
              OR trunc(end_date) BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
                 )
           "))



# Observer CRUISES 
df_cruises <-
  assignments_dates_cr_perm %>%
    group_by(CALENDAR_YEAR, OBSERVER_NAME, OBSERVER_SEQ, CRUISE, DEBRIEF_START_DATE, DEBRIEF_END_DATE) %>%
    summarise(MIN_EMBARK    = min(DEPLOYED_DATE), 
              MAX_DISEMBARK = max(DEPLOYED_DATE), 
              .groups = "drop"
              )
  


# Observer SAMPLES
df_obs_samples <-
  dbGetQuery(
    channel,
    paste0(
    "SELECT nvl(extract(year FROM h.retrv_date_time),
                extract(year FROM o.delivery_end_date)
                ) AS calendar_year,
            s.cruise, s.permit, s.haul_seq, s.offload_seq, s.sample_seq, s.sample_number,
            CASE WHEN parent_sample_seq is not null THEN 'Y' ELSE 'N' END AS subsample_flag,
            CASE WHEN sample_number = 9000 THEN 'Y' ELSE 'N' END AS decksort_flag,
            presorted_flag,
            norpac.ole_statement_pkg.get_unit_description(
                     p_data_cruise   => s.cruise,
                     p_permit        => s.permit,
                     p_answer        => to_char(s.sample_seq),
                     p_incident_unit => 'SAMP'
                     )  AS sample_description    
       FROM norpac.atl_sample s
       LEFT JOIN norpac.atl_haul h ON h.cruise = s.cruise AND h.permit = s.permit AND h.haul_seq = s.haul_seq
       LEFT JOIN norpac.atl_offload o ON o.cruise = s.cruise AND o.permit = s.permit AND o.offload_seq = s.offload_seq
             WHERE s.cruise >= ", first_cruise,
      " AND norpac.ols_cruise.isFMAcruiseTF(s.cruise) = 'T'
        AND (trunc(o.delivery_end_date) BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
         OR  trunc(h.retrv_date_time)   BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
            )
        "))




# Observer MARINE MAMMAL INTERACTIONS
# Adding this because we need it for the MARM unit now.
# only get the ones we need (no need to get MORE), because this is NOT used for rolling joins
df_obs_marm <-
  dbGetQuery(
    channel,
    paste0(
      "SELECT nvl(trunc(h.retrv_date_time),
                  nvl(trunc(delivery_end_date),
                     (SELECT trunc(max(interaction_date)) FROM norpac.atl_mammal_interaction
                       WHERE cruise = m.cruise
                         AND permit = m.permit
                         AND mammal_seq = m.mammal_seq)
                  )) AS interaction_date,
              m.cruise, m.permit, m.mammal_seq, t.trip_seq, h.haul_seq, o.offload_seq,
              sc.mammal_species_code, sc.common_name,
              norpac.ole_statement_pkg.get_unit_description(
                     p_data_cruise   => m.cruise,
                     p_permit        => m.permit,
                     p_answer        => to_char(m.mammal_seq),
                     p_incident_unit => 'MARM'
                     ) AS mammal_interaction_description   
         FROM norpac.atl_mammal m
         LEFT JOIN norpac.atl_lov_mammal_species_code sc ON sc.mammal_species_code = m.mammal_species_code
         LEFT JOIN norpac.atl_fma_trip t  
           ON t.trip_seq = m.trip_seq
          AND t.permit   = m.permit
          AND t.cruise   = m.cruise
        LEFT JOIN norpac.atl_haul h 
          ON h.haul_seq = m.haul_seq
         AND h.permit   = m.permit
         AND h.cruise   = m.cruise
        LEFT JOIN norpac.atl_offload o 
          ON o.offload_seq = m.offload_seq
        and o.permit       = m.permit
        and o.cruise       = m.cruise
       WHERE m.cruise >= ", first_cruise,
        " AND norpac.ols_cruise.isFMAcruiseTF(m.cruise) = 'T'
          AND (trunc(o.delivery_end_date) BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
           OR  trunc(h.retrv_date_time)   BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
           OR  trunc(t.start_date)        BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
           OR  trunc(t.end_date)          BETWEEN to_date('", first_date, "', 'DD-MON-RR') AND to_date('", last_date, "', 'DD-MON-RR')
              )
    "))




# Save Output -------------------------------------------------------------

file_1_name  <- "AR_1_Statements_data.Rdata"

# save the file to the wd (but don't save the "channel object, no point as it will expire anyway)
save(list = ls()[!(ls() == 'channel')], 
     file = file_1_name)

gdrive_upload(file_1_name, AnnRpt_EnfChp_dribble)
