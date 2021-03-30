
--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 2020 Fee Query - DRAFT based on warehouse.sql
-- S:\Observer Program Annual Report\2020_Annual_Report\Chapt2_Fees&Budget
-- C:\Teleworking\Obs Annual Report\Ch 2

-- Use to brainstorm/summarize fee liability by area, gear, vessel length, and species for 2020 Annual Report 
--      (equivalent to tables 2-2 through 2-4 in 2019 Annual Report)
-- BUT include this SQL in a Markdown document for the final summary

-- This query is based on the code used to summarize fee liability for the 2019 Annual report 
--     (S:\Observer Program Annual Report\2019_Annual_Report\Chapt2_Fees&Budget\2019 Fee Query - DRAFT based on warehouse.sql) 

-- Revisions for 2020 include: 
--     1) Identify correct gear (instead of defaulting to HAL) from PNOL when there are IFQ manual landings and no corresponding landing report
--     2) Exclude Clipper Seafoods observer fees that were inappropriately assigned to a CP in full coverage (they originally listed DUT as port instead of FCP)

-- INITIALLY: 23,648 records for $2,471,583.85 (as of 3/24/21)  <-- I appear to be including fees that weren't invoiced (~1,616.84)
-- REVISED:   23,646 records for $2,469,967.01 (as of 3/25/21)  <-- Excluded 2 GF records that are in final status (not invoiced). 
--                                                                  This matches principal due on ALDERS/OMD report but does not take into account Clipper Seafoods correction. 
-- REVISED:   23,644 records for $2,469,241.17 (as of 3/29/21)  <-- This removes the Clipper Seafoods observer fees that were corrected ($725.84)
--                                                                  NOTE: There are 35 IFQ records that have a null fee_person_year_id that appear to have been invoiced ($20,388.03)

-- Note: OMD does not appear to have waived any fees for 2020
-- Note: nothing has been done with carry forwards.
-- Note: Do not include administrative fees or interest for late payments. 

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SELECT gf.year, 'GF' AS report_source, 
       gf.category,
       gf.processor_permit_id as processor_permit,
       gf.vessel_id, gf.adfg_number, --gf.vessel_name, 
       v.length_overall, 
       
       -- summarize the vessels by vessel length category:
       CASE WHEN v.length_overall < 40                              THEN '<40'
            WHEN (v.length_overall >= 40 and v.length_overall < 58) THEN '40 - 57.5'  
                                                                    ELSE '>57.5'
       END AS LOA_GROUP,
     
       gf.report_id, NULL AS confirmation_number, NULL AS ifq_permit_number, 
       NULL as area, fmp.area_code as area_code,
       fmp.area_code AS fmp_area,
       gf.fish_ticket_number, gf.catch_activity_date, 
       gf.catch_report_source_pk, gf.catch_report_species_fact_pk,
       gf.agency_species_id, gf.species_group_id,
       gf.agency_species_name, gf.agency_species_code,
       
       -- Aggregate the species in some cases: 
       CASE WHEN gf.agency_species_code = '110' THEN 'Pacific Cod'
            WHEN gf.agency_species_code = '710' THEN 'Sablefish'
            WHEN gf.agency_species_code = '270' THEN 'Pollock'
            WHEN gf.agency_species_code = '200' THEN 'Halibut'
                                                ELSE 'All Other Species'
       END AS species_group,
       
       gf.gear_group_id, gf.gear_group_description, 
       -- Differentiate HAL/POT/JIG as separate gear types:
       --(SELECT MAX (agency_gear_code) FROM ellr_report z WHERE z.report_id = lp.report_id and z.current_version_flag = 'Y') as gear_code, 
       g.gear_code,       
       gf.weight_pounds, gf.price_per_pound, gf.fee_amount,  gf.fee_person_year_id
  FROM akfish_crf.fee_detail_gf gf -- The table that contains the detailed records which are subject to fees for GF transactions
  -- Join to the catch_report_species_fact tables in order to pull FMP area and gear information from the ELLR and ELPR:
  JOIN akfish_report.catch_report_species_fact crsf ON gf.catch_report_species_fact_pk = crsf.catch_report_species_fact_pk
  -- Join to the area table to pick up FMP area:
  JOIN akfish_report.area fmp ON crsf.fmp_area_pk = fmp.area_pk 
  -- Join to the vessel table in the warehouse from the ELLR/ELPR (catch_report_species_fact table) instead of from v_obsfee_gf_fee: 
  JOIN akfish_report.vessel v ON crsf.catcher_vessel_pk = v.vessel_pk  
  -- Join to the gear table in the warehouse from the ELLR/ELPR (catch_report_species_fact table):
  JOIN akfish_report.gear g ON crsf.gear_pk = g.gear_pk 
 WHERE gf.year = extract(year from sysdate) - 1             -- previous year
   and gf.management_program_id = 12                        -- observer fees (this excludes cost recovery records)
   and gf.fee_person_year_id IS NOT NULL                    -- to only pick up invoiced fees and not ones in final status                            
              
UNION ALL 

-- IFQ landings, area information obtained from IFQ account data:
SELECT ifq.year, 'IFQ' AS report_source, 
       ifq.category,
       ifq.registered_buyer_number as processor_permit, 
       ifq.vessel_id, ifq.adfg_number, --ifq.vessel_name, 
       v.length_overall, 
       
       -- summarize the vessels by vessel length category:
       CASE WHEN v.length_overall < 40                              THEN '<40'
            WHEN (v.length_overall >= 40 and v.length_overall < 58) THEN '40 - 57.5' 
                                                                    ELSE '>57.5'
       END AS LOA_GROUP,
      
       ifq.report_id, ifq.confirmation_number, ifq.ifq_permit_number, 
       ifq.area, ma.code, 
       
       -- Aggregate area as BSAI or GOA:
       CASE WHEN ma.code in ('4A', '4B', '4C', '4D', '4E', 'AI', 'BS') THEN 'BSAI'        
            WHEN ma.code IN ('2C', '3A', '3B', 'CG', 'SE', 'WG', 'WY') THEN 'GOA'
                                                                       ELSE 'UHOH'
       END AS FMP_AREA,  
        
       ifq.fish_ticket_number, ifq.catch_activity_date, 
       NULL AS catch_report_source_pk, NULL AS catch_report_species_fact_pk, 
       ifq.agency_species_id, NULL AS species_group_id,
       ifq.agency_species_name, ifq.agency_species_code,
       
       -- Aggregate the species in some cases: 
       CASE WHEN ifq.agency_species_code = '110' THEN 'Pacific Cod'
            WHEN ifq.agency_species_code = '710' THEN 'Sablefish'
            WHEN ifq.agency_species_code = '270' THEN 'Pollock'
            WHEN ifq.agency_species_code = '200' THEN 'Halibut'
                                               ELSE 'All Other Species'
       END AS species_group,
       
       ifq.gear_group_id, ifq.gear_group_description, 
            -- Note: ObsFee IFQ data contains gear information as FIXED. Use landing report data (stored in catch_report_species_fact) to distinguish types of fixed gear. 
       --       Use MAX(g.gear_code) to identify the only gear on each landing report because catch_report_species_fact has a record for each species catch transaction. 
       --       In the event of a manual landing and the landing report is not available, translate the IFQ gear code from akfish.ifq_landing_transaction:
       NVL((SELECT MAX(g.gear_code) 
              FROM akfish_report.catch_report_source crs
              JOIN akfish_report.catch_report_species_fact crsf ON crs.catch_report_source_pk = crsf.catch_report_source_pk 
              JOIN akfish_report.gear g ON crsf.gear_pk = g.gear_pk 
             WHERE ifq.report_id = crs.el_report_id and crs.expire_date IS NULL), decode(ilt.ifq_gear_code, '61', 'HAL', '91', 'POT', '05', 'JIG', '26', 'JIG')) as GEAR_CODE,  --15 and 25?  JIG?                                           
       ifq.weight_pounds, ifq.price_per_pound, ifq.fee_amount,  ifq.fee_person_year_id
  FROM akfish_crf.fee_detail_ifq ifq
   -- join to management area to translate area IDs:
  JOIN akfish.management_area ma ON ifq.area = ma.id        
  -- join to the vessel file so we can summarize fees by vessel length:
  JOIN akfish.v_vessel v ON ifq.vessel_id = v.id             
  -- join to the PNOL/landing data to pick up gear when there isn't a corresponding landing report (ie out of state landings?):
  JOIN akfish.ifq_landing_transaction ilt ON ifq.confirmation_number = ilt.confirmation_number 
 WHERE ifq.year = extract(year from sysdate) - 1             -- previous year
   and ifq.management_program_id = 12                        -- observer fees
   and (ifq.fee_person_year_id IS NULL or ifq.fee_person_year_id != 6793230)  -- Correction: Clipper Seafoods fees were incorrectly charged and corrected ($725.84)
                                                                              -- NOTE: There are 35 records that have a null fee_person_year_id that appear to have been invoiced ($20,388.03) 
                                                                              
                                                                              