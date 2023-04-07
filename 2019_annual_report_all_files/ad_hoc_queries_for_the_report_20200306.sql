SELECT * FROM norpac_views.akr_obs_haul_mv
 WHERE trunc(retrieval_date) BETWEEN '01-SEP-19' AND '31-OCT-19' --COMMENTED OUT FOR 2019
  AND vessel_id = 993
  ;
  
  select * from atl_landing_mgm_id_mv WHERE vessel_id = 1235 AND trunc(landing_date) >= '01-JAN-19'
  ;
  
  SELECT * FROM observer_affidavits WHERE first_violation_date BETWEEN '01-JAN-19' AND '31-DEC-19'
   AND number_violations is null
   ;
   
 SELECT count (distinct cruise||'0000'||permit) num_assnmts
   FROM atl_haul h
  WHERE nmfs_area in (541, 542, 543) 
    AND cruise||'0000'||permit IN (SELECT cruise||'0000'||permit FROM ols_vessel_plant WHERE coverage_type_code = 'P')
    AND trunc(retrv_date_time) BETWEEN '01-JAN-19' AND '31-DEC-19';
    
     SELECT count (distinct cruise||'0000'||permit) num_assnmts
   FROM atl_haul h
  WHERE nvl(:IFQ_flag, 'N') = indiv_fishing_quota_flag 
        AND :CDQ_FLAG = CASE WHEN cdq_code is null THEN 'N' ELSE 'Y' END
    AND vessel_type = :vessel_type 
    AND gear_type_code = :gear_type
    AND trunc(retrv_date_time) BETWEEN '01-JAN-19' AND '31-DEC-19'
    AND  nmfs_area BETWEEN :FIRST_NMFS_AREA and :SECOND_NMFS_AREA --in (541, 542, 543) 
    AND cruise||'0000'||permit IN (SELECT cruise||'0000'||permit FROM ols_vessel_plant WHERE coverage_type_code = :cov_type)
    ;
    
    SELECT :CDQ_FLAG FROM dual