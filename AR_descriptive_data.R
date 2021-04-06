
# -------------------------------------------------------------------------------------------------------------------- #
#
# Program:AR_descriptive_2019_data.Rmd                                         
# Project:Observer Program Annual Report Descriptive Chapter                                      
# Location: S:\Observer Program Annual Report\2019_Annual_Report\Chap4_Descriptive_Info 
#      or: C:\Teleworking\Obs Annual Report\Ch 4
#
# Objectives:                                                                  
# - Query and perform some data clean-up for the descriptive chapter (Ch.4) of the Observer Program Annual Report
# - Generate catch table summaries that are posted to the AKRO website 
#   (https://www.fisheries.noaa.gov/alaska/fisheries-observers/observed-catch-tables-north-pacific-observer-program)
#
# Inputs:      
# Normal locations:
#  - S:\Observer Program Annual Report\Descriptive_Chapter\AR_descriptive_helper.r 
#      - fig.theme  (libraries, ggplot themes, etc.)
#  - AKRO Database
#      - akfish_sf.valhalla_scratch
#      - akfish_report.transaction_fact
#      - akfish_report.transaction_fact_log
#      - akfish_report.catch_report 
#      - akfish_report.catch_report_source 
#      - akfish_report.species_group 
#      - akfish_report.flag 
#      - akfish_report.mortality_rate 
#  - S:\Observer Program Annual Report\2018_Annual_Report\Chap4_Descriptive_Info\2013_2018_catchtables.csv 
#
# Coronavirus telework locations:
#  - C:\Teleworking\Obs Annual Report\Ch 4\AR_descriptive_helper.r
#      - fig.theme  (libraries, ggplot themes, etc.)
#  - C:\Teleworking\Obs Annual Report\Ch 4\2020-04-03CAS_VALHALLA.RData
#      - VALHALLA data.frame
#  - C:\Teleworking\Obs Annual Report\Ch 4\warehouse_data.csv
#  - C:\Teleworking\Obs Annual Report\Ch 4\2013_2018_catchtables.csv
#
# Output:    
# Normal locations:
#  - S:\Observer Program Annual Report\2019_Annual_Report\Chapt4_Descriptive_Info\AR_descriptive_2019_data.Rdata
#     - catch.tables - 2013-2019 catch tables for posting to the AKRO website
#     - previous.catch.tables - previous year's version of the catch tables (2013-2018)
#     - VALHALLA - the original Valhalla data
#     - warehouse.data - the mortality rates that were applied to the PSC data in the CAS run used in Valhalla's creation 
#     - with.totals - summary of total catch of groundfish, directed halibut, and PSC halibut as observed or not observed
#     - work.data - Valhalla dataset following some clean-up and addition of DMRs for halibut PSC
#  - S:\Observer Program Annual Report\2019_Annual_Report\Chapt4_Descriptive_Info\2013_2019_catchtables.csv
#
# Coronavirus telework locations:
#  - C:\Teleworking\Obs Annual Report\Ch 4\AR_descriptive_2019_data.Rdata
#     - catch.tables - 2013-2019 catch tables for posting to the AKRO website
#     - previous.catch.tables - previous year's version of the catch tables (2013-2018)
#     - VALHALLA - the original Valhalla data 
#     - warehouse.data - the mortality rates that were applied to the PSC data in the CAS run used in Valhalla's creation 
#     - with.totals - summary of total catch of groundfish, directed halibut, and PSC halibut as observed or not observed
#     - work.data - Valhalla dataset following some clean-up and addition of DMRs for halibut PSC
#  - C:\Teleworking\Obs Annual Report\Ch 4\2013_2019_catchtables.csv
#
# -------------------------------------------------------------------------------------------------------------------- #


###############
# USER INPUTS #
###############

#Source your helper file (libraries, ggplot themes, etc.)

# Normally in:
#source("S:/Observer Program Annual Report/Descriptive_Chapter/AR_descriptive_helper.r")
# During Coronavirus teleworking:
source("C:\\Teleworking\\Obs Annual Report\\Ch 4\\AR_descriptive_helper.r")

#Create a generalized YEAR object that corresponds to the Annual Report year
YEAR <- 2019

#Source connection inputs (con, user, pw)
#source("C:\\Teleworking\\R_RODBC_helper_file.r")


# ------------#
# - QUERIES - #
# ----------- #

# As of 4/15/19, a CAS run has been deleted from the warehouse that is needed. Load a snapshot of it from an older run instead:
#load("S:\\Observer Program Annual Report\\2018_Annual_Report\\Chap4_Descriptive_Info\\Outdated\\AR_descriptive_dat_2018.RData")
rm(list= ls()[!(ls() %in% c('warehouse.data', 'YEAR', 'con', 'user', 'pw'))])


#Clean up workspace (Remove everything EXCEPT the objects listed)
#rm(list= ls()[!(ls() %in% c('work.data', 'YEAR', 'con', 'user',  'pw'))])

# Establish connection to ODBC database in order to access database on AKRI:
#channel <- odbcConnect(con, user, pw)

# Query Valhalla data directly from the database for the 2019 Annual Report:
#valhalla.query <- "select * from akfish_sf.valhalla_scratch where adp = 2019"
#valhalla.data <- sqlQuery(channel,paste(valhalla.query),as.is=TRUE,max=10000000)

# Instead, load 2019 Valhalla data I got directly from Phil instead of querying it from database:
load("C:\\Teleworking\\Obs Annual Report\\Ch 4\\2020-04-03CAS_VALHALLA.RData")


#work.data <-data.frame(VALHALLA)
#prep.data <- valhalla.data
prep.data <- data.frame(VALHALLA)


# ----------------------------------------------------------------- #
# ---               Transform some of the data                  --- #
# ----------------------------------------------------------------- #

# Consolidate FMP for BS and AI:
#prep.data$FMP_AREA<-ifelse(prep.data$FMP %in% c('BS', 'AI'), 'BSAI', 'GOA')

# Create an RPP management program flag for the CVs:
# (as they are in full coverage as opposed to other CVs that are in partial coverage)
prep.data$RPP<-ifelse(prep.data$MANAGEMENT_PROGRAM_CODE=='RPP' & prep.data$PROCESSING_SECTOR=='S', 'RPP', ' ')

# Identify the retained catch (source table = 'Y') as R and discards (source table = 'N') as D:
prep.data$RETAINED<-ifelse(prep.data$SOURCE_TABLE == 'Y', 'R', 'D')

# Aggregate the NPT and PTR trawl gears:
prep.data$COMBINED_GEAR <- ifelse(prep.data$AGENCY_GEAR_CODE %in% c('NPT', 'PTR'), 'TRW', prep.data$AGENCY_GEAR_CODE)

# Aggregate species groups the way the Observer Program Annual Report has in the past:
prep.data$SPECIES_GROUP <- ifelse(
  prep.data$FMP=='GOA' & prep.data$SPECIES_GROUP_CODE %in% 
    c('REXS', 'FSOL', 'ARTH', 'DFL4'),'DFL4', 
  (ifelse(prep.data$FMP=='GOA' & prep.data$SPECIES_GROUP_CODE %in% 
            c('SQID', 'OCTP', 'SCLP', 'AMCK'), 'OTHR', 
          (ifelse(prep.data$FMP=='GOA' & prep.data$SPECIES_GROUP_CODE %in% 
                    c('DUSK', 'REYE', 'THDS', 'POPA', 'DEM1', 'NORK', 'SRKR'), 'ROCK',
                  (ifelse(prep.data$FMP=='GOA' & prep.data$SPECIES_GROUP_CODE %in% 
                            c('BSKT', 'USKT', 'LSKT'), 'USKT', 
                          ifelse(prep.data$FMP=='BSAI' & prep.data$SPECIES_GROUP_CODE %in% 
                                   c('AKPL', 'FLO5', 'YSOL', 'RSOL', 'FSOL'), 'FLAT',
                                 (ifelse(prep.data$FMP=='BSAI' & prep.data$SPECIES_GROUP_CODE %in% 
                                           c('SQID', 'OCTP', 'SCLP'), 'OTHR', 
                                         (ifelse(prep.data$FMP=='BSAI' & prep.data$SPECIES_GROUP_CODE %in% 
                                                   c('SRKR', 'POPA', 'NORK', 'REYE'), 'ROCK', 
                                                 (ifelse(prep.data$FMP=='BSAI' & prep.data$SPECIES_GROUP_CODE %in% 
                                                           c('KMKA', 'ARTH', 'GTRB'), 'TURB', 
                                                         prep.data$SPECIES_GROUP_CODE))))))))))))))


# -------------------------------------------------------------- #
# - Some data corrections to strata for the 2019 Annual Report - #
# -------------------------------------------------------------- #

# Hardcode the following STRATA changes here:
#  1. The 2019 ADP calls for 3 vessels (the Middleton (5029), the Kariel (3759), and the Predator (2844)) to participate in the 
#     EM innovation and research zero selection pool. The Defender (1472) is using the deployment of an EM Lite 
#     system (no cameras), so is also part of the EM innovation and research zero selection pool.  
#     Check to see if they got picked up in CAS.
#  2. There are trips in the EM_POT strata that used a tender and are called EM_TenP. For the annual
#     report, consolidate these all into the EM_POT stratum.

table(prep.data$STRATA)
#EM_HAL  EM_POT EM_TenP    FULL     HAL     POT    TenP   TenTR     TRW    ZERO 
#39347    2037     336  802507   80825    7380    1346    2644   40851   45924 

table(prep.data[prep.data$VESSEL_ID == 5029,]$STRATA)
table(prep.data[prep.data$VESSEL_ID == 3759,]$STRATA)
table(prep.data[prep.data$VESSEL_ID == 2844,]$STRATA)
table(prep.data[prep.data$VESSEL_ID == 1472,]$STRATA)


# Create an ORIGINAL.STRATA value and changes some of the STRATA values for the EM Research Zero pool and EM Pot stratum:
# 2019 version:
prep.data <- prep.data %>% 
    rename(ORIGINAL.STRATA = STRATA) %>% 
    mutate(STRATA = ifelse(VESSEL_ID %in% c('5029','3759','2844', '1472'), 'ZERO_EM_RESEARCH', 
                           # Consolidate the EM Tender Pot strata with the EM Pot strata:
                           ifelse(ORIGINAL.STRATA == 'EM_TenP', 'EM_POT', ORIGINAL.STRATA)))


table(prep.data$ORIGINAL.STRATA, prep.data$STRATA)
#        EM_HAL EM_POT   FULL    HAL    POT   TenP  TenTR    TRW   ZERO ZERO_EM_RESEARCH
#EM_HAL   39302      0      0      0      0      0      0      0      0               45
#EM_POT       0   2037      0      0      0      0      0      0      0                0
#EM_TenP      0    336      0      0      0      0      0      0      0                0
#FULL         0      0 802507      0      0      0      0      0      0                0
#HAL          0      0      0  79540      0      0      0      0      0             1285
#POT          0      0      0      0   7380      0      0      0      0                0
#TenP         0      0      0      0      0   1346      0      0      0                0
#TenTR        0      0      0      0      0      0   2644      0      0                0
#TRW          0      0      0      0      0      0      0  40851      0                0
#ZERO         0      0      0      0      0      0      0      0  45924                0
 

# The following 3 fields were commented out (for now?) for 2018 Annual Report ------------------------------------------------#
# Simplify some of the Strata:
#prep.data$SIMPLIFIED_STRATA <- ifelse(prep.data$STRATA == 'EM', 'EM Voluntary',
#                                      ifelse(prep.data$STRATA == 'ZERO_EM_RESEARCH', 'ZERO', prep.data$STRATA))

# Distinguish human observed from EM observed from not observed:
# (NOTE: Not able to do this with 2017 Valhalla)
#prep.data$HUMAN_OBSERVED <- ifelse(prep.data$OBSERVED_FLAG == 'Y' & !(prep.data$SIMPLIFIED_STRATA == 'EM Voluntary'), 
#                                   'Observed', 'Not Observed')

#prep.data$HUMAN_EM_OBSERVED <- ifelse(prep.data$OBSERVED_FLAG == 'Y', 'Observed', 'Not Observed') 

# ----------------------------------------------------------------------------------------------------------------------------#

# Not needed for 2019?:
# Note: some EM_POT and TenEM_POT data are flagged as observed, however, CAS does not use that for estimation in 2018.
#       As a result, that catch is NOT included in Tables 4-3 or 4-4 as observed so create a flag to distinguish it 
#table(prep.data$OBSERVED_FLAG, prep.data$STRATA)


# Add a flag indicating if the trip was in a strata where the observed trip would be used in CAS for catch estimation
#    or not (EM_POT, TenEM_POT, ZERO_EM_RESEARCH, and ZERO):
#prep.data$USED_IN_EST <- ifelse(prep.data$STRATA %in% c('EM_POT','TenEM_POT', 'ZERO_EM_RESEARCH', 'ZERO'), 'N', 'Y') 


# Add a flag indicating if the observed data are used in CAS for estimation or not:
#    (This is the flag distinguishes the EM_POT data from other observed trips)
#prep.data$OBS_FOR_EST <- ifelse(prep.data$OBSERVED_FLAG == 'Y' & (prep.data$USED_IN_EST == 'Y'), 'Observed', 'Not Observed')
prep.data$OBS_FOR_EST <- ifelse(prep.data$OBSERVED_FLAG == 'Y', 'Observed', 'Not Observed')

#table(prep.data$OBSERVED_FLAG, prep.data$OBS_FOR_EST)


# Add a vessel length category for proposed Ch.4 Table 4-1:
prep.data$VESSEL_LENGTH_CATEGORY <- ifelse(as.numeric(prep.data$LENGTH_OVERALL) <40, 'LT40', 
                                           (ifelse((as.numeric(prep.data$LENGTH_OVERALL) >= 40 & 
                                                      as.numeric(prep.data$LENGTH_OVERALL) <57.5), 'BT40_57', 
                                                   (ifelse(as.numeric(prep.data$LENGTH_OVERALL) >= 57.5, 'GT58', 'Error')))))

table(prep.data$VESSEL_LENGTH_CATEGORY)


# ------------------------------------------------------------------------------------- #
# ---                Catch table weights and Halibut PSC estimates                  --- #
# ------------------------------------------------------------------------------------- #

# From the annual report:
#       "DMRs are not applied to raw observer data prior to expansion to the entire fishery. Therefore, in order
#       to present observed and unobserved catch, the data in this chapter are presented without DMRs. As
#       such, these data represent total catch - not total mortality; it is important to recognize that not all of the
#       halibut that were discarded would have died."  (2016 Annual Report, p.87)

# In the data warehouse the species_weight column (and therefore Valhalla's weight_posted column) have had 
# the discard mortality rate applied to halibut PSC. (NOTE: wastage in the halibut fishery has had a DMR of 1 applied which is the 
# same as NOT having a DMR applied)

# In order to remain consistent with previous versions of the annual report, the Ch. 4 catch tables need to include
# halibut PSC estimates rather than halibut mortality: 
#     - For 2017 PSC estimates were added to a version of Valhalla from v_cas_psc_estimate (in Cathy_valhalla_3_14_17.RData).  
#     - For 2018 and 2019, the mortality rates from the warehouse are needed to back calculate the estimates from the mortality
#       (NOTE: you cannot simply use the DMRs in akfish.core_halibut_mortality_rate because some discards occur through deck sorting
#              and have their own haul or vessel specific DMR applied)


# Using the run date from Valhalla, query the data warehouse to get the CAS run used in Valhalla's creation (cas_run_id 10395)
# and get the mortality rates that were applied to the PSC:
warehouse.query <- "WITH cas_run AS
(-- Identify the CAS run that was likely used to create Valhalla (a.k.a. the last successful CAS run before the Valhalla run date)
SELECT * FROM 
(-- Get all the CAS runs for 2019 that happened before the creation of Valhalla:
SELECT *
FROM akfish_report.transaction_fact_log a
WHERE a.year = 2019 
AND a.status = 'SUCCESS'
AND a.end_time < '03-Apr-2020' 
ORDER BY a.end_time desc
)
WHERE rownum = 1 
) 

SELECT distinct crs.catch_report_source_pk, -- identified as ca_reference_key in 2019 Valhalla dataset
cr.data_source_type_code,
crs.catch_report_type_code,
sg.species_group_code, 
txn.mortality_rate_pk,
mr.rate AS mortality_rate,
r.value AS source_table 
FROM akfish_report.transaction_fact txn
JOIN akfish_report.transaction_fact_log tfl ON txn.transaction_fact_log_pk = tfl.transaction_fact_log_pk  
JOIN cas_run cas ON tfl.cas_run_id = cas.cas_run_id 
JOIN akfish_report.catch_report cr ON txn.catch_report_pk = cr.catch_report_pk 
JOIN akfish_report.catch_report_source crs ON cr.catch_report_source_pk = crs.catch_report_source_pk 
JOIN akfish_report.species_group sg ON txn.species_group_pk = sg.species_group_pk 
JOIN akfish_report.flag r ON txn.retained_flag_pk = r.flag_pk 
JOIN akfish_report.mortality_rate mr ON txn.mortality_rate_pk = mr.mortality_rate_pk 
WHERE tfl.year = 2019
AND sg.species_group_code = 'HLBT'
AND r.value = 'N' -- discarded"

#warehouse.data <- sqlQuery(channel,paste(warehouse.query),as.is=TRUE,max=10000000)
# Read in a snapshot instead:
warehouse.data <- read.csv("C:\\Teleworking\\Obs Annual Report\\Ch 4\\warehouse_data.csv")

# The Valhalla data have the old primary key column name for catch reports (ca_reference_key) and the data warehouse data
# have the new primary key (catch_report_source_pk). Rename it, for compatability sake:

warehouse.data <- warehouse.data %>% 
  rename(CA_REFERENCE_KEY = CATCH_REPORT_SOURCE_PK)


# Merge the datasets so the DMR can be used to back calculate the estimate:
apply.dmr <- prep.data %>% 
  left_join(warehouse.data, 
            by = c("CA_REFERENCE_KEY", "SPECIES_GROUP_CODE", "SOURCE_TABLE", "DATA_SOURCE_TYPE_CODE", "CATCH_REPORT_TYPE_CODE")) %>% 
  # Calculate the estimate from the DMR and the mortality for PSC:
  mutate(HALIBUT_WEIGHT_NOMORT = ifelse(!is.na(MORTALITY_RATE), as.numeric(WEIGHT_POSTED)/as.numeric(MORTALITY_RATE), NA))



# ---- Review halibut data and the new halibut_weight_mort field just to make sure I understand which to use for the Ch4 catch tables:
        halibut <- apply.dmr[prep.data$SPECIES_GROUP == 'HLBT',]

        # Halibut records without a HALIBUT_WEIGHT_NOMORT value are SUPPOSED TO BE retained catch:   Use weight_posted for these records.
        retained.halibut <- halibut[is.na(halibut$HALIBUT_WEIGHT_NOMORT),]
        table(retained.halibut$RETAINED)  # these results should ALL be R!!  If not, investigate! 

        # Halibut records with a HALIBUT_WEIGHT_NOMORT value are discarded catch:  Use halibut_weight_nomort for these records.
        discarded.halibut <- halibut[!is.na(halibut$HALIBUT_WEIGHT_NOMORT),]
        table(discarded.halibut$RETAINED)

        discarded.halibut <- discarded.halibut %>% 
          mutate(difference = as.numeric(WEIGHT_POSTED) - HALIBUT_WEIGHT_NOMORT)

        # Verify that when discarded halibut is wastage, there is no difference between the weight posted and the weight 
        # with no DMRs applied:
        table(discarded.halibut[discarded.halibut$GROUNDFISH_FLAG=='Y',]$difference)
# ----------------------------------------------------------------------------------------------------------------------------

# Specify that retained halibut (and all other species groups) should use weight_posted whereas discarded halibut (PSC and wastage)
# should use the halibut_weight_nomort field for the Ch.4 catch tables:
apply.dmr <- apply.dmr %>% 
   mutate(catch_table_weight = ifelse((SPECIES_GROUP == 'HLBT' & RETAINED == 'D'), 
                                      HALIBUT_WEIGHT_NOMORT, as.numeric(WEIGHT_POSTED)))
 
        
# Fix the "problem" record mentioned above by replacing the 1 NA record with 0:
#apply.dmr$catch_table_weight[is.na(apply.dmr$catch_table_weight)] <- 0

# Identify the working dataset:        
work.data <- apply.dmr         


# ----------------------------------------------------------------- #
# ---                     Summarize Catch                       --- #
# ----------------------------------------------------------------- #

# ------- Summarize Total Catch of Groundfish, Directed Halibut, and PSC halibut (observed + not observed): --------- #
calc.total <- work.data %>% 
  # refine to groundfish (which includes directed halibut) OR psc halibut:
  filter(GROUNDFISH_FLAG == 'Y' | (PSC_FLAG == 'Y' & SPECIES_GROUP_CODE == 'HLBT')) %>% 
  group_by(FMP, PROCESSING_SECTOR, RPP, AGENCY_GEAR_CODE, SPECIES_GROUP, RETAINED) %>% 
  summarise(TONS = sum(catch_table_weight)) %>% 
  # add 'Total' as a filler value in the used for estimation flag:
  mutate(OBS_FOR_EST = 'Total') %>% 
  #mutate(OBSERVED_FLAG = 'Total') %>% 
  data.frame()

# ------- Summarize Catch of Groundfish, Directed Halibut, and PSC halibut as observed or not observed --------- #
#           in the context of whether or not the observer data would be used for catch estimation                #
observed <- work.data %>% 
  # refine to groundfish (which includes directed halibut) OR psc halibut:
  filter(GROUNDFISH_FLAG == 'Y' | (PSC_FLAG == 'Y' & SPECIES_GROUP_CODE == 'HLBT')) %>% 
  group_by(FMP, PROCESSING_SECTOR, RPP, OBS_FOR_EST, AGENCY_GEAR_CODE, SPECIES_GROUP, RETAINED) %>% 
  #group_by(FMP, PROCESSING_SECTOR, RPP, OBSERVED_FLAG, AGENCY_GEAR_CODE, SPECIES_GROUP, RETAINED) %>% 
  summarise(TONS = sum(catch_table_weight)) %>% 
  data.frame()


# Combine the observed catch and total catch summaries:
with.totals <- rbind(observed, calc.total[,c(1,2,3,5,6,7,8,4)])


# -------------------------------------------------------------------------- #
# ---                 Create 2013 - YEAR Catch Tables                    --- #
# -------------------------------------------------------------------------- #

# For 2018:
#previous.catch.table <- read.csv(paste0("S://Observer Program Annual Report//", 
#                       YEAR - 1, 
#                       "_Annual_Report//Chap4_Descriptive_Info//2013_", 
#                       YEAR - 1, "_catchtables.csv"))

# For 2019:
previous.catch.table <- read.csv("C:\\Teleworking\\Obs Annual Report\\Ch 4\\2013_2018_catchtables.csv")


#previous.catch.table <- plyr::rename(previous.catch.table, c("GEAR" = "AGENCY_GEAR_CODE", "SECTOR" = "PROCESSING_SECTOR", 
#                           "Observed_FLAG" = "HUMAN_OBSERVED", "RETAINED_FLAG" = "RETAINED"))
#previous.catch.table <- plyr::rename(previous.catch.table, c("HUMAN_OBSERVED" =  "OBS_FOR_EST"))


### CODE FOR WRITING THE CATCH TABLE CSV ###
# **FLAG** This needs to be formatted so it will fit in with the full catch table
with.totals <- with.totals[with.totals$OBS_FOR_EST %in% c('Observed', 'Total'),]
with.totals$YEAR <- YEAR
with.totals <- with.totals[,c(9,1,2,5,3,6,7,4,8)]

catch.tables <- rbind(previous.catch.table, with.totals)

#write.csv(catch.tables,paste0("S:/Observer Program Annual Report/",
#                             YEAR,
#                             "_Annual_Report/Chap4_Descriptive_Info/2013_",
#                             YEAR,
#                             "_catchtables.csv"), row.names = FALSE)

#write.csv(catch.tables,paste0("C:/Teleworking/Obs Annual Report/Ch 4","/2013_", YEAR, "_catchtables.csv"), row.names = FALSE)


# Clean up workspace (removes everything EXCEPT the objects listed) and save RData
rm(list= ls()[!(ls() %in% c('YEAR', 'previous.catch.table','work.data', "with.totals", "catch.tables",'VALHALLA', 'warehouse.data'))])

# For 2018:
#save(YEAR, dat, work.data, with.totals, catch.tables, warehouse.data,
#     file = paste0("S:/Observer Program Annual Report/", YEAR, "_Annual_Report/Chap4_Descriptive_Info/AR_descriptive_", 
#                   YEAR, "_data.RData"))

# For 2019:
#save(YEAR, previous.catch.table, work.data, with.totals, catch.tables, VALHALLA, warehouse.data,
#     file = paste0("C:/Teleworking/Obs Annual Report/Ch 4/AR_descriptive_", YEAR, "_data.RData"))

