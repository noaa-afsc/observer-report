# Setup -------------------------------------------------------------------

# Get packages and user-defined functions
source("3_helper.R")

# Random number seed
set.seed(052870)

# Report year (year that fishing and observing took place)
year <- 2022 

# Establish database connections
channel_cas <- dbConnect(drv = dbDriver('Oracle'), 
                         username =paste(Sys.getenv('CASid')), 
                         password =paste(Sys.getenv('CASpw')), 
                         dbname = Sys.getenv("myCASConnStr"))


# Get data ----------------------------------------------------------------

# As a work-around for the 2022 Annual Report, which will contain a very abbreviated version of Chapter 3 (only the equivalent to Table 3-3 from 
# the 2020 Annual Report), Phil ran 1_AR_data_abbreviated.R which pulled the raw AFSC data for Cathy (who isn't set up to access AFSC data directly)
#load("G:\\FMGROUP\\Observer Program Annual Report\\2022_Annual_Report\\2_AR_raw_afsc_data.RData") 
load("Z:\\FMGROUP\\Observer Program Annual Report\\2022_Annual_Report\\2_AR_raw_afsc_data.RData") 


# * Valhalla ----

# Pull in this report year's Valhalla
# The code that creates Valhalla is maintained separately from this project

#script <- paste0("select * from akfish_sf.valhalla where adp = ", year)
#work.data <- dbGetQuery(channel_cas, script)

# 2021 Valhalla data aren't currently in the database.  Load .RData file instead:
#load("G://FMGROUP//CADQ_library//observer_annual_reports_code//Valhalla Data//2021//2022-04-05CAS_VALHALLA.RData")
#load("G://FMGROUP//CADQ_library//observer_annual_reports_code//Valhalla Data//2021//2022-05-12CAS_VALHALLA.RData")

# 2022 Valhalla data aren't currently in the database.  Load .RData file instead:
#load("G://FMGROUP//CADQ_library//observer_annual_reports_code//Valhalla Data//2022//2023-04-04cas_valhalla.RData")
load("Z://FMGROUP//CADQ_library//observer_annual_reports_code//Valhalla Data//2022//2023-04-04cas_valhalla.RData")

work.data <- valhalla
rm(valhalla)

#Summary of coverage by strata and processing sector
#This is a check to make sure no entries look wonky
table(work.data$COVERAGE_TYPE, work.data$STRATA, work.data$PROCESSING_SECTOR, useNA='always')

# Data check for observed vessels under 40 ft., should be zero rows
work.data %>% 
  filter(LENGTH_OVERALL < 40 & OBSERVED_FLAG == "Y") %>% 
  select(VESSEL_ID, TRIP_ID, REPORT_ID, COVERAGE_TYPE, STRATA, OBSERVED_FLAG, TRIP_TARGET_CODE, REPORTING_AREA_CODE, LANDING_DATE, LENGTH_OVERALL) %>% 
  distinct() %>% 
  arrange(VESSEL_ID)

# Change date format to eliminate times and match what is in the database
work.data <- mutate(work.data, TRIP_TARGET_DATE = as.Date(TRIP_TARGET_DATE), LANDING_DATE = as.Date(LANDING_DATE))

# Check for TRIP_IDs that are repeated across ADP years
if(nrow(select(work.data, TRIP_ID, ADP) %>% 
        distinct() %>% 
        group_by(TRIP_ID) %>% 
        filter(n()>1) %>% 
        data.frame()) > 0){
  
  # If there are duplicate TRIP_IDs across ADP years, add ADP year to the front of *all* TRIP_IDs
  work.data$TRIP_ID <- paste(work.data$ADP, work.data$TRIP_ID, sep = ".")}

# * Salmon dockside monitoring ----

#The following query returns all landing ID's for offloads monitored for salmon all sectors.
#salmon.landings.obs <- dbGetQuery(channel_afsc, script)

# Data checks and clean up

#Number of offloads monitored for salmon by Observer Coverage Type (Full vs Partial)
salmon.landings.obs  %>%  group_by(OBS_COVERAGE_TYPE) %>% summarise(n=n())


# * ODDS ----

# Data checks and clean up

# Check for duplicates - should be no records (= 0)
sum(duplicated(odds.dat$TRIP_PLAN_LOG_SEQ)) 

# Database check - Should be no records
length(odds.dat[!is.na(odds.dat$TRIP_SELECTED_OBS) & odds.dat$TRIP_STATUS_CODE=="CN"]) 

# This confirms the check
table(odds.dat$TRIP_SELECTED_OBS, odds.dat$TRIP_STATUS_CODE, useNA='always')  

# Summary of trip dispositions and observer assignments 
# Sample plan sequences: 11 = gear + tender, 13 = EM selected for review, 14 = EM EFP
# 98 = EM not selected for review where IFQ fishing was to occur in more than one NMFS area.
# Trip status codes: CS	= Cancel by System, PD = Pending, CN = Cancelled, CP = Completed, CC = Cancel Cascaded
table(odds.dat$TRIP_OBS_CODE, odds.dat$TRIP_STATUS_CODE, odds.dat$SAMPLE_PLAN_SEQ, useNA='ifany')

# The trip_selected_obs = "Y" when sample_plan_seq = 98 means that coverage was requested (due 
# to fishing occuring in more than one area) but the random number generated was larger than the 
# programmed rate, and so the video was not selected for review. Since these trips aren't truly
# monitored, make trip_selected_obs = "N". Also change strata to include gear_type_code = 6 (pot)
# and gear_type_code = 8 (longline).
odds.dat <- odds.dat %>% 
  mutate(TRIP_SELECTED_OBS=ifelse(SAMPLE_PLAN_SEQ==98, "N", TRIP_SELECTED_OBS),
         DESCRIPTION=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==6 & TENDER_TRIP_FLAG=="N", paste0(DESCRIPTION, " - POT - No Tender"), DESCRIPTION),
         DESCRIPTION=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==6 & TENDER_TRIP_FLAG=="Y", paste0(DESCRIPTION, " - POT - Tender"), DESCRIPTION),
         DESCRIPTION=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==8 & TENDER_TRIP_FLAG=="N", paste0(DESCRIPTION, " - HAL - No Tender"), DESCRIPTION),
         DESCRIPTION=ifelse(SAMPLE_PLAN_SEQ==98 & GEAR_TYPE_CODE==8 & TENDER_TRIP_FLAG=="Y", paste0(DESCRIPTION, " - HAL - Tender"), DESCRIPTION))


# * EM ----

#Fix EM.data VESSEL_ID
EM.data <- rename(EM.data, PS_VESSEL_ID = VESSEL_ID)

EM.data <- merge(EM.data, transform.EM.data.vessel, 
                 by.x = "PS_VESSEL_ID",
                 by.y = "EM_VESSEL_ID", all.x = TRUE)

rm(transform.EM.data.vessel)


#Select data for this report year and recode gear type to those used by CAS
EM.gear <- select(EM.gear, TRIP_NUMBER, GEAR_TYPE_ID) %>% 
  distinct() %>% 
  arrange(TRIP_NUMBER) %>% 
  mutate(AGENCY_GEAR_CODE=ifelse(GEAR_TYPE_ID==6 | GEAR_TYPE_ID==7, "HAL", "NA"),
         AGENCY_GEAR_CODE=ifelse(GEAR_TYPE_ID==10 | GEAR_TYPE_ID==11, "POT", AGENCY_GEAR_CODE)) %>%
  select(TRIP_NUMBER, AGENCY_GEAR_CODE) %>%  
  distinct()

# Join gear types to EM data
EM.data <- left_join(EM.data, EM.gear, by="TRIP_NUMBER")

# Are there NAs in EM.data$AGENCY_GEAR_CODE?
EM.data %>% group_by(AGENCY_GEAR_CODE) %>% summarise(n=n_distinct(TRIP_NUMBER))

# Isolate VESSEL_IDs for NAs in EM.data$AGENCY_GEAR_CODE
gear_na_vessels <- filter(EM.data, is.na(AGENCY_GEAR_CODE)) %>% distinct(VESSEL_ID) %>% unlist() %>% as.vector()

# Isolate VESSEL_IDs with NAs in EM.data$AGENCY_GEAR_CODE 
# that logged trips of only one gear type in ODDS
single_gear_nas <- filter(odds.dat, VESSEL_ID %in% gear_na_vessels) %>% 
  distinct(VESSEL_ID, GEAR_TYPE_CODE,DESCRIPTION) %>% 
  arrange(VESSEL_ID, GEAR_TYPE_CODE) %>% 
  group_by(VESSEL_ID) %>% 
  filter(uniqueN(GEAR_TYPE_CODE) == 1) %>% 
  mutate(AGENCY_GEAR_CODE=ifelse(GEAR_TYPE_CODE==8, "HAL", NA)) %>% 
  mutate(AGENCY_GEAR_CODE=ifelse(GEAR_TYPE_CODE==6, "POT", AGENCY_GEAR_CODE)) %>% 
  distinct(VESSEL_ID, AGENCY_GEAR_CODE)

# Isolate VESSEL_IDs with NAs in EM.data$AGENCY_GEAR_CODE 
# that logged trips of more than one gear type in ODDS
multiple_gear_nas <- filter(odds.dat, VESSEL_ID %in% gear_na_vessels) %>% 
  distinct(VESSEL_ID, GEAR_TYPE_CODE) %>% 
  arrange(VESSEL_ID, GEAR_TYPE_CODE) %>% 
  group_by(VESSEL_ID) %>% 
  filter(uniqueN(GEAR_TYPE_CODE) > 1)

# Compare ODDS to EM.data for VESSEL_IDs with NAs in EM.data$AGENCY_GEAR_CODE
# to determine the most likely AGENCY_GEAR_CODE
filter(odds.dat, VESSEL_ID %in% multiple_gear_nas$VESSEL_ID) %>% 
  distinct(VESSEL_ID, PLANNED_EMBARK_DATE, GEAR_TYPE_CODE, DESCRIPTION) %>% 
  arrange(VESSEL_ID, PLANNED_EMBARK_DATE, GEAR_TYPE_CODE, DESCRIPTION)

filter(EM.data, VESSEL_ID %in% multiple_gear_nas$VESSEL_ID) %>% 
  distinct(TRIP_NUMBER, VESSEL_ID, TRIP_START_DATE_TIME, AGENCY_GEAR_CODE) %>% 
  arrange(VESSEL_ID, TRIP_START_DATE_TIME)

# Fix NAs in AGENCY_GEAR_CODE for EM.data
EM.data <- EM.data %>% 
           # NAs for vessels that (based on ODDS) fished only one gear in the report year can be assumed to be that gear in the EM data
           mutate(AGENCY_GEAR_CODE=ifelse(VESSEL_ID %in% single_gear_nas$VESSEL_ID[AGENCY_GEAR_CODE=="HAL"] & is.na(AGENCY_GEAR_CODE), "HAL", AGENCY_GEAR_CODE)) %>% 
           mutate(AGENCY_GEAR_CODE=ifelse(VESSEL_ID %in% single_gear_nas$VESSEL_ID[AGENCY_GEAR_CODE=="POT"] & is.na(AGENCY_GEAR_CODE), "POT", AGENCY_GEAR_CODE)) %>% 
           # NAs for vessels that (based on ODDS) fished multiple gears in the report year are recoded manually according to the comparison made immediately above 
           # and by looking at gear used in surrounding trips according to eLandings
           mutate(AGENCY_GEAR_CODE = ifelse(TRIP_NUMBER  == "22_SUMNERSTRAIT01.01", "POT", AGENCY_GEAR_CODE)) %>%
           mutate(AGENCY_GEAR_CODE = ifelse(TRIP_NUMBER  == "22_SUMNERSTRAIT02.01", "POT", AGENCY_GEAR_CODE)) %>%
           mutate(AGENCY_GEAR_CODE = ifelse(TRIP_NUMBER  == "22_SUMNERSTRAIT03.01", "POT", AGENCY_GEAR_CODE)) %>%
           mutate(AGENCY_GEAR_CODE = ifelse(TRIP_NUMBER  == "22_SUMNERSTRAIT04.01", "POT", AGENCY_GEAR_CODE)) %>%
           mutate(AGENCY_GEAR_CODE = ifelse(TRIP_NUMBER  == "22_SUMNERSTRAIT06.01", "POT", AGENCY_GEAR_CODE)) 


# The following query will provide a list of em selected trips and if they have been reviewed or not
# Query will only include trips in completed or pending status and will not include compliance trips.
# This query will also show the declared gear type and if reviewed, will show the em_reviewed_gear_type_code
# This query will also show when the HD was received by PSFMC and when the EM reviewed data was exported and sent to AFSC
# This query will also show the actual em trip start date and time and actual em trip end date and time which comes from the data on the HD.

# Important note: if an EM reviewed trip used multiple gear types on a trip (ie.  pot and longline) there will be 2 records in the output.


# Flip pending trips to completed if they have data reviewed
# For clarification, see email from Glenn Campbell on 3/11/20
EM.review$TRIP_STATUS[EM.review$EM_DATA_REVIEWED == "YES"] <- "COMPLETED"

# Fixed-gear EM research
# em_research <- dbGetQuery(channel_afsc, paste("select distinct adp, vessel_id, vessel_name, sample_plan_seq_desc, em_request_status
#                                               from loki.em_vessels_by_adp
#                                               where sample_plan_seq_desc = 'Electronic Monitoring -  research not logged '
#                                               and adp =", year))


# According to the 2022 ADP, there isn't an agency sponsored em_research in 2022:
# Create a dataframe manually (UGH!)
#em_research <- data.frame(cbind(c("2021", "2021"), c("5029", "1472"), c("Middleton", "Defender"), c("Electronic Monitoring -  research not logged ","Electronic Monitoring -  research not logged "), c("","") ))  
#colnames(em_research) <- c("ADP", "VESSEL_ID", "VESSEL_NAME", "SAMPLE_PLAN_SEQ_DESC", "EM_REQUEST_STATUS")


# Format strata names -----------------------------------------------------

# Translate ODDS sample plans into strata
odds.dat <- mutate(odds.dat, STRATA = recode(DESCRIPTION, 
                                             "Declared Gear & Tender Delivery - Pot No  Tender Delivery" = "POT",    
                                             "Declared Gear & Tender Delivery - Pot   Tender Delivery" = "POT",      
                                             "Declared Gear & Tender Delivery - Trawl  No Tender Delivery" = "TRW",  
                                             "EM Declared Gear & Tender - Longline No Tender Delivery" = "EM HAL",      
                                             "Declared Gear & Tender Delivery - Longline No Tender Delivery" = "HAL",
                                             "EM Declared Gear & Tender - Pot gear - No Tender Delivery" = "EM POT",    
                                             "Declared Gear & Tender Delivery - Trawl  Tender Delivery" = "TRW",    
                                             "EM Compliance Monitoring - Fishing IFQ mulitple Areas - HAL - No Tender" = "EM Compliance HAL",
                                             "Declared Gear & Tender Delivery - Longline  Tender Delivery" = "HAL",
                                             "EM Declared Gear & Tender -  Pot gear Tender delivery" = "EM POT",
                                             "EM Declared Gear & Tender - Longline Tender Delivery" = "EM HAL",
                                             "EM Compliance Monitoring - Fishing IFQ mulitple Areas - POT - No Tender" = "EM Compliance POT",
                                             "EM Exempt fish Permit - Trawl - No Tender Delivery" = "EM TRW EFP",
                                             "EM Exempt Fish Permit -  Trawl - Tender Delivery" = "EM TRW EFP"))

# Format strata names in Valhalla
work.data <- mutate(work.data, STRATA = recode(STRATA,
                    "EM_POT" = "EM POT",
                    "EM_HAL" = "EM HAL",
                    "EM_TRW_EFP" = "EM TRW EFP"))

# Identify trips by EM research vessels
work.data <- work.data %>% 
             mutate(STRATA = ifelse(VESSEL_ID %in% em_research$VESSEL_ID, "Zero EM Research", STRATA))

# Lookup table for strata in partial coverage category
partial_desc <- data.table(STRATA = c("HAL", "POT", "TRW", "EM HAL", "EM POT", "EM TRW EFP"), 
                      descriptions = c("hook-and-line gear", "pot gear", "trawl gear", "hook-and-line gear with electronic monitoring", "pot gear with electronic monitoring", "trawl gear with electronic monitoring")) 


# For some reason the 2022 partial data.table does not contain a row for GOA EM TRW EFP.  Add one manually as a hack:
partial <- rbind(partial, list(0.3330, as.POSIXct('2021-12-31 23:00:00', '%Y-%m-%d %H:%M:%S', tz=Sys.timezone()),13,'Trawl','EM EFP'))


partial[, GEAR := ifelse(GEAR %like% "Pot", "POT", ifelse(GEAR %like% "Longline", "HAL", ifelse(GEAR %like% "Trawl", "TRW", GEAR)))] # Simplify gear types
partial[, STRATA := ifelse(           # Define strata based on sample plan and gear type
  SAMPLE_PLAN %like% "Electronic Monitoring", paste("EM", GEAR, sep=" "), ifelse(
    SAMPLE_PLAN %like% "EM EFP" & GEAR == "TRW", "EM TRW EFP", ifelse(
      SAMPLE_PLAN %like% "Gear Type", GEAR, NA)))]
partial <- unique(partial[, .(Effective_Date = as.Date(EFFECTIVE_DATE), STRATA, Rate = RATE)])[order(Effective_Date, STRATA)]  # Run unique on simplified gear and sample plans
partial[STRATA == "EM TRW EFP", Rate := 0.3330]   # Make the expected rate for partial coverage EM TRW EFP equal to the shoreside monitoring rate (not in ODDS)  
partial[, descriptions := partial_desc[partial, descriptions, on=.(STRATA)]]    # Merge descriptions in 
partial[, formatted_strat := paste0("*", STRATA, "*")]                          # Create formatted_strata column
partial[, txt := paste0(formatC(round(Rate * 100, 2), format='f', digits=2), '% in the ', formatted_strat, ' stratum')]    # Create txt column that combines Rate and formatted_strata
dcast(partial, STRATA ~ Effective_Date, value.var="Rate")   # Note that if the rates change for some strata and not others, 'NA' is returned




# Save --------------------------------------------------------------------

# Remove any remaining unwanted objects and save data
#rm(location, channel_afsc, channel_akro)

# Save
#save.image(file = "2_AR_data.Rdata")
#save.image(file = paste0("G:\\FMGROUP\\Observer Program Annual Report\\",year,"_Annual_Report\\2_AR_data.Rdata"))
#save.image(file = paste0("Z:\\FMGROUP\\Observer Program Annual Report\\",year,"_Annual_Report\\2_AR_data.Rdata"))
