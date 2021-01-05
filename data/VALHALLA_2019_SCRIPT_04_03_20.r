#note- set up for R version 3.5.1 ("Feather Spray") on my desktop. 



library('ROracle')
library('data.table')
library('ggplot2')
library('partykit')
library('compiler')
library('dplyr')
library('sqldf')
library('stringr')

WD<-"S:\\CADQ_library\\observer_annual_reports_code\\Valhalla Data\\2019\\"
 setwd("S:\\CADQ_library\\observer_annual_reports_code\\Valhalla Data\\2019")

channel_cas <- dbConnect(drv = dbDriver('Oracle'), username =paste(Sys.getenv('CASid')) , password =paste(Sys.getenv('CASpw')) , dbname = Sys.getenv("myCASConnStr"))
channel_afsc<-dbConnect(drv = dbDriver('Oracle'), username =paste(Sys.getenv('AFSCid')) , password =paste(Sys.getenv('AFSCPW')) , dbname = Sys.getenv("myAFSCConnStr"))


fnct.getdata<-function(channel_cas,channel_afsc,year_start,year_end){

year_start<-year_start
year_end<-year_end



'%+%'<-function(year_start,year_end) paste(year_start,year_end,sep="")

GASPER_SCHEMA<-'SELECT *
  FROM jgasper.v_valhalla_warehouse a
  where a.year between '%+% year_start %+%'and '%+% year_end %+%'' 


data_warehouse<-data.table(dbGetQuery(channel_cas,GASPER_SCHEMA))
setkey(data_warehouse,REPORT_ID)

REPORT_ID<-"SELECT distinct a.landing_report_id 
  FROM akfish_report.observer_haul_landing_report a
"
REPORT_ID_LIST<-data.table(dbGetQuery(channel_cas,REPORT_ID))


setkey(REPORT_ID_LIST,LANDING_REPORT_ID)
print(REPORT_ID_LIST)

EM<-" SELECT a.PERMIT as VESSEL_ID
    FROM atl_lov_vessel a,
    norpac.odds_eligible_opt_strata ea,
    norpac.odds_em_vessel_request er,
    norpac.odds_em_request_status es
    WHERE
    a.vessel_seq = ea.vessel_seq AND
    ea.eligible_opt_seq = er.eligible_opt_seq AND
    er.em_vessel_req_seq = es.em_vessel_req_seq AND
    (es.status_year between "%+% year_start %+%'and '%+% year_end %+%" AND es.em_req_status = 'A')"

EM_list<-data.table(dbGetQuery(channel_afsc,EM))

print(EM_list)

return(list(EM_list=EM_list,REPORT_ID_LIST=REPORT_ID_LIST,data_warehouse=data_warehouse))
}


QUERIES<-fnct.getdata(channel_cas,channel_afsc,2019,2019)
EM_list<-QUERIES$EM_list
EM_list$VESSEL_ID<-as.integer(EM_list$VESSEL_ID)
REPORT_ID_LIST<-QUERIES$REPORT_ID_LIST
data_warehouse<-QUERIES$data_warehouse
rm(QUERIES)


#NEED TO USE DATE RANGE TO DETERMINE IF REPORT_ID IS IN EM TABLE. JOIN TO ALL REPORTS FROM OBS DATA
#NOTE 50 RATE WON'T WORK SINCE MOST VESSELS IFQ (and hance 50 rate for groundfish is not vessel-specific)

EM_EL_REPORTS<-function(data_warehouse,channel_cas,channel_afsc,REPORT_ID_LIST){

           EM_HAULS<-"select distinct em.original_em_trip_number, v.VESSEL_ID, em.retrieval_start_date as deployment_date ,em.retrieval_end_date AS retrieval_date
                    from akfish_report.catch_report_species_fact f
                    join akfish_report.catch_report_source cr on f.CATCH_REPORT_SOURCE_PK = cr.CATCH_REPORT_SOURCE_PK
                    join akfish_report.vessel v on f.VESSEL_PK = v.VESSEL_PK
                    join akfish_report.em_haul em on f.EM_HAUL_PK = em.EM_HAUL_PK
                    where cr.CATCH_REPORT_TYPE_CODE = 'EM'
                    and cr.expire_date is null"

        em_hauls<-data.table(dbGetQuery(channel_cas,EM_HAULS))

        em_landings_temp<-unique(data_warehouse[CAS_STRATA %like% "EM",.(VESSEL_ID,REPORT_ID,TRIP_TARGET_DATE,LANDING_DATE)])

        REPORT_ID<-data.table(sqldf("SELECT distinct em_hauls.original_em_trip_number, em_landings_temp.REPORT_ID as LANDING_REPORT_ID
                    FROM em_hauls LEFT JOIN em_landings_temp 
                  ON em_hauls.VESSEL_ID = em_landings_temp.VESSEL_ID 
                  AND em_hauls.RETRIEVAL_DATE BETWEEN em_landings_temp.TRIP_TARGET_DATE 
                  AND em_landings_temp.LANDING_DATE"))
        
        REPORT_ID<-REPORT_ID[!is.na(LANDING_REPORT_ID)]

##get pot report ID for 2019

EM_TRIPS_REVIEWED<-"with em as(
                select
                case
                when em_reviewed.trip_plan_log_seq is not null
                then 'YES'
                else 'NO'
                End as EM_DATA_REVIEWED,   
                em_reviewed.trip_number as em_reviewed_trip_number, 
                em_selected.*
                from    
                ( select distinct
                    a.provider_company_name as provider,
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
                    i.user_reqst_coverage,
                    i.sample_plan_seq,
                    e.closed_landing_id,
                    e.closed_return_date,
                    e.closed_plant_offloaded_to,
                    e.closed_processor_id
                    from norpac.odds_provider a,
                    em_provider.empr_vessel_vmp b,
                    norpac.odds_em_vessel_request f,
                    norpac.odds_eligible_opt_strata g,
                    norpac.odds_vessel_sample_plan c,
                    norpac.atl_lov_vessel d,
                    norpac.odds_logged_trip_summary_v e,
                    norpac.odds_em_request_status h,
                    norpac.odds_monitor i
                    where a.provider_seq = b.provider_seq
                    and b.em_vessel_req_seq = f.em_vessel_req_seq
                    and f.eligible_opt_seq = g.eligible_opt_seq
                    and g.vessel_seq = d.vessel_seq
                    and c.vessel_seq = d.vessel_seq
                    and d.permit = e.akr_vessel_id
                    and e.odds_trip_number = i.trip_plan_log_seq
                    and e.year = h.status_year    
                    and e.observer_status_code <> 'NO'
                    --and e.year = 2019
                    --and h.status_year = 2019
                    and i.sample_plan_seq in (13))em_selected
                    
                   left join
                   
                   (select a.trip_plan_log_seq, a.trip_number 
                       from em_pac_review.em_trip a)em_reviewed
                       
                  on em_selected.ODDS_TRIP_NUMBER = em_reviewed.trip_plan_log_seq  
                  
                  order by em_selected.provider asc, em_selected.odds_trip_number asc)

                  select *
                  FROM em
                  WHERE em_data_reviewed = 'YES'"

EM_TRIPS_REVIEWED<-data.table(dbGetQuery(channel_afsc,EM_TRIPS_REVIEWED))

POT_EM<-unique(EM_TRIPS_REVIEWED[GEAR_TYPE_LOGGED=="POT",.(EM_REVIEWED_TRIP_NUMBER, VESSEL_ADFG_NUMBER,CLOSED_LANDING_ID,CLOSED_RETURN_DATE)])
        POT_EM[,VESSEL_ADFG_NUMBER:=str_pad(VESSEL_ADFG_NUMBER,5,pad="0")]


VESSEL_LOOKUP<-"SELECT distinct a.id as VESSEL_ID,a.adfg_number
                FROM akfish.v_vessel a"
VESSEL_LOOKUP<-data.table(dbGetQuery(channel_cas,VESSEL_LOOKUP)) 
    VESSEL_LOOKUP[,ADFG_NUMBER:=as.character(ADFG_NUMBER)]          

POT_EM<-merge(POT_EM,VESSEL_LOOKUP,by.x="VESSEL_ADFG_NUMBER",by.y="ADFG_NUMBER",all.x=TRUE,all.y=FALSE)


POT_REPORT_LIST<-POT_EM[is.na(CLOSED_RETURN_DATE),.(CLOSED_LANDING_ID,VESSEL_ID)][!is.na(CLOSED_LANDING_ID)]
temp <- strsplit(POT_REPORT_LIST$CLOSED_LANDING_ID, split = ",")
POT_REPORT_ID<-data.table(REPORT_ID_OLD = rep(POT_REPORT_LIST$CLOSED_LANDING_ID, sapply(temp, length)), LANDING_REPORT_ID = unlist(temp))

#EXTRACT VESSEL AND LANDING DATES FROM DATA WAREHOUSE AND AFSC TO GET REPORT ID WHERE NOT REPORTED BY AFSC

POT_LANDINGS_LIST<-POT_EM[!is.na(CLOSED_RETURN_DATE),.(CLOSED_RETURN_DATE,VESSEL_ID)]
POT_ALLLANDINGS<-unique(data_warehouse[AGENCY_GEAR_CODE=="POT" & VESSEL_ID%in%POT_LANDINGS_LIST[,VESSEL_ID],.(LANDING_DATE,VESSEL_ID,REPORT_ID)][!is.na(LANDING_DATE)])

POT_LANDINGS<-merge(POT_LANDINGS_LIST,POT_ALLLANDINGS,by.x=c("VESSEL_ID", "CLOSED_RETURN_DATE"),by.y=c("VESSEL_ID", "LANDING_DATE"),all.y=FALSE,all.x=TRUE)

# Reslove NAs
if(nrow(POT_LANDINGS[is.na(REPORT_ID)]) > 0){message("Resloved date mismatches between AFSC and AKRO for EM POT trips.")}
# Isolate trips for which dates don't match exactly, resulting in NAs
POT_LANDINGS_NAS    <- POT_LANDINGS[is.na(REPORT_ID), .(CLOSED_RETURN_DATE, LANDING_DATE = as.Date(CLOSED_RETURN_DATE), VESSEL_ID)][order(VESSEL_ID, LANDING_DATE)]
POT_ALLLANDINGS_NAS <- POT_ALLLANDINGS[VESSEL_ID %in% POT_LANDINGS_NAS$VESSEL_ID][, LANDING_DATE := as.Date(LANDING_DATE)][order(VESSEL_ID, LANDING_DATE)]
# Merge on vessel and nearest date
POT_LANDINGS_NAS    <- POT_ALLLANDINGS_NAS[POT_LANDINGS_NAS, on = .(VESSEL_ID, LANDING_DATE), roll = "nearest"]
# Check that dates are close
POT_ALLLANDINGS_NAS[POT_LANDINGS_NAS, on = .(VESSEL_ID, REPORT_ID)][, .(LANDING_DATE, CLOSED_RETURN_DATE, VESSEL_ID, REPORT_ID)]
# Retain records that look reasonable
POT_LANDINGS_NAS    <- POT_LANDINGS_NAS[year(POT_LANDINGS_NAS$CLOSED_RETURN_DATE) == 2019]
# Isolate columns of interest
POT_LANDINGS_NAS    <- POT_LANDINGS_NAS[, .(CLOSED_RETURN_DATE, VESSEL_ID, REPORT_ID)]
# Bind newly resolved NAs to non-NAs
POT_LANDINGS        <- rbind(POT_LANDINGS[!is.na(REPORT_ID)], POT_LANDINGS_NAS)

POT_REPORT_ID2<-POT_LANDINGS[!is.na(REPORT_ID),.(REPORT_ID)]
POT_REPORT_ID2[,LANDING_REPORT_ID:=REPORT_ID]
POT_REPORT_ID2[,REPORT_ID:=NULL]

REPORT_ID_LIST<-rbind(REPORT_ID_LIST,REPORT_ID[ORIGINAL_EM_TRIP_NUMBER %in% EM_TRIPS_REVIEWED$EM_REVIEWED_TRIP_NUMBER, .(LANDING_REPORT_ID)],POT_REPORT_ID[,.(LANDING_REPORT_ID)],POT_REPORT_ID2)

return(REPORT_ID_LIST)
}






REPORT_ID_LIST<-EM_EL_REPORTS(data_warehouse,channel_cas,channel_afsc,REPORT_ID_LIST)
#create PSC and groundfish flags

data_warehouse[,PSC_FLAG:=ifelse(SPECIES_GROUP_TYPE=="PROHIBITED","Y","N")]
data_warehouse[,GROUNDFISH_FLAG:=ifelse(SPECIES_GROUP_TYPE=="GROUNDFISH","Y","N")]
data_warehouse[,SPECIES_GROUP_TYPE:=NULL]

##################################################################################################
#determine if observed and make sure observed flag is internally consistent within a trip

data_warehouse[,OBSERVED_FLAG:=ifelse(data_warehouse[,REPORT_ID] %in% REPORT_ID_LIST$LANDING_REPORT_ID | 
			CATCH_REPORT_TYPE_CODE=='OBS',"Y","N")]
data_warehouse[,COMBINATION_FLAG:=length(unique(OBSERVED_FLAG)),by=TRIP_ID]
data_warehouse[,OBSERVED_FLAG:=ifelse(COMBINATION_FLAG>1,"Y",OBSERVED_FLAG)]
data_warehouse[,COMBINATION_FLAG:=NULL]


###################################################################################################
###################################################################################################
#determine if trip was tendered 

data_warehouse[,TENDER:=ifelse(!is.na(TENDER_VESSEL_ADFG_NUMBER),"Y","N")]
data_warehouse[,COMBINATION_FLAG:=length(unique(TENDER)),by=TRIP_ID]
data_warehouse[,TENDER:=ifelse(COMBINATION_FLAG>1,"Y",TENDER)]
data_warehouse[,COMBINATION_FLAG:=NULL]

###############################################################################################
###############################################################################################
# Flag- turn to Y for IFQ and CDQ trips since these may have no FFP, but are federal trips
data_warehouse[,FPERMIT_FLAG:=ifelse(FFP_FLAG=="Y" | MANAGEMENT_PROGRAM_CODE %in% c("IFQ","CDQ"),"Y","N")]
setDT(data_warehouse)[,INFRAME_FLAG:=uniqueN(FPERMIT_FLAG),by=TRIP_ID]
data_warehouse[,INFRAME:=ifelse(FFP_FLAG=="Y" | 
				INFRAME_FLAG>1 | 
				MANAGEMENT_PROGRAM_CODE=="IFQ"|
				MANAGEMENT_PROGRAM_CODE=="CDQ" |
				CAS_STRATA=="F" ,"Y","N")]
VALHALLA_TEMP<-data_warehouse[INFRAME=="Y"]
#################################################################################################
##REMOVE STATE FISHERIES

A<-as.data.frame.matrix(table(VALHALLA_TEMP$TRIP_ID,VALHALLA_TEMP$MANAGEMENT_PROGRAM_CODE))
  A$TRIP_ID<-row.names(A)
  #IDENTIFY ANY STATE ONLY TRIPS
    AALL<-A[which(apply(A[,c("A80","AFA","CDQ","IFQ","OA","RPP")],1,sum)==0),]
    #REMOVE STATE ONLY TRIPS
   VALHALLA_TEMP<-VALHALLA_TEMP[which(!VALHALLA_TEMP$TRIP_ID %in% AALL$TRIP_ID),] 

#Clean out AI State water P-cod
VALHALLA_TEMP[,INFRAME:=ifelse(STATE_FISHERY_FLAG=="Y" & AGENCY_SPECIES_CODE==110 & PROCESSING_SECTOR=="M","N",INFRAME)]
VALHALLA<-VALHALLA_TEMP[INFRAME=="Y"]
##########################################################################################################

####################################################################
#EM VESSELS AND STRATA
##################################################################


#Create a variable called EM_FLAG to determine EM vessels FOR DEPLOYMENT. 
EM.IDENTIFY<-function(VALHALLA,EM_list,label){

EM_list[,NORPAC_EMID:=VESSEL_ID]
    VALHALLA[,EM_FLAG:=VALHALLA$VESSEL_ID%in%EM_list$NORPAC_EMID & VALHALLA$AGENCY_GEAR_CODE %in% c('POT','HAL')]

VALHALLA[,DEPLOY_STRATA:=CAS_STRATA]
EM<-label
VALHALLA[EM_FLAG=='TRUE',DEPLOY_STRATA:=paste(EM,AGENCY_GEAR_CODE,sep="_")]
VALHALLA[,EM_FLAG:=NULL]
}

VALHALLA<-EM.IDENTIFY(VALHALLA,EM_list,"EM")
#ADJUST TENDER STRATA TO REFLECT DEPLOYMENT, NOT THE STRATA CAS THINKS IT IS. CAS BASES TENDER STRATA ON WHETHER AN ADFG TENDER ID IS PRESENT, WHICH IS NOT ALWAYS THE CASE ON TENDER DEPLOYMENT TRIP. TRIP_ID IS BASED ON PORTS DB, SO TRIP ID IS ACCURATE FOR A TENDER DEPLOYMENT. 

tender.adjust<-function(EMVALHALLA,label){

TENDER_KEY<-unique(EMVALHALLA[TENDER=="Y" & like(CAS_STRATA,"Ten") & !(DEPLOY_STRATA %like% label),.(CAS_STRATA,TRIP_ID)])
TENDER_KEY[,DEPLOY_STRATAT:=CAS_STRATA]
TENDER_KEY[,TRIPID_TEMP:=TRIP_ID]
TENDER_KEY[,TRIP_ID:=NULL]
TENDER_KEY[,CAS_STRATA:=NULL]

setkey(TENDER_KEY,TRIPID_TEMP)
setkey(EMVALHALLA,TRIP_ID)
OUT<-merge(EMVALHALLA,TENDER_KEY,by.x="TRIP_ID",by.y="TRIPID_TEMP",all.x=TRUE)

#OUT HAS TWO DEPLOY STRATA: EM VOLUNTARY AND NA
#OUT HAS THE TENDER STRATA IN DEPLOY_STRATA
#OUT HAS EXISTING CAS STRATA. 
#NEED TO SET DEPLOY STRATA TO EITHER DEPLOY_STRATA or CAS STRATA

OUT[!is.na(DEPLOY_STRATAT),DEPLOY_STRATA:=DEPLOY_STRATAT]
#print(OUT[is.na(DEPLOY_STRATAT),])
#OUT[is.na(DEPLOY_STRATAT),DEPLOY_STRATA:=CAS_STRATA]
OUT[,DEPLOY_STRATAT:=NULL]

return(OUT)

}

VALHALLA<-tender.adjust(VALHALLA,"EM")

#LINES 300 -363 SHOULD BE REFACTORED. THIS CODE IS POORLY WRITTEN/old AND THESE COMBOS SHOULD BE COMBINED INTO 
#a single function
#CORRECTS FOR COMBO GEAR TRIPS
jig.hal.fix<-function(df1) {

df1$CHECK<-"N"
          TEST<-table(df1$TRIP_ID,df1$DEPLOY_STRATA)
          test<-ifelse(TEST>0,1,0)
              out<-apply(test,1,sum)
              fix<-names(out[out>1])
              print(fix)
                if(length(fix)>0){

                  for (i in 1:length(fix)) {
                      df1[df1$TRIP_ID==fix[i],"CHECK"]<-"Y"
                  }
                }
  return(df1[CHECK=="Y"])
}


MULT_COMPARE<-jig.hal.fix(VALHALLA)



#ADJUST POT AND HOOK AND LINE COMBO DEPLOYMENT TO BE SAME TRIP,
#ADJUST STATE AND FEDERAL IFQ ON SAME TRIP

adjust.halpot<-function(VALHALLA,MULT_COMPARE){
  setkey(MULT_COMPARE,TRIP_ID)
  #Clean up SMO and IFQ on same trip
  problems<-unique(MULT_COMPARE[CHECK=="Y",TRIP_ID])
  problems<-VALHALLA[TRIP_ID%in%problems & DEPLOY_STRATA!="N",.(DEPLOY_STRATA,TRIP_ID)]
      
  for (i in 1:length(problems[,DEPLOY_STRATA])){ 
          temp<-unique(VALHALLA[TRIP_ID==problems$TRIP_ID[i] & DEPLOY_STRATA!='N',DEPLOY_STRATA])   
              if(length(temp)>1) 
                VALHALLA[TRIP_ID==problems$TRIP_ID[i] & DEPLOY_STRATA=='N',DEPLOY_STRATA:=temp]
                rm(temp)
                  }


  prob_temp<-MULT_COMPARE[CHECK=="Y" & DEPLOY_STRATA=="H" | CHECK=="Y" & DEPLOY_STRATA=="P"]
  temp<-as.data.frame.matrix(table(prob_temp$TRIP_ID,prob_temp$DEPLOY_STRATA))
        vector_trip_id<-data.table(EXCEPTION_TRIPS=as.numeric(rownames(temp[temp$P>0 & temp$H>0,])))
            VALHALLA[TRIP_ID%in%vector_trip_id$EXCEPTION_TRIPS,DEPLOY_STRATA:="H"]

  #OCASSIONALLY CDQ will be mixed case full and partial. Need to set to full. One trip in 2019. 
    temp_trips<-unique(VALHALLA[DEPLOY_STRATA=='F',TRIP_ID])
    VALHALLA[TRIP_ID%in%temp_trips,DEPLOY_STRATA:='F'] 

            return(VALHALLA)
        }

VALHALLA<-adjust.halpot(VALHALLA,MULT_COMPARE)


################################################################################################################################

#MANUAL FIXES ON TRIP ID:
#max_trip_id<-max(VALHALLA[,TRIP_ID])
#PUT MANUAL HERE USING max ID. 


#####################################################################################################################################


#setting values for columns

VALHALLA[,`:=`( EXEMPTED=NA,
                TRIP_SEQ=NA,
                TIME_PERIOD=NA,
                INFRAME="Y",
                SELECTION=NA,
                VS_SELECTED_FLAG='N',
                PERMIT=VESSEL_ID,
                OBS_JOIN=NA,
                STRATA=DEPLOY_STRATA,
                ROWCNT=length(TRIP_ID),
                HAUL_NUMBER=NA)]

#removing columns
VALHALLA[,`:=`(FFP_FLAG=NULL,
                FPERMIT_FLAG=NULL,
                COUNT=NULL,
                INFRAME_FLAG=NULL,
                RATE_PRECEDENCE=NULL,
                STATE_FISHERY_FLAG=NULL,
                YEAR=NULL,
                 DEPLOY_STRATA=NULL,
                 TIME_PERIOD=NULL,
                 ROWCNT=NULL,
                 ACCOUNT_NAME=NULL
                )]



VALHALLA[STRATA=='F',STRATA:='FULL']
VALHALLA[STRATA=='H',STRATA:='HAL']
VALHALLA[STRATA=='P',STRATA:='POT']
VALHALLA[STRATA=='N',STRATA:='ZERO']
VALHALLA[STRATA=='TR',STRATA:='TRW']
VALHALLA[STRATA=='EM_P',STRATA:='EM_POT']
VALHALLA[STRATA=='EM_H',STRATA:='EM_HAL']


VALHALLA[,COVERAGE_TYPE:=ifelse(STRATA=="FULL","FULL","PARTIAL")]


#need to remove two vessel from zero due to vessel length issue between FMA and AKRO
VALHALLA[VESSEL_ID %in% c(1313),STRATA:=AGENCY_GEAR_CODE]
VALHALLA[VESSEL_ID %in% c(1313),LENGTH_OVERALL:=44]

# In 2019, the Cerulean was a partial CP that made multiple partial offloads between 7/2 and 8/27.
# These partial offloads showed up as multiple trips. Correct this here, since a CP trip only ends when *all* product is offloaded.
VALHALLA[VESSEL_ID == 33530 & TRIP_TARGET_DATE >= as.POSIXct("2019-07-02") & LANDING_DATE <= as.POSIXct("2019-08-27"), TRIP_ID := min(TRIP_ID)]

##################################################################
##ERROR CHECK: FULL COVERAGE UNOBSERVED TRIPS
print(VALHALLA[STRATA=='FULL'& OBSERVED_FLAG=="N"])
UNFULL <- unique(VALHALLA[STRATA=='FULL'& OBSERVED_FLAG=="N", .(TRIP_ID)])

#run only after confirming exceptions. 
VALHALLA[STRATA=='FULL' & OBSERVED_FLAG=="N" & TRIP_ID %in% unique(VALHALLA[!(REPORT_ID %in% c(4704395, 5519238, 5889872, 5889876, 5920346, 5920377, 5933118, 4636007)), TRIP_ID]), OBSERVED_FLAG:="Y"]
unique(VALHALLA[TRIP_ID %in% UNFULL$TRIP_ID, .(TRIP_ID, VESSEL_ID, OBSERVED_FLAG)])[order(OBSERVED_FLAG, VESSEL_ID, TRIP_ID)]

# REPORT_ID 4704395 was a hand troll trip on a 27ft vessel
# Change COVERAGE_TYPE and STRATA to reflect that
VALHALLA[TRIP_ID %in% unique(VALHALLA[REPORT_ID == 4704395, TRIP_ID]), ':=' (COVERAGE_TYPE="PARTIAL", STRATA="ZERO")]
unique(VALHALLA[TRIP_ID %in% UNFULL$TRIP_ID, .(COVERAGE_TYPE, STRATA, TRIP_ID, REPORT_ID, VESSEL_ID, OBSERVED_FLAG)])[order(OBSERVED_FLAG, VESSEL_ID, TRIP_ID)]

#save off full version
currentDate <- Sys.Date() 
FileName <- paste(WD,currentDate,"VALHALLA.RData",sep="") 
save(VALHALLA, file=FileName) 

VALHALLA%>% group_by(STRATA,OBSERVED_FLAG)%>%summarize(TRIPS=uniqueN(TRIP_ID))

DBVALHALLA<-"Select *
from akfish_sf.valhalla_scratch"

DBVALHALLA<-data.table(dbGetQuery(channel_cas,DBVALHALLA))


cbind(!names(DBVALHALLA) %in% names(VALHALLA),names(DBVALHALLA))
cbind(!names(VALHALLA) %in% names(DBVALHALLA),names(VALHALLA))

VALHALLA[,`:=`(TRANSACTION_PK=NULL,
                  STATE_FISHERY_FLAG=NULL,
                  RATE_PK=NULL,
                  CATCH_REPORT_PK=NULL,
                  HALIBUT_MORTALITY_RATE=NULL,
                  HALIBUT_WEIGHT_NOMORT=NULL,
                  ROWCNT=seq(1:length(VALHALLA[,TRIP_ID]))
                )]


rm(list=setdiff(ls(), "VALHALLA"))

currentDate <- Sys.Date() 
WD<-"S:\\CADQ_library\\observer_annual_reports_code\\Valhalla Data\\2019\\"
FileName <- paste(WD,currentDate,"CAS_VALHALLA.RData",sep="") 
save(VALHALLA, file=FileName)

WD<-"S:\\CADQ_library\\observer_annual_reports_code\\Valhalla Data\\2019\\"
FileName <- paste(WD,currentDate,"CAS_VALHALLA.CSV",sep="") 
write.csv(VALHALLA,file=FileName)
#need to add to existing table: CATCH_REPORT_TYPE_CODE, SPECIES_COUNT, PSC_FLAG, GROUNDFISH_FLAG, CAS_STRATA, STATE_FEDERAL_WATERS_FLAG, ACCOUNT_NAME

################################################################################################################################
#RUN SOME CHECKS
#check on obs flag- should be no combo yes and no flags. 
out<-VALHALLA%>%group_by(TRIP_ID)%>%summarize(out=length(unique(OBSERVED_FLAG)))%>%data.table()
out[out>1]

#check on duplicate strata- SHOULD ONLY BE EM STRATA
    TEST<-table(VALHALLA$TRIP_ID,VALHALLA$STRATA)
          test<-ifelse(TEST>0,1,0)
              out<-apply(test,1,sum)
              fix<-names(out[out>1])

        unique(VALHALLA[TRIP_ID%in%fix,.(STRATA,TRIP_ID)])


#check on duplicate coverage type- SHOULD be one per trip
   TEST<-table(VALHALLA$TRIP_ID,VALHALLA$COVERAGE_TYPE)
          test<-ifelse(TEST>0,1,0)
              out<-apply(test,1,sum)
              fix<-names(out[out>1])
#SHOULD BE ZERO ROWS
        unique(VALHALLA[TRIP_ID%in%fix,.(COVERAGE_TYPE,TRIP_ID)])