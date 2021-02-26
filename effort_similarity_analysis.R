# For 2020 Annual Report, will only use this script to map fishing effort and monitoring over time and space.
# Will not be used for gap analyses due to complications from COVID-19, waivers, short time period for random deployment, etc.

library(data.table)
library(ggplot2)
library(ggpubr)             # Replaced gridExtra (allows quick combining of plots that use the same legend): https://cran.r-project.org/web/packages/ggpubr/index.html
library(lubridate)
library(scales)

PRED <- F  # Turn this to 'T' if you want to look at predicted fishing effort in addition to actual fishing effort

#==============#
# DATA PREP ####

load("2_AR_data.Rdata")                  # Load source data
load("data/effort_prediction.rdata")     # Load effort prediction for 2020 ADP
if(PRED==T) prd_2019 <- setDT(copy(sample_pops_out))[, .(pop_num, sampling_iter, STRATA_NEW, TRIP_ID, NEW_TRIP_ID, rate_obs, ODDS_RANDOM_DRAW, SELECTED)]
act_2019 <- unique(setDT(copy(work.data))[, .(ADP, TRIP_ID, STRATA, COVERAGE_TYPE, AGENCY_GEAR_CODE, TRIP_TARGET_DATE, LANDING_DATE, TRIP_TARGET_CODE, AREA=REPORTING_AREA_CODE, FMP, OBSERVED_FLAG)])
rm(list=setdiff(ls(), c("partial", "work.data", "act_2019", "prd_2019", "PRED")))    # unneeded objects

if(PRED==T){
  pt(load(file="2_final_ADP_data.rdata"))                        # Source data for 2019 ADP (to get trip start/end dates of predicted fishing effort) and predicted EM/ZE effort
  work_data_2019 <- setDT(copy(work.data))                                        # work.data that was used for 2019 ADP (1-Jan-2013 thru 20-Oct-2018)        
  work_data_2019 <- unique(work_data_2019[ADP %in% c(2017, 2018) & CVG_NEW=="PARTIAL" & AGENCY_GEAR_CODE != "JIG", .(ADP, TRIP_ID, STRATA_NEW, TENDER, GEAR=AGENCY_GEAR_CODE, TARGET=TRIP_TARGET_CODE, TRIP_TARGET_DATE, LANDING_DATE, AREA=REPORTING_AREA_CODE, FMP)])
  work_data_2019[, STRATA := ifelse(STRATA_NEW %in% c("TRW", "POT") & TENDER=="Y", paste(STRATA_NEW, "TENDER", sep="_"), STRATA_NEW)]
  work_data_2019[, c("START", "END") := as.list(range(TRIP_TARGET_DATE, LANDING_DATE)), by=TRIP_ID]
  work_data_2019[, c("STRATA_NEW", "TENDER", "TRIP_TARGET_DATE", "LANDING_DATE") := NULL]
  work_data_2019 <- unique(work_data_2019)
  work_data_2019[, POOL := ifelse(STRATA=="EM", "EM", ifelse(STRATA=="ZERO", "ZE", "OB"))]
  work_data_2019[STRATA=="ZERO", STRATA := "ZE"]
  
  trip_flags_2019 <- setDT(copy(trip.flags))   
  unique(work_data_2019[POOL!="ZE", .(TRIP_ID, STRATA)])[, .N, keyby=STRATA]  # number of unique trip_ids
  unique(trip_flags_2019[ADP %in% c(2017, 2018), .(TRIP_ID, STRATA_TENDER_GEAR)])[, .N, keyby=STRATA_TENDER_GEAR]
  rm(list=setdiff(ls(), c("prd_2019", "act_2019", "work_data_2019")))             # remove all objects from the environment except for 2019 predicted and actual effort
}

iter <- 10000    # Number of odds iterations to simulate
set.seed(12345)  # Setting seed number here for ODDS sampling

# TODO - Pull ODDS rates here instead of hard-coding. Should be available from 2_AR_data.Rdata.
# Programmed Rates - for use with predicted fishing effort or for comparisons to realized selection rates
#pr <- data.table(STRATA = c("EM", "HAL", "POT", "TRW", "TRW_TENDER", "POT_TENDER", "ZE"), RATE =   c(0.3000, 0.1771, 0.1543, 0.2370, 0.2712, 0.1611, 0.0000), key="STRATA")   # selection rates for 2019 

#===================#
# PREDICTED DATA ####

# Code for looking at predicted fishing effort is kept here for posterity, but won't be used in 2019 AR
if(PRED==T){
  pred_trp <- copy(work_data_2019)
  pred_trp <- unique(pred_trp[, .(POOL, STRATA), keyby=TRIP_ID])                  # This object has trip_ids and 2019 strata definitions
  
  pred_odds_ob <- copy(prd_2019)  # only OB pool had effort predicted via random sampling
  pred_odds_ob[, ITER := .GRP, by=.(pop_num, sampling_iter)]  # Define 'ITER' as unique combinations of pop_num and sampling_iter (should be 10,000)
  setnames(pred_odds_ob, c("rate_obs", "ODDS_RANDOM_DRAW", "SELECTED"), c("RATE", "ODDS", "SEL"))
  pred_odds_ob[, c("pop_num", "sampling_iter", "STRATA_NEW") := NULL]
  setkey(pred_odds_ob, TRIP_ID)
  pred_odds_ob <- pred_trp[pred_odds_ob]  # Merge in 2019 strata definitions and pool by trip_id
  pred_odds_ob <- pred_odds_ob[, .(ITER, POOL, STRATA, TRIP_ID, RATE, ODDS, SEL, NEW_TRIP_ID)]   # Keep NEW_TRIP_ID or else we'll lose trip_ids that were sampled more than once!
  pred_odds_ob[, SEL := SEL=="Y"]  # Make this boolean instead of character
  
  pred_ob <- work_data_2019[POOL=="OB"]
  uniqueN(pred_odds_ob$TRIP_ID) # 4450 distinct trip IDS in predicted ob data - make sure we get all from pred_ob
  pred_ob_ids <- unique(pred_odds_ob$TRIP_ID)
  pred_ob <- pred_ob[TRIP_ID %in% pred_ob_ids]
  uniqueN(pred_ob$TRIP_ID)      # 4450, so no trips were lost
  pred_ob[, ADP := NULL]        # Remove ADP column, and change year of start and end to 2019
  pred_ob[year(START)==2017, ':=' (START = START + years(2), END = END+years(2))]
  pred_ob[year(START)==2018, ':=' (START = START + years(1), END = END+years(1))]
  pred_ob <- pred_ob[, .(TRIP_ID, POOL, STRATA, TARGET, FMP, AREA, GEAR, START, END)]
  
  # For predicted effort for ZE/EM trips, use Oct 2017 - Oct 2018 and programmed trip selection rates (this matches pred_ob)
  
  max_date <- max(work_data_2019$START)   # "2018-10-17 UTC"
  pred_emze <- work_data_2019[POOL != "OB" & END >= (max_date - years(1))]
  pred_emze[, START := fifelse(ADP==2017, START + years(2), START + years(1))]       # data.table has "fifelse", which prevents these dates from automatically turning into numeric when I want to add 1 year to these dates
  pred_emze[, END   := fifelse(ADP==2017,   END + years(2),   END + years(1))]
  pred_emze <- pred_emze[, .(TRIP_ID, POOL, STRATA, TARGET, FMP, AREA, GEAR, START, END)]
  pred_emze_trp <- pred_trp[TRIP_ID %in% unique(pred_emze$TRIP_ID)]
  pred_emze_trp[, RATE := pr[pred_emze_trp, RATE, on=.(STRATA)]]
  pred_odds_emze <- rbindlist(rep(list(pred_emze_trp), times=iter), idcol="ITER")
  pred_odds_emze[, ODDS := runif(.N, min=0, max=1)]                               # Randomly select trips
  pred_odds_emze[, SEL := ODDS < RATE]
  pred_odds_emze <- pred_odds_emze[, .(ITER, POOL, STRATA, TRIP_ID, RATE, ODDS, SEL)]
  
  pred_odds <- rbind(pred_odds_ob[ITER %in% (1:iter)], pred_odds_emze[ITER %in% (1:iter)], fill=TRUE)[, c("RATE", "ODDS") := NULL]
  pred_date <- rbind(pred_ob, pred_emze)
  pred_date[TARGET=="B", TARGET:="P"]                                             # Coerce any 'bottom pollock' trips to regular pelagic pollock
  pred_date[, ':=' (START = as.Date(START), END = as.Date(END))]                  # Ensure start/end is in date format 
  pred_date <- unique(pred_date)         
  
  
  
  
}

#================#
# ACTUAL DATA ####

actu_date <- copy(act_2019)[COVERAGE_TYPE=="PARTIAL" & STRATA != "Zero EM Research"]
setnames(actu_date, c("TRIP_TARGET_CODE", "AGENCY_GEAR_CODE"), c("TARGET", "GEAR"))
table(actu_date$STRATA)
actu_date[, POOL := fifelse(STRATA %like% "EM", "EM", fifelse(STRATA=="ZERO", "ZE", "OB"))]
# actu_date[POOL %in% c("EM", "ZE"), STRATA := POOL]  # Keep STRATA the same 
# actu_date[POOL=="OB" & STRATA %like% "TRW", ':=' (STRATA = fifelse(STRATA %like% "No", "TRW", "TRW_TENDER"))]   # No longer have tender strata in 2020
# actu_date[POOL=="OB" & STRATA %like% "POT", ':=' (STRATA = fifelse(STRATA %like% "No", "POT", "POT_TENDER"))]
actu_date[, c("START", "END") := as.list(range(as.Date(TRIP_TARGET_DATE), as.Date(LANDING_DATE))), by=TRIP_ID]
actu_date[, GEAR := fifelse(GEAR %in% c("PTR", "NPT"), "TRW", GEAR)]
actu_date[, c("COVERAGE_TYPE", "TRIP_TARGET_DATE", "LANDING_DATE") := NULL]
actu_date[TARGET=="B", TARGET := "P"]
actu_date <- unique(actu_date[, .(TRIP_ID, POOL, STRATA, TARGET, FMP, AREA, GEAR, START, END, OBSERVED = OBSERVED_FLAG)])

# Determine realized trip selection rates. Use the realized rates (instead of programmed rates) so that differences in gap scores
# can be assumed to be due to non-random trip selection
rr <- unique(actu_date[, .(TRIP_ID, POOL, STRATA, SEL=OBSERVED=="Y")])[, .(N = .N, SEL=sum(SEL), RATE = sum(SEL)/.N), keyby=.(POOL, STRATA)]
# TODO - For now, (2020-Feb-25) EM TRW EFP trips are not being marked as monitored correctly.

# Before waivers were listed
unique(actu_date[START <= as.POSIXct("2020-03-25"), .(TRIP_ID, POOL, STRATA, SEL=OBSERVED=="Y")])[, .(N = .N, SEL=sum(SEL), RATE = sum(SEL)/.N), keyby=.(POOL, STRATA)]
# Looking at rates only after waivers were lifted. OB strata were 4-5% below target of 19.6 / 15.4 / 15.2 for TRW/HAL/POT
unique(actu_date[START >= as.POSIXct("2020-07-01"), .(TRIP_ID, POOL, STRATA, SEL=OBSERVED=="Y")])[, .(N = .N, SEL=sum(SEL), RATE = sum(SEL)/.N), keyby=.(POOL, STRATA)]
# When we had no waivers (start and end of year)
unique(actu_date[START <= as.POSIXct("2020-03-25") | START >= as.POSIXct("2020-07-01"), .(TRIP_ID, POOL, STRATA, SEL=OBSERVED=="Y")])[, .(N = .N, SEL=sum(SEL), RATE = sum(SEL)/.N), keyby=.(POOL, STRATA)]


# # The above rates do not split EM by EM_HAL and EM_POT
# # If we were to split EM_HAL and EM_POT so they have different realized rates...
# rr2 <- unique(actu_date[, .(TRIP_ID, POOL, STRATA, GEAR, SEL=OBSERVED=="Y")])
# rr2 <- unique(rr2[POOL=="EM", STRATA := paste("EM_", GEAR, sep="")][, c("POOL", "GEAR") := NULL])[,  .(N=.N, SEL=sum(SEL), RATE=sum(SEL)/.N), keyby=STRATA]
# rr2   # EM_POT had a rate of 36.36% whereas EM_HAL had a rate of 31.77%. It's possible that vessels that fish both gear types had a tendency to fish pot gear when monitored - this should be resolved in 2020, but for these analyses, the trip selection rates for EM were not split by gear type.

# Commenting this out for now - this is used for building distributions by randomly sampling trip selction with realized rates
# set.seed(12345)
# actu_odds <- unique(actu_date[, .(POOL, STRATA, TRIP_ID)])
# actu_odds[, RATE := rr[actu_odds, RATE, on=.(STRATA)]]                          # Merge in selection rates
# actu_odds <- rbindlist(rep(list(actu_odds), times=iter), idcol="ITER")
# actu_odds[, ODDS := runif(.N, min=0, max=1)]                                    # Randomly select trips
# actu_odds[, SEL := ODDS < RATE]
# actu_odds[, c("RATE", "ODDS") := NULL]

# TODO - Stop here, move down to ts_obze

#==================#
# ACQUIRED ODDS ####
acqu_odds <- unique(copy(actu_date)[, .(TRIP_ID, POOL, STRATA, SEL=OBSERVED)])
acqu_odds[, SEL := SEL=="Y"]  # Convert to boolean
acqu_odds[, ITER := 1]

pt(save(actu_date, actu_odds, acqu_odds, iter, pt, pr, rr, file=paste("AR_prep_", iter, ".Rdata", sep="") )) # pred won't be used 

#================================#
# QUICK LOAD FOR GAP ANALYSIS ####

pt(load("AR_prep_10000.Rdata"))     # 10,000 ODDS iterations of actual effort only 

#==========================#
# GAP ANALYSIS FUNCTION ####
gap_fun <- function(date, odds){
  # TEST #    date <- copy(actu_date); odds <- copy(actu_odds)   # for simualated ODDS
  # TEST #    date <- copy(actu_date); odds <- copy(acqu_odds)   # for acquired ODDS
  # TEST #    date <- copy(actu_date); odds <- copy(actu_odds)[ITER<=1000]   # for simualated ODDS but only 1k iterations

  # Performs gap analyses for discards (OB for OBZE and EM for EM) and biologicals (OB for EM)
  orgn <- "1970-01-01" # default origin for as.Date, needed to convert numeric dates back to date format 
  if(!("NEW_TRIP_ID" %in% names(odds))){odds[, NEW_TRIP_ID := TRIP_ID]}         # 'actual' effort doesn't have new_trip_id, so add it as a copy of trip_id
  overlap <- function(x){
    z <- sort(unique(unlist(as.vector(apply(x, 1, function(y) seq.int(y[1], y[2]))))))
    return(t(sapply(split(z, cumsum(c(TRUE, diff(z)!=1))), range)))
  }  # Workhorse function for compiling coverage date ranges 
  
  # Define post-strata for discards first (this will be modified for the biological gap analysis later)
  date[, P_STRATA := ifelse(POOL=="EM", paste("EM", GEAR, sep="_"), ifelse(STRATA %like% "TENDER", STRATA, GEAR))]
  # Later on, for the biological gaps, P_STRATA for POT_TENDER trips will be changed to POT instead of POT_TENDER
  
  full <- unique(date[odds[, -c("POOL", "STRATA")], on=.(TRIP_ID), allow.cartesian=TRUE])
  dmn_itr <- full[, .N, keyby=.(ITER, POOL, P_STRATA, TARGET, FMP)]                   # Get unit counts for all iterations/domains for discard post-strata

  odds_sel_range <- full[SEL==T, .(START, END), keyby=.(ITER, POOL, STRATA, P_STRATA, TARGET, FMP, AREA)]   # Subset trips selected for monitoring
  odds_sel_range[, GRP_ID_A := .GRP, by=.(ITER, POOL, STRATA, P_STRATA, TARGET, FMP, AREA)]  # Create unique group identifier for AREA
  odds_sel_range[, GRP_ID_F := .GRP, by=.(ITER, POOL, STRATA, P_STRATA, TARGET, FMP)]        # Create unique group identifier for FMP
  odds_sel_A_GRP <- unique(odds_sel_range[, .(ITER, POOL, STRATA, P_STRATA, TARGET, AREA), keyby=.(GRP_ID_A)])
  odds_sel_F_GRP <- unique(odds_sel_range[, .(ITER, POOL, STRATA, P_STRATA, TARGET, FMP), keyby=.(GRP_ID_F)])
  
  odds_sel_A <- as.matrix(odds_sel_range[, .(GRP_ID_A, A_S=as.integer(START)-15L, A_E = as.integer(END)+15L)])  # Convert to matrix of integers
  odds_sel_A <- split.data.frame(odds_sel_A[,-1], odds_sel_A[,1])  # Split each group by GRP_ID_A as a separate element in a list. Use split.data.frame to maintain dimensions (regular split will make vectors insted of matrices)
  
  # Compile AREA-level coverage
  message(paste(Sys.time(), ": Compiling AREA-level coverage..."), sep="")
  odds_sel_A <- lapply(odds_sel_A, overlap)        # Compile coverage of trips, finding date ranges in which unobserved trips can get AREA-level data
  odds_sel_A <- cbind(do.call(rbind, odds_sel_A), as.numeric(rep(names(odds_sel_A), lengths(odds_sel_A)/2)))        # Flatten the list and pull out GRP_ID_A out 
  odds_sel_A <- setnames(data.table(odds_sel_A), c("A_START", "A_END", "GRP_ID_A"))      # Convert back to data.table
  odds_sel_A[, A_GRP := seq_len(.N), by=.(GRP_ID_A)]                                     # Probably not necessary? Numbers each continuous period of coverage
  odds_sel_A[, ':=' (A_START = as.Date(A_START, origin=orgn), A_END = as.Date(A_END, origin=orgn))]
  setkey(odds_sel_A, GRP_ID_A)
  a_cover <- setkey(odds_sel_A_GRP[odds_sel_A], ITER, P_STRATA, TARGET, AREA)            # Merge post-strata columns back in

  # Compile FMP-level coverage
  message(paste(Sys.time(), ": Compiling FMP-level coverage..."), sep="")
  odds_sel_F <- copy(a_cover)[, FMP := ifelse(AREA > 600, "GOA", "BSAI")]  # put FMP back in
  odds_sel_F[, GRP_ID_F := odds_sel_F_GRP[odds_sel_F, GRP_ID_F, on=.(ITER, POOL, STRATA, P_STRATA, TARGET, FMP)]]  # Merge in GRP_ID_F
  odds_sel_F <- as.matrix(odds_sel_F[, .(GRP_ID_F, F_S = as.integer(A_START)-30L, F_E = as.integer(A_END)+30L)])  # Extending 30 days to area range to get total of 45 days from original dates
  odds_sel_F <- split.data.frame(odds_sel_F[,-1], odds_sel_F[,1])
  
  odds_sel_F <- lapply(odds_sel_F, overlap)  # Only 1.174 minutes
  odds_sel_F <- cbind(do.call(rbind, odds_sel_F), as.numeric(rep(names(odds_sel_F), lengths(odds_sel_F)/2)))        # Flatten the list and pull out GRP_ID_F out 
  odds_sel_F <- setnames(data.table(odds_sel_F), c("F_START", "F_END", "GRP_ID_F"))      # Convert back to data.table
  odds_sel_F[, F_GRP := seq_len(.N), by=.(GRP_ID_F)]                                     # Probably not necessary? Numbers each continuous period of coverage
  odds_sel_F[, ':=' (F_START = as.Date(F_START, origin=orgn), F_END = as.Date(F_END, origin=orgn))]
  setkey(odds_sel_F, GRP_ID_F)
  f_cover <- setkey(odds_sel_F_GRP[odds_sel_F], ITER, P_STRATA, TARGET, FMP)    # TODO - is making this even necessary? This might simplify things slightly,

  #   ggplot(a_cover[ITER==1 & TARGET=="I" & P_STRATA %like% "HAL" & AREA < 600], aes(x=AREA)) + geom_linerange(aes(ymin=A_START, ymax=A_END), size=2) + coord_flip() + facet_grid(P_STRATA~"")
  #   ggplot(f_cover[ITER==1 & TARGET=="I" & P_STRATA %like% "HAL"], aes(x=FMP)) + geom_linerange(aes(ymin=F_START, ymax=F_END), size=2) + coord_flip() + facet_grid(P_STRATA~"")
  
  #=======================#
  # DISCARDS GAP ANALYSIS #
  #=======================#
  
  ns_dis <- full[SEL==F, -c("GEAR")]                                            # Subset all non-selected trips
  setkey(ns_dis, ITER, P_STRATA, TARGET, AREA)                                  # Use P_STRATA, TARGET, and AREA to join covered trips to ns_dis                     
  
  # DO AREA-LEVEL CHECKS!
  message(paste(Sys.time(), ": Finding AREA-level discard gaps..."))
  ns_A <- a_cover[, -c("POOL", "STRATA")][ns_dis, allow.cartesian=TRUE]         # merge all coverage data at AREA level ~ 18M rows, not too bad!
  ns_A[, A_chk := (START >= A_START & START <= A_END) | (END >= A_START & START <= A_END)]  # NAs mean no trips were covered in area
  ns_A <- unique(ns_A[A_chk==TRUE, .(ITER, POOL, STRATA, P_STRATA, TARGET, FMP, AREA, TRIP_ID, NEW_TRIP_ID)]) # these trips could get data at AREA-level
  # Remove ns_A from ns_obze, leaving behind trips to search for data at FMP-level
  #f_trp_pre <- fsetdiff(unique(ns_dis[, .(ITER, POOL, STRATA, P_STRATA, TARGET, AREA, TRIP_ID, NEW_TRIP_ID)]), ns_A[, -"FMP"]) # these trips will search at FMP-level, but we need the dates from ns_obze again
  ns_F <- ns_dis[fsetdiff(unique(ns_dis[, .(ITER, POOL, STRATA, P_STRATA, TARGET, AREA, TRIP_ID, NEW_TRIP_ID)]), ns_A[, -"FMP"]), on=.(ITER, POOL, STRATA, P_STRATA, TARGET, AREA, TRIP_ID, NEW_TRIP_ID)] # get dates back, left joining f_trp_pre with ns_obze
  
  # DO FMP-LEVEL CHECKS
  message(paste(Sys.time(), ": Finding FMP-level gaps discard..."))
  setkey(ns_F, ITER, P_STRATA, TARGET, FMP)
  ns_F <- f_cover[, -c("POOL", "STRATA")][ns_F, allow.cartesian=TRUE] # left join f_trp with f_cover
  ns_F[, F_chk := (START >= F_START & START <= F_END) | (END >= F_START & START <= F_END)]  # NAs mean no trips were covered in area
  ns_F <- unique(ns_F[F_chk==TRUE, .(ITER, POOL, STRATA, P_STRATA, TARGET, FMP, AREA, TRIP_ID, NEW_TRIP_ID)]) # these trips could get data at FMP-level
  
  # Calculate totals
  message(paste(Sys.time(), ": Calculating discard gap totals..."))
  C_n <- full[SEL==T, .(COVER_n = .N), keyby=.(ITER, POOL, P_STRATA, TARGET, FMP)]     # Count number of observed units
  A_n <- ns_A[, .(AREA_n = .N), keyby=.(ITER, POOL, P_STRATA, TARGET, FMP)]
  F_n <- ns_F[, .(FMP_n = .N), keyby=.(ITER, POOL, P_STRATA, TARGET, FMP)]                            
  # TODO - If we wanted to weight the counts by weight of catch, this would be the place to do it!
  
  dis_n <- F_n[A_n[C_n[dmn_itr]]]          # do a bunch of left joins so nothing is lost along the way
  dis_n[is.na(COVER_n), COVER_n := 0]
  dis_n[is.na(AREA_n), AREA_n := 0]
  dis_n[is.na(FMP_n), FMP_n := 0]
  dis_n[, YTD_n := N - (COVER_n + AREA_n + FMP_n), keyby=.(ITER, POOL, P_STRATA, TARGET, FMP)] # calculate YTD as trips remaining
  
  message(paste(Sys.time(), ": Discards gap analysis complete!"))
  dis_out <- dis_n[, .(ITER, POOL, P_STRATA, TARGET, FMP, N, COVER_n, AREA_n, FMP_n, YTD_n)]  # THE FINAL RESULTS for OB/ZE gap analysis!!!!
  
  #======================#
  # AVG WGT GAP ANALYSIS #
  #======================#
  # This uses the ODDS results from the OB pool to see how representative the OB's biological data (average length/otoliths) of effort within the EM pools
  # Uses the same spatiotemporal definitions of the discards gap analysis
  # Assumes OB_HAL trips provide biological data for EM_HAL and both OB_POT and OB_TenP provide biological data for EM_POT
  # Modifies P_STRATA so that there is no distinction between OB-POT and OB_TenP
  
  message(paste(Sys.time(), ": Preparing the avg wgt gap checks..."))
  
  wgt_A_cover <- a_cover[POOL=="OB" & STRATA %in% c("POT", "TenP", "HAL")]     # Take coverage for fixed-gear OB trips only
  wgt_F_cover <- f_cover[POOL=="OB" & STRATA %in% c("POT", "TenP", "HAL")]     
  wgt_A_cover[P_STRATA == "TenP", P_STRATA := "POT"]                           # For this gap analysis, remove distinction between POT and TenP in P_STRATA
  wgt_F_cover[P_STRATA == "TenP", P_STRATA := "POT"]
  setkey(wgt_A_cover, ITER, P_STRATA, TARGET, AREA)
  setkey(wgt_F_cover, ITER, P_STRATA, TARGET, FMP)
  
  em_trps <- full[POOL=="EM", .(ITER, STRATA, P_STRATA, TARGET, FMP, AREA, START, END), keyby=.(TRIP_ID)]
  em_trps[, P_STRATA := gsub("EM_", "", P_STRATA)]   # Remove EM distinction from P_STRATA so that these trips can join with OB HAL/POT trips
  setkey(em_trps, ITER, P_STRATA, TARGET, AREA)
  
  # AREA-level AVG WGT CHECKS
  message(paste(Sys.time(), ": Finding AREA-level avg wgt gaps..."))
  wgt_A <- wgt_A_cover[, -c("POOL", "STRATA")][em_trps, allow.cartesian=TRUE]
  wgt_A[, A_chk := (START >= A_START & START <= A_END) | (END >= A_START & START <= A_END)] 
  wgt_A <- unique(wgt_A[A_chk==TRUE, .(ITER, STRATA, P_STRATA, TARGET, FMP, AREA, TRIP_ID)]) # these trips could get bio data at AREA-level
  
  # Remove AREA_level trips from obem_base
  wgt_F <- fsetdiff(em_trps[, .(ITER, STRATA, P_STRATA, TARGET, FMP, AREA, TRIP_ID)], wgt_A) # these trips will search at FMP-level, but we need the dates from obem_base again
  setkey(wgt_F, ITER, STRATA, P_STRATA, TARGET, FMP, AREA, TRIP_ID)
  setkey(em_trps, ITER, STRATA, P_STRATA, TARGET, FMP, AREA, TRIP_ID)
  wgt_F <- em_trps[wgt_F]
  
  # FMP-level AVG WGT CHECKS
  message(paste(Sys.time(), ": Finding FMP-level avg wgt gaps..."))
  setkey(wgt_F, ITER, P_STRATA, TARGET, FMP)
  wgt_F <- wgt_F_cover[, -c("POOL", "STRATA")][wgt_F, allow.cartesian=TRUE]     # left join f_trp with f_cover
  wgt_F[, F_chk := (START >= F_START & START <= F_END) | (END >= F_START & START <= F_END)] 
  wgt_F <- unique(wgt_F[F_chk==TRUE, .(ITER, STRATA, P_STRATA, TARGET, FMP, AREA, TRIP_ID)]) # these trips could get bio data at AREA-level
  
  # Calculate totals
  message(paste(Sys.time(), ": Calculating avg wgt totals..."))
  wgt_A_n <- wgt_A[, .(AREA_n = .N), keyby=.(ITER, P_STRATA, TARGET, FMP)]
  wgt_F_n <- wgt_F[, .(FMP_n = .N), keyby=.(ITER, P_STRATA, TARGET, FMP)]
  
  wgt_dmn_itr <- dmn_itr[P_STRATA %like% "EM"][, P_STRATA := gsub("EM_", "", P_STRATA)]
  setkey(wgt_dmn_itr, ITER, P_STRATA, TARGET, FMP)
  wgt_n <- wgt_F_n[wgt_A_n[wgt_dmn_itr]] # do a bunch of left joins so nothing is lost along the way
  wgt_n[is.na(AREA_n), AREA_n := 0]
  wgt_n[is.na(FMP_n), FMP_n := 0]
  wgt_n[, YTD_n := N - (AREA_n + FMP_n), keyby=.(ITER, P_STRATA, TARGET, FMP)] # calculate YTD as trips remaining
  wgt_n[, POOL := "EM"]
  
  message(paste(Sys.time(), ": Avg wgt gap analysis complete!"))
  wgt_out <- wgt_n[, .(ITER, POOL, P_STRATA, TARGET, FMP, N, AREA_n, FMP_n, YTD_n)]  # THE FINAL EM ODDS RESULTS!!!!

  # TRIP COUNTS #
  # Count distinct trips by domain for both discards and average weight gaps (P_STRATA, TARGET, and FMP), opposed to target X area units
  trip_n <- unique(full[, .(ITER, POOL, P_STRATA, TARGET, FMP, TRIP_ID, NEW_TRIP_ID)])[, .N, keyby=.(ITER, POOL, P_STRATA, TARGET, FMP)][, .(N=round(mean(N),1)), keyby=.(POOL, P_STRATA, TARGET, FMP)]
  trip_n[, GAP := ifelse(POOL=="EM", "EM", "OBZE")]
  
  dis_trip_n <- dcast(trip_n, GAP + P_STRATA + TARGET + FMP ~ POOL, value.var="N")
  dis_trip_n[, TYPE := "Discards"]
  dis_trip_n[GAP=="OBZE" & is.na(ZE), ZE := 0]    # FOR OBZE gaps, change NAs to 0 for ZE pool domains without any trips
  
  bio_trip_n <- trip_n[POOL %in% c("EM", "OB") & (P_STRATA %like% "HAL" | P_STRATA %like% "POT")] # Do the same for bio, but don't post-stratify by tender
  bio_trip_n[P_STRATA %like% "EM", P_STRATA := gsub("EM_", "", P_STRATA)]
  bio_trip_n[P_STRATA=="POT_TENDER", P_STRATA:="POT"]
  bio_trip_n <- bio_trip_n[, .(N=sum(N)), keyby=.(POOL, P_STRATA, TARGET, FMP)]
  bio_trip_n[, GAP := "OBEM"]
  bio_trip_n <- dcast(bio_trip_n, GAP + P_STRATA + TARGET + FMP ~ POOL, value.var="N")
  bio_trip_n[, TYPE := "Avg Wgt"]
  bio_trip_n[, ZE := NA]   # Add in "ZE pool column to match dis_trip_n, but set as 'NA'
  bio_trip_n[is.na(EM), EM := 0]   # For OBEM gaps, change NAs to 0 for EM pool domains without any trips
  
  trip_n <- rbind(dis_trip_n, bio_trip_n)[, .(TYPE, GAP, P_STRATA, TARGET, FMP, OB, EM, ZE)]
  
  # FINAL PACKAGING OF RESULTS #

  dis_out[, ':=' (GAP = ifelse(P_STRATA %like% "EM", "EM", "OBZE"), TYPE = "Discards")]
  dis_out[GAP=="EM", P_STRATA := gsub("EM_", "", P_STRATA)]                     # Now that 'gap' and type has been specified, make P_STRATA of EM trips gear-specific for the sake of plotting
  wgt_out[, ':=' (GAP = "OBEM", TYPE = "Avg Wgt")]
  gap_out <- rbind(dis_out, wgt_out, fill=TRUE)
  gap_out[, GAP := factor(GAP, levels=c("OBZE", "EM", "OBEM"))]
  gap_out[, TYPE := factor(TYPE, levels=c("Discards", "Avg Wgt"))]
  
  return(list(GAPS=gap_out, TRIP_N = trip_n))
}

gc()
acqu_gaps <- gap_fun(actu_date, acqu_odds)  
pt(actu_gaps <- gap_fun(actu_date, actu_odds))                                  # Took 14.522 minutes. 

save(acqu_odds, acqu_gaps, actu_date, actu_gaps, acqu_gaps, pr, rr, pt, file=paste("AR_gaps_", iter, ".Rdata", sep=""))  # omitting actu_odds because it is a large file and we don't need after gap analysis

#===============#
# FISH PLOTS ####
# Without predicted fishing effort this time around

pt(load(file="AR_gaps_10000.Rdata"))

wgts <- c(1, 0.75, 0.25, 0); ycutoff <- 0.001; diff_cut <- 0.01; 
n_min <- 5;   # Change this from 5 to 0 if you don't want any domains with low effort to be dropped

trip_n <- actu_gaps$TRIP_N[P_STRATA!="JIG"]   # Keep trip counts (should be the same between actu_gaps and acqu_gaps, but would be different with predicted effort)
actu_gaps <- actu_gaps$GAPS[P_STRATA!="JIG"]  # Gap scores from simulation
acqu_gaps <- acqu_gaps$GAPS[P_STRATA!="JIG"]  # Acquired gap scores
setkey(actu_gaps, P_STRATA, TARGET, FMP)
setkey(acqu_gaps, P_STRATA, TARGET, FMP)

# Count number of trip units within each POOL and domain 
domaS <- unique(actu_gaps[ITER==1, .(N_TOT=sum(N)), keyby=.(P_STRATA, TARGET, FMP)])[N_TOT >= n_min]

actu_gaps <- actu_gaps[domaS[, -"N_TOT"]]                                       # Use domaS to subset the gap data, excluding domains when domains have < n_min (5) trips 
actu_gaps[is.na(COVER_n), COVER_n := 0]                                         # Within OBEM (average weights), COVER_n is always 0
actu_gaps[
  ][, ':=' (C_p = COVER_n/N, A_p = AREA_n/N, F_p = FMP_n/N, Y_p = YTD_n/N)      # Calculate proportions for each odds iteration
    ][, ':=' (GAP_s = C_p*wgts[1] + A_p*wgts[2] + F_p*wgts[3] + Y_p*wgts[4])    # Calculate gap score (these weightings can be played with using the 'wgts' argument)
      ][, ':=' (GRP = .GRP), by=.(TYPE, POOL, P_STRATA, TARGET, FMP)]            # Assign unique grouping number
setkey(actu_gaps, GRP)

# Use the same 'GRP' ids for the acquired dataset
grps <- unique(actu_gaps[, .(GRP, GAP, TYPE, POOL, P_STRATA, TARGET, FMP)])   # Creating a unique grouping object so that acquired gaps will have same grp number as actual
acqu_gaps <- acqu_gaps[domaS]
acqu_gaps[is.na(COVER_n), COVER_n := 0]                                         # Within OBEM (average weights), COVER_n is always 0
acqu_gaps[
  ][, ':=' (C_p = COVER_n/N, A_p = AREA_n/N, F_p = FMP_n/N, Y_p = YTD_n/N)    # Calculate proportions for each odds iteration
    ][, ':=' (GAP_s = C_p*wgts[1] + A_p*wgts[2] + F_p*wgts[3] + Y_p*wgts[4])]  # Calculate gap score (these weightings can be played with using the 'wgts' argument)
acqu_gaps[, GRP := grps[acqu_gaps, GRP, on=.(GAP, TYPE, POOL,  P_STRATA, TARGET, FMP)]]  # Merge in group numbers from 'actual' scenario
setkey(acqu_gaps, GRP)  # All I really need is the acquired gap scores (GAP_s)

# How likely were the acquired scores, given the simulation from the actual effort? Use proportion of outcomes that were LESS extreme.
acqu_tbl <- acqu_gaps[, .(GRP, TYPE, GAP, POOL, P_STRATA, TARGET, FMP, N, GAP_s)]

for(i in unique(acqu_gaps$GRP)){
  # i <- 1
  a <- acqu_tbl[GRP==i]    # Subset of acquired gap scores
  b <- actu_gaps[GRP==i]   # Subset of gaps scores from ODDS simulations
  
  acqu_tbl[GRP==i, MED := median(b$GAP_s)]
  acqu_tbl[GRP==i, c("MIN", "MAX") := as.list(range(b$GAP_s))]
  
  if(a$GAP_s == median(b$GAP_s)) acqu_tbl [GRP==i, QUANT := 0] 
  else(
    if(a$GAP_s > median(b$GAP_s)) acqu_tbl [GRP==i, QUANT := b[GAP_s < a$GAP_s, .N]/nrow(b)-0.5] 
    else acqu_tbl [GRP==i, QUANT := b[GAP_s > a$GAP_s, .N]/nrow(b)-0.5] 
  )
}
acqu_tbl[, FLAG := ifelse(GAP_s < MIN | GAP_s > MAX, T, F)]
acqu_tbl[, DIR := sign(GAP_s-MED)]    # Direction relative to median gap score of simulation
acqu_tbl[FLAG==T]        # Cases where the acquired gap scores were outside the simulated distributions (i.e., non-random sampling likely occured!)
acqu_tbl[order(-QUANT)] # The greater QUANT is (closer to 0.5), the more extreme and unlikely the result was
acqu_tbl[QUANT>=0.4][order(GAP, POOL, P_STRATA, QUANT)]    # Since using version 9 of 2_AR_data, versus version 5, there are way fewer gaps > 0.4 (8 now instead of 13) - Because of EM_POT?

# Make the data for the plots
acqu_sub <- actu_gaps[acqu_tbl[, .(GAP, POOL, P_STRATA, TARGET, FMP)], on=.(GAP, POOL, P_STRATA, TARGET, FMP)]  # Post-strata where we acquired data. Use this to exclude post-strata where we have predicted effort but no actual effort
out <- vector(mode="list", length=uniqueN(acqu_sub$GRP))
med_lst <- vector(mode="list", length=uniqueN(acqu_sub$GRP))
for(i in unique(acqu_sub$GRP)){
  # i <- 21
  
  # TODO - In the future, perhaps use the same bins for both pred/actual datasets. Can use hist() on merged gap scores to get initial bins
  
  b <- acqu_sub[GRP==i, GAP_s]                # Subset the simulated gap scores
  h <- hist(b, breaks=16, plot=FALSE)         # Generate histogram to be modified for ggplot2
  
  # If there are few bins and the data range is very small, manually set the number and size of bins
  if(diff(range(b)) <= 0.25 & uniqueN(round(b,3)) <= 3){
    int <- 1/24    # Number of breaks will be the denom+2, resulting in denom+1 bins, so use an even denom to get odd number bins. 
    # Denom of 24 results in 26 bins, meaning 0 and 1 each have a bin, and 23 bins in between, including bins centered on 0.25, 0.5, and 0.75
    new_breaks <- seq(-int/2, 1+(int/2), by=int)
  }
  else{
    int <- mean(diff(h$breaks))  # interval from histogram
    new_breaks <- seq(min(h$breaks)-int/2, max(h$breaks)+int/2, by=int) # modify breaks of original histogram, and add one bin
  }
  
  dat <- data.table(X=b)
  dat[, CUT := cut(X, new_breaks, include.lowest = TRUE)]                       # Assign bins to each gap score
  dat <- data.table(table(dat$CUT), keep.rownames = TRUE) # Use table instead of dat[, .N, by=CUT], or else cuts with 0 counts are dropped!
  dat <- dat[1:max(which(dat$N>0))]  # Cut off any trailing bins that only have zeroes (i.e., especially when only scores are 0 and 0.25, etc.)
  dat[, Y := N/max(N)*0.45]      # Scale all values relative to a maximum of 0.45 (so that axes limits can be set to c(-0.5, 0.5))
  e <- dat[, .(X = as.numeric(tstrsplit(gsub("\\[|\\]|\\(", "", V1), ","))), by=.(V1, Y)]    # Convert character name of bin back range of two numeric values
  g <- dat[, setNames(tstrsplit(gsub("\\[|\\]|\\(", "", V1), ","), c("lo", "hi")), by=.(V1, Y)]  # Make a separate table that can be used to get y_value of median 
  g[, mid := (as.numeric(lo)+as.numeric(hi))/2]; setkey(g, mid)       # Calculate mid of each range which will be used for nearest neighbor join to acquired gap score
  med_y <- g[J(median(b)), roll="nearest"]$Y     # Find y-value of distribution at the median. J() is a cool trick to join a table with a single value, especially for "nearest" rolling joins
  e <- rbind(e[1], e, e[.N])   # Repeat first and last rows
  e[c(1, .N), Y := 0]          # Set y-value of first and last rows to zero
  #   ggplot(e, aes(x=X, y=Y)) + geom_polygon(fill="steelblue2", alpha=0.5, color="black") + geom_linerange(x=median(b), ymin=0, ymax=med_y)
  
  out[[i]] <- e
  med_lst[[i]] <- data.table(MED = median(b), MED_Y = med_y)
}

out <- rbindlist(out, idcol="GRP")
med_lst <- rbindlist(med_lst, idcol="GRP")

grps_scen <- unique(acqu_sub[, .(GRP, TYPE, GAP, POOL, P_STRATA, TARGET, FMP)])        # Get post-strata columns for each GRP
grps_scen[, PLT_GRP := ifelse(P_STRATA %like% "TENDER", "TEN", P_STRATA)]               # Create plotting groups
target_tbl <- data.table(
  TARGET = c("A", "C", "D", "E", "F", "H", "I", "K", "L", "M", "O", "P", "R", "S", "T", "W", "X", "Y"),
  TARGET_lbl = c("Atka Mackerel", "Pacific Cod", "Deep Water Flatfish", "Alaska Plaice", "Other Flatfish", "Shallow Water Flatfish", "Halibut", "Rockfish", "Flathead Sole", "Kamchatka Flounder", "Other Species", "Pollock", "Rock Sole", "Sablefish", "Greenland Turbot", "Arrowtooth Flounder", "Rex Sole", "Yellowfin Sole"))
grps_scen[, TARGET_lbl := target_tbl[grps_scen, TARGET_lbl, on=.(TARGET)]]                      # Merge in target names

out <- grps_scen[out, on=.(GRP)]                                                          # Merge them back in to the data
med_lst <- grps_scen[med_lst, on=.(GRP)]                                                     # Do the same for the median list
acqu_gaps <- acqu_gaps[, .(GRP, GAP_s)][grps_scen, on=.(GRP)]                                    # Do the same for the acquired scores

# RENAME GAP TYPE (instead of using 'discards' and 'avg wgt', make this more general by using the names of the pools used for data versus pools receiving data)
out[TYPE=="Discards" & POOL=="OB", TYPE2 := "OB-OB"]
out[TYPE=="Discards" & POOL=="ZE", TYPE2 := "OB-ZE"]
out[TYPE=="Discards" & POOL=="EM", TYPE2 := "EM-EM"]
out[TYPE=="Avg Wgt"  & POOL=="EM", TYPE2 := "OB-EM"]
out[, TYPE2 := factor(TYPE2, levels=c("OB-OB", "OB-ZE", "EM-EM", "OB-EM"))]

med_lst[TYPE=="Discards" & POOL=="OB", TYPE2 := "OB-OB"]
med_lst[TYPE=="Discards" & POOL=="ZE", TYPE2 := "OB-ZE"]
med_lst[TYPE=="Discards" & POOL=="EM", TYPE2 := "EM-EM"]
med_lst[TYPE=="Avg Wgt"  & POOL=="EM", TYPE2 := "OB-EM"]
med_lst[, TYPE2 := factor(TYPE2, levels=c("OB-OB", "OB-ZE", "EM-EM", "OB-EM"))]

acqu_gaps[TYPE=="Discards" & POOL=="OB", TYPE2 := "OB-OB"]
acqu_gaps[TYPE=="Discards" & POOL=="ZE", TYPE2 := "OB-ZE"]
acqu_gaps[TYPE=="Discards" & POOL=="EM", TYPE2 := "EM-EM"]
acqu_gaps[TYPE=="Avg Wgt"  & POOL=="EM", TYPE2 := "OB-EM"]
acqu_gaps[, TYPE2 := factor(TYPE2, levels=c("OB-OB", "OB-ZE", "EM-EM", "OB-EM"))]

# Generate the plots
plt_lst <- vector(mode="list", length=uniqueN(grps_scen$PLT_GRP))
for(j in unique(grps_scen$PLT_GRP)){
  ## j <- "HAL"
  ## J <- "TEN"
  
  plt_gap <- copy(out[PLT_GRP==j])[, ':=' (POOL = factor(POOL, levels=c("OB", "ZE", "EM")), FMP = factor(FMP, levels=c("BSAI", "GOA")))]
  plt_med <- copy(med_lst[PLT_GRP==j])[, ':=' (POOL = factor(POOL, levels=c("OB", "ZE", "EM")), FMP = factor(FMP, levels=c("BSAI", "GOA")))]
  plt_act <- copy(acqu_gaps[PLT_GRP==j])[, ':=' (POOL = factor(POOL, levels=c("OB", "ZE", "EM")), FMP = factor(FMP, levels=c("BSAI", "GOA")))]
  
  if(j=="TEN") {
    #plt_lst[[j]] 
    plt_ten <- vector(mode="list")
    for(k in unique(plt_gap$P_STRATA)[order(unique(plt_gap$P_STRATA))]){
      # k <- "POT_TENDER"
      plt_gap_sub <- plt_gap[P_STRATA==k]
      plt_med_sub <- plt_med[P_STRATA==k]
      plt_act_sub <- plt_act[P_STRATA==k]
      
      plt_ten[[k]] <- ggplot(plt_gap_sub, aes(x=X, y=Y)) + 
        facet_grid(TYPE2 ~ FMP + P_STRATA + TARGET_lbl, scales="free_x") +                          
        #facet_wrap(~GAP + P_STRATA + TARGET + FMP, ncol=6, scales="free") +           # Free scales
        geom_polygon(data=plt_gap_sub, alpha=0.5, color="steelblue2", fill="steelblue2") + 
        geom_linerange(data=plt_med_sub, inherit.aes=FALSE, aes(x=MED, ymin=0, ymax=MED_Y), color="blue") +
        geom_vline(data=plt_act_sub, aes(xintercept=GAP_s), linetype=2, color="black") + 
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), strip.text=element_text(margin=margin(0.05, 0.05, 0.05, 0.05,"cm")), legend.position="bottom", axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
        labs(x="Gap Score", y="Relative Frequency", fill="Fishing Effort", color="Fishing Effort") +
        scale_y_continuous(limits=c(-0.05, 0.45), breaks=0) 
    }
    plt_lst[[j]] <- ggarrange(plotlist=plt_list, nrow=1)
    
  }
    
  if(j!="TEN"){
    plt_lst[[j]] <-  ggplot(plt_gap, aes(x=X, y=Y)) + 
      facet_grid(TYPE2 ~ FMP + P_STRATA + TARGET_lbl, scales="free_x") +                          
      #facet_wrap(~GAP + P_STRATA + TARGET + FMP, ncol=6, scales="free") +           # Free scales
      geom_polygon(data=plt_gap, alpha=0.5, color="steelblue2", fill="steelblue2") + 
      geom_linerange(data=plt_med, inherit.aes=FALSE, aes(x=MED, ymin=0, ymax=MED_Y), color="blue") +
      geom_vline(data=plt_act, aes(xintercept=GAP_s), linetype=2, color="black") + 
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), strip.text=element_text(margin=margin(0.05, 0.05, 0.05, 0.05,"cm")), legend.position="bottom", axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      labs(x="Gap Score", y="Relative Frequency", fill="Fishing Effort", color="Fishing Effort") +
      scale_y_continuous(limits=c(-0.05, 0.45), breaks=0) 
  }

}

# Save fish plots to Figures folder
ggsave("GapFigures/gap_plt_ALL.pdf", plot=ggarrange(plt_lst$HAL, plt_lst$POT, plt_lst$TRW, plt_lst$TEN, ncol=1, heights=c(1/3, 1/3, 1/6, 1/6)), device="pdf", width=8.5, height=11, units="in")
ggsave("GapFigures/gap_plt_HAL_TRW.pdf", plot=ggarrange(plt_lst$HAL, plt_lst$TRW, ncol=1, heights=c(2/3, 1/3)), device="pdf", width=11, height=8.5, units="in")
ggsave("GapFigures/gap_plt_POT_TEN.pdf", plot=ggarrange(plt_lst$POT, plt_lst$TEN, ncol=1, heights=c(2/3, 1/3)), device="pdf", width=11, height=8.5, units="in")

# For PPT, export with 1200x800 dimensions
ggarrange(plt_lst$HAL, plt_lst$POT, plt_lst$TRW, plt_lst$TEN, ncol=1, heights=c(1/3, 1/3, 1/6, 1/6)) 
ggarrange(plt_lst$HAL, plt_lst$TRW, ncol=1, heights=c(2/3, 1/3))             
ggarrange(plt_lst$POT, plt_lst$TEN, ncol=1, heights=c(2/3, 1/3))             

# Images for PPT (1200x800) and Word (1000 x 600)
plt_lst$HAL 
plt_lst$TRW
ggarrange(plt_lst$HAL, plt_lst$TRW, ncol=1, heights=c(2/3, 1/3))             
ggarrange(plt_lst$POT, plt_lst$TEN, ncol=1, heights=c(2/3, 1/3))             

# Example gap plot for PPT
a <- data.table(TYPE="Discards", GAP="OBZE", P_STRATA="HAL", TARGET="I", FMP="BSAI")
plt_gap <- copy(out[a, on=.(TYPE, GAP, P_STRATA, TARGET, FMP)])[, ':=' (POOL = factor(POOL, levels=c("OB", "ZE", "EM")), FMP = factor(FMP, levels=c("BSAI", "GOA")))]
plt_med <- copy(med_lst[a, on=.(TYPE, GAP, P_STRATA, TARGET, FMP)])[, ':=' (POOL = factor(POOL, levels=c("OB", "ZE", "EM")), FMP = factor(FMP, levels=c("BSAI", "GOA")))]
plt_act <- copy(acqu_gaps[a, on=.(TYPE, GAP, P_STRATA, TARGET, FMP)])[, ':=' (POOL = factor(POOL, levels=c("OB", "ZE", "EM")), FMP = factor(FMP, levels=c("BSAI", "GOA")))]
ggplot(plt_gap, aes(x=X, y=Y)) + 
  facet_grid(TYPE + POOL ~ P_STRATA + TARGET_lbl + FMP, scales="free_x") +                          
  #facet_wrap(~GAP + P_STRATA + TARGET + FMP, ncol=6, scales="free") +           # Free scales
  geom_polygon(data=plt_gap, alpha=0.5, color="steelblue2", fill="steelblue2") + 
  geom_linerange(data=plt_med, inherit.aes=FALSE, aes(x=MED, ymin=0, ymax=MED_Y), color="blue") +
  geom_vline(data=plt_act, aes(xintercept=GAP_s), linetype=2, color="black") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), strip.text=element_text(margin=margin(0.05, 0.05, 0.05, 0.05,"cm")), legend.position="bottom", axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  labs(x="Gap Score", y="Frequency", fill="Fishing Effort", color="Fishing Effort") +
  scale_y_continuous(limits=c(-0.05, 0.45), breaks=0)

#==========#
# TABLE ####
# Summary of medians, range of actual scores in prediction, and comparison to acquired

acqu_mon <- unique(actu_date[, -("AREA")])[acqu_odds[SEL==T, .(TRIP_ID)], on=.(TRIP_ID)]
dis_acqu_mon <- copy(acqu_mon)[, P_STRATA := ifelse(POOL=="EM", GEAR, STRATA)][, .(MON=.N), keyby=.(POOL, P_STRATA, TARGET, FMP)][, ':=' (TYPE="Discards", GAP=ifelse(POOL=="EM", "EM", "OBZE"))]
bio_acqu_mon <- copy(acqu_mon)[POOL=="OB" & GEAR %in% c("HAL", "POT")][, P_STRATA := GEAR][, .(MON=.N), keyby=.(POOL, P_STRATA, TARGET, FMP)][, ':=' (TYPE="Avg Wgt", GAP="OBEM")]
acqu_mon <- rbind(dis_acqu_mon, bio_acqu_mon)

trip_n[P_STRATA %like% "EM", P_STRATA := gsub("EM_", "", P_STRATA)]
trip_n[, TARGET_lbl := target_tbl[trip_n, TARGET_lbl, on=.(TARGET)]]
trip_n[, MON := acqu_mon[trip_n, MON, on=.(TYPE, GAP, P_STRATA, TARGET, FMP)]]
trip_n[, PERC := 100*MON / ifelse(GAP %like% "OB", OB, EM)]
summary_tbl <- trip_n[acqu_tbl, on=.(TYPE, GAP, P_STRATA, TARGET, FMP)][, .(N, Mon=MON, Perc=round(PERC,2), OB, EM, ZE, Acquired = round(GAP_s,3), Min=round(MIN,3), Med=round(MED,3), Max=round(MAX,3), Quant=QUANT*DIR, FLAG), keyby=.(Type=TYPE, Pool=POOL, Post_Strata=P_STRATA, Target=TARGET_lbl, FMP)]  # Actual trips counts are given by OB, EM, and ZE, and 'N' is the total count of AREA units
summary_tbl[is.na(Mon), Mon := 0]
summary_tbl[is.na(Perc), Perc := 0]
summary_tbl

test <- copy(acqu_tbl) 
test[, QUANT := 0.5-QUANT]
test[P_STRATA=="HAL" & POOL=="OB" & TYPE=="Discards"]  # Herem quant = Proportion of scores that are more extreme. The closer to 0 that QUANT is, the more unlikely the acquired result is.
test[order(QUANT)]

#=======================================#
# ACTUAL FISHING EFFORT AND COVERAGE ####
#=======================================#

# These functions are useful for explaining how coverage differed in time and space, and may highlight instances where monitoring coverage was avoided

#   Save an abbreviated version of work.data for partial coverage
#pt(load(file="2_AR_data.Rdata")); setDT(work.data)
#work_data <- unique(work.data[COVERAGE_TYPE=="PARTIAL", .(TRIP_ID, VESSEL_ID, OBSERVED_FLAG, PORT_CODE, AGENCY_GEAR_CODE,  TENDER, FMP, AREA=REPORTING_AREA_CODE, SPECIES_CODE=AGENCY_SPECIES_CODE, SPECIES_GROUP=SPECIES_GROUP_CODE, WEIGHT_POSTED, SPECIES_COUNT, MANAGEMENT_PROGRAM_CODE, SECTOR=PROCESSING_SECTOR )])
#save(work_data, file="work_data_DT.Rdata")

# kept original work.data object, which is also already with data.table class
# load(file="work_data_DT.Rdata")   # work_data is a slightly summarized version of work.data - must faster than loading 2_ADP all over again
# Needed to merge vessel_ID back in so that areas with <3 distinct vessels can be obscured
work_data <- copy(work.data)

# Version that makes non-confidential version for analysts and confidential version for public
ts_obze <- function(a, legend=TRUE, conf=TRUE, n_min=5, ar_year=2020, ob_extend = 0){
  # These plots summarize OB pool coverage to OB, and ZE pools by GEAR type. 
 
  # TESTING:    a <- actu_date[GEAR=="HAL"]; legend <- T; conf <- F; n_min=5; ar_year=2020; ob_extend <- 0
  # TESTING:    a <- actu_date[GEAR=="TRW"]; legend <- T; conf <- T; n_min=5; ar_year=2020; ob_extend <- 0
  
  a <- a[POOL!="EM"]   # Remove any EM trips
  a[, AREA := as.factor(AREA)] # Convert AREA to factor
  
  # Coverts all trip date ranges to week bin counts
  # TODO Make yr = adp_year
  to_week <- function(x, flip=FALSE, yr=ar_year){
    #  x <- a[POOL=="OB" & STRATA=="HAL" & TARGET=="I" & AREA==519]; flip <- FALSE; yr <- 2019
    #  x <- copy(b)[[1]]
    x[year(START) < yr, START := as.Date(paste0(yr, "-01-01"))]  # If any incoming date ranges are outside of the year, truncate them to be within specified year
    x[year(END) > yr, END := as.Date(paste0(yr, "-12-31"))]
    cols <- unique(x[, c("POOL", "STRATA", "TARGET_lbl", "FMP", "AREA")])
    x <- setnames(x[, apply(.SD, MARGIN=1, function(y) seq.int(week(y[1]), week(y[2]))), by=.(TRIP_ID, AREA), .SDcols=c("START", "END")], c("TRIP_ID", "AREA", "WEEK"))
    x <- x[, .N, keyby=WEEK][SJ(1:week(as.Date(paste0(yr, "-12-31"))))][is.na(N), N := 0][, WEEK := as.numeric(WEEK)]
    x <- cbind(x, cols)  # Put identifier columns back in
    return(x)
  }
  
  target_tbl <- data.table(
    TARGET = c("A", "C", "D", "E", "F", "H", "I", "K", "L", "M", "O", "P", "R", "S", "T", "W", "X", "Y"),
    TARGET_lbl = c("Atka Mackerel", "Pacific Cod", "Deep Water Flatfish", "Alaska Plaice", "Other Flatfish", "Shallow Water Flatfish", "Halibut", "Rockfish", "Flathead Sole", "Kamchatka Flounder", "Other Species", "Pollock", "Rock Sole", "Sablefish", "Greenland Turbot", "Arrowtooth Flounder", "Rex Sole", "Yellowfin Sole"))
  a[, TARGET_lbl := target_tbl[a, TARGET_lbl, on=.(TARGET)]]                     # Merge in target lbl
  a[POOL =="ZE", STRATA := GEAR]    # Define STRATA by gear type for ZE pool to match OB pool
  a <- a[STRATA %like% unique(a$GEAR)]   # Make sure STRATA is like GEAR! (e.g., if POT trips are being looked at but STRATA was HAL, remove those instances!)
  # Like the gap analysis, remove domains with < 5 trips
  a_keep <- a[, .N, keyby=.(STRATA, TARGET, FMP)][N>=n_min]  # Filter out domains with < 3 trips
  a <- a[a_keep, on=.(STRATA, TARGET, FMP)]
  pools <- unique(a$POOL) # Get vector of unique pools - we will use this to decide if we need to copy the monitoring data for other pools
  d <- unique(a[, .(STRATA, TARGET_lbl), by=FMP])[, .N, by=FMP]
  
  if(conf==TRUE){
    # Filter out trips here to maintain confidentiality
    # First, remove any domains with <5 trips. Then If any areas have <3 distinct vessels, remove trips but add a gray label that instead gives percentage of trips monitored
    a[, VESSEL_ID := unique(work_data[, .(TRIP_ID, VESSEL_ID)])[a, VESSEL_ID, on=.(TRIP_ID)]]  # Merge in vessel ID's
    a_remv <- a[, .(VES_N = uniqueN(VESSEL_ID), TRP_N=uniqueN(TRIP_ID), P_OBSERVED=ifelse(POOL=="OB", sum(OBSERVED=="Y")/.N, NA_real_)), keyby=.(POOL, STRATA, TARGET_lbl, FMP, AREA)][VES_N<3]  # these areas will be replaced with gray bar
    a_keep <- a[, .(VES_N = uniqueN(VESSEL_ID), TRP_N=uniqueN(TRIP_ID)), keyby=.(POOL, STRATA, TARGET_lbl, FMP, AREA)][VES_N>=3]
    a <- a[a_keep, on=.(POOL, STRATA, TARGET_lbl, FMP, AREA)]
    
    # Prepare polygons for confidential data
    rect_test <- copy(a_remv)
    rect_test <- rbindlist(rep(list(rect_test), times=5))
    rect_test[, INDEX := seq_len(.N), keyby=.(POOL, STRATA, TARGET_lbl, FMP, AREA)]
    rect_test[, WEEK := ifelse(INDEX %in% c(1,4,5), 1, 54)]
    rect_test[, Y_ADJ := ifelse(INDEX %in% c(1,2,5), -1/8, 3/8)]   # or use 0, 3/8 to prevent any overlap
    rect_test[, COLOR := I("red")]
    rect_o <- copy(rect_test[!is.na(P_OBSERVED) & P_OBSERVED>0])[, ':=' (Y_ADJ = -Y_ADJ, COLOR=I("blue"))]
    rect_test <- rbind(rect_test, rect_o)
    if("ZE" %in% pools) rect_test <- rbind(rect_test, copy(rect_o)[, ':=' (POOL="ZE", P_OBSERVED = NA_real_)]) # changed P_OBSERVED from NA_real_ to 0 to fix plotting error  
    rect_test[, POLY_ID := .GRP, keyby=.(POOL, STRATA, TARGET_lbl, FMP, AREA, COLOR)]
  }
  
  # HERE, observed trips are copied and have the trip start/end date extended by 15 days on each side (Area level)
  # MAde it so function default of ob_extend is 0, but if you make this 15, you can extend the range of observed trips
  b <- copy(a[POOL=="OB" & OBSERVED=="Y"])[, ':=' (START=START-ob_extend, END=END+ob_extend)] # Observed OB trips only - extend date ranges by 30 days to represent data coverage at AREA level
  
  a <- split(a, f=list(a$FMP, a$POOL, a$STRATA, a$TARGET, a$AREA), drop=TRUE)   # Split data by pool, post-strata, and AREA
  b <- split(b, f=list(b$FMP, b$POOL, b$STRATA, b$TARGET, b$AREA), drop=TRUE)   # Split monitored data by pool, post-strata, and AREA
  a <- rbindlist(lapply(a, to_week))  # Convert to weekly bin counts

  b <- rbindlist(lapply(b, to_week))
  c <- rbind(a[, ':=' (GRP="Effort")], b[, ':=' (GRP="Area-Level Data")])
  c[, AREA := as.factor(AREA)]
  c[, N := as.numeric(N)]  # convert from integer to numeric
  c[, N := N/max(N), by=.(POOL, STRATA, TARGET_lbl, FMP)]                  # TODO REMOVED GRP to see what it'd look like # Scale the counts by each pool/strata/target/fmp (i.e., each facet of the final plot)                                         # Create subset of area-level data to be pasted to other pools later
  
  c[, FMP := factor(FMP, levels=c("BSAI", "GOA"))]                              # Set consistent order for FMPs
  c[, TARGET_lbl := as.factor(TARGET_lbl)]                                      # Order target alphabetically
  c[, POOL := factor(POOL, levels=c("OB", "ZE"))]                         # Order the pools in the same way as the gap plots
  c[, GRP := factor(GRP, levels=c("Effort", "Area-Level Data"))]        # Order the effort/monitoring groups
  
  if("ZE" %in% pools) c <- rbind(c, copy(c[GRP=="Area-Level Data"])[, POOL := "ZE"])  # Copy observer coverage to ZE pool 
  
  p_lst <- vector(mode="list")
  x_maj <- c(week(as.Date(paste(ar_year, seq(1,12,3), "01", sep="-"))), 54)     # Week 54 being January of next year
  x_min <- week(as.Date(paste(ar_year, 1:12, "01", sep="-")))

  for(i in unique(c$FMP)){
    # i <- "BSAI"
    # i <- "GOA"
    e <- c[FMP==i & N!=0]
    
    if(conf==FALSE){
      levs <- unique(e$AREA); levs <- levs[order(levs)] 
      e[, AREA := factor(AREA, levels=levs)]
    }
    if(conf==TRUE){
      f <- rect_test[FMP==i]
      levs <- union(unique(e$AREA), unique(f$AREA)); levs <- levs[order(levs)]    # Use this to ensure the order of AREAS on y-axis is correct and no areas are dropped
      e[, AREA := factor(AREA, levels=levs)]
      f[, AREA := factor(AREA, levels=levs)]
    }

    p <- ggplot() +
      geom_vline(xintercept=week(c("2020-03-26", "2020-07-01")), linetype=2) +
      facet_grid(POOL~FMP+STRATA+TARGET_lbl, drop=TRUE) +                       # drop=TRUE will drop targets that don't appear in both BSAI/GOA
      geom_tile(data=e[GRP=="Effort"], aes(x=WEEK, y=AREA, fill=GRP, alpha=N), height=0.5, position=position_nudge(y=1/8)) + 
      geom_tile(data=e[GRP=="Area-Level Data"], aes(x=WEEK, y=AREA, fill=GRP, alpha=N), height=0.5, position=position_nudge(y=-1/8)) + 
      scale_fill_manual(values=c("blue", "red")) +
      scale_y_discrete(limits=levs) + 
      scale_alpha_continuous(name="Relative Concentration", range=c(0.10,0.75), breaks=seq(0, 1, by=1/4)) +   
      scale_x_continuous(limits=c(1, 54), breaks=x_maj, labels=c("Jan", "Apr", "Jul", "Oct", "Jan"), minor_breaks = x_min) + 
      labs(x="Month", y="Area", fill="")   
      # TODO - ADD VERTICAL LINES TO DENOTE TIMES PERIODS -   Mar 26 when waivers first started, July 1st for when waviers lifted, 
      # FIXME - breaks here under theme?
      # theme(legend.position="bottom", strip.text=element_text(margin=margin(0.05, 0.05, 0.05, 0.05,"cm")), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) 
      
    if(conf==TRUE & nrow(f)>0){
      p <- p + geom_polygon(data=f, aes(x=WEEK, y=as.numeric(AREA)+Y_ADJ, group=POLY_ID), fill="white", alpha=0.50) + 
        geom_path(data=f, aes(x=WEEK, y=as.numeric(AREA)+Y_ADJ, group=POLY_ID, color=COLOR), alpha=0.25, size=0.5, linejoin="mitre", lineend="square") + 
        # HERE IS WHERE IT BREAKS - happens when only zero pool (and not PB) has confidential data?
        geom_text(data=f, aes(label=formatC(P_OBSERVED, digits=2, format="f"), x=26, y=as.numeric(AREA)+3/16), size=2.5, na.rm=TRUE, check_overlap = TRUE)
    }
    p_lst[[i]] <- p
  }
  if(legend==T) return( ggarrange(plotlist=p_lst, nrow=1, common.legend = T, legend="bottom", widths=d$N/sum(d$N)) )
  if(legend==F) return( ggarrange(plotlist=p_lst, nrow=1, legend="none", widths=d$N/sum(d$N)) )
}

# 'Confidential' plots will not show fishing effort for target X area combinations with < 3 vessels 
obze_HAL   <- ts_obze(actu_date[GEAR=="HAL"])                    # 1000 x 600  
obze_HAL_c <- ts_obze(actu_date[GEAR=="HAL"], conf=F)   
obze_POT   <- ts_obze(actu_date[GEAR=="POT"])                    # 1000 x 600
obze_POT_c <- ts_obze(actu_date[GEAR=="POT"], conf=F)
obze_TRW   <- ts_obze(actu_date[GEAR=="TRW"])                    # 1000 x 400
obze_TRW_c <- ts_obze(actu_date[GEAR=="TRW"], conf=F)

# TODO - Make a figures folder for the markdown to pull from?
# Saving as PDF makes the tiles looks kind of weird because they overlap slightly and make it not look as neat - save as png isntead?
# ggsave("GapFigures/ts_png/ts_plt_obze_HAL_conf.png", plot=obze_HAL_c, device="png", width=11, height=8.5, units="in")
# ggsave("GapFigures/ts_png/ts_plt_obze_HAL.png", plot=obze_HAL, device="png", width=11, height=8.5, units="in")
# ggsave("GapFigures/ts_png/ts_plt_obze_POT_conf.png", plot=obze_POT_c, device="png", width=11, height=8.5, units="in")
# ggsave("GapFigures/ts_png/ts_plt_obze_POT.png", plot=obze_POT, device="png", width=11, height=8.5, units="in")
# ggsave("GapFigures/ts_png/ts_plt_obze_TRW_conf.png", plot=obze_TRW_c, device="png", width=11, height=8.5, units="in")
# ggsave("GapFigures/ts_png/ts_plt_obze_TRW.png", plot=obze_TRW, device="png", width=11, height=8.5, units="in")
# Can convert these to PDF without much trouble - the text isn't as crisp as if saved as PDF in the first place, but the tiles looks way better

# # Save OB/ZE time/space plots to Figures folder
# ggsave("GapFigures/obze_plt_HAL.pdf", plot=obze_HAL, device="pdf", width=11, height=8.5, units="in")
# ggsave("GapFigures/obze_plt_HAL_conf.pdf", plot=obze_HAL_c, device="pdf", width=11, height=8.5, units="in")
# ggsave("GapFigures/obze_plt_POT.pdf", plot=obze_POT, device="pdf", width=11, height=8.5, units="in")
# ggsave("GapFigures/obze_plt_POT_conf.pdf", plot=obze_POT_c, device="pdf", width=11, height=8.5, units="in")
# ggsave("GapFigures/obze_plt_TRW.pdf", plot=obze_TRW, device="pdf", width=11, height=8.5, units="in")
# ggsave("GapFigures/obze_plt_TRW_conf.pdf", plot=obze_TRW_c, device="pdf", width=11, height=8.5, units="in")

# OBZE Time/Space Combined into one page
obze_HAL_nl <- ts_obze(actu_date[GEAR=="HAL"], legend=F)
obze_POT_nl <- ts_obze(actu_date[GEAR=="POT"], legend=F)
obze_ALL <- ggarrange(obze_HAL_nl, obze_POT_nl, obze_TRW, ncol=1, heights=c(4,4,3)) 
#ggsave("GapFigures/obze_plt_ALL.pdf", plot=obze_ALL, device="pdf", width=8.5, height=11, units="in")
ggsave("GapFigures/ts_png/ts_plt_obze_ALL.png", plot=obze_ALL, device="png", width=8.5, height=11, units="in")

obze_HAL_c_nl <- ts_obze(actu_date[GEAR=="HAL"], conf=F, legend=F)
obze_POT_c_nl <- ts_obze(actu_date[GEAR=="POT"], conf=F, legend=F)
obze_ALL_c <- ggarrange(obze_HAL_c_nl, obze_POT_c_nl, obze_TRW_c, ncol=1, heights=c(4,4,3)) 
#ggsave("GapFigures/obze_plt_ALL_conf.pdf", plot=obze_ALL_c, device="pdf", width=8.5, height=11, units="in")
ggsave("GapFigures/ts_png/ts_plt_obze_ALL_conf.png", plot=obze_ALL_c, device="png", width=8.5, height=11, units="in")

#========================#
# EM Time/Space Plots ####
ts_em <- function(a, legend=TRUE, conf=TRUE, n_min=5){
  # These plots summarize EM and OB pool coverage to EM
  #   a <- actu_date[GEAR=="HAL"]; legend <- TRUE; conf <- FALSE; n_min=5
  #   a <- actu_date[GEAR=="HAL"]; legend <- TRUE; conf <- TRUE; n_min=5
  #   a <- actu_date[GEAR=="POT"]; legend <- TRUE; conf <- FALSE; n_min=5
  #   a <- actu_date[GEAR=="POT"]; legend <- TRUE; conf <- TRUE; n_min=5

  a <- a[POOL!="ZE"]   # Remove any ZE trips
  a[, AREA := as.factor(AREA)] # Convert AREA to factor
  
  # Coverts all trip date ranges to week bin counts
  to_week <- function(x, flip=FALSE, yr=2019){
    #  x <- a[POOL=="OB" & STRATA=="HAL" & TARGET=="I" & AREA==519]; flip <- FALSE; yr <- 2019
    #  x <- copy(b)[[1]]
    x[year(START) < yr, START := as.Date("2019-01-01")]  # If any incoming date ranges are outside of the year, truncate them to be within specified year
    x[year(END) > yr, END := as.Date("2019-12-31")]
    cols <- unique(x[, c("POOL", "STRATA", "TARGET_lbl", "FMP", "AREA")])
    x <- setnames(x[, apply(.SD, MARGIN=1, function(y) seq.int(week(y[1]), week(y[2]))), by=.(TRIP_ID, AREA), .SDcols=c("START", "END")], c("TRIP_ID", "AREA", "WEEK"))
    x <- x[, .N, keyby=WEEK][SJ(1:53)][is.na(N), N := 0][, WEEK := as.numeric(WEEK)]
    x <- cbind(x, cols)  # Put identifier columns back in
    return(x)
  }
  
  target_tbl <- data.table(
    TARGET = c("A", "C", "D", "E", "F", "H", "I", "K", "L", "M", "O", "P", "R", "S", "T", "W", "X", "Y"),
    TARGET_lbl = c("Atka Mackerel", "Pacific Cod", "Deep Water Flatfish", "Alaska Plaice", "Other Flatfish", "Shallow Water Flatfish", "Halibut", "Rockfish", "Flathead Sole", "Kamchatka Flounder", "Other Species", "Pollock", "Rock Sole", "Sablefish", "Greenland Turbot", "Arrowtooth Flounder", "Rex Sole", "Yellowfin Sole"))
  a[, TARGET_lbl := target_tbl[a, TARGET_lbl, on=.(TARGET)]]                     # Merge in target lbl
  a[, STRATA := GEAR]    # Define STRATA by gear type for EM and ZE pools (i.e., data from pot_tender and pot will apply to pot gear EM trips)
  # Like the gap analysis, remove domains with < 5 trips
  a_keep <- a[, .N, keyby=.(STRATA, TARGET, FMP)][N>=n_min]  # Filter out domains with < 3 trips
  a <- a[a_keep, on=.(STRATA, TARGET, FMP)]
  pools <- unique(a$POOL) # Get vector of unique pools - we will use this to decide if we need to copy the monitoring data for other pools
  d <- unique(a[POOL=="EM", .(STRATA, TARGET_lbl), by=FMP])[, .N, by=FMP]
  
  if(conf==TRUE){
    # Filter out trips here to maintain confidentiality
    # First, remove any domains with <5 trips. Then If any areas have <3 distinct vessels, remove trips but add a gray label that instead gives percentage of trips monitored
    a[, VESSEL_ID := unique(work_data[, .(TRIP_ID, VESSEL_ID)])[a, VESSEL_ID, on=.(TRIP_ID)]]  # Merge in vessel ID's
    a_remv <- a[, .(VES_N = uniqueN(VESSEL_ID), TRP_N=uniqueN(TRIP_ID), P_OBSERVED=sum(OBSERVED=="Y")/.N), keyby=.(POOL, STRATA, TARGET_lbl, FMP, AREA)][VES_N<3]  # these areas will be replaced with gray bar
    a_keep <- a[, .(VES_N = uniqueN(VESSEL_ID), TRP_N=uniqueN(TRIP_ID)), keyby=.(POOL, STRATA, TARGET_lbl, FMP, AREA)][VES_N>=3]
    a <- a[a_keep, on=.(POOL, STRATA, TARGET_lbl, FMP, AREA)]
    
    # Prepare polygons for confidential data
    rect_test <- copy(a_remv)
    rect_test <- rect_test[!(POOL=="OB" & P_OBSERVED==0)]  # Remove OB pool domains without any coverage
    rect_test <- rbindlist(rep(list(rect_test), times=5))
    rect_test[, INDEX := seq_len(.N), keyby=.(POOL, STRATA, TARGET_lbl, FMP, AREA)]
    rect_test[, WEEK := ifelse(INDEX %in% c(1,4,5), 1, 54)]
    rect_test[, Y_ADJ := ifelse(INDEX %in% c(1,2,5), -1/8, 3/8)]   # or use 0, 3/8 to prevent any overlap
    rect_test[, COLOR := I("red")]
    
    rect_mntr <- copy(rect_test[P_OBSERVED > 0])[, ':=' (Y_ADJ = -Y_ADJ, COLOR = I("blue"), P_OBSERVED = NA_real_)]  # For monitored domains, copy polygons, flip over x-axis and color blue
    rect_test <- rect_test[!(POOL=="OB" & COLOR=="red")]    # Remove OB Effort from domains that got coverage
    rect_em <- copy(rect_test[POOL=="EM"])[, ':=' (POOL = "OB", P_OBSERVED = NA_real_)]  # Copy polygons for confidential EM effort and paste to OB pool
    rect_test <- rbind(rect_test, rect_mntr, rect_em)
    rect_test[, POLY_ID := .GRP, keyby=.(POOL, STRATA, TARGET_lbl, FMP, AREA, COLOR)]
  }
  
  b <- copy(a[OBSERVED=="Y"])[, ':=' (START=START-15, END=END+15)] # Monitored OB and EM trips only - extend date ranges by 30 days to represent data coverage at AREA level
  a <- split(a, f=list(a$FMP, a$POOL, a$STRATA, a$TARGET, a$AREA), drop=TRUE)   # Split data by pool, post-strata, and AREA
  b <- split(b, f=list(b$FMP, b$POOL, b$STRATA, b$TARGET, b$AREA), drop=TRUE)   # Split monitored data by pool, post-strata, and AREA
  
  a <- rbindlist(lapply(a, to_week))  # Convert to weekly bin counts
  b <- rbindlist(lapply(b, to_week))
  c <- rbind(a[, ':=' (GRP="Effort")], b[, ':=' (GRP="Area-Level Data")])
  c[, AREA := as.factor(AREA)]
  c[, N := as.numeric(N)]  # convert from integer to numeric
  c[, N := N/max(N), by=.(POOL, STRATA, TARGET_lbl, FMP)]                  # TODO REMOVED GRP to see what it'd look like # Scale the counts by each pool/strata/target/fmp (i.e., each facet of the final plot)                                         # Create subset of area-level data to be pasted to other pools later
  
  c[, FMP := factor(FMP, levels=c("BSAI", "GOA"))]                              # Set consistent order for FMPs
  c[, TARGET_lbl := as.factor(TARGET_lbl)]                                      # Order target alphabetically
  c[, POOL := factor(POOL, levels=c("EM", "OB"))]                         # Order the pools in the same way as the gap plots
  c[, GRP := factor(GRP, levels=c("Effort", "Area-Level Data"))]        # Order the effort/monitoring groups
  
  c <- c[!(POOL=="OB" & GRP=="Effort")] # Now that OB area-level data was scaled, remove OB effort
  c <- rbind(c, copy(c[GRP=="Effort"])[, POOL := "OB"])  # Copy EM effort to OB pool facet
  
  p_lst <- vector(mode="list")
  x_maj <- c(week(as.Date(paste("2019", seq(1,12,3), "01", sep="-"))), 54)      # Week 54 being January of next year
  x_min <- week(as.Date(paste("2019", 1:12, "01", sep="-")))
  
  for(i in unique(c$FMP)){
    # i <- "BSAI"
    # i <- "GOA"
    e <- c[FMP==i & N!=0]
    
    if(conf==FALSE){
      levs <- unique(e$AREA); levs <- levs[order(levs)] 
      e[, AREA := factor(AREA, levels=levs)]
    }
    if(conf==TRUE){
      f <- rect_test[FMP==i]
      levs <- union(unique(e$AREA), unique(f$AREA)); levs <- levs[order(levs)]    # Use this to ensure the order of AREAS on y-axis is correct and no areas are dropped
      e[, AREA := factor(AREA, levels=levs)]
      f[, AREA := factor(AREA, levels=levs)]
    }
    
    p <- ggplot() +
      facet_grid(POOL~FMP+STRATA+TARGET_lbl, drop=TRUE) +                       # drop=TRUE will drop targets that don't appear in both BSAI/GOA
      geom_tile(data=e[GRP=="Effort"], aes(x=WEEK, y=AREA, fill=GRP, alpha=N), height=0.5, position=position_nudge(y=1/8)) + 
      geom_tile(data=e[GRP=="Area-Level Data"], aes(x=WEEK, y=AREA, fill=GRP, alpha=N), height=0.5, position=position_nudge(y=-1/8)) + 
      scale_fill_manual(values=c("blue", "red")) +
      scale_y_discrete(limits=levs) + 
      scale_alpha_continuous(name="Relative Concentration", range=c(0.10,0.75), breaks=seq(0, 1, by=1/4)) +   
      scale_x_continuous(limits=c(1, 54), breaks=x_maj, labels=c("Jan", "Apr", "Jul", "Oct", "Jan"), minor_breaks = x_min) + 
      labs(x="Month", y="Area", fill="") + 
      theme(legend.position="bottom", strip.text=element_text(margin=margin(0.05, 0.05, 0.05, 0.05,"cm")), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) 
    
    if(conf==TRUE){
      p <- p + geom_polygon(data=f, aes(x=WEEK, y=as.numeric(AREA)+Y_ADJ, group=POLY_ID), fill="white", alpha=0.50) + 
        geom_path(data=f, aes(x=WEEK, y=as.numeric(AREA)+Y_ADJ, group=POLY_ID, color=COLOR), alpha=0.25, size=0.5, linejoin="mitre", lineend="square") + 
        geom_text(data=f[!is.na(P_OBSERVED)], aes(label=formatC(P_OBSERVED, digits=2, format="f"), x=26, y=as.numeric(AREA)+3/16), size=2.5, na.rm=TRUE, check_overlap = TRUE)
    }
    p_lst[[i]] <- p
  }
  if(legend==T) return( ggarrange(plotlist=p_lst, nrow=1, common.legend = T, legend="bottom", widths=d$N/sum(d$N)) )
  if(legend==F) return( ggarrange(plotlist=p_lst, nrow=1, legend="none", widths=d$N/sum(d$N)) )
}

em_HAL <- ts_em(actu_date[GEAR=="HAL"])   # 1000 x 600
em_POT <- ts_em(actu_date[GEAR=="POT"])
em_HAL_c <- ts_em(actu_date[GEAR=="HAL"], conf=F)
em_POT_c <- ts_em(actu_date[GEAR=="POT"], conf=F)

# Save EM Time/Space plots to Figure folder
ggsave("GapFigures/ts_png/ts_plt_em_HAL.png", plot=em_HAL, device="png", width=11, height=8.5, units="in")
ggsave("GapFigures/ts_png/ts_plt_em_HAL_conf.png", plot=em_HAL_c, device="png", width=11, height=8.5, units="in")
ggsave("GapFigures/ts_png/ts_plt_em_POT.png", plot=em_POT, device="png", width=11, height=8.5, units="in")
ggsave("GapFigures/ts_png/ts_plt_em_POT_conf.png", plot=em_POT_c, device="png", width=11, height=8.5, units="in")

# ggsave("GapFigures/em_plt_HAL.pdf", plot=em_HAL, device="pdf", width=11, height=8.5, units="in")
# ggsave("GapFigures/em_plt_HAL_conf.pdf", plot=em_HAL_c, device="pdf", width=11, height=8.5, units="in")
# ggsave("GapFigures/em_plt_POT.pdf", plot=em_POT, device="pdf", width=11, height=8.5, units="in")
# ggsave("GapFigures/em_plt_POT_conf.pdf", plot=em_POT_c, device="pdf", width=11, height=8.5, units="in")

# EM Time/Space Combined into one page
em_HAL_nl <- ts_em(actu_date[GEAR=="HAL"], legend=F)
em_ALL <- ggarrange(em_HAL_nl, em_POT, ncol=1, heights=c(9.5,10)) 
ggsave("GapFigures/ts_png/ts_plt_em_ALL.png", plot=em_ALL, device="png", width=8.5, height=11, units="in")
#ggsave("GapFigures/em_plt_ALL.pdf", plot=em_ALL, device="pdf", width=8.5, height=11, units="in")

em_HAL_c_nl <- ts_em(actu_date[GEAR=="HAL"], conf=F, legend=F)
em_ALL_c <- ggarrange(em_HAL_c_nl, em_POT_c, ncol=1, heights=c(9.5,10)) 
ggsave("GapFigures/ts_png/ts_plt_em_ALL_conf.png", plot=em_ALL_c, device="png", width=8.5, height=11, units="in")
#ggsave("GapFigures/em_plt_ALL_conf.pdf", plot=em_ALL_c, device="pdf", width=8.5, height=11, units="in")

#===========================================#
# TIME/SPACE PLOTS WITH INDIVIDUAL TRIPS ####
# Older version of time/space plots - here just for posterity

# OLD OBZE TIME-SPACE plots (optionally with EM)
ob_plot <- function(a){
  #  a <- actu_date[GEAR=="POT"]
  #  a <- actu_date[GEAR=="TRW"]
  #  a <- actu_date[GEAR=="HAL"]
  
  b <- copy(a)[GEAR!="JIG" & !(POOL=="OB" & GEAR != gsub("_TENDER", "", STRATA))][order(FMP)]
  b1 <- b[, .N, by=.(STRATA, TARGET, FMP)][N >= 5]  # Exclude any groups that had fewer than 5 trips
  b <- b[b1, on=.(STRATA, TARGET, FMP)]
  
  target_tbl <- data.table(
    TARGET = c("A", "C", "D", "E", "F", "H", "I", "K", "L", "M", "O", "P", "R", "S", "T", "W", "X", "Y"),
    TARGET_lbl = c("Atka Mackerel", "Pacific Cod", "Deep Water Flatfish", "Alaska Plaice", "Other Flatfish", "Shallow Water Flatfish", "Halibut", "Rockfish", "Flathead Sole", "Kamchatka Flounder", "Other Species", "Pollock", "Rock Sole", "Sablefish", "Greenland Turbot", "Arrowtooth Flounder", "Rex Sole", "Yellowfin Sole"))
  b[, TARGET_lbl := target_tbl[b, TARGET_lbl, on=.(TARGET)]]                     # Merge in target lbl
  
  b[POOL=="OB" & OBSERVED=="Y", ':=' (START=START-15L, END=END+15L)]
  b[POOL=="EM", ':=' (OBSERVED="N", STRATA=GEAR)]
  b[POOL=="ZE", ':=' (STRATA=GEAR)]
  
  if(nrow(b[POOL=="EM"])>0){b <- rbind(b, copy(b[OBSERVED=="Y"])[, POOL:="EM"])}
  if(nrow(b[POOL=="ZE"])>0){b <- rbind(b, copy(b[OBSERVED=="Y"])[, POOL:="ZE"])}
  b[, POOL := factor(POOL, levels=c("OB", "EM", "ZE"))]
  
  p_lst <- vector(mode="list", length=uniqueN(b$FMP)) 
  
  for(i in 1:uniqueN(b$FMP)){
    # i <- 1
    c <- b[FMP==unique(b$FMP)[i]]
    p_lst[[i]] <- ggplot(c, aes(x=AREA, ymin=START, ymax=END, color=OBSERVED)) + 
      geom_linerange(data=c[OBSERVED=="Y"], size=5, alpha=0.25) + 
      geom_linerange(data=c[OBSERVED=="N"], size=1, position=position_dodge2(0.4)) + 
      coord_flip() + facet_grid(POOL ~ paste("Gear:", GEAR) + FMP + TARGET_lbl + paste("Strata:", STRATA), scales="free_y", space="free") + 
      labs(y="Month", x="Area", color="Observed", subtitle="OB coverage vs OB/EM/ZE effort.") + 
      theme(legend.position="bottom", strip.text=element_text(margin=margin(0.05, 0.05, 0.05, 0.05,"cm"))) + 
      scale_y_date(labels=date_format(format="%m"), date_minor_breaks = "1 month")
  }
  
  d <- unique(b[, .(STRATA, TARGET), by=FMP])[, .N, by=FMP]  # If number of strata/targets differ between FMP, use this to scale the widths of the FMP plots
  
  return(ggarrange(plotlist=p_lst, nrow=1, widths=d$N/sum(d$N)))
  
}
ob_plot(actu_date[GEAR=="HAL" & POOL!="EM"])
ob_plot(actu_date[GEAR=="POT" & POOL!="EM"])
ob_plot(actu_date[GEAR=="TRW" & POOL!="EM"])

# OLD EM TIME-SPACE plots that showed individual trips and EM / OB data coverage - Here for posterity
em_plot <- function(a){
  #  a <- actu_date[GEAR=="POT"]
  #  a <- actu_date[TARGET=="I"]
  #  a <- actu_date[GEAR=="HAL"]
  
  b <- copy(a)[POOL!="ZE" & GEAR!="JIG"][order(FMP)]                            # Remove ZE and any JIG gear trips
  p_lst <- vector(mode="list", length=uniqueN(b$FMP))
  
  target_tbl <- data.table(
    TARGET = c("A", "C", "D", "E", "F", "H", "I", "K", "L", "M", "O", "P", "R", "S", "T", "W", "X", "Y"),
    TARGET_lbl = c("Atka Mackerel", "Pacific Cod", "Deep Water Flatfish", "Alaska Plaice", "Other Flatfish", "Shallow Water Flatfish", "Halibut", "Rockfish", "Flathead Sole", "Kamchatka Flounder", "Other Species", "Pollock", "Rock Sole", "Sablefish", "Greenland Turbot", "Arrowtooth Flounder", "Rex Sole", "Yellowfin Sole"))
  b[, TARGET_lbl := target_tbl[b, TARGET_lbl, on=.(TARGET)]]                     # Merge in target lbl
  
  for(i in 1:uniqueN(b$FMP)){
    # i <- 1
    
    c <- b[FMP==unique(b$FMP)[i]]
    c <- c[!(POOL=="OB" & OBSERVED=="N")]  # Remove unobserved OB pool trips
    c <- rbind(c, copy(c[POOL=="EM"])[, ':=' (POOL="OB", OBSERVED="N")])  # copy all EM pool effort and place in plot with OB avg wgt data
    c[OBSERVED=="Y", ':=' (START=START-15L, END=END+15L)]   # For observed trips (both EM and OB), extend date range by 15 days on each side
    c[, POOL := factor(POOL, levels=c("EM", "OB"))]
    
    p_lst[[i]] <-   ggplot(c, aes(x=AREA, ymin=START, ymax=END, color=OBSERVED)) + 
      geom_linerange(data=c[OBSERVED=="Y"], size=5, alpha=0.25) + 
      geom_linerange(data=c[OBSERVED=="N"], size=1, position=position_dodge2(0.4)) + 
      coord_flip() + facet_grid(POOL ~ paste("Gear:", GEAR) + FMP + TARGET_lbl + "Strata: EM") + 
      labs(y="Month", x="Area", color="Observed", subtitle="EM and OB coverage vs EM effort.") + 
      theme(legend.position="bottom", strip.text=element_text(margin=margin(0.05, 0.05, 0.05, 0.05,"cm"))) + 
      scale_y_date(labels=date_format(format="%m"), date_minor_breaks = "1 month")
    
  }
  d <- b[, .(TARGET_N = uniqueN(TARGET)), by=FMP]  #If number of targets differ between FMP, use this to scale the widths of the FMP plots

  return(ggarrange(plotlist=p_lst, nrow=1, widths=d$TARGET_N/sum(d$TARGET_N)))
  
}
em_plot(actu_date[GEAR=="HAL"])
em_plot(actu_date[GEAR=="POT"])

#=========================#
#### EXPLORATORY STUFF ####
#=========================#
# The stuff below is super disorganized, but contains some of the additional digging GM did

# Realized rates by NMFS Area
rr_area <- actu_date[, .(N=.N, OBS=sum(OBSERVED=="Y"), PERC=100*round(sum(OBSERVED=="Y")/.N,4)), keyby=.(STRATA, TARGET, FMP, AREA, GEAR)]
rr_area[STRATA=="HAL" & TARGET=="I"]

# Realized rates by FMP
rr_fmp <- unique(actu_date[, .(TRIP_ID, STRATA, TARGET, FMP, OBSERVED, GEAR)])[, .(N=.N, OBS=sum(OBSERVED=="Y"), PERC=100*round(sum(OBSERVED=="Y")/.N,4)), keyby=.(STRATA, TARGET, FMP, GEAR)]
rr_fmp[STRATA=="HAL" & N >= 5]

rr_area[STRATA=="HAL" & TARGET=="C"]
rr_fmp[STRATA=="HAL" & TARGET=="C"]

rr_area[STRATA=="TRW" & TARGET=="W"]
rr_fmp[STRATA=="TRW" & TARGET=="W"]
rr_fmp[STRATA=="TRW" & N >= 5]

rr_fmp[STRATA=="POT"]
rr_area[STRATA=="POT" & TARGET=="C" & FMP=="BSAI"][order(-AREA)]  # Little area-specific data in 516 given 12 trips, and Fall in 509 had little monitoring
rr_fmp[STRATA=="POT" & TARGET=="C"]

rr_fmp[STRATA=="EM" & GEAR=="HAL"]

actu_date[STRATA=="EM" & GEAR=="HAL" & FMP=="BSAI" & TARGET=="I"][order(START)]

work_data[TRIP_ID %in% actu_date[STRATA=="HAL" & TARGET=="S" & FMP=="BSAI", unique(TRIP_ID)], unique(VESSEL_ID)]
actu_date[TRIP_ID %in% work_data[VESSEL_ID==33530, unique(TRIP_ID)]]
unique(actu_date[TRIP_ID %in% work_data[VESSEL_ID==33530, unique(TRIP_ID)], .(TRIP_ID, STRATA, TARGET, START, END, OBSERVED)])[order(START)] # Took 15 trips, but jan trip not in ODDS
setDT(odds.dat)
odds.dat[VESSEL_ID==33530 & TRIP_STATUS_CODE=="CP"][order(PLANNED_EMBARK_DATE)]  # 11 trips logged into ODDS

# Pot tender - sig permutation test for landed catch
pot_ten <- unique(actu_date[STRATA=="POT_TENDER", .(TRIP_ID, STRATA, OBSERVED, START, END)])[order(START)]
pot_ten[, .(N=.N, OBS=sum(OBSERVED=="Y"))]  # 13 of 44 trips observed
pot_ten[OBSERVED=="Y"][, .(DUR = as.numeric(END-START, units="days"))]  # durations were pretty comparable, but one trip had huge amounts of catch
work_data[TRIP_ID %in% pot_ten[OBSERVED=="Y", unique(TRIP_ID)]][, .(TOT_WGT = sum(WEIGHT_POSTED, na.rm=TRUE)), keyby=.(TRIP_ID)][order(TOT_WGT)]
work_data[TRIP_ID %in% pot_ten[OBSERVED=="N", unique(TRIP_ID)]][, .(TOT_WGT = sum(WEIGHT_POSTED, na.rm=TRUE)), keyby=.(TRIP_ID)][order(TOT_WGT)]

hist(work_data[TRIP_ID %in% pot_ten[OBSERVED=="Y", unique(TRIP_ID)]][, .(TOT_WGT = sum(WEIGHT_POSTED, na.rm=TRUE)), keyby=.(TRIP_ID)]$TOT_WGT)
hist(work_data[TRIP_ID %in% pot_ten[OBSERVED=="N", unique(TRIP_ID)]][, .(TOT_WGT = sum(WEIGHT_POSTED, na.rm=TRUE)), keyby=.(TRIP_ID)]$TOT_WGT)

ggplot(work_data[TRIP_ID %in% pot_ten[, unique(TRIP_ID)]][, .(TOT_WGT = sum(WEIGHT_POSTED, na.rm=TRUE)), keyby=.(TRIP_ID, OBSERVED_FLAG)]) + 
  geom_histogram(aes(fill=OBSERVED_FLAG, x=TOT_WGT))
ggplot(work_data[TRIP_ID %in% pot_ten[, unique(TRIP_ID)]][, .(TOT_WGT = sum(WEIGHT_POSTED, na.rm=TRUE)), keyby=.(TRIP_ID, OBSERVED_FLAG)]) + 
  geom_density(aes(fill=OBSERVED_FLAG, x=TOT_WGT), alpha=0.5) + theme(legend.position="bottom")
# This is likely an issue caused where valhalla stitches multiple trips together - a super trip has even one component trip observed, valhalla labels the entire trip observed!

# BSAI-S-HAL days

hal_bsai_s <- unique(actu_date[STRATA=="HAL" & TARGET=="S" & FMP=="BSAI", .(TRIP_ID, START, END)])
hal_bsai_s[, DIFF := as.numeric(END-START, units="days")]
sum(hal_bsai_s$DIFF)

# TEMPORAL PATTERNS IN ODDS
setDT(odds.dat)
odds_ts <- odds.dat[, .(STRATA, VESSEL_ID, LOG_SEQ=TRIP_PLAN_LOG_SEQ, PLANNED_EMBARK_DATE, SEL=TRIP_SELECTED_OBS, TRIP_STATUS_CODE, TRIP_OBS_CODE, INHERIT_OBS_TRIP_SEQ, RN=RANDOM_NUMBER_USED)]
odds_ts[TRIP_OBS_CODE=="OA" & !is.na(INHERIT_OBS_TRIP_SEQ), COLOR:="OBS_INH"]
odds_ts[TRIP_OBS_CODE=="OA" & is.na(INHERIT_OBS_TRIP_SEQ), COLOR := "OBS"]
odds_ts[SEL=="Y" & TRIP_STATUS_CODE=="CP" & TRIP_OBS_CODE=="RO", COLOR := "OBS"]  # Were these observed? why is code 'required obserer' instead of OA?

odds_ts[TRIP_STATUS_CODE=="CN" & TRIP_OBS_CODE=="RO", COLOR:="Y_CN"]
odds_ts[TRIP_STATUS_CODE=="CN" & TRIP_OBS_CODE=="NO", COLOR:="N_CN"]
odds_ts[TRIP_STATUS_CODE=="CN" & TRIP_OBS_CODE=="RL", COLOR:="Y_RL"]  # elected but released 
odds_ts[TRIP_STATUS_CODE=="CP" & TRIP_OBS_CODE=="NO", COLOR:="N_CP"]
odds_ts[TRIP_STATUS_CODE=="CS", COLOR := "SYS_CAN"]
odds_ts[TRIP_STATUS_CODE=="PD", COLOR := "PENDING"]
odds_ts[TRIP_STATUS_CODE=="CC", COLOR := "CSC_CAN"]
odds_ts[is.na(COLOR)]

ggplot(odds_ts[STRATA=="EM HAL"], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1)
ggplot(odds_ts[STRATA=="EM HAL" & COLOR %in% c("OBS", "OBS_INH", "N_CP")], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1) # completed trips
odds_ts[STRATA=="EM HAL" & COLOR %in% c("OBS", "OBS_INH", "N_CP")][, .(N=.N, OBS=sum(TRIP_OBS_CODE=="OA"), PERC = 100*sum(TRIP_OBS_CODE=="OA")/.N)]

ggplot(odds_ts[STRATA=="EM POT"], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1)
ggplot(odds_ts[STRATA=="EM POT" & COLOR %in% c("OBS", "OBS_INH", "N_CP")], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1) # completed trips
odds_ts[STRATA=="EM POT" & COLOR %in% c("OBS", "OBS_INH", "N_CP")][, .(N=.N, OBS=sum(TRIP_OBS_CODE=="OA"), PERC = 100*sum(TRIP_OBS_CODE=="OA")/.N)]
# all 2 inherits realized at beginning of year, and some mismatching between ODDS and eLandings (valhalla)?

ggplot(odds_ts[STRATA=="POT - No Tender"], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1)
ggplot(odds_ts[STRATA=="POT - No Tender" & COLOR %in% c("OBS", "OBS_INH", "N_CP")], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1) # completed trips
odds_ts[STRATA=="POT - No Tender" & COLOR %in% c("OBS", "OBS_INH", "N_CP")][, .(N=.N, OBS=sum(TRIP_OBS_CODE=="OA"), PERC = 100*sum(TRIP_OBS_CODE=="OA")/.N)]
# also had many inherits realized at beginning of year, then more spread out overs weeks 35-52

ggplot(odds_ts[STRATA=="POT - Tender"], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1)
ggplot(odds_ts[STRATA=="POT - Tender" & COLOR %in% c("OBS", "OBS_INH", "N_CP")], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1) # completed trips
odds_ts[STRATA=="POT - Tender" & COLOR %in% c("OBS", "OBS_INH", "N_CP")][, .(N=.N, OBS=sum(TRIP_OBS_CODE=="OA"), PERC = 100*sum(TRIP_OBS_CODE=="OA")/.N)]
# Inherits are spaced fairly evenly, but this has more to do with eLandings and Valhalla with different trip definitions

ggplot(odds_ts[STRATA=="TRW - No Tender"], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1)
ggplot(odds_ts[STRATA=="TRW - No Tender" & COLOR %in% c("OBS", "OBS_INH", "N_CP")], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1) # completed trips
odds_ts[STRATA=="TRW - No Tender" & COLOR %in% c("OBS", "OBS_INH", "N_CP")][, .(N=.N, OBS=sum(TRIP_OBS_CODE=="OA"), PERC = 100*sum(TRIP_OBS_CODE=="OA")/.N)]
trw_nt <- odds_ts[STRATA=="TRW - No Tender" & COLOR %in% c("OBS", "OBS_INH", "N_CP")]
trw_nt[, WEEK := week(PLANNED_EMBARK_DATE)]
trw_nt[, N := .N, keyby=WEEK]
trw_nt2 <- trw_nt[, .(n = .N), keyby=.(WEEK, COLOR, N)]
trw_nt2[, PROP := n/N]
ggplot(trw_nt2, aes(x=WEEK, y=PROP, fill=COLOR)) + geom_col()
# Could be due to inherits but not obvious - inherits wer taken evenly moreorless

ggplot(odds_ts[STRATA=="TRW - Tender"], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1)
ggplot(odds_ts[STRATA=="TRW - Tender" & COLOR %in% c("OBS", "OBS_INH", "N_CP")], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1) # completed trips
odds_ts[STRATA=="TRW - Tender" & COLOR %in% c("OBS", "OBS_INH", "N_CP")][, .(N=.N, OBS=sum(TRIP_OBS_CODE=="OA"), PERC = 100*sum(TRIP_OBS_CODE=="OA")/.N)]

ggplot(odds_ts[STRATA=="HAL"], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1)
ggplot(odds_ts[STRATA=="HAL" & COLOR %in% c("OBS", "OBS_INH", "N_CP")], aes(x=week(PLANNED_EMBARK_DATE), fill=COLOR)) + geom_bar(width=1) # completed trips
odds_ts[STRATA=="HAL" & COLOR %in% c("OBS", "OBS_INH", "N_CP")][, .(N=.N, OBS=sum(TRIP_OBS_CODE=="OA"), PERC = 100*sum(TRIP_OBS_CODE=="OA")/.N)]

pot_ten <- odds_ts[STRATA=="POT - Tender"]
pot_ten[TRIP_STATUS_CODE %in% c("CP", "PD"), sum(TRIP_OBS_CODE=="OA")/.N]  # realized 15.5%
pot_ten[TRIP_STATUS_CODE %in% c("CP", "PD"), table(TRIP_OBS_CODE, exclude=FALSE)]  # 15 observed out of 103. (includes PENDING trip_status_code)
pot_ten[TRIP_OBS_CODE=="OA" & !is.na(INHERIT_OBS_TRIP_SEQ)]  # 5 of those 15 trips were due to inheritsm 3 of which take in late august-September when it went out of range

em_pot <- odds.dat %>% filter(STRATA=="EM POT")  # Trip_selected_obs includes CP and PD trips only
setDT(em_pot)
xtabs(data=em_pot, ~ TRIP_STATUS_CODE + TRIP_OBS_CODE + TRIP_SELECTED_OBS, addNA=TRUE)

odds.ts.daily.cuml %>% filter(STRATA=="EM POT" & TYPE=="Adjusted date: Valid trips only") %>% tail(5)  # 53 out of 149 observed
em_pot[TRIP_STATUS_CODE %in% c("CP", "PD"), table(TRIP_STATUS_CODE)]  # 142 completed, 7 pending
em_pot[!is.na(INHERIT_OBS_TRIP_SEQ)]  # only 2 trips inherited, both completed, both at start of year (Jan 3 and Jan 5)
em_pot[RANDOM_NUMBER_USED < ODDS_SELECTION_PCT/100 & !(TRIP_STATUS_CODE %in% c("CP", "PD"))]  # Only one selected trip not observed due to release,, back in october
em_pot

# Pot -no tender starts off beginning of year higher, eventually ends at expected
pot_nt <- odds_ts[STRATA == "POT - No Tender"]
pot_nt[is.na(SEL), table(TRIP_STATUS_CODE)]
pot_nt[!is.na(SEL), table(TRIP_STATUS_CODE)]
pot_nt[!is.na(SEL), table(TRIP_OBS_CODE)]
pot_nt[!is.na(SEL) & !is.na(INHERIT_OBS_TRIP_SEQ)][order(LOG_SEQ)]  # 18 inherited trips were completed - 9 were taken in January! 

# EM HAL
em_hal <- odds_ts[STRATA == "EM HAL"]
em_hal[is.na(SEL), table(TRIP_STATUS_CODE)]
em_hal[!is.na(SEL), table(TRIP_STATUS_CODE)]
em_hal[!is.na(SEL), table(TRIP_OBS_CODE)]
em_hal[!is.na(SEL) & !is.na(INHERIT_OBS_TRIP_SEQ)][order(LOG_SEQ)]  # 19 trips observed, 1 was somehow SEL=="N"? 2 required an observer but trip_obs_code is not "OA"?
# 5 of these inherited trips wer taken in Jan, 8 were between Jan thru March
odds.dat %>% filter(TRIP_PLAN_LOG_SEQ %in% c(109803, 110677))
odds.ts.daily.cuml %>% filter(STRATA=="EM HAL" & TYPE=="Adjusted date: Valid trips only")  # Should running total include inherited trips?
# Runing proportion of monitored trips due to inherits?

strata <- "EM POT"
test <- odds.ts.daily.cuml %>% filter(STRATA==strata & TYPE=="Adjusted date: Valid trips only") %>% mutate(running.ni=cumsum(ni), running.ni.prop = running.ni/running.n)
ggplot(test, aes(x=Date)) + geom_line(aes(y=running.ni.prop), color="black")
# So why in fig 2 does gray line start veering upwards in september for TRW - Tender?

a <- copy(For.ts.temporal.plot)
setDT(a)
a[, .(PERC = 100*round(sum(Outside.bounds)/.N,3)) , by=STRATA]

setDT(odds_new)
odds_new[ODDS_STRATA=="POT - Tender" & !is.na(SEL)]  # 103 completed/pending trips in odds
actu_date[STRATA=="POT_TENDER", uniqueN(TRIP_ID)]  # Only 44 trips in valhalla

odds_new[ODDS_STRATA=="POT - Tender" & !is.na(INHERIT_SEQ) & !is.na(SEL)]

odds_new[VESSEL_ID==1137 & !is.na(SEL) & ODDS_STRATA=="POT - Tender"]  # 9/7 observed and 9/25 observed, and 3 odds trips 9/11, 9/17, 9/20, all were not selected
val_trp[VESSEL_ID == 1137 & VAL_STRATA=="POT - Tender"]  # YNNNY ODDS trips all rolled into one Valhalla trip

odds_new[VESSEL_ID==3035 & !is.na(SEL)]  # 8/28 observed, 9/4 not observed
val_trp[VESSEL_ID == 3035] # 9/1-9/6 observed, so two ODDS trips (YN) rolled into one valhalla trip? Three odds trips from 9/7, 9/11, and 9/16 all got rolled into one VAL trip

odds_new[VESSEL_ID==5474 & !is.na(SEL)]  # 9/13 inherited, but 9/9-9/9 not observed and 9/17 not observed. So only 1 or 2 of 9 days actually observeed?
val_trp[VESSEL_ID == 5474]  # 9/11 to 9/17 observed, so 3 ODDS trips (NYN) rolled into one VAL trip






# TODO ADDING PORT
# We don't know which port trips departed from - only where they landed!
a <- work_data[COVERAGE_TYPE=="PARTIAL", .(WGT = sum(WEIGHT_POSTED, na.rm=T)), by=.(TRIP_ID, PORT_CODE)][order(TRIP_ID, WGT)]
uniqueN(a$TRIP_ID); nrow(a)   # some trips have multiple ports...
a[, .SD[WGT==max(WGT)], by=TRIP_ID]

