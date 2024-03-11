library(data.table)
library(ggplot2)
library(FMAtools)

project_folder <- gdrive_set_dribble("Projects/AnnRpt-Deployment-Chapter/")

# Load Data and Functions ----------------------------------------------------------------------------------------------

#' *Just for now (use updated dataset when it is ready*
#' Loading outputs of 2024 ADP/analyses/allocation_evaluation/data_prep.R, using the draft version, since the final
#' already had new stratum definitions applied. Only has 2022 data, not 2023
(load("source_data/data_prep_outputs.Rdata"))

#' Load spatial data
(load("source_data/ak_shp.rdata"))

#' Load spatiotemporal functions developed for the 2024 ADP. 
source("functions/spatiotemp_functions.R")

#' Load the ADFG statistical area shapefile.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

# Load work.data from the 2024 Final ADP
(load("source_data/2024_Final_ADP_data_ver6.rdata"))

## Realized Rates ------------------------------------------------------------------------------------------------------

#' TODO Get the realized rates for the 2022 and 2023 ADPs

#' Subset 2022 and get TRIP_IDs for work.data.  *Do this the other way around when the data is ready*
pc_dat <- pc_effort_dt[ADP %in% 2022]   #' TODO *ADD 2023 WHEN WE HAVE IT*
wd_trip_ids <- unique(pc_dat[, .(TRIP_ID, wd_TRIP_ID)])

wd_sub <- work.data[TRIP_ID %in% wd_trip_ids$wd_TRIP_ID]
uniqueN(wd_trip_ids); uniqueN(wd_sub$TRIP_ID)  #' FIXME 6268 ids, short of the 6271 I had. No idea where they went. Oh well
realized_rate <- unique(wd_sub[, .(TRIP_ID, STRATA, OBSERVED_FLAG)])[, .(REALIZED_RATE = sum(OBSERVED_FLAG == "Y")/.N), keyby = .(STRATA)]
realized_rate[, STRATA := fcase(
  STRATA %in% c("EM_HAL", "EM_POT", "ZERO"), STRATA,
  STRATA == "EM_TRW_EFP", "EM_TRW",
  STRATA %in% c("HAL", "POT", "TRW"), paste0("OB_", STRATA)
)]
realized_rate[, ADP := 2022L]
realized_rate[, SAMPLE_RATE := REALIZED_RATE]

## Programmed Rates ----------------------------------------------------------------------------------------------------
#' TODO Get the programmed rates used for the 2022 and 2023 ADPs (ODDS tables pull)
programmed_rates_2022 <- data.table(
  ADP = rep(2022, times = 7),
  STRATA = c("EM_HAL", "EM_POT", "EM_TRW", "OB_HAL", "OB_POT", "OB_TRW", "ZERO"),
  SAMPLE_RATE = c(0.3, 0.3, 0.3333, 0.1902, 0.1748, 0.2965, 0)
)
programmed_rates_2022[, FILL := fcase(
  STRATA %like% "OB", "OB", 
  STRATA %like% "EM_TRW", "EM_TRW",
  STRATA %like% "EM_HAL|EM_POT|EM_FIXED", "EM_FG",
  STRATA %like% "ZERO", "ZERO")]

# Define Boxes ---------------------------------------------------------------------------------------------------------

unique(pc_dat$STRATA)
system.time(box_def <- define_boxes_gs(
  pc_dat, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA"), dmn_cols = c("BSAI_GOA", "GEAR"), geom = T, ps_cols = "GEAR"))
box_def$strata_n_dt
# Need rates in a dt with the columns: ADP, STRATA, STRATA_N, SAMPLE_RATE, n, (don't need ISPN, CV_SCALING, INDEX)
# Might not need STRATA_N?

# Check the stratum definitions
unique(pc_dat$STRATA)
box_def$strata_n_dt

#' *HARDCODING PROGRAMMED RATES FOR NOW FROM THE ADP*, but either grab these from the tables or a separate rdata file
#' TODO Join on ODDS_STRATA and ODDS_STRATA_RATES
programmed_rates_2022 <- data.table(
  ADP = rep(2022, times = 7),
  STRATA = c("EM_HAL", "EM_POT", "EM_TRW", "OB_HAL", "OB_POT", "OB_TRW", "ZERO"),
  SAMPLE_RATE = c(0.3, 0.3, 0.3333, 0.1902, 0.1748, 0.2965, 0)
)
programmed_rates_2022[, FILL := fcase(
  STRATA %like% "OB", "OB", 
  STRATA %like% "EM_TRW", "EM_TRW",
  STRATA %like% "EM_HAL|EM_POT|EM_FIXED", "EM_FG",
  STRATA %like% "ZERO", "ZERO")]

box_def$dmn$strata_dt

# Define acceptor/donor lists
OB.acceptor_donor_lst <- c(
  rep(list(4:5), times = 2),                # 1-2: EM_HAL and EM_POT 
  list(3),                                  # 3: EM_TRW
  rep(list(4:5), times = 2),                # 4-5: OB Fixed Gear
  list(6),                                  # 6: OB Trawl                         
  list(4:5)                                 # 7: ZERO           
)
nonOB.acceptor_donor_lst <- c(
  rep(list(1:2), times = 2),                # 1-2: Fixed-gear EM to itself
  rep(list(NULL), times = 5)                # 4-7: No other donors
)

# Expectation assuming we achieved perfect deployment at our programmed rates
interspersion_2022_expecation <-dmn_interspersion_figs(
  box_def = box_def,
  selection_rates = programmed_rates_2022,
  ob_adl = OB.acceptor_donor_lst,
  nonob_adl = nonOB.acceptor_donor_lst
)
interspersion_2022_expecation$DMN_INSP_OB_SMRY$OVERALL
interspersion_2022_expecation$DMN_INSP_OB_SMRY$BSAI_GOA
interspersion_2022_expecation$DMN_INSP_NONOB_SMRY$OVERALL
interspersion_2022_expecation$DMN_INSP_NONOB_SMRY$BSAI_GOA

# stratum and domain level
exp_ob <- interspersion_2022_expecation$DMN_INSP_OB$RAW[, .(INSP_exp = sum(BOX_DMN_w * BOX_DONOR_SAMPLE_PROB)) , keyby = .(ADP, STRATA, BSAI_GOA, GEAR)]
exp_nonob <- interspersion_2022_expecation$DMN_INSP_NONOB$RAW[, .(INSP_exp = sum(BOX_DMN_w * BOX_DONOR_SAMPLE_PROB)) , keyby = .(ADP, STRATA, BSAI_GOA, GEAR)]
exp <- rbind(
  exp_ob[!(STRATA %like% "EM_HAL|EM_POT|ZERO")],
  exp_nonob
)[order(ADP, STRATA, BSAI_GOA, GEAR)]
exp[, STRATA_DMN_N := box_def$dmn$strata_dmn_n_dt[exp, STRATA_DMN_N, on = .(ADP, STRATA, BSAI_GOA, GEAR)]]


#' TODO Create distributions
#' TODO Requires that I make a stochastic version of the interspersion evaluation! 

iter <- 10000
sim_lst <- vector(mode = "list", length = iter)


trips_2022 <- setkey(unique(pc_dat[, .(ADP, STRATA, TRIP_ID)]), "ADP", "STRATA")
trips_2022_N <- trips_2022[, .(N = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA)]
# Merge in programmed rates
trips_2022_N <- programmed_rates_2022[trips_2022_N, on = .(ADP, STRATA)]
trips_2022_fmp_gear <- setkey(unique(pc_dat[, .(ADP, STRATA, TRIP_ID, BSAI_GOA, GEAR)]), "ADP", "STRATA")


box_def$box_smry

set.seed(12345)
for(i in seq_len(iter)){
  # Sample according to sample rates
  sample_lst <- apply(trips_2022_N, 1, function(x) runif(x[["N"]]) < x[["SAMPLE_RATE"]])
  trips_2022_sample <- copy(trips_2022)
  trips_2022_sample$SAMPLE <- unlist(sample_lst)
  
  #' *Now that all trips are sampled, we can decide how we want to evaluate interspersion*
  #' in the ADP, we grouped domains defined by monitoring method, gear type, and BSAI_GOA, allowing trips to neighbor
  #' outside of strata within the monitoring method.
  #' For the ADP, we should start off by keeping the evaluation STRATUM-SPECIFIC, then we can consider if we want to 
  #' use other domains. All we need to change our expectation is building a new 'box definition' with different dmn parameters.
  
  trips_2022_fmp_gear_sample <- trips_2022_sample[trips_2022_fmp_gear, on = .(ADP, STRATA, TRIP_ID)]
  
  #' TODO incorporate the acceptor donor lists!
  
  
  # Identify sampled boxes and neighbors
  box_sample <- copy(box_def$og_data)
  box_sample <- trips_2022_fmp_gear_sample[box_sample, on = .(ADP, STRATA, TRIP_ID)][SAMPLE == T]
  box_sample <- unique(box_sample[, .(ADP, STRATA, TIME, HEX_ID, BOX_ID, BSAI_GOA, GEAR)])
  
  dmn_sample <- split(box_sample, by = c("ADP", "STRATA", "BSAI_GOA", "GEAR"))
  nbr_sample <- lapply(dmn_sample, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ])
  nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
  
  nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA.BSAI_GOA.GEAR")
  nbr_sample_dt[, c("ADP", "STRATA", "BSAI_GOA", "GEAR") := tstrsplit(ADP.STRATA.BSAI_GOA.GEAR, split = "[.]") ]
  nbr_sample_dt[, "ADP.STRATA.BSAI_GOA.GEAR" := NULL]
  nbr_sample_dt[, ADP := as.integer(ADP)]
  
  # Calculate interspersion for strata to themselves
  box_def$dmn$strata_dmn_n_dt
  insp_dt <- box_def$dmn$box_dmn_smry_dt[nbr_sample_dt, on = .(ADP, STRATA, BSAI_GOA, GEAR, BOX_ID)][!is.na(BOX_DMN_w)]
  insp_dt_sum <- insp_dt[, .(INSP_NUM = sum(BOX_DMN_w)) , keyby = .(ADP, STRATA, BSAI_GOA, GEAR)]
  #insp_dt_sum[, STRATA_DMN_N := box_def$dmn$strata_dmn_n_dt[insp_dt_sum, STRATA_DMN_N, on = .(ADP, STRATA, BSAI_GOA, GEAR)]]
  #insp_dt_sum[, INSP := INSP_NUM / STRATA_DMN_N]
  
  
  compare <- exp[insp_dt_sum, on = .(ADP, STRATA, BSAI_GOA, GEAR)]
  compare[, ":=" (EXP = INSP_exp / STRATA_DMN_N, SIM = INSP_NUM / STRATA_DMN_N)]  
  
  #' TODO *I DIDNT DO THIS RIGHT* OB_POT_BSAI_HAL has high expectation (due to crossover with EM_HAL, which I didn't do!)
  #' I didn't apply the acceptor/donor lists by domain only (ignore strata), which answers a slightly different question.
  
  #' *Stratum-level* 
  #' 
  #' 
  
  compare[, DIFF := SIM - EXP]  # everything is less, as expected, if I don't have cross-stratum results.
} 


#' TODO for interspersion, we should really be focusing on interspersion of all trips in the stratum
#' Adding things like gear and fmp (domain) are post-stratum level and solver different questions

#' TODO *GET REALIZED RATES and REALIZED SELECTION*
#' 
#' 
#' TODO Look at distribution of the number of sample neighbors?
#' 
#' 
#' 

#'*--------------------------------------------------------------------------------------------------------------------*

# Stratum-level  (without domain) ---------------------------------------------------------------------------------

#' FIXME If I define boxes without domains, calculate_dmn_interspersion4() doesn't have anything to work with.
#' Technically I shouldnt need to run it via dmn_interspersion_figs because I can get the mean of my distribution,
#' but it would be nice to be able to confirm both answers are the same.
pc_dat <- pc_effort_dt[ADP %in% 2022]   #' TODO *ADD 2023 WHEN WE HAVE IT* I also have the 2023 strata applied here, so em participation might have changed
system.time(box_def <- define_boxes_gs(
  pc_dat, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA"), geom = T))



# simulation 
trips_2022 <- setkey(unique(pc_dat[, .(ADP, STRATA, TRIP_ID)]), "ADP", "STRATA")
trips_2022_N <- trips_2022[, .(N = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA)]
# Merge in programmed rates
trips_2022_N <- programmed_rates_2022[trips_2022_N, on = .(ADP, STRATA)]

iter <- 10000
sim_lst <- vector(mode = "list", length = iter)
set.seed(12345)

for(i in seq_len(iter)){
  
  if(i %% 100 == 0) cat(paste0(i, ", "))
  
  # Sample according to sample rates
  sample_lst <- apply(trips_2022_N, 1, function(x) runif(x[["N"]]) < x[["SAMPLE_RATE"]])
  trips_2022_sample <- copy(trips_2022)
  trips_2022_sample$SAMPLE <- unlist(sample_lst)
  
  #' *Now that all trips are sampled, we can decide how we want to evaluate interspersion*
  #' in the ADP, we grouped domains defined by monitoring method, gear type, and BSAI_GOA, allowing trips to neighbor
  #' outside of strata within the monitoring method.
  #' For the ADP, we should start off by keeping the evaluation STRATUM-SPECIFIC, then we can consider if we want to 
  #' use other domains. All we need to change our expectation is building a new 'box definition' with different dmn parameters.
  
  # Identify sampled boxes and neighbors
  box_sample <- copy(box_def$og_data)
  box_sample <- trips_2022_sample[box_sample, on = .(ADP, STRATA, TRIP_ID)][SAMPLE == T]
  box_sample <- unique(box_sample[, .(ADP, STRATA, TIME, HEX_ID, BOX_ID)])
  
  dmn_sample <- split(box_sample, by = c("ADP", "STRATA"))
  nbr_sample <- lapply(dmn_sample, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ])
  nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
  
  nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
  nbr_sample_dt[, c("ADP", "STRATA") := tstrsplit(ADP.STRATA, split = "[.]") ]
  nbr_sample_dt[, "ADP.STRATA" := NULL]
  nbr_sample_dt[, ADP := as.integer(ADP)]
  
  # Calculate interspersion for strata to themselves
  strata_n <- box_def$dt_out[, .(N = sum(BOX_w)), keyby = .(ADP, STRATA)]
  insp_dt <- box_def$dt_out[nbr_sample_dt, on = .(ADP, STRATA, BOX_ID)][!is.na(BOX_w)]
  insp_dt_sum <- insp_dt[, .(INSP_NUM = sum(BOX_w)) , keyby = .(ADP, STRATA)]
  insp_dt_sum <- insp_dt_sum[strata_n, on = .(ADP, STRATA)]
  insp_dt_sum[, INSP := INSP_NUM / N]
  sim_lst[[i]] <- insp_dt_sum
} 

iter_lst <- rbindlist(sim_lst, idcol = "ITER")
iter_lst[, .(MEAN = mean(INSP)), keyby = .(ADP, STRATA)]  # Here is my 'expected' value, getting mean of distribution

# Not surprising, EM_POT has a wider distribution. Will be interesting to see what we actually got.
ggplot(iter_lst, aes(x = STRATA, y = INSP)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), na.rm = T) + stat_summary(geom = "point", fun = "mean", color = "red")



## Realized 2022 ####

#' *TODO* Be able to do this for all domain definitions!

# Load work.data from the 2024 Final ADP
(load("source_data/2024_Final_ADP_data_ver6.rdata"))
wd_trip_ids <- unique(pc_dat[, .(TRIP_ID, wd_TRIP_ID)])
wd_sub <- work.data[TRIP_ID %in% wd_trip_ids$wd_TRIP_ID]
wd_sub  #' FIXME 6268 ids, short of the 6271 I had. No idea where they went. Oh well
realized_rate <- unique(wd_sub[, .(TRIP_ID, STRATA, OBSERVED_FLAG)])[, .(REALIZED_RATE = sum(OBSERVED_FLAG == "Y")/.N), keyby = .(STRATA)]
wd_sub <- unique(wd_sub[OBSERVED_FLAG == "Y", .(TRIP_ID, STRATA)])
wd_sub[, uniqueN(TRIP_ID), keyby = .(STRATA)]
wd_sub[, wd_TRIP_ID := TRIP_ID][, c("TRIP_ID", "STRATA") := NULL]
wd_sub <- pc_dat[wd_sub, on = .(wd_TRIP_ID)]
wd_sub <- unique(wd_sub[, .(ADP, STRATA, TRIP_ID)])

# Identify sampled boxes and neighbors
box_sample <- copy(box_def$og_data)
box_sample <- box_sample[wd_sub, on = .(ADP, STRATA, TRIP_ID)]
box_sample <- unique(box_sample[, .(ADP, STRATA, TIME, HEX_ID, BOX_ID)])

dmn_sample <- split(box_sample, by = c("ADP", "STRATA"))
nbr_sample <- lapply(dmn_sample, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ])
nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))

nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
nbr_sample_dt[, c("ADP", "STRATA") := tstrsplit(ADP.STRATA, split = "[.]") ]
nbr_sample_dt[, "ADP.STRATA" := NULL]
nbr_sample_dt[, ADP := as.integer(ADP)]

# Calculate interspersion for strata to themselves
strata_n <- box_def$dt_out[, .(N = sum(BOX_w)), keyby = .(ADP, STRATA)]
insp_dt <- box_def$dt_out[nbr_sample_dt, on = .(ADP, STRATA, BOX_ID)][!is.na(BOX_w)]
insp_dt_sum <- insp_dt[, .(INSP_NUM = sum(BOX_w)) , keyby = .(ADP, STRATA)]
insp_dt_sum <- insp_dt_sum[strata_n, on = .(ADP, STRATA)]
insp_dt_sum[, INSP := INSP_NUM / N]
realized <- copy(insp_dt_sum)

# Looks about right. EM_HAL and EM_POT are probably below because of lack of review data?
# OB_HAL is very low on the distribution, likely because of all the cancelled trips in 2022.
ggplot(iter_lst, aes(x = STRATA, y = INSP)) + 
  geom_violin(draw_quantiles = c(0.025, 0.25, 0.5, 0.75), na.rm = T) + 
  stat_summary(geom = "point", fun = "mean", color = "red", na.rm = T) + 
  geom_point(data = realized, color = "blue", size = 3, na.rm = T)
iter_lst[STRATA == "OB_HAL", quantile(INSP, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))] # Lower bound is 0.9378
realized[STRATA == "OB_HAL"] # Realized 0.9300, so outside of the 95% distribution
# everything was below the average.

realized_rate[, STRATA := fcase(
  STRATA %in% c("EM_HAL", "EM_POT", "ZERO"), STRATA,
  STRATA == "EM_TRW_EFP", "EM_TRW",
  STRATA %in% c("HAL" ,"POT", "TRW"), paste0("OB_", STRATA)
)]
programmed_rates_2022[, REALIZED := realized_rate[programmed_rates_2022, REALIZED_RATE, on = .(STRATA)]]
programmed_rates_2022[, DIFF := (REALIZED - SAMPLE_RATE) * 100]
programmed_rates_2022 # OB_TRW was ~2 points below expected, but that's still 27.7%. OB_HAL was 4.45% below
#' TODO *Can we identify where the gaps were?* Try to identify clusters of boxes that were not near sampled neighbors
#'  that were expected to be sampled?


# Here's essentially half of our fishplot. On y < 0, can put the distribution from the realized rate. I would probably
# create the distributions myself like I did with the fishplots in the 2020 AR, instead of using the density function.
# Can also draw the 0.025 and 0.975 quantiles, a little more visible. The EM_TRW distribution
ggplot(iter_lst[STRATA != "ZERO"], aes(x = INSP)) + 
  facet_wrap(STRATA ~ ., scales = "free", drop = T) + 
  geom_density(na.rm = T, fill = "green", alpha = 0.8) + 
  geom_vline(data = realized[STRATA != "ZERO"], aes(xintercept = INSP), color = "blue", na.rm = T, linewidth = 1)
iter_lst[STRATA == "EM_TRW", table(INSP)]
hist(iter_lst[STRATA == "EM_TRW"]$INSP)  # The distribution is trimodal
hist(iter_lst[STRATA == "EM_TRW"]$INSP, breaks = 30)
hist(iter_lst[STRATA == "EM_TRW"]$INSP, breaks = 60)


## Function for creating distributions ---------------------------------------------------------------------------------

# Make sure the rates also have STRATA_N

simulate_interspersion <- function(box_definition, sample_rates, iter, seed) {
  # box_definition <- copy(box_def_fmp); sample_rates <- copy(programmed_rates_2022) ; iter <- 1000; seed <- 12345
  
  year_strata <- unlist(box_definition$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- box_definition$params$dmn_cols
  
  sim_lst <- vector(mode = "list", length = iter)
  
  set.seed(seed)
  for(i in seq_len(iter)){
    
    if(i %% 100 == 0) cat(paste0(i, ", "))
    
    # Sample according to sample rates
    sample_lst <- apply(sample_rates, 1, function(x) runif(x[["STRATA_N"]]) < x[["SAMPLE_RATE"]])
    trips <- setorderv(unique(subset(box_def_fmp$og_data, select = c(year_strata, "TRIP_ID"))), year_strata)
    trips$SAMPLE <- unlist(sample_lst)
    
    #' *Now that all trips are sampled, we can decide how we want to evaluate interspersion*
    #' in the ADP, we grouped domains defined by monitoring method, gear type, and BSAI_GOA, allowing trips to neighbor
    #' outside of strata within the monitoring method.
    #' For the ADP, we should start off by keeping the evaluation STRATUM-SPECIFIC, then we can consider if we want to 
    #' use other domains. All we need to change our expectation is building a new 'box definition' with different dmn parameters.
    
    # Identify which boxes were sampled in each stratum
    box_sample <- copy(box_definition$og_data)
    box_sample <- trips[box_sample, on =  c(year_strata, "TRIP_ID")][SAMPLE == T]
    box_sample <- unique(subset(box_sample, select = c(year_strata, "TIME", "HEX_ID", "BOX_ID")))
    
    dmn_sample <- split(box_sample, by = year_strata)
    nbr_sample <- lapply(dmn_sample, function(x) box_definition$nbr_lst[ x[["BOX_ID"]] ])
    nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
    
    nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
    nbr_sample_dt[, (year_strata) := tstrsplit(ADP.STRATA, split = "[.]") ]
    nbr_sample_dt[, "ADP.STRATA" := NULL]
    nbr_sample_dt[, ADP := as.integer(ADP)]

    # Get subset of boxes that actually contained fishing trips
    boxes_with_trips <- unique(subset(box_definition$og_data, select = c("BOX_ID", year_strata)))
    # Subset boxes neighboring with those with actual fishing
    nbr_sample_dt <- fintersect(nbr_sample_dt, boxes_with_trips) # 994 boxes with fishing didn't have neighboring trips
    
    # Get domain weights of sampled boxes
    dmn_sample_dt <- box_def_fmp$dmn$box_dmn_smry_dt[nbr_sample_dt, on = c(year_strata, "BOX_ID")]
    dmn_sample_dt <- dmn_sample_dt[, .(SUM_DMN_w = sum(BOX_DMN_w, na.rm = T)), keyby = c(year_strata, domains)]
    dmn_sample_dt[, STRATA_DMN_N := box_def_fmp$dmn$strata_dmn_n_dt[dmn_sample_dt, STRATA_DMN_N, on = c(year_strata, domains)]]
    # Calculate interspersion for each domain
    dmn_sample_dt[, INSP := SUM_DMN_w / STRATA_DMN_N]
    sim_lst[[i]] <- dmn_sample_dt
  } 
  
  sim_dt <- rbindlist(sim_lst, idcol = "ITER")
  sim_dt[, .(MEAN = mean(INSP)), keyby = year_strata] 
  # Capture the domains as an attribute
  setattr(sim_dt, "year_strata_domains", c(year_strata, domains))
  
  sim_dt
}


## Split by BSAI and GOA ----------------------------------------------------------------------------------------------

### Distribution from programmed rates ---------------------------------------------------------------------------------
programmed_rates_2022[, STRATA_N := box_def_fmp$strata_n_dt[programmed_rates_2022, STRATA_N, on = .(ADP, STRATA)] ]
sim.programmed <- simulate_interspersion(box_def_fmp, programmed_rates_2022, iter = 1000, seed = 12345)

### Expectation given the programmed rates
programmed_exp_dt <- calculate_dmn_interspersion4(
  box_def = box_def_fmp,
  selection_rates = programmed_rates_2022,
  acceptor_donor_lst = as.list(seq_len(nrow(box_def_fmp$dmn$strata_dt))) # Make each stratum donate only to itself
)
dmn_cols <- unlist(programmed$params, use.names = F)
programmed_exp_smry_dt <- programmed_exp_dt$RAW[
][, .(BOX_DMN_w = sum(BOX_DMN_w), POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)), keyby = dmn_cols]

### Expectation given the realized rates
realized_exp_dt <- calculate_dmn_interspersion4(
  box_def = box_def_fmp,
  selection_rates = realized_rates_2022,
  acceptor_donor_lst = as.list(seq_len(nrow(box_def_fmp$dmn$strata_dt))) # Make each stratum donate only to itself
)
dmn_cols <- unlist(real_exp_dt$params, use.names = F)
real_exp_smry_dt <- real_exp_dt$RAW[
][, .(BOX_DMN_w = sum(BOX_DMN_w), POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)), keyby = dmn_cols]

### Distribution from realized rates -----------------------------------------------------------------------------------

wd_sub <- work.data[TRIP_ID %in% wd_trip_ids$wd_TRIP_ID]
realized_rate <- unique(wd_sub[, .(TRIP_ID, STRATA, OBSERVED_FLAG)])[, .(REALIZED_RATE = sum(OBSERVED_FLAG == "Y")/.N), keyby = .(STRATA)]
realized_rate[, STRATA := fcase(
  STRATA %in% c("EM_HAL", "EM_POT", "ZERO"), STRATA,
  STRATA == "EM_TRW_EFP", "EM_TRW",
  STRATA %in% c("HAL", "POT", "TRW"), paste0("OB_", STRATA)
)]
realized_rate[, ADP := 2022L]
realized_rate[, SAMPLE_RATE := REALIZED_RATE]

realized_rates_2022 <- copy(realized_rate)
realized_rates_2022[, STRATA_N := box_def_fmp$strata_n_dt[realized_rates_2022, STRATA_N, on = .(ADP, STRATA)] ]
sim.realized <- simulate_interspersion(box_def_fmp, realized_rates_2022, iter = 1000, seed = 12345)

### Actually realized interspersion ------------------------------------------------------------------------------------

calulate_realized_interspersion <- function(box_definition, monitored_trips, rates) {
  # box_definition <- copy(box_def_fmp); monitored_trips <- copy(wd_sub);  rates <- copy(realized_rates_2022)
  
  year_strata <- unlist(box_definition$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- unlist(box_definition$params[c("year_col", "stratum_cols", "dmn_cols")], use.names = F)
  
  # Identify sampled boxes and neighbors
  box_sample <- copy(box_definition$og_data)
  box_sample <- box_sample[monitored_trips, on = c(year_strata,  "TRIP_ID")]
  box_sample <- unique(subset(box_sample, select = c(year_strata, "TIME", "HEX_ID", "BOX_ID")))
  
  dmn_sample <- split(box_sample, by = year_strata)
  nbr_sample <- lapply(dmn_sample, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ])
  nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
  
  nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
  nbr_sample_dt[, (year_strata) := tstrsplit(ADP.STRATA, split = "[.]") ]
  nbr_sample_dt[, "ADP.STRATA" := NULL]
  nbr_sample_dt[, ADP := as.integer(ADP)]
  
  strata_domain_n <- box_definition$dmn$strata_dmn_n_dt
  insp_dt <- box_definition$dmn$box_dmn_smry_dt[nbr_sample_dt, on = c(year_strata, "BOX_ID")][!is.na(BOX_DMN_w)]
  insp_dt_sum <- insp_dt[, .(INSP_NUM = sum(BOX_DMN_w)) , keyby = domains]
  insp_dt_sum <- insp_dt_sum[strata_domain_n, on = domains]
  insp_dt_sum[, INSP := INSP_NUM / STRATA_DMN_N]
  
  insp_dt_sum[!is.na(INSP)]
}

realized_interspersion_2022 <- calulate_realized_interspersion(box_def_fmp, wd_sub, realized_rates_2022)


### Plotting distributions ---------------------------------------------------------------------------------------------

ggplot(sim.realized, aes(x = INSP)) + 
  facet_wrap( ~ STRATA + BSAI_GOA, scales = "free", drop = F, dir = "v", nrow = 2) + 
  geom_density(fill = "green", alpha = 0.5, bounds = c(0, 1), outline.type = "both") + 
  geom_segment(data = real_exp_smry_dt[STRATA != "ZERO"], aes(x = POOL_DMN_INTERSPERSION, xend = POOL_DMN_INTERSPERSION, y = 0, yend = Inf), color = "green4") + 
  geom_density(data = sim.programmed, aes(y = -after_stat(density)), fill = "purple", alpha = 0.5, bounds = c(0, 1), outline.type = "both") +
  geom_segment(data =  programmed_exp_smry_dt[STRATA != "ZERO"], aes(x = POOL_DMN_INTERSPERSION, xend = POOL_DMN_INTERSPERSION, y = 0, yend = -Inf), color = "purple4") + 
  geom_vline(data = realized_interspersion_2022, aes(xintercept = INSP), color = "blue", linewidth = 1)
  labs(x = "Interspersion", y = "Density")

# TODO Can I get the 0.025, 0.5 (median), and 0.95 along with the mean and their Y-values from these distributions?
#' https://stackoverflow.com/questions/34029811/fill-different-colors-for-each-quantile-in-geom-density-of-ggplot
#' A reasonably place to start to make my own figures

### Custom Plots -------------------------------------------------------------------------------------------------------
  
domain_tbl <- unique(sim.realized[, .(ADP, STRATA, BSAI_GOA)])
domain_density <- vector(mode = "list", length = nrow(domain_tbl))


for(i in 1:nrow(domain_tbl)) {
  dmn_subset <- sim.realized[domain_tbl[i,], on = .(ADP, STRATA, BSAI_GOA)]
  dmn_quantile <- quantile(dmn_subset$INSP, probs = c(0.025, 0.5, 0.975))
  
  dmn_density <- ggplot_build(
    ggplot(dmn_subset, aes(x = INSP)) + geom_density(bounds = c(0, 1))
  )$data[[1]]
  
  dmn_density <- data.table(domain_tbl[i,], X = dmn_density$x, Y = dmn_density$y)
  dmn_density$quant = factor(findInterval(dmn_density$X, dmn_quantile))
  dmn_density[, FILL := as.factor(fcase(
    quant %in% c(0, 3), "green4",
    quant %in% c(1, 2), "green"
  ))]
  dmn_density[, GROUP := as.factor(fcase(
    quant == 0 , "lower",
    quant %in% c(1, 2), "mid",
    quant == 3, "upper"
  ))]
  domain_density[[i]] <-  dmn_density
}
domain_density <- rbindlist(domain_density)

library(ggh4x)  # facet_grid2 has cleaner facet labels while allowing for independent x and/or y scales

#' https://github.com/tidyverse/ggplot2/issues/4512
a <- ggplot(domain_density, aes(x = X, y = Y)) + 
  facet_grid2(BSAI_GOA ~ STRATA, scales = "free", independent = "all") + 
  geom_ribbon(aes(ymin = 0, ymax = Y, fill = I(FILL), group = interaction(GROUP) ), color = "black", alpha = 0.5, outline.type = "full") + 
  geom_line() + 
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
    panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  labs(x = "Interspersion", y = "Density")


# Now do it for programmed rates
domain_density <- vector(mode = "list", length = nrow(domain_tbl))
for(i in 1:nrow(domain_tbl)) {
  dmn_subset <- sim.programmed[domain_tbl[i,], on = .(ADP, STRATA, BSAI_GOA)]
  dmn_quantile <- quantile(dmn_subset$INSP, probs = c(0.025, 0.5, 0.975))
  
  dmn_density <- ggplot_build(
    ggplot(dmn_subset, aes(x = INSP)) + geom_density(bounds = c(0, 1))
  )$data[[1]]
  
  dmn_density <- data.table(domain_tbl[i,], X = dmn_density$x, Y = dmn_density$y)
  dmn_density$quant = factor(findInterval(dmn_density$X, dmn_quantile))
  dmn_density[, FILL := as.factor(fcase(
    quant %in% c(0, 3), "purple4",
    quant %in% c(1, 2), "purple"
  ))]
  dmn_density[, GROUP := as.factor(fcase(
    quant == 0 , "lower",
    quant %in% c(1, 2), "mid",
    quant == 3, "upper"
  ))]
  domain_density[[i]] <-  dmn_density
}
domain_density <- rbindlist(domain_density)

# i still really hate how the different parts of the ribbon have a white line
a + 
  geom_ribbon(data = domain_density, aes(ymin = 0, ymax = -Y, fill = FILL, group = GROUP), color = "black", alpha = 0.5, outline.type = "full") + 
  geom_line(data = domain_density, aes(y = -Y)) + 
  # Realized
  geom_vline(data = realized_interspersion_2022, aes(xintercept = INSP), color = "blue", linewidth = 1) + 
  geom_hline(yintercept = 0) +
  scale_fill_identity(
    name = "Distribution",
    guide = "legend",
    breaks = c("green", "purple"),
    labels = c('Assuming Realized Rates', "Assuming Programmed Rates")) +
  theme(legend.position = "bottom")


#' TODO Can I center y=0 on all the plots? hmmm
#' If i plot as polygons I can probably make it look cleaner and center them by scaling them.

b <- copy(domain_density)
b1 <- b[, rbind(.SD[X == min(X)], .SD, .SD[X == max(X)]), keyby = .(ADP, STRATA, BSAI_GOA, GROUP, FILL, quant)]
b2 <- b1[, .SD[, .(X, Y = replace(Y, list = c(1, .N), c(0,0)))], keyby = .(ADP, STRATA, BSAI_GOA, GROUP, FILL, quant)]

ggplot(b2) +
  facet_grid2(BSAI_GOA ~ STRATA, scales = "free", independent = "all") + 
  geom_polygon(aes(x=X, y=Y, group = quant, fill = I(FILL)), color = "black")
# Some lines are thicker because theyre being drawn on separate pixels... If I make each group use the next X value I
# can avoid this issue...

#' TODO Just use the outputs from geom_ribbon of the densities to get Y=0 for each group, but then re-assign the final
#' max(x) values of each group to be next group?


domain_density <- vector(mode = "list", length = nrow(domain_tbl))
for(i in 1:nrow(domain_tbl)) {
  
  dmn_subset <- sim.programmed[domain_tbl[i,], on = .(ADP, STRATA, BSAI_GOA)]
  dmn_quantile <- quantile(dmn_subset$INSP, probs = c(0.025, 0.975))
  dmn_median <- quantile(dmn_subset$INSP, probs = 0.5)
  
  # Run geom_density and extract the result
  dmn_density <- ggplot_build(
    ggplot(dmn_subset, aes(x = INSP)) + geom_density(bounds = c(0, 1))
  )$data[[1]]
  dmn_density <- data.table(domain_tbl[i,], X = dmn_density$x, Y = dmn_density$y)
  
  # Identify the quantiles and assign them to each datapoint
  dmn_density$quant = factor(findInterval(dmn_density$X, dmn_quantile))
  # Define plotting fill colors and plotting groups
  dmn_density[, FILL := as.factor(fcase(quant %in% c(0, 2), "purple4", quant == 1, "purple"))]

  # Extract the cutoffs of each group
  cutoffs <- dmn_density[, .SD[c(1, .N),], keyby = .(ADP, STRATA, BSAI_GOA, quant, FILL)]
  # For the lower , add the x values of the tails 0 and 3
  quant_1.min <- cutoffs[quant == 0][.N][, ':=' (FILL = "purple", quant = 1)][]
  quant_1.max <- cutoffs[quant == 2][1][, ':=' (FILL = "purple", quant = 1)][]

  # Identify the median and split the middle.
  quant_1 <- rbind(quant_1.min, dmn_density[quant == 1], quant_1.max)
  quant_1$GROUP <- findInterval(quant_1$X, dmn_median) + 1
  quant_1_g2 <- copy(quant_1[GROUP == 1][.N])
  quant_1_g2[, GROUP := 2]
  quant_1 <- setorder(rbind(quant_1, quant_1_g2), X, GROUP)
  
  quant_0 <- dmn_density[quant == 0][, GROUP := 0][]
  quant_2 <- dmn_density[quant == 2][, GROUP := 3][]
  
  # Put it all back together
  dmn_density <- rbind(quant_0, quant_1, quant_2)
  
  domain_density[[i]] <-  dmn_density
}
domain_density <- rbindlist(domain_density)
domain_density[, GROUP := as.factor(GROUP)]

# TODO Add domain attributes to sim_res

calculate_density <- function(sim_res, fill_color) {
  # sim_res <- copy(sim.realized); fill_color <- "green"
  
  year_strata_domains <- attr(sim_res, "year_strata_domains")
  domain_tbl <- setorderv(unique(subset(sim_res, select = year_strata_domains)), year_strata_domains)
  tail_color <- paste0(fill_color, "4")
  
  domain_density <- vector(mode = "list", length = nrow(domain_tbl))
  for(i in 1:nrow(domain_tbl)) {
    
    dmn_subset <- sim_res[domain_tbl[i,], on = .(ADP, STRATA, BSAI_GOA)]
    dmn_quantile <- quantile(dmn_subset$INSP, probs = c(0.025, 0.975))
    dmn_median <- quantile(dmn_subset$INSP, probs = 0.5)
    
    # Run geom_density and extract the result
    dmn_density <- ggplot_build(
      ggplot(dmn_subset, aes(x = INSP)) + geom_density(bounds = c(0, 1))
    )$data[[1]]
    dmn_density <- data.table(domain_tbl[i,], X = dmn_density$x, Y = dmn_density$y)
    
    # Identify the quantiles and assign them to each datapoint
    dmn_density$quant = factor(findInterval(dmn_density$X, dmn_quantile))
    # Define plotting fill colors and plotting groups
    dmn_density[, FILL := as.factor(fcase(quant %in% c(0, 2), tail_color, quant == 1, fill_color))]
    
    # Extract the cutoffs of each group
    cutoffs <- dmn_density[, .SD[c(1, .N),], keyby = .(ADP, STRATA, BSAI_GOA, quant, FILL)]
    # For the lower , add the x values of the tails 0 and 3
    quant_1.min <- cutoffs[quant == 0][.N][, ':=' (FILL = fill_color, quant = 1)][]
    quant_1.max <- cutoffs[quant == 2][1][, ':=' (FILL = fill_color, quant = 1)][]
    
    # Identify the median and split the middle.
    quant_1 <- rbind(quant_1.min, dmn_density[quant == 1], quant_1.max)
    quant_1$GROUP <- findInterval(quant_1$X, dmn_median) + 1
    quant_1_g2 <- copy(quant_1[GROUP == 1][.N])
    quant_1_g2[, GROUP := 2]
    quant_1 <- setorder(rbind(quant_1, quant_1_g2), X, GROUP)
    
    quant_0 <- dmn_density[quant == 0][, GROUP := 0][]
    quant_2 <- dmn_density[quant == 2][, GROUP := 3][]
    
    # Put it all back together
    dmn_density <- rbind(quant_0, quant_1, quant_2)
    
    domain_density[[i]] <-  dmn_density
  }
  domain_density <- rbindlist(domain_density)
  domain_density[, GROUP := as.factor(GROUP)]
  
  setattr(domain_density, "year_strata_domains", year_strata_domains)
  domain_density
}

realized.density <- calculate_density(sim.realized, "green")
programmed.density <- calculate_density(sim.programmed, "dodgerblue")

# TODO Annotate the Trip counts to each panel. This will be trivial once all y-axes are scaled and centered
ggplot(realized.density, aes(x = X, fill = I(FILL))) + 
  facet_grid2(BSAI_GOA ~ STRATA, scales = "free", independent = "all") + 
  geom_hline(yintercept = 0, color = "gray") + 
  # realized density
  geom_ribbon(aes(ymin = 0, ymax = Y, group = GROUP), color = "black", outline.type = "full") + 
  # programmed density
  geom_ribbon(data = programmed.density, aes(ymin = 0, ymax = -Y, group = GROUP), color = "black", outline.type = "full") + 
  geom_vline(data = realized_interspersion_2022, aes(xintercept = INSP), color = "blue", linewidth = 1, linetype = 2) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  labs(x = "Interspersion", y = "Density") 

# Combine both datasets, scaling them so that y-axis is centered
combine_distributions <- function(realized, programmed) {
  # realized <- copy(realized.density); programmed <- copy(programmed.density)
  
  year_strata_domains <- attr(realized, "year_strata_domains")
  
  programmed_flipped <- copy(programmed)
  programmed_flipped[, Y := -Y]
  dist_dat <- rbind(cbind(DIST = "REAL", realized), cbind(DIST = "PROG", programmed_flipped))
  
  dist_dat[, MAX_ABS := max(abs(Y)), keyby = year_strata_domains]
  dist_dat[, Y := Y/MAX_ABS]
  setattr(dist_dat, "year_strata_domains", year_strata_domains)
  dist_dat
}

distributions.fmp <- combine_distributions(realized.density, programmed.density)
# Make labels for domain trip counts
dist_x <- distributions.fmp[, .SD[1,], keyby = c(attr(distributions.fmp, "year_strata_domains")) ]
dmn_N <- realized_interspersion_2022[dist_x, on = attr(distributions.fmp, "year_strata_domains")]
dmn_N[, X := pmin(X, INSP)]
dmn_N[, STRATA_DMN_N := formatC(round(STRATA_DMN_N), width = max(nchar(round(STRATA_DMN_N))))]

ggplot(distributions.fmp , aes(x = X)) + 
  facet_grid2(BSAI_GOA ~ STRATA, render_empty = F, scales = "free", independent = "all") +   #' for free scales remove both 'free' and 'independent'
  geom_hline(yintercept = 0, color = "gray") + 
  geom_ribbon(aes(ymin = 0, ymax = Y, fill = I(FILL), group = interaction(DIST, GROUP)), color = "black", outline.type = "full", alpha = 0.8) + 
  geom_vline(data = realized_interspersion_2022, aes(xintercept = INSP, linetype = "Actually Realized"), color = "purple", linewidth = 0.75) + 
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank() ) + 
  ylim(c(-1, 1)) + 
  labs(x = "Interspersion", y = "Density") +  
  scale_fill_identity(
    name = "Monitoring Rate", guide = "legend",
    breaks = c("green", "dodgerblue"), labels = c('Realized', "Programmed") ) +
  scale_linetype_manual(name = NULL, values = 2 ) +
  theme(legend.position = "bottom", legend.key = element_rect(fill = "white")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  geom_text(data = dmn_N, aes(label = STRATA_DMN_N, x = X, y = 1), hjust = 0, vjust = 1, size = 3.5)
#' TODO If we want to include year, can maybe use facet_nested, which also has the 'independent' argument



## Realized Rates Distribution [OLD] -------------------------------------------------------------------------------------

realized_rate
realized_trips <- copy(trips_2022_N)
realized_trips[, SAMPLE_RATE := realized_rate[realized_trips, REALIZED_RATE, on = .(STRATA)]]
real_lst <- vector(mode = "list", length = iter)
set.seed(12345)

for(i in seq_len(iter)){
  
  if(i %% 100 == 0) cat(paste0(i, ", "))
  
  # Sample according to sample rates
  sample_lst <- apply(realized_trips, 1, function(x) runif(x[["N"]]) < x[["SAMPLE_RATE"]])
  trips_2022_sample <- copy(trips_2022)
  trips_2022_sample$SAMPLE <- unlist(sample_lst)
  
  #' *Now that all trips are sampled, we can decide how we want to evaluate interspersion*
  #' in the ADP, we grouped domains defined by monitoring method, gear type, and BSAI_GOA, allowing trips to neighbor
  #' outside of strata within the monitoring method.
  #' For the ADP, we should start off by keeping the evaluation STRATUM-SPECIFIC, then we can consider if we want to 
  #' use other domains. All we need to change our expectation is building a new 'box definition' with different dmn parameters.
  
  # Identify sampled boxes and neighbors
  box_sample <- copy(box_def$og_data)
  box_sample <- trips_2022_sample[box_sample, on = .(ADP, STRATA, TRIP_ID)][SAMPLE == T]
  box_sample <- unique(box_sample[, .(ADP, STRATA, TIME, HEX_ID, BOX_ID)])
  
  dmn_sample <- split(box_sample, by = c("ADP", "STRATA"))
  nbr_sample <- lapply(dmn_sample, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ])
  nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
  
  nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
  nbr_sample_dt[, c("ADP", "STRATA") := tstrsplit(ADP.STRATA, split = "[.]") ]
  nbr_sample_dt[, "ADP.STRATA" := NULL]
  nbr_sample_dt[, ADP := as.integer(ADP)]
  
  # Calculate interspersion for strata to themselves
  strata_n <- box_def$dt_out[, .(N = sum(BOX_w)), keyby = .(ADP, STRATA)]
  insp_dt <- box_def$dt_out[nbr_sample_dt, on = .(ADP, STRATA, BOX_ID)][!is.na(BOX_w)]
  insp_dt_sum <- insp_dt[, .(INSP_NUM = sum(BOX_w)) , keyby = .(ADP, STRATA)]
  insp_dt_sum <- insp_dt_sum[strata_n, on = .(ADP, STRATA)]
  insp_dt_sum[, INSP := INSP_NUM / N]
  real_lst[[i]] <- insp_dt_sum
} 

real_dt <- rbindlist(real_lst, idcol = "ITER")
real_dt[, .(MEAN = mean(INSP)), keyby = .(ADP, STRATA)]  # Here is my 'expected' value, getting mean of distribution

# Here, purple is the expected given the programmed rates, green is the expected given the realized rates 
ggplot(real_dt[STRATA != "ZERO"], aes(x = INSP)) + 
  facet_wrap(STRATA ~ ., scales = "free", drop = T) + 
  geom_density(na.rm = T, fill = "green", alpha = 0.5) + 
  geom_density(data = iter_lst[STRATA != "ZERO"], aes(x = INSP), na.rm = T, fill = "purple", alpha= 0.5) + 
  geom_vline(data = realized[STRATA != "ZERO"], aes(xintercept = INSP), color = "blue", na.rm = T, linewidth = 1)
#' TODO *Even with the realized rates EM_HAL and EM_POT are still WAY below the expected distribution.* Could be because
#' there is a huge chunk at the end of the year that wasn't reviewed?
#' When given the realized rates, OB_HAL falls squarely in its expected distribution. 



pc_dat[wd_sub[, .(TRIP_ID)], on = .()]
wd_sub 


#'*--------------------------------------------------------------------------------------------------------------------*

# By stratum Only (NEW) ------------------------------------------------------------------------------------------------

system.time(box_def <- define_boxes_gs(
  pc_dat, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA"), geom = T))



# Split by BSAI/GOA -----------------------------------------------------------------------------------------------
# Again, not crossing strata, but we do allow neighboring of trips within strata that span across BSAI/GOA HEX_IDs

pc_dat <- pc_effort_dt[ADP %in% 2022]   #' TODO *ADD 2023 WHEN WE HAVE IT* I also have the 2023 strata applied here, so em participation might have changed
system.time(box_def_fmp <- define_boxes_gs(
  pc_dat, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = c("STRATA"), dmn_lst = list(nst = NULL, st = "BSAI_GOA"), geom = T))

#' TODO *put this in the same simulation* as for stratum-only. grab the additional columns as needed.

trips_2022 <- setkey(unique(pc_dat[, .(ADP, STRATA, BSAI_GOA, TRIP_ID)]), "ADP", "STRATA")
trips_2022_N <- trips_2022[, .(N = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA)]
# Merge in programmed rates
trips_2022_N <- programmed_rates_2022[trips_2022_N, on = .(ADP, STRATA)]

## Simulation of realized rate -----------------------------------------------------------------------------------------
realized_rate
realized_trips <- copy(trips_2022_N)
realized_trips[, SAMPLE_RATE := realized_rate[realized_trips, REALIZED_RATE, on = .(ADP, STRATA)]]

iter <- 10000
real_lst <- vector(mode = "list", length = iter)

set.seed(12345)
for(i in seq_len(iter)){

  #' *TODO* box_def_fmp$params  # USE dmn_cols!!!
  
  
  if(i %% 100 == 0) cat(paste0(i, ", "))
  
  # Sample according to sample rates
  sample_lst <- apply(realized_trips, 1, function(x) runif(x[["N"]]) < x[["SAMPLE_RATE"]])
  trips_2022_sample <- copy(trips_2022)
  trips_2022_sample$SAMPLE <- unlist(sample_lst)
  
  #' *Now that all trips are sampled, we can decide how we want to evaluate interspersion*
  #' in the ADP, we grouped domains defined by monitoring method, gear type, and BSAI_GOA, allowing trips to neighbor
  #' outside of strata within the monitoring method.
  #' For the ADP, we should start off by keeping the evaluation STRATUM-SPECIFIC, then we can consider if we want to 
  #' use other domains. All we need to change our expectation is building a new 'box definition' with different dmn parameters.
  
  # TODO If DMN is null, just treat all trips in the same domain so I can conistently use the dmn output?
  
  # Identify sampled boxes and neighbors
  if(F){
    box_def_fmp$dmn$box_dmn_smry_dt   # this contains domain information.
    box_def$og_data                   # this tells which box each TRIP_ID belongs to
    box_def$nbr_lst                   # list of each box with its neighboring boxes, max of 21
  }

  # Identify which boxes were sampled in each stratum
  box_sample <- copy(box_def$og_data)
  box_sample <- trips_2022_sample[box_sample, on = .(ADP, STRATA, TRIP_ID)][SAMPLE == T]
  box_sample <- unique(box_sample[, .(ADP, STRATA, TIME, HEX_ID, BOX_ID)])
  
  # Identify which boxes were sampled or neighboring sampled boxes
  dmn_sample <- split(box_sample, by = c("ADP", "STRATA"))
  nbr_sample <- lapply(dmn_sample, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ])
  nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
  # Make a data.frame for each ADP x stratum with the list of boxes sampled or neighboring a sampled box
  nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
  nbr_sample_dt[, c("ADP", "STRATA") := tstrsplit(ADP.STRATA, split = "[.]") ]
  nbr_sample_dt[, "ADP.STRATA" := NULL]
  nbr_sample_dt[, ADP := as.integer(ADP)]
  
  # Get subset of boxes that actually contained fishing trips
  boxes_with_trips <- unique(copy(box_def$og_data)[, .(BOX_ID, ADP, STRATA)])  # 3527 strata x box_ids with fishing effort
  # Subset boxes neighboring with those with actual fishing
  nbr_sample_dt <- fintersect(nbr_sample_dt, boxes_with_trips) # 994 boxes with fishing didn't have neighboring trips

  # Get domain weights of sampled boxes
  dmn_sample_dt <- box_def_fmp$dmn$box_dmn_smry_dt[nbr_sample_dt, on = .(ADP, STRATA, BOX_ID)]
  dmn_sample_dt <- dmn_sample_dt[, .(SUM_DMN_w = sum(BOX_DMN_w, na.rm = T)), keyby = .(ADP, STRATA, BSAI_GOA)]
  dmn_sample_dt[, STRATA_DMN_N := box_def_fmp$dmn$strata_dmn_n_dt[dmn_sample_dt, STRATA_DMN_N, on = .(ADP, STRATA, BSAI_GOA)]]
  # Calculate interspersion for each domain
  dmn_sample_dt[, INSP := SUM_DMN_w / STRATA_DMN_N]
  real_lst[[i]] <- dmn_sample_dt
} 
real_fmp_dt <- rbindlist(real_lst, idcol = "ITER")[!is.na(INSP)]
real_fmp_dt_mean <- real_fmp_dt[, .(MEAN = mean(INSP)), keyby = .(ADP, STRATA, BSAI_GOA)]  # Here is my 'expected' value, getting mean of distribution
range(real_fmp_dt_mean$MEAN)

## Realized split by FMP 

# Expectation assuming we achieved perfect deployment at our programmed rates
real_exp_dt <- calculate_dmn_interspersion4(
  box_def = box_def_fmp,
  selection_rates = realized_rate,
  acceptor_donor_lst = as.list(seq_len(nrow(box_def_fmp$dmn$strata_dt))) # Make each stratum donate only to itself
)
dmn_cols <- unlist(real_exp_dt$params, use.names = F)
real_exp_smry_dt <- real_exp_dt$RAW[
][, .(BOX_DMN_w = sum(BOX_DMN_w), POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)), keyby = dmn_cols]

# Identify sampled boxes and neighbors when split by BSAI_GIA
box_sample.bsai_goa <- copy(box_def_fmp$og_data)
box_sample.bsai_goa <- box_sample.bsai_goa[wd_sub, on = .(ADP, STRATA, TRIP_ID)]
box_sample.bsai_goa <- unique(box_sample.bsai_goa[, .(ADP, STRATA, TIME, HEX_ID, BOX_ID)])

dmn_sample.bsai_goa <- split(box_sample.bsai_goa, by = c("ADP", "STRATA"))
nbr_sample.bsai_goa <- lapply(dmn_sample.bsai_goa, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ])
nbr_sample.bsai_goa <- lapply(nbr_sample.bsai_goa, function(x) unique(unlist(x)))

nbr_sample_dt.bsai_goa <- rbindlist(lapply(nbr_sample.bsai_goa, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
nbr_sample_dt.bsai_goa[, c("ADP", "STRATA") := tstrsplit(ADP.STRATA, split = "[.]") ]
nbr_sample_dt.bsai_goa[, "ADP.STRATA" := NULL]
nbr_sample_dt.bsai_goa[, ADP := as.integer(ADP)]
nbr_sample_dt.bsai_goa

# Calculate realized interspersion for each domain
strata_n.bsai_goa <- box_def_fmp$dmn$strata_dmn_n_dt
insp_dt.bsai_goa <- box_def_fmp$dmn$box_dmn_smry_dt[nbr_sample_dt.bsai_goa, on = .(ADP, STRATA, BOX_ID)][
][, .(INSP_NUM = sum(BOX_DMN_w)) , keyby = .(ADP, STRATA, BSAI_GOA)]
insp_dt_sum.bsai_goa <- insp_dt_sum.bsai_goa[strata_n.bsai_goa, on = .(ADP, STRATA, BSAI_GOA)]
insp_dt_sum.bsai_goa[, INSP := INSP_NUM / STRATA_DMN_N]
realized.bsai_goa <- copy(insp_dt_sum.bsai_goa)

realized.bsai_goa


## Plot Realized vs expected
test.realized <- ggplot(real_fmp_dt, aes(x = INSP)) + 
  facet_wrap(ADP + STRATA ~ BSAI_GOA, scales = "free", nrow = 2, drop = F, dir = "v") + 
  # Simulated Distribution given realized rates
  geom_density(aes(fill = "green"), alpha = 0.5, bounds = c(0, 1.00001)) + 
  theme(strip.text.x = element_text(margin = margin(0,0,0,0, unit = "pt"))) +
  # Mean of simulated distribution given realized rates
  geom_vline(data = real_fmp_dt_mean, aes(xintercept = MEAN), color = "green3", linetype = 2) +
  # Expectation from Realized rates
  geom_vline(data = real_exp_smry_dt[STRATA != "ZERO"], aes(xintercept = POOL_DMN_INTERSPERSION, color = "green4"), linewidth = 1, show.legend = T) + 
  # Actually realized interspersion in 2022
  geom_vline(data = realized.bsai_goa[STRATA != "ZERO"], aes(xintercept = INSP, color = "dodgerblue"), linewidth = 1, show.legend = T) + 
  labs(y = "density", x = "Interspersion") +
  scale_color_identity(
    name = "Result",
    guide = "legend",
    breaks = c("green4", "dodgerblue"),
    labels = c('Expectation assuming Realized', 'Actually Realized')) +
  scale_fill_identity(
    name = "Distribution",
    guide = "legend",
    breaks = c("green"),
    labels = c('Assuming Realized Rates')) +
  theme(legend.position = "bottom")
#' *Only the EM strata were outside of the expected distribution assuming the realized rates, both in the GOA*. 
#' Assuming my data isn't just out of date and there isn't a huge temporal affect from having no reviewed trips at the end of the year?
#' TODO When/Where did I have data gaps?


# Adding in programmed
ggplot(real_fmp_dt, aes(x = INSP)) + 
  facet_wrap(ADP + STRATA ~ BSAI_GOA, scales = "free", nrow = 2, drop = F, dir = "v") + 
  # Simulated Distribution given realized rates
  geom_density(aes(fill = "green"), alpha = 0.5, bounds = c(0, 1.00001)) + 
  theme(strip.text.x = element_text(margin = margin(0,0,0,0, unit = "pt")))



# Locked scales
ggplot(real_fmp_dt, aes(x = INSP)) + 
  facet_wrap(ADP + STRATA ~ BSAI_GOA, scales = "free_y", nrow = 2, drop = F, dir = "v") + 
  # Simulated Distribution given realized rates
  geom_density(aes(fill = "green"), alpha = 0.5, bounds = c(0, 1.00001)) + 
  theme(strip.text.x = element_text(margin = margin(0,0,0,0, unit = "pt"))) +
  # Mean of simulated distribution given realized rates
  geom_vline(data = real_fmp_dt_mean, aes(xintercept = MEAN), color = "green3", linetype = 2) +
  # Expectation from Realized rates
  geom_vline(data = real_exp_smry_dt[STRATA != "ZERO"], aes(xintercept = POOL_DMN_INTERSPERSION, color = "green4"), linewidth = 1, show.legend = T) + 
  # Actually realized interspersion in 2022
  geom_vline(data = realized.bsai_goa[STRATA != "ZERO"], aes(xintercept = INSP, color = "dodgerblue"), linewidth = 1, show.legend = T) + 
  labs(y = "density", x = "Interspersion") +
  scale_color_identity(
    name = "Result",
    guide = "legend",
    breaks = c("green4", "dodgerblue"),
    labels = c('Expectation assuming Realized', 'Actually Realized')) +
  scale_fill_identity(
    name = "Distribution",
    guide = "legend",
    breaks = c("green"),
    labels = c('Assuming Realized Rates')) +
  theme(legend.position = "bottom")
# Note that in the BSAI the expected ranges are very wide. In 2024, by controlling against low sample sizes, higher rates will be set
# and the expectation will be much higher.

box_def_fmp$dmn$strata_dmn_n_dt


## Look into EM POT in the GOA -----------------------------------------------------------------------------------------

# Subset EM_POT_GOA (296 trips)
em_pot.bsai_goa <- box_def_fmp$dmn$geom_dmn_df %>% dplyr::filter(STRATA == "EM_POT" & BOX_DMN_n > 0)
# Extract the spatial extent of the fishing effort
em_pot.bsai_goa.bbox <- em_pot.bsai_goa %>% st_bbox()

# Subset boxes that were sampled (realized)
em_pot_bsai_goa.sampled <- merge(em_pot.bsai_goa, nbr_sample_dt.bsai_goa[STRATA == "EM_POT"], by = c("ADP", "STRATA", "BOX_ID"))
# And those not near a sampled neighbor
em_pot_bsai_goa.gaps <- setdiff(em_pot.bsai_goa, em_pot_bsai_goa.sampled)
# Plot boxes that didnt have a sample in neighboring boxes with a red outline
test.em_pot.bsai <- ggplot(em_pot.bsai_goa) + 
  #geom_sf(data = shp_nmfs %>% sf::st_set_crs(st_crs(3467))) + 
  geom_sf(data = ak_low_res %>% sf::st_set_crs(st_crs(3467))) +
  geom_sf(aes(fill = BOX_DMN_w)) + 
  facet_wrap(~TIME) + 
  geom_sf(data = em_pot_bsai_goa.gaps, color = "red", alpha = 0, linewidth = 1) + 
  scale_fill_viridis_c() + 
  coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)])
# Definitely seems in the latter half of the year, lots of GOA trips were not sampled.



## Look into EM HAL in the GOA -----------------------------------------------------------------------------------------


box_def_fmp$dmn$strata_dmn_n_dt[STRATA == "EM_HAL"]  # Only 32 trips in the BSAI, 690 in the GOA
# Subset EM_POT_GOA (296 trips)
em_hal.bsai_goa <- box_def_fmp$dmn$geom_dmn_df %>% dplyr::filter(STRATA == "EM_HAL" & BOX_DMN_n > 0)
# Extract the spatial extent of the fishing effort
em_hal.bsai_goa.bbox <- em_hal.bsai_goa %>% st_bbox()

# Subset boxes that were sampled (realized)
em_hal_bsai_goa.sampled <- merge(em_hal.bsai_goa, nbr_sample_dt.bsai_goa[STRATA == "EM_HAL"], by = c("ADP", "STRATA", "BOX_ID"))
# And those not near a sampled neighbor
em_hal_bsai_goa.gaps <- setdiff(em_hal.bsai_goa, em_hal_bsai_goa.sampled)
# Plot boxes that didnt have a sample in neighboring boxes with a red outline
test.em_hal.bsai <- ggplot(em_hal.bsai_goa) + 
  #geom_sf(data = shp_nmfs %>% sf::st_set_crs(st_crs(3467))) + 
  geom_sf(data = ak_low_res %>% sf::st_set_crs(st_crs(3467))) +
  geom_sf(aes(fill = BOX_DMN_w)) + 
  facet_wrap(~TIME) + 
  geom_sf(data = em_hal_bsai_goa.gaps, color = "red", alpha = 0, linewidth = 1) + 
  scale_fill_viridis_c() + 
  coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)])
# Definitely seems during the last quarter of the year, lots of GOA trips were not sampled.
# Also seems like the BSAI was missed in the latter half, but apparently this was not too far from expected? Wide distribution, achieved 0.5, expected 0.675


#' Looks like the distribution and expectation are pretty spot-on
compare_exp_vs_realized_dist <- real_fmp_dt_mean[real_exp_smry_dt[STRATA != "ZERO"], on = .(ADP, STRATA, BSAI_GOA)]
compare_exp_vs_realized_dist[, DIFF := MEAN - POOL_DMN_INTERSPERSION]
compare_exp_vs_realized_dist  # all are within three-hundredths of a point of not less. Smaller strata are a bit more variable

# Save figures
ggsave("test.realized.png", path = "figures/", plot = test.realized, width = 10, height = 5.5, units = "in")
ggsave("test.em_pot.bsai.png", path = "figures/", plot = test.em_pot.bsai, width = 10, height = 7.5, units = "in")
ggsave("test.em_hal.bsai.png", path = "figures/", plot = test.em_hal.bsai, width = 10, height = 7.5, units = "in")



#' TODO Can we look at a time series of expected interspersion?
#' 
#' 
#' 
#' 
#' 
#' 


#=======================#
# ODDS for 2022-2023 ---------------------------------------------------------------------------------------------------
#=======================#

#' Note: Geoff has a odds_cancellation_investigation.R script in his local on the ADP GitHub repo with some figures for
#' looking at ODDS as a time series. Could be useful to investiate deviations from expected rates.

library(odbc)
channel <- dbConnect(
  odbc::odbc(), "AFSC", 
  UID = rstudioapi::askForPassword("Database user"),  PWD = rstudioapi::askForPassword("Database password")
)

#' I can't get the script from 1_AR_data.R to run : *"NORPAC"."ODDS_RANDOM_NUMBER"."GETCONTROLPCT": invalid identifier*
# Pull ODDS data from the ODDS schema
odds_pull <- setDT(dbGetQuery(channel, paste(
  "
  SELECT 
    a.trip_plan_log_seq, a.trip_status_code, a.date_logged, a.vessel_seq, a.cancel_date_time, 
    b.trip_stratas_seq, b.trip_monitor_code AS TRIP_MONITOR_CODE, b.trip_selected,  b.random_number_used, b.strata, b.strata_group_seq,
    c.group_code, c.gear_typecode, c.description AS STRATUM_DESCRIPTION,
    d.release_seq, d.release_request_date_time, d.release_status_seq, d.release_comment, d.release_response_comment, d.release_reason_seq,
    e.release_status_code, e.description AS RELEASE_STATUS_DESCRIPTION,
    f.release_reason_descript,
    g.trip_plan_log_seq AS INHERIT_TRIP_SEQ,
    h.description AS RELEASE_STATUS
   
  FROM odds.odds_trip_plan_log a
    LEFT JOIN odds.odds_trip_stratas b
      ON a.trip_plan_log_seq = b.trip_plan_log_seq 
    LEFT JOIN odds.odds_strata c
      ON b.strata = c.strata
    LEFT JOIN odds.odds_strata_release d
      ON b.trip_stratas_seq = d.trip_stratas_seq
    LEFT JOIN odds.odds_lov_release_status e
      ON d.release_status_seq = e.release_status_seq
    LEFT JOIN odds.odds_lov_release_reason f
      ON d.release_reason_seq = f.release_reason_seq
    LEFT JOIN odds.odds_trip_stratas g
      ON b.inherit_trip_seq = g.trip_stratas_seq
    LEFT JOIN odds.odds_lov_release_status h
      ON d.release_status_seq = h.release_status_seq
  WHERE extract(YEAR FROM a.date_logged) >= 2020
  "
)))

# Assign stratum 
odds_pull_stratum <- unique(odds_pull[, .(TRIP_PLAN_LOG_SEQ, STRATA, GEAR_TYPECODE, GROUP_CODE, STRATUM_DESCRIPTION)])
unique(odds_pull_stratum[, .(STRATA, GROUP_CODE, STRATUM_DESCRIPTION, GEAR_TYPECODE)])[order(STRATA, GROUP_CODE)]
odds_pull_stratum[, STRATUM := fcase(
  STRATA %in% 11:19, fcase(
    GEAR_TYPECODE == 3, "OB_TRW",
    GEAR_TYPECODE == 6, "OB_POT",
    GEAR_TYPECODE == 8, "OB_HAL"),
  STRATA %in% 20:23, fcase(
    GEAR_TYPECODE == 6, "EM_POT",
    GEAR_TYPECODE == 8, "EM_HAL"),
  STRATA %in% 25:26, "EM_TRW",
  STRATA %in% 96:97, "OB_Multi_Area_IFQ",
  STRATA %in% 98, "EM_Multi_Area_IFQ")]         # In 1_AR_data.R, we split EM_Multi_Area_IFQ into the gear-based strata
# Merge in stratum
odds_pull[, STRATUM := odds_pull_stratum[odds_pull, STRATUM, on = .(TRIP_PLAN_LOG_SEQ)]]
# If stratum is NA, it was a 2024 stratum
odds_pull <- odds_pull[!is.na(STRATUM)]

odds_pull[year(DATE_LOGGED) == 2022][, table(TRIP_STATUS_CODE)]
odds_pull[year(DATE_LOGGED) == 2022][TRIP_STATUS_CODE == "CP"][, sum(TRIP_MONITOR_CODE %in% c("OA", "RO")/.N), keyby = .(STRATA = STRATUM)]

odds_pull[year(DATE_LOGGED) == 2023][, table(TRIP_STATUS_CODE)]
odds_pull[year(DATE_LOGGED) == 2023][TRIP_STATUS_CODE == "CP"][, sum(TRIP_MONITOR_CODE %in% c("OA", "RO")/.N), keyby = .(STRATA = STRATUM)]




#==============#
# 2023 Data ------------------------------------------------------------------------------------------------------------
#==============#

#' TODO No hyperlink yet - get this from Phil's message in the Grill chat
(load("source_data/2024-02-20cas_valhalla.Rdata"))  # loads `valhalla` object
valhalla[, range(TRIP_TARGET_DATE)]  # all 2023 trips

#' Calculate realized monitoring rates for Partial Coverage strata
realized_rate_2023 <- unique(valhalla[COVERAGE_TYPE == "PARTIAL", .(ADP, TRIP_ID, STRATA, OBSERVED_FLAG)])[
][, .(SAMPLE_RATE = sum(OBSERVED_FLAG == "Y")/.N), keyby = .(ADP, STRATA)
][, STRATA := fcase(
  STRATA %in% c("EM_HAL", "EM_POT", "ZERO"), STRATA,
  STRATA == "EM_TRW_EFP", "EM_TRW",
  STRATA %in% c("HAL", "POT", "TRW"), paste0("OB_", STRATA)
)][, .SD, keyby = .(ADP, STRATA)]
realized_rate_2023  # Only 16.6% and 16.3% in EM_HAL and EM_POT in 2023. ODDS has 32.1% and 32.1%

#' Both EM_HAL and EM_POT at under 17?? Seem like review is behind.
fgem_trips <- unique(valhalla[STRATA %in% c("EM_HAL", "EM_POT"), .(STRATA, TRIP_ID, TRIP_TARGET_DATE, LANDING_DATE, OBSERVED_FLAG)])
fgem_trips[, ':=' (
  TRIP_TARGET_DATE = min(TRIP_TARGET_DATE, LANDING_DATE, na.rm = T), 
  LANDING_DATE = max(TRIP_TARGET_DATE, LANDING_DATE, na.rm = T)),
  keyby = .(TRIP_ID)]
fgem_trips <- unique(fgem_trips)
ggplot(fgem_trips, aes(x = TRIP_TARGET_DATE, fill = OBSERVED_FLAG)) + 
  facet_grid(STRATA ~ .) + 
  geom_histogram(color = "black") + theme(legend.position = "bottom")
#' TODO How well does this track on the ODDS side?

# What rates were programmed for the 2023 ADP?






#==============#
# 2022 Data ------------------------------------------------------------------------------------------------------------
#==============#
# Pull pull VALHALLA 2022 and see if the OBSERVED_FLAG is nay different than what I have above
valhalla_2022 <- setDT(dbGetQuery(channel, paste("SELECT * FROM loki.akr_valhalla WHERE adp = 2022")))

realized_rate_2022 <- unique(valhalla_2022[COVERAGE_TYPE == "PARTIAL", .(ADP, TRIP_ID, STRATA, OBSERVED_FLAG)])[
][, .(SAMPLE_RATE = sum(OBSERVED_FLAG == "Y")/.N), keyby = .(ADP, STRATA)
][, STRATA := fcase(
  STRATA %in% c("EM_HAL", "EM_POT", "ZERO"), STRATA,
  STRATA == "EM_TRW_EFP", "EM_TRW",
  STRATA %in% c("HAL", "POT", "TRW"), paste0("OB_", STRATA)
)][, .SD, keyby = .(ADP, STRATA)]
realized_rate_2022  # Still only seeing 20.2% and 24.4% in EM_HAL and EM_POT in 2022. ODDS has 31.5% and 36.7%.

#' The 2022 AR had these same monitoring % reported, which was review through April 10, 2023.
#' TODO Do we need to re-run Valhalla for the 2022 year?

# TODOS ----
# clean up functions script to only include what is needed


#======================================================================================================================#
# STARTING ALL OVER ---------------------------------------------------------------------------------------------------
#======================================================================================================================#


#======================================================================================================================#
# Valhalla -------------------------------------------------------------------------------------------------------------
#======================================================================================================================#
# Loading Valhalla for 2022 and 2023

# For now, pull VALHALLA for 2022 and 2023. Will load from an .rdata file when the data is finalized.
if(F){
  
  library(odbc)
  channel <- dbConnect(
    odbc::odbc(), "AFSC", 
    UID = rstudioapi::askForPassword("Database user"), PWD = rstudioapi::askForPassword("Database password"))
  
  # For Valhalla table only has through 2022, with the last run date as 2023-04-10
  valhalla.2022 <- setDT(dbGetQuery(channel, paste("SELECT * FROM loki.akr_valhalla WHERE adp IN (2022, 2023)")))
  
  # Load Phil's 2023 valhalla dataset (available in the GRILL chat)
  (load("source_data/2024-02-20cas_valhalla.Rdata"))
  # Reconcile differences in column classes
  valhalla.2022[, TRIP_TARGET_DATE := as.Date(TRIP_TARGET_DATE)]
  valhalla.2022[, LANDING_DATE := as.Date(LANDING_DATE)]
  # Combine the 2022 and 2023 datasets. Note that TRIP_ID will be a character format (2022 was, but 2023 was numeric)
  valhalla <- rbind(valhalla.2022, valhalla)
}

#' Load spatiotemporal functions (modified from the 2024 ADP)
source("functions/spatiotemp_functions.R")

#' Load the ADFG statistical area shapefile.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

#======================================================================================================================#
# Sample Rates ---------------------------------------------------------------------------------------------------------
#======================================================================================================================#

## Programmed Rates (ODDS ) --------------------------------------------------------------------------------------------
# The ODDS pull will take place in 1_AR_data.R, but for now we can grab the programmed rates from the tables..

# Pull ODDS data from the ODDS schema
odds_rates_pull <- setDT(dbGetQuery(channel, paste(
  "
  SELECT UNIQUE
    a.strata, a.percent, a.effective_date,
    b.group_code, b.description, b.gear_typecode, b.pelagic_nonpelagic_trawl
  FROM odds.odds_strata_rates a 
    JOIN odds.odds_strata b 
        ON a.strata = b.strata
  WHERE extract(YEAR FROM a.effective_date) IN(2022, 2023)
  ORDER BY a.effective_date;
  "
)))

odds_rates <- unique(odds_rates_pull[
# Exclude multi-area IFQ and EM_compliance
][!(STRATA %in% c(96, 97, 98))
# Define ADP Year
][, ADP := as.integer(year(EFFECTIVE_DATE))
# Identify Stratum based on group (Observer/EM) and gear type.
][, STRATUM := fcase(
  GROUP_CODE %in% c(10,11), fcase(
    GEAR_TYPECODE == 6, "OB_POT", 
    GEAR_TYPECODE == 8, "OB_HAL",
    GEAR_TYPECODE == 3, "OB_TRW"),
  GROUP_CODE == 13, fcase(
    GEAR_TYPECODE == 6, "EM_POT",
    GEAR_TYPECODE == 8, "EM_HAL"))
][, .(ADP, STRATA = STRATUM, SAMPLE_RATE = PERCENT/100)])
# Hard-code the rates for Trawl EM
odds_rates.em_trw <- copy(odds_rates[1:2])[, ':=' (ADP = as.integer(2022:2023), STRATA = "EM_TRW", SAMPLE_RATE = 0.3333)][]
# Hard-code the rates for ZERO
odds_rates.zero <- copy(odds_rates[1:2])[, ':=' (ADP = as.integer(2022:2023), STRATA = "ZERO", SAMPLE_RATE = 0)][]
# Combine 
odds_rates_programmed <- rbind(odds_rates, odds_rates.em_trw, odds_rates.zero)[, .SD, keyby = .(ADP, STRATA)]

## Realized Rates (ODDS) -----------------------------------------------------------------------------------------------
#' *This is done in 1_AR_data.R*. We will have to decide how to apply the ODDS trip selection outcomes to VALHALLA if w
#' want to evaluate spatiotemporal monitoring coverage of what we 'think we'll get' versus 'what we have on hand'.

## Realized Rates (Valhalla) -------------------------------------------------------------------------------------------

#' *NOTE* the 'realized' rates from VALHALLA reflect which trips have data, and doesn't reflect which trips were 
#' actually selected/monitored. Therefore, because fixed-gear EM review is so far behind, the 'realized' rates are
#' apparently much lower than the intended 'programmed' rates.
#' *FOR NOW* we will use VALHALLA as-is, but if we can use the ODDS dataset to identify which VALHALLA trips were 
#' monitored, then we can better evaluate what coverage will eventually be achieved in the fixed-gear EM strata.

valhalla_rates_realized <- unique(valhalla[COVERAGE_TYPE == "PARTIAL", .(ADP, STRATA, TRIP_ID, OBSERVED_FLAG)])[
][, .(SAMPLE_RATE = sum(OBSERVED_FLAG == "Y")/.N), keyby = .(ADP, STRATA)
][, STRATA := fcase(
  STRATA %in% c("EM_HAL", "EM_POT", "ZERO"), STRATA,
  STRATA == "EM_TRW_EFP", "EM_TRW",
  STRATA %in% c("HAL", "POT", "TRW"), paste0("OB_", STRATA)
)][, .SD, keyby = .(ADP, STRATA)]

#======================================================================================================================#
# Define Boxes ---------------------------------------------------------------------------------------------------------
#======================================================================================================================#

# Run the Valhalla through a ata wrangling function to prepare it for the box definitions.
pc_effort_st <- spatiotemp_data_prep(valhalla)

# Define boxes based on stratum only
box_def.stratum <- define_boxes(
  data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = T)

box_def.stratum_fmp <- define_boxes(
  data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = T, dmn_lst = list(nst = NULL, st = "BSAI_GOA"))

#======================================================================================================================#
# Evaluate Interspersion -----------------------------------------------------------------------------------------------
#======================================================================================================================#

#' TODO `Could also look at expected distribution vs realized of number of neighboring monitored trips for each box`
#' This can perhaps show with more resolution when/where monitoring was clumped or dispersed, rather than just whether
#' each box had at least one sample (gaps)


## Interspersion: Stratum ----------------------------------------------------------------------------------------------

#' TODO *I should be able to put this into a simple wrapper function
### Expectation given the programmed rates
programmed_exp_dt.stratum <- calculate_dmn_interspersion(
  box_def = box_def.stratum,
  selection_rates = odds_rates_programmed,
  acceptor_donor_lst = as.list(seq_len(nrow(box_def.stratum$dmn$strata_dt))) # Make each stratum donate only to itself
)
dmn_cols <- unlist(programmed_exp_dt.stratum$params, use.names = F)
programmed_exp_smry_dt.stratum <- programmed_exp_dt.stratum$RAW[
][, .(BOX_DMN_w = sum(BOX_DMN_w), POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)), keyby = dmn_cols]

### Expectation given the realized rates
realized_exp_dt.stratum <- calculate_dmn_interspersion(
  box_def = box_def.stratum,
  selection_rates = valhalla_rates_realized,
  acceptor_donor_lst = as.list(seq_len(nrow(box_def.stratum$dmn$strata_dt))) # Make each stratum donate only to itself
)
realized_exp_smry_dt.stratum <- realized_exp_dt.stratum$RAW[
][, .(BOX_DMN_w = sum(BOX_DMN_w), POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)), keyby = dmn_cols]

# Calculate the expected number of monitored trips in each box or each boxe's neighborhood?
# Start with box
realized_exp_dt.stratum.mon_nbr <-  copy(realized_exp_dt.stratum$RAW)
realized_exp_dt.stratum.mon_nbr[, MON_NBR := BOX_DMN_n * BOX_DONOR_SAMPLE_PROB]

## Realized 2023-2024 ----

# This is applicable to any box definition
realized_monitored <- pc_effort_st[unique(valhalla[OBSERVED_FLAG == "Y", .(TRIP_ID)]), on = c(wd_TRIP_ID = "TRIP_ID")]
realized_monitored.boxes <- box_def.stratum$og_data[realized_monitored, on = .(ADP, STRATA, TRIP_ID)]
# Count number of monitored trips in each box
realized_monitored_boxes_n <- realized_monitored.boxes[
][, .(BOX_n = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA, BOX_ID)
][!is.na(STRATA)][STRATA != "ZERO"]

ggplot(realized_exp_dt.stratum.mon_nbr[STRATA != "ZERO"]) + 
  facet_grid2(STRATA ~ ADP, scales = "free", independent = "all") + 
  geom_density(aes(x = MON_NBR)) + 
  geom_density(data = realized_monitored_boxes_n, aes(x = BOX_n), color = "red", alpha = 0.5)



## Interspersion: Stratum X FMP ----------------------------------------------------------------------------------------



### Expectation given the programmed rates
programmed_exp_dt.stratum_fmp <- calculate_dmn_interspersion(
  box_def = box_def.stratum_fmp,
  selection_rates = odds_rates_programmed,
  acceptor_donor_lst = as.list(seq_len(nrow(box_def.stratum_fmp$dmn$strata_dt))) # Make each stratum donate only to itself
)
dmn_cols <- unlist(programmed_exp_dt.stratum_fmp$params, use.names = F)
programmed_exp_smry_dt.stratum_fmp <- programmed_exp_dt.stratum_fmp$RAW[
][, .(
  BOX_DMN_w = sum(BOX_DMN_w), 
  POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)
), keyby = dmn_cols]

### Expectation given the realized rates
realized_exp_dt.stratum_fmp <- calculate_dmn_interspersion(
  box_def = box_def.stratum_fmp,
  selection_rates = valhalla_rates_realized,
  acceptor_donor_lst = as.list(seq_len(nrow(box_def.stratum_fmp$dmn$strata_dt))) # Make each stratum donate only to itself
)
dmn_cols <- unlist(realized_exp_dt.stratum_fmp$params, use.names = F)
realized_exp_smry_dt.stratum_fmp <- realized_exp_dt.stratum_fmp$RAW[
][, .(
  BOX_DMN_w = sum(BOX_DMN_w), 
  POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)
), keyby = dmn_cols]

# Calculate the expected number of monitored trips in each box or each boxe's neighborhood?
# Start with box
realized_exp_dt.stratum_fmp.mon_nbr <-  copy(realized_exp_dt.stratum_fmp$RAW)
realized_exp_dt.stratum_fmp.mon_nbr[, MON_NBR := BOX_DMN_n * BOX_DONOR_SAMPLE_PROB]

## Realized 2023-2024 ----

realized_monitored <- pc_effort_st[unique(valhalla[OBSERVED_FLAG == "Y", .(TRIP_ID)]), on = c(wd_TRIP_ID = "TRIP_ID")]
realized_monitored.boxes <- box_def.stratum_fmp$og_data[realized_monitored, on = .(ADP, STRATA, TRIP_ID)]
# Count number of monitored trips in each box
realized_monitored_boxes_n <- realized_monitored.boxes[
][, .(BOX_n = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA, BOX_ID)
][!is.na(STRATA)][STRATA != "ZERO"]

ggplot(realized_exp_dt.stratum_fmp.mon_nbr[STRATA != "ZERO"]) + 
  facet_grid2(STRATA ~ ADP + BSAI_GOA, scales = "free", independent = "all") + 
  geom_density(aes(x = MON_NBR)) + 
  geom_density(data = realized_monitored_boxes_n, aes(x = BOX_n), color = "red", alpha = 0.5)
# TODO I need to use simulation outputs for this. I have too many fractions of trips expected which doesn't give me the same result!
