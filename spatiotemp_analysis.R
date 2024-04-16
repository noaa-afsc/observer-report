library(data.table)
library(ggplot2)
library(FMAtools)
library(ggh4x)  # facet_grid2 has cleaner facet labels while allowing for independent x and/or y scales
library(sf)          # for manipulating simpler feature objects and mapping
library(sfheaders)   # for sf_remove_holes?  also sf_bbox() to calculate the bbox from the geometry, like st_bbox?

# Assign the address of Shared Gdrive operations for this project
project_folder <- gdrive_set_dribble("Projects/AnnRpt-Deployment-Chapter/")

# Download 2_AR_data.Rdata
gdrive_download("2_AR_data.Rdata", project_folder)
(load("2_AR_data.Rdata"))
# Remove everything except for what is needed
rm(list = setdiff(ls(), c("work.data", "partial", "project_folder", "shp_land", "shp_nmfs")))

# Load Data and Functions ----------------------------------------------------------------------------------------------

#' *Just for now (use updated dataset when it is ready*
#' Loading outputs of 2024 ADP/analyses/allocation_evaluation/data_prep.R, using the draft version, since the final
#' already had new stratum definitions applied. Only has 2022 data, not 2023
#(load("source_data/data_prep_outputs.Rdata"))#

# Make FMP-specific polygons
shp_fmp <- rbind(
  shp_nmfs %>% filter(SubArea %in% c("BS", "AI")) %>% st_buffer(1) %>% st_union() %>% st_sf() %>% sf_remove_holes() %>% 
    mutate(FMP = "GOA"),
  shp_nmfs %>% filter(SubArea == "GOA")  %>% st_buffer(1) %>% st_union() %>% st_sf() %>% sf_remove_holes() %>% 
    mutate(FMP = "BSAI")
) %>% st_set_crs(st_crs(3467))
#' Make lower-res versions that are faster to draw, especially repeatedly for faceted figures
shp_land <- shp_land %>% st_set_crs(st_crs(3467))
ak_low_res <- shp_land %>% st_simplify(dTolerance = 1e4) %>% filter(!st_is_empty(shp_land)) %>% select(geometry) %>%
  st_set_crs(st_crs(3467))
nmfs_low_res <- shp_nmfs %>% st_simplify(dTolerance = 1e4) %>% filter(!st_is_empty(shp_nmfs)) %>% select(geometry) %>% 
  st_set_crs(st_crs(3467))
fmp_low_res <- shp_fmp %>% st_simplify(dTolerance = 1e4) %>% filter(!st_is_empty(shp_fmp)) %>% select(geometry) %>%
  st_set_crs(st_crs(3467))

#' Load spatiotemporal functions developed for the 2024 ADP. 
source("functions/spatiotemp_functions.R")

#' Load the ADFG statistical area shapefile.
stat_area_sf <- st_read(
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

#' * OLD STUFF * 
if(F){
  
  
  
  
  ## Realized Rates ------------------------------------------------------------------------------------------------------
  
  #' TODO Get the realized rates for the 2022 and 2023 ADPs
  
  #' Subset 2022 and get TRIP_IDs for work.data.  *Do this the other way around when the data is ready*
  pc_dat <- pc_effort_dt[ADP %in% 2022]   #' TODO *ADD 2023 WHEN WE HAVE IT*
  wd_trip_ids <- unique(pc_dat[, .(TRIP_ID, wd_TRIP_ID)])
  
  wd_sub <- work.data[TRIP_ID %in% wd_trip_ids$wd_TRIP_ID]
  uniqueN(wd_trip_ids); uniqueN(wd_sub$TRIP_ID)
  realized_rate <- unique(wd_sub[, .(TRIP_ID, STRATA, OBSERVED_FLAG)])[, .(REALIZED_RATE = sum(OBSERVED_FLAG == "Y")/.N), keyby = .(STRATA)]
  realized_rate[, STRATA := fcase(
    STRATA %in% c("EM_HAL", "EM_POT", "ZERO"), STRATA,
    STRATA == "EM_TRW_EFP", "EM_TRW",
    STRATA %in% c("HAL", "POT", "TRW"), paste0("OB_", STRATA)
  )]
  realized_rate[, ADP := 2022L]
  realized_rate[, SAMPLE_RATE := REALIZED_RATE]
  
  ## Programmed Rates ----------------------------------------------------------------------------------------------------
  #' TODO Get the programmed rates used for the 2022 and 2023 ADPs (ODDS tables pull, or get from 'partial' object)
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
  system.time(box_def <- define_boxes(
    pc_dat, c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
    year_col = "ADP", stratum_cols = c("STRATA"), dmn_lst = list(nst = "GEAR", st = "BSAI_GOA"), geom = T, ps_cols = "GEAR"))
  box_def$strata_n_dt
  # Need rates in a dt with the columns: ADP, STRATA, STRATA_N, SAMPLE_RATE, n, (don't need ISPN, CV_SCALING, INDEX)
  # Might not need STRATA_N?
  
  # Check the stratum definitions
  unique(pc_dat$STRATA)
  box_def$strata_n_dt
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
  
  #' simulate_interspersion <- function(box_definition, sample_rates, iter, seed) {
  #'   # box_definition <- copy(box_def.fmp); sample_rates <- copy(programmed_rates) ; iter <- 1000; seed <- 12345
  #'   
  #'   year_strata <- unlist(box_definition$params[c("year_col", "stratum_cols")], use.names = F)
  #'   domains <- box_definition$params$dmn_cols
  #'   
  #'   sim_lst <- vector(mode = "list", length = iter)
  #'   
  #'   set.seed(seed)
  #'   for(i in seq_len(iter)){
  #'     
  #'     if(i %% 100 == 0) cat(paste0(i, ", "))
  #'     
  #'     # Sample according to sample rates
  #'     sample_lst <- apply(sample_rates, 1, function(x) runif(x[["STRATA_N"]]) < x[["SAMPLE_RATE"]])
  #'     trips <- setorderv(unique(subset(box_def_fmp$og_data, select = c(year_strata, "TRIP_ID"))), year_strata)
  #'     trips$SAMPLE <- unlist(sample_lst)
  #'     
  #'     #' *Now that all trips are sampled, we can decide how we want to evaluate interspersion*
  #'     #' in the ADP, we grouped domains defined by monitoring method, gear type, and BSAI_GOA, allowing trips to neighbor
  #'     #' outside of strata within the monitoring method.
  #'     #' For the ADP, we should start off by keeping the evaluation STRATUM-SPECIFIC, then we can consider if we want to 
  #'     #' use other domains. All we need to change our expectation is building a new 'box definition' with different dmn parameters.
  #'     
  #'     # Identify which boxes were sampled in each stratum
  #'     box_sample <- copy(box_definition$og_data)
  #'     box_sample <- trips[box_sample, on =  c(year_strata, "TRIP_ID")][SAMPLE == T]
  #'     box_sample <- unique(subset(box_sample, select = c(year_strata, "TIME", "HEX_ID", "BOX_ID")))
  #'     
  #'     dmn_sample <- split(box_sample, by = year_strata)
  #'     nbr_sample <- lapply(dmn_sample, function(x) box_definition$nbr_lst[ x[["BOX_ID"]] ])
  #'     nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
  #'     
  #'     nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
  #'     nbr_sample_dt[, (year_strata) := tstrsplit(ADP.STRATA, split = "[.]") ]
  #'     nbr_sample_dt[, "ADP.STRATA" := NULL]
  #'     nbr_sample_dt[, ADP := as.integer(ADP)]
  #' 
  #'     # Get subset of boxes that actually contained fishing trips
  #'     boxes_with_trips <- unique(subset(box_definition$og_data, select = c("BOX_ID", year_strata)))
  #'     # Subset boxes neighboring with those with actual fishing
  #'     nbr_sample_dt <- fintersect(nbr_sample_dt, boxes_with_trips) # 994 boxes with fishing didn't have neighboring trips
  #'     
  #'     # Get domain weights of sampled boxes
  #'     dmn_sample_dt <- box_def_fmp$dmn$box_dmn_smry_dt[nbr_sample_dt, on = c(year_strata, "BOX_ID")]
  #'     dmn_sample_dt <- dmn_sample_dt[, .(SUM_DMN_w = sum(BOX_DMN_w, na.rm = T)), keyby = c(year_strata, domains)]
  #'     dmn_sample_dt[, STRATA_DMN_N := box_def_fmp$dmn$strata_dmn_n_dt[dmn_sample_dt, STRATA_DMN_N, on = c(year_strata, domains)]]
  #'     # Calculate interspersion for each domain
  #'     dmn_sample_dt[, INSP := SUM_DMN_w / STRATA_DMN_N]
  #'     sim_lst[[i]] <- dmn_sample_dt
  #'   } 
  #'   
  #'   sim_dt <- rbindlist(sim_lst, idcol = "ITER")
  #'   sim_dt[, .(MEAN = mean(INSP)), keyby = year_strata] 
  #'   # Capture the domains as an attribute
  #'   setattr(sim_dt, "year_strata_domains", c(year_strata, domains))
  #'   
  #'   sim_dt
  #' }
  
  
  # [GRAB FROM HERE] ####
  #' *===================================================================================================================*
  #' * GRABBING FROM HERE *
  #' *===================================================================================================================*
  
  
  
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
  
  # calculate_density <- function(sim_res, fill_color) {
  #   # sim_res <- copy(sim.realized); fill_color <- "green"
  #   
  #   year_strata_domains <- attr(sim_res, "year_strata_domains")
  #   domain_tbl <- setorderv(unique(subset(sim_res, select = year_strata_domains)), year_strata_domains)
  #   tail_color <- paste0(fill_color, "4")
  #   
  #   domain_density <- vector(mode = "list", length = nrow(domain_tbl))
  #   for(i in 1:nrow(domain_tbl)) {
  #     
  #     dmn_subset <- sim_res[domain_tbl[i,], on = year_strata_domains]
  #     dmn_quantile <- quantile(dmn_subset$INSP, probs = c(0.025, 0.975))
  #     dmn_median <- quantile(dmn_subset$INSP, probs = 0.5)
  #     
  #     # Run geom_density and extract the result
  #     dmn_density <- ggplot_build(
  #       ggplot(dmn_subset, aes(x = INSP)) + geom_density(bounds = c(0, 1))
  #     )$data[[1]]
  #     dmn_density <- data.table(domain_tbl[i,], X = dmn_density$x, Y = dmn_density$y)
  #     
  #     # Identify the quantiles and assign them to each datapoint
  #     dmn_density$quant = factor(findInterval(dmn_density$X, dmn_quantile))
  #     # Define plotting fill colors and plotting groups
  #     dmn_density[, FILL := as.factor(fcase(quant %in% c(0, 2), tail_color, quant == 1, fill_color))]
  #     
  #     # Extract the cutoffs of each group
  #     cutoffs <- dmn_density[, .SD[c(1, .N),], keyby = c(year_strata_domains, "quant", "FILL")]
  #     # For the lower , add the x values of the tails 0 and 3
  #     quant_1.min <- cutoffs[quant == 0][.N][, ':=' (FILL = fill_color, quant = 1)][]
  #     quant_1.max <- cutoffs[quant == 2][1][, ':=' (FILL = fill_color, quant = 1)][]
  #     
  #     # Identify the median and split the middle.
  #     quant_1 <- rbind(quant_1.min, dmn_density[quant == 1], quant_1.max)
  #     quant_1$GROUP <- findInterval(quant_1$X, dmn_median) + 1
  #     quant_1_g2 <- copy(quant_1[GROUP == 1][.N])
  #     quant_1_g2[, GROUP := 2]
  #     quant_1 <- setorder(rbind(quant_1, quant_1_g2), X, GROUP)
  #     
  #     quant_0 <- dmn_density[quant == 0][, GROUP := 0][]
  #     quant_2 <- dmn_density[quant == 2][, GROUP := 3][]
  #     
  #     # Put it all back together
  #     dmn_density <- rbind(quant_0, quant_1, quant_2)
  #     
  #     domain_density[[i]] <-  dmn_density
  #   }
  #   domain_density <- rbindlist(domain_density)
  #   domain_density[, GROUP := as.factor(GROUP)]
  #   
  #   setattr(domain_density, "year_strata_domains", year_strata_domains)
  #   domain_density
  # }
  
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
  #' if(F){
  #'   
  #'   library(odbc)
  #'   channel <- dbConnect(
  #'     odbc::odbc(), "AFSC", 
  #'     UID = rstudioapi::askForPassword("Database user"), PWD = rstudioapi::askForPassword("Database password"))
  #'   
  #'   # For Valhalla table only has through 2022, with the last run date as 2023-04-10
  #'   valhalla.2022 <- setDT(dbGetQuery(channel, paste("SELECT * FROM loki.akr_valhalla WHERE adp IN (2022, 2023)")))
  #'   
  #'   # Load Phil's 2023 valhalla dataset (available in the GRILL chat)
  #'   (load("source_data/2024-02-20cas_valhalla.Rdata"))
  #'   # Reconcile differences in column classes
  #'   valhalla.2022[, TRIP_TARGET_DATE := as.Date(TRIP_TARGET_DATE)]
  #'   valhalla.2022[, LANDING_DATE := as.Date(LANDING_DATE)]
  #'   # Combine the 2022 and 2023 datasets. Note that TRIP_ID will be a character format (2022 was, but 2023 was numeric)
  #'   valhalla <- rbind(valhalla.2022, valhalla)
  #' }
  #' 
  #' #' Load spatiotemporal functions (modified from the 2024 ADP)
  #' source("functions/spatiotemp_functions.R")
  #' 
  #' #' Load the ADFG statistical area shapefile.
  #' stat_area_sf <- st_read(
  #'   dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  #'   select(STAT_AREA) %>%
  #'   st_transform(crs = 3467)
  
  wd_trip_ids <- unique(pc_effort_st[, .(TRIP_ID, wd_TRIP_ID)])
  wd_sub <- work.data[TRIP_ID %in% wd_trip_ids$wd_TRIP_ID]
  wd_trip_ids <- unique(pc_effort_st[, .(TRIP_ID, wd_TRIP_ID)])
  wd_sub <- work.data[TRIP_ID %in% wd_trip_ids$wd_TRIP_ID]
  wd_sub <- unique(wd_sub[OBSERVED_FLAG == "Y", .(TRIP_ID, STRATA)])
  wd_sub[, uniqueN(TRIP_ID), keyby = .(STRATA)]
  wd_sub[, wd_TRIP_ID := TRIP_ID][, c("TRIP_ID", "STRATA") := NULL]
  wd_sub <- pc_effort_st[wd_sub, on = .(wd_TRIP_ID)]
  wd_sub <- unique(wd_sub[, .(ADP, STRATA, TRIP_ID)])
  
  #======================================================================================================================#
  # Sample Rates ---------------------------------------------------------------------------------------------------------
  #======================================================================================================================#
  
  ## Programmed Rates (ODDS ) --------------------------------------------------------------------------------------------
  # The ODDS pull will take place in 1_AR_data.R, but for now we can grab the programmed rates from the tables..
  #' * use PARTIAL from 2_AR_data.R!*
  
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
  
  #' TODO * This is not informative - have to get these results from simulataions and show the average distribution, not *
  #' *compare the the expected values*
  ggplot(realized_exp_dt.stratum.mon_nbr[STRATA != "ZERO"]) + 
    facet_grid2(STRATA ~ ADP, scales = "free", independent = "all") + 
    geom_density(aes(x = MON_NBR)) + 
    geom_density(data = realized_monitored_boxes_n, aes(x = BOX_n), color = "red", alpha = 0.5)
  
  
  
  ## Interspersion: Stratum X FMP ----------------------------------------------------------------------------------------
  
  
  # 
  # ### Expectation given the programmed rates
  # programmed_exp_dt.stratum_fmp <- calculate_dmn_interspersion(
  #   box_def = box_def.stratum_fmp,
  #   selection_rates = odds_rates_programmed,
  #   acceptor_donor_lst = as.list(seq_len(nrow(box_def.stratum_fmp$dmn$strata_dt))) # Make each stratum donate only to itself
  # )
  # dmn_cols <- unlist(programmed_exp_dt.stratum_fmp$params, use.names = F)
  # programmed_exp_smry_dt.stratum_fmp <- programmed_exp_dt.stratum_fmp$RAW[
  # ][, .(
  #   BOX_DMN_w = sum(BOX_DMN_w), 
  #   POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)
  # ), keyby = dmn_cols]
  # 
  # ### Expectation given the realized rates
  # realized_exp_dt.stratum_fmp <- calculate_dmn_interspersion(
  #   box_def = box_def.stratum_fmp,
  #   selection_rates = valhalla_rates_realized,
  #   acceptor_donor_lst = as.list(seq_len(nrow(box_def.stratum_fmp$dmn$strata_dt))) # Make each stratum donate only to itself
  # )
  # dmn_cols <- unlist(realized_exp_dt.stratum_fmp$params, use.names = F)
  # realized_exp_smry_dt.stratum_fmp <- realized_exp_dt.stratum_fmp$RAW[
  # ][, .(
  #   BOX_DMN_w = sum(BOX_DMN_w), 
  #   POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)
  # ), keyby = dmn_cols]
  # 
  # # Calculate the expected number of monitored trips in each box or each boxe's neighborhood?
  # # Start with box
  # realized_exp_dt.stratum_fmp.mon_nbr <-  copy(realized_exp_dt.stratum_fmp$RAW)
  # realized_exp_dt.stratum_fmp.mon_nbr[, MON_NBR := BOX_DMN_n * BOX_DONOR_SAMPLE_PROB]
  # 
  # ## Realized 2023-2024 ----
  # 
  # realized_monitored <- pc_effort_st[unique(valhalla[OBSERVED_FLAG == "Y", .(TRIP_ID)]), on = c(wd_TRIP_ID = "TRIP_ID")]
  # realized_monitored.boxes <- box_def.stratum_fmp$og_data[realized_monitored, on = .(ADP, STRATA, TRIP_ID)]
  # # Count number of monitored trips in each box
  # realized_monitored_boxes_n <- realized_monitored.boxes[
  # ][, .(BOX_n = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA, BOX_ID)
  # ][!is.na(STRATA)][STRATA != "ZERO"]
  # 
  # ggplot(realized_exp_dt.stratum_fmp.mon_nbr[STRATA != "ZERO"]) + 
  #   facet_grid2(STRATA ~ ADP + BSAI_GOA, scales = "free", independent = "all") + 
  #   geom_density(aes(x = MON_NBR)) + 
  #   geom_density(data = realized_monitored_boxes_n, aes(x = BOX_n), color = "red", alpha = 0.5)
  # # TODO I need to use simulation outputs for this. I have too many fractions of trips expected which doesn't give me the same result!
  
  
  # Splitting stratum by FMP only
  box_def.stratum_fmp <- define_boxes(
    data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
    year_col = "ADP", stratum_cols = "STRATA", geom = T, dmn_lst = list(nst = NULL, st = "BSAI_GOA"))
  
  #' TODO *put this in the same simulation* as for stratum-only. grab the additional columns as needed.
  
  trips_2022 <- setkey(unique(pc_effort_st[, .(ADP, STRATA, BSAI_GOA, TRIP_ID)]), "ADP", "STRATA")
  trips_2022_N <- trips_2022[, .(N = uniqueN(TRIP_ID)), keyby = .(ADP, STRATA)]
  # Merge in programmed rates
  trips_2022_N <- odds_rates_programmed[trips_2022_N, on = .(ADP, STRATA)]
  
  ## Simulation of realized rate -----------------------------------------------------------------------------------------
  realized_trips <- copy(trips_2022_N)
  realized_trips[, SAMPLE_RATE := valhalla_rates_realized[realized_trips, SAMPLE_RATE, on = .(ADP, STRATA)]]
  
  # iter <- 10000
  iter <- 1000   #' *for testing*
  
  real_lst <- vector(mode = "list", length = iter)
  
  set.seed(12345)
  # TODO MAKE THIS A FUNCTION
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
    box_sample <- copy(box_def.stratum_fmp$og_data)
    box_sample <- trips_2022_sample[box_sample, on = .(ADP, STRATA, TRIP_ID)][SAMPLE == T]
    box_sample <- unique(box_sample[, .(ADP, STRATA, TIME, HEX_ID, BOX_ID)])
    
    # Identify which boxes were sampled or neighboring sampled boxes
    dmn_sample <- split(box_sample, by = c("ADP", "STRATA"))
    nbr_sample <- lapply(dmn_sample, function(x) box_def.stratum_fmp$nbr_lst[ x[["BOX_ID"]] ])
    nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
    # Make a data.frame for each ADP x stratum with the list of boxes sampled or neighboring a sampled box
    nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
    nbr_sample_dt[, c("ADP", "STRATA") := tstrsplit(ADP.STRATA, split = "[.]") ]
    nbr_sample_dt[, "ADP.STRATA" := NULL]
    nbr_sample_dt[, ADP := as.integer(ADP)]
    
    # Get subset of boxes that actually contained fishing trips
    boxes_with_trips <- unique(copy(box_def.stratum_fmp$og_data)[, .(BOX_ID, ADP, STRATA)])  # 3527 strata x box_ids with fishing effort
    # Subset boxes neighboring with those with actual fishing
    nbr_sample_dt <- fintersect(nbr_sample_dt, boxes_with_trips) # 994 boxes with fishing didn't have neighboring trips
    
    # Get domain weights of sampled boxes
    dmn_sample_dt <- box_def.stratum_fmp$dmn$box_dmn_smry_dt[nbr_sample_dt, on = .(ADP, STRATA, BOX_ID)]
    dmn_sample_dt <- dmn_sample_dt[, .(SUM_DMN_w = sum(BOX_DMN_w, na.rm = T)), keyby = .(ADP, STRATA, BSAI_GOA)]
    dmn_sample_dt[, STRATA_DMN_N := box_def.stratum_fmp$dmn$strata_dmn_n_dt[dmn_sample_dt, STRATA_DMN_N, on = .(ADP, STRATA, BSAI_GOA)]]
    # Calculate interspersion for each domain
    dmn_sample_dt[, INSP := SUM_DMN_w / STRATA_DMN_N]
    real_lst[[i]] <- dmn_sample_dt
  } 
  real_fmp_dt <- rbindlist(real_lst, idcol = "ITER")[!is.na(INSP)]
  real_fmp_dt_mean <- real_fmp_dt[, .(MEAN = mean(INSP)), keyby = .(ADP, STRATA, BSAI_GOA)]  # Here is my 'expected' value, getting mean of distribution
  range(real_fmp_dt_mean$MEAN)
  
  ## Realized split by FMP 
  
  # Expectation assuming we achieved perfect deployment at our programmed rates
  real_exp_dt <- calculate_dmn_interspersion(
    box_def = box_def.stratum_fmp,
    selection_rates = valhalla_rates_realized,
    acceptor_donor_lst = as.list(seq_len(nrow(box_def.stratum_fmp$dmn$strata_dt))) # Make each stratum donate only to itself
  )
  dmn_cols <- unlist(real_exp_dt$params, use.names = F)
  real_exp_smry_dt <- real_exp_dt$RAW[
  ][, .(BOX_DMN_w = sum(BOX_DMN_w), POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)), keyby = dmn_cols]
  
  # Identify sampled boxes and neighbors when split by BSAI_GIA
  box_sample.bsai_goa <- copy(box_def.stratum_fmp$og_data)
  box_sample.bsai_goa <- box_sample.bsai_goa[wd_sub, on = .(ADP, STRATA, TRIP_ID)]
  box_sample.bsai_goa <- unique(box_sample.bsai_goa[, .(ADP, STRATA, TIME, HEX_ID, BOX_ID)])
  
  dmn_sample.bsai_goa <- split(box_sample.bsai_goa, by = c("ADP", "STRATA"))
  nbr_sample.bsai_goa <- lapply(dmn_sample.bsai_goa, function(x) box_def.stratum_fmp$nbr_lst[ x[["BOX_ID"]] ])
  nbr_sample.bsai_goa <- lapply(nbr_sample.bsai_goa, function(x) unique(unlist(x)))
  
  nbr_sample_dt.bsai_goa <- rbindlist(lapply(nbr_sample.bsai_goa, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
  nbr_sample_dt.bsai_goa[, c("ADP", "STRATA") := tstrsplit(ADP.STRATA, split = "[.]") ]
  nbr_sample_dt.bsai_goa[, "ADP.STRATA" := NULL]
  nbr_sample_dt.bsai_goa[, ADP := as.integer(ADP)]
  nbr_sample_dt.bsai_goa
  
  # Calculate realized interspersion for each domain
  strata_n.bsai_goa <- box_def.stratum_fmp$dmn$strata_dmn_n_dt
  insp_dt.bsai_goa <- box_def.stratum_fmp$dmn$box_dmn_smry_dt[nbr_sample_dt.bsai_goa, on = .(ADP, STRATA, BOX_ID)][
  ][, .(INSP_NUM = sum(BOX_DMN_w)) , keyby = .(ADP, STRATA, BSAI_GOA)]
  insp_dt_sum.bsai_goa <- insp_dt.bsai_goa[strata_n.bsai_goa, on = .(ADP, STRATA, BSAI_GOA)]
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
  
  


}


#'*=======================================*
# For ANALYSES, START OVER HERE ----
#'*=======================================*

# Functions ----

calculate_expected_interspersion <- function(box_def, sample_rates){
  # box_def <- copy(box_def.stratum); sample_rates <- copy(programmed_rates)
  
  exp_dt <- calculate_dmn_interspersion(
    box_def = box_def,
    selection_rates = sample_rates,
    acceptor_donor_lst = as.list(seq_len(nrow(box_def$dmn$strata_dt))) # Make each stratum donate only to itself
  )
  dmn_cols <- unlist(exp_dt$params, use.names = F)
  out <- exp_dt$RAW[
  ][, .(
    BOX_DMN_w = sum(BOX_DMN_w), 
    POOL_DMN_INTERSPERSION = weighted.mean(BOX_DONOR_SAMPLE_PROB, w = BOX_DMN_w)
  ), keyby = dmn_cols]
  setattr(out, "box_expected", exp_dt )
  out
}

calculate_realized_interspersion <- function(box_def, monitored_trips) {
  # box_def <- copy(box_def.stratum); monitored_trips <- copy(realized_mon); 
  # box_def <- copy(box_def.stratum_fmp); monitored_trips <- copy(realized_mon); 
  
  year_strata <- unlist(box_def$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- unlist(box_def$params[c("year_col", "stratum_cols", "dmn_cols")], use.names = F)
  
  # Identify sampled boxes and neighbors
  box_sample <- copy(box_def$og_data)
  box_sample <- box_sample[monitored_trips, on = c(year_strata,  "TRIP_ID")]
  box_sample <- unique(subset(box_sample, select = c(year_strata, "TIME", "HEX_ID", "BOX_ID")))
  
  dmn_sample <- split(box_sample, by = year_strata)
  nbr_sample <- lapply(dmn_sample, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ])
  nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
  
  nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
  nbr_sample_dt[, (year_strata) := tstrsplit(ADP.STRATA, split = "[.]") ]
  nbr_sample_dt[, "ADP.STRATA" := NULL]
  nbr_sample_dt[, ADP := as.integer(ADP)]
  
  strata_domain_n <- box_def$dmn$strata_dmn_n_dt
  insp_dt <- box_def$dmn$box_dmn_smry_dt[nbr_sample_dt, on = c(year_strata, "BOX_ID")][!is.na(BOX_DMN_w)]
  insp_dt_sum <- insp_dt[, .(SUM_DMN_w = sum(BOX_DMN_w)) , keyby = domains]
  insp_dt_sum <- insp_dt_sum[strata_domain_n, on = domains]
  insp_dt_sum[, INSP := SUM_DMN_w / STRATA_DMN_N]
  
  setattr(insp_dt_sum, "sampled_boxes", nbr_sample_dt)
  
  insp_dt_sum[!is.na(INSP)]
}

#' *OLD VERSION*
if(F){
  simulate_interspersion <- function(box_def, sample_rates, iter, seed) {
  # box_def <- copy(box_def.stratum_fmp); sample_rates <- copy(programmed_rates) ; iter <- 1000; seed <- 12345
  
  year_strata <- unlist(box_def$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- box_def$params$dmn_cols
  sim_lst <- vector(mode = "list", length = iter)
  set.seed(seed)
  for(i in seq_len(iter)){
    
    if(i %% 100 == 0) cat(paste0(i, ", "))
    
    # Sample according to sample rates
    sample_lst <- apply(sample_rates, 1, function(x) runif(x[["STRATA_N"]]) < x[["SAMPLE_RATE"]])
    trips <- setorderv(unique(subset(box_def$og_data, select = c(year_strata, "TRIP_ID"))), year_strata)
    trips$SAMPLE <- unlist(sample_lst)
    
    #' *Now that all trips are sampled, we can decide how we want to evaluate interspersion*
    #' in the ADP, we grouped domains defined by monitoring method, gear type, and BSAI_GOA, allowing trips to neighbor
    #' outside of strata within the monitoring method.
    #' For the ADP, we should start off by keeping the evaluation STRATUM-SPECIFIC, then we can consider if we want to 
    #' use other domains. All we need to change our expectation is building a new 'box definition' with different dmn parameters.
    
    # Identify which boxes were sampled in each stratum
    box_sample <- copy(box_def$og_data)
    box_sample <- trips[box_sample, on =  c(year_strata, "TRIP_ID")][SAMPLE == T]
    box_sample <- unique(subset(box_sample, select = c(year_strata, "TIME", "HEX_ID", "BOX_ID")))
    
    dmn_sample <- split(box_sample, by = year_strata)
    nbr_sample <- lapply(dmn_sample, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ])
    nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
    
    nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
    nbr_sample_dt[, (year_strata) := tstrsplit(ADP.STRATA, split = "[.]") ]
    nbr_sample_dt[, "ADP.STRATA" := NULL]
    nbr_sample_dt[, ADP := as.integer(ADP)]
    
    # Get subset of boxes that actually contained fishing trips
    boxes_with_trips <- unique(subset(box_def$og_data, select = c("BOX_ID", year_strata)))
    # Subset boxes neighboring with those with actual fishing
    nbr_sample_dt <- fintersect(nbr_sample_dt, boxes_with_trips) # 994 boxes with fishing didn't have neighboring trips
    
    # Get domain weights of sampled boxes
    dmn_sample_dt <- box_def$dmn$box_dmn_smry_dt[nbr_sample_dt, on = c(year_strata, "BOX_ID")]
    dmn_sample_dt <- dmn_sample_dt[, .(SUM_DMN_w = sum(BOX_DMN_w, na.rm = T)), keyby = c(year_strata, domains)]
    dmn_sample_dt[, STRATA_DMN_N := box_def$dmn$strata_dmn_n_dt[dmn_sample_dt, STRATA_DMN_N, on = c(year_strata, domains)]]
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
}

# A wrapper for calculate_realized_interspersion, but randomly samples to create new `monitored_trips` object
simulate_interspersion <- function(box_def, sample_rates, iter, seed) {
  # box_def <- copy(box_def.stratum); sample_rates <- copy(programmed_rates); iter <- 1000; seed <- 12345
  
  year_strata <- unlist(box_def$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- box_def$params$dmn_cols
  sim_lst <- vector(mode = "list", length = iter)
  set.seed(seed)
  for(i in seq_len(iter)){
    
    if(i %% 100 == 0) cat(paste0(i, ", "))
    
    # Sample according to sample rates
    sample_lst <- apply(sample_rates, 1, function(x) runif(x[["STRATA_N"]]) < x[["SAMPLE_RATE"]])
    trips <- setorderv(unique(subset(box_def$og_data, select = c(year_strata, "TRIP_ID"))), year_strata)
    #trips$SAMPLE <- unlist(sample_lst)
    
    sim_lst[[i]] <- calculate_realized_interspersion(box_def, trips[unlist(sample_lst)])
  } 
  
  sim_dt <- rbindlist(sim_lst, idcol = "ITER")
  # sim_dt[, .(MEAN = mean(INSP)), keyby = year_strata] 
  # Capture the domains as an attribute
  setattr(sim_dt, "year_strata_domains", c(year_strata, domains))
  
  sim_dt
}

calculate_density <- function(sim_res, fill_color) {
  # sim_res <- copy(sim.programmed.stratum); fill_color <- "dodgerblue"
  
  year_strata_domains <- attr(sim_res, "year_strata_domains")
  domain_tbl <- setorderv(unique(subset(sim_res, select = year_strata_domains)), year_strata_domains)
  tail_color <- paste0(fill_color, "4")

  domain_density <- vector(mode = "list", length = nrow(domain_tbl))
  for(i in 1:nrow(domain_tbl)) {
    
    dmn_subset <- sim_res[domain_tbl[i,], on = year_strata_domains ]
    dmn_quantile <- quantile(dmn_subset$INSP, probs = c(0.025, 0.975))
    dmn_median <- quantile(dmn_subset$INSP, probs = 0.5)
    
    # Due to rounding errors, INSP can be above 1.0. Truncate such cases, but make a check that INSP wasn't incorrectly
    # calculated to be above 1.
    if(nrow(dmn_subset[INSP > 1 + 1e5])) stop( paste(unname(domain_tbl[i,]), collapse = " "), " had INSP values > 1!")
    dmn_subset[INSP > 1, INSP := trunc(INSP, 0)]
    
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
    cutoffs <- dmn_density[, .SD[c(1, .N),], keyby = c(year_strata_domains, "quant", "FILL")]
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
    
    # Put it all back together. If distributions are so tight that there are not 3 quantile groups, omit empty groups.
    dmn_density <- rbind(quant_0, quant_1, quant_2)[!is.na(STRATA)]
    
    domain_density[[i]] <-  dmn_density
  }
  domain_density <- rbindlist(domain_density)
  domain_density[, GROUP := as.factor(GROUP)]
  
  setattr(domain_density, "year_strata_domains", year_strata_domains)
  domain_density[]
}

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

# Generate summary plots of interspersion density. Use facet_ functions to separate year and strata and FMP if desired.
plot_interspersion_density <- function(den, real_interspersion, dmn_N){
  # den <- copy(density.stratum); real_interspersion <- copy(real_interspersion.stratum); dmn_N <- copy(dmn_N.stratum)

  # ggplot(den, aes(x = X)) + 
  #   geom_hline(yintercept = 0, color = "gray") + 
  #   geom_ribbon(aes(ymin = 0, ymax = Y, fill = FILL, group = interaction(DIST, GROUP)), color = "black", outline.type = "full", alpha = 0.8) + 
  #   geom_vline(data = real_interspersion, aes(xintercept = INSP, linetype = "Actually Realized"), color = "purple", linewidth = 0.75) + 
  #   theme(
  #     axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  #     panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank() ) + 
  #   ylim(c(-1, 1)) + 
  #   labs(x = "Interspersion", y = "Density") +  
  #   scale_fill_identity(
  #     name = "Monitoring Rate", guide = "legend",
  #     breaks = c("green", "dodgerblue"), labels = c('Realized', "Programmed")) +
  #   scale_linetype_manual(name = NULL, values = 2 ) +
  #   theme(legend.position = "bottom", legend.key = element_rect(fill = "white")) + 
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  #   geom_text(data = dmn_N, aes(label = STRATA_DMN_N, x = X, y = 1), hjust = 0, vjust = 1, size = 3.5)
  
  # Identify the fill colors of the distributions
  fill_breaks <- unique(den$FILL)[!(unique(den$FILL) %like% '4')]
  
  ggplot(den, aes(x = X)) + 
    geom_hline(yintercept = 0, color = "gray") + 
    geom_ribbon(aes(ymin = 0, ymax = Y, fill = FILL, group = interaction(DIST, GROUP)), color = "black", outline.type = "full", alpha = 0.8) + 
    geom_vline(data = real_interspersion, aes(xintercept = INSP, linetype = "Actually Realized"), color = "purple", linewidth = 0.75) + 
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank() ) + 
    ylim(c(-1, 1)) + 
    labs(x = "Interspersion", y = "Density") +  
    scale_fill_identity(
      name = "Monitoring Rate", guide = "legend",
      breaks = fill_breaks, labels = c('Realized', "Programmed")) +
    scale_linetype_manual(name = NULL, values = 2 ) +
    theme(legend.position = "bottom", legend.key = element_rect(fill = "white")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
    geom_text(data = dmn_N, aes(label = STRATA_DMN_N, x = -Inf, y = Inf), hjust = -0.1, vjust = 1.1, size = 3.5)
}


# Analyses -------------------------------------------------------------------------------------------------------------


## Data Prep ----

# Wrangle the Valhalla data set for spatiotemporal analyses
pc_effort_st <- spatiotemp_data_prep(work.data)

#' FIXME *Some ZERO TRIPS were MONITORED IN 2023 - Phil is working on removing these*
pc_effort_st[STRATA == "ZERO" & OBSERVED_FLAG == "Y", OBSERVED_FLAG := "N"]

# Realized Rates
realized_rates.val <- unique(pc_effort_st[, .(ADP, STRATA, TRIP_ID, OBSERVED_FLAG)])[
][, .(STRATA_N = .N, SAMPLE_RATE = sum(OBSERVED_FLAG == "Y")/.N), keyby = .(ADP, STRATA)]

# Realized Monitored trips
realized_mon <- unique(pc_effort_st[OBSERVED_FLAG == "Y", .(ADP, STRATA, TRIP_ID)])

#' TODO Use the *partial* object from 2_AR_data.R to create this object, when it is ready
programmed_rates <- rbind(
  data.table(
    ADP = rep(2022, times = 7),
    STRATA = c("EM_HAL", "EM_POT", "EM_TRW_EFP", "OB_HAL", "OB_POT", "OB_TRW", "ZERO"),
    SAMPLE_RATE = c(0.3, 0.3, 0.3333, 0.1902, 0.1748, 0.2965, 0)),
  data.table(
    ADP = rep(2023, times = 7),
    STRATA = c("EM_HAL", "EM_POT", "EM_TRW_EFP", "OB_HAL", "OB_POT", "OB_TRW", "ZERO"),
    SAMPLE_RATE = c(0.3, 0.3, 0.3333, 0.1787, 0.1709, 0.2268, 0))
)
# Add STRATA_N to programmed_rates
programmed_rates[, STRATA_N := realized_rates.val[programmed_rates, STRATA_N, on = .(ADP, STRATA)]]
setcolorder(programmed_rates, c("ADP", "STRATA", "STRATA_N", "SAMPLE_RATE"))

#' TODO *Make sure the realized ODDS and realized VALHALLA rates aren't too far off!* We will use the VALHALLA rates 
#' because our fishing effort is based on Valhalla.

#' TODO *Make a check to make sure all strata names match between pc_effort_dt and rates objects!*

## Define Boxes ----

# Define boxes, post-stratifying by FMP only
box_def.stratum <- define_boxes(
  data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = T, dmn_lst = list(nst = NULL, st = NULL))

# Define boxes, post-stratifying by FMP only
box_def.stratum_fmp <- define_boxes(
  data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = T, dmn_lst = list(nst = NULL, st = "BSAI_GOA"))


## Split by Stratum Only -----------------------------------------------------------------------------------------------

# Expected interspersion given the programmed and realized rates
exp_interspersion.programmed.stratum <- calculate_expected_interspersion(box_def.stratum, programmed_rates)
exp_interspersion.realized.stratum <- calculate_expected_interspersion(box_def.stratum, realized_rates.val)

# Actually realized interspersion
real_interspersion.stratum <- calculate_realized_interspersion(box_def.stratum, realized_mon)

### Simulate trip selection to create distributions of interspersion ----

# Programmed rates
sim.programmed.stratum <- simulate_interspersion(box_def.stratum, programmed_rates, iter = 1000, seed = 12345)
# Realized rates
sim.realized.stratum <- simulate_interspersion(box_def.stratum, realized_rates.val, iter = 1000, seed = 12345)

# Calculate density of both distributions
density.programmed.stratum <- calculate_density(sim.programmed.stratum, "dodgerblue")
density.realized.stratum <- calculate_density(sim.realized.stratum, "green")

# Combine distributions
density.stratum <- combine_distributions(density.realized.stratum, density.programmed.stratum)
# Make labels for domain trip counts
dmn_N.stratum <- real_interspersion.stratum[
  density.stratum[, .SD[1,], keyby = c(attr(density.stratum, "year_strata_domains")) ], 
  on = attr(density.stratum, "year_strata_domains")]
dmn_N.stratum[, X := pmin(X, INSP)]
dmn_N.stratum[, STRATA_DMN_N := formatC(round(STRATA_DMN_N), width = max(nchar(round(STRATA_DMN_N))))]
#' TODO If the realized distribution is > programmed, the label won't be consistently placed using X. Use the density.X
#' object to grab the minimum X value of each year x strata.


#' TODO *Subset data for separate years*
#' TODO *Annotate the programmed and realized rates next to trip counts?*

#' Plot distributions vs actually realized
plot_interspersion_density(density.stratum, real_interspersion.stratum, dmn_N.stratum) + 
  facet_grid2(ADP ~ STRATA, scales = "free", independent = "x", labeller = labeller(STRATA = function(x) gsub("_", " ", x)))
#' *So it looks like OB_HAL had random sampling but had lower than expected distribution that expected by the PROGRAMMED rates*
#' *Also, OB_TRW had lower than expected interspersion but still very high (0.975). Monitoring was not randomly distributed!*
programmed_rates[STRATA == "OB_TRW"]    # programmed 0.2965
realized_rates.val[STRATA == "OB_TRW"]  # realized   0.2873, so pretty close
# FIXME - what is up with the 2023 EM_TRW_EFP distribution. Super highly-clumped except for a handful of trips?
programmed_rates[STRATA == "EM_TRW_EFP"]    # programmed 0.2965
realized_rates.val[STRATA == "EM_TRW_EFP"]  # realized   0.2873, so pretty close
#* In 2023, also fell way below the expected distributions?

## Split by Stratum and FMP (BSAI and GOA) -----------------------------------------------------------------------------

# Expected interspersion given the programmed and realized rates
exp_interspersion.programmed.stratum_fmp <- calculate_expected_interspersion(box_def.stratum_fmp, programmed_rates)
exp_interspersion.realized.stratum_fmp <- calculate_expected_interspersion(box_def.stratum_fmp, realized_rates.val)

# Actually realized interspersion
real_interspersion.stratum_fmp <- calculate_realized_interspersion(box_def.stratum_fmp, realized_mon)

### Simulate trip selection to create distributions of interspersion ----

# Programmed rates
sim.programmed.stratum_fmp <- simulate_interspersion(box_def.stratum_fmp, programmed_rates, iter = 1000, seed = 12345)
# Realized rates
sim.realized.stratum_fmp <- simulate_interspersion(box_def.stratum_fmp, realized_rates.val, iter = 1000, seed = 12345)

# Calculate density of both distributions
density.programmed.stratum_fmp <- calculate_density(sim.programmed.stratum_fmp, "dodgerblue")
density.realized.stratum_fmp <- calculate_density(sim.realized.stratum_fmp, "green")

# Combine distributions
density.stratum_fmp <- combine_distributions(density.realized.stratum_fmp, density.programmed.stratum_fmp)
# Make labels for domain trip counts
dist_x <- density.stratum_fmp[, .SD[1,], keyby = c(attr(density.stratum_fmp, "year_strata_domains")) ]
dmn_N.stratum_fmp <- real_interspersion.stratum_fmp[dist_x, on = attr(density.stratum_fmp, "year_strata_domains")]
dmn_N.stratum_fmp[, X := pmin(X, INSP)]
dmn_N.stratum_fmp[, STRATA_DMN_N := formatC(round(STRATA_DMN_N), width = max(nchar(round(STRATA_DMN_N))))]


#' Plot distributions vs actually realized
plot_interspersion_density(density.stratum_fmp, real_interspersion.stratum_fmp, dmn_N.stratum_fmp) + 
  facet_grid2(ADP + BSAI_GOA ~ STRATA, render_empty = F, scales = "free", independent = "x", labeller = labeller(
    STRATA = function(x) gsub("_", " ", x),
    BSAI_GOA = function(x) paste0("FMP : ", x)))
#' *NOTE* When post-stratifying the BSAI and GOA, we are still using a stratum-wide selection rate, not the rates realized in BSAI vs GOA
#' Should I be using the realized rates in each post-stratum for the 'realized', but keep programmed the same?
#' *When split by FMP, we see more patterns!*
#' *OB_HAL was in the expected distributions in the GOA, but below the expectation of the programmed rate in the BSAI*
real_interspersion.stratum_fmp[STRATA == "OB_HAL" & BSAI_GOA == "BSAI"]                             #  0.5660
density.stratum_fmp[STRATA == "OB_HAL" & BSAI_GOA == "BSAI" & quant == 0 & DIST == "REAL", max(X)]  #  0.4618  (realized rate)
density.stratum_fmp[STRATA == "OB_HAL" & BSAI_GOA == "BSAI" & quant == 0 & DIST == "PROG", max(X)]  # *0.5681  (programmed rate)
#' *Good news is that given all realized interspersion met the expectation of the realized rates, it means we had randomly distributed monitoring?*
#' *In the AK-wide plots, OB_TRW was not randomly distributed, but in the FMP-specific plots, both were barely within the expected range?*

real_interspersion.stratum_fmp[STRATA == "OB_TRW" & BSAI_GOA == "GOA"]                              #  0.9854
density.stratum_fmp[STRATA == "OB_TRW" & BSAI_GOA == "GOA" & quant == 0 & DIST == "REAL", max(X)]   #  0.9853
density.stratum_fmp[STRATA == "OB_TRW" & BSAI_GOA == "GOA" & quant == 0 & DIST == "PROG", max(X)]   #  0.9853
#' *OB_TRW was BARELY in the 95% range of the programmed rate*

real_interspersion.stratum_fmp[STRATA == "OB_TRW" & BSAI_GOA == "BSAI"]                             #  0.9261
density.stratum_fmp[STRATA == "OB_TRW" & BSAI_GOA == "BSAI" & quant == 0 & DIST == "REAL", max(X)]  #  0.8956
density.stratum_fmp[STRATA == "OB_TRW" & BSAI_GOA == "BSAI" & quant == 0 & DIST == "PROG", max(X)]  #  0.8984

#' *AK-wide, OB_TRW was below both expected distributions of programmed and realized rate*
real_interspersion.stratum[STRATA == "OB_TRW"]                              #  0.9761
density.stratum[STRATA == "OB_TRW" & quant == 0 & DIST == "REAL", max(X)]   # *0.9781
density.stratum[STRATA == "OB_TRW" & quant == 0 & DIST == "PROG", max(X)]   # *0.9795

#' *[Why is OB_TRW sig for AK-wide but not for either FMP?]*
if(F) {
    
  a1 <- sim.realized.stratum[STRATA == "OB_TRW"]  # AK-wide
  a2 <- sim.realized.stratum_fmp[STRATA == "OB_TRW"
  ][, .(
    SUM_DMN_w = sum(SUM_DMN_w),
    STRATA_DMN_N = sum(STRATA_DMN_N), 
    INSP = weighted.mean(INSP, w = STRATA_DMN_N)  # same as sum(SUM_DMN_w) / sum(STRATA_DMN_N)
    ), keyby = .(ITER, ADP, STRATA)]
  a1[, mean(INSP)]  # basically equivalent, but not. I used the same seeds, so shouldn't this be the same in theory?
  a2[, mean(INSP)]
  a1[, median(INSP)]
  a2[, median(INSP)]
  ggplot(a1, aes(x = INSP)) + 
    geom_histogram(alpha = 0.5, fill = "green") + 
    geom_histogram(data = a2, alpha =0.5, fill = "dodgerblue")
  a3 <- a1[a2, on = .(ITER, ADP, STRATA)]  
  a3 # Most of these look identical?
  a3[, INSP - i.INSP] #Mmost are basically rounding errors, but some are not?
  a3[INSP - i.INSP > 0.003]  # The biggest differences here. Even so, this can't explain the difference in the AK vs FMP-wide differences in expectations, right?
  
  real_interspersion.stratum[STRATA == "OB_TRW"]  # *0.97606   (actually realized)
  quantile(a1$INSP, c(0.025, 0.5, 0.975))         #  0.9781122
  quantile(a2$INSP, c(0.025, 0.5, 0.975))         #  0.9781122 (basically the same when FMPs are combined)
  
  real_interspersion.stratum[STRATA == "OB_TRW"]     # realized 713.5 AK-wide
  real_interspersion.stratum_fmp[STRATA == "OB_TRW"] # realized 106.5 in the BSAI and 607 in the GOA
  sim.realized.stratum[STRATA=="OB_TRW", quantile(SUM_DMN_w, 0.025)] # AK_wide, achieved 715 in lower 2.5%
  sim.realized.stratum_fmp[STRATA=="OB_TRW", quantile(SUM_DMN_w, 0.025), keyby = BSAI_GOA] # When split by FMP, the distributions are different! 103 in BSAI, 607 in GOA.
  # 
  
  
  sim.realized.stratum_fmp[STRATA == "OB_TRW"][, as.list(quantile(INSP, c(0.025))), keyby = .(BSAI_GOA)]
  sim.programmed.stratum_fmp[STRATA == "OB_TRW"][, as.list(quantile(INSP, c(0.025))), keyby = .(BSAI_GOA)]
  real_interspersion.stratum_fmp[STRATA == "OB_TRW"] # Looks like here we are actually equivalent to actually realized in the GOA!
  # Realized rates by FMP for OB_TRW:   2022     OB_TRW 0.3565217 0.2743506
  #' *Note that the realized rate was not uniformly distributed across the FMPs!* 
  #' The realized rate in the (BSAI was 0.3565), but only (0.2744 in the GOA), with AK-wide rate of (0.2872)
  #' Part of the reason why interspersion AK-wide was below expectation was because we had more sampling in the BSAI than in the GOA
  #' We are not applying the FMP-specific realized rates - still using the stratum-specific realized rates!
  #' 
  #' So in conclusion, AK-wide we did not fully deploy randomly because of a bias of more monitoring in the BSAI than in the GOA.
  #' However, within the FMPs, we saw:
  #'   BSAI: In spite of more monitoring than expected, the realized interspersion was below the expected median but still within the 95% interval. This means
  #'   that the spatiotemporal distribution of monitoring within the BSAI was as expected, but on the 'clumped' end (slightly more gaps than on average)
  #'   GOA: The realized interspersion was literally sitting on the lower 2.5% bound of the confidence interval - Coupled with the slightly lower realized
  #'   rate in the GOA (more sparse coverage) that may also be more clumped (see OB_TRW map - white cells could also indicate that many boxes 
  #'   contained multiple monitored trips, rather than having the sampling distributed in more boxes that can therefore neighbor more)
  
}


# Double-checking the average of distributions closely match mathematical expectation
sim.realized.stratum_fmp[, .(MEAN = mean(INSP)), keyby = .(ADP, STRATA, BSAI_GOA)][
][exp_interspersion.realized.stratum_fmp[STRATA != "ZERO"], on = .(ADP, STRATA, BSAI_GOA)
][, .(DIFF = POOL_DMN_INTERSPERSION - MEAN), by = .(ADP, STRATA, BSAI_GOA)]

sim.programmed.stratum_fmp[, .(MEAN = mean(INSP)), keyby = .(ADP, STRATA, BSAI_GOA)][
][exp_interspersion.programmed.stratum_fmp[STRATA != "ZERO"], on = .(ADP, STRATA, BSAI_GOA)
][, .(DIFF = POOL_DMN_INTERSPERSION - MEAN), by = .(ADP, STRATA, BSAI_GOA)]


### Maps

# TODO Do these plots differ if I feed box definitions or realized_interspersion? It shouldn't!

# TODO Move this to functions
plot_interspersion_map <- function(box_def, real_interspersion, exp_interspersion.realized, exp_interspersion.programmed){
  # box_def <- copy(box_def.stratum_fmp); real_interspersion <- copy(real_interspersion.stratum_fmp);  exp_interspersion.realized <- copy(exp_interspersion.realized.stratum_fmp)
  
  # Make a dt of all year * strata
  year_strata <- unname(unlist(box_def$params[c("year_col", "stratum_cols")]))
  year_strata_dt <- box_def$dmn$geom_dmn_df %>% st_drop_geometry() %>% select(all_of(year_strata)) %>% unique()
  
  realized_boxes <- attr(exp_interspersion.realized.stratum_fmp, "box_expected")$RAW[
  ][attr(real_interspersion, "sampled_boxes"), on = c(year_strata, "BOX_ID")
  ][!is.na(HEX_ID)]

  map_out_lst <- vector(mode = "list", length = nrow(year_strata_dt))
  
  # Make plots for each year_strata
  for(i in 1:nrow(year_strata_dt)) {
    # i <- 1
    
    stratum_map_lst <- list(BOX = NA, HEX_ID.realized = NA, HEX_ID.programmed = NA)
    
    # Subset stratum
    stratum_sub <- merge(box_def$dmn$geom_dmn_df, year_strata_dt[i,], on = year_strata, all.y = T) %>% dplyr::filter(BOX_DMN_n > 0)
    # Extract the spatial extent of the fishing effort
    stratum_sub.bbox <- stratum_sub %>% st_bbox()
    # Subset boxes that were sampled (realized)
    stratum_sub.sampled <- merge(
      stratum_sub, 
      attr(real_interspersion, "sampled_boxes"),
      by = c(year_strata, "BOX_ID"))
    
    # And those not near a sampled neighbor
    stratum_sub.gaps <- setdiff(stratum_sub, stratum_sub.sampled)
    # Plot boxes that didn't have a sample in neighboring boxes with a red outline
    # Plot by BOX (HEX_ID and WEEK)
    # This is way more information than we'd share with anyone else, but it's helpful to analysts to see exactly
    # when/where monitoring/gaps occured. 
    stratum_map_lst[["BOX"]] <- ggplot(stratum_sub) + 
      #geom_sf(data = shp_nmfs %>% sf::st_set_crs(st_crs(3467))) + 
      geom_sf(data = ak_low_res %>% sf::st_set_crs(st_crs(3467)), fill = "gray80") +
      geom_sf(data = nmfs_low_res %>% sf::st_set_crs(st_crs(3467)), fill = NA) +
      geom_sf(aes(fill = BOX_DMN_w)) + 
      facet_wrap(~TIME) + 
      geom_sf(data = stratum_sub.gaps, color = "red", alpha = 0, linewidth = 1) + 
      scale_fill_viridis_c(trans = "log") + 
      coord_sf(xlim = stratum_sub.bbox[c(1,3)], ylim = stratum_sub.bbox[c(2,4)]) + 
      theme(legend.position = "bottom")
    # TODO Knowing which boxes were sampled vs which were neighboring might be informative too! 

    # Get expected probability that each box is sampled and its weight
    stratum_hex_exp <- attr(exp_interspersion.realized.stratum_fmp, "box_expected")$RAW[
    ][year_strata_dt[i,], on = year_strata
    ][, .(HEX_EXP = sum(BOX_DMN_w * BOX_DONOR_SAMPLE_PROB)), keyby = c(year_strata, "HEX_ID")]
    
    # Merge in hex_id geometry
    stratum_hex_exp <- merge(
      box_def$geom_sf %>% select(HEX_ID, geometry) %>% unique(), 
      stratum_hex_exp, 
      by = "HEX_ID"
    )
    #' Recall that box is defined by HEX_ID and week, so we can summarize each HEX_ID over all WEEKs. 
    #' If we subset all the 'actually sampled' boxes, we can total up the weight of trips for each HEX_ID and compare it
    #' to the total expected interspersion of each HEX_ID. The difference of each HEX_ID, realized - expected tells us where
    #' we got more or less monitoring relative to our expectation. Using the difference from the *realized rate* because
    #' we are interested in departures from random sampling. Use the *programmed rate* to see differences relative to 
    #' our goal at the start of the ear
    stratum_hex_real <- realized_boxes[year_strata_dt[i,], on = year_strata][
    ][, .(REALIZED = sum(BOX_DMN_w)), by = c(year_strata, "HEX_ID")]
    stratum_hex <- merge(stratum_hex_exp, stratum_hex_real, on = c(year_strata, "HEX_ID"), all.x = T) %>%
      mutate(REALIZED = ifelse(is.na(REALIZED), 0, REALIZED)) %>%
      mutate(DIFF = REALIZED - HEX_EXP) 
    # Fill colors represent difference in the realized  raw number of trips neighboring a monitored trip relative to
    # expectation
    # Plot by HEX_ID (across WEEK)
    stratum_map_lst[["HEX_ID.realized"]] <- ggplot(stratum_hex) + 
      facet_grid(ADP ~ STRATA, labeller = labeller(STRATA = function(x) gsub("_", " ", x))) +
      geom_sf(data = shp_land, fill = "gray80") +
      geom_sf(data = shp_fmp, color = "black", fill = NA) +
      geom_sf(aes(fill = DIFF), alpha = 0.8) + 
      scale_fill_gradient2() + 
      coord_sf(xlim = stratum_sub.bbox[c(1,3)], ylim = stratum_sub.bbox[c(2,4)]) +
      theme(legend.position = "bottom") + 
      labs(
        fill = "Difference (realized - expected) in number of\ntrips monitored or neighboring a monitored trip",
        subtitle = "Relative to realized monitoring rates")

    # Do the same but with programmed rates
    # Get expected probability that each box is sampled and its weight
    stratum_hex_exp <- attr(exp_interspersion.programmed.stratum_fmp, "box_expected")$RAW[
    ][year_strata_dt[i,], on = year_strata
    ][, .(HEX_EXP = sum(BOX_DMN_w * BOX_DONOR_SAMPLE_PROB)), keyby = c(year_strata, "HEX_ID")]
    
    # Merge in hex_id geometry
    stratum_hex_exp <- merge(
      box_def$geom_sf %>% select(HEX_ID, geometry) %>% unique(), 
      stratum_hex_exp, 
      by = "HEX_ID"
    )
    #' Recall that box is defined by HEX_ID and week, so we can summarize each HEX_ID over all WEEKs. 
    #' If we subset all the 'actually sampled' boxes, we can total up the weight of trips for each HEX_ID and compare it
    #' to the total expected interspersion of each HEX_ID. The difference of each HEX_ID, realized - expected tells us where
    #' we got more or less monitoring relative to our expectation. Using the difference from the *realized rate* because
    #' we are interested in departures from random sampling. Use the *programmed rate* to see differences relative to 
    #' our goal at the start of the ear
    stratum_hex_real <- realized_boxes[year_strata_dt[i,], on = year_strata][
    ][, .(REALIZED = sum(BOX_DMN_w)), by = c(year_strata, "HEX_ID")]
    stratum_hex <- merge(stratum_hex_exp, stratum_hex_real, on = c(year_strata, "HEX_ID"), all.x = T) %>%
      mutate(REALIZED = ifelse(is.na(REALIZED), 0, REALIZED)) %>%
      mutate(DIFF = REALIZED - HEX_EXP) 
    
    stratum_map_lst[["HEX_ID.programmed"]] <- ggplot(stratum_hex) + 
      facet_grid(ADP ~ STRATA, labeller = labeller(
        STRATA = function(x) paste0("Stratum : ", gsub("_", " ", x)),
        ADP = function(x) paste0("Year : ", x))) + 
      geom_sf(data = shp_land, fill = "gray80") +
      geom_sf(data = shp_fmp, color = "black", fill = NA) +
      geom_sf(aes(fill = DIFF), alpha = 0.8) + 
      scale_fill_gradient2() + 
      coord_sf(xlim = stratum_sub.bbox[c(1,3)], ylim = stratum_sub.bbox[c(2,4)]) +
      theme(legend.position = "bottom") + 
      labs(
        fill = "Difference (realized - expected) in number of\ntrips monitored or neighboring a monitored trip",
        subtitle = "Relative to programmed monitoring rates")
    
    map_out_lst[[i]] <- stratum_map_lst
    
  }
  names(map_out_lst) <- apply(year_strata_dt, 1, paste0, collapse = ".")
  
  if(F) {
    
    # TODO Since we also simulated sampling with each rate, we can technically find the 95% range of each HEX_ID and
    # see if our outcome was inside or outside that range? We don't have raw outputs at the moment (a lot of data to save!)
    
    # TODO Helpful to count how many sampled trips/neighbors in each box?
  }
  
  map_out_lst
  
}

interspersion_maps <- plot_interspersion_map(box_def.stratum_fmp, real_interspersion.stratum_fmp, exp_interspersion.realized.stratum_fmp, exp_interspersion.programmed)


interspersion_maps[["2022.OB_TRW"]]$BOX
interspersion_maps[["2022.OB_HAL"]]$BOX


interspersion_maps[["2022.EM_HAL"]]$HEX_ID.realized
interspersion_maps[["2022.EM_POT"]]$HEX_ID.realized
interspersion_maps[["2022.EM_TRW_EFP"]]$HEX_ID.realized
interspersion_maps[["2022.OB_HAL"]]$HEX_ID.realized  #' Can see the Pribilof Islands were below expected given the realized monitoring rate
interspersion_maps[["2022.OB_POT"]]$HEX_ID.realized
interspersion_maps[["2022.OB_TRW"]]$HEX_ID.realized  #' Overall more red than blue, meaning the monitoring was more clumped in space than expected given the realized distribution


interspersion_maps[["2023.EM_HAL"]]$HEX_ID.realized
interspersion_maps[["2023.EM_POT"]]$HEX_ID.realized
interspersion_maps[["2023.EM_TRW_EFP"]]$HEX_ID.realized #' huh --- A bunch of the trips from NE of extent not monitored (NMFS area 640?)
interspersion_maps[["2023.EM_TRW_EFP"]]$BOX 

if(F){
  interspersion_maps[["2023.EM_TRW_EFP"]]$HEX_ID.realized + geom_sf(data = nmfs_low_res, fill = NA)  ()
  hex_id_dat <- attr(exp_interspersion.realized.stratum_fmp, "box_expected")$geom 
  ggplot() + geom_sf(data = ak_low_res) +  geom_sf(data = hex_id_dat, fill = NA) + geom_sf_text(data = hex_id_dat, aes(label = HEX_ID))  # HEX_ID 124
  attr(exp_interspersion.realized.stratum_fmp, "box_expected")$RAW[
  ][ADP == 2023 & STRATA == "EM_TRW_EFP" &HEX_ID  == 124
  ][, .(Total_w = sum(BOX_DMN_w), EXP_ISPN = sum(BOX_DMN_w * BOX_DONOR_SAMPLE_PROB))]
  attr(real_interspersion.stratum_fmp, "sampled_boxes")[ADP == 2023 & STRATA == "EM_TRW_EFP" & BOX_ID %in% c(1548, 1558, 1574, 1594, 2250, 2251)]
  attr(exp_interspersion.realized.stratum_fmp, "box_expected")$RAW[
  ][ADP == 2023 & STRATA == "EM_TRW_EFP" &HEX_ID  == 124
  ][, .(TOTAL_w = sum(BOX_DMN_w)), keyby = TIME < 10]  # 6 trips in weeks 4-5, 13 trips in weeks 12-15 
}
interspersion_maps[["2023.OB_HAL"]]$HEX_ID.realized 
interspersion_maps[["2023.OB_POT"]]$HEX_ID.realized
interspersion_maps[["2032.OB_TRW"]]$HEX_ID.realized 

# TODO These maps only discern gaps from no gaps, not where we stacked our monitoring! See OB_TRW in 2022
# Overall, this isn't really an issue as far as evaluating deployment goes. In terms of getting samples in the BSAI, people might care
# to know we got less than expected.
interspersion_maps[["2022.OB_HAL"]]$HEX_ID.programmed  #' Given the programmed rate, OB_HAL generally had more HEX_IDs with lower interspersion than expected.







# Realized rates by FMP ------------------------------------------------------------------------------------------------

programmed_rates
# Realized rates by FMP (defined by the FMP with the majority of catch)
dcast(
  pc_effort_st[STRATA != "ZERO", .(REALIZED = uniqueN(TRIP_ID[OBSERVED_FLAG == "Y"]) / uniqueN(TRIP_ID)), keyby = .(ADP, STRATA, BSAI_GOA)],
  ADP + STRATA ~ BSAI_GOA, value.var = "REALIZED")
realized_rates.val
#' EM_HAL : Rates in the BSAI were only 0.19 instead of 30, but we still got expected interspersion
#' *OB_HAL : Rates in the BSAI were higher than in the GOA, but interspersion was still lower than mean expected*
#' OB_POT : Rates in the BSAI were 0.25, compared to 0.14 in the GOA, perhaps why interspersion was higher than mean expected.
#' OB_TRW : Rates in the BSAI were 0.35, higher than expected, but interspersion was below mean. 0.275 in 

#' *-------------------------------------------------------------------------------------------------------------------*
# INVESTIGATION #### 
#' *-------------------------------------------------------------------------------------------------------------------*
 # Where did the TWR_EM_EFP vessels offload to? Esp those in weeks 4-5
em_trw_gaps <- box_def.stratum$og_data[ADP == 2023 & STRATA == "EM_TRW_EFP" & TIME %in% 4:5 & HEX_ID == 124]
uniqueN(em_trw_gaps$TRIP_ID)   # 6 unique trips
em_trw_gaps.val <- valhalla[TRIP_ID %in% pc_effort_st[TRIP_ID %in% em_trw_gaps$TRIP_ID, unique(wd_TRIP_ID)]]
# Intersting that these all got coded as 'bottom pollock' or K which is 'Rockfish'
unique(em_trw_gaps.val[, .(ADP, STRATA, TRIP_ID, TRIP_TARGET_DATE, AGENCY_GEAR_CODE, LANDING_DATE, REPORT_ID, TRIP_TARGET_CODE, PORT_CODE, OBSERVED_FLAG, TENDER, TENDER_VESSEL_ADFG_NUMBER)])[order(TRIP_TARGET_DATE)]
em_trw_gaps.val[TRIP_TARGET_CODE == "K"]
em_trw_gaps.val[, .(WEIGHT_POSTED = sum(WEIGHT_POSTED, na.rm = T)), keyby = .(TRIP_ID, TRIP_TARGET_CODE, SPECIES_GROUP_CODE)][order(TRIP_ID, -WEIGHT_POSTED)][, .SD[1:3], keyby = .(TRIP_ID, TRIP_TARGET_CODE)]
dcast(
  unique(valhalla[STRATA == "EM_TRW_EFP" & COVERAGE_TYPE == "PARTIAL", .(ADP, TRIP_ID, TENDER, TRIP_TARGET_CODE)])[
  ][, .(N = uniqueN(TRIP_ID)), keyby = .(ADP, TRIP_TARGET_CODE, TENDER)],
  TRIP_TARGET_CODE + TENDER ~ ADP, value.var = "N", fill = 0, drop = F)
