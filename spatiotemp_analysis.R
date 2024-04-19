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


#' [Note: Geoff has a odds_cancellation_investigation.R script in his local on the ADP GitHub repo with some figures for
#' [looking at ODDS as a time series. Could be useful to investigate deviations from expected rates.
  

# Analyses -------------------------------------------------------------------------------------------------------------


## Data Prep ----

# Wrangle the Valhalla data set for spatiotemporal analyses
pc_effort_st <- spatiotemp_data_prep(work.data)

#' Realized Rates *Check that these match up with the realized rates in 4_AR_report*
realized_rates.val <- unique(pc_effort_st[, .(ADP, STRATA, TRIP_ID, OBSERVED_FLAG)])[
][, .(STRATA_N = .N, SAMPLE_RATE = sum(OBSERVED_FLAG == "Y")/.N), keyby = .(ADP, STRATA)]

# Create a subset table of realized monitored trips
realized_mon <- unique(pc_effort_st[OBSERVED_FLAG == "Y", .(ADP, STRATA, TRIP_ID)])

# Calculate the realized rates of each STRATUM x ADP
programmed_rates <- copy(data.table(partial))[
][, -c("formatted_strat", "GEAR")
][, STRATA := gsub(" ", "_", STRATA)][]
setnames(programmed_rates, old = c("YEAR", "Rate"), new = c("ADP", "SAMPLE_RATE"))
# Add ZERO to programmed rates
programmed_rates <- rbind(programmed_rates, data.table(ADP = 2022:2023, STRATA = "ZERO", SAMPLE_RATE = 0))
# Add STRATA_N to programmed_rates
programmed_rates[, STRATA_N := realized_rates.val[programmed_rates, STRATA_N, on = .(ADP, STRATA)]]
setcolorder(programmed_rates, c("ADP", "STRATA", "STRATA_N", "SAMPLE_RATE"))

#' Check to make sure all year/strata names match between rates objects
if( !fsetequal(unique(realized_rates.val[, .(STRATA, ADP)]), unique(programmed_rates[, .(STRATA, ADP)])) ){
  stop("STRATA and ADP in `realized_rates.val` does not match those in `programmed_rates!`")
}

#' Check to make sure all year/strata names match between pc_effort_st and rates objects!
if( !fsetequal(unique(pc_effort_st[, .(STRATA, ADP)]), unique(programmed_rates[, .(STRATA, ADP)])) ){
  stop("STRATA and ADP in `pc_effort_dt` does not match those in `programmed_rates!`")
}

## Define Boxes ----

# Define boxes, post-stratifying by FMP only
box_def.stratum <- define_boxes(
  data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = T, dmn_lst = list(nst = NULL, st = NULL))

# Define boxes, post-stratifying by FMP only
box_def.stratum_fmp <- define_boxes(
  data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = T, dmn_lst = list(nst = NULL, st = "BSAI_GOA"))

# Define boxes, post-stratifying by FMP and Gear type (for OB to EM and ZE interspersion)
box_def.stratum_gear_fmp <- define_boxes(
  data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = T, dmn_lst = list(nst = "GEAR", st = "BSAI_GOA"))

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
#' We can omit EM TRW EFP if we show the stratum-specific results as the result is duplicative (GOA-only)
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

#' TODO Make a table of values quantiles, mean, median, realized


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

#' `BOX` plots show which boxes were gaps (outlined in red) and the count of trip components in each box (effort density)
#' `HEX_ID.realized` is a yearly summary of each box, comparing the realized count of covered trips versus the average across simulations.

interspersion_maps <- plot_interspersion_map(box_def.stratum_fmp, real_interspersion.stratum_fmp, exp_interspersion.realized.stratum_fmp, exp_interspersion.programmed)

# In 2022 for OB_TRW, gaps in weeks 20-27 (find # of trips)
interspersion_maps[["2022.OB_TRW"]]$BOX
interspersion_maps[["2022.OB_TRW"]]$HEX_ID.realized  #' Overall more red than blue, meaning the monitoring was more clumped in space than expected given the realized distribution

# In 2022 for OB_HAL, many gaps in the BSAI, especially in the middle of the year
interspersion_maps[["2022.OB_HAL"]]$BOX
interspersion_maps[["2022.OB_HAL"]]$HEX_ID.realized  #' Can see the Pribilof Islands were below expected given the realized monitoring rate
# Interestingly, we actually had higher than programmed rates in the BSAI, but it must have been concentrated in the AI and not the BS

# EM_HAL and EM_POT at the time of analysis had many fewer than expected trips in the panhandle. We see more gaps in the latter half of the year when review fell behind. Should see
# this in the temporal plots
interspersion_maps[["2022.EM_HAL"]]$BOX
interspersion_maps[["2022.EM_HAL"]]$HEX_ID.realized

interspersion_maps[["2022.EM_POT"]]$HEX_ID.realized


interspersion_maps[["2022.EM_TRW_EFP"]]$HEX_ID.realized
interspersion_maps[["2022.OB_POT"]]$HEX_ID.realized


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



# Realized vs Expected Interspersion of OB with EM and ZERO ------------------------------------------------------------

#' TODO Do I need to get my old 2024 functions or can I use my existing ones?
#' I don't think I currently have a way of calculating realized interspersion, only the mathematical expectation
#' TODO These summaries can omit the OB_TRW and EM_TRW_EFP strata

# calculate_dmn_interspersion <- function(box_def, selection_rates, acceptor_donor_lst)

box_def.stratum_gear_fmp$dmn$strata_dt
ob_em_ze_adl <- c(
  list(4:5),  # OB_HAL and OB_POT to EM_HAL
  list(4:5),  # OB_HAL and OB_POT to EM_POT
  list(3),    # EM_TRW_EFP to EM_TRW_EFP
  list(4:5),  # OB_HAL and OB_POT to OB_HAL
  list(4:5),  # OB_HAL and OB_POT to OB_POT
  list(6),    # OB_TRW to OB_TRW
  list(4:5)   # OB_HAL and OB_POT to ZERO
)
expected_ob_em_ze_interspersion <- calculate_dmn_interspersion(box_def.stratum_gear_fmp, programmed_rates, ob_em_ze_adl)

#' TODO Don't I have some functions to summarize these outputs?
#' What do I want to summarize? Overall overlap between POOLS? Or Gear-specific within pools? FMP?
expected_dmn_interspersion.summary <- expected_ob_em_ze_interspersion$POOLED[
][GEAR != "TRW"  
][, .(EXP_INSP = sum(BOX_DONOR_SAMPLE_PROB * BOX_DMN_w, na.rm = T) / sum(BOX_DMN_w)), keyby = .(ADP, POOL, BSAI_GOA, GEAR)]

# TODO Get the 'realized' interspersion
# TODO Get the distribution of the expectation given perfectly random sampling (using the prior simulation)

# Programmed Rates Simulation

# TODO Make this function use the acceptor_donor_lst too?

box_def.stratum_gear_fmp$dmn$og_data

# Use this object as the subset of TRIP_IDs that were sampled
realized_mon

#' TODO Move this to the functions folder
#' TODO Add the 'theoretical maximum dmn_INSP' assuming all donor trips were sampled
calculate_realized_dmn_interspersion <- function(box_def, monitored_trips, acceptor_donor_lst) {
  # box_def <- copy(box_def.stratum_gear_fmp); monitored_trips <- copy(realized_mon); acceptor_donor_lst <- copy(ob_em_ze_adl)
  
  # Identify the box definition parameters for quick grouping
  year_strata <- unlist(box_def$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- unlist(box_def$params[c("dmn_cols")], use.names = F)
  yst <- c(year_strata, domains)
  
  # For each domain, identify sampled boxes and neighbors
  box_sample <- copy(box_def$dmn$og_data)[monitored_trips, on = c(year_strata,  "TRIP_ID")]
  box_sample <- unique(subset(box_sample, select = c(yst, "TIME", "HEX_ID", "BOX_ID")))
  
  dmn_sample <- split(box_sample, by = c(yst))
  nbr_sample <- lapply(dmn_sample, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ])
  nbr_sample <- lapply(nbr_sample, function(x) unique(unlist(x)))
  
  yst_col <- paste(yst, collapse = ".")

  nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = yst_col)[
  ][, (yst) := tstrsplit(get(yst_col),  split = "[.]")
  ][, (yst_col) := NULL
  ][, ADP := as.integer(ADP)][]
  
  # Get the total count of trips in each domain
  strata_domain_n <- box_def$dmn$strata_dmn_n_dt
  
  # Use the acceptor_donor_lst to determine which `acceptor` boxes were sampled by `donors`
  
  stratum_out_lst <- vector(mode = "list", length = length(acceptor_donor_lst))
  
  for(i in 1:length(acceptor_donor_lst)) {
    # i <- 1
    
    # Identify the stratum accepting monitoring data
    acceptor_stratum <- box_def$dmn$strata_dt[i,]
    # Get the domain summaries of the acceptor stratum
    acceptor_stratum.domain <- box_def$dmn$box_dmn_smry_dt[acceptor_stratum, on = box_def$params$stratum_cols]
    
    # Identify which strata are donors to the acceptor
    donor_strata <- box_def$dmn$strata_dt[acceptor_donor_lst[[acceptor_stratum$STRATUM_ID]], ]
    # Identify all boxes with monitoring by the donor strata
    donor_strata.boxes <- nbr_sample_dt[donor_strata, on = box_def$params$stratum_cols]
    
    # Identify which domains are relevant to the stratum
    focus_domain_dt <- unique(subset(acceptor_stratum.domain, select = domains))
    
    focus_domain_out_lst <- vector(mode = "list", length = nrow(focus_domain_dt))
    
    # For each domain, total up trips that were neighboring monitoring from the donor strata
    for(j in 1:nrow(focus_domain_dt)) {
      # j <- 1
      focus_domain <- focus_domain_dt[j,]
      # Subset the acceptor stratum's boxes to those with fishing effort
      acceptor.domain.sub <- acceptor_stratum.domain[focus_domain, on = domains][BOX_DMN_n > 0]
      # Identify which boxes in the domain were monitored by donors
      donor.domain.monitored_boxes <- unique(donor_strata.boxes[focus_domain, on = domains]$BOX_ID)
      # Flag the boxes that were monitored by donors
      acceptor.domain.sub[, MON := BOX_ID %in% donor.domain.monitored_boxes]
      # Flag the boxes that had potential to be monitored by donors
      potential_donor.boxes <- unique(box_def$dmn$og_data[donor_strata, on = .(STRATA)][focus_domain, on = c(domains)]$BOX_ID)
      potential_donor.neighbors <- unique(unlist(box_def$nbr_lst[potential_donor.boxes]))
      acceptor.domain.sub[, IN_FRAME := BOX_ID %in% potential_donor.neighbors]

      focus_domain_out_lst [[j]] <- acceptor.domain.sub[
      ][, .(
        dmn_INSP = sum(BOX_DMN_w[MON]),  # actually realized trips neighboring monitored donors
        max_INSP = sum(BOX_DMN_w[IN_FRAME]),  # theoretical max if all donor trips were sampled
        sum_BOX_DMN_w = sum(BOX_DMN_w)   # Total trips of domain
        ), keyby = c(year_strata, domains)
      ]
      
    }
    
    stratum_out_lst[[i]] <- rbindlist(focus_domain_out_lst)

  }
  
  out <- rbindlist(stratum_out_lst)
  setattr(out, "sampled_boxes", nbr_sample_dt)
  out
  
}

realized_dmn_interspersion <- calculate_realized_dmn_interspersion(box_def.stratum_gear_fmp, realized_mon, ob_em_ze_adl)

# Add Pool so we can summarize by POOL and domain
realized_dmn_interspersion[, POOL := fcase(
  STRATA %like% "EM", "EM",
  STRATA %like% "OB", "OB",
  STRATA == "ZERO", "ZERO"
)]
# Generate summaries by pool and domain (FMP and GEAR). Exclude TRW domains
realized_dmn_interspersion.summary <- realized_dmn_interspersion[
][GEAR != "TRW"
][, .(
  dmn_N = sum(sum_BOX_DMN_w), 
  dmn_MAX_INSP = sum(max_INSP),
  dmn_INSP = sum(dmn_INSP)
  ), keyby = .(ADP, POOL, GEAR, BSAI_GOA)
]
realized_dmn_interspersion.summary[, ':=' (INSP = dmn_INSP / dmn_N, MAX_INSP = dmn_MAX_INSP / dmn_N)]

# Combine the realized with expected (mean) and melt for plotting
realized_dmn_interpsersion.comparison <- realized_dmn_interspersion.summary[expected_dmn_interspersion.summary, on = .(ADP, POOL, BSAI_GOA, GEAR)]
realized_dmn_interpsersion.comparison.melt <- data.table::melt(realized_dmn_interpsersion.comparison, id.vars = c("ADP", "POOL", "GEAR", "BSAI_GOA"), measure.vars = c("INSP", "EXP_INSP", "MAX_INSP"))

ggplot(realized_dmn_interpsersion.comparison.melt[variable != "MAX_INSP"], aes(x = BSAI_GOA, y = value, fill = variable)) +
  facet_grid(ADP ~ POOL + GEAR) + 
  geom_col(position = position_dodge()) + 
  theme(legend.position = "bottom") + 
  geom_point(data = realized_dmn_interpsersion.comparison.melt[variable == "MAX_INSP"], color = "blue")
# Realized is read, expectation (mathematical mean) is green, theoretical maximum (all donor boxes monitored) is blue
# OB-EM Generally looks as expected across FMP and Gear types for both years
# OB-OB comparisons show OB HAL was a little low in 2022, which matches previous summary (even though realized rates were higher in the BSAI, it was biased towards BS and away from AI)
# OB-ZE was below expected for HAL gear trips in both 2022 and 2023, POT gear was generally as expected.
#' *NOTE* because of the changes to stratification/allocation in 2024, the 'expected' interspersion for all BSAI 
#' comparisons should greatly improve (closer to the theoretical maximums), with small compromise to GOA
#' Does it make sense to have interspersion on the y-axis here rather than on the x? Just need to flip my distributions...
#' Better to have "up is better" or "right is better?

# TODO Fixing define_boxes
data <- copy(pc_effort_st); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; stratum_cols <- "STRATA"; geom <- T; dmn_lst <- list(nst = "GEAR", st = "BSAI_GOA"); ps_cols <- NULL


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
