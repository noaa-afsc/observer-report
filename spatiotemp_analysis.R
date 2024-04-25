library(data.table)
library(ggplot2)
library(FMAtools)
library(ggh4x)       # facet_grid2 has cleaner facet labels while allowing for independent x and/or y scales
library(sf)          # for manipulating simpler feature objects and mapping
library(sfheaders)   # for sf_remove_holes?  also sf_bbox() to calculate the bbox from the geometry, like st_bbox?
library(dplyr)       # For piping and some data munging functions

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

#' We will also capture the HEX_ID-level summaries for the spatial analyses
#' TODO FOR THE FINAL RUN USE [iter = 1e4]

# Programmed rates
sim.programmed.stratum <- simulate_interspersion(box_def.stratum, programmed_rates, iter = 1e3, seed = 12345,  hex_smry = T)
# Realized rates
sim.realized.stratum <- simulate_interspersion(box_def.stratum, realized_rates.val, iter = 1e3, seed = 12345, hex_smry = T)

#' `Quickload`
# save(sim.programmed.stratum, sim.realized.stratum, file = "output_data/spatiotemp_stratum_insp_i1e4.rdata")
# load("output_data/spatiotemp_stratum_insp_i1e4.rdata")

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
plot.interspersion.stratum <- plot_interspersion_density(density.stratum, real_interspersion.stratum, dmn_N.stratum) + 
  facet_grid2(ADP ~ STRATA, scales = "free", independent = "x", labeller = labeller(STRATA = function(x) gsub("_", " ", x)))
#' *So it looks like OB_HAL had random sampling but had lower than expected distribution that expected by the PROGRAMMED rates*
#' *Also, OB_TRW had lower than expected interspersion but still very high (0.975). Monitoring was not randomly distributed!*
if(F){
  ggsave(plot = plot.interspersion.stratum, filename = "output_data/interspersion.stratum.png", width = 8, height = 4)
}
programmed_rates[STRATA == "OB_TRW"]    # programmed 0.2965
realized_rates.val[STRATA == "OB_TRW"]  # realized   0.2873, so pretty close
# FIXME - what is up with the 2023 EM_TRW_EFP distribution. Super highly-clumped except for a handful of trips?
programmed_rates[STRATA == "EM_TRW_EFP"]    # programmed 0.2965
realized_rates.val[STRATA == "EM_TRW_EFP"]  # realized   0.2873, so pretty close
#* In 2023, also fell way below the expected distributions?


percentile.stratum <- insp_percentile(real_interspersion.stratum, sim.programmed.stratum, sim.realized.stratum)
percentile.stratum[, c("PROG_SIG", "REAL_SIG") := lapply(.SD, function(x) {
  fcase(x < 0.025, "<<", x < 0.05, "<", x > 0.975, ">>", x > 0.95, ">", default = "")
}), .SDcols = c("PERC_PROG", "PERC_REAL")]
colnames(percentile.stratum) <- gsub("_", " ", colnames(percentile.stratum))
tbl.percentile.stratum <- percentile.stratum %>%
  flextable() %>%
  colformat_int(j = 1, big.mark = "") %>%
  colformat_double(j = 3, digits = 2) %>%
  colformat_double(j = c(5, 6, 7), digits = 4) %>%
  merge_v(j = 1) %>%
  hline(i = which(diff(percentile.stratum$ADP) > 0)) %>%
  fix_border_issues() %>% 
  autofit()
save_as_docx(tbl.percentile.stratum, path = "output_data/spatiotemp_percentile_stratum.docx")

# OB_TRW in 2022 was low, although rates were within the expected range.


## Split by Stratum and FMP (BSAI and GOA) -----------------------------------------------------------------------------

# Expected interspersion given the programmed and realized rates
exp_interspersion.programmed.stratum_fmp <- calculate_expected_interspersion(box_def.stratum_fmp, programmed_rates)
exp_interspersion.realized.stratum_fmp <- calculate_expected_interspersion(box_def.stratum_fmp, realized_rates.val)

# Actually realized interspersion
real_interspersion.stratum_fmp <- calculate_realized_interspersion(box_def.stratum_fmp, realized_mon)

### Simulate trip selection to create distributions of interspersion ----

# Programmed rates
sim.programmed.stratum_fmp <- simulate_interspersion(box_def.stratum_fmp, programmed_rates, iter = 1e4, seed = 12345)
# Realized rates
sim.realized.stratum_fmp <- simulate_interspersion(box_def.stratum_fmp, realized_rates.val, iter = 1e4, seed = 12345)

#' `Quickload`
# save(sim.programmed.stratum_fmp, sim.realized.stratum_fmp, file = "output_data/spatiotemp_stratum_fmp_insp_i1e4.rdata")
# load("output_data/spatiotemp_stratum_fmp_insp_i1e4.rdata")

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


#' *When split by FMP, we see more patterns!*

#' Plot distributions vs actually realized
#' We can omit EM TRW EFP if we show the stratum-specific results as the result is duplicative (GOA-only)
plot.interspersion.stratum_fmp <- plot_interspersion_density(density.stratum_fmp, real_interspersion.stratum_fmp, dmn_N.stratum_fmp) + 
  facet_grid2(ADP + BSAI_GOA ~ STRATA, render_empty = F, scales = "free", independent = "x", labeller = labeller(
    STRATA = function(x) gsub("_", " ", x),
    BSAI_GOA = function(x) paste0("FMP : ", x)))
if(F){
  ggsave(plot = plot.interspersion.stratum_fmp, filename = "output_data/interspersion.stratum_fmp.png", width = 8, height = 7)
}

#' *NOTE* When post-stratifying the BSAI and GOA, we are still using a stratum-wide selection rate, not the rates realized in BSAI vs GOA

#' *OB_HAL was in the expected distributions in the GOA, and just within the expectation of the programmed rate in the BSAI*
real_interspersion.stratum_fmp[ADP == 2022 & STRATA == "OB_HAL" & BSAI_GOA == "BSAI"]                             #  0.5670  (realized)
density.stratum_fmp[ADP == 2022 & STRATA == "OB_HAL" & BSAI_GOA == "BSAI" & quant == 0 & DIST == "REAL", max(X)]  #  0.4741  (lower exp of realized rate)
density.stratum_fmp[ADP == 2022 & STRATA == "OB_HAL" & BSAI_GOA == "BSAI" & quant == 0 & DIST == "PROG", max(X)]  #  0.5684  (lower exp of programmed rate)
#' TODO Get the percentile 
#' *Good news is that given all realized interspersion met the expectation of the realized rates, it means we had randomly distributed monitoring*

#' *In the AK-wide plots, OB_TRW was not randomly distributed, but in the FMP-specific plots, both were barely within the expected range?*
real_interspersion.stratum_fmp[ADP == 2022 & STRATA == "OB_TRW" & BSAI_GOA == "GOA"]                              #  0.9853
density.stratum_fmp[ADP == 2022 & STRATA == "OB_TRW" & BSAI_GOA == "GOA" & quant == 0, .(Lower_Exp = max(X)), by = "DIST"] # 0.9836 and 0.9844
sim.programmed.stratum_fmp[ADP == 2022 & STRATA == "OB_TRW" & BSAI_GOA == "GOA"]


#' TODO The attributes of the `real_interspersion.stratum_fmp` object has weird names



# Calculate the percentile of realized interspersion given the programmed and realized rates
percentile.stratum_fmp <- insp_percentile(real_interspersion.stratum_fmp, sim.programmed.stratum_fmp, sim.realized.stratum_fmp)
percentile.stratum_fmp[, c("PROG_SIG", "REAL_SIG") := lapply(.SD, function(x) {
  fcase(x < 0.025, "<<", x < 0.05, "<", x > 0.975, ">>", x > 0.95, ">", default = "")
}), .SDcols = c("PERC_PROG", "PERC_REAL")]
colnames(percentile.stratum_fmp) <- gsub("_", " ", colnames(percentile.stratum_fmp))

tbl.percentile.stratum_fmp <- percentile.stratum_fmp %>%
  flextable() %>%
  colformat_int(j = 1, big.mark = "") %>%
  colformat_double(j = 4, digits = 2) %>%
  colformat_double(j = c(6:8), digits = 4) %>%
  merge_v(j = 1) %>%
  hline(i = which(diff(percentile.stratum_fmp$ADP) > 0)) %>%
  fix_border_issues()
save_as_docx(tbl.percentile.stratum_fmp, path = "output_data/spatiotemp_percentile_stratum_fmp.docx")


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

# TODO make a copy of this but feed in simulation results rather than the summaries/means

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


interspersion_maps[["2022.OB_POT"]]$HEX_ID.realized

interspersion_maps[["2022.EM_TRW_EFP"]]$HEX_ID.realized  #' In 2022, had FEWER gaps than expected near Kodiak
interspersion_maps[["2023.EM_TRW_EFP"]]$HEX_ID.realized  #' In 2023, the opposite, MORE gaps near Kodiak


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

if(F){
  p_lst <- lapply(interspersion_maps, "[[", "HEX_ID.realized")
  for(i in seq_along(p_lst)) {
    ggsave(plot = p_lst[[i]], filename = paste0("output_data/spatiotemp_map.", names(p_lst)[[i]], ".png"), width = 6, height = 6, units = "in")
  }
}





#' TODO : If I get the results of each BOX then I should technically be able to see where I fell out of the expected DISTRIBTUION, 
#' not just relative to the mean!

# Interspersion maps by percentile ####
plot_interspersion_map2 <- function(box_def, real_interspersion, sim.real, sim.prog){
  # box_def <- copy(box_def.stratum); real_interspersion <- copy(real_interspersion.stratum);  sim.real <- copy(sim.realized.stratum); sim.prog <- copy(sim.programmed.stratum)

  year_strata <- unname(unlist(box_def$params[c("year_col", "stratum_cols")]))
  year_strata_dt <- box_def$dmn$geom_dmn_df %>% st_drop_geometry() %>% select(all_of(year_strata)) %>% unique()
  
  # Count the number of unique trips that fished in each HEX_ID
  hex_id_trips.N <- box_def$og_data[, .(N = uniqueN(TRIP_ID)), keyby = c(year_strata, "HEX_ID")][STRATA != "ZERO"]

  # Count the realized number of trips monitored in each HEX_ID
  hex_id_trips.real_n <- box_def$og_data[TRIP_ID %in% realized_mon$TRIP_ID, .(real_n = uniqueN(TRIP_ID)), keyby = c(year_strata, "HEX_ID")]
  
  # Count the number of sampled trips in the simulations using the realized rates
  
  trips_hex_dt <- unique(box_def$og_data[, .(ADP, STRATA, HEX_ID, TRIP_ID)])
  hex_id_trips.sim.real <- rbindlist(lapply(attr(sim.real, "mon_lst"), function(x) {
    # x <- attr(sim.real, "mon_lst")[[1]]
    trips_hex_dt[data.table(TRIP_ID = x), on = .(TRIP_ID)][, .(sim_n = .N), keyby = c(year_strata, "HEX_ID")]
  }), idcol = "ITER")
  hex_id_trips.sim.prog <- rbindlist(lapply(attr(sim.prog, "mon_lst"), function(x) {
    # x <- attr(sim.real, "mon_lst")[[1]]
    trips_hex_dt[data.table(TRIP_ID = x), on = .(TRIP_ID)][, .(sim_n = .N), keyby = c(year_strata, "HEX_ID")]
  }), idcol = "ITER")
 
  hex_id_trips.N        # Total trips in each HEX_ID
  hex_id_trips.real_n   # realized n in each hex_id
  hex_id_trips.sim.real # simulated n using realized rates
  
  sim_iter <- length(attr(sim.prog, "mon_lst"))
  
  t1 <- hex_id_trips.real_n[hex_id_trips.N, on = c(year_strata, "HEX_ID")]
  t2 <- hex_id_trips.sim.real[t1, on = c(year_strata, "HEX_ID")]
  t2[is.na(sim_n), sim_n := 0]
  t2[is.na(real_n), real_n := 0]
  t2[, MED_sim_n := median(sim_n), keyby = c(year_strata, "HEX_ID", "N", "real_n")]
  t2[, DIR := sign(real_n - MED_sim_n)]
  t3 <- t2[, .(MORE_EXTREME = fcase(
    DIR == 1, sum(real_n > sim_n)/sim_iter,
    DIR == 0, 0,
    DIR == -1, sum(real_n < sim_n)/sim_iter
  )), keyby = c(year_strata, "HEX_ID", "N", "real_n", "MED_sim_n", "DIR")]
  # Merge STRATA_N in
  t3[, STRATA_N := box_def$strata_n_dt[t3, STRATA_N, on = c(year_strata)]]
  
  # Merge geometry in
  hex_id_geom <- box_def$geom_sf %>% select(HEX_ID, geometry) %>% unique()
  sim.real.perc <- merge(hex_id_geom, t3, by = "HEX_ID")

  # Relative to the number of trips in the hex_Id, scaled for each STRATUM
  ggplot(sim.real.perc) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (real_n - MED_sim_n) / STRATA_N)) + 
    scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") +
    labs(fill = "Difference in # of trips monitored vs expected / STRATA_N")
  
  # The scale makes more sense by number of trips if we can plot each stratum separately
  ggplot(sim.real.perc) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (real_n - MED_sim_n) / STRATA_N, linewidth = abs(MORE_EXTREME) )) + 
    # geom_sf_text(aes(label = HEX_ID), size = 2) + 
    scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") +
    scale_linewidth_continuous(range = c(0.1, 2)) + 
    labs(fill = "Difference in # of trips monitored vs expected / STRATA_N")
  
  #' *THESE PLOTS ARE MY BEST SO FAR*
  # In 2023, can see OB_HAL had 20 more trips than the median observed in one cell!
  ggplot(sim.real.perc %>% filter(ADP == 2023 & STRATA == "OB_HAL") %>% arrange(abs(MORE_EXTREME))) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (real_n - MED_sim_n) / STRATA_N, linewidth = abs(MORE_EXTREME) )) + 
    geom_sf_text(aes(label = HEX_ID), size = 2) + 
    scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") +
    scale_linewidth_continuous(range = c(0.1, 2)) + 
    labs(fill = "Difference in # of trips monitored vs expected / STRATA_N")
  sim.real.perc %>% filter(ADP == 2023 & STRATA == "OB_HAL" & HEX_ID == 115)
  # Realized 42 out of 108, expected 21
  
  ggplot(sim.real.perc %>% filter(ADP == 2022 & STRATA == "OB_HAL") %>% arrange(abs(MORE_EXTREME))) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (real_n - MED_sim_n), linewidth = abs(MORE_EXTREME) )) + 
    geom_sf_text(aes(label = HEX_ID), size = 2) + 
    scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") +
    scale_linewidth_continuous(range = c(0.1, 2)) + 
    labs(fill = "Difference in # of trips monitored vs expected / STRATA_N")
  # Here we can see we have less monitoring in the AI, more in the BS
  
  ggplot(sim.real.perc %>% filter(ADP == 2022 & STRATA == "OB_TRW") %>% arrange(abs(MORE_EXTREME))) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (real_n - MED_sim_n), linewidth = abs(MORE_EXTREME) )) + 
    geom_sf_text(aes(label = HEX_ID), size = 2) + 
    scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") +
    scale_linewidth_continuous(range = c(0.1, 2)) + 
    labs(fill = "Difference in # of trips monitored vs expected / STRATA_N")
  sim.real.perc %>% filter(ADP == 2022 & STRATA == "OB_TRW" & HEX_ID == 78)
  # 163 trips, 47 was median, realized 62, which was more than 98.9% of simulations
  
  ggplot(sim.real.perc %>% filter(ADP == 2022 & STRATA == "EM_POT") %>% arrange(abs(MORE_EXTREME))) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (real_n - MED_sim_n), linewidth = abs(MORE_EXTREME) )) + 
    geom_sf_text(aes(label = HEX_ID), size = 2) + 
    scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") +
    scale_linewidth_continuous(range = c(0.1, 2)) + 
    labs(fill = "Difference in # of trips monitored vs expected / STRATA_N")
  
  
  #' * What if we filled by MORE_EXTREME and text with trips different?
  ggplot(sim.real.perc %>% filter(ADP == 2022 & STRATA == "EM_POT") %>% arrange(abs(MORE_EXTREME))) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = DIR * MORE_EXTREME)) + 
    geom_sf(data = fmp_low_res, alpha = 0) + 
    geom_sf_text(aes(label = (real_n - MED_sim_n)), size = 2) + 
    scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") +
    scale_linewidth_continuous(range = c(0.1, 2)) + 
    labs(fill = "Proportion of outcomes that realized value was more extreme than")
  
  # This explains why OB_TRW in 2022 is near the tails. 
  ggplot(sim.real.perc %>% filter(ADP == 2022 & STRATA == "OB_TRW") %>% arrange(abs(MORE_EXTREME))) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = DIR * MORE_EXTREME)) + 
    geom_sf(data = fmp_low_res, alpha = 0) + 
    geom_sf_text(aes(label = (real_n - MED_sim_n)), size = 2) + 
    scale_fill_gradient2(midpoint = 0, limits = c(-1,1)) + theme(legend.position = "bottom") +
    scale_linewidth_continuous(range = c(0.1, 2)) + 
    labs(fill = "Proportion of outcomes that realized value was more extreme than")
  
  ggplot(sim.real.perc %>% filter(ADP == 2022 & STRATA == "OB_HAL") %>% arrange(abs(MORE_EXTREME))) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(data = ak_low_res, fill = "gray80") +
    geom_sf(data = fmp_low_res, alpha = 0) + 
    geom_sf(aes(fill = DIR * MORE_EXTREME)) + 
    geom_sf_text(aes(label = abs(real_n - MED_sim_n)), size = 3) + 
    scale_fill_gradient2(midpoint = 0, limits = c(-1,1)) + theme(legend.position = "bottom") +
    scale_linewidth_continuous(range = c(0.1, 2)) + 
    labs(fill = "Proportion of outcomes that realized value was more extreme than")
  
  ggplot(sim.real.perc %>% filter(ADP == 2023 & STRATA == "OB_HAL") %>% arrange(abs(MORE_EXTREME))) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(data = ak_low_res, fill = "gray80") +
    geom_sf(data = fmp_low_res, alpha = 0) + 
    geom_sf(aes(fill = DIR * MORE_EXTREME)) + 
    geom_sf_text(aes(label = abs(real_n - MED_sim_n)), size = 3) + 
    scale_fill_gradient2(midpoint = 0, limits = c(-1,1)) + theme(legend.position = "bottom") +
    scale_linewidth_continuous(range = c(0.1, 2)) + 
    labs(fill = "Proportion of outcomes that realized value was more extreme than")
  
  # Get the extent of the hex cells
  hex_bbox <- hex_id_geom %>% st_bbox()
  
  
  p1 <- ggplot(sim.real.perc) + 
    facet_grid(STRATA ~ ADP) + 
    #geom_sf(data = ak_low_res, fill = "gray80") +
    #geom_sf(data = fmp_low_res, alpha = 0) + 
    geom_sf(aes(fill = DIR * MORE_EXTREME)) + 
    #geom_sf_text(aes(label = abs(real_n - MED_sim_n)), size = 3) + 
    #scale_fill_gradient2(midpoint = 0, limits = c(-1,1), low = "purple", high = "green") + 
    scale_fill_gradientn(
      limits = c(-1,1), 
      colors = c("purple", "plum", "white", "white", "white", "greenyellow", "green"),
      values = c(0, 0.05, 0.3, 0.5, 0.7, 0.95, 1)) + 
    theme(
      legend.position = "bottom",
      axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
    scale_linewidth_continuous(range = c(0.1, 2)) + 
    coord_sf(xlim = hex_bbox[c(1,3)], ylim = hex_bbox[c(2,4)]) + 
    labs(fill = "Proportion of outcomes that realized value was more extreme than") 
  # This plots are nice, but using neighbors and monitored trips does tell you more about where regionally sampling was more or less than, right?
  # Here, at least I can get a reasonable number of different outcomes for each hex_id (number of trips monitored), whereas
  # with the hex_smry, its sum(sampled/unsampled * weight), which apparently have fewer distinct outcomes from which to make a distribution
  # As these are strictly spatial, they don't tell me how my sampled trips are distributed in each HEX_ID through time (i could have a bunch of trips
  # sampled in one hex_id but they could either be perfectly distributed through time with effort OR be super clumped, but I wouldn't be able to tell the difference)
  # We are simply looking at which spatial cells were sampled more
  
  
  #' TODO If there are few outcomes for a HEX_ID, percentile doesn't really make much sense
  if(F) {
    
    # Test to see if our outome is MORE EXTREME than 95% of cases
    
    sim.real
    
    
    sim.real.perc <- realized_mon_hex[
    ][attr(sim.real, "hex_smry"), on = c(year_strata, "HEX_ID")]
    #' TODO I DO GET DIFFERENT INTERPRETATIONS WHEN I USE *mean(HEX_w_)* and *median(HEX_w)*. Some cells change color!
    #' This is because the simulated distributions might span across the realized value
    #' *FIXME* Just testing but using mean instead of median here
     sim.real.perc[, MED_w := median(HEX_w), keyby = c(year_strata, "HEX_ID")]  # Calculate the mean across simuation iterations
    #sim.real.perc[, MED_w := mean(HEX_w), keyby = c(year_strata, "HEX_ID")]  # Calculate the mean across simuation iterations
    # Indicate the direction that the realized value is relative to the median of each HEX_ID
    sim.real.perc[, DIR := sign(REAL_w - MED_w), keyby = c(year_strata, "HEX_ID")]
    # Count number of instances where the realized value was more extreme
    sim.real.perc <- sim.real.perc[, .(MORE_EXTREME = fcase(
      DIR == 1, sum(REAL_w > HEX_w)/.N,
      DIR == -1, -sum(REAL_w < HEX_w)/.N,
      DIR == 0, 0
    )), keyby = c(year_strata, "HEX_ID", "HEX_w_total", "REAL_w", "DIR", "MED_w")]
    sim.real.perc[, MED_DIFF := REAL_w - MED_w]
    # Merge STRATA_N in
    sim.real.perc[, STRATA_N := box_def$strata_n_dt[sim.real.perc, STRATA_N, on = c(year_strata)]]
    
    hex_id_geom <- box_def$geom_sf %>% select(HEX_ID, geometry) %>% unique()
    sim.real.perc <- merge(hex_id_geom, sim.real.perc, by = "HEX_ID")

    #' this plot shows which spatial cells were monitored more or less relative to the median of the simulations (i.e, counts how many
    #' simulation iterations where the realized value was MORE EXTREME than the median)
    p2 <- ggplot(sim.real.perc) + 
      facet_grid(STRATA ~ ADP) + 
      geom_sf(aes(fill = MORE_EXTREME)) + 
      scale_fill_gradientn(
        limits = c(-1,1), 
        colors = c("purple", "plum", "white", "white", "white", "greenyellow", "green"),
        values = c(0, 0.05, 0.3, 0.5, 0.7, 0.95, 1)) + 
      theme(
        legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      labs(fill = "Proportion of iterations where the\nrealized monitoring was more extreme")
    
    library(gridExtra)
    p1_grob <- ggplotGrob(p1)
    p2_grob <- ggplotGrob(p2)
    grob_widths <- grid::unit.pmax(p1_grob$widths, p2_grob$widths)
    p1_grob$widths <- grob_widths
    p2_grob$widths <- grob_widths
    grid.arrange(p1_grob)
    grid.arrange(p2_grob)
    # p2 speaks more to how well represented the fishing effort in the cell was, whereas p1 speaks only to how many times the cell was monitored
    # p2 is weighted by the amounting of fishing through time of each cell, p1 is only weighted by total trips
    # p2 suffers in that number of trips that is off is not shown - I should care more about missing spatial cells with more trips
    
    # EM_HAL and EM_POT throw things off
    ggplot(sim.real.perc %>% filter(!(STRATA %like% "EM"))) + 
      facet_grid(STRATA ~ ADP) + 
      geom_sf(aes(fill = MED_DIFF)) + 
      scale_fill_gradient2(midpoint = 0, low = "purple", high = "green") +
      theme(
        legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      labs(fill = "difference in gap trips relative to median")
    # I still don't see how OB_HAL in 2022 in the TS distributions is at the 'expected' when half of tiles are purple, and none green
    
    
    ggplot(sim.real.perc %>% filter(!(STRATA %like% "EM"))) + 
      facet_grid(ADP ~ STRATA) + 
      geom_sf(aes(fill = MED_DIFF / STRATA_N)) + 
      # scale_fill_gradientn(
      #   limits = c(-1,1), 
      #   colors = c("purple", "plum", "white", "white", "white", "greenyellow", "green"),
      #   values = c(0, 0.05, 0.3, 0.5, 0.7, 0.95, 1)) +
      scale_fill_gradient2(midpoint = 0, low = "purple", high = "green") +
      theme(
        legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      labs(fill = "difference in gap trips relative to median")
    
    sim.real.perc %>% st_drop_geometry() %>% filter(ADP == 2023 & STRATA == "OB_TRW")
    # realized interspersion was above median, so why don't I see more green? My realized appears to be 2.3 trips below median
    sim.real.perc %>% st_drop_geometry() %>% filter(ADP == 2023 & STRATA == "OB_TRW") %>% summarize(real_INSP = sum(REAL_w))
    real_interspersion %>% filter(ADP == 2023 & STRATA == "OB_TRW")  # 0.9964485
    sim.realized.stratum %>% filter(ADP == 2023 & STRATA == "OB_TRW") %>% summarise(MEAN = mean(INSP), MED = median(INSP))  # Both around 0.993
    # is looking at the median not desirable for the plots?
    
    sim.real.perc %>% st_drop_geometry() %>% filter(ADP == 2022 & STRATA == "OB_POT")
    sim.real.perc %>% st_drop_geometry() %>% filter(ADP == 2022 & STRATA == "OB_POT") %>% summarize(sum(MED_DIFF))  # If im 35 trips below mean( or 40 below median) explain distribution/
    sim.real.perc %>% st_drop_geometry() %>% filter(ADP == 2022 & STRATA == "OB_POT") %>% summarize(sum(REAL_w / STRATA_N)) # realized  0.96520
    sim.realized.stratum %>% filter(ADP == 2022 & STRATA == "OB_POT") %>% summarise(MEAN = mean(INSP), MED = median(INSP))  # Median at 0.96607
    # TODO the distributions are showing the median (50th quantile) right?
    
    ggplot(sim.real.perc %>% filter(STRATA == "OB_POT" & ADP == 2022)) + 
      facet_grid(STRATA ~ ADP) + 
      geom_sf(aes(fill = MED_DIFF)) + 
      scale_fill_gradient2(midpoint = 0, low = "purple", high = "green") +
      theme(
        legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      labs(fill = "difference in gap trips relative to median")
    
    # Marking hex_ids where the result was more extreme than 95% of simulation iterations
    # Fill color represents whether the hex_id got relatively more (purple) or fewer (green) gaps than expected
    #' *IS THIS GOOD?*
    #' My goal is to show for each spatial cell whether we got more or less representation than expected, and whether 
    #' the realized representation was 'unusual'. The difference is relative to each cell (scaled by the # of total tripsin the cell, not the stratum as a whole)
    
    #' *MARKING 'UNUSUAL' HERE*
    sim.real.perc <- sim.real.perc %>% mutate(SIG = ifelse(abs(MORE_EXTREME) > 0.95, "•", ""))  # dot created from alt+numpad7
    hex_bbox <- hex_id_geom %>% st_bbox()
    
    ggplot(sim.real.perc) + 
      facet_grid(STRATA ~ ADP) + 
      geom_sf(data = fmp_low_res, fill = NA) + 
      geom_sf(aes(fill = MED_DIFF / HEX_w_total)) + 
      geom_sf_text(aes(label = SIG), hjust = 0.35, vjust = 0.4) + 
      scale_fill_gradient2(midpoint = 0, low = "purple", high = "green") +
      coord_sf(xlim = hex_bbox[c(1,3)], ylim = hex_bbox[c(2,3)]) +
      theme(
        legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      labs(fill = "difference in gap trips relative to median")
    
    
    ggplot(sim.real.perc) + 
      facet_grid(ADP ~ STRATA) + 
      geom_sf(data = ak_low_res) + 
      geom_sf(aes(fill = MED_DIFF / HEX_w_total)) + 
      geom_sf(data = fmp_low_res, fill = NA, linetype = 2) + 
      stat_sf_coordinates(data = sim.real.perc %>% filter(SIG != ""), shape = 21) + 
      scale_fill_gradient2(midpoint = 0, low = "purple", high = "green") +
      coord_sf(xlim = hex_bbox[c(1,3)], ylim = hex_bbox[c(2,3)]) +
      theme(
        legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      labs(fill = "difference in gap trips relative to median")
    
    
    #'*===========================*
    # MY BEST FIGS ----
    #'*===========================*
    
    
    # 2022
    ggplot(sim.real.perc %>% filter(ADP == 2022)) + 
      facet_nested_wrap(. ~ STRATA, nrow = 3, dir = "v", labeller = labeller(STRATA = function(x) paste0(2022, " : ", gsub("_", " ", x)))) + 
      geom_sf(data = ak_low_res) + 
      geom_sf(aes(fill = MED_DIFF / HEX_w_total)) + 
      geom_sf(data = fmp_low_res, fill = NA, linetype = 2) + 
      stat_sf_coordinates(data = sim.real.perc %>% filter(ADP == 2022 & SIG != ""), shape = 21) + 
      scale_fill_gradient2(midpoint = 0, low = "purple", high = "green") +
      coord_sf(xlim = hex_bbox[c(1,3)], ylim = hex_bbox[c(2,4)]) +
      theme(
        legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      labs(fill = "Difference in coverage relative to expectation")

    # 2023
    ggplot(sim.real.perc %>% filter(ADP == 2023)) + 
      facet_nested_wrap(. ~ STRATA, nrow = 3, dir = "v", labeller = labeller(STRATA = function(x) paste0(2023, " : ", gsub("_", " ", x)))) + 
      geom_sf(data = ak_low_res) + 
      geom_sf(aes(fill = MED_DIFF / HEX_w_total)) + 
      geom_sf(data = fmp_low_res, fill = NA, linetype = 2) + 
      stat_sf_coordinates(data = sim.real.perc %>% filter(ADP == 2023 & SIG != ""), shape = 21) + 
      scale_fill_gradient2(midpoint = 0, low = "purple", high = "green") +
      coord_sf(xlim = hex_bbox[c(1,3)], ylim = hex_bbox[c(2,4)]) +
      theme(
        legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      labs(fill = "Difference in coverage relative to expectation")
    
    #' What do values of -1 and +1 mean? They correspond to the proportion of coverage achieved relative to expectation
    #' -1 means we didn't get any data when we expected all of it, +1 means we got all data when we expected none of it
    #' Doesn't tell you the magnitude of gaps covered relative to the stratum - just the spatial representation
    sim.real.perc %>% filter(ADP == 2022) %>% st_drop_geometry() %>% mutate(METRIC = MED_DIFF / HEX_w_total) %>% arrange(METRIC)
    sim.real.perc %>% filter(ADP == 2022) %>% st_drop_geometry() %>% mutate(METRIC = MED_DIFF / HEX_w_total) %>% arrange(-METRIC)
    

    #'*===========================*
    
    
    
    
    
    what <- copy(sim.real.perc)
    what <- what %>% mutate(
      POOL = ifelse(STRATA %like% "EM", "EM", "OB"),
      GEAR = gsub("^.*_", "", STRATA))
    ggplot(what %>% filter(ADP == 2022)) + 
      facet_grid(GEAR ~ ADP + POOL) + 
      geom_sf(data = ak_low_res) + 
      geom_sf(aes(fill = MED_DIFF / HEX_w_total)) + 
      geom_sf(data = fmp_low_res, fill = NA, linetype = 2) + 
      stat_sf_coordinates(data = what %>% filter(ADP == 2022 & SIG != ""), shape = 21) + 
      scale_fill_gradient2(midpoint = 0, low = "purple", high = "green") +
      coord_sf(xlim = hex_bbox[c(1,3)], ylim = hex_bbox[c(2,4)]) +
      theme(
        legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      labs(fill = "Difference in coverage relative to expectation")
    
    # The fill color and its darkness tells you how many more trips were gaps (red) or near a monitored trip (blue) relative to median
    # The color and thickness of the outline of a cell indicates the proportion of outcome where the realized value was more extreme
    ggplot(sim.real.perc %>% filter(!(STRATA %like% "EM")) %>% arrange(ADP, STRATA, abs(MORE_EXTREME))) + 
    #ggplot(sim.real.perc) + 
      facet_grid(STRATA ~ ADP) + 
      geom_sf(aes(fill = MED_DIFF, color = MORE_EXTREME, linewidth = abs(MORE_EXTREME))) + 
      geom_sf_text(aes(label = HEX_ID), size = 2) + 
      scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") + 
      scale_color_gradient2(midpoint = 0) + theme(legend.position = "bottom") + 
      labs(
        fill = "Number of realized monitored trips\ndifferent from the median",
        color = "Proportion of iterations where the\nrealized monitoring was more extreme") + 
      scale_linewidth_continuous(range = c(0, 1.5))
    
    
    
    # HEX_ID 64 (and 56) is where I had more gaps in the Pribs in OB_HAL in 2022 than I would have expected
    box_def$og_data[ADP == 2022 & STRATA == "OB_HAL" & HEX_ID == 64]
    box_def$og_data[ADP == 2022 & STRATA == "OB_HAL" & HEX_ID == 64, uniqueN(TRIP_ID)]  # 20 trips occurred in this box
    box_def$dt_out[ADP == 2022 & STRATA == "OB_HAL" & HEX_ID == 64, sum(BOX_w)] # total weight of 12.4166 trips
    
    realized_mon_box[ADP == 2022 & STRATA == "OB_HAL" & HEX_ID == 64][order(TIME)] # I had monitored trips in weeks 26 27 and 28 but none after that
    # How many unique trips fished in HEX_ID 64, and how many were monitored?
    a <- realized_mon[box_def$og_data, on = .(ADP, STRATA, TRIP_ID)][ADP == 2022 & STRATA == "OB_HAL"]
    a[HEX_ID == 64, uniqueN(TRIP_ID)]  # Here are my 20 trips that occurred in this box
    length(intersect(a[HEX_ID == 64, unique(TRIP_ID)], realized_mon$TRIP_ID))  # Only 1 of the 20 trips that fished in this HEX_ID were monitored!
    
    # How about HEX_ID 143?
    realized_mon_box[ADP == 2022 & STRATA == "OB_HAL" & HEX_ID == 143][order(TIME)] # I had monitored trips in weeks 15, 22-24, 38 and 42
    # How many unique trips fished in HEX_ID 64, and how many were monitored?
    a <- realized_mon[box_def$og_data, on = .(ADP, STRATA, TRIP_ID)][ADP == 2022 & STRATA == "OB_HAL"]
    a[HEX_ID == 143, uniqueN(TRIP_ID)]  # there were 17 trips that occurred in this box
    length(intersect(a[HEX_ID == 143, unique(TRIP_ID)], realized_mon$TRIP_ID))  # 3 trips out of 17 were monitored (17%), which isn't far out of the ordinary.
    # However, neighboring cells 142 and 140 were probably not monitored either
    a[HEX_ID %in% c(140, 142, 143), uniqueN(TRIP_ID)]   # 58 trips occurred in these boxes
    length(intersect(a[HEX_ID %in% c(140, 142, 143), unique(TRIP_ID)], realized_mon$TRIP_ID))  # 7 out of 58 were monitored = (12%)
    
    
    #' The fact that I see a lot of very red cells means that my realized number of trips monitored or neighboring a monitored trip
    #' within each spatial cell was less than virtually all iteration. That is, I had an unlikely high number of gaps (cells without any monitoring)
    #' This figure does not tell me the MAGNITUDE of the number of trips that were not sampled.
    #' The reason we probably don't see a lot of blue tiles (especially in the GOA) is that we have instances where we have more monitored trips
    #' stacked up in boxes/spatial cells that already have monitoring
    
    #' TODO Can I count number of monitored trips in each BOX/HEX_ID relative to what I would expect?
    #' This would tell me more about where MONITORED TRIPS TOOK PLACE relative to an expectation, less so the influence the neighbors.
    
    ggplot(sim.prog.perc) + 
      facet_grid(STRATA ~ ADP) + 
      geom_sf(aes(fill = MORE_EXTREME)) + 
      scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") + 
      labs(fill = "Proportion of iterations where the\nrealized monitoring was more extreme")
    
    # This figure scales the previous figure by the number of trips in each hex cell, but again, doens't actually speak to MAGNITUDE
    ggplot(sim.prog.perc) + 
      facet_grid(STRATA ~ ADP) + 
      geom_sf(aes(fill = MORE_EXTREME * HEX_w_total)) + 
      scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") + 
      labs(fill = "Proportion of iterations where the\nrealized monitoring was more extreme")
    
    ggplot(sim.prog.perc %>% filter(ADP == 2023 & STRATA == "OB_HAL")) + 
      facet_grid(STRATA ~ ADP) + 
      geom_sf(aes(fill = MORE_EXTREME * HEX_w_total)) + 
      scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") + 
      labs(fill = "Proportion of iterations where the\nrealized monitoring was more extreme")
    
    ggplot(sim.prog.perc %>% filter(ADP == 2023 & STRATA == "OB_HAL")) + 
      facet_grid(STRATA ~ ADP) + 
      geom_sf(aes(fill = MORE_EXTREME * HEX_w_total)) + 
      scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") + 
      labs(fill = "Proportion of iterations where the\nrealized monitoring was more extreme")
    
    
    ggplot(sim.real.perc %>% filter(ADP == 2023 & STRATA == "OB_HAL")) + 
      facet_grid(STRATA ~ ADP) + 
      geom_sf(aes(fill = MORE_EXTREME)) + 
      geom_sf_text(aes(label = HEX_ID)) + 
      scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom")
    
    test <- realized_mon_hex[
    ][attr(sim.real, "hex_smry"), on = c(year_strata, "HEX_ID")]
    test[ADP == 2023 & STRATA == "OB_HAL" & HEX_ID == 101, table(HEX_w)]  # All simulations had at least 8.333, realized only 6.25
    sim.real.perc %>% st_drop_geometry() %>% filter(ADP == 2023 & STRATA == "OB_HAL" & HEX_ID == 101)
    
  }
  
  
  # Find the percentile of the 'actually realized' count of monitored trips relative to the simulations
  # Count up cases where REAL_w was >= HEX_w (simulated outcome)
  sim.real.perc <- realized_mon_hex[
  ][attr(sim.real, "hex_smry"), on = c(year_strata, "HEX_ID")
  ][, .(PERC = 100 * sum(REAL_w >= HEX_w)/.N) , keyby = c(year_strata, "HEX_ID", "HEX_w_total", "REAL_w")]
  
  sim.prog.perc <- realized_mon_hex[
  ][attr(sim.prog, "hex_smry"), on = c(year_strata, "HEX_ID")
  ][, .(PERC = 100 *sum(REAL_w >= HEX_w)/.N) , keyby = c(year_strata, "HEX_ID", "HEX_w_total", "REAL_w")]
  
  if(F){
    #' *WHAT If we did <, meaning 
    sim.real.perc <- realized_mon_hex[
    ][attr(sim.real, "hex_smry"), on = c(year_strata, "HEX_ID")
    ][, .(PERC = 100 * sum(REAL_w >= HEX_w)/.N) , keyby = c(year_strata, "HEX_ID", "HEX_w_total", "REAL_w")]
    
    sim.prog.perc <- realized_mon_hex[
    ][attr(sim.prog, "hex_smry"), on = c(year_strata, "HEX_ID")
    ][, .(PERC = 100 *sum(REAL_w >= HEX_w)/.N) , keyby = c(year_strata, "HEX_ID", "HEX_w_total", "REAL_w")]
    
  }
  
  # Merge the geometry in
  hex_id_geom <- box_def$geom_sf %>% select(HEX_ID, geometry) %>% unique()
  sim.real.perc <- merge(hex_id_geom, sim.real.perc, by = "HEX_ID")
  sim.prog.perc <- merge(hex_id_geom, sim.prog.perc, by = "HEX_ID")
  
  # Custom function for re-scaling fill values of percentiles (values 0 to 100, centered on 50)
  library(scales)
  perc_sqrt_abs <- function(x) sign(x - 50) * sqrt(abs(x - 50))
  perc_sqrt_abs_inverse <- function(x) sign(x - 50) * (x^2 + 50)  
  perc_sqrt_abs_trans <- function() trans_new("perc_sqrt_abs", perc_sqrt_abs, perc_sqrt_abs_inverse)
  perc_sqrt_abs(sim.real.perc$PERC)
  
  # Relative to what was realized
  ggplot(sim.real.perc) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = PERC)) + 
    scale_fill_gradient2(midpoint = 50, transform = "perc_sq_abs") + theme(legend.position = "bottom")
  
  # Relative to what was realized
  ggplot(sim.real.perc) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = PERC)) + 
    scale_fill_gradient() + theme(legend.position = "bottom")
  
  
  
  ggplot(sim.real.perc %>% filter(ADP == 2022 & STRATA == "OB_HAL")) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = PERC)) + 
    geom_sf_text(aes(label = HEX_ID)) + 
    scale_fill_gradient2(midpoint = 50) + theme(legend.position = "bottom") 
  
  
  # try scaling by difference in mean?
  sim.real.perc <- realized_mon_hex[
  ][attr(sim.real, "hex_smry"), on = c(year_strata, "HEX_ID")
  ][, .(MEAN = mean(HEX_w), PERC = 100 * sum(REAL_w >= HEX_w)/.N) , keyby = c(year_strata, "HEX_ID", "HEX_w_total", "REAL_w")]
  sim.prog.perc <- realized_mon_hex[
  ][attr(sim.prog, "hex_smry"), on = c(year_strata, "HEX_ID")
  ][, .(MEAN = mean(HEX_w), PERC = 100 * sum(REAL_w >= HEX_w)/.N) , keyby = c(year_strata, "HEX_ID", "HEX_w_total", "REAL_w")]
  # Merge the geometry in
  hex_id_geom <- box_def$geom_sf %>% select(HEX_ID, geometry) %>% unique()
  sim.real.perc <- merge(hex_id_geom, sim.real.perc, by = "HEX_ID")
  sim.prog.perc <- merge(hex_id_geom, sim.prog.perc, by = "HEX_ID")
  
  # this is where we had fewer trips than expected (this is essentially the same as my previous plots, but uses simuilation rather than mathematical mean)
  ggplot(sim.real.perc %>% filter(STRATA == "OB_HAL" & ADP == 2022)) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(
      fill = (REAL_w - MEAN),
      color = abs(PERC - 50) > 45,
      linewidth = abs(PERC - 50) > 45
      )) +    # Difference in expected number of trips monitored 
    scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") +
    scale_color_manual(values = c(NA, "red")) + 
    scale_linewidth_manual(values = c(0.5, 1))
  
  ggplot(sim.real.perc) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(
      fill = (REAL_w - MEAN),
      color = abs(PERC - 50) > 45,
      linewidth = abs(PERC - 50) > 45
    )) +    # Difference in expected number of trips monitored 
    scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") +
    scale_color_manual(values = c(NA, "black")) + 
    scale_linewidth_manual(values = c(0.5, 1))  # The expectation was basically 100%, so we can't go any HIGHER
  
  ggplot(sim.prog.perc %>% filter(STRATA == "OB_TRW" & ADP == 2023)) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(
      fill = (REAL_w - MEAN),
      color = abs(PERC - 50) > 45,
      linewidth = abs(PERC - 50) > 45
    )) +    # Difference in expected number of trips monitored 
    scale_fill_gradient2(midpoint = 0) + theme(legend.position = "bottom") +
    scale_color_manual(values = c(NA, "black")) + 
    scale_linewidth_manual(values = c(0.5, 1))
  
  # Scaled in trips, the different in the number of 'gap' trips
  

  
  
  # Percentile - In TRW, I am virtually guaranteed to be in the 100% percentile If I have monitoring, and 0% if I miss any boxes
  ggplot(sim.real.perc %>% filter(STRATA == "OB_TRW" & ADP == 2022)) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (PERC))) +    # Difference in expected number of trips monitored 
    scale_fill_gradient2(midpoint = 50) + theme(legend.position = "bottom")
  # as a percentile, the number of outcome where the realized value was >= simulated outcomes
  
  ggplot(sim.real.perc %>% filter(STRATA == "OB_TRW" & ADP == 2022)) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (PERC-50) * (REAL_w - MEAN))) +    # Difference in expected number of trips monitored 
    scale_fill_gradient2() + theme(legend.position = "bottom")
  
  
  ggplot(sim.real.perc %>% filter(STRATA == "OB_TRW" & ADP == 2022)) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (PERC))) +    # Difference in expected number of trips monitored 
    scale_fill_gradient2(midpoint = 50) + 
    theme(legend.position = "bottom")
  
  # With custom scale with more colors, boundaries at percentiles of 10 and 90
  ggplot(sim.real.perc %>% filter(STRATA == "OB_TRW" & ADP == 2022)) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (PERC))) +    # Difference in expected number of trips monitored 
    scale_fill_gradientn(
      colors = c("red", rgb(1, 0.9, 0.9, 1), "white", rgb(0.9, 0.9, 1, 1), "blue"),
      values = c(0, 0.1, 0.5, 0.9, 1)) + 
    theme(legend.position = "bottom")
  
  ggplot(sim.real.perc %>% filter(STRATA == "OB_HAL" & ADP == 2022)) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (PERC))) +    # Difference in expected number of trips monitored 
    scale_fill_gradientn(
      colors = c("red", rgb(1, 0.9, 0.9, 1), "white", rgb(0.9, 0.9, 1, 1), "blue"),
      values = c(0, 0.1, 0.5, 0.9, 1)) + 
    theme(legend.position = "bottom")
  
  ggplot(sim.real.perc %>% filter(STRATA == "OB_HAL" & ADP == 2022)) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = (PERC+1))) +    # Difference in expected number of trips monitored 
    scale_fill_gradient2(midpoint = 50, transform = "log") + 
    theme(legend.position = "bottom")
  

  sim.real.perc %>% st_drop_geometry() 
  
  a <- realized_mon_hex[
  ][attr(sim.real, "hex_smry")[STRATA == "OB_TRW" & ADP == 2022], on = c(year_strata, "HEX_ID")]
  a[HEX_ID == 46, table(HEX_w)]  
  a1 <- sim.real.perc %>% st_drop_geometry %>% as.data.table
  a1[STRATA == "OB_TRW" & ADP == 2022 & HEX_ID == 46]
  
  
  # Why do I not get a legend...
  sim.real.perc %>% st_drop_geometry()
  
  
  sim.real.perc %>% filter(STRATA == "OB_HAL" & ADP == 2023 & HEX_ID == 88)  # Realized 0 which is probably totally expected
  attr(sim.real, "hex_smry")[STRATA == "OB_HAL" & ADP == 2023 & HEX_ID == 88, table(HEX_w)] # All or nothing, either 0 or 3


  ggplot(sim.real.perc) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = PERC)) + 
    scale_fill_gradient2(midpoint = 50)
  
  # Relative to what was intended
  ggplot(sim.prog.perc) + 
    facet_grid(STRATA ~ ADP) + 
    geom_sf(aes(fill = PERC)) + 
    scale_fill_gradient2(midpoint = 50)
  
  
  
  #'*Old*
  
  if(F){
    #' *New*
    realized_boxes <- box_def$dt_out[attr(real_interspersion, "sampled_boxes"), on = c(year_strata, "BOX_ID")
    ][!is.na(HEX_ID)]
    
   
    realized_boxes <- attr(sim.real, "box_expected")$RAW[
    ][attr(real_interspersion, "sampled_boxes"), on = c(year_strata, "BOX_ID")
    ][!is.na(HEX_ID)]
    
  }
  
  # TODO I just want to compare each boxes realized value to the distribution. I can simply count cases where BOX_ID was Higher or lower, right?

  map_out_lst <- vector(mode = "list", length = nrow(year_strata_dt))
  
  # Make plots for each year_strata
  for(i in 1:nrow(year_strata_dt)) {
    # i <- 1
    
    #' DO I NEED THIS
    stratum_map_lst <- list(BOX = NA, HEX_ID.realized = NA, HEX_ID.programmed = NA)
    
    # Subset stratum
    stratum_sub <- merge(box_def$dmn$geom_dmn_df, year_strata_dt[i,], on = year_strata, all.y = T) %>% dplyr::filter(BOX_DMN_n > 0)
    # Extract the spatial extent of the fishing effort
    stratum_sub.bbox <- stratum_sub %>% st_bbox()
    
    
    
    str(sim.real)
    sim.real
    #' TODO My simulations don't capture the results of each box! It's summarized! Add it as an attribute? It's gonna be a huge table...
    #' I could only care about spatial cells (HEX_ID) if I want... And not week since that's covered by out temporal analyses. Saving it as a list
    #' might be the most space-efficient? I want:
    #' List of [ADP, STRATA, BSAI_GOA], list of [HEX_ID], vector of [iter] each with sum of interspersed trips (I can convert this to ISPN too knowing SUM_W across weeks)
    
    
    
    
    
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
    stratum_hex_exp <- attr(sim.real.stratum_fmp, "box_expected")$RAW[
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
    stratum_hex_exp <- attr(sim.prog.stratum_fmp, "box_expected")$RAW[
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


#' *====================================================================================================================*
#' Realized vs Expected Domain Interspersion of OB with EM and ZERO ----------------------------------------------------
#' *====================================================================================================================*

#' TODO These summaries can omit the OB_TRW and EM_TRW_EFP strata. I do this later, but should be able to do this easier
#' If I exlcude all TRW strata from the box_def and rates objects.

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


#' TODO Should really be sampling trip selection once, and then applying each to the box definitions
#' Simply use the 'calculate_realized_dmn_interspersion()' function on each simulated trip selection given the rates?
#' Here we are simulating the realized and programmed rates in terms of domain interspersion.

#' FIXME `programmed_rates` and `realized_rates.val` have different STRATA_N with what box_def creates because `JIG` trips are dropped!



# 10K iterations (1e4) is probably the max you'd want to set given memory and time constraints. 1e4 took around 4 hours.
dmn_insp.prog <- simulate_dmn_interspersion(box_def.stratum_gear_fmp, programmed_rates, ob_em_ze_adl, iter = 1e4, seed = 12345)
dmn_insp.real <- simulate_dmn_interspersion(box_def.stratum_gear_fmp, realized_rates.val, ob_em_ze_adl, iter = 1e4, seed = 12345)

#' Quickload
# save(dmn_insp.prog, dmn_insp.real, file = "output_data/spatiotemp_dmn_insp_i1e4.rdata")
# load( "output_data/spatiotemp_dmn_insp_i1e4.rdata")


#' TODO *THese plots are rough and can probably be removed.
# Plot programmed domain interspersion versus realized values.
ggplot(dmn_insp.prog$dim_insp.pool[GEAR != "TRW"], aes(x = INSP)) +
  facet_grid2(ADP + BSAI_GOA ~ POOL + GEAR, scales = "free", independent = "all") + 
  geom_density() + 
  geom_vline(data = realized_dmn_interspersion.summary, aes(xintercept = INSP), color = "red") + 
  theme(legend.position = "bottom") 

# unscaled y, locked x
ggplot(dmn_insp.prog$dim_insp.pool[GEAR != "TRW"], aes(x = INSP)) +
  facet_grid2(ADP + BSAI_GOA ~ POOL + GEAR, scales = "free_y", independent = "y") + 
  geom_density() + 
  geom_vline(data = realized_dmn_interspersion.summary, aes(xintercept = INSP), color = "red") + 
  theme(legend.position = "bottom") 
#' We can see that the 'expected' range for many of these comparisons are quite low/wide, especially in the BSAI. The 
#' 2024 ADP and the new stratification/allocation methods should generally improve the expected and realized 
#' interspersion values by allocating more at-sea observer monitoring in the BSAI strata. 

#' Everything was in the expected range EXCEPT:
#' - *2022 ZERO HAL gear in the BSAI was low*
#' - *2023 ZERO POT gear in the GOA was low, but now quite in the 2.5% tail*




dmn_insp.density.real <- calculate_dmn_density(dmn_insp.real, fill_color = "dodgerblue")
dmn_insp.density.prog <- calculate_dmn_density(dmn_insp.prog, fill_color = "green")
dmn_insp.density.combined <- combine_dmn_distributions(dmn_insp.density.real, dmn_insp.density.prog)




dmn_intperspersion.dmn_N <- dmn_insp.prog$dim_insp.pool[, .(dmn_N = round(mean(dmn_N), 1)), keyby = .(ADP, POOL, GEAR, BSAI_GOA)][GEAR != "TRW"]
# Free scales
plot.dmn_insp.free <- plot_dmn_interspersion_density(dmn_insp.density.combined[GEAR != "TRW"], realized_dmn_interspersion.summary, dmn_intperspersion.dmn_N) + 
  facet_nested(
    ADP + BSAI_GOA ~ GEAR + POOL, scales = "free", independent = "x", 
    labeller = labeller(
      GEAR = function(x) paste0("Gear: ", x),
      POOL = function(x) paste0("Pool: ", x),
      BSAI_GOA = function(x) paste0("FMP: ", x)
  ))
ggsave(plot = plot.dmn_insp.free, file = "output_data/spatiotemp_dmn_insp_free.png", width = 8, height = 6)


# fixed scales
plot.dmn_insp.fixed <-plot_dmn_interspersion_density(dmn_insp.density.combined[GEAR != "TRW"], realized_dmn_interspersion.summary, dmn_intperspersion.dmn_N) + 
  facet_nested(
    ADP + BSAI_GOA ~ GEAR + POOL, scales = "free_y", independent = "y", 
    labeller = labeller(
      GEAR = function(x) paste0("Gear: ", x),
      POOL = function(x) paste0("Pool: ", x),
      BSAI_GOA = function(x) paste0("FMP: ", x)))


# Get percentile of expectation using ecdf()
#' [Note: the value that goes into ecdf isn't returned by quantile (but reducing that value by 0.001 gets you there?)]
ecdf_fun <- ecdf(dmn_insp.prog$dim_insp.pool[ADP == 2022 & POOL == "ZERO" & GEAR ==  "HAL" & BSAI_GOA == "BSAI", INSP])
ecdf_fun(realized_dmn_interspersion[ADP == 2022 & POOL == "ZERO" & GEAR ==  "HAL" & BSAI_GOA == "BSAI", dmn_INSP/sum_BOX_DMN_w]) #' 0.0452
quantile(dmn_insp.prog$dim_insp.pool[ADP == 2022 & POOL == "ZERO" & GEAR ==  "HAL" & BSAI_GOA == "BSAI", INSP], 0.0452)
quantile(dmn_insp.prog$dim_insp.pool[ADP == 2022 & POOL == "ZERO" & GEAR ==  "HAL" & BSAI_GOA == "BSAI", INSP], 0.0451)

ecdf_fun <- ecdf(dmn_insp.prog$dim_insp.pool[ADP == 2023 & POOL == "ZERO" & GEAR ==  "POT" & BSAI_GOA == "GOA", INSP])
ecdf_fun(realized_dmn_interspersion[ADP == 2023 & POOL == "ZERO" & GEAR ==  "POT" & BSAI_GOA == "GOA",  dmn_INSP/sum_BOX_DMN_w])  # 0.0722
quantile(dmn_insp.prog$dim_insp.pool[ADP == 2023 & POOL == "ZERO" & GEAR ==  "POT" & BSAI_GOA == "GOA", INSP], 0.0721) 

#' TODO FIXME I NEED TO SUMMARIZE MORE
test <- copy(realized_dmn_interspersion)
test[, INSP := dmn_INSP/sum_BOX_DMN_w]
percentile.dmn <- dmn_insp_percentile(test, dmn_insp.prog, dmn_insp.real)
percentile.dmn[, c("PROG_SIG", "REAL_SIG") := lapply(.SD, function(x) {
  fcase(x < 0.025, "<<", x < 0.05, "<", x > 0.975, ">>", x > 0.95, ">", default = "")
}), .SDcols = c("PERC_PROG", "PERC_REAL")]
colnames(percentile.dmn) <- gsub("_", " ", colnames(percentile.dmn))
percentile.dmn


# Realized rates by FMP ------------------------------------------------------------------------------------------------

#' The realized rates may differ by FMP due to random chance and/or manipulation/observer effects.

library(flextable)
set_flextable_defaults(font.family = "Calibri")

stratum_levels <- c("OB_HAL", "OB_POT", "OB_TRW", "EM_HAL", "EM_POT", "EM_TRW_EFP", "ZERO")

programmed_rates
# Realized rates by FMP (defined by the FMP with the majority of catch)
realized_rates.val.fmp <- dcast(
  pc_effort_st[STRATA != "ZERO", .(
    N = uniqueN(TRIP_ID), 
    n = uniqueN(TRIP_ID[OBSERVED_FLAG == "Y"]),
    REALIZED = 100 *uniqueN(TRIP_ID[OBSERVED_FLAG == "Y"]) / uniqueN(TRIP_ID)), 
    keyby = .(ADP, STRATA, BSAI_GOA)],
  ADP + STRATA ~ BSAI_GOA, value.var = c("N", "n", "REALIZED"))
# Merge in the programmed rates (AK-wide)
realized_rates.val.fmp[, PROG := 100 * programmed_rates[realized_rates.val.fmp, SAMPLE_RATE, on = .(ADP, STRATA)]]
setcolorder(realized_rates.val.fmp, c("ADP", "STRATA", "PROG", "N_BSAI", "n_BSAI", "REALIZED_BSAI", "N_GOA", "n_GOA", "REALIZED_GOA"))
realized_rates.val.fmp[, STRATA := gsub("_", " ", STRATA)]
realized_rates.val.fmp[, STRATA := factor(STRATA, levels = gsub("_", " ", stratum_levels))]

Tbl.Realized_Rates.FMP <- realized_rates.val.fmp %>% 
  flextable() %>%
  #add_header_row(values = c("", "BSAI", "GOA"), colwidths = c(3, 3, 3)) %>%
  align(j = 1, align = "center") %>%
  set_header_labels(
    ADP = "Year", STRATA = "Strata", "PROG" = "Selection\nrate",
    N_BSAI = "N", n_BSAI = "n", REALIZED_BSAI = "%",
    N_GOA  = "N", n_GOA  = "n", REALIZED_GOA  = "%") %>%
  colformat_int(j = 1, big.mark = "") %>%
  colformat_double(j = c(3, 6,9), digits = 2) %>%
  add_header_row(values = c("Year", "Strata", "Selection\nrate", "BSAI", "GOA"), colwidths = c(1, 1, 1, 3, 3)) %>%
  italic(i = 2, j = c(4:5, 7:8), part = "header") %>%
  align(i = 1, j = c(4, 7), part = "header", align = "center") %>%
  merge_v(j = 1:3, part = "header") %>%
  hline(i = which(diff(realized_rates.val.fmp$ADP) == 1)) %>%
  fix_border_issues() %>%
  autofit()
Tbl.Realized_Rates.FMP
save_as_docx(Tbl.Realized_Rates.FMP, path = "output_data/spatiotemp_rates_by_strata_fmp.docx")

# Realized AK-wide rates
realized_rates.val
#' OB_HAL : Rates in the BSAI were higher than in the GOA
#' To date in 2023, EM_HAL and EM_POT are both below 7% in the BSAI
#' OB_POT : Rates in the BSAI were 0.25, compared to 0.14 in the GOA, perhaps why interspersion was higher than mean expected.
#' OB_TRW : Rates in the BSAI were 0.35, higher than expected, but interspersion was below mean. 0.275 in 


#' *-------------------------------------------------------------------------------------------------------------------*
# Investigation #### 
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

#'*--------------------------------------------------------------------------------------------------------------------*
# Save objects for Rmarkdown ####
#'*--------------------------------------------------------------------------------------------------------------------*
