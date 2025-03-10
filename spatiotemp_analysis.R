library(data.table)
library(ggplot2)
library(FMAtools)
library(ggh4x)       # facet_grid2 has cleaner facet labels while allowing for independent x and/or y scales
library(sf)          # for manipulating simpler feature objects and mapping
library(sfheaders)   # for sf_remove_holes
library(dplyr)       # for piping and some data munging functions
library(flextable)   # for creating print-ready tables

# Set the simulation parameters
sim_iter <- 1e4      # 1e3 iterations is good for testing quickly, 1e4 is better for the final run
seed <- 12345

# Assign the address of Shared Gdrive operations for this project
project_folder <- gdrive_set_dribble("Projects/AnnRpt-Deployment-Chapter/")

# Download 2_AR_data.Rdata
gdrive_download("2_AR_data.Rdata", project_folder)
(load("2_AR_data.Rdata"))
# Remove everything except for what is needed
rm(list = setdiff(ls(), c("work.data", "partial", "project_folder", "shp_land", "shp_nmfs", "year", "sim_iter", "seed")))

# Load Data and Functions ----------------------------------------------------------------------------------------------

# Make FMP-specific polygons
shp_fmp <- rbind(
  shp_nmfs %>% filter(SubArea %in% c("BS", "AI")) %>% st_buffer(1) %>% st_union() %>% st_sf() %>% sf_remove_holes() %>% 
    mutate(FMP = "GOA"),
  shp_nmfs %>% filter(SubArea == "GOA") %>% st_buffer(1) %>% st_union() %>% st_sf() %>% sf_remove_holes() %>% 
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
  dsn = "source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = TRUE) %>%
  select(STAT_AREA) %>%
  st_transform(crs = 3467)

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
programmed_rates <- copy(data.table(partial)) |>
  # Remove gear type so STRATA and Rate are 1:1
  _[, -c("formatted_strat", "GEAR")] |>
  unique() |>
  _[, STRATA := gsub(" ", "_", STRATA)][]
setnames(programmed_rates, old = c("YEAR", "Rate"), new = c("ADP", "SAMPLE_RATE"))
# Add ZERO to programmed rates
programmed_rates <- rbind(programmed_rates, data.table(ADP = year, STRATA = "ZERO", SAMPLE_RATE = 0))
# Add STRATA_N to programmed_rates
programmed_rates[, STRATA_N := realized_rates.val[programmed_rates, STRATA_N, on = .(ADP, STRATA)]]
setcolorder(programmed_rates, c("ADP", "STRATA", "STRATA_N", "SAMPLE_RATE"))

#' Check to make sure all year/strata names match between rates objects
if(!fsetequal(unique(realized_rates.val[, .(STRATA, ADP)]), unique(programmed_rates[, .(STRATA, ADP)])) ){
  stop("STRATA and ADP in `realized_rates.val` does not match those in `programmed_rates!`")
}

#' Check to make sure all year/strata names match between pc_effort_st and rates objects!
if(!fsetequal(unique(pc_effort_st[, .(STRATA, ADP)]), unique(programmed_rates[, .(STRATA, ADP)])) ){
  stop("STRATA and ADP in `pc_effort_dt` does not match those in `programmed_rates!`")
}

# Specify ordering of strata. Adding a blank stratum for the sake of aligning BSAI/GOA in plots.
strata_levels <- c("OB_FIXED_BSAI", "OB_FIXED_GOA", "OB_TRW_BSAI", "OB_TRW_GOA", "EM_FIXED_BSAI", "EM_FIXED_GOA", "", "EM_TRW_GOA_(EFP)")


## Define Boxes ----

#' \TODO *I believe I need to use a box definition with ps_cols = "GEAR", but shouldnt need dmn_lst.*
#' I should be able to define a separate box definition with dmn_lst = list(nst = "GEAR", st = NULL), which will allow
#' for cross-stratum comparisons. *note* that I would be leaving st = NULL and would need to allow both OB_FIXED_GOA and
#' OB_FIXED_BSAI to apply to the ZERO stratum, which is not stratified by FMP!

#' Define boxes using the 2024 stratum definitions, post-stratifying the fixed gear strata by Gear type. Exclude the 
#' ZERO stratum as we don't need to simulate interspersion for a stratum with a 0% selection rate.

box_def <- define_boxes(
  data = pc_effort_st[STRATA != "ZERO"], space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = TRUE, ps_cols = "GEAR")

# Disable for now - these may be used for cross-strata comparisons?
if(F) {
  # Define boxes, post-stratifying by FMP only.
  #' \TODO Do I need this? currently used by `plot_interspersion_map`
  box_def.stratum_fmp <- define_boxes(
    data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
    year_col = "ADP", stratum_cols = "STRATA", geom = TRUE, dmn_lst = list(nst = NULL, st = "BSAI_GOA"))
  
  # Define boxes, post-stratifying by FMP and Gear type (for OB to EM and ZE interspersion)
  #' \TODO Do I need this? currently used in the `cross-strata OB EM ZE comparisons`
  box_def.stratum_gear_fmp <- define_boxes(
    data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
    year_col = "ADP", stratum_cols = "STRATA", geom = TRUE, dmn_lst = list(nst = "GEAR", st = "BSAI_GOA"))
}

## Stratum-Specific Proximity ------------------------------------------------------------------------------------------

# Expected interspersion given the programmed and realized rates
exp_interspersion.programmed <- calculate_interspersion(box_def, sample_rates = programmed_rates)
exp_interspersion.realized <- calculate_interspersion(box_def, sample_rates = realized_rates.val)

# Actually realized interspersion
real_interspersion.stratum <- calculate_interspersion(box_def, realized_mon = realized_mon)

### Simulate trip selection to create distributions of interspersion ----

#' Here, we will also capture the HEX_ID-level summaries for the spatial analyses using `hex_smry = TRUE`
#' This should take ~2 minutes to run.

# Programmed rates
sim.programmed.stratum <- calculate_interspersion(box_def, sample_rates = programmed_rates, sim_iter = sim_iter, seed = seed, hex_smry = TRUE)
# Realized rates
sim.realized.stratum <- calculate_interspersion(box_def, sample_rates = realized_rates.val, sim_iter = sim_iter, seed = seed, hex_smry = TRUE)

# Calculate density of both distributions
density.programmed.stratum <- calculate_density(sim.programmed.stratum, "dodgerblue", adjust = 2)
density.realized.stratum <- calculate_density(sim.realized.stratum, "green", adjust = 2)

# Combine distributions
density.stratum <- combine_distributions(density.realized.stratum, density.programmed.stratum)

#' Plot distributions vs actually realized
#' \TODO Make better legend descriptions like density assuming realized rate, density assuming programmed rate, etc
#' \TODO I should I make box_def make create DMN, i.e., make it the same as ADP_STRATA when it's null? Maybe when I do 
#' the cross-stratua comparison I'll go back to this...
plt.proximity.stratum <- plot_interspersion_density(
  density.stratum, real_interspersion.stratum, strata_levels) + 
  facet_nested_wrap(
    STRATA ~ ., dir = "h", ncol = 2, scales = "free", drop = F,
    labeller = labeller(STRATA = function(x) paste0(year, " : ", gsub("_", " ", x) )))

#' \TODO Would look cleaner to remove the unused panel. Can do this with grid/gridExtra but it looks a little more
#' complicated than usual when building plots with facet_nested_wrap

# Calculate percentiles of the outcomes relative to both distributions

#' \TODO Make the column names better, match what is in the document
percentile.stratum <- insp_percentile(
  real_interspersion.stratum[, -"ITER"], 
  sim.programmed.stratum, 
  sim.realized.stratum) |>
  _[, c("PROG_SIG", "REAL_SIG") := lapply(.SD, function(x) {
    fcase(
      x < 0.025, "<<", 
      x < 0.05, "<", 
      x > 0.975, ">>", 
      x > 0.95, ">", default = "")
    }), .SDcols = c("PERC_PROG", "PERC_REAL")
  # ecdf function can't have an open right side, so check if the realized rate is outside of 95% interval
  ][, PROG_SIG_CHECK := data.table::between(INSP, prog_0.025, prog_0.975)
  ][PROG_SIG_CHECK == T, PROG_SIG := ""
  ][, REAL_SIG_CHECK := data.table::between(INSP, real_0.025, real_0.975)
  ][REAL_SIG_CHECK == T, REAL_SIG := ""
  ][, c("prog_0.025", "prog_0.975", "real_0.025", "real_0.975", "PROG_SIG_CHECK", "REAL_SIG_CHECK") := NULL
  ][, STRATA := factor(STRATA, levels = strata_levels)][]
setorder(percentile.stratum, ADP, STRATA)
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

### Proximity Maps ####

#' [NOTE] *These maps are for internal use to get more granularity of the patterns that led to the resulting proximity
proximity_maps <- plot_interspersion_map(box_def, real_interspersion.stratum, exp_interspersion.realized.stratum, exp_interspersion.programmed)

#' \TODO Add trip count text labels to $HEX_ID.realized plot cells 

proximity_maps[[paste0(year, ".OB_FIXED_BSAI")]]$BOX
proximity_maps[[paste0(year, ".OB_FIXED_BSAI")]]$HEX_ID.realized  
# Range of expected differences is very tight (-1 to 1) so nothing surprising here

proximity_maps[[paste0(year, ".OB_FIXED_GOA")]]$BOX
proximity_maps[[paste0(year, ".OB_FIXED_GOA")]]$HEX_ID.realized  
#' Gaps in WGOA 610 during weeks 16-27
#' \TODO Why is there a trip in the BSAI here?

proximity_maps[[paste0(year, ".OB_TRW_BSAI")]]$BOX
proximity_maps[[paste0(year, ".OB_TRW_BSAI")]]$HEX_ID.realized
#' No gaps, stratum is in only two cells...

proximity_maps[[paste0(year, ".OB_TRW_GOA")]]$BOX
proximity_maps[[paste0(year, ".OB_TRW_GOA")]]$HEX_ID.realized
#' Small gap in WGOA? Only two fewer trips?

proximity_maps[[paste0(year, ".EM_FIXED_BSAI")]]$BOX
proximity_maps[[paste0(year, ".EM_FIXED_BSAI")]]$HEX_ID.realized
#' Seem to have lower than expected monitoring for BSAI trips in EBSAI/WGOA

proximity_maps[[paste0(year, ".EM_FIXED_GOA")]]$BOX
proximity_maps[[paste0(year, ".EM_FIXED_GOA")]]$HEX_ID.realized
#' Data needs to be fixed here, but it looks like a bunch of 640 trips were monitored? These plots use the realized 
#' monitoring rate, not programmed, so this distribution should change as the data is fixed. Still, kind of strange.

### Proximity (coverage, but with a spatial focus) ####

#' For each stratum, visually inspect whether the achieved monitoring coverage was distributed evenly through space. 
#' The fill color of each spatial cell (hexagon) corresponds to areas where the level of coverage of more (green) or 
#' less (purple) than expected (i.e., median outcome of simulations), and cells containing a circle indicate outcomes
#' that were more extreme than 95% of simulation outcomes

#' Look for clusters of spatial cells with circles, indicating a pattern of departures from expected coverage.

#' This is not counting # of trips in each cell and comparing with how many were monitored! It's calculating the 
#' proportion of trips in the spatial cell that were covered (monitored or proximal to monitoring) and comparing that to what
#' was achieved in simulations of random sampling. More trips monitored in a cell does not necessarily translate to 
#' higher coverage if the monitoring is clumped in time and not distributed evenly with fishing effort! This is very 
#' apparent in the fixed-gear EM strata where 

#' White (value of ~0) means the achieved monitoring coverage matched the expectation.
#' Green (value of +1) means no coverage was expected but was fully covered.
#' Purple (value of -1) means all fishing effort was expected to be covered but none was realized.
#' When more cells are purple than green, it means that sampling was more clumped up temporally than expected, i.e, the
#' fishing effort within the spatial cell was not well-represented temporally by neighboring trips.

# These plots use the simulation results rather than the averages
spatial_plots_cov <- plot_spatial_coverage(box_def, realized_mon, sim.realized.stratum, sim.programmed.stratum, strata_levels)
plt.spatial_cov <- spatial_plots_cov$coverage

#' \TODO Remove blank facet

#' *====================================================================================================================*
# Spatial Analyses ####
#' *====================================================================================================================*

## Spatial Arrangement of Monitored Trips ----

#' Determine whether the spatial arrangement of monitoring was distributed evenly (as expected by random sampling). 
#' Counts the number of monitored trips in each HEX_ID and compares to the simulation outcomes, highlighting any 
#' actual outcomes that were more extreme than 95% of simulated outcomes. Cells start to get color if they are more 
#' extreme than 80% of outcomes.

spatial_plots <- plot_monitoring_spatial(box_def, realized_mon, sim.realized.stratum, strata_levels)
plt.spatial <- spatial_plots$plt.spatial.2024

#' \TODO Remove blank facet
#' \TODO Note that EM TRW GOA EFP has a noticeable spatial bias. Double-check once we verify the valhalla dataset is
#' correct, but there really shouldn't be a spatial bias with 1/3 offloads monitored, right? In any case, in the 2025
#' ADP, we shouldn't have any bias if all offloads are monitored.

#' *====================================================================================================================*
# Realized vs Expected Domain Interspersion of OB with EM and ZERO -----------------------------------------------------
#' *====================================================================================================================*

#' These analyses will not be included in the 2023 AR to the council but can be included as an Appendix in the 
#' Processed Report

#' TODO These summaries can omit the OB_TRW and EM_TRW_EFP strata. I do this later, but should be able to do this easier
#' If I exclude all TRW strata from the box_def and rates objects.

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

#' Calculate the expected interspersion for each domain
expected_dmn_interspersion.summary <- expected_ob_em_ze_interspersion$POOLED[
][GEAR != "TRW"  
][, .(EXP_INSP = sum(BOX_DONOR_SAMPLE_PROB * BOX_DMN_w, na.rm = TRUE) / sum(BOX_DMN_w)), keyby = .(ADP, POOL, BSAI_GOA, GEAR)]

# Programmed Rates Simulation
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

#' FIXME `programmed_rates` and `realized_rates.val` have different STRATA_N with what box_def creates because `JIG` trips are dropped!

# 10K iterations (1e4) is probably the max you'd want to set given memory and time constraints. 1e4 took around 4 hours.
dmn_insp.prog <- simulate_dmn_interspersion(box_def.stratum_gear_fmp, programmed_rates, ob_em_ze_adl, iter = 1e4, seed = 12345)
dmn_insp.real <- simulate_dmn_interspersion(box_def.stratum_gear_fmp, realized_rates.val, ob_em_ze_adl, iter = 1e4, seed = 12345)

#' Quickload
# save(dmn_insp.prog, dmn_insp.real, file = "output_data/spatiotemp_dmn_insp_i1e4.rdata")
# load( "output_data/spatiotemp_dmn_insp_i1e4.rdata")

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

# Fixed scales
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
#' Realized rates by FMP (defined by the FMP with the majority of catch)
realized_rates.val.fmp <- dcast(
  pc_effort_st[STRATA != "ZERO", .(
    N = uniqueN(TRIP_ID), 
    n = uniqueN(TRIP_ID[OBSERVED_FLAG == "Y"]),
    REALIZED = 100 * uniqueN(TRIP_ID[OBSERVED_FLAG == "Y"]) / uniqueN(TRIP_ID)), 
    keyby = .(ADP, STRATA, BSAI_GOA)],
  ADP + STRATA ~ BSAI_GOA, value.var = c("N", "n", "REALIZED"))
# Merge in the programmed rates (AK-wide)
realized_rates.val.fmp[, PROG := 100 * programmed_rates[realized_rates.val.fmp, SAMPLE_RATE, on = .(ADP, STRATA)]]
setcolorder(realized_rates.val.fmp, c("ADP", "STRATA", "PROG", "N_BSAI", "n_BSAI", "REALIZED_BSAI", "N_GOA", "n_GOA", "REALIZED_GOA"))
realized_rates.val.fmp[, STRATA := gsub("_", " ", STRATA)]
realized_rates.val.fmp[, STRATA := factor(STRATA, levels = gsub("_", " ", strata_levels))]

Tbl.Realized_Rates.FMP <- realized_rates.val.fmp %>% 
  flextable() %>%
  align(j = 1, align = "center") %>%
  set_header_labels(
    ADP = "Year", STRATA = "Strata", "PROG" = "Selection\nrate",
    N_BSAI = "N", n_BSAI = "n", REALIZED_BSAI = "%",
    N_GOA  = "N", n_GOA  = "n", REALIZED_GOA  = "%") %>%
  colformat_int(j = 1, big.mark = "") %>%
  colformat_double(j = c(3, 6, 9), digits = 2) %>%
  add_header_row(values = c("Year", "Strata", "Selection\nrate", "BSAI", "GOA"), colwidths = c(1, 1, 1, 3, 3)) %>%
  italic(i = 2, j = c(4:5, 7:8), part = "header") %>%
  align(i = 1, j = c(4, 7), part = "header", align = "center") %>%
  merge_v(j = 1:3, part = "header") %>%
  merge_v(j = 1, part = "body") %>%
  hline(i = which(diff(realized_rates.val.fmp$ADP) == 1)) %>%
  fix_border_issues() %>%
  autofit()
Tbl.Realized_Rates.FMP

# Realized AK-wide rates, for comparison
realized_rates.val

#'*--------------------------------------------------------------------------------------------------------------------*
# Save objects for Rmarkdown ####
#'*--------------------------------------------------------------------------------------------------------------------*

save(
  
  # TODO OMIT EM_TRW_EFP from the FMP-specific proximity plots
  # TODO split FMP-specific rates table, or don't include it?
  
  # Interspersion distributions
  plt.proximity.stratum.2022,
  plt.proximity.stratum.2023,
  
  plt.proximity.stratum_fmp.2022,
  plt.proximity.stratum_fmp.2023,
  
  proximity_maps,  # This object is particularly large but provides the most granularity on proximity.
  percentile.stratum,
  percentile.stratum_fmp,
  tbl.percentile.stratum,
  tbl.percentile.stratum_fmp,
  
  # Spatiotemporal coverage map
  plt.spatial_cov.2022,
  plt.spatial_cov.2023,
  
  # Spatial plots
  plt.spatial.2022,
  plt.spatial.2023,
  
  # Rate
  realized_rates.val,
  realized_rates.val.fmp,
  Tbl.Realized_Rates.FMP,
  file = "2023_AR_spatiotemp_analysis.rdata"
)

# Upload to Shared Gdrive. 
gdrive_upload("2023_AR_spatiotemp_analysis.rdata", project_folder)
