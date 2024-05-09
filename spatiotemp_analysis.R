library(data.table)
library(ggplot2)
library(FMAtools)
library(ggh4x)       # facet_grid2 has cleaner facet labels while allowing for independent x and/or y scales
library(sf)          # for manipulating simpler feature objects and mapping
library(sfheaders)   # for sf_remove_holes?  also sf_bbox() to calculate the bbox from the geometry, like st_bbox?
library(dplyr)       # for piping and some data munging functions
library(flextable)   # for creating print-ready tables

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
if(!fsetequal(unique(realized_rates.val[, .(STRATA, ADP)]), unique(programmed_rates[, .(STRATA, ADP)])) ){
  stop("STRATA and ADP in `realized_rates.val` does not match those in `programmed_rates!`")
}

#' Check to make sure all year/strata names match between pc_effort_st and rates objects!
if(!fsetequal(unique(pc_effort_st[, .(STRATA, ADP)]), unique(programmed_rates[, .(STRATA, ADP)])) ){
  stop("STRATA and ADP in `pc_effort_dt` does not match those in `programmed_rates!`")
}

# Specify ordering of strata
strata_levels <- c("OB_HAL", "OB_POT", "OB_TRW", "EM_HAL", "EM_POT", "EM_TRW_EFP")


## Define Boxes ----

# Define boxes, post-stratifying by FMP only
box_def.stratum <- define_boxes(
  data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = TRUE, dmn_lst = list(nst = NULL, st = NULL))

# Define boxes, post-stratifying by FMP only
box_def.stratum_fmp <- define_boxes(
  data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = TRUE, dmn_lst = list(nst = NULL, st = "BSAI_GOA"))

# Define boxes, post-stratifying by FMP and Gear type (for OB to EM and ZE interspersion)
box_def.stratum_gear_fmp <- define_boxes(
  data = pc_effort_st, space = c(2e5, 2e5), time = c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"),
  year_col = "ADP", stratum_cols = "STRATA", geom = TRUE, dmn_lst = list(nst = "GEAR", st = "BSAI_GOA"))

## Stratum-Specific Proximity ------------------------------------------------------------------------------------------

# Expected interspersion given the programmed and realized rates
exp_interspersion.programmed.stratum <- calculate_expected_interspersion(box_def.stratum, programmed_rates)
exp_interspersion.realized.stratum <- calculate_expected_interspersion(box_def.stratum, realized_rates.val)

# Actually realized interspersion
real_interspersion.stratum <- calculate_realized_interspersion(box_def.stratum, realized_mon)

### Simulate trip selection to create distributions of interspersion ----


#' [Here, we will also capture the HEX_ID-level summaries for the spatial analyses using `hex_smry = TRUE`]
# Programmed rates
sim.programmed.stratum <- simulate_interspersion(box_def.stratum, programmed_rates, iter = 1e4, seed = 12345, hex_smry = TRUE)
# Realized rates
sim.realized.stratum <- simulate_interspersion(box_def.stratum, realized_rates.val, iter = 1e4, seed = 12345, hex_smry = TRUE)

#' `Quickload`
# save(sim.programmed.stratum, sim.realized.stratum, file = "output_data/spatiotemp_stratum_insp_i1e4.rdata")
# load("output_data/spatiotemp_stratum_insp_i1e4.rdata")

# Calculate density of both distributions
density.programmed.stratum <- calculate_density(sim.programmed.stratum, "dodgerblue", adjust = 2)
density.realized.stratum <- calculate_density(sim.realized.stratum, "green", adjust = 2)

# Combine distributions
density.stratum <- combine_distributions(density.realized.stratum, density.programmed.stratum)
# Make labels for domain trip counts
dmn_N.stratum <- real_interspersion.stratum[
  density.stratum[, .SD[1,], keyby = c(attr(density.stratum, "year_strata_domains")) ], 
  on = attr(density.stratum, "year_strata_domains")]
dmn_N.stratum[, X := pmin(X, INSP)]
dmn_N.stratum[, STRATA_DMN_N := formatC(round(STRATA_DMN_N), width = max(nchar(round(STRATA_DMN_N))))]

#' Plot distributions vs actually realized
#' [2022]
plt.proximity.stratum.2022 <- plot_interspersion_density(
  density.stratum[ADP == 2022], real_interspersion.stratum[ADP == 2022], dmn_N.stratum[ADP == 2022], strata_levels) + 
  facet_nested_wrap(
    STRATA ~ ., dir = "v", nrow = 3, scales = "free", 
    labeller = labeller(STRATA = function(x) paste0(2022, " : ", gsub("_", " ", x) )))
#' [2023]
plt.proximity.stratum.2023 <- plot_interspersion_density(
  density.stratum[ADP == 2023], real_interspersion.stratum[ADP == 2023], dmn_N.stratum[ADP == 2023], strata_levels) + 
  facet_nested_wrap(
    STRATA ~ ., dir = "v", nrow = 3, scales = "free", 
    labeller = labeller(STRATA = function(x) paste0(2023, " : ", gsub("_", " ", x) )))

# Calculate percentiles of the outcomes relative to both distributions
percentile.stratum <- insp_percentile(real_interspersion.stratum, sim.programmed.stratum, sim.realized.stratum)
percentile.stratum[, c("PROG_SIG", "REAL_SIG") := lapply(.SD, function(x) {
  fcase(x < 0.025, "<<", x < 0.05, "<", x > 0.975, ">>", x > 0.95, ">", default = "")
}), .SDcols = c("PERC_PROG", "PERC_REAL")]
percentile.stratum[, STRATA := factor(STRATA, levels = strata_levels)]
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

## Proximity with Stratum and FMP (BSAI and GOA) -----------------------------------------------------------------------

# Expected interspersion given the programmed and realized rates
exp_interspersion.programmed.stratum_fmp <- calculate_expected_interspersion(box_def.stratum_fmp, programmed_rates)
exp_interspersion.realized.stratum_fmp <- calculate_expected_interspersion(box_def.stratum_fmp, realized_rates.val)

# Actually realized interspersion
real_interspersion.stratum_fmp <- calculate_realized_interspersion(box_def.stratum_fmp, realized_mon)

### Simulate trip selection to create distributions of proximity ----

# Programmed rates
sim.programmed.stratum_fmp <- simulate_interspersion(box_def.stratum_fmp, programmed_rates, iter = 1e4, seed = 12345)
# Realized rates
sim.realized.stratum_fmp <- simulate_interspersion(box_def.stratum_fmp, realized_rates.val, iter = 1e4, seed = 12345)

#' `Quickload`
# save(sim.programmed.stratum_fmp, sim.realized.stratum_fmp, file = "output_data/spatiotemp_stratum_fmp_insp_i1e4.rdata")
# load("output_data/spatiotemp_stratum_fmp_insp_i1e4.rdata")

# Calculate density of both distributions
density.programmed.stratum_fmp <- calculate_density(sim.programmed.stratum_fmp, "dodgerblue", adjust = 2)
density.realized.stratum_fmp <- calculate_density(sim.realized.stratum_fmp, "green", adjust = 2)

# Combine distributions
density.stratum_fmp <- combine_distributions(density.realized.stratum_fmp, density.programmed.stratum_fmp)
# Make labels for domain trip counts
dist_x <- density.stratum_fmp[, .SD[1, ], keyby = c(attr(density.stratum_fmp, "year_strata_domains")) ]
dmn_N.stratum_fmp <- real_interspersion.stratum_fmp[dist_x, on = attr(density.stratum_fmp, "year_strata_domains")]
dmn_N.stratum_fmp[, X := pmin(X, INSP)]
dmn_N.stratum_fmp[, STRATA_DMN_N := formatC(round(STRATA_DMN_N), width = max(nchar(round(STRATA_DMN_N))))]

#' Plot distributions vs actually realized

# Split by Year
# Re-order thes strata levels so we can facet_nested_wrap vertically
strata_levels2 <- c("OB_HAL", "EM_HAL", "OB_POT", "EM_POT", "OB_TRW", "EM_TRW_EFP")

plt.proximity.stratum_fmp.2022 <- plot_interspersion_density(
  density.stratum_fmp[ADP == 2022], real_interspersion.stratum_fmp[ADP == 2022], dmn_N.stratum_fmp[ADP == 2022], strata_levels2) + 
  facet_nested_wrap(
    ~ STRATA + BSAI_GOA, dir = "h", drop = FALSE, ncol = 4, scales = "free", 
    labeller = labeller(
      STRATA = function(x) paste0(2022, " : ", gsub("_", " ", x)),
      BSAI_GOA = function(x) paste0("FMP : ", x)))

plt.proximity.stratum_fmp.2023 <- plot_interspersion_density(
  density.stratum_fmp[ADP == 2023], real_interspersion.stratum_fmp[ADP == 2023], dmn_N.stratum_fmp[ADP == 2023], strata_levels2) + 
  facet_nested_wrap(
    ~ STRATA + BSAI_GOA, dir = "h", drop = FALSE, ncol = 4, scales = "free", 
    labeller = labeller(
      STRATA = function(x) paste0(2023, " : ", gsub("_", " ", x)),
      BSAI_GOA = function(x) paste0("FMP : ", x)))

# Calculate the percentile of realized interspersion given the programmed and realized rates
percentile.stratum_fmp <- insp_percentile(real_interspersion.stratum_fmp, sim.programmed.stratum_fmp, sim.realized.stratum_fmp)
percentile.stratum_fmp[, c("PROG_SIG", "REAL_SIG") := lapply(.SD, function(x) {
  fcase(x < 0.025, "<<", x < 0.05, "<", x > 0.975, ">>", x > 0.95, ">", default = "")
}), .SDcols = c("PERC_PROG", "PERC_REAL")]
percentile.stratum_fmp[, STRATA := factor(STRATA, levels = strata_levels)]
setorder(percentile.stratum_fmp, ADP, STRATA)
colnames(percentile.stratum_fmp) <- gsub("_", " ", colnames(percentile.stratum_fmp))

tbl.percentile.stratum_fmp <- percentile.stratum_fmp %>% 
  flextable() %>%
  colformat_int(j = 1, big.mark = "") %>%
  colformat_double(j = 4, digits = 2) %>%
  colformat_double(j = c(6:8), digits = 4) %>%
  merge_v(j = 1:2) %>%
  hline(i = which(diff(percentile.stratum_fmp$ADP) > 0)) %>%
  fix_border_issues()

# Double-checking the average of distributions closely match mathematical expectation
sim.realized.stratum_fmp[, .(MEAN = mean(INSP)), keyby = .(ADP, STRATA, BSAI_GOA)][
][exp_interspersion.realized.stratum_fmp[STRATA != "ZERO"], on = .(ADP, STRATA, BSAI_GOA)
][, .(DIFF = POOL_DMN_INTERSPERSION - MEAN), by = .(ADP, STRATA, BSAI_GOA)]

sim.programmed.stratum_fmp[, .(MEAN = mean(INSP)), keyby = .(ADP, STRATA, BSAI_GOA)][
][exp_interspersion.programmed.stratum_fmp[STRATA != "ZERO"], on = .(ADP, STRATA, BSAI_GOA)
][, .(DIFF = POOL_DMN_INTERSPERSION - MEAN), by = .(ADP, STRATA, BSAI_GOA)]

### Proximity Maps ####

#' [NOTE] *These maps are for internal use to get more granularity of the patterns that led to the resulting proximity
proximity_maps <- plot_interspersion_map(box_def.stratum_fmp, real_interspersion.stratum_fmp, exp_interspersion.realized.stratum_fmp, exp_interspersion.programmed)

# In 2022 for OB_TRW, gaps in weeks 20-27 (find # of trips)
proximity_maps[["2022.OB_TRW"]]$BOX
proximity_maps[["2022.OB_TRW"]]$HEX_ID.realized  
#' Overall more red than blue, meaning the monitoring was more clumped in space than expected given the realized distribution

# In 2022 for OB_HAL, many gaps in the BSAI, especially in the middle of the year
proximity_maps[["2022.OB_HAL"]]$BOX
proximity_maps[["2022.OB_HAL"]]$HEX_ID.realized  
#' Can see the Pribilof Islands were below expected given the realized monitoring rate
# Interestingly, we actually had higher than programmed rates in the BSAI, but it must have been concentrated in the AI and not the BS

# EM_HAL and EM_POT at the time of analysis had many fewer than expected trips in the panhandle. 
# We see more gaps in the latter half of the year when review fell behind.
proximity_maps[["2022.EM_HAL"]]$BOX
proximity_maps[["2022.EM_HAL"]]$HEX_ID.realized

proximity_maps[["2022.EM_POT"]]$BOX
proximity_maps[["2022.EM_POT"]]$HEX_ID.realized

#' EM_TRW_EFP. In 2022, had FEWER gaps than expected near Kodiak. In 2023, the opposite, MORE gaps near Kodiak
proximity_maps[["2022.EM_TRW_EFP"]]$HEX_ID.realized
proximity_maps[["2023.EM_TRW_EFP"]]$HEX_ID.realized

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
spatial_plots_cov <- plot_spatial_coverage(box_def.stratum, realized_mon, sim.realized.stratum, sim.programmed.stratum, strata_levels)
plt.spatial_cov.2022 <- spatial_plots_cov$coverage_2022
plt.spatial_cov.2023 <- spatial_plots_cov$coverage_2023

#' *====================================================================================================================*
# Spatial Analyses ####
#' *====================================================================================================================*

## Spatial Arrangement of Monitored Trips ----

#' Determine whether the spatial arrangement of monitoring was distributed evenly (as expected by random sampling). 
#' Counts the number of monitored trips in each HEX_ID and compares to the simulation outcomes, highlighting any 
#' actual outcomes that were more extreme than 95% of simulated outcomes. Cells start to get color if they are more 
#' extreme than 80% of outcomes.

spatial_plots <- plot_monitoring_spatial(box_def.stratum, realized_mon, sim.realied.stratum, strata_levels)
plt.spatial.2022 <- spatial_plots$plt.spatial.2022
plt.spatial.2023 <- spatial_plots$plt.spatial.2023

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
