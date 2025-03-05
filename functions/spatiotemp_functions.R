# These are the functions from analyses/allocation_evaluation
# Some are further developed version of those in analyses/spatiotemporal_boxes

library(data.table)   # For data wrangling, very efficient with joins, especially rolling joins
library(dplyr)        # For data wrangling/piping with sf package
library(sf)           # For spatial statistics

#' TODO
#' [] Make the params also store the class of each column (i.e., integer for ADP, character for stratum etc)

#======================================================================================================================#
# Data Preparation -----------------------------------------------------------------------------------------------------
#======================================================================================================================#

# This function is used to prepare VALHALLHA datasets for spatiotemporal analyses, e.g., define boxes. Adapted from
# the 2024 ADP, GitHub repo ADP/analyses/allocation_evaluation/data_prep.R. Notably, the calcuation of trip durations, 
# the 'DAYS' column, by modeling the duration of observer trips and estimating the duration of EM trips, was removed.
spatiotemp_data_prep <- function(valhalla){
  #' This function is used for the Annual Report, and therefore uses STRATA to define strata, not STRATA_NEW, which is 
  #' used for the Annual Deployment Plans.
  
  # Subset to partial coverage trips only
  pc_effort_dt <- valhalla[COVERAGE_TYPE == "PARTIAL"]
  # Change TRIP_ID to integer class, keeping original TRIP_ID for posterity as (wd_TRIP_ID, or 'work.data TRIP_ID')
  pc_effort_dt[, wd_TRIP_ID := TRIP_ID][, TRIP_ID := NULL][, TRIP_ID := .GRP, keyby = .(wd_TRIP_ID)]
  # Change VESSEL_ID to integer class and use it to replace PERMIT (some years don't have PERMIT!)
  pc_effort_dt[, PERMIT := NULL][, PERMIT := as.integer(VESSEL_ID)]
  # Count total of partial coverage trips
  pc_trip_id_count <- uniqueN(pc_effort_dt$TRIP_ID)
  
  # For each trip, identify which FMP had most retained catch, splitting FMP by BSAI and GOA
  fmp_bsai_goa <- pc_effort_dt[, .(
    FMP_WT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)
  ), by = .(TRIP_ID, BSAI_GOA = FMP)
  ][, .SD[which.max(FMP_WT)], by = .(TRIP_ID)
  ][, FMP_WT := NULL][]
  if( (pc_trip_id_count != uniqueN(fmp_bsai_goa$TRIP_ID) ) | (pc_trip_id_count != nrow(fmp_bsai_goa)) ) {
    stop("Something went wrong making 'fmp_bsai_goa'")
  }
  
  # For each trip, Identify which FMP had most retained catch, splitting FMP by BS, AI and GOA
  fmp_bs_ai_goa <- copy(pc_effort_dt)[
  ][, BS_AI_GOA := fcase(
    REPORTING_AREA_CODE %in% c(541, 542, 543), "AI",
    REPORTING_AREA_CODE %in% c(508, 509, 512, 513, 514, 516, 517, 518, 519, 521, 523, 524), "BS",
    REPORTING_AREA_CODE %in% c(610, 620 ,630, 640, 649, 650, 659), "GOA")  
  ][, .(
    FMP_WT = sum(WEIGHT_POSTED[SOURCE_TABLE == "Y"], na.rm = T)
  ), by = .(TRIP_ID, BS_AI_GOA)][, .SD[which.max(FMP_WT)], by = .(TRIP_ID)][, FMP_WT := NULL][]
  if( 
    (pc_trip_id_count != uniqueN(fmp_bs_ai_goa$TRIP_ID) ) | 
    (pc_trip_id_count != uniqueN(fmp_bs_ai_goa[!is.na(BS_AI_GOA), TRIP_ID]) ) 
  ){
    stop("Something went wrong making 'fmp_bs_ai_goa'")
  }
  
  pc_effort_dt <- unique(
    pc_effort_dt[, .(
      PERMIT, TARGET = TRIP_TARGET_CODE, AREA = as.integer(REPORTING_AREA_CODE), AGENCY_GEAR_CODE, 
      GEAR = ifelse(AGENCY_GEAR_CODE %in% c("PTR", "NPT"), "TRW", AGENCY_GEAR_CODE), STRATA, OBSERVED_FLAG,
      TRIP_TARGET_DATE, LANDING_DATE, ADFG_STAT_AREA_CODE = as.integer(ADFG_STAT_AREA_CODE), wd_TRIP_ID),
      keyby = .(ADP = as.integer(ADP), TRIP_ID)])
  
  # Merge in FMP classifications
  pc_effort_dt <- pc_effort_dt[fmp_bsai_goa, on = .(TRIP_ID)][fmp_bs_ai_goa, on = .(TRIP_ID)]
  
  # Assign Pool
  pc_effort_dt[, POOL := ifelse(STRATA %like% "EM", "EM", ifelse(STRATA == "ZERO", "ZE", "OB"))]
  
  # Check for missing landing dates
  if(nrow(pc_effort_dt[is.na(LANDING_DATE)])) {
    warning(paste0(nrow(pc_effort_dt[is.na(LANDING_DATE)]), " records are missing LANDING_DATE! and were removed!"))
    pc_effort_dt <- pc_effort_dt[!is.na(LANDING_DATE)]
  }
  
  # Make sure no full coverage trips are still in the dataset.
  if(any(unique(pc_effort_dt$STRATA) == "FULL")) stop("There are some FULL coverage trips in the partial coverage dataset!")
  
  # Replace spaces in STRATA with underscores
  pc_effort_dt[, STRATA := gsub(" ", "_", STRATA)]
  
  # Set the order of columns
  setcolorder(pc_effort_dt, neworder = c(
    "ADP", "POOL", "PERMIT", "TRIP_ID", "STRATA", "AGENCY_GEAR_CODE", "GEAR", "TRIP_TARGET_DATE", "LANDING_DATE", "AREA", 
    "ADFG_STAT_AREA_CODE", "BSAI_GOA", "BS_AI_GOA", "TARGET", "wd_TRIP_ID", "OBSERVED_FLAG"
  ))
  setorder(pc_effort_dt, ADP, POOL, PERMIT, TRIP_TARGET_DATE)
  
  # For some reason, my shapefiles don't include ADFG STAT AREA 515832. Seem like it was merged into 515831. 
  pc_effort_dt[ADFG_STAT_AREA_CODE == 515832, ADFG_STAT_AREA_CODE := 515831]
  pc_effort_dt <- unique(pc_effort_dt)
  
  # Double-check that all trips have a non-NA for STRATA
  if(nrow(pc_effort_dt[is.na(STRATA)])) stop("Some trips don't have a non-NA STRATA")
  
  # Output the results
  pc_effort_dt
}


#======================================================================================================================#
# Space/Time Box Definition  -------------------------------------------------------------------------------------------
#======================================================================================================================#

# Converts ADFG Stat Area to an iso-area hexagon grid. Specify the cell size in meters as the width of each hex cell.
# cell size. The output is a dataframe with corresponding ADFG_STAT_AREA_CODEs and HEX_IDs, as well as the
# simple feature object (class sf) of the hex cell polygons.
stat_area_to_hex <- function(cell_size, spatial_sf){
  
  # 'cell_size' should be specified as a distance in meters. For hex cells, this is the distance between opposite sides.
  # 'stat_area_sf' is the shapefile of the ADFG stat areas in Alaska Albers projection (3467), with each statistical 
  # area and its polygon geometry. For example:
  # stat_area_sf <- st_read("source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  #   select(STAT_AREA) %>%
  #   st_transform(crs = 3467)
  
  # Get centroids of statistical areas
  stat_area_centroid_sf <- suppressWarnings(st_centroid(spatial_sf))  
  
  # Generate a hex cell grid using the projection and boundaries from stat_area_centroid_sf, will cell size specified by
  # 'cell_size'. Subset this using stat_area_centroid_sf so only cells overlapping with it are retained.
  hex_grid_sf <- st_make_grid(
    x = stat_area_centroid_sf, 
    cellsize = cell_size, 
    square = F
  )[stat_area_centroid_sf] 
  # Add data frame to simple feature collection to define hex cell identifier
  hex_grid_sf <- st_as_sf(
    x = data.frame(HEX_ID = seq_len(length(hex_grid_sf))), 
    geometry = hex_grid_sf
  )    
  
  # Identify intersections of hex_grid polygons with statistical area centroids
  stat_area_hex_df <- do.call(
    what = rbind,
    args = Map(
      f = function(x,y) data.frame(HEX_ID = x, ADFG_STAT_AREA_CODE = stat_area_centroid_sf$STAT_AREA[y]),
      x = hex_grid_sf$HEX_ID,
      y = st_intersects(hex_grid_sf, stat_area_centroid_sf)
    )
  )
  
  # TODO assign attribute of cell_size to output object?
  return(
    list(
      CELL_SIZE = cell_size,
      STAT_AREA_HEX_DF = stat_area_hex_df,
      HEX_GEOMETRY = hex_grid_sf
    )
  )
}

# Applies a box definition to fishing effort prepared by spatiotemp_data_prep(). A modified version of define_boxes_gs
# that was used in the 2024 ADP, but adjusted to separate spatiotemporal domains from nonspatiotemporal domains, which
# allows trips that are defined to be in either the BSAI or GOA to still neighbor each other according to the normal
# neighboring rules of the box definition
define_boxes <- function(data, space, time, year_col, stratum_cols, dmn_lst = NULL, spatial_sf = stat_area_sf, geom = F, ps_cols = NULL) {
  # TODO Haven't done much testing with both ps_cols and dmn_lst defined with GEAR type. 
  
  # data <- copy(swor_bootstrap.effort); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; stratum_cols <- c("STRATA"); geom <- F; ps_cols <- c("GEAR"); dmn_lst <- NULL
  
  
  # data <-  copy(val_mixed); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- "GEAR"; stratum_cols <- c("STRATA"); geom = T; ps_cols <- "GEAR"
  
  # data <- copy(pc_dat); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- "BSAI_GOA"; stratum_cols <- c("STRATA");  geom = T; ps_cols <- NULL
  
  # using new dmn_cols
  #'*GEAR BSAI_GOA*   data <- copy(pc_dat); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_lst <- list(nst = "GEAR", st = "BSAI_GOA"); stratum_cols <- c("STRATA");  geom = T; ps_cols <- NULL
  #'*GEAR*            data <- copy(pc_dat); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_lst <- list(nst = "GEAR", st = NULL); stratum_cols <- c("STRATA");  geom = T; ps_cols <- NULL
  #'*BSAI_GOA*        data <- copy(pc_dat); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_lst <- list(nst = NULL, st = "BSAI_GOA"); stratum_cols <- c("STRATA");  geom = T; ps_cols <- NULL
  #'*NULL*            data <- copy(pc_dat); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_lst <- NULL; stratum_cols <- c("STRATA");  geom = T; ps_cols <- NULL
  
  #' TODO Make sure none are duplicated between ts and nonts dmn_cols
  
  # Testing with ps_cols = NULL
  # data <-  copy(val_mixed); space <- c(2e5, 2e5); time <- c("week", 1, "TRIP_TARGET_DATE", "LANDING_DATE"); year_col <- "ADP"; dmn_cols <- "GEAR"; stratum_cols <- c("STRATA"); geom = T; ps_cols <- NULL
  
  if( is.null(dmn_lst) ){
    dmn_lst <- list(nst = NULL, st = NULL)
  } else {
    # Make sure dmn_lst doesn't have repeats
    if( length(intersect(dmn_lst$nst, dmn_lst$st)) > 1 ) {
      stop("`dmn_lst` can't have the same column under both categorical and spatiotemporal elements!")
    }
  }
  # Prepare domain column vector
  dmn_cols <- unlist(dmn_lst, use.names = F)
  
  #==================================#
  # NOTE! Remove any jig-gear trips! #
  #==================================# 
  # Jig-gear trips are generally in zero coverage (though sometimes HAL or EM_HAL trips might also fish
  # with jig gear in a trip?). Therefore, from an allocation perspective, it's not important if the rate will be 0.
  # Additionally, from an evaluation perspective, we don't use observed trips to make estimates for jig trips, so
  # we can remove them without issue. MAKE SURE 'data' HAS NO JIG GEAR COMPONENTS!
  
  # Check for jig-gear in the dataset
  check_jig_cols <- unique(c(stratum_cols, dmn_cols, ps_cols))
  check_jig_cols <- apply(data[, ..check_jig_cols], 2, function(x) any(x == "JIG"))
  # Remove jig records
  if(any(check_jig_cols)) {
    jig_cols <- names(which(check_jig_cols))
    for(i in jig_cols) {
      jig_trips <- data[which(data[[jig_cols]] == "JIG"), ]
      cat(paste0("Removing ", nrow(jig_trips), " rows from ", uniqueN(jig_trips$TRIP_ID), " JIG trips from ", i, " column."), "\n")
      data <- fsetdiff(data, jig_trips)
    }
  }
  
  win <- as.integer(time[2])
  
  # Make sure integer TRIP_ID, integer ADFG_STAT_AREA_CODE, and 'dmn_cols' are specified in the data
  if( length( intersect(colnames(data), c("TRIP_ID", "ADFG_STAT_AREA_CODE", dmn_cols))) !=  length(c("TRIP_ID", "ADFG_STAT_AREA_CODE", dmn_cols)) ) {
    stop(paste0("'data' must have columns 'TRIP_ID' and 'ADFG_STAT_AREA_CODE and dmn_cols: ", paste(dmn_cols, collapse = ", ")))
  } else {
    if( all(lapply(data[, c("TRIP_ID", "ADFG_STAT_AREA_CODE")], class) != "integer")) {
      stop ("'TRIP_ID' and 'ADFG_STAT_AREA_CODE' must be of class integer.")
    }
  }
  if(nrow(unique(data[, .(TRIP_ID, STRATA)])) != uniqueN(data$TRIP_ID)) stop("A trip is not in only one stratum!")
  
  # Remove any unused columns
  keep_cols <- unique(c(year_col, stratum_cols, dmn_cols, "ADFG_STAT_AREA_CODE", time[3], time[4], "TRIP_ID", ps_cols))
  data <- unique(subset(data, select = keep_cols))      
  
  #========================#
  # Convert ADFG to HEX_ID #
  #========================#
  
  stat_area_lst <- stat_area_to_hex(space[1], spatial_sf)
  stat_area_dist_lst <- suppressWarnings(apply(
    X = round(st_distance(st_centroid(stat_area_lst$HEX_GEOMETRY))), 
    MARGIN = 1, 
    FUN = function(x) which(x <= space[2])))
  data <- unique(data.table(stat_area_lst$STAT_AREA_HEX_DF)[
  ][data, on = .(ADFG_STAT_AREA_CODE)
  ][, -"ADFG_STAT_AREA_CODE"])
  setcolorder(data, neworder = unique(c(year_col, stratum_cols, ps_cols, dmn_cols, "HEX_ID", time[3], time[4])))
  setkeyv(data, cols = unique(c(year_col, stratum_cols, ps_cols, dmn_cols, "HEX_ID", time[3], time[4])))
  
  if(nrow(data[is.na(HEX_ID)])) {
    print(data[is.na(HEX_ID)])
    stop("Could not assign a HEX_ID!")
  }
  
  #======================#
  # Define temporal unit #
  #======================#
  
  # First, get all years and a table converting date to week
  
  if(time[1] != "week") stop("So far this function only works with 'week()' function!")
  
  time_cols <- time[3:4]
  
  date_range <- range(unlist(data[, ..time_cols]))     # Get range of dates, converting to numeric
  date_vec <- as.Date(date_range[1] : date_range[2])   # Get date class of all dates within range
  week_vec <- sapply(date_vec, get(time[[1]]))         # Identify the week of each date
  dates_mtx <- cbind(date_vec, week_vec)               # Combine date vector and week vector
  
  dates_start <- min(dates_mtx[, 1]) - 1  # Get first date and subtract 1.
  dates_mtx[, 1] <- dates_mtx[, 1] - dates_start  # This makes it so matrix can be reference by row index, much faster
  # TODO Check behavior of neighboring at ADP year thresholds
  
  # Grab time columns and define groups based on TRIP_ID and HEX_ID
  time_int <- data[, ..time_cols]
  time_int[, (time_cols) := lapply(.SD, as.integer), .SDcols = time_cols]
  setnames(time_int, new = c("S", "E"))
  data_int <- data[, .(TRIP_ID, HEX_ID)][, GRP := .GRP, by = .(TRIP_ID, HEX_ID)][]
  # Convert to matrix and split by TRIP_ID and HEX_ID
  time_lst <- as.matrix(cbind(time_int - dates_start, data_int[, .(GRP)]))
  time_lst <- lapply(split(time_lst, time_lst[, "GRP"], drop = F), matrix, ncol = 3)
  # For each TRIP_ID × HEX_ID, identify unique weeks
  time_lst <- lapply(time_lst, function(x) {
    dates_int <- unique(unlist(apply(x, 1, function(y) y[1] : y[2], simplify = F)))  # get unique days
    unique(dates_mtx[dates_int, 2, drop = F])                     # Identify week using dates_mtx
  }) 
  
  # Collapse into a data.table (this is much faster than rbindlist(lapply(data_lst, as.data.table)))
  data_dt <- as.data.table(
    do.call(rbind, Map(function(x1, x2) cbind(x1, x2), x1 = as.list(seq_along(time_lst)), x2 = time_lst)))
  setnames(data_dt, new = c("GRP", "TIME"))
  # Merge
  data_int <- unique(data_int)[data_dt, on = .(GRP)][, - "GRP"]
  keep_cols2 <- c(setdiff(keep_cols, c(time_cols, "ADFG_STAT_AREA_CODE")), "HEX_ID")
  data <- unique(data[, ..keep_cols2])[data_int, on = .(TRIP_ID, HEX_ID)]
  
  #============#
  # Define Box #
  #============#
  
  setkeyv(data, c(year_col, stratum_cols, "TIME", "HEX_ID"))
  setcolorder(data, unique(c(year_col, stratum_cols, ps_cols, "TIME", "HEX_ID")))
  # 'BOX_ID' is defined by HEX_ID and TIME and if specified, ps_cols, and is common to all data. This will be used to determine neighbors.
  # 'stratum_cols' is not included so that we can later compare overlap between strata
  data[, BOX_ID := .GRP, by = c(year_col, "TIME", "HEX_ID")]              
  if(!is.null(ps_cols)) {
    data[, PS_ID := .GRP, keyby = ps_cols]
    ps_cols_tbl <- unique(subset(data, select = c(ps_cols, "PS_ID")))
  } else {
    ps_cols_tbl <- data.table(PS = "NA", PS_ID = 1L)
    data[, PS_ID := 1L] 
  }
  #' TODO *NEW* Add columns for ts_dmn_cols and nonts_dmn_cols if NULL?
  
  setkey(data, BOX_ID)
  
  #==================#
  # Define Neighbors #
  #==================#
  
  # For each BOX_ID, find which BOX_IDs are neighboring based on week and spatial cells.
  sub_cols <- c(year_col, "TIME", "HEX_ID", "BOX_ID")
  st_mtx <- as.matrix(unique(data[, ..sub_cols]))
  nbr_lst <- apply(st_mtx, MARGIN = 1, function(x) {
    # x <- st_mtx[1,]
    box_mtx <- st_mtx[st_mtx[,1] == x[1], , drop = F]             # subset by year
    nbr_time <- (x[2] + (-win:win))                               # identify neighboring TIME
    box_mtx <- box_mtx[box_mtx[,2] %in% nbr_time, , drop = F]     # subset by time range
    nbr_hex_id <- stat_area_dist_lst[[x[3]]]                      # identify neighboring HEX_IDs
    box_mtx[box_mtx[, 3] %in% nbr_hex_id, 4]                      # subset by neighboring HEX_IDs, then grab BOX_ID
  }) # 0.5 sec, but should save time when identifying number of neighbors?
  # each element of this list corresponds to BOX_ID and contains all neighboring BOX_IDs
  
  #===================================================#
  # Split up the dataset by year_col and stratum_cols #
  #===================================================#
  
  data_dmn <- copy(data)
  if(!is.null(dmn_cols)) {
    data <- unique(data[, -..dmn_cols])  # Remove 'dmn_cols' for now
  }
  
  group_cols  <- c(year_col, stratum_cols)
  setkeyv(data, cols = c(group_cols, "PS_ID", "BOX_ID"))
  data_lst <- lapply(
    X = split(x = subset(data, select = c(group_cols, "BOX_ID", "TRIP_ID", "PS_ID")), by = group_cols, keep.by = F),
    FUN = as.matrix)
  
  # Make the frequency table of each TRIP_ID (so that trips are properly split by HEX_ID, TIME, and if present, ps_cols)
  trip_id_mat <- do.call(rbind, lapply(
    data_lst,
    function(p) {
      trip_id_frq <- table(p[, "TRIP_ID"])
      matrix(
        c(as.integer(names(trip_id_frq)), trip_id_frq),
        ncol = 2, dimnames = list(NULL, c("TRIP_ID", "Freq")))
    }))
  trip_id_mat <- trip_id_mat[order(trip_id_mat[,1]), ]
  trip_id_vec <- vector(mode = "integer")
  trip_id_vec[trip_id_mat[, 1]] <- trip_id_mat[, 2]
  
  #==================================================#
  # Identify which TRIP_IDs are neighboring each box #
  #==================================================#
  
  box_smry <- lapply(
    data_lst,
    function(stratum_mtx) {
      # as [1:BOX_ID] [2:TRIP_ID] and if specified, [3:PS_ID]
      # stratum_mtx <- data_lst[[23]]   # 2022.OB_FIXED
      # stratum_mtx <- data_lst[[24]]   # 2022.OB_TRW
      
      # First, split by PS_ID
      ps_lst <- lapply(split(stratum_mtx[, 1:2], stratum_mtx[,3]), matrix, ncol = 2)
      
      # Now for each PS_ID group, identify neighboring trips
      lapply(ps_lst, function(x) {
        # x <- ps_lst[[1]]    # Here PS_ID = 1 is POT
        
        # Get all unique time and space boxes
        x1 <- unique(x[, 1])
        
        # Now for each BOX_ID listed in 'x1', count the number unique TRIP_IDs in neighboring BOX_IDs
        x2 <- do.call(rbind, lapply(x1, function(y) {
          # y <- x1[1]   # Box 5749
          
          trip_id_centered <- x[x[,1] == y, 2]    # Identify number of trips actually in the box
          # There shouldn't ever be the same trip counted twice in the same box.
          if( length(trip_id_centered) != length(unique(trip_id_centered)) ) stop("There is a duplicate trip_id!")
          
          cbind(
            BOX_ID = y,
            # Count of trips centered in BOX_ID
            BOX_n = length(trip_id_centered) ,
            # Weight of trips centered in BOX_ID
            # FIXME calculating BOX_w is slow!
            BOX_w = sum(1/trip_id_vec[trip_id_centered]), 
            # # Count of unique TRIP_IDs in neighboring BOX_IDs
            BOX_nbr = length(unique(x[(x[,1] %in% nbr_lst[[y]]), 2])) 
          )
          
        }))
      })
    }
  ) 
  
  # Calculate STRATA_N and ensure the sum of weights is equivalent
  strata_N_dt <- data[, .(STRATA_N = uniqueN(TRIP_ID)), by = group_cols]
  
  # Double-check that weights sum to STRATA_N
  if(!(all(
    unname(sapply(box_smry, function(x) sum(sapply(x, function(y) sum(y[, "BOX_w"]))))) == strata_N_dt$STRATA_N
  ))) stop("STRATA_N and sum(BOX_w) are not equal!")
  
  # Get post-strata weights (number of component trips in post-strata)
  ps_W_dt <- data[, .(ps_W = sum(1 / trip_id_vec[TRIP_ID])), by = c(group_cols, "PS_ID")]
  ps_W_dt <- ps_W_dt[ps_cols_tbl, on = .(PS_ID)]  # Merge in ps_cols on PS_ID
  setkeyv(ps_W_dt, c(year_col, stratum_cols, "PS_ID"))
  
  # Create data.table output
  st_dt <- unique(data[, .(BOX_ID, HEX_ID, TIME)])   # Table of BOX_ID, HEX_ID, and TIME
  dt_out <- 
    rbindlist(lapply(box_smry, function(x) rbindlist(lapply(x, as.data.table), idcol = "PS_ID")), idcol = "GROUP_COLS")[
    ][, (group_cols) := tstrsplit(GROUP_COLS, split = "[.]")
    ][, GROUP_COLS := NULL][
    ][st_dt, on = .(BOX_ID)]
  dt_out <- dt_out[
  ][, which(colnames(dt_out) == year_col) := as.integer(dt_out$ADP)
  ][, PS_ID := as.integer(PS_ID)
  ][ps_cols_tbl, on = .(PS_ID)]
  setcolorder(dt_out, c(group_cols, "PS_ID", "BOX_ID", "BOX_n", "BOX_w", "BOX_nbr", ps_cols, "HEX_ID", "TIME"))
  
  # Initialize outputs
  box_res <- list(
    box_smry = box_smry,
    strata_n_dt = strata_N_dt,
    ps_W_dt = ps_W_dt,
    dt_out = dt_out,
    og_data = data,
    nbr_lst = nbr_lst,
    params = list(stratum_cols = stratum_cols, year_col = year_col, dmn_cols = dmn_cols, ps_cols = ps_cols)
  )
  
  #==========================#
  # Handle dmn_cols now (GEAR)
  #==========================#
  
  #' TODO dmn_lst is never not null since it is now created if left null. Can remove? Always create dmn outputs?
  if( !is.null(dmn_cols) ) {
    
    # Save the raw form of data_dmn for the output
    data_dmn_og <- copy(data_dmn)
    
    # Get trip weights, splitting by dmn_cols as well (both categorical and spatiotemporal)
    trip_id_dmn_mat <- as.matrix(data_dmn[, .N, by = .(TRIP_ID)])
    trip_id_dmn_vec <- vector(mode = "integer")
    trip_id_dmn_vec[trip_id_dmn_mat[, 1]] <- trip_id_dmn_mat[, 2]
    
    # Codify domains. If nonspatiotemporal or spatiotemporal domains are null, create placeholders
    if( is.null(dmn_lst$nst) ){
      data_dmn[, NST_DMN := 1L]
      nst_dmn_vec <- "NST_DMN"
      nst_dmn_tbl <- unique(data_dmn[, ..nst_dmn_vec])
    } else {
      nst_dmn_vec <- unlist(dmn_lst$nst, use.names = F)
      data_dmn[, NST_DMN := .GRP, keyby = nst_dmn_vec]
      nst_dmn_tbl <- unique(subset(data_dmn, select = c("NST_DMN", nst_dmn_vec)))
      data_dmn[, (nst_dmn_vec) := NULL]
    }
    if( is.null(dmn_lst$st) ){
      data_dmn[, ST_DMN := 1L]
      st_dmn_vec <- "ST_DMN"
      st_dmn_tbl <- unique(data_dmn[, ..st_dmn_vec])
    } else {
      st_dmn_vec <- unlist(dmn_lst$st, use.names = F)
      data_dmn[, ST_DMN := .GRP, keyby = st_dmn_vec]
      st_dmn_tbl <- unique(subset(data_dmn, select = c("ST_DMN", st_dmn_vec)))
      data_dmn[, (st_dmn_vec) := NULL]
    }
    
    #' Split by domain
    nst_dmn_lst <- split(data_dmn, by = "NST_DMN", keep.by = F)
    #' *For each non-spatiotemporal domain [x]*
    dmn_nbr_dt <- rbindlist(lapply(nst_dmn_lst, function(x) {
      # x <- nst_dmn_lst[[1]]
      
      # Identify boxes relevant to nst_dmn
      nst_dmn_boxes <- unique(x$BOX_ID)
      #' Split by stratum and convert to matrix
      nst_dmn_stratum_lst <- lapply(
        split(x[, -c(year_col, "TIME", "HEX_ID"), with = F], by = stratum_cols, keep.by = F), 
        as.matrix
      )
      
      #' *For each stratum [y]*
      rbindlist(lapply(nst_dmn_stratum_lst, function(y) {
        # y <- nst_dmn_stratum_lst[[1]]
        
        nst_dmn_stratum_st_dmn_lst <- lapply(
          split(y, f = y[, "ST_DMN"]), 
          matrix, ncol = ncol(y), dimnames = list(NULL, colnames(y))
        )
        
        #' *For each spatiotemporal domain*
        rbindlist(lapply(nst_dmn_stratum_st_dmn_lst, function(z) {
          # z <- nst_dmn_stratum_st_dmn_lst[[1]]
          
          #' *For each BOX_ID*
          as.data.table(do.call(rbind, lapply(nst_dmn_boxes, function(z1) {
            # z1 <- nst_dmn_boxes[1]
            
            # Identify all trips in the domain × stratum in this box
            trip_id_centered <- unique(z[z[, "BOX_ID"] == z1, "TRIP_ID"])
            c(
              BOX_ID = z1,
              BOX_DMN_n = length(trip_id_centered),
              BOX_DMN_w = sum(1 / trip_id_dmn_vec[trip_id_centered]),
              # count number of neighboring trips in the nst_dmn × stratum ()
              BOX_DMN_nbr = length(unique(y[y[, "BOX_ID"] %in% nbr_lst[[z1]], "TRIP_ID"]))
            )
            
          })))
        }), idcol = "ST_DMN")
      }), idcol = "STRATUM_COLS")
    }), idcol = "NST_DMN")
    
    # Restore stratum and domain column names
    dmn_groups <- c("NST_DMN", "ST_DMN")
    dmn_nbr_dt[
    ][, (stratum_cols) := tstrsplit(STRATUM_COLS, split = "[.]")
    ][, STRATUM_COLS := NULL
    ][, (dmn_groups) := lapply(.SD, as.integer), .SDcols = dmn_groups]
    dmn_nbr_dt <- dmn_nbr_dt[nst_dmn_tbl, on = .(NST_DMN)][st_dmn_tbl, on = .(ST_DMN)]
    dmn_nbr_dt[, (dmn_groups) := NULL]
    
    #' TODO *is BOX_ID unique to ADP or can there be crossover between years? Does time start over each year, or is it independent?*
    
    # Merge details back in (ADP year, HEX_ID and TIME)
    box_id_details <- unique(data_dmn[, .(BOX_ID, ADP, HEX_ID, TIME)])
    dmn_nbr_dt <- box_id_details[dmn_nbr_dt, on = .(BOX_ID)]
    setcolorder(dmn_nbr_dt, c(year_col, stratum_cols, dmn_cols, "BOX_ID", "BOX_DMN_n", "BOX_DMN_w", "BOX_DMN_nbr"))
    
    # Calculate Number of trips in each STRATA × dmn_cols. Note that trips that have multiple 
    # 'dmn_cols' were split here!
    strata_dmn_N_dt <- dmn_nbr_dt[, .(STRATA_DMN_N = sum(BOX_DMN_w)), by = c(year_col, stratum_cols, dmn_cols)]
    
    # Double-check that weights sum to STRATA_N
    if(!fsetequal(
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][STRATA_N != 0],
      strata_N_dt
    )) stop("STRATA_N and sum(BOX_DMN_w) are not equal!")
    
    box_res$dmn <- list()
    box_res$dmn$og_data <- data_dmn_og 
    box_res$dmn$strata_dmn_n_dt <- strata_dmn_N_dt
    box_res$dmn$box_dmn_smry_dt <- dmn_nbr_dt
    box_res$dmn$strata_dt <- setorderv(unique(strata_N_dt[, ..stratum_cols]), cols = stratum_cols)[, STRATUM_ID := .I][]
    
  }
  
  # Create sf object with geometry if requested
  if(geom == T) {
    geom_sf <- merge(stat_area_lst$HEX_GEOMETRY, dt_out, on = .(HEX_ID))
    box_res$geom_sf <- geom_sf
    
    geom_dmn_sf <- merge(stat_area_lst$HEX_GEOMETRY, dmn_nbr_dt, on = .(HEX_ID))
    box_res$dmn$geom_dmn_df <- geom_dmn_sf
  }
  
  # Return results
  box_res
  
}

#======================================================================================================================#
# Evaluation Functions -------------------------------------------------------------------------------------------------
#======================================================================================================================#

# calculate_dmn_interspersion4 from the 2024 ADP
calculate_dmn_interspersion <- function(box_def, selection_rates, acceptor_donor_lst) {
  
  # TODO Manually add the fill for the pools!

  year_col <- box_def$params$year_col
  stratum_cols <- box_def$params$stratum_cols
  dmn_cols <- box_def$params$dmn_cols
  
  stratum_dt <- box_def$dmn$strata_dt
  
  # If 'geom' is present, get HEX_ID geometries
  if(!is.null(box_def$dmn$geom_dmn_df)) hex_id_geom <- box_def$dmn$geom_dmn_df %>% select(HEX_ID, geometry) %>% unique()
  
  # Merge in the specified sample rates
  dmn_dat <- copy(box_def$dmn$box_dmn_smry_dt)
  dmn_dat[, STRATUM_ID := stratum_dt[dmn_dat, STRATUM_ID, on = stratum_cols]]  # Merge in STRATUM_ID
  dmn_dat[, SAMPLE_RATE := selection_rates[dmn_dat, SAMPLE_RATE, on = c(year_col, stratum_cols)]]
  # For each domain, calculate the probability that it will be sampled give the sample rate and number of neighbors
  dmn_dat[, BOX_SAMPLE_PROB :=  1 - (1 - SAMPLE_RATE)^BOX_DMN_nbr]
  
  out <- vector(mode = "list", length = nrow(stratum_dt))
  
  # For each acceptor stratum...
  for(i in 1:length(out)) {
    # i <- 4

    if( nrow(stratum_dt[ acceptor_donor_lst[[i]] ]) == 0 ) {
      # If there are no donors, skip
      out[[i]] <- NULL
    } else {
      
      # Identify the 'donor' stratum
      focus_stratum <- stratum_dt[i, ..stratum_cols]
      
      # Subset the data to include the acceptor and its donors, then split by domain
      focus_dmn_dt <- unique(rbind(
        dmn_dat[focus_stratum, on = stratum_cols],
        dmn_dat[stratum_dt[acceptor_donor_lst[[i]], ..stratum_cols], on = stratum_cols]
      ))
      if(is.null(dmn_cols)){
        focus_dmn_lst <- list(focus_dmn_dt)
      } else {
        focus_dmn_lst <- split(focus_dmn_dt, by = dmn_cols)
      }
      # focus_dmn_lst <- split(unique(rbind(
      #   dmn_dat[focus_stratum, on = stratum_cols],
      #   dmn_dat[stratum_dt[acceptor_donor_lst[[i]], ..stratum_cols], on = stratum_cols]
      # )), by = dmn_cols)
      
      out[[i]] <- rbindlist(lapply(focus_dmn_lst, function(x) {
        # x <- focus_dmn_lst[[1]]
        
        # Subset acceptor boxes, excluding any that have no trips (weight of 0)
        acceptor_dt <- x[focus_stratum, on = stratum_cols][BOX_DMN_w > 0]
        acceptor_dt[, c("SAMPLE_RATE", "BOX_SAMPLE_PROB", "STRATUM_ID") := NULL]
        
        donor_dt_cols <- c(year_col, "STRATUM_ID",  "BOX_ID", "BOX_SAMPLE_PROB")
        donor_dt <- x[stratum_dt[acceptor_donor_lst[[i]]], on = stratum_cols][, ..donor_dt_cols]
        
        # Merge in donor probabilities to acceptor by BOX_ID. 
        donor_sample_probs <- merge(acceptor_dt, donor_dt, by = c(year_col, "BOX_ID"), all.x = T)
        # Then, for each BOX, find the probability that at least one trip by any neighboring donor is sampled 
        # which is (1 - probability that no neighboring donor trips are sampled)
        donor_sample_probs[, .(BOX_DONOR_SAMPLE_PROB = 1 - prod(1 - BOX_SAMPLE_PROB)), by = c(year_col, stratum_cols, dmn_cols, "BOX_ID", "HEX_ID", "TIME", "BOX_DMN_n", "BOX_DMN_w")]
        
      }))
      
    }

    # For the acceptor's domains, combine the box sample probabilities by all donors
 
  }
  
  out <- rbindlist(out)
  out  # This is a raw output that has probabilities separated by year, stratum, and domain. In practice, we will
  # likely combine strata into pools (e.g., all HAL trips within EM_HAL and EM_POT or all HAL trips within OB_HAL and OB_POT)
  
  
  # TODO Make summaries that groups by POOL and dmn_cols, with and without splitting by BSAI_GOA
  
  # Group by pool! (Include this in stratum_dt, so that this group and dmn_cols are combined)
  
  # Identify Pool
  pool_dt <- copy(out)
  pool_dt[, POOL := fcase(STRATA %like% "EM", "EM", STRATA %like% "OB", "OB", STRATA %like% "ZERO", "ZERO")][]
  
  list(
    RAW = out,
    POOLED = pool_dt,
    params = box_def$params,
    geom = hex_id_geom
  )
  
}

#' This function calculates the expected stratum-specific interspersion (the acceptor_donor_lst matches each stratum 
#' with itself only)
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


#' \TODO I think this function is doing what I've asked to do so far, but it probably isn't handling non-spatiotemporal
#' domains correctly! I have since updated define_boxes to have $dmn$og_data so that each TRIP_ID and the domains it 
#' belongs to are specified. 
calculate_realized_interspersion <- function(box_def, monitored_trips) {
  # box_def <- copy(box_def.stratum); monitored_trips <- copy(realized_mon)
  # box_def <- copy(box_def.stratum_fmp); monitored_trips <- trips[unlist(sample_lst)]
  #' \TESTING monitored_trips <- copy(trips[unlist(sample_lst)])
  
  year_strata <- unlist(box_def$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- unlist(box_def$params[c("year_col", "stratum_cols", "dmn_cols")], use.names = F)
  
  # Identify which boxes were sampled This join is faster when keyed
  setkeyv(monitored_trips, c(year_strata,  "TRIP_ID"))
  setkeyv(box_def$og_data, c(year_strata,  "TRIP_ID"))
  box_sample <- copy(box_def$og_data)[monitored_trips]
  box_sample <- unique(subset(box_sample, select = c(year_strata, "TIME", "HEX_ID", "BOX_ID")))
  # split by year and strata
  dmn_sample <- split(box_sample, by = year_strata)
  # Identify boxes in sampled neighborhoods using the neighbor list
  nbr_sample <- lapply(
    lapply(dmn_sample, function(x) box_def$nbr_lst[ x[["BOX_ID"]] ]), 
    function(y) unique(unlist(y)))
  
  #' *4.7ms*
  nbr_sample_dt <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")[
  ][, (year_strata) := tstrsplit(ADP.STRATA, split = "[.]")   # This takes 3ms
  ][, "ADP.STRATA" := NULL
  ][, ADP := as.integer(ADP)][]
  
  if(F) {
    
    a <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")
    a[, ADP := gsub("\\..*$", "", ADP.STRATA)]
    a[, STRATA := gsub("^.*\\.", "", ADP.STRATA)]
    
    a <- rbindlist(lapply(nbr_sample, function(x) data.table(BOX_ID = x)), idcol = "ADP.STRATA")[
    ][, (year_strata) := tstrsplit(ADP.STRATA, split = "[.]", type.convert = T)   # This takes 3ms
    ][, "ADP.STRATA" := NULL][]
    
   
    #'\TODO Is it possible to Just load a huge matrix across iterations of sampled boxes? and applying weights to each?
    
    #' This is a little faster since I'm not having to run tstrsplit on every row, and dont' need to create and delete ADP.STRATA
    b <- do.call(rbind, Map(
      function(x,y) {
        cbind(setnames(as.data.table( tstrsplit(y, split = "[.]", type.convert = T)), year_strata),
              data.table(BOX_ID = x))},
      x = nbr_sample,
      y = names(nbr_sample)
    ))
  
    
  }
  
  #' *=================*
  
  strata_domain_n <- box_def$dmn$strata_dmn_n_dt
  
  insp_dt <- box_def$dmn$box_dmn_smry_dt[
  ][nbr_sample_dt, on = c(year_strata, "BOX_ID")
  ][!is.na(BOX_DMN_w)]
  
  insp_dt_sum <- insp_dt[
  ][, .(SUM_DMN_w = sum(BOX_DMN_w)), keyby = domains
  ][strata_domain_n, on = domains
  ][, INSP := SUM_DMN_w / STRATA_DMN_N][]
  
  setattr(insp_dt_sum, "sampled_boxes", nbr_sample_dt)
  
  insp_dt_sum[!is.na(INSP)]
}


# A wrapper for calculate_realized_interspersion, but randomly samples to create new `monitored_trips` object
simulate_interspersion <- function(box_def, sample_rates, iter, seed, hex_smry = F) {
  # box_def <- copy(box_def.stratum); sample_rates <- copy(programmed_rates); iter <- 1; seed <- 12345
  # box_def <- copy(box_def.stratum_fmp); sample_rates <- copy(programmed_rates); iter <- 1000; i <- 1; seed <- 12345
  
  year_strata <- unlist(box_def$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- box_def$params$dmn_cols
  sim_lst <- vector(mode = "list", length = iter)
  mon_lst <- vector(mode = "list", length = iter)  # A list of TRIP_IDs sampled
  
  set.seed(seed)
  
  # Iterate sampling of trips, then calculate the interspersion achieved from each iteration of sampling
  for(i in seq_len(iter)){
    
    if(i %% round(iter/10) == 0) cat(paste0(i, ", "))
    
    # Sample according to sample rates, 
    sample_lst <- apply(setorderv(sample_rates, year_strata), 1, function(x) runif(x[["STRATA_N"]]) < x[["SAMPLE_RATE"]])
    # Make a data.table of trips
    trips <- setorderv(unique(subset(box_def$og_data, select = c(year_strata, "TRIP_ID"))), year_strata)
    #trips$SAMPLE <- unlist(sample_lst)
    
    mon_lst[[i]] <- trips[unlist(sample_lst), TRIP_ID]
    sim_lst[[i]] <- calculate_realized_interspersion(box_def, trips[unlist(sample_lst)])            #' \TODO Speed this function up!!
  } 
  
  sim_dt <- rbindlist(sim_lst, idcol = "ITER")
  
  # sim_dt[, .(MEAN = mean(INSP)), keyby = year_strata] 
  # Capture the domains as an attribute
  setattr(sim_dt, "year_strata_domains", c(year_strata, domains))
  setattr(sim_dt, "mon_lst", mon_lst)
  
  # If you want a hex-level summary of each, count the number of times each BOX was sampled
  if(hex_smry) {
    # For each simulation, total monitored/neighboring monitored trips for each HEX_ID
    cat("\nCompiling summary of HEX_IDs for spatial analyses...\n")

    nbr_smry <- lapply(sim_lst, function(x) {
      # x <- sim_lst[[1]]
      mon_box.lst <- lapply(
        split(setkeyv(attr(x, "sampled_boxes"), year_strata), by = c(year_strata), keep.by = F), 
        unlist, use.names = F)
      # Identify neighbors of monitored trips
      lapply(mon_box.lst, function(x) unique(as.integer(unlist(box_def$nbr_lst[x]))))
    })

    # Compile results for each year_strata across iterations
    hex_smry_dt <-  rbindlist(
      lapply(nbr_smry, function(x) {
        # x <- nbr_smry[[1]]
        rbindlist(lapply(x, function(y) data.table(BOX_ID = y)), idcol = "YEAR_STRATA")
      }), idcol = "ITER") |>
      _[, (year_strata) := tstrsplit(YEAR_STRATA, split = "[.]")
      ][, YEAR_STRATA := NULL
      ][, ADP := as.integer(ADP)
        # Merge with the box definition to get box weights
      ][box_def$dt_out, on = c(year_strata, "BOX_ID"), allow.cartesian = T
        # By iteration and stratum, total the weight of boxes of HEX_IDs in sampled neighborhoods
      ][, .(HEX_w = sum(BOX_w)), keyby = c("ITER", year_strata, "HEX_ID")]
    # Attach the summary to the outputs as an attribute
    setattr(sim_dt, "hex_smry", hex_smry_dt)
  }
  
  # Return the results
  sim_dt
}

# Function for getting percentile given distribution and realized rates
insp_percentile <- function(realized_insp, prog_sim, real_sim){
  # realized_insp <- copy(real_interspersion.stratum_fmp); prog_sim <- copy(sim.programmed.stratum_fmp); real_sim = copy(sim.realized.stratum_fmp)
  
  year_strata_domains <- attr(prog_sim, "year_strata_domains")
  
  out_lst <- vector(mode = "list", length = nrow(realized_insp))
  for(i in 1:length(out_lst)) {
    
    # Subset the data for each group
    dat_sub <- realized_insp[i,]
    # Define functions to calculate percentiles (feed in all simulated interspersion values from both distributions
    ecdf_fun.prog <- ecdf(prog_sim[dat_sub[, ..year_strata_domains], on = (year_strata_domains), INSP])
    ecdf_fun.real <- ecdf(real_sim[dat_sub[, ..year_strata_domains], on = (year_strata_domains), INSP])
    # Calculate the percentiles and send to output list
    out_lst[[i]] <- cbind(dat_sub, PERC_PROG = ecdf_fun.prog(dat_sub$INSP), PERC_REAL = ecdf_fun.real(dat_sub$INSP))
  }
  
  setorderv(rbindlist(out_lst), year_strata_domains)[]
  
}

dmn_insp_percentile <- function(realized_insp, prog_sim, real_sim){

  # realized_insp <- copy(test); prog_sim <- copy(dmn_insp.prog); real_sim <- copy(dmn_insp.real)
  year_pool_domains <- c("ADP", "POOL", "BSAI_GOA", "GEAR")
  
  out_lst <- vector(mode = "list", length = nrow(realized_insp))
  for(i in 1:length(out_lst)) {
    
    # Subset the data for each group
    dat_sub <- realized_insp[i,]
    # Define functions to calculate percentiles (feed in all simulated interspersion values from both distributions
    ecdf_fun.prog <- ecdf(prog_sim$dim_insp.pool[dat_sub[, ..year_pool_domains], on = (year_pool_domains), INSP])
    ecdf_fun.real <- ecdf(real_sim$dim_insp.pool[dat_sub[, ..year_pool_domains], on = (year_pool_domains), INSP])
    # Calculate the percentiles and send to output list
    out_lst[[i]] <- cbind(dat_sub, PERC_PROG = ecdf_fun.prog(dat_sub$INSP), PERC_REAL = ecdf_fun.real(dat_sub$INSP))
  }
  
  setorderv(rbindlist(out_lst), year_pool_domains)[]
  
}

calculate_density <- function(sim_res, fill_color, adjust = NULL) {
  # sim_res <- copy(sim.programmed.stratum); fill_color <- "dodgerblue"
  # sim_res <- copy(sim.programmed.stratum_fmp); fil_color <- "dodgerblue"; adjust = rep(2, times = 24)
  
  year_strata_domains <- attr(sim_res, "year_strata_domains")
  domain_tbl <- setorderv(unique(subset(sim_res, select = year_strata_domains)), year_strata_domains)
  tail_color <- paste0(fill_color, "4")
  
  # Adjust the bandwidth. Null will set the bandwidth to the default, 1
  if(is.null(adjust)) adj <- rep(1, times = nrow(domain_tbl)) else {
    if(length(adjust) == 1) adj <- rep(adjust, times = nrow(domain_tbl)) else {
      if(length(adjust) != nrow(domain_tbl)) stop("'adjust' needs to be the same as `nrow(domain_tbl)`")
      adj <- adjust
    }
  }
  
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
      #' *ORIGINAL* ggplot(dmn_subset, aes(x = INSP)) + geom_density(bounds = c(0, 1))
      
      ggplot(dmn_subset, aes(x = INSP)) + geom_density(bounds = c(0, 1), adjust = adj[i])

  
      
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
plot_interspersion_density <- function(den, real_interspersion, dmn_N, strata_levels){
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
  den[, STRATA := factor(STRATA, levels = strata_levels)]
  real_interspersion[, STRATA := factor(STRATA, levels = strata_levels)]
  dmn_N[, STRATA := factor(STRATA, levels = strata_levels)]
  
  ggplot(den, aes(x = X)) + 
    geom_hline(yintercept = 0, color = "gray") + 
    geom_ribbon(aes(ymin = 0, ymax = Y, fill = FILL, group = interaction(DIST, GROUP)), color = "black", outline.type = "full", alpha = 0.8) + 
    geom_vline(data = real_interspersion, aes(xintercept = INSP, linetype = "Actually Realized"), color = "purple", linewidth = 0.75) + 
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank() ) + 
    ylim(c(-1, 1)) + 
    labs(x = "Proximity Index", y = "Density") +  
    scale_fill_identity(
      name = "Monitoring Rate", guide = "legend",
      breaks = fill_breaks, labels = c('Realized', "Programmed")) +
    scale_linetype_manual(name = NULL, values = 2 ) +
    theme(legend.position = "bottom", legend.key = element_rect(fill = "white")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
    geom_text(data = dmn_N, aes(label = STRATA_DMN_N, x = -Inf, y = Inf), hjust = -0.1, vjust = 1.1, size = 3.5)
}


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
      coord_sf(xlim = stratum_sub.bbox[c(1, 3)], ylim = stratum_sub.bbox[c(2, 4)]) + 
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
      geom_sf(data = ak_low_res, fill = "gray80") +
      geom_sf(data = fmp_low_res, color = "black", fill = NA) +
      geom_sf(aes(fill = DIFF), alpha = 0.8) + 
      scale_fill_gradient2() + 
      coord_sf(xlim = stratum_sub.bbox[c(1, 3)], ylim = stratum_sub.bbox[c(2, 4)]) +
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
      geom_sf(data = ak_low_res, fill = "gray80") +
      geom_sf(data = fmp_low_res, color = "black", fill = NA) +
      geom_sf(aes(fill = DIFF), alpha = 0.8) + 
      scale_fill_gradient2() + 
      coord_sf(xlim = stratum_sub.bbox[c(1, 3)], ylim = stratum_sub.bbox[c(2, 4)]) +
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

plot_monitoring_spatial <- function(box_def, realized_mon, sim.real, strata_levels){
  # box_def <- copy(box_def.stratum); sim.real <- copy(sim.realized.stratum); sim.prog <- copy(sim.programmed.stratum);
  
  year_strata <- unname(unlist(box_def$params[c("year_col", "stratum_cols")]))
  
  hex_geom <- unique(select(box_def$geom_sf, HEX_ID) )
  hex_geom.bbox <- st_bbox(hex_geom)
  trips_dt <- unique(subset(box_def$og_data, select = c(year_strata, "HEX_ID", "TRIP_ID")))
  sim_iter <- length(attr(sim.realized.stratum, "mon_lst"))
  
  # Summarize count of trips in each HEX_ID for each stratum
  hex_trips_N <- trips_dt[, .(N = uniqueN(TRIP_ID)), keyby = c(year_strata, "HEX_ID")]
  
  # Count number of trips actually monitored in each HEX_ID
  hex_trips_n <- trips_dt[realized_mon[, .(TRIP_ID)], on = .(TRIP_ID)][, .(n = .N), keyby = c(year_strata, "HEX_ID")]
  hex_trips_n <- hex_trips_n[hex_trips_N, on = c(year_strata, "HEX_ID")]
  hex_trips_n[is.na(n), n := 0]
  
  # Using the selection simulations assuming the realized rate, count the number of times each HEX_ID was sampled in each iteration
  hex_trips_n.sim <- rbindlist(lapply(attr(sim.realized.stratum, "mon_lst"), function(x){
    # x <- test[[1]]
    trips_dt[data.table(TRIP_ID = x), on = .(TRIP_ID)]
  }), idcol = "ITER")
  # Count trips per HEX_ID for each iteration
  hex_trips_n.sim <- hex_trips_n.sim[, .(n_sim = .N), keyby = c("ITER", year_strata, "HEX_ID")]
  # Combine actual with simulations
  hex_trips_n_sim <- hex_trips_n.sim[hex_trips_n, on = c(year_strata, "HEX_ID")]
  
  # Calculate median trips monitored in simulations
  hex_trips_n_sim[
  ][, sim_MED := median(n_sim), by = c(year_strata, "HEX_ID")
  ][, DIR := sign(n - sim_MED)]
  # Calculate proportion of iterations where the actual value was more extreme
  hex_trips_n_sim.smry <- hex_trips_n_sim[, .(MORE_EXTREME = fcase(
    DIR == 0, 0L,
    DIR == 1, sum(n > n_sim),
    DIR == -1, sum(n < n_sim)
  )), by = c(year_strata, "HEX_ID", "N", "n", "sim_MED" ,"DIR")]
  # Flag instances where the actual value was on the 5% tail
  hex_trips_n_sim.smry[, TAIL := MORE_EXTREME >= (0.95 * sim_iter)]
  # Drop strata levels not in strata_levels
  hex_trips_n_sim.smry <- hex_trips_n_sim.smry[(STRATA %in% strata_levels)]
  # Set stratum levels
  hex_trips_n_sim.smry[, STRATA := factor(STRATA, levels = strata_levels)]
  # Create a facet category that combines year and stratum
  setorderv(hex_trips_n_sim.smry, year_strata)
  hex_trips_n_sim.smry[, FACET := paste(ADP, " : ", gsub("_", " ", STRATA))]
  hex_trips_n_sim.smry[, FACET := factor(FACET, levels = unique(FACET))]
  
  # Create label
  hex_trips_n_sim.smry[, label := ifelse(n - sim_MED != 0, (n - sim_MED), NA)] 
  
  # Set the range of the fill scale
  fill_range <- hex_trips_n_sim.smry[, range(n - sim_MED)]
  fill_range <- sign(fill_range) * (5 * ceiling(abs((fill_range ) / 5)))
  
  # Merge the geometry in
  hex_trips_n_sim.smry <- merge(hex_geom, hex_trips_n_sim.smry, on = "HEX_ID")
  
  # Split data by year
  map_years <- unique(trips_dt$ADP)
  map_years.list <- vector(mode = "list", length = length(map_years))
  
  for(i in seq_along(map_years)) {
    
    dat_sub <- hex_trips_n_sim.smry %>% filter(ADP == map_years[i]) 
    
    map_years.list[[i]] <- ggplot() + 
      facet_wrap(~ FACET, dir = "h", ncol = 2, drop = F) + 
      geom_sf(data = ak_low_res, fill = "gray80") + 
      geom_sf(data = fmp_low_res, fill  = NA, linetype = 2) + 
      geom_sf(data = dat_sub %>% filter(TAIL == F), aes(fill = DIR * (MORE_EXTREME / sim_iter))) + 
      geom_sf_text(data = dat_sub %>% filter(TAIL == F), aes(label = label), size = 3, na.rm = T) + 
      # Outline trips with unusual outcomes
      geom_sf(data = dat_sub %>% filter(TAIL == T), aes(fill = DIR * (MORE_EXTREME / sim_iter)), linewidth = 1, color = "dodgerblue") + 
      geom_sf_text(data = dat_sub %>% filter(TAIL == T), aes(label = label), size = 3, fontface = "bold") + 
      scale_fill_gradientn(colors = c("violet", "plum", "white", "white", "white", "palegreen", "green"), values = c(0, 0.1, 0.2, 0.5, 0.8, 0.9, 1), limits = c(-1, 1)) +
      theme(
        legend.position = "bottom", legend.key.width = unit(0.5, "in"), legend.frame = element_rect(color = "black"), legend.ticks = element_line(color = "black"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank() ) + 
      coord_sf(xlim = hex_geom.bbox[c(1, 3)], ylim = hex_geom.bbox[c(2, 4)]) + 
      labs(fill = "Proportion of simulated outcomes\nwhere actual value was more extreme") 
  }
  names(map_years.list) <- paste0("plt.spatial.", map_years)
  
  map_years.list
  
}

# For spatial analyses, summarize the coverage achieved in each HEX_ID
plot_spatial_coverage <- function(box_def, realized_mon, sim.real, sim.prog, strata_levels){
  # box_def <- copy(box_def.stratum);  sim.real <- copy(sim.realized.stratum); sim.prog <- copy(sim.programmed.stratum)
  
  #' [NOTE] *sim.real and sim.prog must have been created by simulate_interspersion with hex_smry = T*
  
  year_strata <- unname(unlist(box_def$params[c("year_col", "stratum_cols")]))
  year_strata_dt <- box_def$dmn$geom_dmn_df %>% st_drop_geometry() %>% select(all_of(year_strata)) %>% unique()
  
  # Extract the geometries of the HEX_IDs
  hex_id_geom <- box_def$geom_sf %>% select(HEX_ID, geometry) %>% unique()
  # Get the bounding box of the HEX_IDs
  hex_bbox <- hex_id_geom %>% st_bbox()
  
  #' *Create summary of realized monitoring by hex_id*
  
  # By HEX_ID, Total the weight of trips in each HEX_ID
  realized_mon.hex <- box_def$dt_out[, .(HEX_w_total = sum(BOX_w)), keyby = c(year_strata, "HEX_ID")]
  # Using `realized_mon`, identify the boxes that were monitored in each year x stratum
  realized_mon.box <- unique(copy(box_def$og_data)[TRIP_ID %in% realized_mon$TRIP_ID, .(ADP, STRATA, BOX_ID)])
  # Identify the BOX_IDs of monitored neighborhoods
  realized_mon.nbr <- rbindlist(lapply(
    split(realized_mon.box, by = c(year_strata)), 
    FUN = function(x) cbind(unique(x[, ..year_strata]), BOX_ID = unique(as.integer(unlist(box_def$nbr_lst[x$BOX_ID]))))
  ))
  realized_mon.nbr[, MON := T]
  # Total the weight of trips in monitored neighborhoods
  realized_mon.nbr <- realized_mon.nbr[
  ][box_def$dt_out, on = c(year_strata, "BOX_ID")
  ][, .(REAL_w = sum(BOX_w[MON], na.rm = T)), keyby = c(year_strata, "HEX_ID") ]
  realized_mon.hex <- realized_mon.hex[realized_mon.nbr]
  
  #' *Realized Rates Distribution*
  
  # Merge the realized monitoring with the simulations using the realized rates
  sim.real.perc <- realized_mon.hex[
  ][attr(sim.real, "hex_smry"), on = c(year_strata, "HEX_ID")]
  sim.real.perc[
    # Calculate the mean across simuation iterations
  ][, MED_w := median(HEX_w), keyby = c(year_strata, "HEX_ID")
    # Identify the directionality of the realized value relative to the median
  ][, DIR := sign(REAL_w - MED_w), keyby = c(year_strata, "HEX_ID")]
  # Sum up iterations where the realized value was more extreme
  sim.real.perc <- sim.real.perc[, .(MORE_EXTREME = fcase(
    DIR == 1, sum(REAL_w > HEX_w)/.N,
    DIR == -1, -sum(REAL_w < HEX_w)/.N,
    DIR == 0, 0
  )), keyby = c(year_strata, "HEX_ID", "HEX_w_total", "REAL_w", "DIR", "MED_w")]
  sim.real.perc[
    # Calculate the difference of the realized rate to the median of the simulations
  ][, MED_DIFF := REAL_w - MED_w
    # Merge STRATA_N in
  ][, STRATA_N := box_def$strata_n_dt[sim.real.perc, STRATA_N, on = c(year_strata)]
    # Flag results that were in the outer 5%
  ][, TAIL := abs(MORE_EXTREME) > 0.95
  ][, STRATA := factor(STRATA, levels = strata_levels)]
  # Merge the geometry in
  sim.real.perc <- merge(hex_id_geom, sim.real.perc, by = "HEX_ID")
  
  year <- unique(sim.real.perc$ADP)
  spatial <- ggplot(sim.real.perc) + 
    facet_nested_wrap(. ~ STRATA, ncol = 2, dir = "h", drop = F, labeller = labeller(STRATA = function(x) paste0(year, " : ", gsub("_", " ", x)))) + 
    geom_sf(data = ak_low_res, fill = "gray80") + 
    geom_sf(data = fmp_low_res, fill = NA, linetype = 2) + 
    geom_sf(aes(fill = MED_DIFF / HEX_w_total)) + 
    stat_sf_coordinates(data = sim.real.perc %>% filter(ADP == year & TAIL == T), shape = 21) + 
    scale_fill_gradient2(midpoint = 0, low = "purple", high = "green") +
    coord_sf(xlim = hex_bbox[c(1, 3)], ylim = hex_bbox[c(2, 4)]) +
    theme(
      legend.position = "bottom", legend.key.width = unit(0.5, "in"), legend.frame = element_rect(color = "black"), legend.ticks = element_line(color = "black"),
      axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
    labs(fill = "Difference in coverage\nrelative to expectation")
  
  
  list(
    coverage = spatial
  )
}


#' Simulate domain interspersion given monitoring rates and the relationships between strata.
#' This function would benefit from optimization, especially `calculate_realized_dmn_interspersion()`
simulate_dmn_interspersion <- function(box_def, sample_rates, ob_em_ze_adl, iter, seed) {
  # box_def <- copy(box_def.stratum_gear_fmp); sample_rates <- copy(programmed_rates); iter <- 1; seed <- 12345
  
  year_strata <- unlist(box_def$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- box_def$params$dmn_cols
  sim_lst <- vector(mode = "list", length = iter)
  set.seed(seed)
  
  # Merge the sample rates with the strata
  sample_rates <- sample_rates[, -"STRATA_N"][box_def$strata_n_dt, on = (year_strata)]
  
  for(i in seq_len(iter)){
    
    # Print the current iteration number every 10% of progress
    if(i %% round(iter/10) == 0) cat(paste0(i, ", "))
    
    # Sample according to sample rates, 
    sample_lst <- apply(setorderv(sample_rates, year_strata), 1, function(x) runif(x[["STRATA_N"]]) < x[["SAMPLE_RATE"]])
    # Make a data.table of trips
    trips <- setorderv(unique(subset(box_def$og_data, select = c(year_strata, "TRIP_ID"))), year_strata)
    #trips$SAMPLE <- unlist(sample_lst)
    
    sim_lst[[i]] <- calculate_realized_dmn_interspersion(box_def, trips[unlist(sample_lst)], ob_em_ze_adl)
  } 
  
  sim_dt <- rbindlist(sim_lst, idcol = "ITER")
  # sim_dt[, .(MEAN = mean(INSP)), keyby = year_strata] 
  # Capture the domains as an attribute
  
  # Add Pool so we can summarize by POOL and domain
  sim_dt[, POOL := fcase(
    STRATA %like% "EM", "EM",
    STRATA %like% "OB", "OB",
    STRATA == "ZERO", "ZERO"
  )]
  
  # Generate summaries by pool and domain (FMP and GEAR). 
  sim_dt.summary <- sim_dt[
  ][, .(
    dmn_N = sum(sum_BOX_DMN_w), 
    dmn_MAX_INSP = sum(max_INSP),
    dmn_INSP = sum(dmn_INSP)
  ), keyby = .(ITER, ADP, POOL, GEAR, BSAI_GOA)
  ]
  sim_dt.summary[, ':=' (INSP = dmn_INSP / dmn_N, MAX_INSP = dmn_MAX_INSP / dmn_N)]
  
  setattr(sim_dt, "year_pool_domains", c(box_def$params$year_col, "POOL", domains))
  setattr(sim_dt.summary, "year_pool_domains", c(box_def$params$year_col, "POOL", domains))
  
  list(
    dmn_insp.strata = sim_dt,
    dim_insp.pool  = sim_dt.summary
  )
}

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

# Same as calculate_density(), but for domains by pool
calculate_dmn_density <- function(sim_res, fill_color) {
  # sim_res <- copy(dmn_insp.prog); fill_color <- "dodgerblue"
  
  year_pool_domains  <- attr(sim_res$dim_insp.pool, "year_pool_domains")
  
  domain_tbl <- setorderv(unique(subset(sim_res$dim_insp.pool, select =  year_pool_domains)),  year_pool_domains)
  tail_color <- paste0(fill_color, "4")
  
  domain_density <- vector(mode = "list", length = nrow(domain_tbl))
  for(i in 1:nrow(domain_tbl)) {
    
    dmn_subset <- sim_res$dim_insp.pool[domain_tbl[i,], on =  year_pool_domains ]
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
    cutoffs <- dmn_density[, .SD[c(1, .N),], keyby = c(year_pool_domains, "quant", "FILL")]
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
    dmn_density <- rbind(quant_0, quant_1, quant_2)
    
    domain_density[[i]] <-  dmn_density
  }
  domain_density <- rbindlist(domain_density)
  domain_density[, GROUP := as.factor(GROUP)]
  
  setattr(domain_density, "year_pool_domains", year_pool_domains)
  domain_density[]
}

# same thing but for dmn interspersion. Make the function more general (see which attribute exists)
# Combine both datasets, scaling them so that y-axis is centered
combine_dmn_distributions <- function(realized, programmed) {
  # realized <- copy(realized.density); programmed <- copy(programmed.density)
  
  year_strata_domains <- attr(realized, "year_pool_domains")
  
  programmed_flipped <- copy(programmed)
  programmed_flipped[, Y := -Y]
  dist_dat <- rbind(cbind(DIST = "REAL", realized), cbind(DIST = "PROG", programmed_flipped))
  
  dist_dat[, MAX_ABS := max(abs(Y)), keyby = year_strata_domains]
  dist_dat[, Y := Y/MAX_ABS]
  setattr(dist_dat, "year_pool_domains", year_strata_domains)
  dist_dat
}

# Generate summary plots of interspersion density. Use facet_ functions to separate year and strata and FMP if desired.
plot_dmn_interspersion_density <- function(den, real_interspersion, dmn_N){
  # den <- copy(dmn_insp.density.combined); real_interspersion <- copy(realized_dmn_interspersion.summary); dmn_N <- copy(dmn_intperspersion.dmn_N )
  
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
    geom_text(data = dmn_N, aes(label = dmn_N, x = -Inf, y = Inf), hjust = -0.1, vjust = 1.1, size = 3.5)
}

