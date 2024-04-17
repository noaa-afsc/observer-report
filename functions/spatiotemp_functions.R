# These are the functions from analyses/allocation_evaluation
# Some are further developed version of those in analyses/spatiotemporal_boxes

library(data.table)   # For data wrangling, very efficient with joins, especially rolling joins
library(dplyr)        # For data wrangling/piping with sf package
library(sf)           # For spatial statistics

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
stat_area_to_hex <- function(cell_size, stat_area_sf){
  
  # 'cell_size' should be specified as a distance in meters. For hex cells, this is the distance between opposite sides.
  # 'stat_area_sf' is the shapefile of the ADFG stat areas in Alaska Albers projection (3467), with each statistical 
  # area and its polygon geometry. For example:
  # stat_area_sf <- st_read("source_data/ADFG_Stat_Area_shapefile/PVG_Statewide_2001_Present_GCS_WGS1984.shp", quiet = T) %>%
  #   select(STAT_AREA) %>%
  #   st_transform(crs = 3467)
  
  # Get centroids of statistical areas
  stat_area_centroid_sf <- suppressWarnings(st_centroid(stat_area_sf))  
  
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
define_boxes <- function(data, space, time, year_col, stratum_cols, dmn_lst = NULL, stata_area_sf = stat_area_sf, geom = F, ps_cols = NULL) {
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
  
  stat_area_lst <- stat_area_to_hex(space[1], stat_area_sf)
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

  dates_start <- min(dates_mtx[, 1]) - 1  # Get first date and subtract 1. T
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
  # For each TRIP_ID x HEX_ID, identify unique weeks
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

  # Make the frequency table of each TRIP_ID (so that trips are properly split by HEX_ID, TIME, and if present, ps_cols
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
          
          trip_id_centered <- x[x[,1] == y, 2]    # Identify number of trips actually  the box
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
  if( !is.null(dmn_lst) ) {
    
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
            
            # Identify all trips in the domain x stratum in this box
            trip_id_centered <- unique(z[z[, "BOX_ID"] == z1, "TRIP_ID"])
            c(
              BOX_ID = z1,
              BOX_DMN_n = length(trip_id_centered),
              BOX_DMN_w = sum(1 / trip_id_dmn_vec[trip_id_centered]),
              # count number of neighboring trips in the nst_dmn X stratum ()
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
    
    # Calculate Number of trips in each STRATA x dmn_cols. Note that trips that have multiple 
    # 'dmn_cols' were split here!
    strata_dmn_N_dt <- dmn_nbr_dt[, .(STRATA_DMN_N = sum(BOX_DMN_w)), by = c(year_col, stratum_cols, dmn_cols)]
    
    # Double-check that weights sum to STRATA_N
    if(!fsetequal(
      strata_dmn_N_dt[, .(STRATA_N = sum(STRATA_DMN_N)), keyby = c(year_col, stratum_cols)][STRATA_N != 0],
      strata_N_dt
    )) stop("STRATA_N and sum(BOX_DMN_w) are not equal!")
    
    box_res$dmn <- list()
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
    # i <- 1

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


#' TODO *CLEANUP*
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


# A wrapper for calculate_realized_interspersion, but randomly samples to create new `monitored_trips` object
simulate_interspersion <- function(box_def, sample_rates, iter, seed) {
  # box_def <- copy(box_def.stratum); sample_rates <- copy(programmed_rates); iter <- 1; seed <- 12345
  
  year_strata <- unlist(box_def$params[c("year_col", "stratum_cols")], use.names = F)
  domains <- box_def$params$dmn_cols
  sim_lst <- vector(mode = "list", length = iter)
  set.seed(seed)
  
  #' TODO Make sure `sample rates` and `trips` are ordered the same way
  
  for(i in seq_len(iter)){
    
    if(i %% 100 == 0) cat(paste0(i, ", "))
    
    # Sample according to sample rates, 
    sample_lst <- apply(setorderv(sample_rates, year_strata), 1, function(x) runif(x[["STRATA_N"]]) < x[["SAMPLE_RATE"]])
    # Make a data.table of trips
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
