# For 2020 Annual Report, will only use this script to map fishing effort and monitoring over time and space.
# Will not be used for gap analyses due to complications from COVID-19, waivers, short time period for random deployment, etc.

if(!require(data.table))  install.packages(data.table)         # For data wrangling
if(!require(ggplot2))     install.packages(ggplot2)            # For plotting
if(!require(ggpubr))      install.packages(ggpubr)             # For combining plots that use the same legend
if(!require(lubridate))   install.packages(lubridate)          # For wrangling dates
      
GAP_ANALYSIS    <- F        # Evaluate achieved gap indices with distribution of gap indices that would be acquire under perfectly random deployment
TIME_SPACE      <- T        # Visualize fishing effort and monitored trips in time and space. Allows within-gear comparisons between pools.

#==============#
# DATA PREP ####

load("2_AR_data.Rdata")     # Load source data. work.data contains data for relevant year for Annual Report
partial_effort <- unique(setDT(copy(work.data))[COVERAGE_TYPE=="PARTIAL" & STRATA != "Zero EM Research", .(ADP, TRIP_ID, PERMIT, STRATA, COVERAGE_TYPE, AGENCY_GEAR_CODE, TRIP_TARGET_DATE, LANDING_DATE, TRIP_TARGET_CODE, AREA=REPORTING_AREA_CODE, FMP, OBSERVED_FLAG)])
rm(list=setdiff(ls(), c("partial", "work.data", "partial_effort", "GAP_ANALYSIS", "TIME_SPACE")))             # Remove unneeded objects
setnames(partial_effort, c("TRIP_TARGET_CODE", "AGENCY_GEAR_CODE"), c("TARGET", "GEAR"))   # Simplify long column names

partial_effort[, POOL := fifelse(STRATA %like% "EM", "EM", fifelse(STRATA=="ZERO", "ZE", "OB"))]   # Define pool (OB, ZE, or EM)
partial_effort[, c("START", "END") := as.list(range(as.Date(TRIP_TARGET_DATE), as.Date(LANDING_DATE))), by=TRIP_ID]   # Unify trip start/end dates by TRIP_ID
partial_effort[, GEAR := fifelse(GEAR %in% c("PTR", "NPT"), "TRW", GEAR)]          # Define Gear, removing distinction between NPT and PTR
partial_effort[, c("COVERAGE_TYPE", "TRIP_TARGET_DATE", "LANDING_DATE") := NULL]   # Remove unneeded columns
partial_effort[TARGET=="B", TARGET := "P"]                                         # Remove distinction between 'bottom pollock' and 'pelagic pollock'
partial_effort <- unique(partial_effort[, .(PERMIT, TRIP_ID, POOL, STRATA, TARGET, FMP, AREA, GEAR, START, END, OBSERVED = OBSERVED_FLAG)])

# Determine realized trip selection rates. 
rr <- unique(partial_effort[, .(TRIP_ID, POOL, STRATA, SEL=OBSERVED=="Y")])[, .(N = .N, SEL=sum(SEL), RATE = sum(SEL)/.N), keyby=.(POOL, STRATA)]

### For 2020 Only #
# Due to COVID, there was a period between Mar 25 and July 1 where a general waiver of partial coverate was in place.
# After waivers were lifted, prorammed rates were increased for fixed gear observer pool strata to account for trips excluded from sample frame (ports)

# Before waivers were lifted
unique(partial_effort[START <= as.POSIXct("2020-03-25"), .(TRIP_ID, POOL, STRATA, SEL=OBSERVED=="Y")])[, .(N = .N, SEL=sum(SEL), RATE = sum(SEL)/.N), keyby=.(POOL, STRATA)]
# Looking at rates only after waivers were lifted. OB strata were 4-5% below target of 19.6 / 15.4 / 15.2 for TRW/HAL/POT
unique(partial_effort[START >= as.POSIXct("2020-07-01"), .(TRIP_ID, POOL, STRATA, SEL=OBSERVED=="Y")])[, .(N = .N, SEL=sum(SEL), RATE = sum(SEL)/.N), keyby=.(POOL, STRATA)]
# When we had no waivers (start and end of year)
unique(partial_effort[START <= as.POSIXct("2020-03-25") | START >= as.POSIXct("2020-07-01"), .(TRIP_ID, POOL, STRATA, SEL=OBSERVED=="Y")])[, .(N = .N, SEL=sum(SEL), RATE = sum(SEL)/.N), keyby=.(POOL, STRATA)]

#==============================================#
# TIMESPACE PLOTS OF EFFORT  AND MONITORING ####

if(TIME_SPACE == TRUE){
  
  # 2020 Only - Specify when waivers were put in place and then lifted
  waiver_week <-  week(c("2020-03-26", "2020-07-01"))
  
  ts_fun_final <- function(ar_data, effort, monitored, legend=TRUE, obscure=TRUE, n_min=5, ob_extend=0, output_tbl=FALSE, vline=NULL){
    # These plots where fishing effort occured and where/when monitoring data was collected.
    # Allows comparisons within gear types between pools.
    #   'data' identifies POOL that is a data source. Can be a vector
    #   'effort' identifies POOL to show fishing effort. Can be a vector.
    #   'vline' allows you to create vertical lines at a specified week. 
    
    # Function to count number of trips within weekly bins.
    to_week <- function(x, yr=ar_year){
      x[year(START) < yr, START := as.Date(paste0(yr, "-01-01"))]  # If any incoming date ranges are outside of the year, truncate them to be within specified year
      x[year(END) > yr, END := as.Date(paste0(yr, "-12-31"))]
      cols <- unique(x[, c("POOL", "GEAR", "TARGET_lbl", "FMP", "AREA")])
      x <- setnames(x[, apply(.SD, MARGIN=1, function(y) seq.int(week(y[1]), week(y[2]))), by=.(TRIP_ID, AREA), .SDcols=c("START", "END")], c("TRIP_ID", "AREA", "WEEK"))
      x <- x[, .N, keyby=WEEK][SJ(1:week(as.Date(paste0(yr, "-12-31"))))][is.na(N), N := 0][, WEEK := as.numeric(WEEK)]
      x <- cbind(x, cols)  # Put identifier columns back in
      return(x)
    }
    
    # TODO - Make this an object we can make once and save for use forever?
    # Pretty names for targets
    target_tbl <- data.table(
      TARGET = c("A", "C", "D", "E", "F", "H", "I", "K", "L", "M", "O", "P", "R", "S", "T", "W", "X", "Y"),
      TARGET_lbl = c("Atka Mackerel", "Pacific Cod", "Deep Water Flatfish", "Alaska Plaice", "Other Flatfish", "Shallow Water Flatfish", "Halibut", "Rockfish", "Flathead Sole", "Kamchatka Flounder", "Other Species", "Pollock", "Rock Sole", "Sablefish", "Greenland Turbot", "Arrowtooth Flounder", "Rex Sole", "Yellowfin Sole"))
    
    effort_monitored = list(EFFORT = effort, MONITORED = monitored)
    # Set levels of pools (uses the same order as entered in the function)
    pool_levels <-  union(effort_monitored$EFFORT, effort_monitored$MONITORED)
    # Make all combinations of effort_monitored - This table specifies which data is used for which facets (e.g. copies monitored data to be placed over effort for different pools)
    effort_monitored_tbl <- setnames(CJ(effort_monitored$EFFORT, effort_monitored$MONITORED), c("EFFORT", "MONITORED"))
    effort_monitored_tbl[, ':=' (EFFORT = factor(EFFORT, levels=pool_levels), MONITORED = factor(MONITORED, levels=pool_levels))]
    setkey(effort_monitored_tbl, EFFORT, MONITORED)
    effort_monitored_tbl[, GRP_lbl := paste(EFFORT, "effort x", MONITORED, "data")]
    effort_monitored_tbl[, GRP_lbl := factor(GRP_lbl, levels=effort_monitored_tbl$GRP_lbl)]    # Set order of GRP_lbl for facets in plots
    
    # Prep the data
    ar_data[, AREA := as.factor(AREA)]                                # Ensure area is a factor
    ar_data[, TARGET_lbl := target_tbl[ar_data, TARGET_lbl, on=.(TARGET)]]  # Merge in target lbl
    ar_data <- ar_data[POOL %in% unique(unlist(effort_monitored))]          # Remove any pools not included in EFFORT or MONITORED
    ar_year <- ar_data[, .N, keyby=.(year(START))][N==max(N), year]               # Identify year from ar_data
    
    # Like the gap analysis, remove domains with < 5 trips
    ar_data_nmin <- ar_data[, .N, keyby=.(GEAR, TARGET, FMP)][N>=n_min]  # Filter out domains with < n_min trips (counting by trip_id x AREA, not uniqueN)
    ar_data <- ar_data[ar_data_nmin, on=.(GEAR, TARGET, FMP)]
    plot_width <- unique(ar_data[, .(GEAR, TARGET_lbl), by=FMP])[, .N, by=FMP]   
    # If obscuring domain X areas with fewer than 3 vessels, separate from full dataset to be drawn will a single tile spanning year instead of week.
    if(obscure==TRUE){
      # Obscure trips here to maintain confidentiality
      # First, remove any domains with <5 trips. Then If any areas have <3 distinct vessels, remove trips but add a gray label that instead gives percentage of trips monitored
      ar_data_remv <- ar_data[, .(VES_N = uniqueN(PERMIT), TRP_N=uniqueN(TRIP_ID), P_OBSERVED=sum(OBSERVED=="Y")/.N), keyby=.(POOL, GEAR, TARGET_lbl, FMP, AREA)][VES_N<3]  # these areas will be replaced with gray bar
      ar_data_keep <- ar_data[, .(VES_N = uniqueN(PERMIT), TRP_N=uniqueN(TRIP_ID)), keyby=.(POOL, GEAR, TARGET_lbl, FMP, AREA)][VES_N>=3]
      ar_data <- ar_data[ar_data_keep, on=.(POOL, GEAR, TARGET_lbl, FMP, AREA)]
      
      # Prepare tiles for confidential data
      rect_effort <- copy(ar_data_remv)
      rect_effort[, COLOR := I("red")]
      rect_monitored <- rect_effort[POOL %in% effort_monitored$MONITORED & P_OBSERVED >0 ]
      rect_monitored[, COLOR := I("blue")]
      
      # For domains where P_OBSERVED > 0, first check to see if POOL is in effort_monitored$MONITORED. If so, copy that tile and make it fill = "blue"
      ar_combined_rect <- data.table()
      for(l in 1:nrow(effort_monitored_tbl)){
        ar_combo_rect <- rbind(
          cbind(rect_effort[POOL==effort_monitored_tbl[l, EFFORT]], effort_monitored_tbl[l, .(GRP_lbl)]),
          cbind(rect_monitored[POOL==effort_monitored_tbl[l, MONITORED]], effort_monitored_tbl[l, .(GRP_lbl)]))[!is.na(POOL)]
        ar_combined_rect <- rbind(ar_combined_rect, ar_combo_rect)
      }
      ar_combined_rect[order(GRP_lbl, POOL, TARGET_lbl, FMP, AREA)]
      ar_combined_rect[, split_char := nchar(sapply(strsplit(as.character(GRP_lbl), split='x'), '[[', 2))]
      ar_combined_rect[, DATA_POOL := substr(GRP_lbl, start=nchar(as.character(GRP_lbl)) - split_char+2, stop=nchar(as.character(GRP_lbl))-5)]
      ar_combined_rect[, split_char := NULL]
      ar_combined_rect[POOL != DATA_POOL, P_OBSERVED := NA_real_]  # If the effort pool (POOL) matches DATA_POOL, then keep P_OBSERVED. Otherwise, force to NA
      ar_combined_rect[COLOR=="blue", P_OBSERVED := NA_real_]      # For monitored trips, removed P_OBSERVED, as we'll only use P_OBSERVED from effort (red) trips if relevant
    }
    
    # Split trips into 'effort' and 'monitored' and convert into weekly bin counts.
    ar_effort <- copy(ar_data[POOL %in% effort_monitored$EFFORT])               # Subset fishing effort
    ar_effort <- split(ar_effort, by=c("FMP", "POOL", "GEAR", "TARGET", "AREA"), drop=T)   # Split into separate elements
    ar_effort <- rbindlist(lapply(ar_effort, to_week))                          # Convert to weekly bin counts
    ar_effort[, GRP := "Effort"]
    
    ar_monitored <- copy(ar_data[POOL %in% effort_monitored$MONITORED & OBSERVED=="Y"])[, ':=' (START=START-ob_extend, END=END+ob_extend)]  # Subset data, and if specified, extend date ranges
    ar_monitored <- split(ar_monitored, by=c("FMP", "POOL", "GEAR", "TARGET", "AREA"), drop=T)
    ar_monitored <- rbindlist(lapply(ar_monitored, to_week))
    ar_monitored[, GRP := "Monitoring Data"]
    
    # For each effort x monitored combination specified in effort_monitored_tbl, combine effort and monitored into a single table.
    ar_combined <- data.table()
    for(l in 1:nrow(effort_monitored_tbl)){
      ar_combo <- rbind(
        cbind(ar_effort[POOL==effort_monitored_tbl[l, EFFORT]], effort_monitored_tbl[l, .(GRP_lbl)]),
        cbind(ar_monitored[POOL==effort_monitored_tbl[l, MONITORED]], effort_monitored_tbl[l, .(GRP_lbl)]))[!is.na(POOL)]
      ar_combined <- rbind(ar_combined, ar_combo)
    }
    ar_combined[, N := as.numeric(N)]                                           # Convert from integer to numeric
    ar_combined[, N := N/max(N), by=.(POOL, GEAR, GRP, FMP)]                    # Scale counts by the maximum within each GRP_lbl (facet row) and GRP (data or effort)        
    ar_combined[, FMP := factor(FMP, levels=c("BSAI", "GOA"))]                  # Set consistent order for FMPs
    setorder(ar_combined, FMP)
    ar_combined[, TARGET_lbl := as.factor(TARGET_lbl)]                          # Order target alphabetically
    ar_combined[, POOL := factor(POOL, levels=pool_levels)]                     # Order the pools in the same way as the gap plots
    ar_combined[, GRP := factor(GRP, levels=c("Effort", "Monitoring Data"))]    # Order the effort/monitoring groups
    
    # Prepare plots
    p_lst <- vector(mode="list")
    x_maj <- c(week(as.Date(paste(ar_year, seq(1,12,3), "01", sep="-"))), week(as.Date(paste0(ar_year, "-12-31"))) + 1)  # Tick marks for each week, plus one week
    x_min <- week(as.Date(paste(ar_year, 1:12, "01", sep="-")))
    
    # Build plots by FMP
    for(i in unique(ar_combined$FMP)){
      # TEST:      i <- "BSAI";       i <- "GOA"
      plot_data <- ar_combined[FMP==i & N!=0]       # Only grab all weeks in FMP with at least one trip
      
      if(obscure==FALSE){
        levs <- unique(plot_data$AREA); levs <- levs[order(levs)]    # Exclude unused levels for AREA factor
        plot_data[, AREA := factor(AREA, levels=levs)]
        f <- data.table()
      } else {
        f <- ar_combined_rect[FMP==i]
        levs <- union(unique(plot_data$AREA), unique(f$AREA)); levs <- levs[order(levs)]    # Use this to ensure the order of AREAS on y-axis is correct and no areas are dropped
        plot_data[, AREA := factor(AREA, levels=levs)]
        f[, AREA := factor(AREA, levels=levs)]
        f[, Y_NUDGE := ifelse(COLOR=="red", 1/8, -1/8)]
      }
      
      # Create plot
      p <- ggplot() 
      if(!is.null(vline)) p <- p + geom_vline(xintercept=vline, linetype=2)       # If specified, add vertical lines
      p <- p + 
        facet_grid(GRP_lbl~FMP+GEAR+TARGET_lbl, drop=TRUE) +                      # drop=TRUE will drop targets that don't appear in both BSAI/GOA
        geom_tile(data=plot_data[GRP=="Effort"], aes(x=WEEK, y=AREA, fill=GRP, alpha=N), height=0.5, position=position_nudge(y=1/8)) + 
        geom_tile(data=plot_data[GRP=="Monitoring Data"], aes(x=WEEK, y=AREA, fill=GRP, alpha=N), height=0.5, position=position_nudge(y=-1/8)) + 
        scale_fill_manual(values=c("red", "blue")) +
        scale_y_discrete(limits=levs) + 
        scale_alpha_continuous(name="Relative Concentration", range=c(0.15,0.75)) +  
        scale_x_continuous(limits=c(1, 54), breaks=x_maj, labels=c("Jan", "Apr", "Jul", "Oct", "Jan"), minor_breaks = x_min) + 
        labs(x="Month", y="Area", fill="") + 
        # TODO TESTING THIS BELOW
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), strip.text=element_text(margin=margin(0.05, 0.05, 0.05, 0.05,"cm")), legend.position="bottom") 
        
      # If there are obscured tiles, add them on now
      if(obscure==TRUE & nrow(f) >0 ){
        p <- p + 
          geom_tile(data=f, aes(x=mean(c(1,54)), y=as.numeric(AREA)+Y_NUDGE, color=COLOR), width=52, height=0.5, alpha=0.5, fill="white") +
          geom_text(data=f, aes(x=mean(c(1,54)), y=AREA, label=ifelse(is.na(P_OBSERVED), "", formatC(P_OBSERVED, digits=2, format="f"))), position=position_nudge(y=3/16), size=2.5) 
      }
      p_lst[[i]] <- p
    }
    
    # Make final plot object
    if(legend==T) {
      fin_plot <- ggarrange(plotlist=p_lst, nrow=1, common.legend = T, legend="bottom", widths=plot_width$N/sum(plot_width$N))
    } else {
      fin_plot <- ggarrange(plotlist=p_lst, nrow=1, legend="none", widths=plot_width$N/sum(plot_width$N))
    }
    
    # Package outputs. If output_tbl is TRUE, then also return the final table used to generate figures.
    if(output_tbl == T) {
      return(list(PLOT = fin_plot, DATA = ar_combined))
    } else{
      return(fin_plot)
    }
  } # ts_fun_final END
  
  # Confidential versions for analyst use
  plt_hal_con <- ts_fun_final(partial_effort[GEAR=="HAL"], effort=c("OB", "ZE"), monitored="OB", vline=waiver_week, obscure=F)        # Exclude BSAI HAL Sablefish? (2 trips)
  plt_pot_con <- ts_fun_final(partial_effort[GEAR=="POT"], effort=c("OB", "ZE"), monitored="OB", vline=waiver_week, obscure=F)        # Exclude GOA POT HAlibut? 
  plt_trw_con <- ts_fun_final(partial_effort[GEAR=="TRW"], effort=c("OB"), monitored="OB", vline=waiver_week, obscure=F)
  
  plt_em_hal_con <- ts_fun_final(partial_effort[GEAR=="HAL"], effort=c("EM"), monitored=c("EM", "OB"), vline=waiver_week, obscure=F)
  plt_em_pot_con <- ts_fun_final(partial_effort[GEAR=="POT"], effort=c("EM"), monitored=c("EM", "OB"), vline=waiver_week, obscure=F)
  
  # Obscured versions for public use
  plt_hal <- ts_fun_final(partial_effort[GEAR=="HAL"], effort=c("OB", "ZE"), monitored="OB", vline=waiver_week)      # Fig B3 in 2019 AR
  plt_pot <- ts_fun_final(partial_effort[GEAR=="POT"], effort=c("OB", "ZE"), monitored="OB", vline=waiver_week)      # Fig B4 in 2019 AR
  plt_trw <- ts_fun_final(partial_effort[GEAR=="TRW"], effort=c("OB"), monitored="OB", vline=waiver_week)            # Fig B5 in 2019 AR
  
  plt_em_hal <- ts_fun_final(partial_effort[GEAR=="HAL"], effort=c("EM"), monitored=c("EM", "OB"), vline=waiver_week)   # FIG B6 in 2019 AR
  plt_em_pot <- ts_fun_final(partial_effort[GEAR=="POT"], effort=c("EM"), monitored=c("EM", "OB"), vline=waiver_week)   # Fig B7 in 2019 AR
  
  # Save plots to a single pdf
  plot_list <- list(plt_hal_con, plt_hal, plt_pot_con, plt_pot, plt_trw_con, plt_trw, plt_em_hal_con, plt_em_hal, plt_em_pot_con, plt_em_pot)
  pdf("figures/time_space_plots.pdf", onefile=T, width=11, height=8.5)
  for(i in seq(length(plot_list))){
    print(plot_list[[i]])
    }
  dev.off()
  
} # TIMESPACE PLOTS END

# NOTE: in 2020, of the trips that targetting Halibut and used POT GEAR, most used both HAL and POT gear. However, 7 Trips only used pot gear. 
unique(partial_effort[TRIP_ID %in% partial_effort[GEAR=="POT" & FMP=="GOA" & POOL=="OB" & TARGET=="I", TRIP_ID]][, .(TRIP_ID, STRATA, GEAR, TARGET)])[, uniqueN(GEAR), keyby=TRIP_ID]
unique(partial_effort[TRIP_ID %in% partial_effort[GEAR=="POT" & FMP=="GOA" & POOL=="OB" & TARGET=="I", TRIP_ID]][, .(TRIP_ID, STRATA, GEAR, TARGET)])[, uniqueN(GEAR), keyby=TRIP_ID][V1==1]

# Can also plot all OB monitoring data against all three pools
ts_fun_final(partial_effort[GEAR=="HAL"], effort=c("OB", "EM", "ZE"), monitored="OB", vline=waiver_week, obscure=F) 
ts_fun_final(partial_effort[GEAR=="POT"], effort=c("OB", "EM", "ZE"), monitored="OB", vline=waiver_week, obscure=F) 

#=================#
# GAP ANALYSIS ####

# TODO - The gap analysis code was last revised for 2019 Annual Report. This section will not be used for 2020 Annual Report
# The code largely needs updating to run on more recent datasets, and will not run as-is currently. It was left here for use in the future.

if(GAP_ANALYSIS == TRUE){
  iter <- 10000    # Number of odds iterations to simulate
  set.seed(12345)  # Setting seed number here for ODDS sampling

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

} # GAP_ANALYSIS END