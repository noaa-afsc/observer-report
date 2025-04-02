
# Annual Report Enforcement chapter: Table generation --------------------------
# Contact Andy Kingham
# 206-526-4212

library(reshape2)
library(tidyverse)
library(patchwork)
library(FMAtools)

# User inputs ------------------------------------------------------------------------------------------------------------
# Set the .Rdata file we will load
file_3_name <- "AR_3_rate_output.Rdata"

# Factor order for plotting
fact_order <- c("DEPLOY", "DAYS", "TRIP", "OFFLOAD", "HAUL", "SAMPLE", "MAR MAM")

start_color   <- "navy"
end_color     <- "red3"

# Rate
rate_x <- 100

# Load data ------------------------------------------------------------------------------------------------------------
# Assign the address of the Annual Report Project in the Shared Gdrive
AnnRpt_EnfChp_dribble <- gdrive_set_dribble("Projects/Annual Report OLE chapter 5/2024_data")

# Pull Rdata file from google drive. This will update your local if the GDrive version is newer,
#  otherwise it will load your local
gdrive_download(file_3_name, AnnRpt_EnfChp_dribble)

load(file = file_3_name)
# Data manipulation ----------------------------------------------------------------------------------------------------

#TODO - Placeholder until Andy is done in AR_2 & AR_3, then make sure this is in there
subcat_units_rate_priority <-
  subcat_units_rate_priority %>%
  # filter(SUBCATEGORY %in% c("ACCESS", "ASSAULT", "BIN MONITORING",
  #                           "DESTRUCTION OF SAMPLE/WORK/PERSONAL EFFECTS",
  #                           "FOOD AND ACCOMMODATIONS", "FORCED TO PERFORM CREW DUTIES",
  #                           "HOSTILE WORK ENVIRONMENT", "IMPEDIMENT",
  #                           "INTIMIDATION/BRIBERY/COERCION", "MARINE CASUALTY",
  #                           "NOTIFICATION", "OBSERVER SAMPLING STATION",
  #                           "REASONABLE ASSISTANCE", "SAFETY",
  #                           "SAMPLING INTERFERENCE", "SCALES", "SEXUAL ASSAULT",
  #                           "SEXUAL HARASSMENT", "VIDEO MONITORING SYSTEM",
  #                           "VMS REQUIREMENTS")) %>%
   mutate(CAT_COMBO = ifelse(str_detect(CATEGORY, "USCG|SAFETY"),
                            "SAFETY / WORK ENVIRONMENT", CATEGORY))

# Priority subcategories
subcat_priority <- 
  subcat_units_rate_priority %>%
  filter(CALENDAR_YEAR == adp_yr) %>%
  mutate(!! paste0("RATE_X_", rate_x) := RATE * rate_x,
         # Create dummy column for color scale
         #!! paste0("RATE_X_", rate_x, "_plot") := pmin(RATE, threshold_priority/rate_x) * rate_x,
         INCIDENT_UNIT = case_when(INCIDENT_UNIT == "OFFL" ~ "OFFLOAD",
                                   INCIDENT_UNIT == "SAMP" ~ "SAMPLE",
                                   INCIDENT_UNIT == "DEPL" ~ "DEPLOY",
                                   INCIDENT_UNIT == "MARM" ~ "MAR MAM",
                                   TRUE ~ INCIDENT_UNIT),
         CAT_COMBO = ifelse(str_detect(CATEGORY, "GEAR"), "GEAR / EQUIPMENT REQUIREMENTS",
                            CAT_COMBO)) %>%
  select(CAT_COMBO, SUBCATEGORY, INCIDENT_UNIT, RATE, paste0("RATE_X_", rate_x)#,
        # paste0("RATE_X_", rate_x, "_plot")
        )

# Non-priority subcategories
subcat_other <- 
  subcat_units_rate %>%
  filter(CALENDAR_YEAR == adp_yr,
         !(SUBCATEGORY %in% subcat_priority$SUBCATEGORY)) %>%
  mutate(CAT_COMBO = case_when(str_detect(CATEGORY, "SAFETY-USCG|MARPOL") ~
                                 "SAFETY-USCG / MARPOL / OIL SPILL",
                               str_detect(CATEGORY, "PERMITS/DOCUMENTS|OPERATIONAL|GEAR") ~
                                 "GEAR / EQUIPMENT / OPERATIONAL REQUIREMENTS / PERMITS / DOCUMENTS / RECORD KEEPING AND REPORTING",
                               str_detect(CATEGORY, "SUSTAINABLE|SPECIES") ~
                                 "PROHIBITED SPECIES / MARINE MAMMALS / SEABIRDS / SUSTAINABLE FISHERIES",
                               TRUE ~ CATEGORY),
         INCIDENT_UNIT = case_when(INCIDENT_UNIT == "OFFL" ~ "OFFLOAD",
                                   INCIDENT_UNIT == "SAMP" ~ "SAMPLE",
                                   INCIDENT_UNIT == "DEPL" ~ "DEPLOY",
                                   INCIDENT_UNIT == "MARM" ~ "MAR MAM",
                                   TRUE ~ INCIDENT_UNIT),
         !! paste0("RATE_X_", rate_x) := RATE * rate_x,
         # Create dummy column for color scale
       #  !! paste0("RATE_X_", rate_x, "_plot") := pmin(RATE, threshold_other/rate_x) * rate_x
       ) %>%
  select(CAT_COMBO, SUBCATEGORY, INCIDENT_UNIT, RATE, paste0("RATE_X_", rate_x)#,
     #    paste0("RATE_X_", rate_x, "_plot")
     )

# Start Christian Tables -----------------------------------------------------------------------------------------------

#library(flextable)
#library(officer)

# Summary of total observer statements per reporting category with total numbers
#  of various factors for comparisons of total effort

# Total observed for each unit type
T_summary_units <- 
  subcat_units_rate %>%
  filter(CALENDAR_YEAR == adp_yr) %>%
  distinct(INCIDENT_UNIT, TOTAL_UNITS) %>%
  mutate(INCIDENT_UNIT = fct_recode(factor(INCIDENT_UNIT),
                                    "Deployments" = "DEPL",
                                    "Hauls" = "HAUL",
                                    "Days" = "DAYS",
                                    "Trips" = "TRIP",
                                    "Offloads" = "OFFL",
                                    "Samples" = "SAMP",
                                    "Marine Mammal Interactions" = "MARM")) %>%
  # Number of unique vessels/plants that were observed
  bind_rows(assignments_dates_cr_perm %>%
              filter(CALENDAR_YEAR == adp_yr) %>%
              group_by(VESSEL_OR_PLANT) %>%
              summarize(TOTAL_UNITS = n_distinct(PERMIT)) %>%
              ungroup() %>%
              mutate(VESSEL_OR_PLANT = ifelse(VESSEL_OR_PLANT == "V", "Vessels", "Plants")) %>%
              rename(INCIDENT_UNIT = VESSEL_OR_PLANT)
            ) %>% 
  arrange(desc(TOTAL_UNITS)) %>%
  rename("Incident Unit" = INCIDENT_UNIT,
         "Total Units" = TOTAL_UNITS)

#Total number of Statements 
T_statement_totals <-
rbind(subcat_units_rate %>%
               filter(CALENDAR_YEAR == adp_yr) %>%
               group_by(CATEGORY) %>%
               summarize(`Statements (#)` = sum(N_STATEMENTS)) %>% 
        arrange(desc(`Statements (#)`)),
      merge(data.frame(CATEGORY = "TOTAL"),
            subcat_units_rate %>%
        filter(CALENDAR_YEAR == adp_yr) %>%
        summarize(`Statements (#)` = sum(N_STATEMENTS))
        )
      ) %>%
  mutate(CATEGORY = str_to_title(CATEGORY),
         CATEGORY = case_when(str_detect(CATEGORY, "Uscg-Equipment") ~
                                "Safety-USCG: Equipment",
                              str_detect(CATEGORY, "Uscg-Fail") ~
                                "Safety-USCG: Fail to Conduct Drills and/or Safety Orientation",
                              str_detect(CATEGORY, "Uscg-Marine") ~
                                "Safety-USCG: Marine Casualty",
                              str_detect(CATEGORY, "Observer Safety") ~
                                "Observer Safety and Work Environment",
                              str_detect(CATEGORY, "Permits") ~
                                "Permits/Documents/Record Keeping and Reporting",
                              str_detect(CATEGORY, "Marpol") ~
                                "MARPOL/Oil Spill",
                              str_detect(CATEGORY, "Interference") ~
                                "Interference with Duties",
                              TRUE ~ CATEGORY))

# TODO - Create flextable object

# Heatmaps -------------------------------------------------------------------------------------------------------------

# Because of the way ggplot handles faceting, each category will need to be plotted
#  separately and then combined using patchwork so that tiles are the same size across
#  panels.

# Plots will need to be adjusted depending on what categories/subcategories are included
#  For example, not all category/subcategory combinations contain the same units.
#  In these cases, you'll need to adjust "incident_units" in the "process_data()" fxn
#  when creating the plots so that they match between all categories that will be
#  plotted together.

## Functions ----

# Plot function
#  The create_plot function is designed to create a customizable faceted heatmap-style
#  plot in ggplot2. It is flexible in terms of handling facet labels, legend placement,
#  and formatting based on the user's input.
create_plot <- function(data, txt_width, type = c("top", "mid", "bottom")) {
  # txt_width = width in characters for facet row labels before being wrapped
  # top:    include facet labels for columns but no legend
  # mid:    no facet labels for columns or legend
  # bottom: no facet labels for columns, legend on right
  
  type <- match.arg(type)  # Ensure valid input for type
  
  plot <- ggplot(data) +
    geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)),
                  fill = .data[[paste0("RATE_X_", rate_x, "_plot")]])) +
    geom_text(aes(x = 1, y = SUBCATEGORY,
                  label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
              color = "white", size = 4) +
    facet_grid(cols = vars(INCIDENT_UNIT),
               rows = vars(CAT_COMBO),
               labeller = labeller(CAT_COMBO = label_wrap_gen(width = txt_width)),
               scales = "free_y") +
    theme_bw() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 8),
        panel.grid = element_blank(),
        plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm"),
        panel.spacing.x = unit(0.1, "cm")
      )
    
    # Add theme customization based on the type
    if (type == "top") {
      plot + theme(
        legend.position = "none"
      )
    } else if (type == "mid") {
      plot + theme(
        strip.text.x = element_blank(), 
        legend.position = "none"
      )
    } else if (type == "bottom") {
      plot + theme(
        legend.title = element_text(size = 8),
        strip.text.x = element_blank(),
        legend.text = element_text(size = 8)
      )
    }
}

library(classInt)
library(ggplot2)
library(viridis)
library(scales)

# Function to calculate Goodness of Variance Fit (GVF)
calc_gvf <- function(x, breaks) {
  SST <- sum((x - mean(x))^2)
  class_assignments <- cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE)
  SSW <- sum(tapply(x, class_assignments, function(z) sum((z - mean(z))^2)))
  (SST - SSW) / SST
}

# Function to determine optimal Jenks natural breaks.
optimal_jenks_breaks <- function(x, min_k = 2, max_k = 10, gvf_threshold = 0.9, top_count_threshold = 6) {
  best_result <- NULL
  for (k in min_k:max_k) {
    ci <- classIntervals(x, n = k, style = "jenks")
    current_breaks <- ci$brks
    current_gvf <- calc_gvf(x, current_breaks)
    top_break_count <- sum(x >= current_breaks[length(current_breaks) - 1] & x <= current_breaks[length(current_breaks)])
    cat("For", k, "classes: GVF =", round(current_gvf, 3), "| Top break count =", top_break_count, "\n")
    if (current_gvf >= gvf_threshold && top_break_count < top_count_threshold) {
      best_result <- list(n_breaks = k, breaks = current_breaks, gvf = current_gvf)
      break
    }
  }
  if (is.null(best_result)) {
    ci <- classIntervals(x, n = max_k, style = "jenks")
    best_result <- list(n_breaks = max_k, breaks = ci$brks, gvf = calc_gvf(x, ci$brks))
  }
  best_result
}

opt_result_priority <- optimal_jenks_breaks(subcat_units_rate_priority$RATE, 
                                   min_k = 2, 
                                   max_k = 10, 
                                   gvf_threshold = 0.9, 
                                   top_count_threshold = 6)
opt_result_priority$breaks <- opt_result_priority$breaks * rate_x #Adjusts for the correct column.
opt_result_priority$breaks[1] <- 0 #Sets the lower end of the first break to be zero.


# Non-priority subcategories
opt_result_other <- optimal_jenks_breaks(subcat_units_rate$RATE[!(subcat_units_rate$SUBCATEGORY %in% subcat_units_rate_priority$SUBCATEGORY)], 
                                   min_k = 2, 
                                   max_k = 10, 
                                   gvf_threshold = 0.8, 
                                   top_count_threshold = 6)
opt_result_other$breaks <- opt_result_other$breaks * rate_x #Adjusts for the correct column.
opt_result_other$breaks[1] <- 0 #Sets the lower end of the first break to be zero.

#'*----------------------------------------------------------*
## Prepare data ----
subcat_priority <- subcat_priority %>% mutate(
  Label= cut(RATE * rate_x, breaks = opt_result_priority$breaks)) # Create dummy column for color scale

subcat_other <- subcat_other %>% mutate(
  Label= cut(RATE * rate_x, breaks = opt_result_other$breaks)) # Create dummy column for color scale
#'*----------------------------------------------------------*
## Priority categories plot ----

library(ggplot2)
library(stringr) #Allows for plot overflow of labels for rows

plot_format_fxn <- function(
    df,
    rate_x,                       # user can pass 100 if wanting RATE_X_100
    factor_col   = "Label",
    start_color  = "navy",
    end_color    = "red",
    numeric_format = "%.2f",
    text_color     = "white",
    text_size      = 4,
    legend_title   = "Rate Category",
    base_theme     = theme_bw
) {
  # 1) Reorder the factor levels so the highest interval is last
  df[[factor_col]] <- factor(
    df[[factor_col]],
    levels = sort(levels(df[[factor_col]]))
  )
  
  # 2) Create "nice" labels from cut() defaults (e.g., "(0.05,0.1]" -> "0.05 - 0.1")
  old_labels <- levels(df[[factor_col]])
  new_labels <- gsub("\\(", "", old_labels)       # remove "("
  new_labels <- gsub("\\]", "", new_labels)       # remove "]"
  new_labels <- gsub(",",  " - ", new_labels)     # replace commas with dash
  new_labels <- gsub("-Inf", "-∞", new_labels)    # optional: nicer negative infinity
  new_labels <- gsub("Inf",  "∞",  new_labels)    # optional: nicer infinity
  
  # 3) Create a color palette from user-supplied start_color to end_color
  n_levels  <- length(old_labels)
  my_colors <- colorRampPalette(c(start_color, end_color))(n_levels)
  
  # 4) Build the ggplot
  p <- ggplot(df, aes(x = factor(1))) +
    geom_tile(aes(
      y = reorder(SUBCATEGORY, desc(SUBCATEGORY)),
      fill = .data[[factor_col]]
    )) +
    geom_text(
      aes(
        y = SUBCATEGORY,
        label = sprintf(numeric_format, .data[[paste0("RATE_X_", rate_x)]])
      ),
      color = text_color,
      size  = text_size
    ) +
    # Use "free_y" and "free" space so that facets don't compress rows unevenly
    facet_grid(
      CAT_COMBO ~ INCIDENT_UNIT,
      scales = "free_y",
      space  = "free_y",    
      labeller = labeller(
        # Wrap the facet labels for CAT_COMBO to a given width (e.g., 20 characters)
        CAT_COMBO = function(x) str_wrap(x, width = 10)
      )) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_fill_manual(
      values = my_colors,
      labels = new_labels,
      # Reverse the legend order so highest interval is on top
      guide = guide_legend(reverse = TRUE)
    ) +
    base_theme() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.text.y  = element_text(size = 8), #NEW
      strip.text.x = element_text(size = 8),  # column facet labels
      strip.text.y = element_text(size = 8),   # row facet labels
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid   = element_blank(),
      strip.placement = "outside",                # Place strips fully outside the plot area
      strip.text.y.right = element_text(angle = 0) # Force row labels to be horizontal on the right
    ) +
    labs(fill = legend_title)
  
  return(p)
}

# ------------------- Usage Examples --------------------
#
# If you want to display RATE_X_100, do:
# p <- plot_format_fxn(df = subcat_priority, rate_x = rate_x)
#
plot_format_fxn(df = subcat_priority, rate_x = rate_x, start_color = start_color, end_color = end_color)

plot_format_fxn(df = subcat_other, rate_x = rate_x, start_color = start_color, end_color = end_color)


# Detailed dive into high rates with factors ---------------------------------------------------------------------------







# Save Output -------------------------------------------------------------

# file_4_name <- "AR_4_summary_tables_output.Rdata"
# 
# save(list = ls(),
#      file = file_4_name)
# 
# gdrive_upload(file_4_name, AnnRpt_EnfChp_dribble)
