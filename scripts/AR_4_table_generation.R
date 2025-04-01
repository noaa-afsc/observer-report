
# Annual Report Enforcement chapter: Table generation --------------------------
# Contact Andy Kingham
# 206-526-4212

library(reshape2)
library(tidyverse)
library(patchwork)
library(FMAtools)

# Load data ------------------------------------------------------------------------------------------------------------
# Set the .Rdata file we will load
file_3_name <- "AR_3_rate_output.Rdata"

# Assign the address of the Annual Report Project in the Shared Gdrive
AnnRpt_EnfChp_dribble <- gdrive_set_dribble("Projects/Annual Report OLE chapter 5/2024_data")

# Pull Rdata file from google drive. This will update your local if the GDrive version is newer,
#  otherwise it will load your local
gdrive_download(file_3_name, AnnRpt_EnfChp_dribble)
load(file = file_3_name)


# Data manipulation ----------------------------------------------------------------------------------------------------

#TODO - Placeholder until Andy is done in AR_2 & AR_3, then make sure this is in there
subcat_units_rate_priority <-
  subcat_units_rate %>%
  filter(SUBCATEGORY %in% c("ACCESS", "ASSAULT", "BIN MONITORING",
                            "DESTRUCTION OF SAMPLE/WORK/PERSONAL EFFECTS",
                            "FOOD AND ACCOMMODATIONS", "FORCED TO PERFORM CREW DUTIES",
                            "HOSTILE WORK ENVIRONMENT", "IMPEDIMENT",
                            "INTIMIDATION/BRIBERY/COERCION", "MARINE CASUALTY",
                            "NOTIFICATION", "OBSERVER SAMPLING STATION",
                            "REASONABLE ASSISTANCE", "SAFETY",
                            "SAMPLING INTERFERENCE", "SCALES", "SEXUAL ASSAULT",
                            "SEXUAL HARASSMENT", "VIDEO MONITORING SYSTEM",
                            "VMS REQUIREMENTS")) %>%
  mutate(CAT_COMBO = ifelse(str_detect(CATEGORY, "USCG|SAFETY"),
                            "OBSERVER SAFETY AND WORK ENVIRONMENT / USCG-SAFETY", CATEGORY))

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
subcat_other <- subcat_units_rate %>%
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

library(flextable)
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

# Create flextable object

# Heatmaps -------------------------------------------------------------------------------------------------------------

# Because of the way ggplot handles faceting, each category will need to be plotted
#  separately and then combined using patchwork so that tiles are the same size across
#  panels.

# Plots will need to be adjusted depending on what categories/subcategories are included
#  For example, not all category/subcategory combinations contain the same units.
#  In these cases, you'll need to adjust "incident_units" in the "process_data()" fxn
#  when creating the plots so that they match between all categories that will be
#  plotted together.

## Inputs ----

# Factor order for plotting
fact_order <- c("DEPLOY", "DAYS", "TRIP", "OFFLOAD", "HAUL", "SAMPLE", "MAR MAM")

# Rate
rate_x <- 100

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

#TODO - I think that this is a mistake.  Better to use natural breaks (jenks) and set by that.
###########################################################################################

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

# Create a heatmap with ggplot2 using the computed natural breaks in the color bar
# ggplot(df, aes(x = factor(x), y = factor(y), fill = rate)) +
#   geom_tile(color = "white") +
#   scale_fill_gradientn(
#     colours = viridis(100),
#     breaks = opt_result$breaks,
#     labels = number_format(accuracy = 0.1)
#   ) +
#   theme_minimal() +
#   labs(x = "X-axis", y = "Y-axis", fill = "Rate")

############################################################################################
# Define common color scale with global limits
#  The color_scale function is used to apply a customized color gradient scale
#  to a numeric variable, typically a rate. It provides a navy-to-red gradient,
#  adjusts the scale based on the data's range and a user-defined upper limit (max),
#  and customizes the appearance of the color scale's legend.
color_scale <- function(data, max) {
  # max: set appropriate "threshold_" object
  
  color_scale <- scale_fill_gradient(
    low = "navy", 
    high = "red",
    na.value = "transparent", 
    name = paste0("Rate (× ", rate_x, ")"),
    limits = c(min(data[[paste0("RATE_X_", rate_x)]], na.rm = TRUE),
               max(max, na.rm = TRUE)),
    # Legend (currently not used)
    breaks = data.frame(n = seq(1, max(data[[paste0("RATE_X_", rate_x)]]) + 25, by = 25)) %>%
      mutate(n = ifelse(n > 1, n - 1, n)) %>% unlist(use.names = FALSE),
    guide = guide_colorbar(
      order = 2,
      title.hjust = 0.8,
      ticks = FALSE,
      barheight = unit(1.33, "in"),
      barwidth = unit(0.33, "in")
    )
  )
}

# Process data for individual plots
#  The process_data function processes a dataset by:
#  1) Filtering rows based on a search string that identifies a specific category
#     in the CAT_COMBO column.
#  2) Adding missing incident units for visualization, ensuring that all required
#     INCIDENT_UNIT types are present in the dataset.
#  3) Adjusting the factor levels of the INCIDENT_UNIT column to a predefined
#     order for consistent plotting.
process_data <- function(data, search_string, incident_units) {
  # search_string:  unique string that identifies a specific category.
  #       enter as a string, e.g., "SPECIES"
  # incident_units: missing unit types to be added for plotting.
  #       enter as: c("UNIT_TYPE_1", "UNIT_TYPE_2", "ETC.")
  
  data %>%
    filter(str_detect(CAT_COMBO, search_string)) %>%
    bind_rows(.,
              tibble(CAT_COMBO = pull(., CAT_COMBO)[1],
                     SUBCATEGORY = pull(., SUBCATEGORY)[1],
                     INCIDENT_UNIT = incident_units)) %>%
    mutate(INCIDENT_UNIT = factor(INCIDENT_UNIT, levels = fact_order))
}

# Set threshold value where all colors >= will be the "hottest" color
#  Threshold is needed when there are large outliers
#  The function will check the maximum value within the data and if it is 2×
#  greater or more than the next largest value, it will set a threshold
#  value = 1.55× the second largest value 

calculate_threshold <- function(df, return_name) {
  # return_name: the object name you want the value to be returned as
  
  # Compute threshold priority
  threshold <- df %>%
    filter(CALENDAR_YEAR == adp_yr) %>%
    select(RATE) %>%
    filter(!is.na(RATE)) %>%
    unlist() %>%
    { 
      max_rate <- max(.)
      second_largest_rate <- sort(., decreasing = TRUE)[2]
      ifelse(max_rate < 2 * second_largest_rate,
             max_rate * rate_x,
             1.55 * second_largest_rate * rate_x)
    }
  # Dynamically create the variable name in the parent environment
  assign(return_name, threshold, envir = parent.frame())
}

#'*----------------------------------------------------------*
## Check data values ----
# Priority subcategories
ggplot(filter(subcat_units_rate_priority, CALENDAR_YEAR == adp_yr),
       aes(x = SUBCATEGORY, y = RATE * rate_x)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Christian
calculate_threshold(subcat_units_rate_priority, "threshold_priority")
threshold_priority

# Craig
opt_result_priority <- optimal_jenks_breaks(subcat_units_rate_priority$RATE, 
                                   min_k = 2, 
                                   max_k = 10, 
                                   gvf_threshold = 0.8, 
                                   top_count_threshold = 6)
opt_result_priority$breaks * 100


# Non-priority subcategories
ggplot(filter(subcat_units_rate,
              !(SUBCATEGORY %in% subcat_units_rate_priority$SUBCATEGORY) &
                CALENDAR_YEAR == adp_yr),
       aes(x = SUBCATEGORY, y = RATE * rate_x)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Christian
calculate_threshold(filter(subcat_units_rate,
                           !(SUBCATEGORY %in% subcat_units_rate_priority$SUBCATEGORY)),
                    "threshold_other")
threshold_other

# Craig
opt_result_other <- optimal_jenks_breaks(subcat_units_rate$RATE[!(subcat_units_rate$SUBCATEGORY %in% subcat_units_rate_priority$SUBCATEGORY)], 
                                   min_k = 2, 
                                   max_k = 10, 
                                   gvf_threshold = 0.8, 
                                   top_count_threshold = 6)
opt_result_other$breaks * 100


#'*----------------------------------------------------------*
## Prepare data ----
subcat_priority <- subcat_priority %>% mutate(
  !!paste0("RATE_X_", rate_x, "_plot") := pmin(RATE, threshold_other/rate_x) * rate_x) # Create dummy column for color scale

subcat_other <- subcat_other %>% mutate(
  !!paste0("RATE_X_", rate_x, "_plot") := pmin(RATE, threshold_other/rate_x) * rate_x) # Create dummy column for color scale
#'*----------------------------------------------------------*
## Priority categories plot ----

# Step 1: Create individual plots for each CATEGORY
# Plot for CATEGORY_1
plot1 <- create_plot(process_data(subcat_priority, "GEAR",
                                  c("OFFLOAD", "SAMPLE")),
                     txt_width = 17,
                     "top") +
  color_scale(subcat_priority, max = threshold_priority)

# plot for CATEGORY_2
plot2 <- create_plot(process_data(subcat_priority, "INTERFERENCE",
                                  c("TRIP")),
                     txt_width = 15,
                     "mid") +
  color_scale(subcat_priority, max = threshold_priority)

# Plot for CATEGORY_3
plot3 <- create_plot(process_data(subcat_priority, "USCG",
                                  c("HAUL", "OFFLOAD", "SAMPLE")),
                     txt_width = 37,
                     "mid") +
  color_scale(subcat_priority, max = threshold_priority)

# Step 2: Combine the plots using patchwork
priority_subcat_plot <- plot1 / plot2 / plot3 + plot_layout(
  heights = subcat_priority %>%
    group_by(CAT_COMBO) %>%
    summarize(n = n_distinct(SUBCATEGORY)) %>%
    .[["n"]]) +
  theme(
    axis.title.y = element_text(size = 10,
                                hjust = 1.2, # Likely have to play around with this to get it centered
                                margin = margin(r = 20),
                                angle = 90)) +
  labs(y = "SUBCATEGORY")

# Step 3: Display and save the combined plot
priority_subcat_plot

ggsave(paste0("confidential_figures/fig_", adp_yr, "_priority_subcat_plot.jpg"),
       device = "jpeg",
       width = 9,
       height = 6,
       units = "in",
       dpi = 300)

#'*----------------------------------------------------------*
### ALT: Priority categories plot ----
#TODO - this doesnt look right.  Uses priority data and then plots non priority
# This still needs to be cleaned up a lot, but here is what the data look like
# Wanted you to at least have something while I'm away CG 20250328

library(ggh4x)

color_scale_factor <- function(data, max) {
  # max: set appropriate "threshold_" object
  
  color_scale <- scale_fill_gradient(
    low = "navy", 
    high = "red",
    na.value = "transparent", 
    name = paste0("Rate (× ", rate_x, ")"),
    limits = c(min(data[[paste0("RATE_X_", rate_x)]], na.rm = TRUE),
               max(max, na.rm = TRUE))
  )
}

create_factor_plot <- function(data, txt_width, type = c("top", "mid", "bottom")) {
  # txt_width = width in characters for facet row labels before being wrapped
  # top:    include facet labels for columns but no legend
  # mid:    no facet labels for columns or legend
  # bottom: no facet labels for columns, legend on right
  
  type <- match.arg(type)  # Ensure valid input for type
  
  plot <- ggplot(data) +
    geom_tile(aes(x = VESSEL_TYPE, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)),
                  fill = .data[[paste0("RATE_X_", rate_x, "_plot")]])) +
    geom_text(aes(x = VESSEL_TYPE, y = SUBCATEGORY,
                  label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
              color = "white", size = 4) +
    facet_nested(rows = vars(CAT_COMBO),
                 cols = vars(INCIDENT_UNIT, COVERAGE_TYPE, NMFS_REGION),
                 labeller = labeller(CAT_COMBO = label_wrap_gen(width = txt_width)),
                 scales = "free_y") +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 8, color = "black"),
      axis.title.y = element_blank(),
      strip.text = element_text(size = 8),
      panel.grid = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      panel.spacing.x = unit(0.1, "cm"),
      legend.position = "none",
      axis.text.x = element_text(angle = 90, hjust= 0, vjust = 0.5)
    )
  
  # # Add theme customization based on the type
  # if (type == "top") {
  #   plot + theme(
  #     axis.text.x = element_blank(),
  #     legend.position = "none"
  #   )
  # } else if (type == "mid") {
  #   plot + theme(
  #     axis.text.x = element_blank(),
  #     strip.text.x = element_blank(), 
  #     legend.position = "none"
  #   )
  # } else if (type == "bottom") {
  #   plot + theme(
  #     strip.text.x = element_blank(),
  #     legend.position = "none"
  #   )
  # }
}

subcat_units_rate_for_factors_priority <-
  subcat_units_rate_for_factors %>%
  filter(SUBCATEGORY %in% c("ACCESS", "ASSAULT", "BIN MONITORING",
                            "DESTRUCTION OF SAMPLE/WORK/PERSONAL EFFECTS",
                            "FOOD AND ACCOMMODATIONS", "FORCED TO PERFORM CREW DUTIES",
                            "HOSTILE WORK ENVIRONMENT", "IMPEDIMENT",
                            "INTIMIDATION/BRIBERY/COERCION", "MARINE CASUALTY",
                            "NOTIFICATION", "OBSERVER SAMPLING STATION",
                            "REASONABLE ASSISTANCE", "SAFETY",
                            "SAMPLING INTERFERENCE", "SCALES", "SEXUAL ASSAULT",
                            "SEXUAL HARASSMENT", "VIDEO MONITORING SYSTEM",
                            "VMS REQUIREMENTS")) %>%
  mutate(CAT_COMBO = ifelse(str_detect(CATEGORY, "USCG|SAFETY"),
                            "OBSERVER SAFETY AND WORK ENVIRONMENT / USCG-SAFETY", CATEGORY))

calculate_threshold(subcat_units_rate_for_factors_priority, "threshold_factor_priority")

subcat_factors_priority <- subcat_units_rate_for_factors_priority %>%
  filter(CALENDAR_YEAR == adp_yr) %>%
  mutate(!! paste0("RATE_X_", rate_x) := RATE * rate_x,
         # Create dummy column for color scale
         !! paste0("RATE_X_", rate_x, "_plot") := pmin(RATE, threshold_factor_priority/rate_x) * rate_x,
         INCIDENT_UNIT = case_when(INCIDENT_UNIT == "OFFL" ~ "OFFLOAD",
                                   INCIDENT_UNIT == "SAMP" ~ "SAMPLE",
                                   INCIDENT_UNIT == "DEPL" ~ "DEPLOY",
                                   INCIDENT_UNIT == "MARM" ~ "MAR MAM",
                                   TRUE ~ INCIDENT_UNIT),
         CAT_COMBO = ifelse(str_detect(CATEGORY, "GEAR"), "GEAR / EQUIPMENT REQUIREMENTS",
                            CAT_COMBO)) %>%
  select(CAT_COMBO, SUBCATEGORY, INCIDENT_UNIT, paste0("RATE_X_", rate_x),
         paste0("RATE_X_", rate_x, "_plot"), COVERAGE_TYPE, VESSEL_TYPE, NMFS_REGION) %>%
  filter(!is.na(NMFS_REGION))

create_factor_plot(subcat_factors_priority,
                   txt_width = 17) +
  color_scale_factor(subcat_factors_priority, max = threshold_factor_priority)

#'*----------------------------------------------------------*
## Non-priority categories plot ----

# Step 1: Create individual plots for each CATEGORY
# Plot for CATEGORY_1
plot1 <- create_plot(process_data(subcat_other, "CONTRACTOR",
                                  c("HAUL", "MAR MAM", "OFFLOAD", "SAMPLE", "TRIP")),
                     txt_width = 15,
                     "top") +
  color_scale(subcat_other, max = threshold_other)

# Plot for CATEGORY_2
plot2 <- create_plot(process_data(subcat_other, "OPERATIONAL REQ",
                                  c("DEPLOY", "MAR MAM")),
                     txt_width = 55,
                     "mid") +
  color_scale(subcat_other, max = threshold_other)

# Plot for CATEGORY_3
plot3 <- create_plot(process_data(subcat_other, "SPECIES",
                                  c("DAYS", "DEPLOY", "SAMPLE")),
                     txt_width = 37,
                     "mid") +
  color_scale(subcat_other, max = threshold_other)

# Plot for CATEGORY_4
plot4 <- create_plot(process_data(subcat_other, "USCG",
                                  c("DEPLOY", "HAUL", "MAR MAM", "OFFLOAD", "SAMPLE")),
                     txt_width = 19,
                     "mid") +
  color_scale(subcat_other, max = threshold_other)

# Step 2: Combine the plots using patchwork
non_priority_subcat_plot <- plot1 / plot2 / plot3 / plot4 + plot_layout(
  heights = subcat_other %>%
    group_by(CAT_COMBO) %>%
    summarize(n = n_distinct(SUBCATEGORY)) %>%
    .[["n"]]) +
  theme(
    axis.title.y = element_text(size = 10,
                                hjust = 17, # Likely have to play around with this to get it centered
                                margin = margin(r = 20),
                                angle = 90)) +
  labs(y = "SUBCATEGORY")

# Step 3: Display and save the combined plot
non_priority_subcat_plot

ggsave(paste0("confidential_figures/fig_", adp_yr, "_non_priority_subcat_plot.jpg"),
       device = "jpeg",
       width = 10,
       height = 9.5,
       units = "in",
       dpi = 300)

#'*----------------------------------------------------------*
#TODO - Hate it.
### ALT: Non-priority categories plot ----
# This can be cleaned up a lot, but I just wanted to get it in here since I'll be away CG 20250328

plot1 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "DEP"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
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
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot2 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "DAY"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
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
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot3 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "TRIP"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
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
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot4 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "OFF"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
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
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot5 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "HAUL"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
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
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot6 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "SAM"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
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
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

plot7 <- ggplot(filter(subcat_other, str_detect(INCIDENT_UNIT, "MAR"))) +
  geom_tile(aes(x = 1, y = reorder(SUBCATEGORY, desc(SUBCATEGORY)), fill = RATE_X_100_plot)) +
  geom_text(aes(x = 1, y = SUBCATEGORY,
                label = sprintf("%.2f", .data[[paste0("RATE_X_", rate_x)]])),
            color = "white", size = 4) +
  facet_grid(rows = vars(INCIDENT_UNIT), scales = "free_y") + 
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
  ) +
  color_scale(subcat_other, max = threshold_other) +
  theme(
    legend.position = "none")

non_priority_subcat_plot <- plot1 / plot2 / plot3 / plot4 / plot5 / plot6 / plot7 + plot_layout(
  heights = subcat_other %>%
    mutate(INCIDENT_UNIT = factor(INCIDENT_UNIT, levels = fact_order)) %>%
    group_by(INCIDENT_UNIT) %>%
    summarize(n = n_distinct(SUBCATEGORY)) %>%
    .[["n"]]) +
  theme(
    axis.title.y = element_text(size = 10,
                                hjust = 17, # Likely have to play around with this to get it centered
                                margin = margin(r = 20),
                                angle = 90)) +
  labs(y = "SUBCATEGORY")

non_priority_subcat_plot

ggsave(paste0("confidential_figures/fig_", adp_yr, "_non_priority_subcat_plot_long.jpg"),
       device = "jpeg",
       width = 7,
       height = 10.5,
       units = "in",
       dpi = 300)


# Clear unneeded objects
rm(color_scale, create_plot, process_data, fact_order, rate_x,
   plot1, plot2, plot3, plot4, subcat_other, subcat_priority, threshold_other,
   threshold_priority, calculate_threshold)


# Detailed dive into high rates with factors ---------------------------------------------------------------------------







# Save Output -------------------------------------------------------------

file_4_name <- "AR_4_summary_tables_output.Rdata"

save(list = ls(),
     file = file_4_name)

gdrive_upload(file_4_name, AnnRpt_EnfChp_dribble)
