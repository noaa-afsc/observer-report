
# Annual Report Enforcement chapter: Table generation --------------------------
# Contact Craig Faunce

#TODO - source data has RATE == NA!
#TODO - flextable the proposed table outputs

library(reshape2)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(stringr) #Allows for plot overflow of labels for rows
library(classInt)
library(FMAtools)

# User inputs ------------------------------------------------------------------------------------------------------------
# Set the .Rdata file we will load
file_3_name <- "AR_3_rate_output.Rdata"

# Factor order for plotting
fact_order <- c("DEPLOY", "DAYS", "TRIP", "OFFLOAD", "HAUL", "SAMPLE", "MAR MAM")

end_color   <- "red"
start_color     <- "darkblue"

# Rate
rate_x <- 100

# Load data ------------------------------------------------------------------------------------------------------------
# Assign the address of the Annual Report Project in the Shared Gdrive
AnnRpt_EnfChp_dribble <- gdrive_set_dribble("Projects/Annual Report OLE chapter 5/2024_data")

# Pull Rdata file from google drive. This will update your local if the GDrive version is newer,
#  otherwise it will load your local
gdrive_download(file_3_name, AnnRpt_EnfChp_dribble)

load(file = file_3_name)

rm(list = ls()[!ls() %in% c("subcat_units_rate", "subcat_units_rate_for_factors","assignments_dates_cr_perm",
                            "start_color", "end_color", "rate_x", "adp_yr")])

# Data manipulation ----------------------------------------------------------------------------------------------------

subcat_priority <-
  subcat_units_rate %>%
  filter(CALENDAR_YEAR == adp_yr,
         CATEGORY %in% c("GEAR/EQUIPMENT REQUIREMENTS", "INTERFERENCE WITH DUTIES",
                         "OBSERVER SAFETY AND WORK ENVIRONMENT", "SAFETY-USCG-MARINE CASUALTY")) %>%
   mutate(SUPER_CAT = "PRIORITY",
          COVERAGE_TYPE = NA,
          VESSEL_TYPE = NA,
          NMFS_REGION = NA) %>% 
  select(SUPER_CAT, COVERAGE_TYPE, VESSEL_TYPE, NMFS_REGION, CATEGORY, SUBCATEGORY, INCIDENT_UNIT, RATE)

# Priority subcategories by factor
factor_priority <-   
  subcat_units_rate_for_factors %>%
  filter(CALENDAR_YEAR == adp_yr, CATEGORY %in% subcat_priority$CATEGORY) %>%
  mutate(SUPER_CAT = "PRIORITY_FACTORS") %>%
  select(SUPER_CAT, COVERAGE_TYPE, VESSEL_TYPE, NMFS_REGION, CATEGORY, SUBCATEGORY, INCIDENT_UNIT, RATE)

priority <- rbind(subcat_priority, factor_priority) %>%
  mutate(CAT_COMBO = ifelse(str_detect(CATEGORY, "USCG|SAFETY"),
                   "SAFETY / WORK ENVIRONMENT", CATEGORY),
         CAT_COMBO = ifelse(str_detect(CATEGORY, "GEAR"), 
                            "GEAR / EQUIPMENT REQUIREMENTS",
                   CAT_COMBO))

# Non-priority subcategories
subcat_other <- 
  subcat_units_rate %>%
  filter(CALENDAR_YEAR == adp_yr,
         !(CATEGORY %in% subcat_priority$CATEGORY)) %>%
  mutate(SUPER_CAT = "NON-PRIORITY",
         COVERAGE_TYPE = NA,
         VESSEL_TYPE = NA,
         NMFS_REGION = NA) %>% 
  select(SUPER_CAT, COVERAGE_TYPE, VESSEL_TYPE, NMFS_REGION, CATEGORY, SUBCATEGORY, INCIDENT_UNIT, RATE)

 factor_other <-  
   subcat_units_rate_for_factors %>%
   filter(CALENDAR_YEAR == adp_yr, 
          !(CATEGORY %in% subcat_priority$CATEGORY)) %>%
   mutate(SUPER_CAT = "NON-PRIORITY_FACTORS") %>%
   select(SUPER_CAT, COVERAGE_TYPE, VESSEL_TYPE, NMFS_REGION, CATEGORY, SUBCATEGORY, INCIDENT_UNIT, RATE)
 
 non_priority <- 
   rbind(subcat_other, factor_other) %>%
   mutate(CAT_COMBO = case_when(str_detect(CATEGORY, "SAFETY-USCG|MARPOL") ~
                                  "SAFETY-USCG / MARPOL / OIL SPILL",
                                str_detect(CATEGORY, "PERMITS/DOCUMENTS|OPERATIONAL|GEAR") ~
                                  "GEAR / EQUIPMENT / OPERATIONAL REQUIREMENTS / PERMITS / DOCUMENTS / RECORD KEEPING AND REPORTING",
                                str_detect(CATEGORY, "SUSTAINABLE|SPECIES") ~
                                  "PROHIBITED SPECIES / MARINE MAMMALS / SEABIRDS / SUSTAINABLE FISHERIES",
                                TRUE ~ CATEGORY))
  
for_figures <- 
  rbind(priority, non_priority) %>%
   mutate(INCIDENT_UNIT = case_when(INCIDENT_UNIT == "OFFL" ~ "OFFLOAD",
                             INCIDENT_UNIT == "SAMP" ~ "SAMPLE",
                             INCIDENT_UNIT == "DEPL" ~ "DEPLOY",
                             INCIDENT_UNIT == "MARM" ~ "MAR MAM",
                             TRUE ~ INCIDENT_UNIT),
        !! paste0("RATE_X_", rate_x) := RATE * rate_x) %>% filter(!is.na(RATE)) #NAs removed

rm(subcat_priority, subcat_other, priority, non_priority, factor_priority, factor_other)
# Tables -----------------------------------------------------------------------------------------------
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

# Determine color breaks ------------------------------------------------------------------------------------
calc_gvf <- function(x, breaks) {
  SST <- sum((x - mean(x))^2)
  class_assignments <- cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE)
  SSW <- sum(tapply(x, class_assignments, function(z) sum((z - mean(z))^2)))
  (SST - SSW) / SST
}

optimal_jenks_breaks <- function(x, 
                                 min_k = 2, 
                                 max_k = 10, 
                                 gvf_threshold = 0.9, 
                                 top_count_threshold = 6) {
  # -- 1) Remove all NAs --
  x <- na.omit(x)
  
  # -- 2) If there's not enough data left, return a default result --
  if (length(x) < 2) {
    cat("No data or insufficient data after removing NAs.\n")
    return(list(n_breaks = NA, breaks = NA, gvf = NA))
  }
  
  best_result <- NULL
  for (k in min_k:max_k) {
    ci <- classIntervals(x, n = k, style = "jenks")
    current_breaks <- ci$brks
    
    # Calculate current GVF
    current_gvf <- calc_gvf(x, current_breaks)
    
    # Count how many data points fall in the top break
    top_break_count <- sum(x >= current_breaks[length(current_breaks) - 1] &
                             x <= current_breaks[length(current_breaks)])
    
    cat("For", k, "classes: GVF =", round(current_gvf, 3),
        "| Top break count =", top_break_count, "\n")
    
    # -- 3) Safely check for NA in current_gvf --
    if (!is.na(current_gvf) && current_gvf >= gvf_threshold && top_break_count < top_count_threshold) {
      best_result <- list(n_breaks = k, breaks = current_breaks, gvf = current_gvf)
      break
    }
  }
  
  # If we never found a solution that met the conditions, fall back to max_k
  if (is.null(best_result)) {
    ci <- classIntervals(x, n = max_k, style = "jenks")
    best_result <- list(
      n_breaks = max_k, 
      breaks   = ci$brks, 
      gvf      = calc_gvf(x, ci$brks)
    )
  }
  
  best_result
}

# 1) Get unique categories from SUPER_CAT
cats <- unique(for_figures$SUPER_CAT)

# 2) For each category, compute Jenks breaks and store in a list
breaks_list <- setNames(vector("list", length(cats)), cats)

for (cat in cats) {
  #testing cat <- cats[1]
  # Subset RATE values only for this category
  x_subset <- for_figures$RATE[for_figures$SUPER_CAT == cat]
  
  # Calculate optimal Jenks breaks
  best <- optimal_jenks_breaks(x_subset, 
                               min_k = 2, 
                               max_k = 5, 
                               gvf_threshold = 0.8, 
                               top_count_threshold = 6)
  
  # Scale breaks by rate_x
  best$breaks <- best$breaks * rate_x
  
  # (Optional) Set the lower bound to zero
  best$breaks[1] <- 0
  
  # Store results
  breaks_list[[cat]] <- best
}

# 3) Create a new column 'Label' using the category-specific breaks
for_figures$Label <- NA  # initialize

for (cat in cats) {
  # Index only the rows for this category
  idx <- for_figures$SUPER_CAT == cat
  
  # Use the precomputed breaks for this category
  cat_breaks <- breaks_list[[cat]]$breaks
  
  # Cut the scaled RATE values into the category-specific bins
  for_figures$Label[idx] <- as.character(cut(
    x      = for_figures$RATE[idx] * rate_x,
    breaks = cat_breaks))
  #Intervals look like (lower, upper], meaning the upper bound is included while the lower bound is not.
}

# Check results
head(for_figures)
# You now have a 'Label' factor with intervals that depend on each
# category's own Jenks classification.

rm(best, breaks_list, cat, cat_breaks, cats, idx, x_subset)

# Plots ---------------------------------------------------------------------------
## Plot function
plot_format_fxn <- function(
    df,
    rate_x,                       # user can pass 100 if wanting RATE_X_100
    factor_col    = "Label",
    start_color   = "navy",
    end_color     = "red",
    numeric_format = "%.2f",
    text_color    = "white",
    text_size     = 4,
    legend_title  = "Rate Category",
    base_theme    = theme_bw,
    # New argument lets you override the facet formula:
    facet_formula = CAT_COMBO ~ INCIDENT_UNIT
) {
  # 1) Reorder the factor levels so the highest interval is last
  df[[factor_col]] <- factor(
    df[[factor_col]],
    levels = sort(unique(df[[factor_col]]))
  )
  
  # 2) Create "nice" labels from cut() defaults
  old_labels <- levels(df[[factor_col]])
  new_labels <- gsub("\\(", "", old_labels)  # remove "("
  new_labels <- gsub("\\]", "", new_labels)  # remove "]"
  new_labels <- gsub(",",  " - ", new_labels)
  new_labels <- gsub("( - )(\\d+(\\.\\d+)?)$", " - <\\2", new_labels)
  
  # 3) Create a color palette
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
    facet_grid(
      facet_formula,
      scales = "free_y",
      space  = "free_y",
      labeller = labeller(
        # Wrap if you still want custom label behavior – or remove if not needed
        CAT_COMBO = function(x) str_wrap(x, width = 10)
      )
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_fill_manual(
      values = my_colors,
      labels = new_labels,
      guide = guide_legend(reverse = TRUE)
    ) +
    base_theme() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.text.y  = element_text(size = 8),
      strip.text.x = element_text(size = 8),
      strip.text.y = element_text(size = 8),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid   = element_blank(),
      strip.placement = "outside",
      strip.text.y.right = element_text(angle = 0)
    ) +
    labs(fill = legend_title)
  
  return(p)
}

# If you want to display RATE_X_100, do:
# p <- plot_format_fxn(df = subcat_priority, rate_x = rate_x)
#
priority_plot <- plot_format_fxn(df = for_figures %>% filter(SUPER_CAT == "PRIORITY"), rate_x = rate_x, start_color = start_color, end_color = end_color)

non_priority_plot <- plot_format_fxn(df = for_figures %>% filter(SUPER_CAT == "NON-PRIORITY"), rate_x = rate_x, start_color = start_color, end_color = end_color)

# Detailed dive into high rates with factors ---------------------------------------------------------------------------
#For factors, we want to use coverage type, FMP, and Vessel Type.
super_levels <- data.frame(SUPER_CAT = c("PRIORITY", "NON-PRIORITY"))
super_levels$SUPER_FACT = paste0(super_levels$SUPER_CAT, "_FACTORS")

#Lets get the super group
for(i in 1:nrow(super_levels)){
  
df_super <- for_figures %>% filter(SUPER_CAT == super_levels$SUPER_CAT[i])
df_super$Label <- factor(df_super$Label, levels = sort(unique(df_super$Label)))
# This ensures the lowest interval is first in levels(), and the highest is last.

# 2) Grab only rows that have the highest (last) factor level
highest_level <- tail(levels(df_super$Label), 1)             # e.g. "(2,3]"
df_highest    <- subset(df_super, Label == highest_level)

#Now we grab the factors data
df <- for_figures %>% filter(SUPER_CAT == super_levels$SUPER_FACT[i])

#Now limit df to just those categories
df <- merge(df, 
            df_highest %>% select(CATEGORY, SUBCATEGORY, INCIDENT_UNIT) %>% distinct())
#Make plot
factor_plot <- plot_format_fxn(df = df, rate_x = rate_x, 
                               start_color = start_color, 
                               end_color = end_color,
                               facet_formula = NMFS_REGION ~ COVERAGE_TYPE + VESSEL_TYPE + INCIDENT_UNIT
                               )
#Give it a name
assign(paste0(tolower(gsub("-", "_", super_levels$SUPER_FACT[i])), "_plot"), factor_plot)

#cleanup
rm(df_super, highest_level, df_highest, df, factor_plot)
}
rm(super_levels, i)

# Save Output -------------------------------------------------------------
ggsave("priority_plot.pdf", plot = priority_plot, width = 12, height = 6, units = "in", path = "Plots/")
ggsave("non_priority_plot.pdf", plot = non_priority_plot, width = 12, height = 6, units = "in", path = "Plots/")
ggsave("priority_factors_plot.pdf", plot = priority_factors_plot, width = 12, height = 6, units = "in", path = "Plots/")
ggsave("non_priority_factors_plot.pdf", plot = non_priority_factors_plot, width = 6, height = 6, units = "in", path = "Plots/")

save.image(file = "AR_4_summary_tables_output.Rdata")
# 
gdrive_upload("AR_4_summary_tables_output.Rdata", AnnRpt_EnfChp_dribble)
