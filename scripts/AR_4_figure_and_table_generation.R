
# Annual Report Enforcement chapter: Table generation --------------------------
# Contact Craig Faunce

#TODO - source data has RATE == NA!
#TODO - Generate summary of number of regulations at the category level for Table

library(tidyverse)
library(ggplot2)
library(stringr) #Allows for plot overflow of labels for rows
library(classInt)
library(ggh4x) # (pretty headers) https://teunbrand.github.io/ggh4x/articles/Facets.html
library(flextable)
library(officer)
library(readxl) #for importing the odds data
library(FMAtools)

# User inputs ------------------------------------------------------------------------------------------------------------
# Set the .Rdata file we will load
file_3_name <- "AR_3_rate_output.Rdata"
odds_file <- "possible_trips_not_logged_or_logged_incorrectly_copy_4_7_2025.xlsx"

# Factor order for plotting
fact_order <- c("DEPLOY", "DAYS", "TRIP", "OFFLOAD", "HAUL", "SAMPLE", "MAMMALS")

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
gdrive_download(odds_file, AnnRpt_EnfChp_dribble)

#load violations
load(file = file_3_name)

#load odds violations
#Identify the available sheets
excel_sheets(odds_file)
odds_dat <- read_excel(odds_file, sheet = "2024 Issues") #TODO - update this every year

rm(list = ls()[!ls() %in% c("df_obs_statements", "subcat_units_rate", "subcat_units_rate_for_factors","assignments_dates_cr_perm",
                            "start_color", "end_color", "rate_x", "adp_yr", "AnnRpt_EnfChp_dribble", "odds_dat")])
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

# Other subcategories
subcat_other <- 
  subcat_units_rate %>%
  filter(CALENDAR_YEAR == adp_yr,
         !(CATEGORY %in% subcat_priority$CATEGORY)) %>%
  mutate(SUPER_CAT = "OTHER",
         COVERAGE_TYPE = NA,
         VESSEL_TYPE = NA,
         NMFS_REGION = NA) %>% 
  select(SUPER_CAT, COVERAGE_TYPE, VESSEL_TYPE, NMFS_REGION, CATEGORY, SUBCATEGORY, INCIDENT_UNIT, RATE)

 factor_other <-  
   subcat_units_rate_for_factors %>%
   filter(CALENDAR_YEAR == adp_yr, 
          !(CATEGORY %in% subcat_priority$CATEGORY)) %>%
   mutate(SUPER_CAT = "OTHER_FACTORS") %>%
   select(SUPER_CAT, COVERAGE_TYPE, VESSEL_TYPE, NMFS_REGION, CATEGORY, SUBCATEGORY, INCIDENT_UNIT, RATE)
 
 other <- 
   rbind(subcat_other, factor_other) %>%
   mutate(CAT_COMBO = case_when(str_detect(CATEGORY, "SAFETY-USCG|MARPOL") ~
                                  "SAFETY-USCG / MARPOL / OIL SPILL",
                                str_detect(CATEGORY, "PERMITS/DOCUMENTS|OPERATIONAL|GEAR") ~
                                  "GEAR / EQUIPMENT / OPERATIONAL REQUIREMENTS / PERMITS / DOCUMENTS / RECORD KEEPING AND REPORTING",
                                str_detect(CATEGORY, "SUSTAINABLE|SPECIES") ~
                                  "PROHIBITED SPECIES / MARINE MAMMALS / SEABIRDS / SUSTAINABLE FISHERIES",
                                TRUE ~ CATEGORY))
  
for_figures <- 
  rbind(priority, other) %>%
   mutate(INCIDENT_UNIT = case_when(INCIDENT_UNIT == "OFFL" ~ "OFFLOAD",
                             INCIDENT_UNIT == "SAMP" ~ "SAMPLE",
                             INCIDENT_UNIT == "DEPL" ~ "DEPLOY",
                             INCIDENT_UNIT == "MARM" ~ "MAMMALS",
                             TRUE ~ INCIDENT_UNIT),
        !! paste0("RATE_X_", rate_x) := RATE * rate_x) %>% filter(!is.na(RATE)) #NAs removed

#ODDS
odds_dat$`Issue Category` <- ifelse(grepl("not logged", odds_dat$`Issue Category`, ignore.case = TRUE), 
                                    "No Logged Trip", 
                                    odds_dat$`Issue Category`)
odds_dat$`Issue Category` <- ifelse(grepl("canceled", odds_dat$`Issue Category`, ignore.case = TRUE), 
                                    "Canceled Trip Fished", 
                                    odds_dat$`Issue Category`)
odds_dat$`Issue Category` <- ifelse(grepl("NMFS", odds_dat$`Issue Category`, ignore.case = TRUE), 
                                    "Incorrect FMP Area", 
                                    odds_dat$`Issue Category`)
odds_dat$`Trip Ending Port` <- ifelse(grepl("San Point", odds_dat$`Trip Ending Port`, ignore.case = TRUE), 
                                      "Sand Point", 
                                      odds_dat$`Trip Ending Port`)


rm(subcat_priority, subcat_other, priority, other, factor_priority, factor_other)


# Tables -----------------------------------------------------------------------------------------------
# Summary of total observer statements per reporting category with total numbers
#  of various factors for comparisons of total effort

# Total observed for each unit type
T_summary_units <- 
  subcat_units_rate %>%
  filter(CALENDAR_YEAR == adp_yr) %>%
  group_by(OCCURRENCE_UNIT = INCIDENT_UNIT) %>%
  summarise(TOTAL_UNITS = max(TOTAL_UNITS),
            N_UNITS_REPORTED = sum(N_UNITS_REPORTED),
            .groups = "drop") %>%
  mutate(OCCURRENCE_UNIT = fct_recode(factor(OCCURRENCE_UNIT),
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
              left_join(df_obs_statements %>%
                          mutate(PERMIT = as.numeric(PERMIT)),
                        relationship = "many-to-many") %>%
              group_by(VESSEL_OR_PLANT) %>%
              summarize(TOTAL_UNITS = n_distinct(PERMIT),
                        N_UNITS_REPORTED = n_distinct(PERMIT[!is.na(OLE_OBS_STATEMENT_SEQ)])) %>%
              ungroup() %>%
              mutate(VESSEL_OR_PLANT = paste0(ifelse(VESSEL_OR_PLANT == "V", "Vessels", "Plants"), '*')) %>%
              rename(OCCURRENCE_UNIT = VESSEL_OR_PLANT)
            ) %>% 
  mutate(PERCENT_SELECTED = round((N_UNITS_REPORTED/TOTAL_UNITS)*100, 1)) %>%
  arrange(desc(TOTAL_UNITS)) %>%
  rename("Occurrence Unit" = OCCURRENCE_UNIT,
         "Total Units (#)" = TOTAL_UNITS,
         "Selected in Statements (#)" = N_UNITS_REPORTED,
         "Selected (%)" = PERCENT_SELECTED
         )
# make pretty table
T_summary_units <- autofit(flextable(T_summary_units))
T_summary_units <- bold(T_summary_units, bold = TRUE, part = "header")
T_summary_units <-
  T_summary_units %>% 
    colformat_double() %>% 
    hline(i      = ~ before(`Occurrence Unit`, "Vessels*"),
          border = fp_border_default())

T_summary_units

#Total number of Statements 
T_statement_totals <-
  # rbind(subcat_units_rate %>%
  #                filter(CALENDAR_YEAR == adp_yr) %>%
  #                group_by(CATEGORY) %>%
  #                summarize(`Statements (#)` = sum(N_STATEMENTS)) %>% 
  #         arrange(desc(`Statements (#)`)),
  #       merge(data.frame(CATEGORY = "TOTAL"),
  #             subcat_units_rate %>%
  #         filter(CALENDAR_YEAR == adp_yr) %>%
  #         summarize(`Statements (#)` = sum(N_STATEMENTS))
  #         )
  #       ) %>%
  
  # UPDATE ADK 20250409
  # need to use "df_obs_statements" because "subcat_units_rate" filters out statements 
  # that do not have any units (i.e, the units are NA).
  # Those need to be INCLUDED in high-level statement summaries.
  # they are excluded from the rate calc because NA's cause problems with division and joins.
  # but we need to 
  #   a) include them
  #   b) come up with a way to handle them
  
  # Also adding add'l columns for more data
  
  rbind(df_obs_statements %>%
          filter(FIRST_VIOL_YEAR == adp_yr) %>% 
          group_by(CATEGORY) %>%
          summarise(`Statements (#)`              = n_distinct(OLE_OBS_STATEMENT_SEQ),
                    `Regs Selected (#)`           = n_distinct(OLE_REGULATION_SEQ),
                    `Occurrences (#)`             = n_distinct(OLE_OBS_STATEMENT_UNIT_SEQ, na.rm = TRUE),
                    .groups = "drop" ) %>%
          # Adding the UNITS that were selected as well
          # Wanted to just do it as another line in the summarise statement,
          # but I couldn't figure out how to get the distinct INCIDENT UNITS to list in the summarize correctly, 
          # so I'm doing it separately and left-joining it
          # Makes the code ugly.  But oh well.
          left_join(
            df_obs_statements %>%
              filter(!is.na(OLE_OBS_STATEMENT_UNIT_SEQ), # here we DO need to filter out the NAs because we only care about the units that have value
                     FIRST_VIOL_YEAR == adp_yr) %>% 
              distinct(CATEGORY, INCIDENT_UNIT) %>%
              mutate(INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'DEPL', 'Deployments', INCIDENT_UNIT),
                     INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'TRIP', 'Trips',       INCIDENT_UNIT),
                     INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'HAUL', 'Hauls',       INCIDENT_UNIT),
                     INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'SAMP', 'Samples',     INCIDENT_UNIT),
                     INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'MARM', 'Marine Mammal Interactions', INCIDENT_UNIT),
                     INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'OFFL', 'Offloads',    INCIDENT_UNIT),
                     INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'DAYS', 'Days',        INCIDENT_UNIT),
              ) %>%
              group_by(CATEGORY) %>%
              summarise(`Occurrence Units`  = paste(INCIDENT_UNIT, collapse = ", "),
                        .groups = "drop" ) 
          ) %>% 
          arrange(desc(`Statements (#)`)),
        merge(data.frame(CATEGORY = "TOTAL"),
              df_obs_statements %>%
                filter(FIRST_VIOL_YEAR == adp_yr) %>%
                summarise(`Statements (#)`              = n_distinct(OLE_OBS_STATEMENT_SEQ),
                          `Regs Selected (#)`           = n_distinct(OLE_REGULATION_SEQ),
                          `Occurrences (#)`             = n_distinct(OLE_OBS_STATEMENT_UNIT_SEQ, na.rm = TRUE),
                          .groups = "drop" ) %>%
                cross_join(
                  df_obs_statements %>%
                    filter(!is.na(OLE_OBS_STATEMENT_UNIT_SEQ), # here it is OK to filter them out because we only care about the units that have value
                           FIRST_VIOL_YEAR == adp_yr) %>% 
                    distinct(INCIDENT_UNIT) %>%
                    mutate(INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'DEPL', 'Deployments', INCIDENT_UNIT),
                           INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'TRIP', 'Trips',       INCIDENT_UNIT),
                           INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'HAUL', 'Hauls',       INCIDENT_UNIT),
                           INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'SAMP', 'Samples',     INCIDENT_UNIT),
                           INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'MARM', 'Marine Mammal Interactions', INCIDENT_UNIT),
                           INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'OFFL', 'Offloads',    INCIDENT_UNIT),
                           INCIDENT_UNIT = ifelse(INCIDENT_UNIT == 'DAYS', 'Days',        INCIDENT_UNIT),
                    ) %>%
                    summarise(`Occurrence Units`  = paste(INCIDENT_UNIT, collapse = ", "),
                              .groups = "drop" ) 
                )
              
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
                              TRUE ~ CATEGORY)) %>%
  rename("Category" = CATEGORY)

T_statement_totals <- autofit(flextable(T_statement_totals))

# make bold headers and total
T_statement_totals <- bold(T_statement_totals, bold = TRUE, part = "header")
T_statement_totals <- bold(T_statement_totals, bold = TRUE, 
                           i= ~ Category == "Total",
                           j= ~ `Category` + `Statements (#)` + `Regs Selected (#)` + `Occurrences (#)`
                           )

#Add a line above totals https://github.com/davidgohel/flextable/issues/421
T_statement_totals <- 
  T_statement_totals %>% colformat_double() %>% 
  hline(i = ~ before(Category, "Total"), border = fp_border_default())

T_statement_totals

#ODDS
odds_table <- 
  merge(odds_dat %>% group_by(Port = `Trip Ending Port`, 
                              Issue = `Issue Category`) %>% 
          summarize(total_records = n(),
                    .groups= "drop") %>%
          pivot_wider(names_from = Issue, values_from = total_records)
        ,
        odds_dat %>% group_by(Port = `Trip Ending Port`) %>% 
          summarize(total_records = n(),
                    Cases = sum(!is.na(`Case Number`)),
                    .groups= "drop")
  ) %>% arrange(desc(total_records)) %>%
  rename(`Ending Port` = Port,
         `Records (#)` = total_records,
         `Cases (#)` = Cases) %>% 
  select(-`Records (#)`) #Decision by OLE

# add a total row for this table
odds_totals <- colSums(odds_table[sapply(odds_table, is.numeric)], na.rm = TRUE) #only numeric columns
total_row <- c(Name = "Total", odds_totals) # add a descriptor

odds_table <- rbind(odds_table, total_row) # add row to table

odds_table <- autofit(flextable(odds_table))

# make bold headers
odds_table <- bold(odds_table, bold = TRUE, part = "header")

odds_table <- bold(odds_table, bold = TRUE,  i= ~ `Ending Port` == "Total"  )

odds_table <- #Add pretty line above totals as in prior tables
  odds_table %>% colformat_double() %>% 
  hline(i = ~ before(`Ending Port`, "Total"), border = fp_border_default())

odds_table

# Create a new Word document (portrait by default)
doc <- read_docx()
# Optionally, add a heading or title
#doc <- body_add_par(doc, "Two Tables Stacked on One Page", style = "heading 1")

doc <- body_add_flextable(doc, value = T_summary_units) # Add first flextable
doc <- body_add_par(doc, "Space Added")# Add some space or another paragraph (optional)
doc <- body_add_flextable(doc, value = T_statement_totals)
doc <- body_add_par(doc, "Space Added")
doc <- body_add_flextable(doc, value = odds_table)

# Determine color breaks ------------------------------------------------------------------------------------

breaks_fxn <- function(cats, df_in, rate_x = 1) {
  # Create list to store breaks for each category
  breaks_list <- setNames(vector("list", length(cats)), cats)
  
  for (cat in cats) {

    # Subset RATE values only for this category
    x_subset <- df_in$RATE[df_in$SUPER_CAT == cat]
    
    # Determine number of unique values; Jenks requires at least two classes.
    unique_vals <- unique(x_subset)
    n_classes <- if (length(unique_vals) < 5) max(2, length(unique_vals)) else 5
    
    # Compute Jenks natural breaks with the adjusted number of classes
    ci <- classIntervals(x_subset, n = n_classes, style = "jenks")
    breaks <- ci$brks
    
    # Scale breaks by rate_x
    breaks <- breaks * rate_x
    
    # (Optional) Set the lower bound to zero
    breaks[1] <- 0
    
    # Store the breaks for this category
    breaks_list[[cat]] <- breaks
  }
  
  # Create a new column 'Label' using the category-specific breaks
  df_in$Label <- NA  # initialize
  
  for (cat in cats) {
    # Subset the rows for this category
    idx <- df_in$SUPER_CAT == cat
    
    # Get precomputed breaks for this category
    cat_breaks <- breaks_list[[cat]]
    
    # Cut the scaled RATE values into the category-specific bins
    df_in$Label[idx] <- as.character(cut(
      x = df_in$RATE[idx] * rate_x,
      breaks = cat_breaks
    ))
    # Note: intervals are of the form (lower, upper] meaning the upper bound is included.
  } 
  
  return(df_in)
}

# 1) Get unique categories from SUPER_CAT
cats <- unique(for_figures$SUPER_CAT[!grepl("_FACTOR", for_figures$SUPER_CAT)]) #Ignoring factors dfs.
for_figures <- breaks_fxn(cats, for_figures, rate_x = rate_x)

# Check results
#head(for_figures)
# You now have a 'Label' factor with intervals that depend on each
# category's own Jenks classification.

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
    legend_title  = "Rate (%)",
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
        label = ifelse(
          sprintf(numeric_format, .data[[paste0("RATE_X_", rate_x)]]) == "0.00",
          "< 0.01",
          sprintf(numeric_format, .data[[paste0("RATE_X_", rate_x)]])
        )
      ),
      color = text_color,
      size  = text_size
    ) +
    facet_nested(
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

other_plot <- plot_format_fxn(df = for_figures %>% filter(SUPER_CAT == "OTHER"), rate_x = rate_x, start_color = start_color, end_color = end_color)

# Detailed dive into high rates with factors ---------------------------------------------------------------------------
#For factors, we want to use coverage type, FMP, and Vessel Type.
super_levels <- data.frame(SUPER_CAT = c("PRIORITY", "OTHER"))
super_levels$SUPER_FACT = paste0(super_levels$SUPER_CAT, "_FACTORS")

#Lets get the super group
for(i in 1:nrow(super_levels)){
#TESTING i <- 1  
df_super <- for_figures %>% filter(SUPER_CAT == super_levels$SUPER_CAT[i])
df_super$Label <- factor(df_super$Label, levels = sort(unique(df_super$Label)))
# This ensures the lowest interval is first in levels(), and the highest is last.

# 2) Grab only rows that have the highest (last) factor level
highest_level <- tail(levels(df_super$Label), 1)             # e.g. "(2,3]"
df_highest    <- subset(df_super, Label == highest_level)

#Now we grab the factors data
df <- for_figures %>% filter(SUPER_CAT == super_levels$SUPER_FACT[i])

#Make a SASH df for adding in later
df_SASH <- df %>% filter(SUBCATEGORY %in% c("SEXUAL HARASSMENT", "SEXUAL ASSAULT"))

#Now limit df to just those categories
df <- merge(df, 
            df_highest %>% select(CATEGORY, SUBCATEGORY, INCIDENT_UNIT) %>% distinct())
df <- rbind(df, df_SASH)

if(i == 1){df_out <- df}
if(i > 1){df_out <- rbind(df_out, df)}
}

cats <- super_levels$SUPER_FACT
for_factor_figures <- suppressWarnings(breaks_fxn(cats, df_out, rate_x = rate_x))

#TODO - cleanup

#Make plot

for(i in 1:length(cats)){
factor_plot <- plot_format_fxn(df = for_factor_figures %>% filter(SUPER_CAT == cats[i]), 
                               rate_x = rate_x, 
                               start_color = start_color, 
                               end_color = end_color,
                               facet_formula = NMFS_REGION ~ COVERAGE_TYPE + VESSEL_TYPE + INCIDENT_UNIT
                               )
#Give it a name
assign(paste0(tolower(gsub("-", "_", super_levels$SUPER_FACT[i])), "_plot"), factor_plot)
}

#cleanup
rm(df, df_SASH, i, df_super, df_out, df_highest, super_levels)

# Save Output -------------------------------------------------------------
ggsave("priority_plot.pdf", plot = priority_plot, width = 12, height = 6, units = "in", path = "Plots/")
ggsave("other_plot.pdf", plot = other_plot, width = 12, height = 6, units = "in", path = "Plots/")
ggsave("priority_factors_plot.pdf", plot = priority_factors_plot, width = 12, height = 6, units = "in", path = "Plots/")
ggsave("other_factors_plot.pdf", plot = other_factors_plot, width = 6, height = 6, units = "in", path = "Plots/")

#Save the Word document
print(doc, target = "tables/tables.docx")

save.image(file = "AR_4_summary_tables_output.Rdata")
# 
gdrive_upload("AR_4_summary_tables_output.Rdata", AnnRpt_EnfChp_dribble)
