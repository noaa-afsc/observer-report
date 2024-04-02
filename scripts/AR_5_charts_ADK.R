

# Define GG's ----------------------------------------

## Heatmap definitions ----------------------------------------

### Heatmap theme elements  ----------------------------------------
fn_heatmap_theme <- function(){ 
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust = 1))
}


### Heatmap 1000 days gg-----------------------------------------
fn_heatmap_1000_gg <-
  function(ggdf, ggcategory, ggtitle){
    ggplot(data = ggdf, 
           aes(x = '', 
               y = STATEMENT_TYPE)) +
      geom_tile(aes(fill  = INCIDENTS_PER_1000_DEPLOYED_DAYS)) +
      geom_text(aes(label = round(INCIDENTS_PER_1000_DEPLOYED_DAYS, 1)),
                fontface = "bold") +
      facet_nested(OLD_OLE_CATEGORY ~ CALENDAR_YEAR + OLE_SYSTEM,
                   scales = "free_y") +
      scale_fill_gradient(low  = "blue",
                          high = "yellow",
                          name =  paste("Occurrences",  
                                        "Per 1000", 
                                        "Deployed",
                                        "Days", sep = "\n"))  +
      labs(x = '', 
           y = "", # "Statement Type", 
           title = ggtitle) +
      fn_heatmap_theme() 
  }






fn_heatmap_assnmt_gg <-
  function(ggdf, ggcategory, ggtitle){
    ggplot(data = ggdf, 
           aes(x = '', 
               y = STATEMENT_TYPE)) +
      geom_tile(aes(fill  = INCIDENTS_PER_ASSIGNMENT)) +
      geom_text(aes(label = round(INCIDENTS_PER_ASSIGNMENT, 2)),
                fontface = "bold") +
      facet_nested(OLD_OLE_CATEGORY ~ CALENDAR_YEAR + OLE_SYSTEM,
                   scales = "free_y") +
      scale_fill_gradient(low  = "blue",
                          high = "yellow",
                          name =  paste("Occurrences", 
                                        "Per Vessel/Plant", 
                                        "Assignment", sep = "\n")) +
      labs(x = '', 
           y = "", # "Statement Type", 
           title = ggtitle) +
      fn_heatmap_theme() 
  }





fn_dist_plot_1000_gg <-
  function(ggdf, ggcategory, ggtitle){
    ggplot(data = rate_by_subcat_cruise_priority, 
           aes(x = STATEMENT_TYPE, 
               y = OCCURRENCES_PER_ASSIGNMENT
               )
           ) +
      geom_boxplot() +
      facet_nested(OLD_OLE_CATEGORY ~ CALENDAR_YEAR + OLE_SYSTEM,
                   scales = "free") +
      labs(x = '', 
           y ="Occurrences per Vessel/Plant Assignment", 
           title = ggtitle) +
      fn_heatmap_theme() 
  }






## Plot them----------------------------------------

### Incidents Per 1000 Deployed Days ----------------------------------------
# incis_per_1000_days_priority <-
  fn_heatmap_1000_gg(
    ggdf = rate_by_subcat_priority,
    ggcategory = '',
    ggtitle    = "OLE PRIORITY Statement Category Groups"
  )

### Incidents Per assignment ----------------------------------------
# incis_per_1000_days_priority <-
fn_heatmap_assnmt_gg(
  ggdf = rate_by_subcat_priority,
  ggcategory = '',
  ggtitle    = "OLE PRIORITY Statement Category Groups"
)

### Boxplot Incidents Per Deployed Day ----------------------------------------
# incis_per_1000_days_priority <-
fn_dist_plot_1000_gg(
  ggcategory = 'OLE PRIORITY: SAFETY AND DUTIES',
  ggtitle    = "OLE PRIORITY Statement Category Groups"
)
