# functions to calculate summary statistics from LME_OP_CHinook.R script output. 

results <- readRDS("output/sizeatage_LME_results.RDS")

calculate_size_changes <- function(results_list, reference_year = 1990) {
  
  summary_list <- list()
  
  for(age_name in names(results_list$models)) {
    
    model_obj <- results_list$models[[age_name]]
    ocean_age <- model_obj$ocean_age
    
    # Extract year effects (these are brood years)
    year_effects <- extract_year_effects(model_obj)
    
    # Convert brood year to catch year
    year_effects$catch_year <- year_effects$year + 1 + ocean_age
    
    # Get first and last catch years
    first_catch_year <- min(year_effects$catch_year)
    last_catch_year <- max(year_effects$catch_year)
    
    # Calculate average effect for first 5 catch years
    first_5_catch_years <- head(sort(unique(year_effects$catch_year)), 5)
    first_5_avg <- mean(year_effects$effect[year_effects$catch_year %in% first_5_catch_years], na.rm = TRUE)
    
    # Calculate average effect for last 5 catch years
    last_5_catch_years <- tail(sort(unique(year_effects$catch_year)), 5)
    last_5_avg <- mean(year_effects$effect[year_effects$catch_year %in% last_5_catch_years], na.rm = TRUE)
    
    # Change from first 5 to last 5 years average
    total_change <- last_5_avg - first_5_avg
    
    # Change per decade
    n_years <- last_catch_year - first_catch_year
    change_per_decade <- (total_change / n_years) * 10
    
    # Change from reference catch year to last catch year
    if(reference_year %in% year_effects$catch_year) {
      ref_effect <- year_effects$effect[year_effects$catch_year == reference_year]
      last_effect <- year_effects$effect[year_effects$catch_year == last_catch_year]
      change_from_ref <- last_effect - ref_effect
      years_from_ref <- last_catch_year - reference_year
    } else {
      change_from_ref <- NA
      years_from_ref <- NA
    }
    
    # Store results
    summary_list[[age_name]] <- data.frame(
      ocean_age = ocean_age,
      first_catch_year = first_catch_year,
      last_catch_year = last_catch_year,
      first_5_catch_years = paste(first_5_catch_years, collapse = ", "),
      last_5_catch_years = paste(last_5_catch_years, collapse = ", "),
      total_change_mm = round(total_change, 1),
      change_per_decade_mm = round(change_per_decade, 1),
      reference_catch_year = reference_year,
      change_from_reference_mm = round(change_from_ref, 1),
      years_from_reference = years_from_ref
    )
  }
  
  # Combine into single data frame
  summary_df <- bind_rows(summary_list)
  
  # Calculate average change across all age classes
  avg_total_change <- mean(summary_df$total_change_mm, na.rm = TRUE)
  avg_change_per_decade <- mean(summary_df$change_per_decade_mm, na.rm = TRUE)
  avg_change_from_ref <- mean(summary_df$change_from_reference_mm, na.rm = TRUE)
  
  # Print summary statements
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  SIZE CHANGE SUMMARY (BY CATCH YEAR)                        ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  for(i in 1:nrow(summary_df)) {
    age <- summary_df$ocean_age[i]
    
    cat("OCEAN AGE", age, "\n")
    cat("──────────────────────────────────────────────────────────\n")
    cat("  Catch year range:", summary_df$first_catch_year[i], "to", summary_df$last_catch_year[i], "\n")
    cat("  First 5 catch years:", summary_df$first_5_catch_years[i], "\n")
    cat("  Last 5 catch years:", summary_df$last_5_catch_years[i], "\n")
    cat("  Total change (avg of first 5 vs last 5):", summary_df$total_change_mm[i], "mm\n")
    cat("  Rate of change:", summary_df$change_per_decade_mm[i], "mm per decade\n\n")
    
    if(!is.na(summary_df$change_from_reference_mm[i])) {
      cat("  COMPARISON TO CATCH YEAR", reference_year, ":\n")
      cat("  An average ocean age", age, "fish caught in", 
          summary_df$last_catch_year[i], "is\n")
      cat("  ", abs(summary_df$change_from_reference_mm[i]), "mm",
          ifelse(summary_df$change_from_reference_mm[i] < 0, "SHORTER", "LONGER"),
          "than an average fish caught in", reference_year, "\n\n")
    }
    
    cat("\n")
  }
  
  # Print overall average across age classes
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║  AVERAGE ACROSS ALL AGE CLASSES                             ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  cat("  Average total change (first 5 vs last 5 catch years):", round(avg_total_change, 1), "mm\n")
  cat("  Average rate of change:", round(avg_change_per_decade, 1), "mm per decade\n")
  if(!is.na(avg_change_from_ref)) {
    cat("  Average change from catch year", reference_year, ":", round(avg_change_from_ref, 1), "mm\n")
  }
  cat("\n")
  
  # Add overall averages to the returned data frame as attributes
  attr(summary_df, "avg_total_change") <- avg_total_change
  attr(summary_df, "avg_change_per_decade") <- avg_change_per_decade
  attr(summary_df, "avg_change_from_reference") <- avg_change_from_ref
  
  return(summary_df)
}

# Usage:
size_changes <- calculate_size_changes(results, reference_year = 1990)

# Save to CSV
write.csv(size_changes, "output/size_change_summary.csv", row.names = FALSE)

# Optionally, create a separate summary file with the overall averages
overall_summary <- data.frame(
  metric = c("Average total change (mm)", 
             "Average change per decade (mm)", 
             "Average change from reference catch year (mm)"),
  value = c(attr(size_changes, "avg_total_change"),
            attr(size_changes, "avg_change_per_decade"),
            attr(size_changes, "avg_change_from_reference"))
)
write.csv(overall_summary, "output/overall_average_summary.csv", row.names = FALSE)