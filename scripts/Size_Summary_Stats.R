# functions to calculate summary statistics from LME_OP_CHinook.R script output. 

results <- readRDS("output/sizeatage_LME_results.RDS")

calculate_size_changes <- function(results_list, reference_year = 1990) {
  
  summary_list <- list()
  
  for(age_name in names(results_list$models)) {
    
    model_obj <- results_list$models[[age_name]]
    ocean_age <- model_obj$ocean_age
    
    # Extract year effects
    year_effects <- extract_year_effects(model_obj)
    
    # Get first and last years
    first_year <- min(year_effects$year)
    last_year <- max(year_effects$year)
    
    # Calculate average effect for first 5 years
    first_5_years <- head(sort(unique(year_effects$year)), 5)
    first_5_avg <- mean(year_effects$effect[year_effects$year %in% first_5_years], na.rm = TRUE)
    
    # Calculate average effect for last 5 years
    last_5_years <- tail(sort(unique(year_effects$year)), 5)
    last_5_avg <- mean(year_effects$effect[year_effects$year %in% last_5_years], na.rm = TRUE)
    
    # Change from first 5 to last 5 years average
    total_change <- last_5_avg - first_5_avg
    
    # Change per decade
    n_years <- last_year - first_year
    change_per_decade <- (total_change / n_years) * 10
    
    # Change from reference year to last year
    if(reference_year %in% year_effects$year) {
      ref_effect <- year_effects$effect[year_effects$year == reference_year]
      last_effect <- year_effects$effect[year_effects$year == last_year]
      change_from_ref <- last_effect - ref_effect
      years_from_ref <- last_year - reference_year
    } else {
      change_from_ref <- NA
      years_from_ref <- NA
    }
    
    # Store results
    summary_list[[age_name]] <- data.frame(
      ocean_age = ocean_age,
      first_year = first_year,
      last_year = last_year,
      first_5_years = paste(first_5_years, collapse = ", "),
      last_5_years = paste(last_5_years, collapse = ", "),
      total_change_mm = round(total_change, 1),
      change_per_decade_mm = round(change_per_decade, 1),
      reference_year = reference_year,
      change_from_reference_mm = round(change_from_ref, 1),
      years_from_reference = years_from_ref
    )
  }
  
  # Combine into single data frame
  summary_df <- bind_rows(summary_list)
  
  # Print summary statements
  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║  SIZE CHANGE SUMMARY                                        ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
  
  for(i in 1:nrow(summary_df)) {
    age <- summary_df$ocean_age[i]
    
    cat("OCEAN AGE", age, "\n")
    cat("──────────────────────────────────────────────────────────\n")
    cat("  Time period:", summary_df$first_year[i], "to", summary_df$last_year[i], "\n")
    cat("  First 5 years:", summary_df$first_5_years[i], "\n")
    cat("  Last 5 years:", summary_df$last_5_years[i], "\n")
    cat("  Total change (avg of first 5 vs last 5):", summary_df$total_change_mm[i], "mm\n")
    cat("  Rate of change:", summary_df$change_per_decade_mm[i], "mm per decade\n\n")
    
    if(!is.na(summary_df$change_from_reference_mm[i])) {
      cat("  COMPARISON TO", reference_year, ":\n")
      cat("  An average ocean age", age, "fish caught in", 
          summary_df$last_year[i], "is\n")
      cat("  ", abs(summary_df$change_from_reference_mm[i]), "mm",
          ifelse(summary_df$change_from_reference_mm[i] < 0, "SHORTER", "LONGER"),
          "than an average fish caught in", reference_year, "\n\n")
    }
    
    cat("\n")
  }
  
  return(summary_df)
}

# Usage:
size_changes <- calculate_size_changes(results, reference_year = 1990)

# Save to CSV
write.csv(size_changes, "output/size_change_summary.csv", row.names = FALSE)
