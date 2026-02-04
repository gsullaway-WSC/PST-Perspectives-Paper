# functions to calcualte summary statistics from LME_OP_CHinook.R script output. 

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
    
    # Change from first to last year
    first_effect <- year_effects$effect[year_effects$year == first_year]
    last_effect <- year_effects$effect[year_effects$year == last_year]
    total_change <- last_effect - first_effect
    
    # Change per decade
    n_years <- last_year - first_year
    change_per_decade <- (total_change / n_years) * 10
    
    # Change from reference year to last year
    if(reference_year %in% year_effects$year) {
      ref_effect <- year_effects$effect[year_effects$year == reference_year]
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
    total_age <- age + 1  # Approximate total age (ocean age + 1 year freshwater)
    
    cat("OCEAN AGE", age, "(~", total_age, "years old total):\n")
    cat("──────────────────────────────────────────────────────────\n")
    cat("  Time period:", summary_df$first_year[i], "to", summary_df$last_year[i], "\n")
    cat("  Total change:", summary_df$total_change_mm[i], "mm\n")
    cat("  Rate of change:", summary_df$change_per_decade_mm[i], "mm per decade\n\n")
    
    if(!is.na(summary_df$change_from_reference_mm[i])) {
      cat("  COMPARISON TO", reference_year, ":\n")
      cat("  An average", total_age, "year old fish caught in", 
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

# View as table
print(size_changes)

# Save to CSV
write.csv(size_changes, "size_change_summary.csv", row.names = FALSE)
 
# 
# This will output something like:
#   ```
# OCEAN AGE 3 (~ 4 years old total):
#   ──────────────────────────────────────────────────────────
# Time period: 1975 to 2020
# Total change: -89.3 mm
# Rate of change: -19.8 mm per decade
# 
# COMPARISON TO 1990:
#   An average 4 year old fish caught in 2020 is
# 45.2 mm SHORTER than an average fish caught in 1990