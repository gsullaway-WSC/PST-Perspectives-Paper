# Supplementary Utilities for Chinook LME Analysis
# Additional functions for data checking, validation, and reporting

library(tidyverse)
library(nlme)

# ============================================================================
# DATA VALIDATION AND EXPLORATION
# ============================================================================

check_data_structure <- function(df) {
  
  cat("========================================\n")
  cat("DATA STRUCTURE CHECK\n")
  cat("========================================\n\n")
  
  cat("Dimensions:", nrow(df), "rows x", ncol(df), "columns\n\n")
  
  # Check required columns
  required_cols <- c("length", "brood_year", "run_year", "fishery", 
                     "run", "sex", "recovery_date")
  
  missing_cols <- setdiff(required_cols, names(df))
  if(length(missing_cols) > 0) {
    cat("WARNING: Missing required columns:\n")
    print(missing_cols)
  } else {
    cat("✓ All basic required columns present\n")
  }
  
  # Check for length data
  cat("\nLength data:\n")
  cat("  Non-missing:", sum(!is.na(df$length)), "\n")
  cat("  Missing:", sum(is.na(df$length)), "\n")
  cat("  Range:", min(df$length, na.rm = TRUE), "-", 
      max(df$length, na.rm = TRUE), "mm\n")
  
  # Check brood years
  cat("\nBrood year range:", min(df$brood_year, na.rm = TRUE), "-", 
      max(df$brood_year, na.rm = TRUE), "\n")
  
  # Check for ocean age calculation needs
  cat("\nAge-related columns:\n")
  cat("  Total age (run_year - brood_year) range:", 
      min(df$run_year - df$brood_year, na.rm = TRUE), "-",
      max(df$run_year - df$brood_year, na.rm = TRUE), "\n")
  
  if("freshwater_age" %in% names(df)) {
    cat("  Freshwater age present:", sum(!is.na(df$freshwater_age)), "records\n")
  } else {
    cat("  WARNING: No freshwater_age column found\n")
    cat("  You'll need to add this to calculate ocean age\n")
  }
  
  # Check rearing type indicators
  cat("\nRearing type indicators:\n")
  if("hatchery_location_code" %in% names(df)) {
    cat("  Hatchery records:", sum(!is.na(df$hatchery_location_code)), "\n")
  }
  if("hatchery_location_name" %in% names(df)) {
    cat("  Unique hatcheries:", 
        length(unique(df$hatchery_location_name[!is.na(df$hatchery_location_name)])), "\n")
  }
  
  # Fishery codes
  cat("\nFishery codes:\n")
  cat("  Unique fisheries:", length(unique(df$fishery[!is.na(df$fishery)])), "\n")
  
  # Run types
  cat("\nRun types:\n")
  print(table(df$run, useNA = "ifany"))
  
  # Sex
  cat("\nSex distribution:\n")
  print(table(df$sex, useNA = "ifany"))
  
  cat("\n========================================\n")
}

# ============================================================================
# CREATE SAMPLE SIZE TABLES
# ============================================================================

create_sample_size_table <- function(df_prepared) {
  
  # Sample sizes by ocean age and brood year
  sample_table <- df_prepared %>%
    group_by(ocean_age, brood_year, rearing_type) %>%
    summarise(n = n(), .groups = 'drop') %>%
    pivot_wider(names_from = rearing_type, values_from = n, values_fill = 0)
  
  cat("\nSample sizes by ocean age, brood year, and rearing type:\n")
  print(sample_table)
  
  # Total by ocean age
  age_totals <- df_prepared %>%
    group_by(ocean_age) %>%
    summarise(
      total = n(),
      hatchery = sum(rearing_type == "hatchery"),
      wild = sum(rearing_type == "wild")
    )
  
  cat("\nTotal sample sizes by ocean age:\n")
  print(age_totals)
  
  return(list(
    by_year = sample_table,
    by_age = age_totals
  ))
}

# ============================================================================
# VISUALIZE RAW DATA
# ============================================================================

plot_raw_data <- function(df_prepared, max_age = 5) {
  
  # Filter to available ocean ages
  df_plot <- df_prepared %>%
    filter(ocean_age <= max_age, ocean_age >= 1)
  
  # Plot 1: Length by brood year and ocean age
  p1 <- ggplot(df_plot, aes(x = brood_year, y = length, color = factor(ocean_age))) +
    geom_point(alpha = 0.1, size = 0.5) +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(~ocean_age, scales = "free_y", ncol = 2) +
    labs(
      title = "Length-at-age trends over time",
      x = "Brood Year",
      y = "Length (mm)",
      color = "Ocean Age"
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  # Plot 2: Hatchery vs Wild
  p2 <- ggplot(df_plot, aes(x = brood_year, y = length, color = rearing_type)) +
    geom_point(alpha = 0.1, size = 0.5) +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(~ocean_age, scales = "free_y", ncol = 2) +
    labs(
      title = "Length-at-age trends: Hatchery vs Wild",
      x = "Brood Year",
      y = "Length (mm)",
      color = "Rearing Type"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # Plot 3: Sample sizes over time
  sample_counts <- df_plot %>%
    group_by(brood_year, ocean_age) %>%
    summarise(n = n(), .groups = 'drop')
  
  p3 <- ggplot(sample_counts, aes(x = brood_year, y = n, fill = factor(ocean_age))) +
    geom_col() +
    facet_wrap(~ocean_age, scales = "free_y", ncol = 2) +
    labs(
      title = "Sample sizes by brood year and ocean age",
      x = "Brood Year",
      y = "Number of observations",
      fill = "Ocean Age"
    ) +
    theme_bw() +
    theme(legend.position = "none")
  
  return(list(p1 = p1, p2 = p2, p3 = p3))
}

# ============================================================================
# EXTRACT MODEL COEFFICIENTS TO TABLE
# ============================================================================

extract_model_summary <- function(model_obj, file_name = NULL) {
  
  # Get summary
  model_summary <- summary(model_obj$model)
  
  # Extract fixed effects
  fixed_df <- as.data.frame(model_summary$tTable)
  fixed_df$variable <- rownames(fixed_df)
  fixed_df$ocean_age <- model_obj$ocean_age
  
  # Reorder columns
  fixed_df <- fixed_df %>%
    select(ocean_age, variable, Value, Std.Error, DF, `t-value`, `p-value`) %>%
    arrange(`p-value`)
  
  # Print significant effects
  cat("\nSignificant fixed effects (p < 0.05) for Ocean Age", model_obj$ocean_age, ":\n")
  sig_effects <- fixed_df %>%
    filter(`p-value` < 0.05)
  print(sig_effects)
  
  # Save to file if requested
  if(!is.null(file_name)) {
    write.csv(fixed_df, file_name, row.names = FALSE)
    cat("\nFull results saved to:", file_name, "\n")
  }
  
  return(fixed_df)
}

# ============================================================================
# PREDICT SIZE-AT-AGE TRENDS
# ============================================================================

predict_size_trends <- function(model_obj, 
                                year_range = NULL,
                                rearing_type = "hatchery") {
  
  if(is.null(year_range)) {
    year_range <- range(as.numeric(as.character(model_obj$data$year)))
  }
  
  # Create prediction data frame with reference levels
  # Get most common levels for factors
  ref_fishery <- names(sort(table(model_obj$data$fishery), decreasing = TRUE))[1]
  ref_fw_age <- names(sort(table(model_obj$data$freshwater_age), decreasing = TRUE))[1]
  ref_run <- names(sort(table(model_obj$data$run_type), decreasing = TRUE))[1]
  ref_hatchery <- names(sort(table(model_obj$data$hatchery_name), decreasing = TRUE))[1]
  
  newdata <- expand.grid(
    year = factor(seq(year_range[1], year_range[2], by = 1)),
    rearing_type = factor(rearing_type),
    fishery = factor(ref_fishery),
    freshwater_age = factor(ref_fw_age),
    run_type = factor(ref_run),
    sex_clean = factor("unknown"),
    day_of_year = mean(model_obj$data$day_of_year, na.rm = TRUE),
    hatchery_name = factor(ref_hatchery)
  )
  
  # Make predictions
  newdata$predicted_length <- predict(model_obj$model, 
                                       newdata = newdata, 
                                       level = 0)
  
  newdata$ocean_age <- model_obj$ocean_age
  
  return(newdata)
}

plot_predicted_trends <- function(predictions_list) {
  
  all_preds <- bind_rows(predictions_list)
  
  all_preds$year_numeric <- as.numeric(as.character(all_preds$year))
  
  ggplot(all_preds, aes(x = year_numeric, y = predicted_length, 
                        color = factor(ocean_age))) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = "Predicted size-at-age trends",
      subtitle = paste("Reference: Hatchery fish, most common fishery/run type"),
      x = "Brood Year",
      y = "Predicted Length (mm)",
      color = "Ocean Age"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
}

# ============================================================================
# ESCAPEMENT-ONLY ANALYSIS
# ============================================================================

run_escapement_analysis <- function(hatchery_data, 
                                   escapement_fishery_codes,
                                   ocean_ages = 1:5) {
  
  cat("\n========================================\n")
  cat("ESCAPEMENT-ONLY ANALYSIS\n")
  cat("========================================\n\n")
  
  # Filter to escapement data only
  escapement_data <- hatchery_data %>%
    filter(fishery %in% escapement_fishery_codes)
  
  cat("Original data:", nrow(hatchery_data), "rows\n")
  cat("Escapement data:", nrow(escapement_data), "rows\n")
  cat("Proportion escapement:", 
      round(nrow(escapement_data) / nrow(hatchery_data) * 100, 1), "%\n\n")
  
  # Run analysis on escapement data
  results_escapement <- run_full_analysis(escapement_data, ocean_ages)
  
  return(results_escapement)
}

# ============================================================================
# EXPORT RESULTS TO EXCEL
# ============================================================================

export_results_to_excel <- function(results, filename = "lme_results.xlsx") {
  
  require(openxlsx)
  
  wb <- createWorkbook()
  
  # Sheet 1: Model comparison
  comparison_df <- data.frame(
    ocean_age = sapply(results$models, function(x) x$ocean_age),
    n_obs = sapply(results$models, function(x) x$n_obs),
    AIC = sapply(results$models, function(x) AIC(x$model)),
    BIC = sapply(results$models, function(x) BIC(x$model))
  )
  
  addWorksheet(wb, "Model_Comparison")
  writeData(wb, "Model_Comparison", comparison_df)
  
  # Sheets 2-6: Fixed effects for each age
  for(age_name in names(results$models)) {
    model_obj <- results$models[[age_name]]
    fixed_effects <- extract_model_summary(model_obj)
    
    sheet_name <- paste0("Age_", model_obj$ocean_age, "_Effects")
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, fixed_effects)
  }
  
  saveWorkbook(wb, filename, overwrite = TRUE)
  cat("\nResults exported to:", filename, "\n")
}

cat("\n========================================\n")
cat("Supplementary utilities loaded!\n")
cat("========================================\n")
