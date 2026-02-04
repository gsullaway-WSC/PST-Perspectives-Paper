# Chinook Salmon Size-at-Age Analysis
# Recreating Ohlberger et al. 2018 LME approach for WA coast data
# Author: Analysis Script
# Date: 2026-02-03

# Load required packages
library(tidyverse)
library(nlme)
library(MuMIn)
library(lme4)

# ============================================================================
# 1. DATA PREPARATION
# ============================================================================

# Assuming your data is loaded as 'hatchery'
# You'll need to prepare the data with the following steps:

prepare_data <- function(df) {
  
  df_clean <- df %>%
    # Calculate ocean age (total age - freshwater age)
    mutate(
      # Convert recovery_date to Date format if needed
      recovery_date = as.Date(as.character(recovery_date), format = "%Y%m%d"),
      
      # Extract day of year
      day_of_year = as.numeric(format(recovery_date, "%j")),
      
      # Calculate total age and ocean age
      # You'll need to adjust this based on your data structure
      # Assuming you have age information in your dataset
      total_age = run_year - brood_year,
      ocean_age = total_age - freshwater_age,  # You'll need freshwater_age column
      
      # Create rearing type factor (hatchery vs wild)
      # Adjust this based on how your data indicates rearing type
      rearing_type = factor(ifelse(is.na(hatchery_location_code), "wild", "hatchery")),
      
      # Convert factors
      run_type = factor(run),
      sex_clean = factor(case_when(
        sex %in% c("M", "m") ~ "male",
        sex %in% c("F", "f") ~ "female",
        TRUE ~ "unknown"
      )),
      fishery = factor(fishery),
      freshwater_age = factor(freshwater_age),
      year = factor(brood_year),
      hatchery_name = factor(hatchery_location_name),  # Added as per your request
      
      # Population identifier (you may need to create this)
      # This could be based on release location or hatchery
      population = factor(coalesce(release_location_code, hatchery_location_code))
    ) %>%
    # Remove rows with missing length (response variable)
    filter(!is.na(length)) %>%
    # Remove rows with missing key variables
    filter(!is.na(ocean_age), !is.na(brood_year))
  
  return(df_clean)
}

# ============================================================================
# 2. FILTER DATA BY MINIMUM OBSERVATIONS
# ============================================================================

filter_by_min_observations <- function(df, min_obs = 25) {
  # Count observations per category for each factor
  # Remove factor levels with < min_obs observations
  
  df_filtered <- df %>%
    group_by(fishery) %>%
    filter(n() >= min_obs) %>%
    ungroup() %>%
    group_by(run_type) %>%
    filter(n() >= min_obs) %>%
    ungroup() %>%
    group_by(freshwater_age) %>%
    filter(n() >= min_obs) %>%
    ungroup() %>%
    group_by(hatchery_name) %>%
    filter(n() >= min_obs) %>%
    ungroup() %>%
    # Drop unused factor levels
    mutate(
      fishery = fct_drop(fishery),
      run_type = fct_drop(run_type),
      freshwater_age = fct_drop(freshwater_age),
      hatchery_name = fct_drop(hatchery_name)
    )
  
  return(df_filtered)
}

# ============================================================================
# 3. FIT LME MODELS FOR EACH OCEAN AGE
# ============================================================================

fit_lme_by_age <- function(df, ocean_age_value, include_sex = TRUE) {
  
  cat("\n========================================\n")
  cat("Fitting models for Ocean Age", ocean_age_value, "\n")
  cat("========================================\n")
  
  # Filter data for specific ocean age
  df_age <- df %>%
    filter(ocean_age == ocean_age_value) %>%
    filter_by_min_observations(min_obs = 25)
  
  cat("Sample size:", nrow(df_age), "\n")
  
  if(nrow(df_age) < 100) {
    cat("Insufficient data for ocean age", ocean_age_value, "\n")
    return(NULL)
  }
  
  # Build formula
  if(include_sex) {
    fixed_formula <- "length ~ year * rearing_type + fishery + freshwater_age + run_type + sex_clean + day_of_year + hatchery_name"
  } else {
    fixed_formula <- "length ~ year * rearing_type + fishery + freshwater_age + run_type + day_of_year + hatchery_name"
  }
  
  # Test for random effects structure
  cat("\nTesting random effects structure...\n")
  
  # Model 1: No random effects
  m0 <- gls(as.formula(fixed_formula), 
            data = df_age,
            method = "REML",
            na.action = na.omit)
  
  # Model 2: Random intercept for year
  m1 <- tryCatch({
    lme(as.formula(fixed_formula),
        random = ~1|year,
        data = df_age,
        method = "REML",
        na.action = na.omit)
  }, error = function(e) NULL)
  
  # Model 3: Random intercept for population
  m2 <- tryCatch({
    lme(as.formula(fixed_formula),
        random = ~1|population,
        data = df_age,
        method = "REML",
        na.action = na.omit)
  }, error = function(e) NULL)
  
  # Model 4: Year nested within population (as in Ohlberger)
  m3 <- tryCatch({
    lme(as.formula(fixed_formula),
        random = ~1|population/year,
        data = df_age,
        method = "REML",
        na.action = na.omit)
  }, error = function(e) NULL)
  
  # Compare models using AIC
  models_list <- list(no_random = m0)
  if(!is.null(m1)) models_list$year_random <- m1
  if(!is.null(m2)) models_list$pop_random <- m2
  if(!is.null(m3)) models_list$nested_random <- m3
  
  aic_comparison <- sapply(models_list, AIC)
  cat("\nRandom effects comparison (AIC):\n")
  print(sort(aic_comparison))
  
  # Select best random effects structure
  best_model <- models_list[[which.min(aic_comparison)]]
  
  # Test for variance structure
  cat("\nTesting variance structures...\n")
  
  # Test weights by year
  m_var_year <- tryCatch({
    update(best_model, weights = varIdent(form = ~1|year))
  }, error = function(e) NULL)
  
  # Test weights by population
  m_var_pop <- tryCatch({
    update(best_model, weights = varIdent(form = ~1|population))
  }, error = function(e) NULL)
  
  var_models <- list(base = best_model)
  if(!is.null(m_var_year)) var_models$var_year <- m_var_year
  if(!is.null(m_var_pop)) var_models$var_pop <- m_var_pop
  
  aic_var <- sapply(var_models, AIC)
  cat("\nVariance structure comparison (AIC):\n")
  print(sort(aic_var))
  
  # Select best variance structure
  final_base_model <- var_models[[which.min(aic_var)]]
  
  return(list(
    model = final_base_model,
    data = df_age,
    ocean_age = ocean_age_value,
    n_obs = nrow(df_age)
  ))
}

# ============================================================================
# 4. MODEL SELECTION USING MUMIN
# ============================================================================

run_model_selection <- function(model_obj) {
  
  cat("\n========================================\n")
  cat("Running model selection for Ocean Age", model_obj$ocean_age, "\n")
  cat("========================================\n")
  
  # Refit with ML for model selection
  model_ml <- update(model_obj$model, method = "ML")
  
  # Set na.action for MuMIn
  options(na.action = "na.fail")
  
  # Generate all possible models
  cat("\nGenerating candidate model set...\n")
  cat("Warning: This may take a while for complex models\n")
  
  dredge_result <- tryCatch({
    dredge(model_ml, rank = "AIC", extra = c("R^2"))
  }, error = function(e) {
    cat("Error in dredge:", e$message, "\n")
    return(NULL)
  })
  
  options(na.action = "na.omit")
  
  if(is.null(dredge_result)) {
    return(NULL)
  }
  
  # Get top models (delta AIC < 2)
  top_models <- subset(dredge_result, delta < 2)
  
  cat("\nTop models (delta AIC < 2):\n")
  print(top_models)
  
  # Model averaging
  avg_model <- model.avg(dredge_result, subset = delta < 2)
  
  cat("\nModel-averaged parameters:\n")
  print(summary(avg_model))
  
  return(list(
    dredge_result = dredge_result,
    top_models = top_models,
    avg_model = avg_model
  ))
}

# ============================================================================
# 5. EXTRACT AND VISUALIZE YEAR EFFECTS
# ============================================================================

extract_year_effects <- function(model_obj) {
  
  # Get fixed effects
  fixed_eff <- fixef(model_obj$model)
  
  # Extract year coefficients
  year_coefs <- fixed_eff[grep("^year", names(fixed_eff))]
  
  # Extract interaction coefficients if present
  interaction_coefs <- fixed_eff[grep("year.*:.*rearing_type", names(fixed_eff))]
  
  # Create data frame for plotting
  year_effects_df <- data.frame(
    year = as.numeric(gsub("year", "", names(year_coefs))),
    effect = as.numeric(year_coefs),
    ocean_age = model_obj$ocean_age
  )
  
  return(year_effects_df)
}

plot_year_trends <- function(year_effects_list) {
  
  # Combine year effects from all ages
  all_effects <- bind_rows(year_effects_list)
  
  ggplot(all_effects, aes(x = year, y = effect, color = factor(ocean_age))) +
    geom_line(size = 1) +
    geom_point() +
    labs(
      title = "Temporal trends in size-at-age",
      subtitle = "Year effects from LME models",
      x = "Brood Year",
      y = "Year effect on length (mm)",
      color = "Ocean Age"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
}

# ============================================================================
# 6. MODEL DIAGNOSTICS
# ============================================================================

model_diagnostics <- function(model_obj) {
  
  cat("\n========================================\n")
  cat("Model Diagnostics for Ocean Age", model_obj$ocean_age, "\n")
  cat("========================================\n")
  
  # Residual plots
  par(mfrow = c(2, 2))
  
  # Plot 1: Residuals vs Fitted
  plot(model_obj$model, 
       main = paste("Ocean Age", model_obj$ocean_age))
  
  # Plot 2: Q-Q plot
  qqnorm(residuals(model_obj$model, type = "normalized"))
  qqline(residuals(model_obj$model, type = "normalized"))
  
  # Plot 3: Scale-location
  plot(fitted(model_obj$model), 
       sqrt(abs(residuals(model_obj$model, type = "normalized"))),
       xlab = "Fitted values",
       ylab = "Sqrt(|Normalized residuals|)",
       main = "Scale-Location")
  
  # Plot 4: Residuals by year
  boxplot(residuals(model_obj$model) ~ model_obj$data$year,
          xlab = "Year",
          ylab = "Residuals",
          main = "Residuals by Year",
          las = 2)
  
  par(mfrow = c(1, 1))
}

# ============================================================================
# 7. MAIN ANALYSIS WORKFLOW
# ============================================================================

run_full_analysis <- function(hatchery_data, ocean_ages = 1:5) {
  
  # Step 1: Prepare data
  cat("Preparing data...\n")
  df_prepared <- prepare_data(hatchery_data)
  
  # Step 2: Fit models for each ocean age
  cat("\nFitting LME models for each ocean age...\n")
  
  results_list <- list()
  
  for(age in ocean_ages) {
    
    # Fit model with sex
    model_with_sex <- fit_lme_by_age(df_prepared, age, include_sex = TRUE)
    
    # Fit model without sex (for comparison)
    model_without_sex <- fit_lme_by_age(df_prepared, age, include_sex = FALSE)
    
    if(!is.null(model_with_sex) && !is.null(model_without_sex)) {
      
      # Compare models
      cat("\nComparing models with and without sex:\n")
      cat("AIC with sex:", AIC(model_with_sex$model), "\n")
      cat("AIC without sex:", AIC(model_without_sex$model), "\n")
      
      # Use model with lower AIC
      if(AIC(model_with_sex$model) < AIC(model_without_sex$model)) {
        results_list[[paste0("age_", age)]] <- model_with_sex
        cat("Selected model WITH sex\n")
      } else {
        results_list[[paste0("age_", age)]] <- model_without_sex
        cat("Selected model WITHOUT sex\n")
      }
      
      # Run model diagnostics
      model_diagnostics(results_list[[paste0("age_", age)]])
    }
  }
  
  # Step 3: Model selection for each age
  cat("\n\nRunning model selection...\n")
  
  selection_results <- list()
  for(age_name in names(results_list)) {
    selection_results[[age_name]] <- run_model_selection(results_list[[age_name]])
  }
  
  # Step 4: Extract and plot year effects
  year_effects <- lapply(results_list, extract_year_effects)
  year_plot <- plot_year_trends(year_effects)
  print(year_plot)
  
  # Return all results
  return(list(
    models = results_list,
    selection = selection_results,
    year_effects = year_effects,
    year_plot = year_plot,
    data = df_prepared
  ))
}

# ============================================================================
# 8. COMPARE MODELS ACROSS AGES
# ============================================================================

compare_age_models <- function(results) {
  
  cat("\n========================================\n")
  cat("Summary of models across ocean ages\n")
  cat("========================================\n")
  
  comparison_df <- data.frame(
    ocean_age = integer(),
    n_obs = integer(),
    AIC = numeric(),
    BIC = numeric(),
    logLik = numeric(),
    r_squared = numeric()
  )
  
  for(age_name in names(results$models)) {
    model_obj <- results$models[[age_name]]
    
    comparison_df <- rbind(comparison_df, data.frame(
      ocean_age = model_obj$ocean_age,
      n_obs = model_obj$n_obs,
      AIC = AIC(model_obj$model),
      BIC = BIC(model_obj$model),
      logLik = as.numeric(logLik(model_obj$model)),
      r_squared = NA  # Calculate if needed
    ))
  }
  
  print(comparison_df)
  return(comparison_df)
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

# Assuming your data is loaded as 'hatchery':
# results <- run_full_analysis(hatchery, ocean_ages = 1:5)
# comparison <- compare_age_models(results)

# To access specific model results:
# summary(results$models$age_3$model)
# results$selection$age_3$top_models

cat("\n========================================\n")
cat("Script loaded successfully!\n")
cat("========================================\n")
cat("\nTo run the analysis, use:\n")
cat("results <- run_full_analysis(hatchery, ocean_ages = 1:5)\n")
cat("comparison <- compare_age_models(results)\n")
