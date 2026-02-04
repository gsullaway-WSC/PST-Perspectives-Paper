# OP Chinook Salmon Size-at-Age Analysis
# Recreating Ohlberger et al. 2018 LME approach for WA coast data
# Author: Sullaway
 
library(nlme)
library(MuMIn)
library(lme4)
library(tidyverse)
library(here) 
library(gridExtra)

# 1. DATA PREPARATION ============================================================================
 
prepare_data <- function(df) {
  
  df_clean <- df %>%
    arrange(brood_year) %>% # so first year is a reference year 
    rename(ocean_age = "ocean.age") %>% # rename ocean age for standard naming
    mutate(
      # Convert recovery_date to Date format i 
      recovery_date = as.Date(as.character(recovery_date), format = "%Y%m%d"),
      
      # Extract day of year
      day_of_year = as.numeric(format(recovery_date, "%j")),
      
      # Calculate total age and FW age 
      total_age = run_year - brood_year,
      freshwater_age = total_age - ocean_age,
      # ocean_age = total_age - freshwater_age,  # You'll need freshwater_age column
      # 
      # currently not using wild stocks so dont need this. 
      # rearing_type = factor(ifelse(is.na(hatchery_location_code), "wild", "hatchery")),
      
      # release stage may effect final size 
      release_stage = factor(release_stage), 
      
        
      # Convert factors
      run_type = factor(run),
      sex_clean = factor(case_when(
        sex %in% c("M", "m") ~ "male",
        sex %in% c("F", "f") ~ "female",
        TRUE ~ "unknown"
      )),
      # currently just using fishery, gear is a factor within fishery.... 
      fishery = factor(fishery),
      freshwater_age = factor(freshwater_age),
      year = factor(brood_year),
      hatchery_name = factor(hatchery_location_name)#,  # Added as per your request
      
      # Population identifier (you may need to create this)
      # This could be based on release location or hatchery
      # population = factor(coalesce(release_location_code, hatchery_location_code))
    ) %>%
    # Remove rows with missing length (response variable)
    filter(!is.na(length)) %>%
    # Remove rows with missing key variables
    filter(!is.na(ocean_age), !is.na(brood_year))
  
  return(df_clean)
}

#df_clean<-prepare_data(df = df)
# 2. FILTER DATA BY MINIMUM OBSERVATIONS ============================================================================
 
# this only drops ~100, good. 
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

 # 3. FIT LME MODELS FOR EACH OCEAN AGE============================================================================

 
fit_lme_by_age <- function(df, ocean_age_value, include_sex = FALSE) {
  
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
  
  # Build formula -- may want to include sex later but it has lots of NAs
  # if(include_sex) {
  #   fixed_formula <- "length ~ year * release_stage + fishery + freshwater_age + run_type + sex_clean + day_of_year + hatchery_name"
  # } else {
  if(ocean_ages == 5) { # only one level of FW age for age 5 fish so remving from model 
    fixed_formula <- "length ~ year + fishery + run_type + day_of_year + release_stage + hatchery_name"
} else {
  fixed_formula <- "length ~ year + fishery + freshwater_age + run_type + day_of_year + release_stage + hatchery_name"
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
  
  # Model 3: Random intercept for hatchery_name
  m2 <- tryCatch({
    lme(as.formula(fixed_formula),
        random = ~1|hatchery_name,
        data = df_age,
        method = "REML",
        na.action = na.omit)
  }, error = function(e) NULL)
  
  # Model 4: Year nested within hatchery_name (as in Ohlberger)
  # m3 <- tryCatch({
  #   lme(as.formula(fixed_formula),
  #       random = ~1|hatchery_name/year,
  #       data = df_age,
  #       method = "REML",
  #       na.action = na.omit)
  # }, error = function(e) NULL)
  
  # Compare models using AIC
  models_list <- list(no_random = m0)
  if(!is.null(m1)) models_list$year_random <- m1
  if(!is.null(m2)) models_list$pop_random <- m2
  # if(!is.null(m3)) models_list$nested_random <- m3
  
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
  
  # Test weights by hatchery_name
  m_var_pop <- tryCatch({
    update(best_model, weights = varIdent(form = ~1|hatchery_name))
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

#model_obj <- fit_lme_by_age(df = df_clean, ocean_age_value=5)
# df = df_clean
 # 4. MODEL SELECTION USING MUMIN============================================================================
 
run_model_selection <- function(model_obj) {
  
  cat("\n========================================\n")
  cat("Running model selection for Ocean Age", model_obj$ocean_age, "\n")
  cat("========================================\n")
  
  # Remove rows with NAs
  df_complete <- model_obj$data %>%
    dplyr::select(all_of(all.vars(formula(model_obj$model)))) %>%
    na.omit()
  
  cat("Complete cases:", nrow(df_complete), "out of", nrow(model_obj$data), "\n")
  
  # Get the formula and check for random effects
  model_formula <- formula(model_obj$model)
  has_random <- inherits(model_obj$model, "lme")
  
  # Refit with ML and na.fail
  if(has_random) {
    # Get random effects - use getVarCov to extract the actual structure
    random_struct <- formula(model_obj$model$modelStruct$reStruct)
    
    model_ml <- lme(model_formula,
                    random = random_struct,
                    data = df_complete,
                    method = "ML",
                    na.action = na.fail,
                    control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100))
  } else {
    model_ml <- gls(model_formula,
                    data = df_complete,
                    method = "ML",
                    na.action = na.fail)
  }
  
  # Set global option
  options(na.action = "na.fail")
  
  # Run dredge
  cat("\nGenerating candidate model set...\n")
  
  dredge_result <- tryCatch({
    dredge(model_ml, rank = "AIC")
  }, error = function(e) {
    cat("Error in dredge:", e$message, "\n")
    return(NULL)
  })
  
  # Reset
  options(na.action = "na.omit")
  
  if(is.null(dredge_result)) {
    return(NULL)
  }
  
  # Get top models (delta AIC < 2)
  top_models <- subset(dredge_result, delta < 2)
  
  cat("\nTop models (delta AIC < 2):\n")
  print(top_models)
  
  # Model averaging
  # avg_model <- model.avg(dredge_result, subset = delta < 2)
  # 
  # cat("\nModel-averaged parameters:\n")
  # print(summary(avg_model))
  
  return(list(
    dredge_result = dredge_result,
    top_models = top_models#,
    # avg_model = avg_model
  ))
}

#top_mods<- run_model_selection(model_obj = model_obj)

 # 5. EXTRACT AND VISUALIZE YEAR EFFECTS ============================================================================
 
extract_year_effects <- function(model_obj) {
  
  # Get fixed effects
  fixed_eff <- if(inherits(model_obj$model, "lme")) {
    fixef(model_obj$model)
  } else {
    coef(model_obj$model)
  }
  
  # Get standard errors
  se <- sqrt(diag(vcov(model_obj$model)))
  
  # Extract year coefficients
  year_coefs <- fixed_eff[grep("^year", names(fixed_eff))]
  year_se <- se[grep("^year", names(se))]
  
  # Create data frame for plotting
  year_effects_df <- data.frame(
    year = as.numeric(gsub("year", "", names(year_coefs))),
    effect = as.numeric(year_coefs),
    se = as.numeric(year_se),
    ocean_age = model_obj$ocean_age
  )
  
  # Calculate 95% confidence intervals
  year_effects_df$lower <- year_effects_df$effect - 1.96 * year_effects_df$se
  year_effects_df$upper <- year_effects_df$effect + 1.96 * year_effects_df$se
  
  return(year_effects_df)
}
#year_eff<-extract_year_effects(model_obj)

plot_year_trends <- function(year_effects_list) {
  
  # Combine year effects from all ages
  all_effects <- bind_rows(year_effects_list)
  
  ggplot(all_effects, aes(x = year, y = effect, color = factor(ocean_age), fill = factor(ocean_age))) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "Temporal trends in size-at-age",
      subtitle = "Year effects from LME models with 95% confidence intervals",
      x = "Brood Year",
      y = "Year effect on length (mm)",
      color = "Ocean Age",
      fill = "Ocean Age"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
}
#plot_year <- plot_year_trends(year_effects_list=year_eff)
 
# 6. MODEL DIAGNOSTICS ============================================================================
 model_diagnostics <- function(model_obj) {
  
  cat("\n========================================\n")
  cat("Model Diagnostics for Ocean Age", model_obj$ocean_age, "\n")
  cat("========================================\n")
  
  
  # Get residuals and fitted values
  resids <- residuals(model_obj$model, type = "normalized")
  fitted_vals <- fitted(model_obj$model)
  used_rows <- as.numeric(names(resids))
  years <- model_obj$data$year[used_rows]
  
  # Create data frame
  diag_df <- data.frame(
    residuals = resids,
    fitted = fitted_vals,
    year = years,
    sqrt_abs_resid = sqrt(abs(resids))
  )
  
  # Plot 1: Residuals vs Fitted
  p1 <- ggplot(diag_df, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    geom_smooth(se = FALSE) +
    labs(title = paste("Residuals vs Fitted - Ocean Age", model_obj$ocean_age),
         x = "Fitted values", y = "Residuals") +
    theme_bw()
  
  # Plot 2: Q-Q plot
  p2 <- ggplot(diag_df, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_bw()
  
  # Plot 3: Scale-Location
  p3 <- ggplot(diag_df, aes(x = fitted, y = sqrt_abs_resid)) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE) +
    labs(title = "Scale-Location",
         x = "Fitted values", y = "√|Normalized residuals|") +
    theme_bw()
  
  # Plot 4: Residuals by year
  p4 <- ggplot(diag_df, aes(x = year, y = residuals, group = year)) +
    geom_boxplot() +
    labs(title = "Residuals by Year", x = "Year", y = "Residuals") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Combine plots
  combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)
  
  return(combined_plot)
}
 
# 7. MAIN ANALYSIS WORKFLOW ============================================================================
run_full_analysis <- function(input_data, ocean_ages) {
  
  # Step 1: Prepare data
  cat("Preparing data...\n")
  df_prepared <- prepare_data(input_data)
  
  # Step 2: Fit models for each ocean age
  cat("\nFitting LME models for each ocean age...\n")
  
  results_list <- list()
  diagnostic_plots <- list()  # Add this
  
  for(age in ocean_ages) {
    
    # Fit model without sex - default 
    model_list <- fit_lme_by_age(df_prepared, age)
    
    results_list[[paste0("age_", age)]] <- model_list
    
    # Run model diagnostics and save plot
    diag_plot <- model_diagnostics(results_list[[paste0("age_", age)]])
    diagnostic_plots[[paste0("age_", age)]] <- diag_plot  # Save it
    print(diag_plot)  # Still display it
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
    diagnostics = diagnostic_plots,  # Add this
    data = df_prepared
  ))
}
# 8. COMPARE MODELS ACROSS AGES ============================================================================
 
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


# CALL and RUN ===========
## load data ====
df <- read_csv("data/OP_Chinook_RMIS_tidy.csv") %>% 
  filter(!brood_year < 1980)

## call functions for all ages ====

# ocean age one at a time 
# results1 <- run_full_analysis(input_data=df, ocean_ages = 1)
# results2 <- run_full_analysis(input_data=df, ocean_ages = 2)
# results3 <- run_full_analysis(input_data=df, ocean_ages = 3)
# results4 <- run_full_analysis(input_data=df, ocean_ages = 4)
# results5 <- run_full_analysis(input_data=df, ocean_ages = 5)

# all_results <-list(results1,results1,results1,results1)
 
# once they work individually, run them all in one so its easier for plotting etc. 
 results <- run_full_analysis(df, ocean_ages = 1:5)
 
 write_rds(results, "output/sizeatage_LME_results.RDS")
  
 ## compare for all ages ===== 
  comparison <- compare_age_models(results)
  