library(tidyverse)
library(here)
 
# notes:
# how to group these? currently doing the basins, which yields 2 basins. Otherwise question is do you group by relase location, hatchery or popualtion?
# but then populations are like queets x quinalt, because of blending brood stock.... 
# so i think basin by run type is probably best. 
# not much info on sex, good amount of missing values are we ok not knowing this?? 
library(tidyverse)
library(here)
 
# Load releases ========= 
op_release <- read_csv("data/RMIS_releases/OP_Releases.csv") %>%
  dplyr::rename(tag_code = "tag_code_or_release_id") %>% 
  filter(species == 1,  
         release_location_state == "WA",
         release_location_rmis_region == "NWC") %>%
  dplyr::select(tag_code, run, brood_year, first_release_date, 
                release_location_code, hatchery_location_code,
                release_stage, avg_weight, avg_length, 
                release_location_name, hatchery_location_name,
                stock_location_name, release_location_rmis_region,
                release_location_rmis_basin) %>%
  dplyr::mutate(tag_code = as.character(tag_code))

# Function to process a single recovery file
process_recovery_file <- function(file_path, release_data) {
  
  # Read and tidy the recovery file
  recovery_tidy <- read_csv(file_path, show_col_types = FALSE) %>% 
    dplyr::select(recovery_id,
                  run_year,
                  recovery_date,
                  fishery,
                  gear, 
                  estimation_level,
                  sex,
                  weight,
                  weight_code,
                  weight_type,
                  length,
                  length_code,
                  length_type,
                  tag_code,
                  catch_sample_id,
                  sampled_maturity,
                  sampled_run,
                  sampled_sex,
                  number_cwt_estimated) %>%
    dplyr::mutate(tag_code = as.character(tag_code))
  
  # Join with release data and filter
  recovery_joined <- left_join(recovery_tidy, release_data, by = "tag_code") %>% 
    filter(!is.na(stock_location_name)) %>%
    # separate(recovery_date, sep = -4, into = c("recovery_year", "del"), remove = FALSE) %>%
    separate(first_release_date, sep = -4, into = c("release_year", "del2"), remove = FALSE) %>%
    select(-c(del2)) %>%
    dplyr::mutate(
      ocean.age = as.numeric(run_year) - as.numeric(release_year),
      run = case_when(
        run == 1 ~ "Spring",
        run == 2 ~ "Summer",
        run == 3 ~ "Fall"
      )
    ) %>% 
        filter(!ocean.age < 0,
               !ocean.age > 5
               )
  
  return(recovery_joined)
}


# Load recovery files ===== 
folder_path <- "data/RMIS_recoveries/OP_Chinook"
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
# recovery_test <- read_csv("data/RMIS_recoveries/OP_Chinook/")
## Process all files and combine ===== 
all_recoveries <- map_dfr(file_list, process_recovery_file, release_data = op_release)
 
## QAQC ======= 
# 1. Count of records by year
ggplot(all_recoveries, aes(x = run_year)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Records by Run Year",
       x = "Run Year",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Count by release location basin
ggplot(all_recoveries, aes(y = reorder(release_location_rmis_basin, 
                                  release_location_rmis_basin, 
                                  function(x) length(x)))) +
  geom_bar(fill = "coral") +
  labs(title = "Number of Records by Release Location Basin",
       y = "Basin",
       x = "Count") +
  theme_minimal()

# 3. Heatmap of years by basin
 all_recoveries %>%
  count(run_year, release_location_rmis_basin) %>%
  ggplot(aes(x = run_year, y = release_location_rmis_basin, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(title = "Release Counts: Year by Basin",
       x = "Run Year",
       y = "Basin",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Length distribution to identify outliers
ggplot(all_recoveries, aes(x = length)) +
  geom_histogram(bins = 50, fill = "darkgreen", alpha = 0.7) +
  geom_vline(aes(xintercept = median(length, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Length Distribution (Check for Outliers)",
       x = "Length",
       y = "Count") +
  theme_minimal() + 
  facet_wrap(~release_location_rmis_basin)

# 5. Boxplot of length to see outliers clearly
ggplot(all_recoveries, aes(y = length)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Length Boxplot - Outlier Detection",
       y = "Length") +
  theme_minimal()

# 6. Weight distribution
ggplot(all_recoveries, aes(x = weight)) +
  geom_histogram(bins = 50, fill = "purple", alpha = 0.7) +
  geom_vline(aes(xintercept = median(weight, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Weight Distribution (Check for Outliers)",
       x = "Weight",
       y = "Count") +
  theme_minimal()

# 7. Age distribution  
ggplot(all_recoveries, aes(x = ocean.age)) +
  geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
  labs(title = "Age Distribution (Check for Unusual Values)",
       x = "Age",
       y = "Count") +
  theme_minimal()

# Missing data visualization ==== 
all_recoveries %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  ggplot(aes(x = reorder(variable, missing_count), y = missing_count)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(title = "Missing Values by Variable",
       x = "Variable",
       y = "Number of Missing Values") +
  theme_minimal()
  
 
# 10. Identify potential data issues
# Check for negative or zero values
all_recoveries %>%
  filter(length <= 0 | weight <= 0 | ocean.age < 0) %>%
  select(recovery_id, length, weight, ocean.age, run_year)

# Check for extreme outliers (beyond 3 SD)
all_recoveries %>%
  mutate(
    length_z = abs((length - mean(length, na.rm = TRUE)) / sd(length, na.rm = TRUE)),
    weight_z = abs((weight - mean(weight, na.rm = TRUE)) / sd(weight, na.rm = TRUE))
  ) %>%
  filter(length_z > 3 | weight_z > 3) %>%
  select(recovery_id, length, weight, length_z, weight_z, run_year)

# Plot basin by run type recovery ages and lengths === 
# Age plot
ggplot(all_recoveries, aes(x = factor(run), y = ocean.age, fill = release_location_rmis_basin)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Run Type and Basin",
       x = "Run Type",
       y = "Age (years)",
       fill = "Basin") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Basin by Run Type for Length
ggplot(all_recoveries, aes(x = factor(run), y = length, fill = release_location_rmis_basin)) +
  geom_boxplot() +
  labs(title = "Length Distribution by Run Type and Basin",
       x = "Run Type",
       y = "Length",
       fill = "Basin") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Length violin plot
ggplot(all_recoveries, aes(x = factor(run), y = length, fill = release_location_rmis_basin)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  labs(title = "Length Distribution by Run Type and Basin",
       x = "Run Type",
       y = "Length",
       fill = "Basin") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot length by age by basin ==================
 
# 1. Basic scatter plot: Length vs Age, faceted by basin, colored by year
ggplot(all_recoveries %>% filter(!is.na(length), !is.na(ocean.age)), 
       aes(x = ocean.age, y = length, color = run_year)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~release_location_rmis_basin) +
  scale_color_viridis_c() +
  labs(title = "Length by Age Through Time",
       x = "Ocean Age",
       y = "Length",
       color = "Year") +
  theme_minimal()

# 2. Box plots: Length by age for each basin, faceted by time periods
all_recoveries_time <- all_recoveries %>%
  mutate(time_period = cut(run_year, 
                           breaks = seq(min(run_year, na.rm = TRUE), 
                                        max(run_year, na.rm = TRUE) + 5, 
                                        by = 5),
                           include.lowest = TRUE))

ggplot(all_recoveries_time %>% filter(!is.na(length), !is.na(ocean.age)), 
       aes(x = factor(ocean.age), y = length, fill = release_location_rmis_basin)) +
  geom_boxplot() +
  facet_wrap(~time_period, ncol = 3) +
  labs(title = "Length by Age Across Time Periods",
       x = "Ocean Age",
       y = "Length",
       fill = "Basin") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Mean length by age over time - line plot
all_recoveries_summary <- all_recoveries %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(run_year, ocean.age, release_location_rmis_basin) %>%
  summarise(
    mean_length = mean(length, na.rm = TRUE),
    n = n(),
    se = sd(length, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

ggplot(all_recoveries_summary, 
       aes(x = run_year, y = mean_length, color = factor(ocean.age))) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~release_location_rmis_basin) +
  labs(title = "Mean Length Over Time by Age",
       x = "Run Year",
       y = "Mean Length",
       color = "Ocean Age") +
  theme_minimal()
 

# 5. Trend lines with confidence intervals
ggplot(all_recoveries %>% filter(!is.na(length), !is.na(ocean.age)), 
       aes(x = run_year, y = length, color = factor(ocean.age))) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", se = TRUE, size = 1) +
  facet_wrap(~release_location_rmis_basin) +
  labs(title = "Length Trends Over Time by Age",
       x = "Run Year",
       y = "Length",
       color = "Ocean Age") +
  theme_minimal()

# 6. Violin plots: Distribution of length by age for each basin
ggplot(all_recoveries %>% filter(!is.na(length), !is.na(ocean.age)), 
       aes(x = factor(ocean.age), y = length, fill = release_location_rmis_basin)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_wrap(~release_location_rmis_basin) +
  labs(title = "Length Distribution by Age and Basin",
       x = "Ocean Age",
       y = "Length",
       fill = "Basin") +
  theme_minimal()


# 8. Sample size visualization
sample_sizes <- all_recoveries %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  count(run_year, ocean.age, release_location_rmis_basin)

ggplot(sample_sizes, 
       aes(x = run_year, y = factor(ocean.age), size = n, color = n)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~release_location_rmis_basin) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_viridis_c() +
  labs(title = "Sample Sizes: Age by Year and Basin",
       x = "Run Year",
       y = "Ocean Age",
       size = "N",
       color = "N") +
  theme_minimal()

# Trends =====  
# 1. Calculate mean length by age and basin (overall baseline)
baseline_means <- all_recoveries %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(ocean.age, run,release_location_rmis_basin) %>%
  summarise(
    baseline_mean_length = mean(length, na.rm = TRUE),
    baseline_sd = sd(length, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Calculate annual means and scale them
scaled_trends <- all_recoveries %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(run_year, ocean.age, run, release_location_rmis_basin) %>%
  summarise(
    annual_mean_length = mean(length, na.rm = TRUE),
    n = n(),
    se = sd(length, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  left_join(baseline_means, by = c("ocean.age", "release_location_rmis_basin", "run")) %>%
  dplyr::mutate(
    # Deviation from age-specific mean
    length_anomaly = annual_mean_length - baseline_mean_length,
    # Standardized anomaly (z-score)
    length_scaled = (annual_mean_length - baseline_mean_length) / baseline_sd,
    # Percent deviation
    length_pct_change = ((annual_mean_length - baseline_mean_length) / baseline_mean_length) * 100
  )

# 3. Plot scaled anomalies over time
ggplot(data = all_recoveries %>%
         group_by(run, release_location_rmis_basin,ocean.age) %>%
        dplyr::mutate(length_scaled= as.numeric(scale(length))) %>%
         filter(run == "Fall"),
       aes(x = run_year, y = length_scaled, color = factor(ocean.age))) +
  geom_smooth(size = 1) +
  geom_point( alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_grid( ocean.age~release_location_rmis_basin, scales = "free" ) +
  labs(title = "Scaled Length Anomalies Over Time",
       subtitle = "Standardized deviation from age-specific mean length",
       x = "Run Year",
       y = "Standardized Length Anomaly (SD units)",
       color = "Ocean Age",
       size = "Sample Size") +
  theme_minimal() +
  theme(legend.position = "right")


# 3. Plot scaled anomalies over time
ggplot(scaled_trends, 
       aes(x = run_year, y = length_scaled, color = factor(ocean.age))) +
  geom_line(size = 1) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_grid(run~release_location_rmis_basin, scales = "free" ) +
  labs(title = "Scaled Length Anomalies Over Time",
       subtitle = "Standardized deviation from age-specific mean length",
       x = "Run Year",
       y = "Standardized Length Anomaly (SD units)",
       color = "Ocean Age",
       size = "Sample Size") +
  theme_minimal() +
  theme(legend.position = "right")

# 6. Heatmap of scaled anomalies
ggplot(scaled_trends, 
       aes(x = run_year, y = factor(ocean.age), fill = length_scaled)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-3, 3), oob = scales::squish) +
  facet_grid(run~release_location_rmis_basin ) +
  labs(title = "Standardized Length Anomalies Heatmap",
       x = "Run Year",
       y = "Ocean Age",
       fill = "SD Anomaly ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8. Statistical summary of trends
trend_stats <- scaled_trends %>%
  group_by(ocean.age, release_location_rmis_basin) %>%
  filter(n >= 10) %>%  # Only include years with sufficient sample size
  summarise(
    n_years = n(),
    mean_anomaly = mean(length_anomaly, na.rm = TRUE),
    sd_anomaly = sd(length_anomaly, na.rm = TRUE),
    # Linear trend
    trend_slope = coef(lm(length_anomaly ~ run_year))[2],
    trend_pvalue = summary(lm(length_anomaly ~ run_year))$coefficients[2, 4],
    .groups = "drop"
  ) %>%
  mutate(
    trend_direction = case_when(
      trend_pvalue < 0.05 & trend_slope > 0 ~ "Increasing",
      trend_pvalue < 0.05 & trend_slope < 0 ~ "Decreasing",
      TRUE ~ "No significant trend"
    )
  )

# 9. Visualize trend statistics
ggplot(trend_stats, 
       aes(x = factor(ocean.age), y = trend_slope, 
           fill = trend_direction)) +
  geom_col() +
  geom_text(aes(label = round(trend_slope, 3)), 
            vjust = -0.5, size = 3) +
  facet_wrap(~release_location_rmis_basin) +
  scale_fill_manual(values = c("Increasing" = "red", 
                               "Decreasing" = "blue", 
                               "No significant trend" = "gray")) +
  labs(title = "Linear Trend Slopes: Length Anomaly Over Time",
       subtitle = "mm/year change in length-at-age",
       x = "Ocean Age",
       y = "Trend Slope (mm/year)",
       fill = "Trend Direction") +
  theme_minimal()

 