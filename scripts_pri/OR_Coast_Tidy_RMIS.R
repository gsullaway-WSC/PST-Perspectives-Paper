library(tidyverse)
library(here)
 
# notes:
  
# how to group these? 
# Filtering notes:
# remove juvenile sampling and foreign high seas 
 
# modeling notes 
 
# Grouping notes:
 # unique(oc_release$release_location_state)
 # unique(oc_release$release_location_rmis_region)
# unique(oc_release$release_location_rmis_region)

 # Load releases ========= 
oc_release <- read_csv("data/RMIS_releases/OR_Coast_Releases.csv") %>%
  dplyr::rename(tag_code = "tag_code_or_release_id") %>% 
  filter(species == 1,
         !is.na(release_location_rmis_region),
         # release_location_state == "OR"#,
          !release_location_rmis_region %in% c("SNAK","ORGN")
         ) %>%
         # release_location_rmis_region == "NWC") %>%
  dplyr::select(tag_code, run, brood_year, first_release_date, 
                release_location_code, hatchery_location_code,
                release_stage, avg_weight, avg_length, 
                release_location_name, hatchery_location_name,
                stock_location_name, stock_location_code,
                release_location_rmis_region,
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
                  # sampled_sex,
                  number_cwt_estimated) %>%
    dplyr::mutate(tag_code = as.character(tag_code))
  # return(recovery_tidy)
  
  # # Join with release data and filter
  recovery_joined <- left_join(recovery_tidy, release_data, by = "tag_code") %>%
    # filter(!is.na(stock_location_name)) %>%
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
folder_path <- "data/RMIS_recoveries"
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
# recovery_test <- read_csv("data/RMIS_recoveries/OP_Chinook/")
## Process all files and combine ===== 
all_recoveries <- map_dfr(file_list, process_recovery_file, release_data = oc_release) 

# filtering out QAQC ========
recoveries <- all_recoveries %>%
  # remove juvenile sampling and foreign high seas 
  filter(!fishery %in% c(88,95, 83,74,75,70,71,72,73,79),
         !ocean.age ==0
         # !run_year < 1990
         #!run =="Summer"
         ) #%>%
  # dplyr::mutate(run = case_when(run == "Summer" & release_location_rmis_basin == "QUHO" ~ "Spring",
  #               TRUE ~ run)) %>%
  #filter(!run == "Summer")
unique(recoveries$release_location_rmis_region)

t<- unique(recoveries[c("release_location_rmis_region", "release_location_rmis_basin")])

# Create summary table =====
summary_df1 <- recoveries %>%
  group_by(stock_location_name, 
           release_location_rmis_basin, 
           hatchery_location_name, 
           release_location_name, 
           run) %>%
  dplyr::summarise(
    number_of_broodyears = n_distinct(brood_year),
    year_range = paste(min(brood_year, na.rm = TRUE), 
                       max(brood_year, na.rm = TRUE), 
                       sep = "-"),
    total_recoveries = n(),
    .groups = 'drop'
  ) 

# remove release location 
summary_df2 <- recoveries %>%
  group_by( 
    release_location_rmis_basin, 
    hatchery_location_name,  
    release_stage,
    run) %>%
  dplyr::summarise(
    number_of_broodyears = n_distinct(brood_year),
    year_range = paste(min(brood_year, na.rm = TRUE), 
                       max(brood_year, na.rm = TRUE), 
                       sep = "-"),
    total_recoveries = n(),
    .groups = 'drop'
  ) %>% 
  dplyr::mutate(use = case_when(number_of_broodyears>5 ~ "yes",
                                TRUE ~"no"))

## save tables ====
write_csv(summary_df1,"output/summary_df1_longer.csv")

write_csv(summary_df2,"output/summary_df2.csv")

 # Create a hatchery filtered DF and save =====
filter_hatcheries <- summary_df2 %>%
  filter(use == "yes")  

hatchery <- recoveries %>%
  filter(hatchery_location_name %in% c(filter_hatcheries$hatchery_location_name))

write_csv(hatchery, "data/OR_Coast_Chinook_RMIS_tidy.csv")


# Match fishery and gear names into df ========
#match <- read_csv("data/RMIS_chapter8_fishery_coding.csv")

# explore popualtion groupings ====

unique(recoveries$release_location_rmis_region)
unique(recoveries$release_location_rmis_basin)
unique(recoveries$stock_location_name)
unique(recoveries$hatchery_location_name)
unique(recoveries$release_location_name)

## Plots comparing release basin ======= 
# 1. Count of records by year ===== 
all_ages <- ggplot(recoveries, aes(x = brood_year)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Records by Run Year",
       x = "Run Year",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ocean.age, scales = "free_y") + 
  geom_hline(yintercept = 25, linetype = 2)  +
  geom_hline(yintercept = 10) 

all_ages
ggsave("output/plots/OR_Coast_age_record_count_allages.jpeg",width = 8, height =6)

age_5_count <- ggplot(recoveries %>% filter(ocean.age==5), aes(x = brood_year)) +
  geom_bar(fill = "steelblue") +
  geom_hline(yintercept = 25, linetype = 2) + 
  geom_hline(yintercept = 5) + 
  labs(title = "Number of Records by Run Year",
       x = "Run Year",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ocean.age, scales = "free_y")

age_5_count
ggsave("output/plots/OR_Coast_age_record_count_age5.jpeg",width = 8, height =6)

# 2. Count by release location basin =====
ggplot(recoveries, aes(y = reorder(release_location_rmis_basin, 
                                  release_location_rmis_basin, 
                                  function(x) length(x)))) +
  geom_bar(fill = "coral") +
  labs(title = "Number of Records by Release Location Basin",
       y = "Basin",
       x = "Count") +
  theme_minimal() +
  facet_wrap(~run)

# 3. Heatmap of years by basin =========
 recoveries %>%
  count(brood_year, release_location_rmis_basin,run,ocean.age) %>%
  ggplot(aes(x = brood_year, y = release_location_rmis_basin, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "gray", high = "darkblue") +
  labs(title = "Release Counts: Year by Basin",
       x = "Run Year",
       y = "Basin",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(ocean.age~run)

# 4. Length distribution to identify outliers===========
ggplot(recoveries, aes(x = length)) +
  geom_histogram(bins = 50, fill = "darkgreen", alpha = 0.7) +
  geom_vline(aes(xintercept = median(length, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Length Distribution (Check for Outliers)",
       x = "Length",
       y = "Count") +
  theme_minimal() + 
  facet_wrap(~release_location_rmis_basin)

# 5. Boxplot of length to see outliers clearly======
ggplot(recoveries, aes(y = length)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Length Boxplot - Outlier Detection",
       y = "Length") +
  theme_minimal()

# 6. Weight distribution========
ggplot(recoveries, aes(x = weight)) +
  geom_histogram(bins = 50, fill = "purple", alpha = 0.7) +
  geom_vline(aes(xintercept = median(weight, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Weight Distribution (Check for Outliers)",
       x = "Weight",
       y = "Count") +
  theme_minimal()

# 7. Age distribution  ======
age_dist <- ggplot(recoveries, aes(x = ocean.age)) +
  geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
  labs(title = "Age Distribution (Check for Unusual Values)",
       x = "Age",
       y = "Count") +
  theme_minimal()

age_dist
ggsave("output/plots/age_distribution.jpeg",width = 8, height =6)

# 8. Missing data visualization ==== 
recoveries %>%
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
  
  
# Check for negative or zero values - this should be 0! 
recoveries %>%
  filter(length <= 0 | weight <= 0 | ocean.age < 0) %>%
  select(recovery_id, length, weight, ocean.age, brood_year)

# 9. Length violin plot ==========

ggplot(recoveries, aes(x = factor(ocean.age), y = length, fill = factor(run) )) +
  geom_violin(alpha = 0.7) +
  labs(title = "Length Distribution by Run Type and Basin",
       x = "Run Type",
       y = "Length",
       fill = "Basin") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(run~release_location_rmis_basin)


#10 Mean length by age over time - line plot ===========
all_recoveries_summary <- recoveries %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(brood_year, ocean.age,run, release_location_rmis_basin) %>%
  summarise(
    mean_length = mean(length, na.rm = TRUE),
    n = n(),
    se = sd(length, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

ggplot(all_recoveries_summary, 
       aes(x = brood_year, y = mean_length, color = factor(ocean.age))) +
  geom_path() +
  scale_color_viridis_d() + 
  geom_point() +
  facet_grid(run~release_location_rmis_basin) +
  labs(title = "Mean Length Over Time by Age",
       x = "Run Year",
       y = "Mean Length",
       color = "Ocean Age") +
  theme_minimal()
 
# 11 Violin plots: Distribution of length by age for each basin =======
ggplot(recoveries %>% filter(!is.na(length), !is.na(ocean.age)), 
       aes(x = factor(ocean.age), y = length, fill = release_location_rmis_basin)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_grid(run~release_location_rmis_basin) +
  labs(title = "Length Distribution by Age and Basin",
       x = "Ocean Age",
       y = "Length",
       fill = "Basin") +
  theme_minimal()

# 8. Sample size visualization
sample_sizes <- recoveries %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  count(run,brood_year, ocean.age, release_location_rmis_basin)

ggplot(sample_sizes, 
       aes(x = brood_year, y = factor(ocean.age), size = n, color = n)) +
  geom_point(alpha = 0.6) +
  facet_grid(run~release_location_rmis_basin) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_viridis_c() +
  labs(title = "Sample Sizes: Age by Year and Basin",
       x = "Run Year",
       y = "Ocean Age",
       size = "N",
       color = "N") +
  theme_minimal()

#   Calculate mean length by age and basin (overall baseline)
baseline_means <- recoveries %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(ocean.age, run,release_location_rmis_basin) %>%
  summarise(
    baseline_mean_length = mean(length, na.rm = TRUE),
    baseline_sd = sd(length, na.rm = TRUE),
    .groups = "drop"
  )

scaled_trends <-recoveries %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(brood_year, ocean.age, run, release_location_rmis_basin) %>%
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

# 12. Plot scaled anomalies over time =========
scaled_length <- ggplot(scaled_trends, 
       aes(x = brood_year, y = length_scaled, color = release_location_rmis_basin)) +
  geom_path(size = 0.5) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_grid(run~ factor(ocean.age), scales = "free" ) +
  labs(title = "Scaled Length Anomalies Over Time",
       subtitle = "Standardized deviation from age-specific mean length",
       x = "Run Year",
       y = "Standardized Length Anomaly (SD units)",
       color = "Ocean Age") +
  theme_minimal() +
  theme(legend.position = "right")


scaled_length
ggsave("output/plots/scaled_length.jpeg",width = 8, height =6)

# 13. Heatmap of scaled anomalies ===========
ggplot(scaled_trends, 
       aes(x = brood_year, y = factor(ocean.age), fill = length_scaled)) +
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

# 14. plot mean diff btwn start end length ======
scaled_trends <-recoveries %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(brood_year, ocean.age, run, release_location_rmis_basin) %>%
  summarise(
    annual_mean_length = mean(length, na.rm = TRUE),
    n = n(),
    se = sd(length, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) 

k_years = 10

diff_stats <- scaled_trends %>%
  group_by(ocean.age, release_location_rmis_basin, run) %>%
  # filter(n >= 10) %>%                     # same sample size filter
  arrange(brood_year) %>%
  mutate(
    year_rank_start = row_number(),
    year_rank_end   = row_number(desc(brood_year))
  ) %>%
  summarise(
    n_years = n(),
    
    start_mean = mean(annual_mean_length[year_rank_start <= k_years],
                      na.rm = TRUE),
    
    end_mean   = mean(annual_mean_length[year_rank_end <= k_years],
                      na.rm = TRUE),
    
    mean_diff = end_mean - start_mean,
    .groups = "drop"
  ) %>%
  mutate(
    change_direction = case_when(
      mean_diff > 0  ~ "Increase",
      mean_diff < 0  ~ "Decrease",
      TRUE           ~ "No change"
    )
  )

ggplot(diff_stats,
       aes(x = factor(ocean.age),
           y = mean_diff,
           fill = change_direction)) +
  geom_col() +
  facet_wrap(run ~ release_location_rmis_basin) +
  scale_fill_manual(values = c(
    "Increase"  = "red",
    "Decrease"  = "blue",
    "No change" = "gray"
  )) +
  labs(
    title = "Difference in mean length from Start to End of Time Series",
    subtitle = paste0("Difference between first and last ", k_years, " years"),
    x = "Ocean Age",
    y = "Diff in mean length (mm)",
    fill = "Direction of change"
  ) +
  theme_minimal()

## Plots comparing hatchery ======= 
# 1. Count of records by year ===== 
ggplot(hatchery, aes(x = brood_year)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Records by brood Year",
       x = "brood Year",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(hatchery_location_name~run, scales = "free")
 
# 3. Heatmap of years by basin =========
hatchery %>%
  count(brood_year, hatchery_location_name,run,ocean.age) %>%
  ggplot(aes(x = brood_year, y = hatchery_location_name, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "gray", high = "darkblue") +
  labs(title = "Release Counts: Year by Basin",
       x = "Run Year",
       y = "Basin",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(ocean.age~run)

# 4. Length distribution to identify outliers===========
ggplot(hatchery, aes(x = length)) +
  geom_histogram(bins = 50, fill = "darkgreen", alpha = 0.7) +
  geom_vline(aes(xintercept = median(length, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Length Distribution (Check for Outliers)",
       x = "Length",
       y = "Count") +
  theme_minimal() + 
  facet_wrap(~hatchery_location_name)

# 5. Boxplot of length to see outliers clearly======
ggplot(hatchery, aes(y = length)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Length Boxplot - Outlier Detection",
       y = "Length") +
  theme_minimal()
 
# 7. Age distribution  ======
ggplot(hatchery, aes(x = ocean.age)) +
  geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
  labs(title = "Age Distribution (Check for Unusual Values)",
       x = "Age",
       y = "Count") +
  theme_minimal() +
  facet_wrap(~hatchery_location_name,scales = "free")

# 8. Missing data visualization ==== 
hatchery %>%
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
 
# Check for negative or zero values - this should be 0! 
hatchery %>%
  filter(length <= 0 | weight <= 0 | ocean.age < 0) %>%
  select(recovery_id, length, weight, ocean.age, brood_year)

# 9. Length violin plot ==========
ggplot(hatchery, aes(x = factor(ocean.age), y = length, fill = factor(run) )) +
  geom_violin(alpha = 0.7) +
  labs(title = "Length Distribution by Run Type and Basin",
       x = "Run Type",
       y = "Length",
       fill = "Basin") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(run~hatchery_location_name)

#10. Mean length by age over time - line plot ===========
all_recoveries_summary <- hatchery %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(brood_year, ocean.age,run, hatchery_location_name) %>%
  summarise(
    mean_length = mean(length, na.rm = TRUE),
    n = n(),
    se = sd(length, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

mean_L_through_time <- ggplot(all_recoveries_summary, 
       aes(x = brood_year, y = mean_length, color = factor(ocean.age))) +
  geom_path() +
  scale_color_viridis_d() + 
  geom_point() +
  facet_grid(run~hatchery_location_name) +
  labs(title = "Mean Length Over Time by Age",
       x = "brood Year",
       y = "Mean Length",
       color = "Ocean Age") +
  theme_minimal()

mean_L_through_time
ggsave("output/plots/mean_length_time_data.jpeg",width = 8, height =6)


# 11. Violin plots: Distribution of length by age for each basin =======
ggplot(hatchery %>% filter(!is.na(length), !is.na(ocean.age)), 
       aes(x = factor(ocean.age), y = length, fill = hatchery_location_name)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_grid(run~hatchery_location_name) +
  labs(title = "Length Distribution by Age and Hatchery",
       x = "Ocean Age",
       y = "Length",
       fill = "Basin") +
  theme_minimal()


# 8. Sample size visualization
sample_sizes <- hatchery  %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  count(run,brood_year, ocean.age, hatchery_location_name)

ggplot(sample_sizes, 
       aes(x = brood_year, y = factor(ocean.age), size = n, color = n)) +
  geom_point(alpha = 0.6) +
  facet_grid(run~hatchery_location_name) +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_viridis_c() +
  labs(title = "Sample Sizes: Age by Year and Basin",
       x = "Brood Year",
       y = "Ocean Age",
       size = "N",
       color = "N") +
  theme_minimal()

# 12. Calculate mean length by age and basin (overall baseline) ========
baseline_means <- hatchery %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(ocean.age, run,hatchery_location_name) %>%
  summarise(
    baseline_mean_length = mean(length, na.rm = TRUE),
    baseline_sd = sd(length, na.rm = TRUE),
    .groups = "drop"
  )

scaled_trends <-hatchery %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(brood_year, ocean.age, run, hatchery_location_name) %>%
  summarise(
    annual_mean_length = mean(length, na.rm = TRUE),
    n = n(),
    se = sd(length, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  left_join(baseline_means, by = c("ocean.age", "hatchery_location_name", "run")) %>%
  dplyr::mutate(
    # Deviation from age-specific mean
    length_anomaly = annual_mean_length - baseline_mean_length,
    # Standardized anomaly (z-score)
    length_scaled = (annual_mean_length - baseline_mean_length) / baseline_sd,
    # Percent deviation
    length_pct_change = ((annual_mean_length - baseline_mean_length) / baseline_mean_length) * 100
  )
 

# Create a helper column to identify gaps in consecutive years
scaled_trends_Group <- scaled_trends %>%
  arrange(hatchery_location_name, run, ocean.age, brood_year) %>%
  group_by(hatchery_location_name, run, ocean.age) %>%
  mutate(
    year_diff = brood_year - lag(brood_year),
    group_id = cumsum(is.na(year_diff) | year_diff > 1)
  ) %>%
  ungroup() %>%
  mutate(line_group = paste(hatchery_location_name, run, ocean.age, group_id, sep = "_"))

# Plot with line_group in aes
trends_plot <- ggplot(scaled_trends_Group, 
       aes(x = brood_year, y = length_scaled, 
           color = hatchery_location_name,
           group = line_group)) +  # Add group aesthetic
  geom_path(size = 0.5) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_grid(run ~ factor(ocean.age), scales = "free") +
  labs(title = "Scaled Length Anomalies Over Time",
       subtitle = "Standardized deviation from age-specific mean length",
       x = "Brood Year",
       y = "Standardized Length Anomaly (SD units)",
       color = "Hatchery Location") +
  theme_minimal() +
  theme(legend.position = "right")
trends_plot
ggsave("output/plots/length_trends_time.jpeg",width = 8, height =6)

# 6. Heatmap of scaled anomalies
ggplot(scaled_trends, 
       aes(x = brood_year, y = factor(ocean.age), fill = length_scaled)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-3, 3), oob = scales::squish) +
  facet_grid(run~hatchery_location_name ) +
  labs(title = "Standardized Length Anomalies Heatmap",
       x = "Run Year",
       y = "Ocean Age",
       fill = "SD Anomaly ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8. plot mean diff btwn start end length ===
scaled_trends <-hatchery %>%
  filter(!is.na(length), !is.na(ocean.age)) %>%
  group_by(brood_year, ocean.age, run, hatchery_location_name) %>%
  summarise(
    annual_mean_length = mean(length, na.rm = TRUE),
    n = n(),
    se = sd(length, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) 

k_years = 10

diff_stats <- scaled_trends %>%
  group_by(ocean.age, hatchery_location_name, run) %>%
  # filter(n >= 10) %>%                     # same sample size filter
  arrange(brood_year) %>%
  mutate(
    year_rank_start = row_number(),
    year_rank_end   = row_number(desc(brood_year))
  ) %>%
  summarise(
    n_years = n(),
    
    start_mean = mean(annual_mean_length[year_rank_start <= k_years],
                      na.rm = TRUE),
    
    end_mean   = mean(annual_mean_length[year_rank_end <= k_years],
                      na.rm = TRUE),
    
    mean_diff = end_mean - start_mean,
    .groups = "drop"
  ) %>%
  mutate(
    change_direction = case_when(
      mean_diff > 0  ~ "Increase",
      mean_diff < 0  ~ "Decrease",
      TRUE           ~ "No change"
    )
  )

summary_stat_plot<- ggplot(diff_stats,
       aes(x = factor(ocean.age),
           y = mean_diff,
           fill = change_direction)) +
  geom_col() +
  facet_wrap(run ~ hatchery_location_name) +
  scale_fill_manual(values = c(
    "Increase"  = "red",
    "Decrease"  = "blue",
    "No change" = "gray"
  )) +
  labs(
    title = "Difference in mean length from Start to End of Time Series",
    subtitle = paste0("Difference between first and last ", k_years, " years"),
    x = "Ocean Age",
    y = "Diff in mean length (mm)",
    fill = "Direction of change"
  ) +
  theme_minimal()

summary_stat_plot
ggsave("output/plots/summary_stat_plot.jpeg",width = 8, height =6)

