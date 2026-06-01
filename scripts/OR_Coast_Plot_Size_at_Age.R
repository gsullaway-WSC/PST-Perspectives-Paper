
library(tidyverse)
library(here)

# function  ============================================================================
extract_year_effects <- function(model_obj) {
  
  fixed_eff <- if(inherits(model_obj$model, "lme")) {
    fixef(model_obj$model)
  } else {
    coef(model_obj$model)
  }
  
  se <- sqrt(diag(vcov(model_obj$model)))
  intercept <- fixed_eff["(Intercept)"]
  
  year_coefs <- fixed_eff[grep("^year", names(fixed_eff))]
  year_se <- se[grep("^year", names(se))]
  
  # Get reference year (first year in data)
  ref_year <- min(as.numeric(as.character(model_obj$data$year)))
  
  year_effects_df <- data.frame(
    year = as.numeric(gsub("year", "", names(year_coefs))),
    effect = as.numeric(year_coefs),
    se = as.numeric(year_se),
    ocean_age = model_obj$ocean_age
  )
  
  # Add reference year with effect = 0
  ref_row <- data.frame(
    year = ref_year,
    effect = 0,
    se = 0,
    ocean_age = model_obj$ocean_age
  )
  
  year_effects_df <- bind_rows(ref_row, year_effects_df) %>%
    arrange(year)
  
  # Calculate CIs
  year_effects_df$lower <- year_effects_df$effect - 1.96 * year_effects_df$se
  year_effects_df$upper <- year_effects_df$effect + 1.96 * year_effects_df$se
  
  # Predicted lengths
  year_effects_df$predicted_length <- year_effects_df$effect + intercept
  year_effects_df$predicted_lower <- year_effects_df$lower + intercept
  year_effects_df$predicted_upper <- year_effects_df$upper + intercept
  
  return(year_effects_df)
}
#year_eff<-extract_year_effects(model_obj)
plot_year_trends <- function(year_effects_list) {
  
  # Combine year effects from all ages
  all_effects <- bind_rows(year_effects_list)
  
  # Plot 1: Year effects (deviations from baseline)
  p1 <- ggplot(all_effects, aes(x = year, y = effect, color = factor(ocean_age), fill = factor(ocean_age))) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(size = 1) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = "Temporal trends in size-at-age",
      subtitle = "Year effects (deviations from baseline year)",
      x = "Brood Year",
      y = "Year effect on length (mm)",
      color = "Ocean Age",
      fill = "Ocean Age"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # Plot 2: Predicted absolute lengths
  p2 <- ggplot(all_effects, aes(x = year, y = predicted_length, color = factor(ocean_age), fill = factor(ocean_age))) +
    geom_ribbon(aes(ymin = predicted_lower, ymax = predicted_upper), alpha = 0.2, color = NA) +
    geom_line(size = 1) +
    geom_point() +
    labs(
      title = "Predicted length-at-age over time",
      subtitle = "Model-predicted lengths with 95% confidence intervals",
      x = "Brood Year",
      y = "Predicted length (mm)",
      color = "Ocean Age",
      fill = "Ocean Age"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # Combine both plots
  combined <- grid.arrange(p1, p2, ncol = 1)
  
  return(list(effects_plot = p1, predicted_plot = p2, combined = combined))
}

#plot_year <- plot_year_trends(year_effects_list=year_eff)

# Load results ===== 
# make plot for story that has size at age for all ages, model output year effects 
results <- readRDS("output/OR_Coast_sizeatage_LME_results.RDS")

# Extract year effects for all ages
all_year_effects <- lapply(results$models, extract_year_effects)
combined_effects <- bind_rows(all_year_effects) %>%
  group_by(ocean_age) %>%
  complete(year = full_seq(year, 1)) %>%  # Fill in missing years with NA
  ungroup()   

# Convert length to fecundity (1mm = 7.8 eggs)
combined_effects <- combined_effects %>%
  mutate(
    predicted_fecundity = predicted_length * 7.8,
    predicted_fecundity_lower = predicted_lower * 7.8,
    predicted_fecundity_upper = predicted_upper * 7.8
  )

# Year effects (deviations from base year) ======
effects_plot <- ggplot(combined_effects, aes(x = year, y = effect, 
                                             color = factor(ocean_age), 
                                             fill = factor(ocean_age))) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                                "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                     name = "Ocean Age") +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#D55E00","5" = "#CC79A7"),
                    name = "Ocean Age") +
  labs(
    title = "Temporal trends in Chinook salmon size-at-age",
    subtitle = "Year effects (deviation from baseline) with 95% confidence intervals",
    x = "Brood Year",
    y = "Year effect on length (mm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold")
  )
effects_plot

# Calculate means for first 5 and last 5 years
first_last_means <- combined_effects %>%
  group_by(ocean_age) %>%
  arrange(year) %>%
  mutate(
    year_rank = row_number(),
    n_years = n()
  ) %>%
  filter(!is.na(predicted_length)) %>%
  group_by(ocean_age) %>%
  mutate(
    year_rank = row_number(),
    n_years = n()
  ) %>%
  filter(year_rank <= 5 | year_rank > (n_years - 5)) %>%
  mutate(period = ifelse(year_rank <= 5, "first_5", "last_5")) %>%
  group_by(ocean_age, period) %>%
  summarise(
    mean_length = mean(predicted_length, na.rm = TRUE),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    .groups = 'drop'
  )

# Prepare data for boxplot (individual year data for first 5 and last 5)
boxplot_data <- combined_effects %>%
  group_by(ocean_age) %>%
  arrange(year) %>%
  mutate(
    year_rank = row_number(),
    n_years = n()
  ) %>%
  filter(!is.na(predicted_length)) %>%
  group_by(ocean_age) %>%
  mutate(
    year_rank = row_number(),
    n_years = n()
  ) %>%
  filter(year_rank <= 5 | year_rank > (n_years - 5)) %>%
  mutate(
    period = ifelse(year_rank <= 5, 
                    paste0("First 5 years\n(", min(year[year_rank <= 5]), "-", max(year[year_rank <= 5]), ")"),
                    paste0("Last 5 years\n(", min(year[year_rank > (n_years - 5)]), "-", max(year[year_rank > (n_years - 5)]), ")"))
  ) %>%
  ungroup()

# Create boxplot - LENGTH
boxplot_first_last <- ggplot(boxplot_data, aes(x = period, y = predicted_length, 
                                               fill = factor(ocean_age))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, coef = Inf) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                    name = "Ocean Age") +
  labs(
    title = "Change in Chinook salmon length-at-age over time",
    subtitle = "Comparison of first 5 years vs last 5 years of time series",
    x = "Time Period",
    y = "Predicted length (mm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray90")
  ) +
  facet_wrap(~ocean_age, scales = "free_x", labeller = labeller(ocean_age = function(x) paste("Ocean Age", x)))

boxplot_first_last
 
# USE FOR STORY predicted lengths Stacked ====== 
# Convert mm to inches first
combined_effects_in <- combined_effects %>%
  mutate(
    predicted_length  = predicted_length / 25.4,
    predicted_lower   = predicted_lower / 25.4,
    predicted_upper   = predicted_upper / 25.4
  ) %>% 
  filter(!year < 1986)
 
predicted_plot_together <- ggplot(combined_effects_in %>% filter(!ocean_age %in% c(1,5),!year ==1980), 
                                  aes(x = year, y = predicted_length, 
                                      color = factor(ocean_age), 
                                      fill = factor(ocean_age))) +
  # Model predictions
  geom_ribbon(aes(ymin = predicted_lower, ymax = predicted_upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2) +
  scale_color_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                                "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                     name = "Ocean Age") +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                    name = "Ocean Age") +
  labs(
    title = "Trends in Oregon Coast Chinook Salmon\nPredicted Length-at-Age",
    x = "Brood Year",
    y = "Predicted Length (in)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold")
  ) 

predicted_plot_together

ggsave("output/plots/OR_Coast_size_at_age_predicted_together.png", predicted_plot_together, 
       width = 8, height = 9, dpi = 300)


## predicted lengths - just age 4 ====== 
combined_effects_in <- combined_effects %>%
  mutate(
    predicted_length  = predicted_length / 25.4,
    predicted_lower   = predicted_lower / 25.4,
    predicted_upper   = predicted_upper / 25.4
  ) %>% 
  filter(!year < 1986)

predicted_plot_age4 <- ggplot(combined_effects_in %>% filter(ocean_age %in% c(4),
                                                             !year ==1980), 
                                  aes(x = year, y = predicted_length, 
                                      color = factor(ocean_age), 
                                      fill = factor(ocean_age))) +
  # Model predictions
  geom_ribbon(aes(ymin = predicted_lower, ymax = predicted_upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1999, linetype = 2)+
  scale_color_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                                "3" = "#009E73", "4" = "#F5B800", "5" = "#CC79A7"),
                     name = "Ocean Age") +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#F5B800", "5" = "#CC79A7"),
                    name = "Ocean Age") +
  labs(
    title = "Trends in Oregon Coast Chinook Salmon\nPredicted Length-at-Age",
    x = "Brood Year",
    y = "Predicted Length (in)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(face = "bold")
  ) 

predicted_plot_age4

ggsave("output/plots/OR_Coast_size_AGE4_predicted_together.png", predicted_plot_age4, 
       width = 8, height = 9, dpi = 300)

# Predicted lengths Faceted ====== 
predicted_plot_facet <- ggplot(combined_effects %>% filter(!ocean_age %in% c(1,5)), 
                               aes(x = year, y = predicted_length, 
                                   color = factor(ocean_age), 
                                   fill = factor(ocean_age))) +
  # Model predictions
  geom_ribbon(aes(ymin = predicted_lower, ymax = predicted_upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2) +
  # Add horizontal lines for first 5 and last 5 year means
  # geom_segment(data = first_last_means,
  #              aes(x = min_year, xend = max_year, 
  #                  y = mean_length, yend = mean_length,
  #                  color = "black" ),
  #              size = 1.5,  linetype = "solid") +
  scale_color_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                                "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                     name = "Ocean Age") +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                    name = "Ocean Age") +
  labs(
    title = "Predicted length-at-age over time",
    # subtitle = "Solid grey lines represent 5-year average length",
    x = "Brood Year",
    y = "Predicted length (mm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold")
  ) +
  facet_wrap(~ocean_age, scales = "free_y")

predicted_plot_facet

# FECUNDITY PLOTS ======

# Predicted fecundity Faceted ====== 
predicted_fecundity_facet <- ggplot(combined_effects %>% filter(!ocean_age %in% c(1,5)), 
                                    aes(x = year, y = predicted_fecundity, 
                                        color = factor(ocean_age), 
                                        fill = factor(ocean_age))) +
  # Model predictions
  geom_ribbon(aes(ymin = predicted_fecundity_lower, ymax = predicted_fecundity_upper), 
              alpha = 0.2, color = NA) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2) +
  scale_color_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                                "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                     name = "Ocean Age") +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                    name = "Ocean Age") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Predicted fecundity-at-age over time",
    subtitle = "Based on length-fecundity relationship (1mm = 7.8 eggs)",
    x = "Brood Year",
    y = "Predicted fecundity (number of eggs)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold")
  ) +
  facet_wrap(~ocean_age, scales = "free_y")

predicted_fecundity_facet

# Create boxplot - FECUNDITY
boxplot_fecundity_first_last <- ggplot(boxplot_data, aes(x = period, y = predicted_fecundity, 
                                                         fill = factor(ocean_age))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, coef = Inf) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                    name = "Ocean Age") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Change in Chinook salmon fecundity-at-age over time",
    subtitle = "Comparison of first 5 years vs last 5 years (1mm = 7.8 eggs)",
    x = "Time Period",
    y = "Predicted fecundity (number of eggs)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray90")
  ) +
  facet_wrap(~ocean_age, scales = "free_x", labeller = labeller(ocean_age = function(x) paste("Ocean Age", x)))

boxplot_fecundity_first_last

# Save ====== 
ggsave("output/plots/OR_Coast_size_at_age_effects_ESTIMATED.png", effects_plot, 
       width = 12, height = 8, dpi = 300)
# ggsave("output/plots/size_at_age_predicted_together.png", predicted_plot_together, 
#        width = 8, height = 9, dpi = 300)
ggsave("output/plots/OR_Coast_size_at_age_predicted_facet.png", predicted_plot_facet, 
       width = 12, height = 8, dpi = 300)
ggsave("output/plots/OR_Coast_size_at_age_boxplot_first_last.png", boxplot_first_last, 
       width = 12, height = 8, dpi = 300)
ggsave("output/plots/OR_Coast_fecundity_at_age_predicted_facet.png", predicted_fecundity_facet, 
       width = 12, height = 8, dpi = 300)
ggsave("output/plots/OR_Coast_fecundity_at_age_boxplot_first_last.png", boxplot_fecundity_first_last, 
       width = 12, height = 8, dpi = 300)
 