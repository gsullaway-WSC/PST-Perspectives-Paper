# make plot for story that has size at age for all ages, model output year effects 
results <- readRDS("output/sizeatage_LME_results.RDS")

# Extract year effects for all ages
all_year_effects <- lapply(results$models, extract_year_effects)
combined_effects <- bind_rows(all_year_effects) %>%
  group_by(ocean_age) %>%
  complete(year = full_seq(year, 1)) %>%  # Fill in missing years with NA
  ungroup()  

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

# Predicted lengths ====== 
predicted_plot <- ggplot(combined_effects, aes(x = year, y = predicted_length, 
                                               color = factor(ocean_age), 
                                               fill = factor(ocean_age))) +
  # Model predictions
  geom_ribbon(aes(ymin = predicted_lower, ymax = predicted_upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2) +
  # Add horizontal lines for first 5 and last 5 year means
  geom_segment(data = first_last_means,
               aes(x = min_year, xend = max_year, 
                   y = mean_length, yend = mean_length,
                   color = "black" ),
               size = 1.5,  linetype = "solid") +
  scale_color_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                                "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                     name = "Ocean Age") +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                    name = "Ocean Age") +
  labs(
    title = "Predicted length-at-age over time",
    subtitle = "Solid grey lines represent 5-year average length",
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
predicted_plot

## mean scaled plot ==== 
# Mean-scale the predictions
combined_effects_scale <- combined_effects %>%
  group_by(ocean_age) %>%
  mutate(
    predicted_scaled = scale(predicted_length, center = TRUE, scale = FALSE)[,1],
    predicted_scaled_lower = scale(predicted_lower, center = TRUE, scale = FALSE)[,1],
    predicted_scaled_upper = scale(predicted_upper, center = TRUE, scale = FALSE)[,1]
  ) %>%
  ungroup()

# PLOT: Mean-scaled predictions
scaled_plot <- ggplot(combined_effects_scale, aes(x = year, y = predicted_scaled, 
                                                  color = factor(ocean_age), 
                                                  fill = factor(ocean_age))) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                                "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                     name = "Ocean Age") +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#D55E00", "5" = "#CC79A7"),
                    name = "Ocean Age") +
  labs(
    title = "Mean-scaled length-at-age predicted trends",
    subtitle = "Predicted lengths centered on their means",
    x = "Brood Year",
    y = "Deviation from mean length (mm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold")
  )  

scaled_plot


# Save ====== 
ggsave("output/plots/size_at_age_effects_ESTIMATED.png", effects_plot, 
       width = 12, height = 8, dpi = 300)
ggsave("output/plots/size_at_age_predicted_ESTIMATED.png", predicted_plot, 
       width = 12, height = 8, dpi = 300)
ggsave("output/plots/size_at_age_meanscale_ESTIMATED.png", scaled_plot, 
       width = 12, height = 8, dpi = 300)