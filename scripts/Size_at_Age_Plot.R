# make plot for story that has size at age for all ages, model output year effects 
results <- readRDS("output/sizeatage_LME_results.RDS")
 
# Extract year effects for all ages
all_year_effects <- lapply(results$models, extract_year_effects)
combined_effects <- bind_rows(all_year_effects) %>%
  filter(!ocean_age ==5)

# Create combined plot with custom colors
combined_plot <- ggplot(combined_effects, aes(x = year, y = effect, 
                                              color = factor(ocean_age), 
                                              fill = factor(ocean_age))) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                                "3" = "#009E73", "4" = "#D55E00"),
                     name = "Ocean Age") +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#D55E00"),
                    name = "Ocean Age") +
  labs(
    title = "Temporal trends in Chinook salmon size-at-age",
    subtitle = "Year effects from LME models with 95% confidence intervals",
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

# Display
print(combined_plot)

# Save
ggsave("output/plots/size_at_age_trends_oceanages_1_4.png", combined_plot, 
       width = 12, height = 8, dpi = 300)

# Optional: Faceted version (separate panel per age)
faceted_plot <- ggplot(combined_effects, aes(x = year, y = effect)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(ocean_age)), 
              alpha = 0.3, color = NA) +
  geom_line(aes(color = factor(ocean_age)), size = 1.2) +
  geom_point(aes(color = factor(ocean_age)), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ocean_age, ncol = 1, scales = "free_y",
             labeller = labeller(ocean_age = function(x) paste("Ocean Age", x))) +
  scale_color_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                                "3" = "#009E73", "4" = "#D55E00")) +
  scale_fill_manual(values = c("1" = "#E69F00", "2" = "#56B4E9", 
                               "3" = "#009E73", "4" = "#D55E00")) +
  labs(
    title = "Temporal trends in Chinook salmon size-at-age",
    x = "Brood Year",
    y = "Year effect on length (mm)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray90")
  )

print(faceted_plot)
ggsave("output/plots/size_at_age_trends_1_4faceted.png", faceted_plot, 
       width = 12, height = 10, dpi = 300)


