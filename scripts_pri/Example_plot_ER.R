
# Template for Plotting Escapement vs. Exploitation Rate Plot  =============================================================================

# This script produces a faceted bar + background-shading plot that shows:
#   - Annual escapement (bars)
#   - Exploitation rate (red shading intensity)
#   - Escapement goals (dashed line)
#   - "Vulnerable" years (red points) = above-average ER & below-goal escapement
#
# All data below are simulated and can be replaced with the real datasets if your replace the "Simulated Data" section with your own data code.

library(tidyverse)

# 1. SIMULATED DATA  =============================================================================
# Real data came from two sources: annual run/escapement/ER by population, escapement goals by stock

set.seed(42)

# 1a. Stock / population definitions
# Three stocks are featured in the final plot (one row each in the facet).
# 'population' matches internal IDs; 'StockName' matches the goals table.
stocks <- tibble(
  population = c("fake_stock1"),
  StockName  = c("fake_stock1"),
  label      = c("fake_stock1")   # facet labels
)

years <- 1979:2022   # adjust to your real year range

# 1b. Simulate annual escapement and exploitation rate
# esc_tot  = total adult escapement count
# er       = exploitation rate (proportion, 0–1); typically 0.3–0.7 for OP Chinook
# aabm_tot = AABM (Area-by-Area Management) harvest total (not used in this plot
#            variant, but kept for reference)

sim_run <- expand_grid(population = stocks$population, year = years) %>%
  left_join(stocks, by = "population") %>%
  mutate(
    # Escapement: random walk around a baseline, with a declining trend
    esc_tot  = pmax(500,
                    3000 + cumsum(rnorm(n(), mean = -10, sd = 400)) +
                      rnorm(n(), sd = 200)),
    # Exploitation rate: mean ~0.46, variable year-to-year
    er       = pmin(0.95, pmax(0.05, rnorm(n(), mean = 0.46, sd = 0.12))),
    aabm_tot = esc_tot * er * runif(n(), 0.4, 0.8),   # rough harvest proxy
    region   = "OP",
    season   = if_else(str_ends(population, "_sp"), "spring", "fall")
  )

# 1c. Simulate escapement goals
# Goals change occasionally (e.g. after PSC agreements), so we create a simple
# step-function: one goal value per stock for "early" years, another for "recent."

sim_goals <- bind_rows(
  tibble(StockName = "fake_stock1",    year = 1979:2022, esc_goal = 2800))

# 2. Add Vulnerability Flags =============================================================================
# "Vulnerable" years here means escapement is BELOW the goal AND exploitation rate is
# ABOVE the stock's long-run average ER. Could also do 1SD instead of average 

plot_df <- sim_run %>%
  left_join(sim_goals, by = c("StockName", "year")) %>%
  # Flag years that missed the escapement goal
  mutate(under_goal = esc_tot < esc_goal) %>%
  # Per-stock mean ER (used to define "above average")
  group_by(StockName) %>%
  mutate(
    mean_er       = mean(er, na.rm = TRUE),
    above_avg_er  = er > mean_er,
    # Vulnerable = missed goal AND above-average ER in the same year
    vulnerable_er = under_goal & above_avg_er
  ) %>%
  ungroup() %>%
  # Rename for nicer labels
  mutate(population = label) %>%
  # Drop any year you want to exclude (e.g. incomplete data)
  filter(!year == 2023)

# 3. PLOT  =============================================================================

new_ER_Plot <- ggplot(plot_df, aes(x = year, y = esc_tot)) +
  
  # ── Background: one rectangle per year, fill = exploitation rate ──────────
  # xmin/xmax create tiles centred on each year (±0.5).
  # ymin = -Inf / ymax = Inf fills the full panel height.
  geom_rect(
    aes(xmin = year - 0.5, xmax = year + 0.5,
        ymin = -Inf,        ymax = Inf,
        fill = er),
    alpha = 0.8,
    inherit.aes = FALSE   # prevent conflict with the y aesthetic above
  ) +
  
  # This is the escapement bars
  geom_col(alpha = 0.85) +
  
# this is the escapement goal
  geom_path(aes(y = esc_goal), color = "black", linetype = 2) +
  

  # Vulnerable-year marker: red point ────────────────────────────────────
  geom_point(
    data = filter(plot_df, vulnerable_er),
    aes(x = year, y = esc_tot),
    color = "#c0392b", size = 1.5,
    inherit.aes = FALSE
  ) +
  
  # color scale for background tiles
  # scale_fill_gradient2 requires low / mid / high + midpoint.
  # Set midpoint to the mean ER so "average" years stay pale.
  scale_fill_gradient2(
    low      = "#fff5f5",   # near-white for low ER
    mid      = "#fff5f5",   # keep pale through the midpoint
    high     = "#c0392b",   # deep red for high ER
    midpoint = 0.46,        # ← update to mean(plot_df$er, na.rm=TRUE) for your data
    name     = "Exploitation Rate"
  ) +
  
  # Axis formatting
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(
    breaks = seq(1980, 2020, by = 10),
    expand = c(0, 0)
  ) +
  
# Facet: if you are plotting multiple stocks
  facet_wrap(~ population, scales = "free_y", nrow = 1) +
  
  # Labels
  labs(
    title    = "Fake Stock Escapement vs. Exploitation Rates",
    subtitle = paste(
      "Red shading = Exploitation Rate ",
      "Dashed line = Escapement Goal",
      "Red points  = Above-average ER & below-goal escapement (vulnerable years)",
      sep = "\n"
    ),
    x = "Year",
    y = "Escapement"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold",  size = 10),
    plot.title       = element_text(face = "bold",  hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 9, color = "grey40"),
    panel.grid.minor = element_blank()
  )

new_ER_Plot
