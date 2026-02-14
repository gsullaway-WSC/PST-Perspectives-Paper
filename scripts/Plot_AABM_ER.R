# Plot AABM harvest versus total stock escapements 

library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(PNWColors)
library(patchwork)  

# columns are the region_ either: n=Net, s=Sport, t=Troll.

# Load data =====
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx")
# head(data)

OP_total_run_df <- data %>%  
  dplyr::select(year, population, region, total_run, er) %>% 
  filter(region == "OP") %>% 
  dplyr::mutate(season = case_when(str_ends(population, "_fa") ~ "fall",
                                   str_ends(population, "_sp") ~ "spring",
                                   TRUE ~ "NA"),
                total_run = as.numeric(total_run))   


aabmcatch <- read_csv("data/AABM_fishery_performance_data_all_2026-01-28.csv") %>% 
  filter(DataType == "catch")

## test plot run sie  ======
ggplot(data = OP_total_run_df, aes(x = year, y = total_run, group = population, color = population)) +
  geom_point() +
  geom_line() +
  facet_wrap(~population)  

## test plot aabm ====
ggplot(data = aabmcatch, aes(x = Year, y = Values, group = Fishery, color = Fishery)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Fishery) 

# Make DF ======= 
corr_df_raw <- data %>%  
  dplyr::select(year, population, region, total_run, 
                tot_mixedER, aabm_tot) %>% 
  filter(region == "OP") %>% 
  dplyr::mutate(
    season = case_when(str_ends(population, "_fa") ~ "fall",
                       str_ends(population, "_sp") ~ "spring",
                       TRUE ~ "NA"),
    total_run = as.numeric(total_run),
    tot_mixedER = as.numeric(tot_mixedER),  
    aabm_tot = as.numeric(aabm_tot) * 1000  # it is in thousands
  ) %>%
  group_by(population, region) %>%
  dplyr::mutate(
    exploitation_rate = tot_mixedER*100, #(aabm_tot / total_run) * 100,
    avg_run = mean(total_run, na.rm = TRUE),
    avg_catch = mean(aabm_tot, na.rm = TRUE),
    avg_ER = mean(exploitation_rate, na.rm = TRUE),
    weak_stock = total_run < avg_run, 
    high_exploitation = exploitation_rate > mean(exploitation_rate, na.rm = TRUE),
    high_aabm = aabm_tot > mean(aabm_tot, na.rm = TRUE), 
    # high_exploitation = exploitation_rate > median(exploitation_rate, na.rm = TRUE),
    # Flag particularly vulnerable stocks
    highlighted = if_else(population %in% c("Hoh_sp", "Queets_sp"), 
                          "Weak Stocks", "Other Stocks")
  ) %>%
  ungroup()

# Keep standardized version for correlation analysis
corr_df <- data %>%  
  dplyr::select(year, population, region, total_run, tot_mixedER, aabm_tot) %>% 
  filter(region == "OP") %>% 
  dplyr::mutate(
    season = case_when(str_ends(population, "_fa") ~ "fall",
                       str_ends(population, "_sp") ~ "spring",
                       TRUE ~ "NA"),
    total_run = as.numeric(total_run),
    tot_mixedER = as.numeric(tot_mixedER),  
    aabm_tot = as.numeric(aabm_tot) * 1000
  ) %>%
  group_by(population, region) %>% 
  dplyr::mutate(
    total_run = as.numeric(scale(total_run)),
    aabm_tot = as.numeric(scale(aabm_tot))
  ) %>%
  select(-season) 

# Correlation summary
corr_summary <- corr_df %>%
  group_by(population) %>%
  summarise(
    correlation = cor(aabm_tot, total_run, use = "complete.obs"),
    n_obs = n()
  )

print(corr_summary)

# ===== VISUALIZATION OPTIONS =====

corr_df_raw_labeled <- corr_df_raw %>%
  mutate(population = case_when( population == "Hoh_sp" ~ "Hoh S.",
                                 population == "Hoh_fa" ~ "Hoh F.",
                                 population == "Queets_sp" ~ "Queets S.",
                                 population == "Queets_fa" ~ "Queets F.",
                                 population == "Grays_Harbor_sp" ~ "Grays Harbor S.",
                                 population == "Grays_Harbor_fa" ~ "Grays Harbor F.",
                                 population == "Quillayute_fa" ~ "Quillayute F."
  )) %>% 
  group_by(population) %>%
  dplyr::mutate(
    n_vulnerable = sum(weak_stock & high_aabm, na.rm = TRUE),
    population_label = paste0(population, " (n=", n_vulnerable, ")")) %>% 
  ungroup() %>%
  dplyr::mutate(  population_label = fct_reorder(population_label, n_vulnerable, .desc = TRUE))

### 3A: Highlight years when weak stocks overlap with high AABM  ======
# plot 3 used high exploitation rates but i am not sure if those are AABM or all mixed stock fishing 

# plot3A_vulnerability <- ggplot(corr_df_raw_labeled, aes(x = year)) +
#   # Shade vulnerable periods (weak stock + high exploitation)
#   geom_rect(
#     data = corr_df_raw_labeled %>% filter(weak_stock & high_aabm),
#     aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf),
#     fill = "red", alpha = 0.15
#   ) +
#   geom_line(aes(y = total_run, color = "Total Run"), size = 1) +
#   geom_line(aes(y = aabm_tot, color = "AABM Catch"), size = 1) +
#   geom_hline(aes(yintercept = avg_run), linetype = "dashed", color = "blue") +
#   facet_wrap(~ population_label, scales = "free_y", ncol = 3) +
#   scale_color_manual(
#     values = c("Total Run" = "steelblue", "AABM Catch" = "darkred"),
#     name = "Type"
#   ) +
#   labs(
#     title = "Vulnerable Periods: Weak Stocks Face High Exploitation",
#     subtitle = "Red shading = below-average runs overlap with above-average AABM catch\nn = number of vulnerable years",
#     y = "Abundance",
#     x = "Year"
#   ) +
#   theme_bw() +
#   theme(
#     strip.text = element_text(face = "bold"),
#     legend.position = "bottom"
#   )
# 
# print(plot3A_vulnerability)
# ggsave("plots/plot3A_vulnerability.jpeg", width = 8, height =6)

## 3B with points =========
plot_aabm_points <- corr_df_raw_labeled %>%
  dplyr::mutate( ER_point_colors = case_when(weak_stock ==TRUE & high_exploitation == TRUE ~ "Vulnerable",
                                          TRUE ~ "Average"),
                 AABM_point_colors = case_when(weak_stock ==TRUE & aabm_tot == TRUE ~ "high",
                                             TRUE ~ "low"))

plot3B_vulnerability <- ggplot(plot_aabm_points, aes(x = year,y = total_run)) +
  # Shade vulnerable periods (weak stock + high exploitation)
  geom_rect(
    data = plot_aabm_points %>% filter(weak_stock & high_aabm),
    aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf),
    fill = "red", alpha = 0.15
  ) +
  geom_point( data = plot_aabm_points %>% filter(ER_point_colors == "Vulnerable"),
              aes(color = ER_point_colors)) +
  geom_line() + 
  # geom_line(aes(y = total_run, color = "Total Run", size = 1)) +
  # geom_line(aes(y = aabm_tot, color = "AABM Catch"), size = 1) +
  geom_hline(aes(yintercept = avg_run), linetype = "dashed", color = "blue") +
  facet_wrap(~ population_label, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = c("Average" = "forestgreen", "Vulnerable" = "red"),
    name = " "
  ) +
  labs(
    title = "Vulnerable Periods: Weak Stocks Face High Exploitation",
    subtitle = "Red shading/Red Points = Years with below-average runs and above-average AABM catch\nn = number of vulnerable years",
    y = "Abundance",
    x = "Year"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold")
  )
 
print(plot3B_vulnerability)
ggsave("output/plots/plot3B_vulnerability.jpeg", width = 8, height =5)

##4A: Scatter plot showing exploitation rate vs stock size  ======
plot4A_scatter <- ggplot( corr_df_raw, 
                         aes(x = total_run, y = exploitation_rate)) +
  # geom_rect(aes(xmin = -Inf, xmax = 0, ymin = avg_ER, ymax = Inf),
  #           fill = "red", alpha = 0.02, inherit.aes = FALSE) +
  geom_point(aes(color = year), size = 3, alpha = 0.7) +
  # geom_vline(aes(xintercept = avg_run), linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  facet_wrap(~ population, scales = "free", ncol = 3) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Exploitation Rate vs Stock Size",
    subtitle = "Dashed line = average run size. Ideal pattern: high exploitation at high abundance, low at low abundance",
    x = "Total Run Size",
    y = "Exploitation Rate (%)",
    color = "Year"
  ) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"))

print(plot4A_scatter)

ggsave("output/plots/scatter_exploitationVSstocksize.jpg", plot4A_scatter, 
       width = 12, height = 8, dpi = 300)

## 4B: Scatter plot with quadrant, mean scale exploitation rate + stock size  ======
plot4B_scatter <- ggplot( corr_df_raw %>% 
                           group_by(population) %>% 
                           dplyr::mutate(total_run= as.numeric(scale(total_run))),
                         aes(x = total_run, y = exploitation_rate)) +
  # Add quadrant backgrounds
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = avg_ER, ymax = Inf),
            fill = "red", alpha = 0.02, inherit.aes = FALSE) +
  # geom_rect(aes(xmin = 0, xmax = Inf, ymin = avg_ER, ymax = Inf),
  #           fill = "lightgreen", alpha = 0.02, inherit.aes = FALSE) +
  geom_point(aes(color = year), size = 3, alpha = 0.7) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = avg_ER), linetype = "dashed", color = "black") +
  facet_wrap(~ population, scales = "free", ncol = 3) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Exploitation Rate vs Stock Size",
    # subtitle = "Dashed lines indicate average run size (xaxis) and average exploitation rate (yaxis).",
    x = "Total Run Size",
    y = "Exploitation Rate (%)",
    color = "Year"
  ) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "bottom")

print(plot4B_scatter)
ggsave("output/plots/quadrant_exploitationVSstocksize.jpg", plot4B_scatter, 
       width = 12, height = 8, dpi = 300)


# # OPTION 6: Summary statistics table
# vulnerability_summary <- corr_df_raw %>%
#   group_by(population) %>%
#   summarise(
#     avg_run_size = mean(total_run, na.rm = TRUE),
#     avg_catch = mean(aabm_tot, na.rm = TRUE),
#     avg_exploitation_rate = mean(exploitation_rate, na.rm = TRUE),
#     max_exploitation_rate = max(exploitation_rate, na.rm = TRUE),
#     pct_years_weak = mean(weak_stock, na.rm = TRUE) * 100,
#     pct_years_weak_AND_high_exploit = mean(weak_stock & high_exploitation, na.rm = TRUE) * 100,
#     correlation = cor(aabm_tot, total_run, use = "complete.obs")
#   ) %>%
#   arrange(desc(pct_years_weak_AND_high_exploit))
# 
# print(vulnerability_summary)



## 5 - Super Simple options =====
### Traffic light visualization ======
traffic_light_plot <- corr_df_raw_labeled %>%
  mutate(
    status = case_when(
      weak_stock & high_exploitation ~ "High Risk",
      weak_stock & !high_exploitation ~ "Caution",
      !weak_stock & high_exploitation ~ "Monitor",
      TRUE ~ "Good"
    ),
    status = factor(status, levels = c("Good", "Monitor", "Caution", "High Risk"))
  ) %>%
  ggplot(aes(x = year, y = population, fill = status)) +
  geom_tile(color = "white", size = 1) +
  scale_fill_manual(
    values = c("Good" = "#2E7D32", 
               "Monitor" = "#FDD835", 
               "Caution" = "#FB8C00",
               "High Risk" = "#C62828"),
    name = "Stock Status"
  ) +
  labs(
    title = "Salmon Stock Health Over Time",
    subtitle = "Red = Low abundance + High fishing pressure (most vulnerable)\nGreen = Healthy abundance",
    x = "Year",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(face = "bold")
  )

print(traffic_light_plot)
ggsave("output/plots/traffic_light_status.jpg", traffic_light_plot, 
       width = 10, height = 6, dpi = 300)


## Imbalance score visualization ======
imbalance_data <- corr_df_raw_labeled %>%
  group_by(population) %>%
  summarise(
    # Years when fishing was high despite low abundance
    risky_years = sum(weak_stock & high_exploitation, na.rm = TRUE),
    # Years when fishing was appropriately low with low abundance
    protective_years = sum(weak_stock & !high_exploitation, na.rm = TRUE),
    total_weak_years = sum(weak_stock, na.rm = TRUE),
    # Calculate "protection rate" when stocks are weak
    protection_rate = if_else(total_weak_years > 0, 
                              protective_years / total_weak_years * 100, 
                              NA_real_)
  ) %>%
  arrange(protection_rate)

imbalance_plot <- ggplot(imbalance_data, 
                         aes(x = reorder(population, protection_rate), 
                             y = protection_rate)) +
  geom_col(aes(fill = protection_rate), width = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray30", size = 1) +
  coord_flip() +
  scale_fill_gradient2(
    low = "#C62828", 
    mid = "#FDD835", 
    high = "#2E7D32",
    midpoint = 50,
    name = "% Protected"
  ) +
  labs(
    title = "How Well Are Weak Stocks Protected?",
    subtitle = "When salmon runs are below average, how often is fishing pressure also reduced?\nAbove 50% = fishing adjusts appropriately | Below 50% = fishing stays high despite low abundance",
    x = "",
    y = "% of Low-Abundance Years with Reduced Fishing Pressure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

print(imbalance_plot)
ggsave("output/plots/protection_rate_bars.jpg", imbalance_plot, 
       width = 10, height = 6, dpi = 300)

## Simplified scatter with zones ======
simple_scatter <- corr_df_raw_labeled %>%
  mutate(
    zone = case_when(
      total_run >= avg_run & exploitation_rate >= avg_ER ~ "High abundance,\nHigh fishing\n", 
      total_run >= avg_run & exploitation_rate < avg_ER ~  "High abundance,\nLow fishing\n(Low Risk)",
      total_run < avg_run & exploitation_rate >= avg_ER ~ "Low abundance,\nHigh fishing\n(High Risk)", 
      total_run < avg_run & exploitation_rate < avg_ER ~ "Low abundance,\nLow fishing\n"
    ),
    zone = factor(zone, levels = c(
      "Low abundance,\nLow fishing\n",
      "Low abundance,\nHigh fishing\n(High Risk)", 
      "High abundance,\nLow fishing\n(Low Risk)",
      "High abundance,\nHigh fishing\n" 
    ))
  ) %>%
  filter(!is.na(zone))

simple_plot <- ggplot(simple_scatter, 
                      aes(x = total_run, y = exploitation_rate)) +
  geom_vline(aes(xintercept = avg_run), linetype = "dashed", 
             color = "gray40", size = 0.8) +
  geom_hline(aes(yintercept = avg_ER), linetype = "dashed", 
             color = "gray40", size = 0.8) +
  geom_point(aes(color = zone, shape = zone), size = 4, alpha = 0.8) +
  facet_wrap(~ population, scales = "free", ncol = 3) +
  scale_color_manual(
    values = c(
      "Low abundance,\nLow fishing\n" = "#2E7D32",
      "Low abundance,\nHigh fishing\n(High Risk)" = "#C62828",
      "High abundance,\nLow fishing\n(Low Risk)" = "#1976D2",
      "High abundance,\nHigh fishing\n" = "#388E3C"
    ),
    name = ""
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 18),
    name = ""
  ) +
  labs(
    title = "Exploitation Rate vs Individual Run Size",
    # subtitle = "Dashed lines show average values | Red points = concerning pattern",
    x = "Salmon Run Size",
    y = "Exploitation Rate (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))

print(simple_plot)
ggsave("output/plots/simple_zones_scatterFACET.jpg", simple_plot, 
       width = 12, height = 8, dpi = 300)

simple_scatter_meanscale <- simple_scatter %>% 
  group_by(population) %>%
  dplyr::mutate(total_run = as.numeric(scale(total_run)),
                exploitation_rate = as.numeric(scale(exploitation_rate)))
  
simple_plot <- ggplot(simple_scatter_meanscale, 
                      aes(x = total_run, y = exploitation_rate)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", 
             color = "gray40", size = 0.8) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", 
             color = "gray40", size = 0.8) +
  geom_point(aes(color = zone, shape = population), size = 4, alpha = 0.8) +
  # facet_wrap(~ population, scales = "free", ncol = 3) +
  scale_color_manual(
    values = c(
      "Low abundance,\nLow fishing\n" = "gray",
      "Low abundance,\nHigh fishing\n(High Risk)" = "#C62828",
      "High abundance,\nLow fishing\n(Low Risk)" = "gray",
      "High abundance,\nHigh fishing\n" = "gray"
    ),
    name = ""
  ) + 
  labs(
    title = "Exploitation Rate vs OP Chinook Salmon Run Sizes",
    # subtitle = "Dashed lines show average values | Red points = concerning pattern",
    x = "Salmon Run Size (Mean-Scaled))",
    y = "Exploitation Rate (Mean-Scaled)",
    shape = "Population"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))

print(simple_plot)
ggsave("output/plots/simple_zones_scatter.jpg", simple_plot, 
       width = 12, height = 8, dpi = 300)
