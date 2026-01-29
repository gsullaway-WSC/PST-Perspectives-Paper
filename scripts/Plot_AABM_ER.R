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

## OPTION 1: Unstandardized time series showing mismatch ======
plot1_unstandardized <- ggplot(corr_df_raw, aes(x = year)) +
  geom_col(aes(y = total_run), fill = "lightblue", alpha = 0.6) +
  geom_line(aes(y = aabm_tot), color = "darkred", size = 1.2) +
  geom_point(aes(y = aabm_tot), color = "darkred", size = 2) +
  geom_hline(aes(yintercept = avg_run), linetype = "dashed", color = "blue", alpha = 0.5) +
  facet_wrap(~ population, scales = "free_y", ncol = 3) +
  labs(
    title = "Aggregate Abundance Management Fails to Protect Weak Stocks",
    subtitle = "Red line (AABM catch) doesn't scale with blue bars (actual run size)",
    y = "Abundance (number of fish)",
    x = "Year",
    caption = "Dashed line = average run size. When stocks are low, AABM catch remains high."
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )

print(plot1_unstandardized)

## OPTION 2: Exploitation rate over time  ======
plot2_exploitation <- ggplot(corr_df_raw, aes(x = year, y = exploitation_rate)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(aes(color = weak_stock), size = 2) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.5) +
  facet_wrap(~ population, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = c("TRUE" = "red", "FALSE" = "steelblue"),
    labels = c("TRUE" = "Below average run", "FALSE" = "Above average run"),
    name = "Stock Status"
  ) +
  labs(
    title = "Exploitation Rates Don't Decrease When Stocks Are Weak",
    subtitle = "Red points = below-average run years still face fishing pressure",
    y = "Exploitation Rate (%)",
    x = "Year"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot2_exploitation)

## OPTION 3: Highlight years when weak stocks face high exploitation  ======
plot3_vulnerability <- ggplot(corr_df_raw, aes(x = year)) +
  # Shade vulnerable periods (weak stock + high exploitation)
  geom_rect(
    data = corr_df_raw %>% filter(weak_stock & high_exploitation),
    aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf),
    fill = "red", alpha = 0.15
  ) +
  geom_line(aes(y = total_run, color = "Total Run"), size = 1) +
  geom_line(aes(y = aabm_tot, color = "AABM Catch"), size = 1) +
  geom_hline(aes(yintercept = avg_run), linetype = "dashed", color = "blue") +
  facet_wrap(~ population, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = c("Total Run" = "steelblue", "AABM Catch" = "darkred"),
    name = "Type"
  ) +
  labs(
    title = "Vulnerable Periods: Weak Stocks Face High Exploitation",
    subtitle = "Red shading = below-average runs overlap with above-average AABM exploitation rates",
    y = "Abundance",
    x = "Year"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot3_vulnerability)

### OPTION 3B: Highlight years when weak stocks overlap with high AABM  ======
# plot 3 used high exploitation rates but i am not sure if those are AABM or all mixed stock fishing 
unique(corr_df_raw_labeled$population)

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


# mutate(
#   population_label = factor(
#     population_label,
#     levels = unique(population_label[order(-n_vulnerable)])  # negative sign for descending
#   )
# )
# Create the plot using the labeled population variable
plot3B_vulnerability <- ggplot(corr_df_raw_labeled, aes(x = year)) +
  # Shade vulnerable periods (weak stock + high exploitation)
  geom_rect(
    data = corr_df_raw_labeled %>% filter(weak_stock & high_aabm),
    aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf),
    fill = "red", alpha = 0.15
  ) +
  geom_line(aes(y = total_run, color = "Total Run"), size = 1) +
  geom_line(aes(y = aabm_tot, color = "AABM Catch"), size = 1) +
  geom_hline(aes(yintercept = avg_run), linetype = "dashed", color = "blue") +
  facet_wrap(~ population_label, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = c("Total Run" = "steelblue", "AABM Catch" = "darkred"),
    name = "Type"
  ) +
  labs(
    title = "Vulnerable Periods: Weak Stocks Face High Exploitation",
    subtitle = "Red shading = below-average runs overlap with above-average AABM catch\nn = number of vulnerable years",
    y = "Abundance",
    x = "Year"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot3B_vulnerability)
ggsave("plots/plot3B_vulnerability.jpeg", width = 8, height =6)

## OPTION 4: Scatter plot showing exploitation rate vs stock size  ======
plot4_scatter <- ggplot(corr_df_raw, aes(x = total_run, y = exploitation_rate)) +
  geom_point(aes(color = year), size = 3, alpha = 0.7) +
  geom_vline(aes(xintercept = avg_run), linetype = "dashed", color = "red") +
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

print(plot4_scatter)

## OPTION 5: Focus on Hoh_sp and Queets_sp (the most vulnerable)  ======
problem_stocks <- corr_df_raw %>%
  filter(population %in% c("Hoh_sp", "Queets_sp"))

plot5a_problem_stocks <- ggplot(problem_stocks, aes(x = year)) +
  geom_col(aes(y = total_run), fill = "lightblue", alpha = 0.7) +
  geom_line(aes(y = aabm_tot), color = "darkred", size = 1.5) +
  geom_point(aes(y = aabm_tot), color = "darkred", size = 3) +
  geom_hline(aes(yintercept = avg_run), linetype = "dashed", color = "blue") +
  facet_wrap(~ population, scales = "free_y", ncol = 1) +
  labs(
    y = "Abundance (number of fish)", 
    title = "Most Vulnerable Stocks: Hoh Spring & Queets Spring",
    subtitle = "Catch (red) doesn't decline proportionally with run size (blue)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.title.x = element_blank()
  )

plot5b_problem_exploitation <- ggplot(problem_stocks, aes(x = year, y = exploitation_rate)) +
  geom_line(size = 1.5, color = "darkred") +
  geom_point(size = 3, color = "darkred") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "black", alpha = 0.5) +
  facet_wrap(~ population, ncol = 1) +
  labs(
    y = "Exploitation Rate (%)", 
    x = "Year",
    title = "Exploitation Rate Remains High Even in Weak Years"
  ) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 12))

# Combine the problem stock plots
plot5_combined <- plot5a_problem_stocks / plot5b_problem_exploitation

print(plot5_combined)

# OPTION 6: Summary statistics table
vulnerability_summary <- corr_df_raw %>%
  group_by(population) %>%
  summarise(
    avg_run_size = mean(total_run, na.rm = TRUE),
    avg_catch = mean(aabm_tot, na.rm = TRUE),
    avg_exploitation_rate = mean(exploitation_rate, na.rm = TRUE),
    max_exploitation_rate = max(exploitation_rate, na.rm = TRUE),
    pct_years_weak = mean(weak_stock, na.rm = TRUE) * 100,
    pct_years_weak_AND_high_exploit = mean(weak_stock & high_exploitation, na.rm = TRUE) * 100,
    correlation = cor(aabm_tot, total_run, use = "complete.obs")
  ) %>%
  arrange(desc(pct_years_weak_AND_high_exploit))

print(vulnerability_summary)

# Save key plots
ggsave("figures/aabm_weak_stocks_unstandardized.png", plot1_unstandardized, 
       width = 12, height = 8, dpi = 300)
ggsave("figures/aabm_exploitation_rates.png", plot2_exploitation, 
       width = 12, height = 8, dpi = 300)
ggsave("figures/aabm_vulnerable_periods.png", plot3_vulnerability, 
       width = 12, height = 8, dpi = 300)
ggsave("figures/aabm_problem_stocks_combined.png", plot5_combined, 
       width = 10, height = 10, dpi = 300)