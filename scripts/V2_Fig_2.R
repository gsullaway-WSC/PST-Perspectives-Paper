library(here)
library(tidyverse)
 
# PSC Chinook CWT indicator stocks: local ("home") vs away exploitation rate

# This script uses the PSC/CTC Appendix C calendar-year
# mortality distributions (adult-equivalent, AEQ) to compare, for each
# indicator stock and year, how much of its total exploitation rate (ER)
# was taken by fisheries in its "local" jurisdiction vs fisheries in non-local
# jurisdictions.  
 
data <- read_csv(here("data/PSC_CTC_Chinook_master_table_long.csv"))

# Home jurisdiction for each of the 20 indicator stocks ----------------
# Stikine, Taku, and Unuk rivers originate mostly in BC even though their
# mouths (and terminal fisheries) are in Southeast Alaska, so, their home
# jurisdiction is coded as British Columbia (flagged with * in plot labels because they are transboundary).
# Columbia River stocks are coded Washington due to the specific stocks included. 

home_lookup <- tribble(
  ~population,                      ~home_jurisdiction,     ~population_clean,
  "Chilkat River",                  "Alaska",               "Chilkat",
  "Unuk River",                     "British Columbia",     "Unuk*",
  "Stikine River",                  "British Columbia",     "Stikine*",
  "Taku River",                     "British Columbia",     "Taku*",
  "Atnarko River",                  "British Columbia",     "Atnarko",
  "Cowichan River Fall",            "British Columbia",     "Cowichan (Fall)",
  "Harrison River",                 "British Columbia",     "Harrison",
  "Lower Shuswap River Summer",     "British Columbia",     "Lower Shuswap (Summer)",
  "Hoh",                            "Washington",           "Hoh",
  "Queets Fall Fingerling",         "Washington",           "Queets (Fall)",
  "Quillayute",                     "Washington",           "Quillayute",
  "Skagit Spring Fingerling",       "Washington",           "Skagit (Spring)",
  "Skagit Summer Fingerling",       "Washington",           "Skagit (Summer)",
  "Columbia River Summers",         "Washington",           "Columbia (Summers)",
  "Columbia River Upriver Bright",  "Washington",           "Columbia (URB)",
  "Hanford Wild Brights",           "Washington",           "Hanford Reach",
  "Lewis River Wild",               "Washington",           "Lewis",
  "Nehalem",                        "Oregon",                "Nehalem",
  "Siletz",                         "Oregon",                "Siletz",
  "Siuslaw",                        "Oregon",                "Siuslaw"
)

stopifnot(setequal(home_lookup$population, unique(data$population)))

region_group_lookup <- tribble(
  ~home_jurisdiction,   ~region_group,
  "Alaska",             "AK",
  "British Columbia",   "BC",
  "Washington",         "WA",
  "Oregon",             "OR"
) %>% 
  mutate(region_group = factor(region_group, levels = c("AK", "BC", "WA", "OR")))

#   Fishery jurisdiction ----------------------------
# Map the fishery to jurisdiction that actually catches the fish.
# US terminal catch (`usterm_*`) is handled separately below because which
# US jurisdiction it represents depends on the stock (e.g., WA/OR
# terminal-area fisheries for that state's own stocks.

fishery_jurisdiction_lookup <- tribble(
  ~fishery,       ~fishery_jurisdiction,
  "seak_t",       "Alaska",
  "seak_n",       "Alaska",
  "seak_s",       "Alaska",
  "seakterm_t",   "Alaska",
  "seakterm_n",   "Alaska",
  "seakterm_s",   "Alaska",
  "nbc_t",        "British Columbia",
  "nbc_s",        "British Columbia",
  "wcvi_t",       "British Columbia",
  "wcvi_s",       "British Columbia",
  "nbcis_t",      "British Columbia",
  "nbcis_n",      "British Columbia",
  "nbcis_s",      "British Columbia",
  "sbcis_t",      "British Columbia",
  "sbcis_n",      "British Columbia",
  "sbcis_s",      "British Columbia",
  "canterm_n",    "British Columbia",
  "canterm_s",    "British Columbia",
  "nfalc_t",      "Washington",
  "nfalc_s",      "Washington",
  "wac_n",        "Washington",
  "ps_n",         "Washington",
  "ps_s",         "Washington",
  "sfalc_t",      "Oregon",
  "sfalc_s",      "Oregon"
)

# US terminal (`usterm_*`) catching jurisdiction, by population.
# For AK-mouth transboundary stocks this is effectively always 0 (their
# terminal catch shows up under seakterm_*, not usterm_*).
usterm_jurisdiction_lookup <- home_lookup %>%
  mutate(usterm_jurisdiction = case_when(
    population %in% c("Chilkat River", "Unuk River", "Stikine River", "Taku River") ~ "Alaska",
    home_jurisdiction == "British Columbia" ~ "Washington",  # boundary-area US interception of BC-origin fish
    TRUE ~ home_jurisdiction                                  # WA/OR/Columbia stocks: usterm = home-state terminal catch
  )) %>%
  dplyr::select(population, usterm_jurisdiction)

fishery_cols <- data %>%
  dplyr::select(starts_with("n_"), -n_stray, -n_esc_pct) %>%
  names()

catch_long <- data %>%
  dplyr::select(population, region, calendar_year, river_mouth_lat, escapement, total_er, all_of(fishery_cols)) %>%
  pivot_longer(all_of(fishery_cols), names_to = "fishery", values_to = "mort_frac") %>%
  dplyr::mutate(fishery = str_remove(fishery, "^n_")) %>%
  filter(!is.na(mort_frac), mort_frac != 0) %>%
  left_join(fishery_jurisdiction_lookup, by = "fishery") %>%
  left_join(usterm_jurisdiction_lookup, by = "population") %>%
  dplyr::mutate(fishery_jurisdiction = if_else(str_starts(fishery, "usterm"), usterm_jurisdiction, fishery_jurisdiction)) %>%
  dplyr::select(-usterm_jurisdiction) %>%
  left_join(home_lookup, by = "population") %>%
  dplyr::mutate(is_home = fishery_jurisdiction == home_jurisdiction)

## Per-stock, annual local vs away exploitation rate  ====== 
yearly_ratios <- catch_long %>%
  group_by(population, population_clean, home_jurisdiction, river_mouth_lat, calendar_year) %>%
  summarise(
    local_ER = sum(mort_frac[is_home], na.rm = TRUE),
    other_ER = sum(mort_frac[!is_home], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(year = calendar_year) %>%
  dplyr::mutate(
    TM = local_ER + other_ER,
    local_harvest_ratio = local_ER / TM,
    away_harvest_ratio  = other_ER / TM
  ) %>%
  arrange(population, year)

# 2024 is dropped: only 8 of 20 stocks had complete mortality distributions
# reported at the time this table was pulled.
yearly_ratios <- yearly_ratios %>% filter(year >= 2000, year <= 2023)
 
# Treaty Period 1: 2009-2018 =============================================================================
er_diff_data_p1 <- yearly_ratios %>%
  filter(year >= 2009, year <= 2018) %>%
  group_by(population, population_clean, home_jurisdiction, river_mouth_lat) %>%
  summarise(
    mean_local_ER = mean(local_ER, na.rm = TRUE),
    mean_other_ER = mean(other_ER, na.rm = TRUE),
    SD_local_ER = sd(local_ER, na.rm = TRUE),
    SD_other_ER = sd(other_ER, na.rm = TRUE),
    ER_diff = mean_local_ER - mean_other_ER,
    .groups = "drop"
  ) %>%
  left_join(region_group_lookup, by = "home_jurisdiction")

# Flag using +/- 1 SD of ER_diff within this period's own distribution
p1_stats <- er_diff_data_p1 %>%
  summarise(mean_ER_diff = mean(ER_diff, na.rm = TRUE), sd_ER_diff = sd(ER_diff, na.rm = TRUE))

er_diff_data_p1 <- er_diff_data_p1 %>%
  dplyr::mutate(
    er_diff_pct = ER_diff * 100,
    er_flag = case_when(
      ER_diff < (p1_stats$mean_ER_diff - p1_stats$sd_ER_diff) ~ "External >> Local (< -1 SD)",
      ER_diff > (p1_stats$mean_ER_diff + p1_stats$sd_ER_diff) ~ "Local >> External (> +1 SD)",
      TRUE ~ "Approximately Equal (+/- 1 SD)"
    ),
    er_flag = factor(er_flag, levels = c(
      "Local >> External (> +1 SD)",
      "Approximately Equal (+/- 1 SD)",
      "External >> Local (< -1 SD)"
    ))
  )

er_diff_data_p1 %>% count(er_flag)

a <- ggplot(er_diff_data_p1, aes(x = ER_diff, y = fct_reorder(population_clean, river_mouth_lat))) +
  geom_col(fill = "#4A6B5A") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = p1_stats$mean_ER_diff - p1_stats$sd_ER_diff, linetype = "dashed", color = "darkgray") +
  geom_vline(xintercept = p1_stats$mean_ER_diff + p1_stats$sd_ER_diff, linetype = "dashed", color = "darkgray") +
  labs(
    title = "Treaty Period 2009 to 2018",
    x = "Mean ER Difference (Local - Other)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("label", x = 0.05, y = 10, label = "More 'local' harvest",
           hjust = 0, size = 3.2, fontface = "italic",
           fill = "white", color = "gray30", label.size = 0) +
  annotate("segment", x = 0.35, xend = 0.4, y = 10, yend = 10,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = "gray30", linewidth = 0.5) +
  scale_x_continuous(limits = c(-0.5, 0.5))
a

# Treaty Period 2: 2019 - 2023 (2024 removed) =============================================================================
er_diff_data_p2 <- yearly_ratios %>%
  filter(year >= 2019) %>%
  group_by(population, population_clean, home_jurisdiction, river_mouth_lat) %>%
  summarise(
    mean_local_ER = mean(local_ER, na.rm = TRUE),
    mean_other_ER = mean(other_ER, na.rm = TRUE),
    SD_local_ER = sd(local_ER, na.rm = TRUE),
    SD_other_ER = sd(other_ER, na.rm = TRUE),
    ER_diff = mean_local_ER - mean_other_ER,
    .groups = "drop"
  ) %>%
  left_join(region_group_lookup, by = "home_jurisdiction")

p2_stats <- er_diff_data_p2 %>%
  summarise(mean_ER_diff = mean(ER_diff, na.rm = TRUE), sd_ER_diff = sd(ER_diff, na.rm = TRUE))

er_diff_data_p2 <- er_diff_data_p2 %>%
  mutate(
    er_diff_pct = ER_diff * 100,
    er_flag = case_when(
      ER_diff < (p2_stats$mean_ER_diff - p2_stats$sd_ER_diff) ~ "External >> Local (< -1 SD)",
      ER_diff > (p2_stats$mean_ER_diff + p2_stats$sd_ER_diff) ~ "Local >> External (> +1 SD)",
      TRUE ~ "Approximately Equal (+/- 1 SD)"
    ),
    er_flag = factor(er_flag, levels = c(
      "Local >> External (> +1 SD)",
      "Approximately Equal (+/- 1 SD)",
      "External >> Local (< -1 SD)"
    ))
  )

er_diff_data_p2 %>% count(er_flag)

b <- ggplot(er_diff_data_p2, aes(x = ER_diff, y = fct_reorder(population_clean, river_mouth_lat))) +
  geom_col(fill = "#4A6B5A") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = p2_stats$mean_ER_diff - p2_stats$sd_ER_diff, linetype = "dashed", color = "darkgray") +
  geom_vline(xintercept = p2_stats$mean_ER_diff + p2_stats$sd_ER_diff, linetype = "dashed", color = "darkgray") +
  labs(
    title = "Treaty Period 2019 to 2023",
    x = "Mean ER Difference (Local - Other)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3)
  ) +
  annotate("label", x = 0.05, y = 10, label = "More 'local' harvest",
           hjust = 0, size = 3.2, fontface = "italic",
           fill = "white", color = "gray30", label.size = 0) +
  annotate("segment", x = 0.35, xend = 0.4, y = 10, yend = 10,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = "gray30", linewidth = 0.5) +
  scale_x_continuous(limits = c(-0.5, 0.5))
 

fig_2 <- ggpubr::ggarrange(a, b, labels = c("A.", "B."), ncol = 2)
 
#
ggsave("output/plots/Figure_2.jpeg", fig_2, width = 12, height = 7)

