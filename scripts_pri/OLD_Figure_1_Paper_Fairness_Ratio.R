library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(cowplot)
library(showtext)

# font_add_google("DM Sans", "dm_sans")
# showtext_auto()

custom_pal <- c(
  "Washington" = "#2E5F6E",    
  "Oregon"    = "#6A9BA8",  
  "British Columbia" = "#8A9E7A",  
  "Alaska" = "#4A7A50"
)

region_pal <- c(
  # British Columbia — greens
  "NBC"  = "#4A7A50",
  "SBC"  = "#6A9B5A",
  "WCVI" = "#8A9E7A",
  
  # Alaska — purples
  "SEAK" = "#7A4A8A",
  "AK"   = "#9A6AAA",
  
  # Washington — blues
  "WA"   = "#2E5F6E",
  "OP"   = "#4A7F8E",
  "PS"   = "#6A9BA8",
  
  # Oregon — oranges/browns
  "OR"   = "#B57A4A",
  "OC"   = "#C58A5A",
  "ORC"  = "#D59A6A",
  "MCR"  = "#A56A3A",
  "LCR"  = "#956030",
  "COL"  = "#854A20"
)

 
region_to_jurisdiction <- c(
  # British Columbia
  "NBC"  = "British Columbia",
  "SBC"  = "British Columbia",
  "WCVI" = "British Columbia",
  
  # Alaska
  "SEAK" = "Alaska",
  "AK"   = "Alaska",
  
  # Washington
  "WA"   = "Washington",
  "OP"   = "Washington",
  "PS"   = "Washington",    # Puget Sound
  
  # Oregon
  "MCR" = "Oregon",
  "LCR"= "Oregon",
  "OC" = "Oregon",
  "ORC" = "Oregon",
  "OR"   = "Oregon",
  "COL"  = "Oregon"         # Columbia — adjust if Columbia stocks split WA/OR
)

# load ======
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
  filter(!population %in% c("Elk", "Cowlitz.fa", "Middle_Shuswap"), !year ==2023)

total_run_jurisdictiondf <- data %>%
  mutate(jurisdiction = region_to_jurisdiction[region]) %>% 
  dplyr::group_by(year,jurisdiction) %>%
  dplyr::summarise(total_run_jurisdiction = sum(as.numeric(total_run), na.rm = TRUE))

total_run_df <- data %>%
  dplyr::group_by(year,population, region) %>%
  dplyr::summarise(total_run = sum(as.numeric(total_run, na.rm = TRUE)))

# PST wide annual totals (use distinct to avoid double counting run size)

pst_totals <- data %>%
  group_by(year) %>%
  dplyr::summarise(
    pst_total_run = sum(as.numeric(total_run), na.rm = TRUE)) %>%
  left_join(
    data %>%
      dplyr::mutate(total_FM_numbers= as.numeric(total_run)-as.numeric(esc_tot)) %>% 
      group_by(year) %>%
      dplyr::summarise(pst_total_harvest = sum(as.numeric(total_FM_numbers), na.rm = TRUE))) 

catch_distributions <- data %>%
  dplyr::select(c(1:39)) %>%
  gather(4:ncol(.), key = "fishery_region", value = "percent_mort") %>%
  filter(!fishery_region %in% c(
    "stray", 
    "aabm_tot", 
    "nbc_is_tot",
    "sbc_is_tot",
    "US_is_tot", 
    "esc_pct", 
    "er",  
    "term_tot")) %>%
  dplyr::mutate(percent_mort = as.numeric(percent_mort) / 100) %>%
  left_join(total_run_df) %>%
  filter(!is.na(percent_mort)) %>%
  dplyr::mutate(
    mortality_numbers = total_run * percent_mort,
    broad_fishing_region = case_when(
      str_detect(fishery_region, "^seak") ~ "Alaska",
      str_detect(fishery_region, "^ak_term") ~ "Alaska",
      str_detect(fishery_region, "^US_term") & population %in% c("Chilkat", "Stikine", "Taku", "Unuk") ~ "Alaska",
      str_detect(fishery_region, "^US_term") & population %in% c("Grays", "Nisqually", "Green", "Nooksack", "Queets_fa", "Hoh_fa", "Hoh_sp",
                                                                 "Queets_sp", "Grays_Harbor_fa", "Grays_Harbor_sp", "Quillayute_fa", "Skagit_sp",
                                                                 "Snohomish_fa", "Skagit_fa", "Stillaguamish", "Elochoman",
                                                                 "Nicola", "Phillips") ~ "Washington",
      str_detect(fishery_region, "^US_term") & population %in% c("Cowlitz.fa", "Elk", "South_umpqua", "Coquille", "Coweeman", "Lewis",
                                                                 "Nehalem_fa", "Siletz_fa", "Siuslaw_fa", "Hanford_br_w") ~ "Oregon",
      str_detect(fishery_region, "^US_term") & region %in% c("WCVI", "SBC", "NBC") ~ "filter",
      str_detect(fishery_region, "^can_term") ~ "British Columbia",
      str_detect(fishery_region, "^wcvi") ~ "British Columbia",
      str_detect(fishery_region, "^nbc") ~ "British Columbia",
      str_detect(fishery_region, "^sbc") ~ "British Columbia",
      str_detect(fishery_region, "^sfalc") ~ "Oregon",
      fishery_region == "nfalc_s" ~ "Washington",
      fishery_region == "nfalc_t" ~ "Washington",
      fishery_region == "US_is_tot" ~ "Washington",
      fishery_region == "PS_n" ~ "Washington",
      fishery_region == "PS_s" ~ "Washington",
      fishery_region == "wac_n" ~ "Washington",
      TRUE ~ "Check"
    )) %>%
  filter(!broad_fishing_region == "filter") %>% 
  group_by(broad_fishing_region, year) %>%
  dplyr::summarise(FM_Sum_region = sum(mortality_numbers,na.rm=TRUE)) %>%
  rename(jurisdiction = "broad_fishing_region" ) %>%
  left_join(total_run_jurisdictiondf, by = c("year","jurisdiction")) %>%
  dplyr::rename(fish_production = "total_run_jurisdiction" ) %>%
  # dplyr::mutate(ratio = FM_Sum_region/fish_production) %>%
  left_join(pst_totals) %>%
  mutate(harvest_share = FM_Sum_region/pst_total_harvest,
         production_share = fish_production/pst_total_run,
         ratio = harvest_share/production_share, 
         ) %>%
  filter(!year <2000)

## Line Plot  ====
# jurisdiction color palette
jurisdiction_pal <- c(
  "Alaska"           = "#7A4A8A",
  "British Columbia" = "#4A7A50",
  "Washington"       = "#2E5F6E",
  "Oregon"           = "#B57A4A"
)

# calculate mean for reference
jurisdiction_means <- catch_distributions %>%
  group_by(jurisdiction) %>%
  summarise(mean_ratio = mean(ratio, na.rm = TRUE), .groups = "drop")

# time series plot faceted by jurisdiction
ratio_time_plot <- ggplot(catch_distributions, 
                          aes(x = year, y = ratio, 
                              color = jurisdiction,
                              fill = jurisdiction)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, 
             color = "black", linewidth = 0.8) +
  # mean line per jurisdiction
  # geom_hline(data = jurisdiction_means,
  #            aes(yintercept = mean_ratio, color = jurisdiction),
  #            linetype = "dotted", linewidth = 0.8) +
  facet_wrap(~jurisdiction, scales = "free_y", ncol = 2) +
  scale_color_manual(values = jurisdiction_pal, guide = "none") +
  scale_fill_manual(values = jurisdiction_pal, guide = "none") +
  scale_x_continuous(breaks = seq(2000, max(catch_distributions$year), by = 4)) +
  labs(
    title    = "Harvest Equity Ratio by Jurisdiction Over Time",
    subtitle = "Ratio = 1 indicates harvest share proportional to production share\nRatio < 1 indicates jurisdiction receives less harvest than its production warrants\nDotted line = period mean",
    x        = "Year",
    y        = "Equity Ratio (Harvest Share / Production Share)"
  ) +
  theme_minimal() #base_family = "dm_sans") +
  # theme(
  #   plot.title       = element_text(face = "bold", size = 12),
  #   plot.subtitle    = element_text(size = 9, color = "grey40"),
  #   axis.text.x      = element_text(angle = 45, hjust = 1),
  #   panel.grid.minor = element_blank(),
  #   strip.text       = element_text(face = "bold", size = 11)
  # )

ratio_time_plot

# ggsave("output/plots/equity_ratio_jurisdiction_timeseries.pdf",
#        ratio_time_plot, width = 10, height = 8)

## Bar plot =======

catch_distributions <- catch_distributions %>%
  mutate(ratio_centered = ratio - 1)

catch_distributions_plot <- catch_distributions %>%
  mutate(
    ratio_centered = ratio - 1,
    fill_color = case_when(
      jurisdiction == "Alaska"           & ratio_centered >= 0 ~ "#9A6AAA",
      jurisdiction == "Alaska"           & ratio_centered <  0 ~ "#4A1A5A",
      jurisdiction == "British Columbia" & ratio_centered >= 0 ~ "#8A9E7A",
      jurisdiction == "British Columbia" & ratio_centered <  0 ~ "#2A5A30",
      jurisdiction == "Washington"       & ratio_centered >= 0 ~ "#6A9BA8",
      jurisdiction == "Washington"       & ratio_centered <  0 ~ "#1A3F4E",
      jurisdiction == "Oregon"           & ratio_centered >= 0 ~ "#C58A5A",
      jurisdiction == "Oregon"           & ratio_centered <  0 ~ "#7A3A10"
    )
  )

jurisdiction_means <- catch_distributions_plot %>%
  group_by(jurisdiction) %>%
  summarise(mean_ratio_centered = mean(ratio_centered, na.rm = TRUE), 
            .groups = "drop")

ratio_bar_plot <- ggplot(catch_distributions_plot,
                         aes(x = year, y = ratio_centered,
                             fill = fill_color)) +
  geom_col(alpha = 0.9) +
  geom_hline(yintercept = 0,  
             color = "black", linewidth = 0.8) + 
  facet_wrap(~jurisdiction, scales = "free_y", ncol = 2) +
  scale_fill_identity(
    guide  = "legend",
    name   = "Status",
    labels = c(
      "#9A6AAA" = "Alaska: above",
      "#4A1A5A" = "Alaska: below",
      "#8A9E7A" = "BC: above",
      "#2A5A30" = "BC: below",
      "#6A9BA8" = "Washington: above",
      "#1A3F4E" = "Washington: below",
      "#C58A5A" = "Oregon: above",
      "#7A3A10" = "Oregon: below"
    ),
    breaks = c(
      "#9A6AAA", "#4A1A5A",
      "#8A9E7A", "#2A5A30",
      "#6A9BA8", "#1A3F4E",
      "#C58A5A", "#7A3A10"
    )
  ) +
  scale_x_continuous(breaks = seq(2000, max(catch_distributions$year), by = 4)) +
  labs(
    x = "Year",
    y = "Score (Harvest Share − Production Share)"
  ) +
  theme_minimal() + 
  # theme_bw(base_size = 16) +
  theme(
    # plot.title         = element_text(face = "bold", size = 18),
    # plot.subtitle      = element_text(size = 14, color = "grey40"),
    axis.text.x        = element_text(angle = 45, hjust = 1),#, size = 13),
    # axis.text.y        = element_text(size = 15),
    # axis.title         = element_text(size = 15),
    # panel.grid.minor   = element_blank(),
    # panel.grid.major.x = element_blank(),
    # strip.text         = element_text(face = "bold", size = 16),
    # legend.position    = "bottom",
    # legend.text        = element_text(size = 15),
    # legend.title       = element_text(size = 15)
  )

ratio_bar_plot
# 
# ggsave("output/plots/equity_ratio_jurisdiction_barplot.png",
#        ratio_bar_plot, width = 8, height = 6)

## Two Time Periods =====
# calculate means for two time periods
equity_period_means <- catch_distributions %>%
  mutate(
    ratio_centered = ratio - 1,
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019 ~ "2019-Present",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(jurisdiction, period) %>%
  summarise(
    mean_equity = mean(ratio_centered, na.rm = TRUE),
    sd_equity   = sd(ratio_centered, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = c("2009-2018", "2019-Present")),
    fill_color = case_when(
      jurisdiction == "Alaska"           & mean_equity >= 0 ~ "#9A6AAA",
      jurisdiction == "Alaska"           & mean_equity <  0 ~ "#4A1A5A",
      jurisdiction == "British Columbia" & mean_equity >= 0 ~ "#8A9E7A",
      jurisdiction == "British Columbia" & mean_equity <  0 ~ "#2A5A30",
      jurisdiction == "Washington"       & mean_equity >= 0 ~ "#6A9BA8",
      jurisdiction == "Washington"       & mean_equity <  0 ~ "#1A3F4E",
      jurisdiction == "Oregon"           & mean_equity >= 0 ~ "#C58A5A",
      jurisdiction == "Oregon"           & mean_equity <  0 ~ "#7A3A10"
    )
  )

# plot
equity_period_plot <- ggplot(equity_period_means,
                             aes(x = period, y = mean_equity,
                                 fill = fill_color, group = jurisdiction)) +
  geom_col(alpha = 0.9, width = 0.6) +
  geom_errorbar(aes(ymin = mean_equity - sd_equity,
                    ymax = mean_equity + sd_equity),
                width = 0.2, color = "grey40", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  annotate("text", x = 0.5, y = 0.02,
           label = "Equitable (0)",
           hjust = 0, size = 3,
           family = "dm_sans", color = "grey40") +
  # facet_wrap(~jurisdiction, ncol = 2) +
  scale_fill_identity(
    guide  = "legend",
    name   = "Equity Status",
    labels = c(
      "#9A6AAA" = "Alaska: above",
      "#4A1A5A" = "Alaska: below",
      "#8A9E7A" = "BC: above",
      "#2A5A30" = "BC: below",
      "#6A9BA8" = "Washington: above",
      "#1A3F4E" = "Washington: below",
      "#C58A5A" = "Oregon: above",
      "#7A3A10" = "Oregon: below"
    ),
    breaks = c(
      "#9A6AAA", "#4A1A5A",
      "#8A9E7A", "#2A5A30",
      "#6A9BA8", "#1A3F4E",
      "#C58A5A", "#7A3A10"
    )
  ) +
  labs(
    title    = "Mean Harvest Equity Score by Jurisdiction and Time Period",
    subtitle = "0 = equitable; positive = receiving more than production warrants;\nnegative = receiving less than production warrants\nError bars = ± 1 SD",
    x        = "Time Period",
    y        = "Mean Equity Score (Harvest Share − Production Share)"
  ) +
  theme_minimal(base_family = "dm_sans") +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, color = "grey40"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "bottom",
    axis.text.x        = element_text(size = 10)
  )

equity_period_plot

# ggsave("output/plots/equity_score_period_comparison.pdf",
#        equity_period_plot, width = 10, height = 8, units = "in")

# ALTERNATE Option 2 =======
# Above ratio is not totally correct because its just considering indicator stocks, 
# but CTC has scaled up/expanded estimates based on indicators to represent all the 
# stocks. 
## Do a ratio/each indicator stock -----

catch_distributions <- data %>%
  dplyr::select(c(1:39)) %>%
  gather(4:ncol(.), key = "fishery_region", value = "percent_mort") %>%
  filter(!fishery_region %in% c(
    "stray", 
    "aabm_tot", 
    "nbc_is_tot",
    "sbc_is_tot",
    "US_is_tot", 
    "esc_pct", 
    "er",  
    "term_tot")) %>%
  dplyr::mutate(percent_mort = as.numeric(percent_mort) / 100) %>%
  left_join(total_run_df) %>%
  filter(!is.na(percent_mort)) %>%
  dplyr::mutate(
    mortality_numbers = total_run * percent_mort,
    broad_fishing_region = case_when(
      str_detect(fishery_region, "^seak") ~ "Alaska",
      str_detect(fishery_region, "^ak_term") ~ "Alaska",
      str_detect(fishery_region, "^US_term") & population %in% c("Chilkat", "Stikine", "Taku", "Unuk") ~ "Alaska",
      str_detect(fishery_region, "^US_term") & population %in% c("Grays", "Nisqually", "Green", "Nooksack", "Queets_fa", "Hoh_fa", "Hoh_sp",
                                                                 "Queets_sp", "Grays_Harbor_fa", "Grays_Harbor_sp", "Quillayute_fa", "Skagit_sp",
                                                                 "Snohomish_fa", "Skagit_fa", "Stillaguamish", "Elochoman",
                                                                 "Nicola", "Phillips") ~ "Washington",
      str_detect(fishery_region, "^US_term") & population %in% c("Cowlitz.fa", "Elk", "South_umpqua", "Coquille", "Coweeman", "Lewis",
                                                                 "Nehalem_fa", "Siletz_fa", "Siuslaw_fa", "Hanford_br_w") ~ "Oregon",
      str_detect(fishery_region, "^US_term") & region %in% c("WCVI", "SBC", "NBC") ~ "filter",
      str_detect(fishery_region, "^can_term") ~ "British Columbia",
      str_detect(fishery_region, "^wcvi") ~ "British Columbia",
      str_detect(fishery_region, "^nbc") ~ "British Columbia",
      str_detect(fishery_region, "^sbc") ~ "British Columbia",
      str_detect(fishery_region, "^sfalc") ~ "Oregon",
      fishery_region == "nfalc_s" ~ "Washington",
      fishery_region == "nfalc_t" ~ "Washington", 
      fishery_region == "PS_n" ~ "Washington",
      fishery_region == "PS_s" ~ "Washington",
      fishery_region == "wac_n" ~ "Washington",
      TRUE ~ "Check"
    )) %>%
  filter(!broad_fishing_region == "filter") %>% 
  group_by( year,population, region, broad_fishing_region) %>%
  dplyr::summarise(FM_Sum_region = sum(mortality_numbers,na.rm=TRUE)) %>%
  dplyr::rename(jurisdiction = "broad_fishing_region" ) %>%
  # left_join(total_run_jurisdictiondf, by = c("year","jurisdiction")) %>%
  # dplyr::rename(fish_production = "total_run_jurisdiction" ) %>%
  # # dplyr::mutate(ratio = FM_Sum_region/fish_production) %>%
  # left_join(pst_totals) %>%
  # mutate(harvest_share = FM_Sum_region/pst_total_harvest,
  #        production_share = fish_production/pst_total_run,
  #        ratio = harvest_share/production_share, 
  # ) %>%
  filter(!year <2000, !FM_Sum_region==0)

## Define crosswalk for each stock's home `region`, which broad_fishing_region value(s) count as "local"? ----
home_lookup <- tribble(
  ~region,          ~jurisdiction,
  "SEAK",           "Alaska",
  "NBC",            "British Columbia",
  "CBC",            "British Columbia",
  "WCVI",           "British Columbia",    # WCVI catch attributed to "Canada", not "Alaska"
  "GEO",            "British Columbia",
  "SBC",            "British Columbia",
  "PS",             "Washington",   # example - match to your actual PS label
  "WAC",            "Washington",
  "OP",             "Washington", 
  "COL",            "Columbia River",
  "LCR",            "Columbia River",
  "MCR",            "Columbia River",
  "ORC",            "Oregon",
  "OC",             "Oregon")

## 2. Join and flag home vs away ----
catch_flagged <- catch_distributions %>%
  left_join(home_lookup, by = "region", relationship = "many-to-many") %>%
  # after join, broad_fishing_region.x = actual catch region, .y = "home" region(s)
  rename(
    broad_fishing_region = jurisdiction.x,
    home_broad_region    = jurisdiction.y
  ) %>%
  mutate(
    is_home = broad_fishing_region == home_broad_region
  )

 ## Per-population, per-year home/away ratio =========
total_run_pop <- data %>% select(year, population, total_run)

## best version of yearly rations ===========
yearly_ratios <- catch_flagged %>% 
  group_by(population, region, year) %>%
  summarise(
    HH = sum(FM_Sum_region[is_home], na.rm = TRUE),
    AH = sum(FM_Sum_region[!is_home], na.rm = TRUE)) %>%
  left_join(total_run_pop) %>% 
  dplyr::mutate(total_run = as.numeric(total_run)) %>% 
  mutate(TM = HH + AH,
    local_harvest_ratio = HH / TM,
    away_harvest_ratio = AH / TM,
    local_ER = HH/total_run,
    other_ER = AH/total_run
  ) %>%
  arrange(population, year) %>%
  filter(!region %in% c("LCR", "MCR")) %>%
  mutate(region = case_when(region == "ORC" ~ "OC",
                            TRUE ~ region ))

### plot ==== 
ggplot(yearly_ratios, aes(x = year, y = local_harvest_ratio, group = population, color = population)) +
  geom_line(alpha = 0.4) + #, color = "steelblue") +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Local Retention Ratio by Population Over Time",
    y = "Home Harvest / Total Harvest",
    x = "Year"
  ) +
  theme_minimal()

ggplot(yearly_ratios, aes(x = year, y = local_ER, group = population, color = population)) +
  geom_line(alpha = 0.4) + #, color = "steelblue") +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Local  Ratio by total run Over Time",
    y = "Home Harvest / Total Run",
    x = "Year"
  ) +
  theme_minimal()

ggplot(yearly_ratios, aes(x = year, y = away_harvest_ratio,
                          group = population, color = population)) +
  geom_line(alpha = 0.4) + #, color = "steelblue") +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "OtherRatio by total run Over Time",
    y = "Other Harvest / Total Run",
    x = "Year"
  ) +
  theme_minimal()

### Away vs Local ER ========
 
plot_data <- yearly_ratios %>%
  select(population, year, total_run, local_ER, other_ER) %>%
  pivot_longer(cols = c(local_ER, other_ER),
               names_to = "metric", values_to = "exploitation_rate")

ggplot(plot_data, aes(x = year)) +
  geom_col(aes(y = total_run / max(total_run, na.rm = TRUE)), 
           fill = "gray85", width = 1) +   # run size as background bars, scaled 0-1
  geom_line(aes(y = exploitation_rate, color = metric), linewidth = 0.9) +
  facet_wrap(~ population, scales = "free_y") +
  scale_color_manual(values = c(
    "local_ER" = "steelblue",
    "other_ER" = "firebrick"
  ), labels = c("Home", "Away")) +
  labs(
    title = "Exploitation Rate vs. Run Size Over Time",
    subtitle = "Gray bars = relative run size (scaled). Lines = home/away exploitation rate",
    x = NULL, y = "Exploitation Rate", color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Mean Scaled ER's  ======= 

plot_data <- yearly_ratios %>%
  ungroup() %>%
  select(population, year, total_run, local_ER, other_ER) %>%
  pivot_longer(cols = c(total_run, local_ER, other_ER),
               names_to = "metric", values_to = "value") %>%
  group_by(population, metric) %>%
  mutate(
    mean_scaled = scale(value),
    # mean_scaled = value / mean(value, na.rm = TRUE)
    # values now centered around 1.0 -- above 1 = above that metric's own average,
    # below 1 = below average. Comparable across metrics/populations regardless
    # of absolute magnitude.
  ) %>%
  ungroup()

ggplot(plot_data, aes(x = year, y = mean_scaled, color = metric)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ population, scales = "free_x") +  # y no longer needs to be free -- all scaled to same range
  scale_color_manual(
    values = c("total_run" = "black", "local_ER" = "steelblue", "other_ER" = "firebrick"),
    labels = c("Run Size", "Home ER", "Away ER")
  ) +
  labs(
    title = "Mean-Scaled Trends: Run Size vs. Home/Away Exploitation Rate",
    subtitle = "Values relative to each series' own mean (1.0 = average). Dashed line = average.",
    x = NULL, y = "Value / Mean", color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")



