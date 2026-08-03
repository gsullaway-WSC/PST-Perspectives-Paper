library(here)
library(tidyverse)

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


# Local subtracted from outside harvest plot =========

# Organize plot by state province ========
region_group_lookup <- tribble(
  ~region, ~region_group,
  "SEAK",  "AK",
  "WCVI",  "BC",
  "NBC",   "BC",
  "CBC",   "BC",
  "SBC",   "BC",
  "WAC",   "WA",
  "PS" ,   "WA",
  "OP" ,   "WA",
  # "COL",   "OR/WA",
  "ORC",   "OR", 
  "OC",    "OR" 
)
## Compute mean ER difference per indicator stock 2009 -2018 ----
# positive = local ER > other ER (more local harvest)
# negative = other ER > local ER (more outside harvest)
er_diff_data <- yearly_ratios %>%
  filter(year > 2008 & year < 2019) %>% 
  group_by(population, region) %>%
  # dplyr::mutate(
  #   ER_diff = local_ER - other_ER 
  # ) %>%
  dplyr::summarise(
    mean_local_ER = mean(local_ER, na.rm = TRUE),
    mean_other_ER = mean(other_ER, na.rm = TRUE),
    ER_diff =  mean_local_ER-mean_other_ER,
    .groups = "drop"
  ) %>%
  left_join(region_group_lookup, by = "region") %>% # if you want region coloring/faceting
  dplyr::mutate(region_group = factor(region_group, levels = c("AK", "BC", "WA", "OR")))

## Plot: diverging horizontal bar, stocks on y, diff on x ----
ggplot(er_diff_data, aes(x = ER_diff, y = reorder(population, ER_diff))) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  # facet_grid(region_group ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Local vs. Outside Harvest by Indicator Stock",
    subtitle = "Averaged from 2009 to 2018. Positive = More Local Harvest",
    x = "ER Difference (Local-Other)",
    y = NULL,
    fill = "Stock"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
    # strip.text.y = element_text(angle = 0),
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    # panel.grid.major.y = element_blank()
  ) + 
  annotate("text", x = 0.05, y = 15, label = "More local harvest -->",
           hjust = 0, size = 3.2, color = "gray30", fontface = "italic") +
  # annotate("text", x = -0.05, y = 15, label = "<-- Less local harvest",
  #          hjust = 1, size = 3.2, color = "gray30", fontface = "italic") +
  scale_x_continuous(limits = c(-0.5, 0.5))

### Compute mean ER difference per indicator stock 2019 -2022 ----
# positive = local ER > other ER (more local harvest)
# negative = other ER > local ER (more outside harvest)
er_diff_data <- yearly_ratios %>%
  filter(!year < 2019) %>% 
  group_by(population, region) %>%
  # dplyr::mutate(
  #   ER_diff = local_ER - other_ER 
  # ) %>%
  dplyr::summarise(
    mean_local_ER = mean(local_ER, na.rm = TRUE),
    mean_other_ER = mean(other_ER, na.rm = TRUE),
    ER_diff = mean_local_ER - mean_other_ER,
    .groups = "drop"
  ) %>%
  left_join(region_group_lookup, by = "region") %>% # if you want region coloring/faceting
  dplyr::mutate(region_group = factor(region_group, levels = c("AK", "BC", "WA", "OR")))

ggplot(er_diff_data, aes(x = ER_diff, y = reorder(population, ER_diff))) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  # facet_grid(region_group ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Local vs. Outside Harvest by Indicator Stock",
    subtitle = "Averaged from 2019 to 2022. Positive = More Local Harvest",
    x = "ER Difference (Local-Other)",
    y = NULL,
    fill = "Stock"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
    # strip.text.y = element_text(angle = 0),
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    # panel.grid.major.y = element_blank()
  ) + 
  annotate("text", x = 0.05, y = 15, label = "More local harvest -->",
           hjust = 0, size = 3.2, color = "gray30", fontface = "italic") +
  annotate("text", x = -0.05, y = 15, label = "<-- Less local harvest",
           hjust = 1, size = 3.2, color = "gray30", fontface = "italic") +
  scale_x_continuous(limits = c(-0.5, 0.5))
