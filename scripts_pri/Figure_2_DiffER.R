library(here)
library(tidyverse)

region_lat_lookup <- tribble(
  ~population,        ~pop_lat, 
  # Alaska
  "Chilkat",           59.24,   # mouth near Haines, AK
  "Stikine",           56.63,   # mouth near Wrangell, AK
  "Taku",              58.33,   # mouth near Juneau, AK
  "Unuk",              55.53,   # mouth near Ketchikan, AK
  
  # British Columbia - Northern/Central
  "Kitsumkalum",       54.62,   # Skeena tributary, near Terrace
  "Atnarko",           52.35,   # Bella Coola system
  
  # British Columbia - WCVI
  "Artlish_fa",        50.10,
  "Kaouk_fa",          50.08,
  "Tahsish_fa",        50.10,
  "Colonial_fa",       49.35,   # Colonial/Cayegle R., Clayoquot Sound
  "Bedwell_fa",        49.23,   # Clayoquot Sound
  "Megin_fa",          49.28,   # Clayoquot Sound
  "Moyeha_fa",         49.28,   # Clayoquot Sound
  
  # British Columbia - SBC / Georgia Strait / Fraser
  "Cowichan",          48.80,   # Vancouver Island, Georgia Strait
  "Puntledge_su",      49.68,   # Vancouver Island, Comox
  "Harrison",          49.30,   # Fraser tributary
  "Nicola",            50.15,   # Fraser (Thompson) tributary
  "Lower_Shuswap",     50.85,   # interior BC, Thompson system
  "South_Thompson",    50.90,   # interior BC, Thompson system
  
  # Washington - Puget Sound / Coastal
  "Nooksack",          48.72,
  "Skagit_fa",         48.37,
  "Skagit_sp",         48.37,
  "Stillaguamish",     48.22,
  "Snohomish_fa",      47.92,
  "Green",             47.53,
  "Nisqually",         47.10,
  "Grays_Harbor_fa",   46.97,   # Chehalis mouth, Grays Harbor
  "Grays_Harbor_sp",   46.97,
  "Quillayute_fa",     47.90,
  "Hoh_fa",            47.75,
  "Hoh_sp",            47.75,
  "Queets_fa",         47.53,
  "Queets_sp",         47.53,
  "Phillips",          46.90,   # Willapa/Grays Harbor area - VERIFY
  
  # Washington - Lower Columbia tributaries
  "Grays",             46.30,   # Grays River, Wahkiakum Co.
  "Elochoman",         46.23,
  "Cowlitz.fa",        46.10,
  "Coweeman",          46.10,
  "Lewis",             45.85,
  "Hanford_br_w",      46.60,   # Hanford Reach, mid-Columbia - VERIFY
  
  # Oregon
  "Nehalem_fa",        45.65,
  "Siletz_fa",         44.91,
  "Siuslaw_fa",        44.02,
  "South_umpqua",      43.72,
  "Coquille",          43.12,
  "Elk",               42.75
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
  # "MCR" = "Oregon",
  # "LCR"= "Oregon",
  "OC" = "Oregon",
  "ORC" = "Oregon",
  "OR"   = "Oregon" 
  # "COL"  = "Oregon"         # Columbia — adjust if Columbia stocks split WA/OR
)

clean_population_names <- function(x) {
  str_split(x, "_") %>%
    map_chr(function(parts) {
      last <- length(parts)
      # Replace known suffix codes only in the last token
      parts[last] <- case_when(
        parts[last] == "fa" ~ "F",
        parts[last] == "sp" ~ "S",
        parts[last] == "su" ~ "Su",
        TRUE ~ str_to_title(parts[last])
      )
      # Capitalize all non-last parts too (in case of lowercase names like "umpqua")
      if (last > 1) {
        parts[1:(last - 1)] <- str_to_title(parts[1:(last - 1)])
      }
      paste(parts, collapse = " ")
    }) %>%
    # Add asterisk to specific populations (based on ORIGINAL name, before cleaning)
    {ifelse(x %in% c("Taku", "Unuk", "Stikine"), paste0(., "*"), .)}
}
 

# load ======
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
  filter(!population %in% c("Elk", "Cowlitz.fa", "Middle_Shuswap"), !year ==2023) %>% # filter because no fishing data 
  mutate(region = case_when(population %in% c( "Taku", "Stikine", "Unuk") ~ "NBC",# TRANSBOUDNARY RIVERS, BUT A MAJORITY OF THEIR SPAWNING IS IN BC! HOME REGION HERE WILL BE CONSIDERED BC. 
                            population %in% c("Hanford_br_w", "Elochoman","Grays","Coweeman","Lewis") ~ "WAC", # CR stock in WA
                            TRUE ~ region))  

# get total run size for each state 
# total_run_jurisdictiondf <- data %>%
#   dplyr::mutate(jurisdiction = region_to_jurisdiction[region]) %>% 
#   dplyr::group_by(year,jurisdiction) %>%
#   dplyr::summarise(total_run_jurisdiction = sum(as.numeric(total_run), na.rm = TRUE))

# used to calculate ER
total_run_df <- data %>%
  dplyr::group_by(year,population, region) %>%
  dplyr::summarise(total_run = sum(as.numeric(total_run, na.rm = TRUE)))
# 
# # PST wide annual totals (use distinct to avoid double counting run size)
# pst_totals <- data %>%
#   group_by(year) %>%
#   dplyr::summarise(
#     pst_total_run = sum(as.numeric(total_run), na.rm = TRUE)) %>%
#   left_join(
#     data %>%
#       dplyr::mutate(total_FM_numbers= as.numeric(total_run)-as.numeric(esc_tot)) %>% 
#       group_by(year) %>%
#       dplyr::summarise(pst_total_harvest = sum(as.numeric(total_FM_numbers), na.rm = TRUE))) 

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
      str_detect(fishery_region, "^US_term") & population %in% c("Stikine", "Taku", "Unuk", "Chilkat") ~ "Alaska",
      str_detect(fishery_region, "^US_term") & population %in% c("Grays", "Nisqually", "Green", "Nooksack", "Queets_fa", "Hoh_fa", "Hoh_sp",
                                                                 "Queets_sp", "Grays_Harbor_fa", "Grays_Harbor_sp", "Quillayute_fa", "Skagit_sp",
                                                                 "Snohomish_fa", "Skagit_fa", "Stillaguamish", "Elochoman","Lewis",
                                                                 "Coweeman", "Hanford_br_w", "Nicola", "Phillips") ~ "Washington",
      str_detect(fishery_region, "^US_term") & population %in% c("Cowlitz.fa", "Elk", "South_umpqua", "Coquille",  
                                                                 "Nehalem_fa", "Siletz_fa", "Siuslaw_fa" ) ~ "Oregon",
      str_detect(fishery_region, "^US_term") & region %in% c("WCVI", "SBC", "NBC") ~ "filter", #0's, so remove. US terminal cant represent BC. 
      str_detect(fishery_region, "^can_term") ~ "British Columbia",
      str_detect(fishery_region, "^wcvi") ~ "British Columbia",
      str_detect(fishery_region, "^nbc") ~ "British Columbia",
      str_detect(fishery_region, "^sbc") ~ "British Columbia",
      fishery_region == "nfalc_s" ~ "Washington",
      fishery_region == "nfalc_t" ~ "Washington", 
      fishery_region == "PS_n" ~ "Washington",
      fishery_region == "PS_s" ~ "Washington",
      fishery_region == "wac_n" ~ "Washington",
      str_detect(fishery_region, "^sfalc") ~ "Oregon",
      TRUE ~ "Check"
    )) %>%
  filter(!broad_fishing_region == "filter") %>% 
  group_by(year,population, region, broad_fishing_region) %>%
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

## Define crosswalk for each stock's home `region`

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
  # "COL",            "Columbia River",
  # "LCR",            "Columbia River",
  # "MCR",            "Columbia River",
  "ORC",            "Oregon",
  "OC",             "Oregon")

## Join and flag home vs away ----
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
         local_harvest_ratio = HH / TM,# fraction of total mortlaity 
         away_harvest_ratio = AH / TM,
         local_ER = HH/total_run, # exploitation rate 
         other_ER = AH/total_run
  ) %>%
  arrange(population, year) %>%
  filter(!region %in% c("LCR", "MCR")) %>%
  mutate(region = case_when(region == "ORC" ~ "OC",
                            TRUE ~ region ))


## Local subtracted from outside harvest plot =========
## Organize plot by state province ========
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

# Treaty I - Compute mean ER difference per indicator stock 2009 -2018 =============
# positive = local ER > other ER (more local harvest)
# negative = other ER > local ER (more outside harvest)
er_diff_data_p1 <- yearly_ratios %>%
  filter(year > 2008 & year < 2019) %>% 
  group_by(population, region) %>%
  # dplyr::mutate(
  #   ER_diff = local_ER - other_ER 
  # ) %>%
  dplyr::summarise(
    mean_local_ER = mean(local_ER, na.rm = TRUE),
    mean_other_ER = mean(other_ER, na.rm = TRUE),
    SD_local_ER = sd(local_ER, na.rm = TRUE),
    SD_other_ER = sd(other_ER, na.rm = TRUE),
    ER_diff =  mean_local_ER - mean_other_ER,
    .groups = "drop"
  ) %>%
  left_join(region_group_lookup, by = "region") %>% # if you want region coloring/faceting
  dplyr::mutate(region_group = factor(region_group, levels = c("AK", "BC", "WA", "OR"))) %>%
# add a flag 
  mutate(
    er_diff_pct = ER_diff * 100,  # convert to percentage points if ER is in decimal
    er_flag = case_when(
      er_diff_pct < -20  ~ "External >> Local (< -20%)", # these numbers are +/- 1 SD 
      er_diff_pct > 19   ~ "Local >> External (> 20%)",
      TRUE               ~ "Approximately Equal (-20% to 20%)"
    ),
    er_flag = factor(er_flag, levels = c(
      "Local >> External (> 20%)",
      "Approximately Equal (-20% to 20%)",
      "External >> Local (< -20%)"
    ))
  ) %>%
  left_join(region_lat_lookup, by = "population") %>%
  mutate(population_clean = clean_population_names(population))

# check distribution
er_diff_data_p1 %>%
  count(er_flag)

## Get whole SD =====
er_diff_data_p1 %>%
  summarise(
    mean_ER_diff = mean(ER_diff, na.rm = TRUE),
    sd_ER_diff   = sd(ER_diff, na.rm = TRUE),
    mean_plus_1sd  = mean_ER_diff + sd_ER_diff,
    mean_minus_1sd = mean_ER_diff - sd_ER_diff
  )

 
### Plot Treaty 1 =========
a<- ggplot(er_diff_data_p1, aes(x = ER_diff, y = fct_reorder(population_clean, pop_lat))) +
  geom_col(fill = "#4A6B5A")  + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  # facet_grid(region_group ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Treaty Period 2009 to 2018",
    # subtitle = "Averaged from 2009 to 2018. Positive = More Local Harvest",
    x = "Mean ER Difference (Local-Other)",
    y = NULL,
    fill = "Stock"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    strip.text.y = element_text(angle = 0, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA)
  ) +  
  geom_vline(xintercept = -.20, linetype = "dashed",color= "darkgray") +
  geom_vline(xintercept = .19, linetype = "dashed",color= "darkgray") +
  annotate("label", x = 0.05, y = 20, label = "More 'local' harvest",
           hjust = 0, size = 3.2, fontface = "italic",
           fill = "white", color = "gray30", label.size = 0) + 
  annotate("segment", x = 0.32, xend = 0.38, y = 20, yend = 20,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = "gray30", linewidth = 0.5) +
  scale_x_continuous(limits = c(-0.5, 0.5))
a

# Treaty II - Compute mean ER difference per indicator stock 2019 -2022 =======
# positive = local ER > other ER (more local harvest)
# negative = other ER > local ER (more outside harvest)
er_diff_data_p2 <- yearly_ratios %>%
  filter(!year < 2019) %>% 
  group_by(population, region) %>%
  # dplyr::mutate(
  #   ER_diff = local_ER - other_ER 
  # ) %>%
  dplyr::summarise(
    mean_local_ER = mean(local_ER, na.rm = TRUE),
    mean_other_ER = mean(other_ER, na.rm = TRUE),
    SD_local_ER = sd(local_ER, na.rm = TRUE),
    SD_other_ER = sd(other_ER, na.rm = TRUE),
    ER_diff = mean_local_ER - mean_other_ER,
    .groups = "drop"
  ) %>%
  left_join(region_group_lookup, by = "region") %>% # if you want region coloring/faceting
  dplyr::mutate(region_group = factor(region_group, levels = c("AK", "BC", "WA", "OR"))) %>%
# add a flag 
mutate(
  er_diff_pct = ER_diff * 100,  # convert to percentage points if ER is in decimal
  er_flag = case_when(
    er_diff_pct < -23  ~ "External >> Local (< -20%)",
    er_diff_pct > 16   ~ "Local >> External (> 20%)",
    TRUE               ~ "Approximately Equal (-20% to 20%)"
  ),
  er_flag = factor(er_flag, levels = c(
    "Local >> External (> 20%)",
    "Approximately Equal (-20% to 20%)",
    "External >> Local (< -20%)"
  ))
) %>% 
  left_join(region_lat_lookup, by = "population") %>%
  mutate(population_clean = clean_population_names(population))

# check distribution
er_diff_data_p2 %>%
  count(er_flag)

# mean and SD of ER_diff across all stocks
er_diff_data_p2 %>%
  summarise(
    mean_ER_diff = mean(ER_diff, na.rm = TRUE),
    sd_ER_diff   = sd(ER_diff, na.rm = TRUE),
    mean_plus_1sd  = mean_ER_diff + sd_ER_diff,
    mean_minus_1sd = mean_ER_diff - sd_ER_diff
  )

b <- ggplot(er_diff_data_p2, aes(x = ER_diff, y = fct_reorder(population_clean, pop_lat))) +
  geom_col(fill =  "#4A6B5A")  +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  # facet_grid(region_group ~ ., scales = "free_y", space = "free_y") +
   labs(
     title = "Treaty Period 2019 to 2022",
    # subtitle = "Averaged from 2019 to 2022. Positive = More Local Harvest",
    x = "Mean ER Difference (Local-Other)",
    y = NULL,
    fill = "Stock"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    strip.text.y = element_text(angle = 0, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = NA)
  ) + 
  geom_vline(xintercept = -.23, linetype = "dashed",color= "darkgray") +
  geom_vline(xintercept = .16, linetype = "dashed",color= "darkgray") +
  annotate("label", x = 0.05, y = 20, label = "More 'local' harvest",
           hjust = 0, size = 3.2, fontface = "italic",
           fill = "white", color = "gray30", label.size = 0) + 
  annotate("segment", x = 0.32, xend = 0.38, y = 20, yend = 20,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = "gray30", linewidth = 0.5)+
  scale_x_continuous(limits = c(-0.5, 0.5))
b

# fig_2 <- ggpubr::ggarrange(a,b, labels = c("A.", "B."), ncol =1)
# fig_2
# ggsave("output/plots/Figure_2.jpeg", width = 7, height =12)

 
fig_2 <- ggpubr::ggarrange(a,b, labels = c("A.", "B."), ncol =2)
fig_2
ggsave("output/plots/Figure_2.jpeg", width = 12, height =7)
