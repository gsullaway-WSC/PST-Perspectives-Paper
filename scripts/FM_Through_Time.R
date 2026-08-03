# Plot Fisheries Mortality Through Time ==== 
# Potentially use this for the paper, see if stocks caught by state have changed through time. 
library(tidyverse)
library(here)
library(ggrepel)

data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
  filter(!year == 2023, !is.na(total_run), !total_run =="NA")
        
total_run_df<-data %>%  
        dplyr::group_by(year,population) %>%
        dplyr::summarise(total_run = sum(as.numeric(total_run)))

catch_distributions <- data %>%
  dplyr::select(c(1:39)) %>%
  gather(4:ncol(.), key = "fishery_region", value = "percent_mort") %>%
  filter(!fishery_region %in% c( "stray", 
                          "aabm_tot", 
                          "nbc_is_tot",
                          "sbc_is_tot",
                          "US_is_tot", 
                          "esc_pct", 
                          "er",  
                          "term_tot")) %>% 
  # rename fishery regions by state =====
  dplyr::mutate(broad_region = case_when(
    # Alaska
    str_detect(fishery_region, "^seak") ~ "Alaska",
    str_detect(fishery_region, "^ak_term")  ~ "Alaska",
    str_detect(fishery_region, "^US_term") & population %in% c("Chilkat","Stikine", "Taku", "Unuk") ~  "Alaska", 
    
    # if its US terminal net and the regoins are candian remove from data ---- 
    # change this so it is based off of the run, goes to that state. 
    str_detect(fishery_region, "^US_term") & population %in% c("Grays" ,"Nisqually","Green","Nooksack","Queets_fa","Hoh_fa", "Hoh_sp",
                                                               "Queets_sp","Grays_Harbor_fa","Grays_Harbor_sp", "Quillayute_fa","Skagit_sp",
                                                               "Snohomish_fa", "Skagit_fa", "Stillaguamish","Elochoman",
                                                               "Nicola","Phillips") ~ "Washington",
    str_detect(fishery_region, "^US_term") & population %in% c("Cowlitz.fa", "Elk", "South_umpqua","Coquille","Coweeman", "Lewis",
                                                               "Nehalem_fa","Siletz_fa","Siuslaw_fa","Hanford_br_w") ~ "Oregon",
    
    str_detect(fishery_region, "^US_term") & region %in% c("WCVI","SBC","NBC") ~ "filter", # US terminal should not have BC fish, this is from Will doing an expand grid most likely 
    
    # Canada
    str_detect(fishery_region, "^can_term")  ~ "British Columbia",
    str_detect(fishery_region, "^wcvi")     ~ "British Columbia",#"West Coast Vancouver Island",
    str_detect(fishery_region, "^nbc")      ~ "British Columbia", #"North Coast BC",
    str_detect(fishery_region, "^sbc")      ~ "British Columbia",
    
    # str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
    
    # Falcon (US)
    str_detect(fishery_region, "^sfalc")    ~ "Oregon",
    # str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
    
    fishery_region == "nfalc_s" ~ "Washington",
    fishery_region == "nfalc_t" ~ "Washington",
    
    # WA     
    fishery_region == "PS_n" ~ "Washington",
    fishery_region == "PS_s" ~ "Washington",
    
    fishery_region == "wac_n" ~ "Washington",
    # Other
            TRUE ~ "Check")) %>%
  filter(!broad_region == "filter") %>% # there are some bad data where the fishery region is in BC, but its labeled as US Terminal catch. 
  dplyr::mutate(percent_mort = as.numeric(percent_mort)/100) 

plot_df <- catch_distributions %>%
  left_join(total_run_df) %>% 
  filter(!is.na(percent_mort)) %>% 
  dplyr::mutate(mortality_numbers = total_run *percent_mort) %>%
  ## create population groups ==== 
dplyr::mutate(
  population_group = case_when(
    # SE Alaska
    population %in% c("Chilkat", "Taku", "Stikine", "Unuk") ~ "SE Alaska",
    
    # Northern BC
    population %in% c("Kitsumkalum", "Atnarko") ~ "Northern BC",
    
    # Vancouver Island
    population %in% c("Artlish_fa", "Kaouk_fa", "Tahsish_fa",
                      "Bedwell_fa", "Megin_fa", "Moyeha_fa",
                      "Puntledge_su", "Cowichan") ~ "Vancouver Island",
    
    # Southern BC (Fraser tributaries)
    population %in% c("Harrison", "Lower_Shuswap", "South_Thompson",
                      "Nicola", "Colonial_fa","Phillips") ~ "Southern BC",
    
    # Puget Sound
    population %in% c("Nooksack", "Skagit_sp", "Skagit_fa",
                      "Stillaguamish", "Snohomish_fa",
                      "Green", "Nisqually") ~ "Puget Sound",
    
    # Olympic Peninsula
    population %in% c("Quillayute_fa", "Hoh_fa", "Hoh_sp",
                      "Queets_fa", "Queets_sp",
                      "Grays_Harbor_fa", "Grays_Harbor_sp",
                      "Grays") ~ "Olympic Peninsula",
    
    # Columbia River
    population %in% c("Coweeman", "Elochoman", "Lewis",
                      "Hanford_br_w") ~ "Columbia River",
    
    # Oregon Coast
    population %in% c("Nehalem_fa", "Siletz_fa", "Siuslaw_fa",
                      "Coquille", "South_umpqua") ~ "Oregon Coast",
    
    TRUE ~ "Check" )) %>% 
  group_by(year, broad_region, population_group)  %>%
  dplyr::summarise(total_FM_numbers = sum(mortality_numbers),
                   total_run = sum(total_run)
                   #p_mortality = total_FM_numbers/total_run # this is exploitation rate, we arent looking at that, we are syaing, of all the fish caught this year, what was the realized allocation. 
  ) %>% # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize. 
  group_by(year,broad_region)  %>%
  dplyr::mutate(p_FM = total_FM_numbers/sum(total_FM_numbers)) %>%
  dplyr::rename(fishery_broad_region = "broad_region") %>% 
  mutate(population_group = factor(population_group, levels = c("SE Alaska","Northern BC","Southern BC",
                                                                   "Vancouver Island","Puget Sound", "Olympic Peninsula",
                                                                   "Columbia River" ,"Oregon Coast"))) %>%
  filter(year >1999)
 
# stacked bar plot
p_stacked <- ggplot(plot_df,
                    aes(x = year, y = p_FM, fill = population_group)) +
  geom_col(position = "stack", alpha = 0.9, width = 0.9) +
  facet_wrap(~fishery_broad_region, ncol = 2) +
  scale_fill_manual(
    values = c(
      "SE Alaska"        = "#7A4A8A",
      "Northern BC"      = "#4A7A50",
      "Vancouver Island" = "#6A9B5A",
      "Southern BC"      = "#8A9E7A",
      "Puget Sound"      = "#2E5F6E",
      "Olympic Peninsula"= "#4A7F8E",
      "Columbia River"   = "#6A9BA8",
      "Oregon Coast"     = "#B57A4A"
    ),
    name = "Stock Origin"
  ) +
  scale_x_continuous(breaks = seq(1979, max(plot_df$year), by = 8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    # title    = "Regional Realized Fishing Allocation for Chinook Salmon",
    # subtitle = "Each bar shows the proportion of total fishing mortality by stock origin",
    x        = "Year",
    y        = "Proportional Harvest"
  ) +
  theme_minimal( ) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 9, color = "grey40"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9)
  )

p_stacked
# ggsave("output/plots/FM_proportional_stacked_by_fishery_region.pdf",
#        p_stacked, width = 12, height = 10)


# Proportional Harvest with Different population groups =======
## Total Harvest by year, Wills Dataset and StockGroups =========
catch_distributions <- data %>%
  dplyr::select(c(1:39)) %>%
  gather(4:ncol(.), key = "fishery_region", value = "percent_mort") %>%
  filter(!fishery_region %in% c( "stray", 
                                 "aabm_tot", 
                                 "nbc_is_tot",
                                 "sbc_is_tot",
                                 "US_is_tot", 
                                 "esc_pct", 
                                 "er",  
                                 "term_tot")) %>% 
  # rename fishery regions by state =====
dplyr::mutate(broad_region = case_when(
  # Alaska
  str_detect(fishery_region, "^seak") ~ "Alaska",
  str_detect(fishery_region, "^ak_term")  ~ "Alaska",
  str_detect(fishery_region, "^US_term") & population %in% c("Chilkat","Stikine", "Taku", "Unuk") ~  "Alaska", 
  
  # if its US terminal net and the regoins are candian remove from data ---- 
  # change this so it is based off of the run, goes to that state. 
  str_detect(fishery_region, "^US_term") & population %in% c("Grays" ,"Nisqually","Green","Nooksack","Queets_fa","Hoh_fa", "Hoh_sp",
                                                             "Queets_sp","Grays_Harbor_fa","Grays_Harbor_sp", "Quillayute_fa","Skagit_sp",
                                                             "Snohomish_fa", "Skagit_fa", "Stillaguamish","Elochoman",
                                                             "Nicola","Phillips") ~ "Washington",
  str_detect(fishery_region, "^US_term") & population %in% c("Cowlitz.fa", "Elk", "South_umpqua","Coquille","Coweeman", "Lewis",
                                                             "Nehalem_fa","Siletz_fa","Siuslaw_fa","Hanford_br_w") ~ "Oregon",
  
  str_detect(fishery_region, "^US_term") & region %in% c("WCVI","SBC","NBC") ~ "filter", # US terminal should not have BC fish, this is from Will doing an expand grid most likely 
  
  # Canada
  str_detect(fishery_region, "^can_term")  ~ "British Columbia",
  str_detect(fishery_region, "^wcvi")     ~ "British Columbia",#"West Coast Vancouver Island",
  str_detect(fishery_region, "^nbc")      ~ "British Columbia", #"North Coast BC",
  str_detect(fishery_region, "^sbc")      ~ "British Columbia",
  
  # str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
  
  # Falcon (US)
  str_detect(fishery_region, "^sfalc")    ~ "Oregon",
  # str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
  
  fishery_region == "nfalc_s" ~ "Washington",
  fishery_region == "nfalc_t" ~ "Washington",
  
  # WA     
  fishery_region == "PS_n" ~ "Washington",
  fishery_region == "PS_s" ~ "Washington",
  
  fishery_region == "wac_n" ~ "Washington",
  # Other
  TRUE ~ "Check")) %>%
  filter(!broad_region == "filter") %>% # there are some bad data where the fishery region is in BC, but its labeled as US Terminal catch. 
  dplyr::mutate(percent_mort = as.numeric(percent_mort)/100) 

plot_df <- catch_distributions %>%
  left_join(total_run_df) %>% 
  filter(!is.na(percent_mort)) %>% 
  dplyr::mutate(mortality_numbers = total_run *percent_mort) %>%
mutate(
  population_group = case_when(
    
    # Southeast Alaska/Transboundary
    population %in% c("Chilkat", "Taku", "Stikine", "Unuk") ~ "Southeast Alaska/Transboundary",
    
    # North/Central British Columbia
    population %in% c("Kitsumkalum", "Atnarko") ~ "North/Central British Columbia",
    
    # West Coast Vancouver Island
    population %in% c("Artlish_fa", "Kaouk_fa", "Tahsish_fa",
                      "Bedwell_fa", "Megin_fa", "Moyeha_fa",
                      "Puntledge_su", "Cowichan") ~ "West Coast Vancouver Island",
    
    # Strait of Georgia / Fraser
    # splitting Southern BC into Fraser early/late and Strait of Georgia
    population %in% c("Harrison", "Lower_Shuswap", 
                      "South_Thompson", "Nicola") ~ "Fraser late",
    population %in% c("Colonial_fa", "Phillips")  ~ "Strait of Georgia",
    
    # Puget Sound
    population %in% c("Nooksack", "Skagit_sp", "Skagit_fa",
                      "Stillaguamish", "Snohomish_fa",
                      "Green", "Nisqually") ~ "Puget Sound",
    
    # Washington Coast
    population %in% c("Quillayute_fa", "Hoh_fa", "Hoh_sp",
                      "Queets_fa", "Queets_sp",
                      "Grays_Harbor_fa", "Grays_Harbor_sp",
                      "Grays") ~ "Washington Coast",
    
    # Columbia River — split into bright/spring-summer/tule where possible
    # Hanford upriver brights = Columbia bright
    population %in% c("Hanford_br_w")            ~ "Columbia bright",
    # Coweeman, Lewis, Elochoman are lower Columbia tules
    population %in% c("Coweeman", "Elochoman", 
                      "Lewis")                    ~ "Columbia tule",
    
    # Oregon Coast
    population %in% c("Nehalem_fa", "Siletz_fa", "Siuslaw_fa",
                      "Coquille", "South_umpqua") ~ "Oregon Coast",
    
    TRUE ~ "Check"
  )
) %>% 
  group_by(year, broad_region, population_group)  %>%
  dplyr::summarise(total_FM_numbers = sum(mortality_numbers),
                   total_run = sum(total_run)
                   #p_mortality = total_FM_numbers/total_run # this is exploitation rate, we arent looking at that, we are syaing, of all the fish caught this year, what was the realized allocation. 
  ) %>% # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize. 
  group_by(year,broad_region)  %>%
  dplyr::mutate(p_FM = total_FM_numbers/sum(total_FM_numbers)) %>%
  dplyr::rename(fishery_broad_region = "broad_region") %>% 
  # mutate(population_group = factor(population_group, levels = c("SE Alaska","Northern BC","Southern BC",
  #                                                               "Vancouver Island","Puget Sound", "Olympic Peninsula",
  #                                                               "Columbia River" ,"Oregon Coast"))) %>%
  filter(year >1999)


# stacked bar plot
p_stacked <- ggplot(plot_df,
                    aes(x = year, y = p_FM, fill = population_group)) +
  geom_col(position = "stack", alpha = 0.9, width = 0.9) +
  facet_wrap(~fishery_broad_region, ncol = 2) +
  # scale_fill_manual(
  #   values = c(
  #     "SE Alaska"        = "#7A4A8A",
  #     "Northern BC"      = "#4A7A50",
  #     "Vancouver Island" = "#6A9B5A",
  #     "Southern BC"      = "#8A9E7A",
  #     "Puget Sound"      = "#2E5F6E",
  #     "Olympic Peninsula"= "#4A7F8E",
  #     "Columbia River"   = "#6A9BA8",
  #     "Oregon Coast"     = "#B57A4A"
  #   ),
  #   name = "Stock Origin"
  # ) +
  scale_x_continuous(breaks = seq(1979, max(plot_df$year), by = 8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    # title    = "Regional Realized Fishing Allocation for Chinook Salmon",
    # subtitle = "Each bar shows the proportion of total fishing mortality by stock origin",
    x        = "Year",
    y        = "Proportional Harvest"
  ) +
  theme_minimal( ) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 9, color = "grey40"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9)
  )

p_stacked
# ggsave("output/plots/FM_proportional_stacked_by_fishery_region.pdf",
#        p_stacked, width = 12, height = 10)


# Total Harvest by year, Wills Dataset =========
## Using harvest data from above, which is based on ER from each stock mu;ltiplied by total run ======

harvest_plot <- plot_df %>% 
  group_by(year, fishery_broad_region) %>%
  dplyr::summarise(total_harvest = sum(total_FM_numbers)) 

p_harvest <- ggplot(harvest_plot,
                    aes(x = year, y = total_harvest / 1000,
                        fill = fishery_broad_region)) +
  geom_col(alpha = 0.9, width = 0.8) +
  facet_wrap(~fishery_broad_region, ncol = 2) +
  scale_fill_manual(
    values = c(
      "Alaska"           = "#7A4A8A",
      "British Columbia" = "#4A7A50",
      "Washington"       = "#2E5F6E",
      "Oregon"           = "#B57A4A"
    ),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(min(harvest_plot$year),
                                  max(harvest_plot$year), by = 4)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Harvest by Fishery Region Over Time",
    x     = "Year",
    y     = "Total Harvest (thousands of fish)"
  ) +
  theme_minimal( ) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "none"
  )

p_harvest
# ggsave("output/plots/harvest_by_region_timeseries.pdf",
#        p_harvest, width = 10, height = 8)

 
# Total Harvest and proportions by year PSC direct catch data ======
# Using different data that seems to include info from non PSC fisheries? Numbers are way higher than the other data sheet with pop specific ER's 
# Data from this page: https://psc1.shinyapps.io/ctc-shiny-app/_w_efbbad27/
# accessed on july 22 2026

harvest <- read_csv("data/AABM_ISBM_fishery_stock_composition_data_all_2026-07-22.csv") %>% 
  filter(!Year <1999) %>% 
  # reassign fishery groups : 
  mutate(
    fishery_jurisdiction = case_when(
      
      # Alaska
      FisheryName %in% c(
        "Alaska Troll",
        "Alaska Yakutat Terminal Net",
        "Alaska Net",
        "Alaska Transboundary Terminal Net",
        "Alaska Transboundary Terminal Sport",
        "Alaska Sport",
        "Yukon Yakutat Freshwater Net"
      ) ~ "Alaska",
      
      # British Columbia
      FisheryName %in% c(
        "North British Columbia Troll",
        "Central British Columbia Troll",
        "West Coast Vancouver Island Troll",
        "Strait of Georgia Troll",
        "North British Columbia Net",
        "Central British Columbia Net",
        "West Coast Vancouver Island Net",
        "Juan De Fuca Net",
        "British Columbia Transboundary Freshwater Net",
        "Central British Columbia Freshwater Net",
        "Strait of Georgia Freshwater Net",
        "Fraser Freshwater Net",
        "Johnstone Strait Net",
        "Fraser Net",
        "Central British Columbia Sport",
        "North British Columbia AABM Sport",
        "North British Columbia ISBM Sport",
        "West Coast Vancouver Island AABM Sport",
        "West Coast Vancouver Island ISBM Sport",
        "British Columbia Juan De Fuca Sport",
        "North British Columbia Freshwater Sport",
        "Central British Columbia Freshwater Sport",
        "West Coast Vancouver Island Freshwater Sport",
        "Fraser River Freshwater Sport",
        "Strait of Georgia  Sport",
        "Strait of Georgia  Freshwater Sport"
      ) ~ "British Columbia",
      
      # Washington
      FisheryName %in% c(
        "North of Falcon Troll",
        "Puget Sound North Net",
        "Puget Sound Other Net",
        "Washington Coast Net",
        "Puget Sound Freshwater Net",
        "Washington Coast Freshwater Net",
        "North of Falcon Sport",
        "Puget Sound North Sport",
        "Puget Sound Other Sport",
        "Puget Sound Freshwater Sport"
      ) ~ "Washington",
      
      # Oregon
      FisheryName %in% c(
        "South of Falcon Troll",
        "Columbia River Net",
        "Columbia River Sport",
        "South of Falcon Sport",
        "South of Falcon Freshwater Sport"
      ) ~ "Oregon",
      
      TRUE ~ "Check"
    )
  ) 

harvest_summ_total <- harvest %>%
  group_by(Year, fishery_jurisdiction) %>% 
  dplyr::summarise(total_harvest = sum(LandedCatch)) %>%
  dplyr::mutate(fishery_jurisdiction = factor(fishery_jurisdiction, levels = c("Alaska", "British Columbia",
                                                                               "Washington", "Oregon")))
p_harvest <- ggplot(harvest_summ_total,
                    aes(x = Year, y = total_harvest / 1000,
                        fill = fishery_jurisdiction)) +
  geom_col(alpha = 0.9, width = 0.8) +
  facet_wrap(~fishery_jurisdiction, ncol = 2) +
  scale_fill_manual(
    values = c(
      "Alaska"           = "#7A4A8A",
      "British Columbia" = "#4A7A50",
      "Washington"       = "#2E5F6E",
      "Oregon"           = "#B57A4A"
    ),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(min(harvest$Year),
                                  max(harvest$Year), by = 4)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Harvest by Fishery Region Over Time",
    x     = "Year",
    y     = "Total Harvest (thousands of fish)"
  ) +
  theme_minimal( ) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "none"
  )

p_harvest
# ggsave("output/plots/harvest_by_region_timeseries.pdf",
#        p_harvest, width = 10, height = 8)

## Proportional Harvest ==========
# calculate proportions within each fishery and year
# calculate proportions within each jurisdiction and year
harvest_prop <- harvest %>%
  filter(!is.na(LandedCatch),
         !fishery_jurisdiction == "Check") %>%
  group_by(Year, fishery_jurisdiction, StockGroup) %>%
  summarise(
    total_catch = sum(LandedCatch, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Year, fishery_jurisdiction) %>%
  mutate(
    total_catch_jurisdiction = sum(total_catch, na.rm = TRUE),
    prop_catch = total_catch / total_catch_jurisdiction
  ) %>%
  ungroup()

# stock group color palette — update once you paste unique StockGroups
# stock_pal <- c(
#   "Southeast Alaska/Transboundary"  = "#7A4A8A",
#   "North/Central British Columbia"  = "#4A7A50",
#   "West Coast Vancouver Island"     = "#6A9B5A",
#   "Strait of Georgia"               = "#8A9E7A",
#   "Fraser late"                     = "#2E5F6E",
#   "Fraser early"                    = "#4A7F8E",
#   "Washington Coast"                = "#6A9BA8",
#   "Puget Sound"                     = "#9ABBC8",
#   "Columbia River"                  = "#B57A4A",
#   "Oregon Coast"                    = "#C58A5A"
# )

stock_pal <- c(
  "Southeast Alaska/Transboundary"  = "#7B2D8B",  # deep purple
  "North/Central British Columbia"  = "#1A6B3C",  # dark forest green
  "West Coast Vancouver Island"     = "#E63946",  # red
  "Strait of Georgia"               = "#F4A261",  # warm orange
  "Fraser"                          = "#023E8A",  # dark navy blue
    # "Fraser late"                     = "#023E8A",  # dark navy blue
  # "Fraser early"                    = "#48CAE4",  # bright sky blue
  "Washington Coast"                = "#FFBE0B",  # bright yellow
  "Puget Sound"                     = "#3A86FF",  # bright blue
  "Columbia River"                  = "#FB5607",  # vivid orange-red
  "Oregon Coast"                    = "#8AC926"   # lime green
)

# jurisdiction order
harvest_prop <- harvest_prop %>%
  dplyr::mutate(fishery_jurisdiction = factor(fishery_jurisdiction,
                                       levels = c("Alaska", "British Columbia",
                                                  "Washington", "Oregon")),
         StockGroup = case_when(StockGroup %in% c("Columbia bright",
                                                  "Columbia spring/summer",
                                                  "Columbia tule") ~ "Columbia River",
                                StockGroup %in% c("Fraser late", "Fraser early") ~ "Fraser",
                              TRUE ~ StockGroup),
         StockGroup = factor(StockGroup, levels =c(
           "Southeast Alaska/Transboundary",
           "North/Central British Columbia",
           "West Coast Vancouver Island",
           "Strait of Georgia",
           "Fraser",
           "Washington Coast",
           "Puget Sound",
           "Columbia River",
           "Oregon Coast"
         )))

p_stacked_harvest <- ggplot(harvest_prop,
                            aes(x = Year,
                                y = prop_catch,
                                fill = StockGroup)) +
  geom_col(position = "stack", alpha = 0.9, width = 0.9) +
  facet_wrap(~fishery_jurisdiction, ncol = 2) +
  # scale_fill_viridis_d(name = "Stock Group") + 
   scale_fill_manual(values = stock_pal, name = "Stock Group") +
  scale_x_continuous(breaks = seq(min(harvest_prop$Year),
                                  max(harvest_prop$Year), by = 8)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportional Catch by Stock Group and Fishery Jurisdiction",
    x     = "Year",
    y     = "Proportion of Total Catch"
  ) +
  theme_minimal( ) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 12),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    legend.title       = element_text(size = 10, face = "bold")
  )

p_stacked_harvest
# ggsave("output/plots/harvest_prop_stacked_by_jurisdiction_stockgroup.pdf",
#        p_stacked_harvest, width = 12, height = 8)

# Geom Area Stacked Plot ==========
stacked_data <- yearly_ratios %>%
  ungroup() %>%
  dplyr::select(population, year, local_ER, other_ER) %>%
  pivot_longer(cols = c(local_ER, other_ER),
               names_to = "source", values_to = "ER") %>%
  dplyr::mutate(
    source = factor(source, 
                    levels = c("other_ER", "local_ER"),  # local plotted first = bottom of stack
                    labels = c("Other (Away)", "Local (Home)"))
  )

ggplot(stacked_data, aes(x = year, y = ER, fill = source)) +
  geom_area(position = "stack", alpha = 0.85, color = "white", linewidth = 0.2) +
  facet_wrap(~ population )+#, scales = "free_y") +
  scale_fill_manual(
    values = c("Local (Home)" = "steelblue", "Other (Away)" = "firebrick"),
    name = NULL
  ) +
  labs(
    title = "Stacked Exploitation Rate: Local vs. Other, by Indicator Stock",
    y = "Exploitation Rate",
    x = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top") #+
  # scale_y_continuous(limits = c(0,0.8)) 

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

stacked_data <- yearly_ratios %>%
  select(population, region, year, local_ER, other_ER) %>%
  pivot_longer(cols = c(local_ER, other_ER), names_to = "source", values_to = "ER") %>%
  mutate(source = factor(source, levels = c("other_ER", "local_ER"),
                         labels = c("Other (Away)", "Local (Home)"))) %>%
  left_join(region_group_lookup, by = "region")
 
# Function to build one region's column of stacked-area panels ----

# Select a few BC and WA and have the rest in the supplement. 
plot_region <- function(region_name) {
  d <- stacked_data %>% filter(region_group == region_name)
  
  ggplot(d, aes(x = year, y = ER, fill = source)) +
    geom_area(position = "stack", alpha = 0.85, color = "white", linewidth = 0.2) +
    facet_wrap(~ population, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = c("Local (Home)" = "steelblue", "Other (Away)" = "firebrick"),
                      name = NULL) +
    labs(title = region_name, y = "ER", x = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none",              # dropped here, add once at combined level
      strip.text = element_text(size = 8),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

library(patchwork)

# Build one plot per region and combine as columns ----
ak_plot   <- plot_region("AK")
bc_plot   <- plot_region("BC")
wa_plot <- plot_region("WA")
or_plot <- plot_region("OR")

combined <- (ak_plot | bc_plot | wa_plot | or_plot) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined


# Local subtracted from outside harvest plot =========
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
  annotate("text", x = -0.05, y = 15, label = "<-- Less local harvest",
           hjust = 1, size = 3.2, color = "gray30", fontface = "italic") +
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

#  Quadrant format Plot ER Differences =====
## 2009-2018 ====  
# positive = local ER > other ER (more local harvest)
# negative = other ER > local ER (more outside harvest)
er_diff_data <- yearly_ratios %>%
  filter(year > 2008 & year < 2019) %>% 
  group_by(population, region) %>%
  dplyr::summarise(
    mean_local_ER = mean(local_ER, na.rm = TRUE),
    mean_other_ER =mean(other_ER, na.rm = TRUE),
    # ER_diff = mean_local_ER - mean_other_ER,
    .groups = "drop") %>%
  # dplyr::mutate(mean_local_ER = as.numeric(scale(mean_local_ER)),
  #        mean_other_ER = as.numeric(scale(mean_other_ER))) %>% 
  left_join(region_group_lookup, by = "region") %>% # if you want region coloring/faceting
  dplyr::mutate(region_group = factor(region_group, levels = c("AK", "BC", "WA", "OR")))
 
upper_left <- er_diff_data %>%
  filter(mean_local_ER >= 0, mean_local_ER < 0.1, mean_other_ER > 0.3)

p <- ggplot(er_diff_data, aes(x = mean_local_ER, y = mean_other_ER, color = population)) +
  annotate("rect", xmin = 0, xmax = 0.1, ymin = 0.3, ymax = Inf,
           fill = "gray80", alpha = 0.3) +
  geom_point() +
  geom_text_repel(
    data = upper_left,
    aes(x = mean_local_ER, y = mean_other_ER, label = population),
    size = 3,
    inherit.aes = FALSE,
    max.overlaps = Inf
  ) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_continuous(limits = c(0, 0.5))

# make sure you have a real device open before printing
dev.new(width = 8, height = 6)
print(p)

## Compute mean ER difference per indicator stock 2019 -2022 ----
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

ggplot(er_diff_data, aes(x = ER_diff, y = reorder(population, ER_diff), fill = region_group)) +
  geom_col() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  facet_grid(region_group ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Local vs. Outside Harvest by Indicator Stock",
    subtitle = "ER Difference (Local − Other), averaged from 2019 to 2022. Positive = more local harvest",
    x = "ER Difference (Local − Other)",
    y = NULL,
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",       # redundant with facet strips
    strip.text.y = element_text(angle = 0)
  ) + 
  scale_x_continuous(limits = c(-0.45, 0.45))

 



