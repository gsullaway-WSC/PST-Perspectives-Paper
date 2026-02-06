library(tidyverse)
library(here)
library(readxl)
library(viridis)

# columns are the region_ either: n=Net, s=Sport, t=Troll.
col_index <- which(names(data) == "er")
col_index

data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
  filter(region == "OP")
 

total_run_df<-data %>%  
  dplyr::select( year,population,region,total_run)

catch_distributions <- data %>%
  dplyr::select(c(1:39)) %>%
  gather(4:ncol(.), key = "fishery_region", value = "percent_mort") %>%
  dplyr::mutate(percent_mort = as.numeric(percent_mort)) %>%
  # check these with will.... only want to remove things that represent duplicate mortality
  # leave in terminal fishing?? 
  filter(!fishery_region %in% c("US_is_tot",
                        "nbc_is_tot",
                        "aabm_tot",
                        "sbc_is_tot", 
                        "esc_pct",
                        "er", #   Exploitation rate 
                        "term_tot"
                        ))



# OP individual runs =====
## Seperated by Fishery Region ==== 
OP_individ_run_fish <- catch_distributions %>%
  filter(region == "OP") %>% 
  # filter(population %in% c("Quillayute_fa","Queets_fa","Hoh_fa","Hoh_sp","Queets_sp")) %>% # include Grays harbor
  dplyr::mutate(run = case_when(str_ends(population, "_fa") ~ "Fall",
                                   str_ends(population, "_sp") ~ "Spring",
                                   TRUE ~ "NA")) %>% 
  # left_join(total_run_df) %>% 
  # dplyr::mutate(total_run = as.numeric(total_run),
  #               number_mort = total_run*percent_mort) %>%
  # group_by(year, region, population,season, fishery_region) %>%
  # dplyr::summarise(total_mort = sum(number_mort),
  #                  total_run = sum(total_run)) %>% 
  dplyr::mutate(#percent_mort = total_mort/total_run,
                gear = case_when(
                  str_ends(fishery_region, "_t") ~ "Troll",
                  str_ends(fishery_region, "_n") ~ "Net",
                  str_ends(fishery_region, "_s") ~ "Sport",
                  TRUE                           ~ "NA"),
                broad_region = case_when(
                  # Alaska
                  str_detect(fishery_region, "^seak")     ~ "Southeast Alaska",
                  str_detect(fishery_region, "^ak_term")  ~ "Alaska Terminal",
                  
                  # Canada
                  str_detect(fishery_region, "^wcvi")     ~ "West Coast Vancouver Island",
                  str_detect(fishery_region, "^nbc")      ~ "North Coast BC",
                  str_detect(fishery_region, "^sbc")      ~ "South Coast BC",
                  str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
                  
                  # South of Falcon (US)
                  str_detect(fishery_region, "^sfalc")    ~ "South of Falcon",
                  str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
                  str_detect(fishery_region, "^wac")      ~ "Washington Coast",
                  str_detect(fishery_region, "^PS")       ~ "Puget Sound",
                  str_detect(fishery_region, "^US_term")  ~ "US Terminal",
                  
                  # Other
                  fishery_region == "stray"               ~ "Stray",
                  TRUE                                    ~ "NA"),
            
                # Create better labels
                fishery_label = case_when(
                  str_ends(fishery_region, "_t") ~ paste0(broad_region, " - Troll"),
                  str_ends(fishery_region, "_n") ~ paste0(broad_region, " - Net"),
                  str_ends(fishery_region, "_s") ~ paste0(broad_region, " - Sport"),
                  fishery_region == "stray"      ~ "Stray",
                  TRUE                           ~ fishery_region
                ),
                fishery_label = factor(fishery_label, levels = c(
                
                  "Southeast Alaska - Net", 
                  "Southeast Alaska - Sport",
                  "Southeast Alaska - Troll",
                  # "Alaska Terminal",
                  
                  "North Coast BC - Net",
                  "North Coast BC - Sport",
                  "North Coast BC - Troll",
                  
                  "West Coast Vancouver Island - Sport",
                  "West Coast Vancouver Island - Troll",
                  
                  "South Coast BC - Net",
                  "South Coast BC - Sport",
                  "South Coast BC - Troll",
                  
                  # "Canada Terminal",
                  
                  "Puget Sound - Net",  
                  "Puget Sound - Sport",  
                  
                  "Washington Coast - Net",
                  
                  "North of Falcon - Sport", 
                  "North of Falcon - Troll", 
                  
                  "South of Falcon - Sport",
                  "South of Falcon - Troll" 
                  # "US Terminal",
                  # "Stray",
                  # "NA"
                ))) %>%
  filter(!is.na(fishery_label))

## run a loop to make the plot for each river because itll be easier to see =====
op_pops <- unique(OP_individ_run_fish$population)
plot_list <- list()

for (i in 1:length(op_pops)) {
  name <- op_pops[[i]]
  
  plot_list[[i]] <- ggplot(OP_individ_run_fish %>% filter( population == name,
                                                           !is.na(gear), 
                                                           !gear == "NA", 
                                                           !percent_mort ==0),
                           aes(x = year,
                               y = percent_mort,
                               group = fishery_label,   # preserves original %s
                               fill  = fishery_label )) +
    geom_area(color = "black", size = 0.2, alpha = 0.9) +
    # facet_grid(gear~ season) +
    scale_y_continuous(
      limits = c(0, 100),
      expand = c(0, 0),
      name = "Percent of total run (%)"
    ) +
    scale_x_continuous(
      name = "Year",
      expand = c(0, 0)
    ) +
    labs(fill = "Fishery Region", title = paste0(name)) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1, "lines")
    ) + 
    scale_fill_viridis_d()
}

for(i in seq_along(plot_list)) {
  name = op_pops[[i]]
  ggsave(paste0("output/plots/OP_Chinook_Regions_", name, ".jpeg"),
         plot = plot_list[[i]], 
         width = 8, height = 6)
}



# OP ======= 
Oly_Pen_fish <- catch_distributions %>%
  filter(region == "OP") %>% 
  # filter(population %in% c("Quillayute_fa","Queets_fa","Hoh_fa","Hoh_sp","Queets_sp")) %>% # include Grays harbor
  dplyr::mutate(season = case_when(str_ends(population, "_fa") ~ "fall",
                                   str_ends(population, "_sp") ~ "spring",
                                   TRUE ~ "NA")) %>%
  # left_join(data %>% select( year,population,region,total_run)) %>%
  left_join(total_run_df) %>% 
  dplyr::mutate(total_run = as.numeric(total_run),
                number_mort = total_run*percent_mort) %>%
  group_by(year, region, season, fishery_region) %>%
  dplyr::summarise(total_mort = sum(number_mort),
                   total_run = sum(total_run)) %>% 
  dplyr::mutate(percent_mort = total_mort/total_run,
                gear = case_when(
                  str_ends(fishery_region, "_t") ~ "Troll",
                  str_ends(fishery_region, "_n") ~ "Net",
                  str_ends(fishery_region, "_s") ~ "Sport",
                  # fishery_region == "stray"      ~ "stray",
                  TRUE                           ~ "NA"),
                broad_region = case_when(
                  # Alaska
                  str_detect(fishery_region, "^seak")     ~ "Southeast Alaska",
                  str_detect(fishery_region, "^ak_term")  ~ "Alaska Terminal",
                  
                  # Canada
                  str_detect(fishery_region, "^wcvi")     ~ "West Coast Vancouver Island",
                  str_detect(fishery_region, "^nbc")      ~ "North Coast BC",
                  str_detect(fishery_region, "^sbc")      ~ "South Coast BC",
                  str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
                  
                  # South of Falcon (US)
                  str_detect(fishery_region, "^sfalc")    ~ "South of Falcon",
                  str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
                  str_detect(fishery_region, "^wac")      ~ "Washington Coast",
                  str_detect(fishery_region, "^PS")       ~ "Puget Sound",
                  str_detect(fishery_region, "^US_term")  ~ "US Terminal",
                  
                  # Other
                  fishery_region == "stray"               ~ "Stray",
                  TRUE                                    ~ "NA"))


OP_FM <- ggplot(Oly_Pen_fish %>% filter(!is.na(gear), !gear == "NA", !percent_mort ==0),
                aes(x = year,
                    y = percent_mort,
                    group = fishery_region,   # preserves original %s
                    fill  = broad_region )) +
  geom_area(color = "black", size = 0.2, alpha = 0.9) +
  facet_grid(gear~ season) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = c(0, 0),
    name = "Percent of total run (%)"
  ) +
  scale_x_continuous(
    name = "Year",
    expand = c(0, 0)
  ) +
  labs(fill = "Fishery Region", title = "OP Chinook Fishing Mortality") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  ) + 
  scale_fill_viridis_d()

OP_FM
ggsave("plots/OP_Chinook_FishingMortality.jpeg", width = 8, height = 6)


# Northern OR Coast ======
N_OR_fish <- catch_distributions %>% 
  filter(region == "OC") %>% 
  # left_join(data %>% select( year,population,region,total_run)) %>%
  left_join(total_run_df) %>% 
  dplyr::mutate(total_run = as.numeric(total_run),
                number_mort = total_run*percent_mort) %>%
  group_by(year, region, fishery_region) %>%
  dplyr::summarise(total_mort = sum(number_mort),
                   total_run = sum(total_run)) %>% 
  dplyr::mutate(percent_mort = total_mort/total_run,
                gear = case_when(
                  str_ends(fishery_region, "_t") ~ "Troll",
                  str_ends(fishery_region, "_n") ~ "Net",
                  str_ends(fishery_region, "_s") ~ "Sport",
                  # fishery_region == "stray"      ~ "stray",
                  TRUE                           ~ "NA"),
                broad_region = case_when(
                  # Alaska
                  str_detect(fishery_region, "^seak")     ~ "Southeast Alaska",
                  str_detect(fishery_region, "^ak_term")  ~ "Alaska Terminal",
                  
                  # Canada
                  str_detect(fishery_region, "^wcvi")     ~ "West Coast Vancouver Island",
                  str_detect(fishery_region, "^nbc")      ~ "North Coast BC",
                  str_detect(fishery_region, "^sbc")      ~ "South Coast BC",
                  str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
                  
                  # South of Falcon (US)
                  str_detect(fishery_region, "^sfalc")    ~ "South of Falcon",
                  str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
                  str_detect(fishery_region, "^wac")      ~ "Washington Coast",
                  str_detect(fishery_region, "^PS")       ~ "Puget Sound",
                  str_detect(fishery_region, "^US_term")  ~ "US Terminal",
                  
                  # Other
                  fishery_region == "stray"               ~ "Stray",
                  TRUE                                    ~ "NA"))

N_OR_FM <- ggplot(N_OR_fish %>% filter(!is.na(gear), 
                                       !gear == "NA", 
                                       !percent_mort ==0),
                aes(x = year,
                    y = percent_mort,
                    group = fishery_region,   # preserves original %s
                    fill  = broad_region )) +
  geom_area(color = "black", size = 0.2, alpha = 0.9) +
  facet_grid( ~ gear) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = c(0, 0),
    name = "Percent of total run (%)"
  ) +
  scale_x_continuous(
    name = "Year",
    expand = c(0, 0)
  ) +
  labs(fill = "Fishery Region", title = "N OR Chinook Fishing Mortality") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  ) + 
  scale_fill_viridis_d()

N_OR_FM
ggsave("plots/NOR_Chinook_FishingMortality.jpeg", width = 8, height = 6)

# Nisqually ======= 
Nisqually_fish <- catch_distributions %>%
  filter(population == "Nisqually") %>% 
  # filter(population %in% c("Quillayute_fa","Queets_fa","Hoh_fa","Hoh_sp","Queets_sp")) %>% # include Grays harbor
  # dplyr::mutate(season = case_when(str_ends(population, "_fa") ~ "fall",
  #                                  str_ends(population, "_sp") ~ "spring",
  #                                  TRUE ~ "NA")) %>%
  # left_join(data %>% select( year,population,region,total_run)) %>%
  # left_join(total_run_df) %>% 
  # dplyr::mutate(total_run = as.numeric(total_run),
  #               number_mort = total_run*percent_mort) %>%
  # group_by(year, region, season, fishery_region) %>%
  # dplyr::summarise(total_mort = sum(number_mort),
  #                  total_run = sum(total_run)) %>% 
  dplyr::mutate(#percent_mort = total_mort/total_run,
    gear = case_when(
      str_ends(fishery_region, "_t") ~ "Troll",
      str_ends(fishery_region, "_n") ~ "Net",
      str_ends(fishery_region, "_s") ~ "Sport",
      # fishery_region == "stray"      ~ "stray",
      TRUE                           ~ "NA"),
    broad_region = case_when(
      # Alaska
      str_detect(fishery_region, "^seak")     ~ "Southeast Alaska",
      str_detect(fishery_region, "^ak_term")  ~ "Alaska Terminal",
      
      # Canada
      str_detect(fishery_region, "^wcvi")     ~ "West Coast Vancouver Island",
      str_detect(fishery_region, "^nbc")      ~ "North Coast BC",
      str_detect(fishery_region, "^sbc")      ~ "South Coast BC",
      str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
      
      # South of Falcon (US)
      str_detect(fishery_region, "^sfalc")    ~ "South of Falcon",
      str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
      str_detect(fishery_region, "^wac")      ~ "Washington Coast",
      str_detect(fishery_region, "^PS")       ~ "Puget Sound",
      str_detect(fishery_region, "^US_term")  ~ "US Terminal",
      
      # Other
      fishery_region == "stray"               ~ "Stray",
      TRUE                                    ~ "NA"))


Nisqually_FM <- ggplot(Nisqually_fish %>% filter(!is.na(gear),
                                                 !gear == "NA", 
                                                 !percent_mort ==0),
                       aes(x = year,
                           y = percent_mort,
                           #group = gear#,   # preserves original %s 
                           fill  = fishery_region 
                       )) +
  geom_area(color = "black", size = 0.2, alpha = 0.9) +
  # facet_grid( ~ gear) +
  # scale_y_continuous(
  #   limits = c(0, 100),
  #   expand = c(0, 0),
  #   name = "Percent of total run (%)"
  # ) +
  # scale_x_continuous(
  #   name = "Year",
  #   expand = c(0, 0)
  # ) +
  labs(fill = "Fishery Region", title = "Nisqually Chinook Fishing Mortality") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  ) + 
  scale_fill_viridis_d()

Nisqually_FM
ggsave("plots/Nisqually_Chinook_FishingMortality.jpeg", width = 8, height = 6)



Nisqually_FM <- ggplot(Nisqually_fish %>% filter(!is.na(gear),
                                                 !gear == "NA", 
                                                 !percent_mort ==0),
                       aes(x = year,
                           y = percent_mort,
                           #group = gear#,   # preserves original %s 
                           fill  = broad_region 
                       )) +
  geom_area(color = "black", size = 0.2, alpha = 0.9) +
  facet_grid( ~ gear) +
  # scale_y_continuous(
  #   limits = c(0, 100),
  #   expand = c(0, 0),
  #   name = "Percent of total run (%)"
  # ) +
  # scale_x_continuous(
  #   name = "Year",
  #   expand = c(0, 0)
  # ) +
  labs(fill = "Fishery Region", title = "Nisqually Chinook Fishing Mortality") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  ) + 
  scale_fill_viridis_d()

Nisqually_FM
ggsave("plots/Nisqually_Chinook_FishingMortality_gear.jpeg", width = 8, height = 6)

