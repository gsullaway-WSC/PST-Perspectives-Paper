library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(cowplot) 
library(ggrepel)

# custom colors =====
custom_pal <- c(
  "Oregon Coast\nIn-River" =  "#0096C7",    
  "Oregon Coast"    =  "#8B7BB5",
  "Washington"           =  "#E8E8E8",   
  "Puget Sound"               =   "grey",  
  "British Columbia"          =  "darkgrey",  
  "Alaska"                    =  "#888888")

# custom_pal <- c(
#   "Washington Coast\nIn-River" =  "#0096C7",   # light gray
#   "Washington Coast\nOcean"    =  "#8B7BB5",
#   "South of Falcon"           =  "#7AD151", #"#2A788E",
#   "Puget Sound"               =   "#D3D3D3",  
#   "British Columbia"          =  "#57A773",
#   "Alaska"                    =  "#FDE725"  # viridis purple for Alaska
# )

# load ====== 
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
  filter(region %in% c("OC","ORC"),
         !year == 2023, !is.na(total_run), !total_run =="NA") 
 
 total_run_df<-data %>%  
  dplyr::select(year,population,region,total_run) %>%
  dplyr::mutate(total_run = as.numeric(total_run))

catch_distributions <- data %>%
  dplyr::select(c(1:39)) %>%
  gather(4:ncol(.), key = "fishery_region", value = "percent_mort") %>%
  filter(!fishery_region %in% c(#"US_is_tot",
                                "stray", 
                               "aabm_tot", 
                                "esc_pct", 
                                "er",  
                               "term_tot"
                                )) %>%
  dplyr::mutate(percent_mort = as.numeric(percent_mort)/100) 
 

# OC whole ======= 
OC_fish <- catch_distributions %>%
  left_join(total_run_df) %>% 
  filter(!is.na(percent_mort )) %>% 
  dplyr::mutate(mortality_numbers = total_run *percent_mort,
                broad_region = case_when(
                  # Alaska
                  str_detect(fishery_region, "^seak") ~ "Alaska",
                  str_detect(fishery_region, "^ak_term")  ~ "Alaska",
                  
                  str_detect(fishery_region, "^US_term")  ~ "Oregon Coast\nIn-River",
                  str_detect(fishery_region, "^term_tot")  ~ "Oregon Coast\nIn-River",

                   # Canada
                  str_detect(fishery_region, "^can_term")  ~ "British Columbia",
                  str_detect(fishery_region, "^wcvi")     ~ "British Columbia",#"West Coast Vancouver Island",
                  str_detect(fishery_region, "^nbc")      ~ "British Columbia", #"North Coast BC",
                  str_detect(fishery_region, "^sbc")      ~ "British Columbia",
                  
                  # str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
                  
                  # Falcon (US)
                  str_detect(fishery_region, "^sfalc")    ~  "Oregon Coast",#"South of Falcon",
                  # str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
                  
                  fishery_region == "nfalc_s" ~ "Washington",#"Washington Coast\nOcean",
                  fishery_region == "nfalc_t" ~  "Washington",#"Washington Coast\nOcean",
                  fishery_region == "US_is_tot" ~  "Washington",#"Washington Coast\nOcean",

                  # WA     
                  fishery_region == "PS_n" ~ "Puget Sound",
                  fishery_region == "PS_s" ~ "Puget Sound",
                  
                  fishery_region == "wac_n" ~  "Washington",#"Washington Coast\nIn-River",
                  # Other
                  TRUE                                    ~ "Check")) 

uniqueOC<- data.frame(unique(OC_fish[c("broad_region","fishery_region")]))

uniqueOC <- uniqueOC %>% 
  arrange(broad_region)
 
total_FMnumbers <- OC_fish %>%
  group_by(year) %>%
  # get annual sum of all fishery mortality from OP for the year
  dplyr::summarise(total_FM_numbers = sum(mortality_numbers))  # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize. 

OC_plot_df<- OC_fish %>% 
  dplyr::select(-c(total_run, percent_mort,fishery_region)) %>%
  ungroup() %>%
  group_by(year, broad_region) %>%
  dplyr::summarise(mort_broad_region = sum(mortality_numbers)) %>% 
  left_join(total_FMnumbers) %>%
  dplyr::mutate(percent_mort = mort_broad_region/total_FM_numbers,
                broad_region = factor(broad_region, levels = c(
                  "Oregon Coast",
                  "Oregon Coast\nIn-River",
                  "Washington", 
                  # "South of Falcon",
                  "Puget Sound",
                  "British Columbia", 
                  "Alaska" 
                  # "British Columbia", 
                  # "Puget Sound",  
                  # "South of Falcon",
                  # "Washington Coast Ocean", 
                  # "Washington Coast In-River"
                )))

# bar and pie ==== 
## 1. Prep pie chart data (last 5 years) ==========
pie_df <- OC_plot_df %>%
  filter(year >2008) %>%
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%  # match fill order
  mutate(
    cum_pos = cumsum(avg_mort) - avg_mort / 2,  # midpoint of each slice
    label = paste0(round(avg_mort * 100, 1), "%")  # format as percent
  )

## 2. Build pie chart ==========
OP_pie <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_label_repel(
    aes(y = cum_pos, label = ifelse(avg_mort >= 0.05, paste0(round(avg_mort * 100, 1), "%"), "")),
    size = 3,
    fontface = "bold",
    nudge_x = 0.6,          # push labels outward
    show.legend = FALSE,
    segment.size = 0.3
  ) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_pal) + 
  #  scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(title = "Avg 2009-2020") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )
OP_pie

# ## 2. Build pie chart 
# OP_pie <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
#   geom_col(color = "black", alpha = 0.9, width = 1) +
#   geom_text(aes(y = cum_pos, 
#                 label = ifelse(avg_mort >= 0.05, paste0(round(avg_mort * 100, 1), "%"), "") #label = label
#                 ), 
#             size = 3, 
#             fontface = "bold",
#             color = "black") +
#   coord_polar(theta = "y") +
#   scale_fill_manual(values = custom_pal) + 
# #  scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
#   labs(title = "Avg 2009-2020") +
#   theme_void() +
#   theme(
#     legend.position = "none",
#     plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
#     plot.background = element_blank()
#   )
# 
# OP_pie

##  3. Main bar chart ==================
OC_FM_stacked <- ggplot(OC_plot_df,
                            aes(x = year,
                                y = percent_mort,
                                fill = broad_region)) +
                       geom_col(color = "black", alpha = 0.9, width = 1) +
                       scale_y_continuous(
                         expand = c(0, 0),
                         name = "Proportion of Fishery Mortality",
                         labels = scales::percent_format()
                       ) +
                       scale_x_continuous(
                         name = "Year",
                         expand = c(0, 0)
                       ) +
                      scale_fill_manual(values = custom_pal) + 
                       # scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
                       labs(
                         fill = "Fishery Region",
                         title = "Oregon Coast Chinook Salmon",
                         subtitle = "Proportional Fishery Mortality by Region & Year"
                       ) +
                       theme_minimal() +
                       theme(
                         panel.grid.minor = element_blank(),
                         panel.spacing = unit(1, "lines"),
                         legend.position = "none")

## 4. Legend =============
legend <- get_legend(ggplot(OC_plot_df,
                        aes(x = year,
                            y = percent_mort,
                            fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  scale_y_continuous(
    expand = c(0, 0),
    name = "Proportion of Fishery Mortality",
    labels = scales::percent_format()
  ) +
  scale_x_continuous(
    name = "Year",
    expand = c(0, 0)
  ) +
    scale_fill_manual(values = custom_pal) + 
  # scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(
    fill = "Fishery Region",
    title = "Oregon Coast Chinook Salmon",
    subtitle = "Proportional Fishery Mortality by Region & Year"
  ) +
  theme_minimal() +
  theme( 
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.background = element_blank(),  # remove the grey border box
    legend.box.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.spacing = unit(1, "lines"),
    legend.position = c(0.88, 0.3),# 0.35),   # nudge down so it clears the pie inset
     legend.justification = c(1, 0),
    legend.title = element_text(hjust = 0.5, size = 9, 
                                face = "bold"),  # bigger title
    legend.text = element_text(size = 8),                                # bigger text
    legend.key.size = unit(0.5, "cm")                                    # bigger color boxes
  ))

## 4. Save: inset pie into upper right =========== 
rightside <- ggpubr::ggarrange(OP_pie, legend, nrow = 2,
                                heights = c(1, 1.2)) 
rightside

# Add a blank spacer on top to push content down and align pie with bar top
spacer <- ggplot() + theme_void()  # empty plot as top padding

rightside_padded <- ggpubr::ggarrange(spacer, rightside, nrow = 2,
                                      heights = c(0.2, 1))  # adjust 0.3 to move down more

final_plot <- ggpubr::ggarrange(OC_FM_stacked, rightside_padded,
                                ncol = 2,
                                widths = c(3.3, 1))
final_plot

# --- 5. Save as JPEG ---
ggsave(
  filename = "output/plots/OR_Coast_FM_stacked.jpeg",
  plot = final_plot,
  width =6,
  height =5,
  dpi = 300,
  units = "in"
)

# old ==========
## stacked bar ======             

OP_FM_stacked <- ggplot(OP_plot_df,
                        aes(x = year,
                            y = percent_mort,
                            fill = broad_region)) +
  geom_col(color = "black",alpha = 0.9, width = 1) +
  scale_y_continuous( 
    expand = c(0, 0),
    name = "Proportion of Fishery Mortality",
    labels = scales::percent_format()
  ) +
  scale_x_continuous(
    name = "Year",
    expand = c(0, 0)
  ) +
  labs(fill = "Fishery Region", title = "Olympic Peninsula Chinook Salmon", subtitle= "Proportional Fishery Mortality by Region & Year") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  ) + 
  scale_fill_viridis_d(drop = FALSE)#, option = "plasma")

OP_FM_stacked

ggsave("output/plots/OP_FM_stacked.jpeg", height = 7, width = 7)
 
# aktest <-  catch_distributions %>% 
#   filter(fishery_region %in% c("US_term_s"))
# 
# unique(catch_distributions$fishery_region)
# OP individual runs Broad Label =====
## Run Level Mortality ==== 
OP_individ_run_fish <- catch_distributions %>%
  left_join(total_run_df) %>% 
  dplyr::mutate( mortality_numbers = total_run *percent_mort)
  dplyr::mutate(
    population = case_when(population == "Grays_Harbor_fa" ~ "Grays Harbor Fall",
                           population == "Grays_Harbor_sp" ~ "Grays Harbor Spring",
                           population == "Hoh_fa" ~ "Hoh Fall", 
                           population == "Hoh_sp" ~ "Hoh Spring",
                           population == "Quillayute_fa" ~ "Quillayute Fall",
                           population == "Quillayute_sp" ~ "Quillayute Spring",
                           population == "Queets_fa" ~ "Queets Fall",
                           population == "Queets_sp" ~ "Queets Spring",
                           TRUE ~ "Check"), 
    broad_region = case_when(
      # Alaska
      str_detect(fishery_region, "^seak") ~ "Alaska",
      str_detect(fishery_region, "^ak_term")  ~ "Alaska",
      
      str_detect(fishery_region, "^US_term")  ~ "US Terminal",
      
      # Canada
      str_detect(fishery_region, "^wcvi")     ~ "West Coast Vancouver Island",
      str_detect(fishery_region, "^nbc")      ~ "British Columbia", #"North Coast BC",
      str_detect(fishery_region, "^sbc")      ~ "British Columbia",
      
      # str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
      
      # Falcon (US)
      str_detect(fishery_region, "^sfalc")    ~ "South of Falcon",
      # str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
     
      fishery_region == "nfalc_s" ~ "North of Falcon",
      fishery_region == "nfalc_t" ~ "North of Falcon",
      
      # WA     
      fishery_region == "PS_n" ~ "Puget Sound",
      fishery_region == "PS_s" ~ "Puget Sound",
      
      fishery_region == "wac_n" ~ "Washington Coast",
      # Other
      TRUE                                    ~ "Check")) %>%
 # filter(!is.na(broad_region)) %>% 
  group_by(year, population, broad_region) %>%
 # filter(!is.na(percent_mort)) %>% 
  dplyr::summarise(sum_mortality_numbers = sum(mortality_numbers))

## Stacked Bar =====
op_pops <- unique(OP_individ_run_fish$population)
plot_list <- list()

for (i in 1:length(op_pops)) {
  name <- op_pops[[i]]
  print(name)
  
  # Filter data for this population
  pop_data <- OP_individ_run_fish %>% 
    filter(population == name) 
  
  # Create all combinations of year and broad_region
  all_combos <- expand.grid(
    year = unique(pop_data$year),
    broad_region = unique(OP_individ_run_fish$broad_region),
    stringsAsFactors = FALSE
  )
  
  # Join with actual data, filling NAs with 0
  plot_data <- all_combos %>%
    left_join(pop_data, by = c("year", "broad_region")) %>%
    mutate(percent_mort = replace_na(percent_mort, 0),
           population = name) %>%
    mutate(broad_region = factor(broad_region, levels = c(
      "Alaska", 
      "North Coast BC", 
      "West Coast Vancouver Island", 
      "South Coast BC",  
      "Puget Sound - Net",  
      "Puget Sound - Sport",  
      "Washington Coast - Net",
      "North of Falcon - Sport", 
      "North of Falcon - Troll", 
      "South of Falcon"
    ))) %>%
    filter(!is.na(broad_region))  %>%
    # Calculate proportion of total mortality for each year
    group_by(year) %>%
    mutate(
      total_mort = sum(percent_mort),
      proportion_mort = if_else(total_mort > 0, percent_mort / total_mort, 0)
    ) %>%
    ungroup() 
  
  plot_list[[i]] <- ggplot(plot_data,
                           aes(x = year,
                               y = proportion_mort,
                               fill = broad_region)) +
    geom_col(color = "black", size = 0.2, alpha = 0.9, width = 1) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = c(0, 0),
      name = "Proportion of Fishery Mortality*",
      labels = scales::percent_format()
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
    scale_fill_viridis_d(drop = FALSE)
}

for(i in seq_along(plot_list)) {
  name = op_pops[[i]]
  ggsave(paste0("output/plots/OP_Chinook_Population_Stacked_", name, ".jpeg"),
         plot = plot_list[[i]], 
         width = 8, height = 6)
}

## Geom Area =====
op_pops <- unique(OP_individ_run_fish$population)
plot_list <- list()

for (i in 1:length(op_pops)) {
  name <- op_pops[[i]]
  print(name)
  
  # Filter data and complete with all broad_region levels
  plot_data <- OP_individ_run_fish %>% 
    filter(population == name) 
  
  # Create all combinations of year and broad_region
  all_combos <- expand.grid(
    year = unique(pop_data$year),
    broad_region = unique(OP_individ_run_fish$broad_region),
    stringsAsFactors = FALSE
  )
  
  # Join with actual data, filling NAs with 0
  plot_data <- all_combos %>%
    left_join(pop_data, by = c("year", "broad_region")) %>%
    mutate(percent_mort = replace_na(percent_mort, 0),
           population = name) %>%
    dplyr::mutate(broad_region = factor(broad_region, levels = c(
      "Alaska", 
      
      "North Coast BC", 
      
      "West Coast Vancouver Island", 
      
      "South Coast BC",  
      
      "Puget Sound - Net",  
      "Puget Sound - Sport",  
      
      "Washington Coast - Net",
      
      "North of Falcon - Sport", 
      "North of Falcon - Troll", 
      
      "South of Falcon"
    ))) %>%
    filter(!is.na(broad_region))  
  
  plot_list[[i]] <- ggplot(plot_data,
                           aes(x = year,
                               y = percent_mort,
                               group = broad_region,   # preserves original %s
                               fill  = broad_region )) +
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
    scale_fill_viridis_d(drop = FALSE)  # Also important: don't drop unused levels
}

for(i in seq_along(plot_list)) {
  name = op_pops[[i]]
  ggsave(paste0("output/plots/OP_Chinook_IndividPop_Area", name, ".jpeg"),
         plot = plot_list[[i]], 
         width = 8, height = 6)
}



# ## geom area =====
# OP_FM <- ggplot(Oly_Pen_fish %>% filter(!is.na(gear), !gear == "NA", !percent_mort ==0),
#                 aes(x = year,
#                     y = percent_mort,
#                     group = fishery_region,   # preserves original %s
#                     fill  = broad_region )) +
#   geom_area(color = "black", size = 0.2, alpha = 0.9) +
#   facet_grid(gear~ season) +
#   scale_y_continuous(
#     limits = c(0, 100),
#     expand = c(0, 0),
#     name = "Percent of total run (%)"
#   ) +
#   scale_x_continuous(
#     name = "Year",
#     expand = c(0, 0)
#   ) +
#   labs(fill = "Fishery Region", title = "OP Chinook Fishing Mortality") +
#   theme_minimal() +
#   theme(
#     panel.grid.minor = element_blank(),
#     panel.spacing = unit(1, "lines")
#   ) + 
#   scale_fill_viridis_d()
# 
# OP_FM
# ggsave("plots/OP_Chinook_FishingMortality.jpeg", width = 8, height = 6)
# 

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

