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
                  fishery_region == "PS_n" ~ "Washington", # "Puget Sound",
                  fishery_region == "PS_s" ~ "Washington", # "Puget Sound",
                  
                  fishery_region == "wac_n" ~  "Washington",#"Washington Coast\n In-River",
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
                  # "Puget Sound",
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
    aes(y = cum_pos, label = ifelse(avg_mort >= 0.02, paste0(round(avg_mort * 100, 1), "%"), "")),
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
 

## 2.5 OC Pie Stand Alone ==========
pie_df2 <- OC_plot_df %>%
  filter(year > 2008) %>%
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%
  dplyr::mutate( 
      custom_region = case_when(broad_region == "Alaska" ~ "AK",
                              broad_region == "British Columbia" ~ "BC",
                              broad_region == "Washington" ~ "WA",
                              broad_region == "Oregon Coast" ~ "OR Ocean",
                              broad_region == "Oregon Coast\nIn-River" ~ "OR In-River",
                              TRUE ~ NA), 
    cum_pos = cumsum(avg_mort) - avg_mort / 2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  ) 

custom_pal <- c(
  "Oregon Coast" = "#2D6E7E",    
  "Oregon Coast\nIn-River"    = "#7FA8B8",
  "Washington"                = "grey",  
  "British Columbia"           = "#A8B8A0",  
  "Alaska"                     = "#C8D4D8"
)

OC_pie2 <- ggplot(pie_df2, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  
  # Labels inside for large slices
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort >= 0.05, paste0(label), "")),
            size = 4, fontface = "bold", color = "black") + 
  # Labels outside for small slices — nudge x past 1 to push outside pie
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort < 0.05 & avg_mort > 0.001, paste0(label), "")),
            x = 1.63,   # >1 pushes outside the pie (pie lives at x = 1)
            size = 3, fontface = "bold", color = "black") + 
  coord_polar(theta = "y", clip = "off") +   
  scale_fill_manual(values = custom_pal, name = "Fishery Region") +  
  theme_void() +
  theme(
    legend.position   = "right",
    legend.title      = element_text(size = 10, face = "bold"),
    legend.text       = element_text(size = 9),
    legend.margin     = margin(t = 10, b = -10, unit = "mm"),  # positive t pushes down, negative b pulls pie up
    # legend.key.size   = unit(0.4, "cm"),   # smaller legend keys
    legend.spacing.x  = unit(0.2, "cm"),   # tighten horizontal spacing
    plot.margin       = margin(0, 5, 0, 5, "mm"),  # reduce outer margins
    plot.background   = element_blank()
  )


OC_pie2

ggsave(
  filename = "output/plots/OC_FM_PieOnly.jpeg",
  plot = OC_pie2,
  width =6,
  height =5.5,
  dpi = 300,
  units = "in"
)

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

# Individual stocks =======
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
                  fishery_region == "PS_n" ~ "Washington", #"Puget Sound",
                  fishery_region == "PS_s" ~ "Washington", #"Puget Sound",
                  
                  fishery_region == "wac_n" ~  "Washington",#"Washington Coast\nIn-River",
                  # Other
                  TRUE                                    ~ "Check")) 

# uniqueOC<- data.frame(unique(OC_fish[c("broad_region","fishery_region")]))
# 
# uniqueOC <- uniqueOC %>% 
#   arrange(broad_region)

total_FMnumbers <- OC_fish %>%
  group_by(year,population) %>%
  # get annual sum of all fishery mortality from OP for the year
  dplyr::summarise(total_FM_numbers = sum(mortality_numbers))  # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize.

OC_plot_df<- OC_fish %>% 
  dplyr::select(-c(total_run,fishery_region)) %>%
  ungroup() %>%
  group_by(year, broad_region,population) %>%
  dplyr::summarise(mort_broad_region = sum(mortality_numbers)) %>% 
  left_join(total_FMnumbers) %>%
  dplyr::mutate(percent_mort = mort_broad_region/total_FM_numbers,
                broad_region = factor(broad_region, 
                                      levels = rev(c(
                  "Oregon Coast",
                  "Oregon Coast\nIn-River",
                  "Washington",  
                  "British Columbia", 
                  "Alaska"))), 
                population = case_when(population == "Siletz_fa"  ~  "Siletz Fall",
                                       population == "Siuslaw_fa"  ~ "Siuslaw Fall",
                                       population == "Nehalem_fa"  ~ "Nehalem Fall",
                                       population == "south_umpqua"  ~ "South Umpqua",
                                       TRUE ~ population))

### Nehalem ========= 
pie_df <- OC_plot_df %>%
  filter(year > 2008,
         population %in% c("Nehalem Fall")) %>% #, "Siletz Fall", "Siuslaw Fall")) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  # group_by(population) %>%                          # <-- group by population ONLY
  arrange(desc(broad_region)) %>%             # consistent ordering within each facet
  mutate(
    cum_pos = cumsum(avg_mort) - avg_mort / 2,      # midpoint within each facet's stack
    label = paste0(round(avg_mort * 100, 1), "%")
  ) %>%
  ungroup()


Nehalem <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_label_repel(
    aes(y = cum_pos, label = ifelse(avg_mort >= 0.02, paste0(round(avg_mort * 100, 1), "%"), "")),
    size = 3,
    fontface = "bold",
    nudge_x = 0.6,          # push labels outward
    show.legend = FALSE,
    segment.size = 0.3
  ) +
  # facet_wrap(~population) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_pal) + 
  #  scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(title = "Nehalem Fall, Avg 2009-2020") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )
Nehalem

### Siletz ========= 
pie_df <- OC_plot_df %>%
  filter(year > 2008,
         population %in% c("Siletz Fall")) %>% #, "Siletz Fall", "Siuslaw Fall")) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  # group_by(population) %>%                          # <-- group by population ONLY
  arrange(desc(broad_region)) %>%             # consistent ordering within each facet
  mutate(
    cum_pos = cumsum(avg_mort) - avg_mort / 2,      # midpoint within each facet's stack
    label = paste0(round(avg_mort * 100, 1), "%")
  ) %>%
  ungroup()


Siletz <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_label_repel(
    aes(y = cum_pos, label = ifelse(avg_mort >= 0.02, paste0(round(avg_mort * 100, 1), "%"), "")),
    size = 3,
    fontface = "bold",
    nudge_x = 0.6,          # push labels outward
    show.legend = FALSE,
    segment.size = 0.3
  ) +
  # facet_wrap(~population) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_pal) + 
  #  scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(title = "Siletz Fall, Avg 2009-2020") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )
Siletz

### Siuslaw ========= 
pie_df <- OC_plot_df %>%
  filter(year > 2008,
         population %in% c("Siuslaw Fall")) %>% #, "Siletz Fall", "Siuslaw Fall")) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  # group_by(population) %>%                          # <-- group by population ONLY
  arrange(desc(broad_region)) %>%             # consistent ordering within each facet
  mutate(
    cum_pos = cumsum(avg_mort) - avg_mort / 2,      # midpoint within each facet's stack
    label = paste0(round(avg_mort * 100, 1), "%")
  ) %>%
  ungroup()


Siuslaw <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_label_repel(
    aes(y = cum_pos, label = ifelse(avg_mort >= 0.02, paste0(round(avg_mort * 100, 1), "%"), "")),
    size = 3,
    fontface = "bold",
    nudge_x = 0.6,          # push labels outward
    show.legend = FALSE,
    segment.size = 0.3
  ) +
  # facet_wrap(~population) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_pal) + 
  #  scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(title = "Siuslaw Fall, Avg 2009-2020") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )
Siuslaw

### Save individual Pies ========
individ_pie <- ggpubr::ggarrange(Siuslaw,Siletz, Nehalem, nrow = 1)
ggsave(
  filename = "output/plots/Individ_Pie.jpeg",
  plot = individ_pie,
  width =6,
  height =4,
  dpi = 300,
  units = "in"
)
##  3. Main bar chart ==================
OC_FM_stacked_Facet <- ggplot(OC_plot_df %>% filter(year>2008 ),
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
  facet_wrap(~population) + 
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
OC_FM_stacked_Facet

## 3B. Select stocks bar chart ==================
OC_FM_stacked_Facet2 <- ggplot(OC_plot_df %>% filter(year>2008, 
                                                    population %in% c("Nehalem Fall", "Siletz Fall", "Siuslaw Fall")),
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
  facet_wrap(~population) + 
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
    panel.spacing = unit(1, "lines")#,
    #legend.position = "none"
    )
OC_FM_stacked_Facet2

# 4. Save as JPEG =======
ggsave(
  filename = "output/plots/OR_Coast_FM_FACET1_stacked.jpeg",
  plot = OC_FM_stacked_Facet,
  width =6,
  height =5,
  dpi = 300,
  units = "in"
)
 

ggsave(
  filename = "output/plots/OR_Coast_FM_FACET2_stacked.jpeg",
  plot = OC_FM_stacked_Facet2,
  width =6,
  height =5,
  dpi = 300,
  units = "in"
)
