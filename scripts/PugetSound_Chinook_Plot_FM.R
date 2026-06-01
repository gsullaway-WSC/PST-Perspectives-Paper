library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(cowplot) 
library(ggrepel)
 
# custom colors =====

custom_pal_pie <- c(
  "Puget Sound\nIn-River" = "#2D6E7E",    
  "Puget Sound"    = "#7FA8B8",
  "Washington"            = "#E8E8E8",   
  "Oregon"                = "grey",  
  "British Columbia"           = "#A8B8A0",  
  "Alaska"                     = "#C8D4D8"
)
custom_pal <- c(
  "Puget Sound\nIn-River" =  "#0096C7",    
  "Puget Sound"  =  "#8B7BB5",
  "Washington"           =  "#E8E8E8",   
  "Oregon"                =   "grey",  
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
  filter(region %in% c("PS"),
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
 
 

# PS whole ======= 
PS_fish <- catch_distributions %>%
  left_join(total_run_df) %>% 
  filter(!is.na(percent_mort )) %>% 
  dplyr::mutate(mortality_numbers = total_run *percent_mort,
                broad_region = case_when(
                  # Alaska
                  str_detect(fishery_region, "^seak") ~ "Alaska",
                  str_detect(fishery_region, "^ak_term")  ~ "Alaska",
                  
                  str_detect(fishery_region, "^US_term")  ~ "Puget Sound\nIn-River",
                  str_detect(fishery_region, "^term_tot")  ~ "Puget Sound\nIn-River",

                   # Canada
                  str_detect(fishery_region, "^can_term")  ~ "British Columbia",
                  str_detect(fishery_region, "^wcvi")     ~ "British Columbia",#"West Coast Vancouver Island",
                  str_detect(fishery_region, "^nbc")      ~ "British Columbia", #"North Coast BC",
                  str_detect(fishery_region, "^sbc")      ~ "British Columbia",
                  
                  # str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
                  
                  # Falcon (US)
                  str_detect(fishery_region, "^sfalc")    ~  "Oregon",#"South of Falcon",
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

uniquePS<- data.frame(unique(PS_fish[c("broad_region","fishery_region")]))

uniquePS <- uniquePS %>% 
  arrange(broad_region)
 
total_FMnumbers <- PS_fish %>%
  group_by(year) %>%
  # get annual sum of all fishery mortality from OP for the year
  dplyr::summarise(total_FM_numbers = sum(mortality_numbers))  # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize. 

PS_plot_df<- PS_fish %>% 
  dplyr::select(-c(total_run, percent_mort,fishery_region)) %>%
  ungroup() %>%
  group_by(year, broad_region) %>%
  dplyr::summarise(mort_broad_region = sum(mortality_numbers)) %>% 
  left_join(total_FMnumbers) %>%
  dplyr::mutate(percent_mort = mort_broad_region/total_FM_numbers,
                broad_region = factor(broad_region, levels = c(
                
                  "Puget Sound",
                  "Puget Sound\nIn-River",
                  "Oregon",
                  "Washington", 
                  "British Columbia", 
                  "Alaska" 
                )))

# bar and pie ==== 
## 1. Prep pie chart data (last 5 years) ==========
pie_df <- PS_plot_df %>%
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
PS_pie <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
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
PS_pie

## 2.5  Pie Stand Alone ==========
pie_df2 <- PS_plot_df %>%
  filter(year > 2008) %>%
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%
  dplyr::mutate(
    custom_region = case_when(broad_region == "Alaska" ~ "AK",
                              broad_region == "British Columbia" ~ "BC",
                              broad_region == "Puget Sound" ~ "PS",
                              broad_region == "Oregon" ~ "OR",   
                              broad_region == "Puget Sound\nIn-River" ~ "PS\nIn-River", 
                              broad_region == "Washington" ~ "WA",
                              TRUE ~ NA), 
    cum_pos = cumsum(avg_mort) - avg_mort / 2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  ) 
 

PS_pie2 <- ggplot(pie_df2, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  # Labels inside for large slices
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort >= .12, paste0(label), "")),
            size = 4, fontface = "bold", color = "black") + 
  # Labels outside for small slices — nudge x past 1 to push outside pie
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort < .12 & avg_mort > 0.02 , paste0(label), "")),
            x = 1.3,   # >1 pushes outside the pie (pie lives at x = 1)
            size =3, fontface = "bold", color = "black") + 
  coord_polar(theta = "y", clip = "off") +   
  ggtitle("Puget Sound") + 
  scale_fill_manual(values = custom_pal_pie, name = "Fishery Region") +  
  theme_void() +
  theme(
    legend.position   = "top",
    legend.title      = element_text(size = 10, face = "bold"),
    legend.text       = element_text(size = 9),
    legend.margin     = margin(t = 10, b = -10, unit = "mm"),  # positive t pushes down, negative b pulls pie up
    # legend.key.size   = unit(0.4, "cm"),   # smaller legend keys
    legend.spacing.x  = unit(0.2, "cm"),   # tighten horizontal spacing
    plot.margin       = margin(0, 5, 0, 5, "mm"),  # reduce outer margins
    plot.background   = element_blank()
  )

PS_pie2

ggsave(
  filename = "output/plots/Puget Sound Plots/PS_FM_PieOnly.jpeg",
  plot = PS_pie2,
  width =6,
  height =5.5,
  dpi = 300,
  units = "in"
)


##  3. Main bar chart ==================
PS_FM_stacked <- ggplot(PS_plot_df,
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
                         title = "Puget Sound Chinook Salmon",
                         subtitle = "Proportional Fishery Mortality by Region & Year"
                       ) +
                       theme_minimal() +
                       theme(
                         panel.grid.minor = element_blank(),
                         panel.spacing = unit(1, "lines"),
                         legend.position = "none")

## 4. Legend =============
legend <- get_legend(ggplot(PS_plot_df,
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
    title = "Puget Sound Chinook Salmon",
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
rightside <- ggpubr::ggarrange(PS_pie, legend, nrow = 2,
                                heights = c(1, 1.2)) 
rightside

# Add a blank spacer on top to push content down and align pie with bar top
spacer <- ggplot() + theme_void()  # empty plot as top padding

rightside_padded <- ggpubr::ggarrange(spacer, rightside, nrow = 2,
                                      heights = c(0.2, 1))  # adjust 0.3 to move down more

final_plot <- ggpubr::ggarrange(PS_FM_stacked, rightside_padded,
                                ncol = 2,
                                widths = c(3.3, 1))
final_plot

# --- 5. Save as JPEG ---
ggsave(
  filename = "output/plots/Puget Sound Plots/PS_Chinook_FM_stacked.jpeg",
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

PS_fish <- catch_distributions %>%
  left_join(total_run_df) %>% 
  filter(!is.na(percent_mort )) %>% 
  dplyr::mutate(mortality_numbers = total_run *percent_mort,
                broad_region = case_when(
                  # Alaska
                  str_detect(fishery_region, "^seak") ~ "Alaska",
                  str_detect(fishery_region, "^ak_term")  ~ "Alaska",
                  
                  str_detect(fishery_region, "^US_term")  ~ "Puget Sound\nIn-River",
                  str_detect(fishery_region, "^term_tot")  ~ "Puget Sound\nIn-River",
                  
                  # Canada
                  str_detect(fishery_region, "^can_term")  ~ "British Columbia",
                  str_detect(fishery_region, "^wcvi")     ~ "British Columbia",#"West Coast Vancouver Island",
                  str_detect(fishery_region, "^nbc")      ~ "British Columbia", #"North Coast BC",
                  str_detect(fishery_region, "^sbc")      ~ "British Columbia",
                  
                  # str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
                  
                  # Falcon (US)
                  str_detect(fishery_region, "^sfalc")    ~  "Oregon",#"South of Falcon",
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

total_FMnumbers <- PS_fish %>%
  group_by(year,population) %>%
  # get annual sum of all fishery mortality from OP for the year
  dplyr::summarise(total_FM_numbers = sum(mortality_numbers))  # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize.

PS_plot_df<- PS_fish %>% 
  dplyr::select(-c(total_run,fishery_region)) %>%
  ungroup() %>%
  group_by(year, broad_region,population) %>%
  dplyr::summarise(mort_broad_region = sum(mortality_numbers)) %>% 
  left_join(total_FMnumbers) %>%
  dplyr::mutate(percent_mort = mort_broad_region/total_FM_numbers)

### Nooksack ========= 
pie_df <- PS_plot_df %>%
  filter(year > 2008,
         population %in% c("Nooksack")) %>% #, "Siletz Fall", "Siuslaw Fall")) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  # group_by(population) %>%                          # <-- group by population ONLY
  arrange(desc(broad_region)) %>%             # consistent ordering within each facet
  dplyr::mutate(
    custom_region = case_when(broad_region == "Alaska" ~ "AK",
                              broad_region == "British Columbia" ~ "BC",
                              broad_region == "Puget Sound" ~ "PS\nOther",
                              broad_region == "Oregon" ~ "OR",   
                              broad_region == "Puget Sound\nIn-River" ~ "In-River", 
                              broad_region == "Washington" ~ "WA",
                              TRUE ~ NA), 
    cum_pos = cumsum(avg_mort) - avg_mort / 2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  ) %>% 
  ungroup()

Nooksack <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort >= .05, paste0(label), "")),
            x = 1.3, size = 4, fontface = "bold", color = "black") + 
  # Labels outside for small slices — nudge x past 1 to push outside pie
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort < .05 & avg_mort > 0.02 , paste0(label), "")),
            x = 1.63,   # >1 pushes outside the pie (pie lives at x = 1)
            size =3, fontface = "bold", color = "black") + 
  coord_polar(theta = "y", clip = "off") +   
  scale_fill_manual(values = custom_pal_pie, name = "Fishery Region") +  
  labs(title = "Nooksack, Avg 2009-2020") +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )
Nooksack

ggsave(
  filename = "output/plots/Puget Sound Plots/Nooksack_FM_PieOnly.jpeg",
  plot = Nooksack,
  width =6,
  height =5.5,
  dpi = 300,
  units = "in"
) 

### Stillaguamish ========= 
pie_df <- PS_plot_df %>%
  filter(year > 2008,
         population %in% c("Stillaguamish")) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%             # consistent ordering within each facet
  dplyr::mutate(
    custom_region = case_when(broad_region == "Alaska" ~ "AK",
                              broad_region == "British Columbia" ~ "BC",
                              broad_region == "Puget Sound" ~ "PS\nOther",
                              broad_region == "Oregon" ~ "OR",   
                              broad_region == "Puget Sound\nIn-River" ~ "In-River", 
                              broad_region == "Washington" ~ "WA",
                              TRUE ~ NA), 
    cum_pos = cumsum(avg_mort) - avg_mort / 2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  ) %>% 
  ungroup()

Stillaguamish <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort >= .16, paste0(label), "")),
            size = 4, fontface = "bold", color = "black") + 
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort > .1 & avg_mort < 0.16 , paste0(label), "")),
            x = 1.2,   # >1 pushes outside the pie (pie lives at x = 1)
            size =3, fontface = "bold", color = "black") + 
  # Labels outside for small slices — nudge x past 1 to push outside pie
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort < .1 & avg_mort > 0.02 , paste0(label), "")),
            x = 1.63,   # >1 pushes outside the pie (pie lives at x = 1)
            size =3, fontface = "bold", color = "black") + 
  coord_polar(theta = "y", clip = "off") +  
  scale_fill_manual(values = custom_pal_pie, name = "Fishery Region") +  
  labs(title = "Stillaguamish, Avg 2009-2020") +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )

Stillaguamish

ggsave(
  filename = "output/plots/Puget Sound Plots/Stillaguamish_FM_PieOnly.jpeg",
  plot = Stillaguamish,
  width =6,
  height =5.5,
  dpi = 300,
  units = "in"
) 

### Skagit Spring ========= 
pie_df <- PS_plot_df %>%
  filter(year > 2008,
         population %in% c("Skagit_sp")) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%             # consistent ordering within each facet
  dplyr::mutate(
    custom_region = case_when(broad_region == "Alaska" ~ "AK",
                              broad_region == "British Columbia" ~ "BC",
                              broad_region == "Puget Sound" ~ "PS\nOther",
                              broad_region == "Oregon" ~ "OR",   
                              broad_region == "Puget Sound\nIn-River" ~ "In-River", 
                              broad_region == "Washington" ~ "WA",
                              TRUE ~ NA), 
    cum_pos = cumsum(avg_mort) - avg_mort / 2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  ) %>% 
  ungroup()

skagit_sp <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort >= .16, paste0(label), "")),
            size = 4, fontface = "bold", color = "black") + 
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort > .1 & avg_mort < 0.16 , paste0(label), "")),
            x = 1.2,   # >1 pushes outside the pie (pie lives at x = 1)
            size =3, fontface = "bold", color = "black") + 
  # Labels outside for small slices — nudge x past 1 to push outside pie
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort < .1 & avg_mort > 0.02 , paste0(label), "")),
            x = 1.63,   # >1 pushes outside the pie (pie lives at x = 1)
            size =3, fontface = "bold", color = "black") + 
  coord_polar(theta = "y", clip = "off") +  
  scale_fill_manual(values = custom_pal_pie, name = "Fishery Region") +  
  labs(title = "Skagit Spring, Avg 2009-2020") +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )

skagit_sp 

ggsave(
  filename = "output/plots/Puget Sound Plots/Skagit_Sp_FM_PieOnly.jpeg",
  plot = skagit_sp,
  width =6,
  height =5.5,
  dpi = 300,
  units = "in"
) 

### Skagit Fall ========= 
pie_df <- PS_plot_df %>%
  filter(year > 2008,
         population %in% c("Skagit_fa")) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%             # consistent ordering within each facet
  dplyr::mutate(
    custom_region = case_when(broad_region == "Alaska" ~ "AK",
                              broad_region == "British Columbia" ~ "BC",
                              broad_region == "Puget Sound" ~ "PS\nOther",
                              broad_region == "Oregon" ~ "OR",   
                              broad_region == "Puget Sound\nIn-River" ~ "In-River", 
                              broad_region == "Washington" ~ "WA",
                              TRUE ~ NA), 
    cum_pos = cumsum(avg_mort) - avg_mort / 2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  ) %>% 
  ungroup()

skagit_fa <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort >= .16, paste0(label), "")),
            size = 4, fontface = "bold", color = "black") + 
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort > .1 & avg_mort < 0.16 , paste0(label), "")),
            x = 1.2,   # >1 pushes outside the pie (pie lives at x = 1)
            size =3, fontface = "bold", color = "black") + 
  # Labels outside for small slices — nudge x past 1 to push outside pie
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort < .1 & avg_mort > 0.02 , paste0(label), "")),
            x = 1.63,   # >1 pushes outside the pie (pie lives at x = 1)
            size =3, fontface = "bold", color = "black") + 
  coord_polar(theta = "y", clip = "off") +  
  scale_fill_manual(values = custom_pal_pie, name = "Fishery Region") +  
  labs(title = "Skagit Fall, Avg 2009-2020") +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )

skagit_fa

ggsave(
  filename = "output/plots/Puget Sound Plots/Skagit_Fa_FM_PieOnly.jpeg",
  plot = skagit_sp,
  width =6,
  height =5.5,
  dpi = 300,
  units = "in"
) 


