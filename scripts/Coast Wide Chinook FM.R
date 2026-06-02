 
# get Coast-Wide Chinook FM pie charts, and then put them on a map ====== 

library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(cowplot)  

library(showtext)
font_add_google("DM Sans", "dm_sans")
showtext_auto()

custom_pal <- c(
  "Washington" = "#2E5F6E",    
  "Oregon"    = "#6A9BA8",  
  "British Columbia"           = "#8A9E7A",  
  "Alaska"                     = "#4A7A50"
)

unique(data$population)

# load ====== 
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
  filter(!year == 2023) # NAs 

total_run_df<-data %>%  
  dplyr::select( year,population,region,total_run) %>%
  dplyr::mutate(total_run = as.numeric(total_run))

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
  dplyr::mutate(percent_mort = as.numeric(percent_mort)/100) 

group_df <- catch_distributions %>%
  left_join(total_run_df) %>% 
  filter(!is.na(percent_mort )) %>% 
  dplyr::mutate(mortality_numbers = total_run *percent_mort,
                broad_region = case_when(
                  # Alaska
                  str_detect(fishery_region, "^seak") ~ "Alaska",
                  str_detect(fishery_region, "^ak_term")  ~ "Alaska",
                  str_detect(fishery_region, "^US_term") & population %in% c("Chilkat","Stikine", "Taku", "Unuk") ~  "Alaska", 
                   
                  # if its US terminal net and the regoins are candian remove from data ---- 
                  # change this so it is baed off of the run, goes to that state. 
                  str_detect(fishery_region, "^US_term") & population %in% c("Grays" ,"Nisqually","Green","Nooksack","Queets_fa","Hoh_fa", "Hoh_sp",
                                                                             "Queets_sp","Grays_Harbor_fa","Grays_Harbor_sp", "Quillayute_fa","Skagit_sp",
                                                                             "Snohomish_fa", "Skagit_fa", "Stillaguamish","Elochoman",
           "Nicola","Phillips",) ~ "Washington",
                  str_detect(fishery_region, "^US_term") & population %in% c("Cowlitz.fa", "Elk", "South_umpqua","Coquille","Coweeman", "Lewis",
                                                                             "Nehalem_fa","Siletz_fa","Siuslaw_fa","Hanford_br_w") ~ "Oregon",
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
                  fishery_region == "US_is_tot" ~ "Washington",
                  
                  # WA     
                  fishery_region == "PS_n" ~ "Washington",
                  fishery_region == "PS_s" ~ "Washington",
                  
                  fishery_region == "wac_n" ~ "Washington",
                  # Other
                  TRUE                                    ~ "Check")) 

unique<- data.frame(unique(group_df[c("broad_region","fishery_region")]))

uniqueOP <- uniqueOP %>% 
  arrange(broad_region)
# # 
# test <- Oly_Pen_fish %>%
#   filter(fishery_region %in% c("US_term_n","US_term_s","US_term_t", "term_tot")) %>%
#   dplyr::select(year, population, fishery_region, total_run) %>%
#   spread(fishery_region, total_run) %>%
#   mutate(sum = US_term_n+ US_term_s+US_term_t)


total_FMnumbers <- Oly_Pen_fish %>%
  group_by(year) %>%
  # get annual sum of all fishery mortality from OP for the year
  dplyr::summarise(total_FM_numbers = sum(mortality_numbers))  # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize. 

OP_plot_df<- Oly_Pen_fish %>% 
  dplyr::select(-c(total_run, percent_mort,fishery_region)) %>%
  ungroup() %>%
  group_by(year, broad_region) %>%
  dplyr::summarise(mort_broad_region = sum(mortality_numbers)) %>% 
  left_join(total_FMnumbers) %>%
  dplyr::mutate(percent_mort = mort_broad_region/total_FM_numbers,
                broad_region = factor(broad_region, levels = c(
                  "Washington Coast\nIn-River",
                  "Washington Coast\nOcean",  
                  "South of Falcon",
                  "Puget Sound",
                  "British Columbia", 
                  "Alaska" 
                  # "British Columbia", 
                  # "Puget Sound",  
                  # "South of Falcon",
                  # "Washington Coast Ocean", 
                  # "Washington Coast In-River"
                )))
 
  
## 2.5 OP Pie Stand Alone ==========
pie_df2 <- OP_plot_df %>%
  filter(year > 2008) %>%
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%
  dplyr::mutate(
    custom_region = case_when(broad_region == "Alaska" ~ "AK",
                              broad_region == "British Columbia" ~ "BC",
                              broad_region == "Puget Sound" ~ "PS",
                              broad_region == "Washington Coast\nOcean" ~ "WA Ocean",
                              broad_region == "Washington Coast\nIn-River" ~ "WA In-River",
                              TRUE ~ NA), 
    cum_pos = cumsum(avg_mort) - avg_mort / 2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  ) 

 

OP_pie2 <- ggplot(pie_df2, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  
  
  # Labels inside for large slices
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort >= 0.05, paste0(label), "")),
            size = 4, fontface = "bold", family = "dm_sans",color = "black") + 
  # Labels outside for small slices — nudge x past 1 to push outside pie
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort < 0.05 & avg_mort > 0.001, paste0(label), "")),
            x = 1.7,   # >1 pushes outside the pie (pie lives at x = 1)
            size = 2.6, fontface = "bold", family = "dm_sans",color = "black") + 
  coord_polar(theta = "y", clip = "off") +   
  scale_fill_manual(values = custom_pal, name = "Fishery Region") +  
  theme_void() +
  theme(
    legend.position   = "none",
    legend.title      = element_text(family = "dm_sans",size = 10, face = "bold"),
    legend.text       = element_text(family = "dm_sans",size = 9),
    legend.margin     = margin(t = 10, b = -10, unit = "mm"),  # positive t pushes down, negative b pulls pie up
    legend.key.size   = unit(0.4, "cm"),   # smaller legend keys
    legend.spacing.x  = unit(0.2, "cm"),   # tighten horizontal spacing
    plot.margin       = margin(0, 5, 0, 5, "mm"),  # reduce outer margins
    plot.background   = element_blank()
  )

OP_pie2

showtext_opts(dpi = 300)

ggsave(
  filename = "output/plots/OP_FM_PieOnly.png",
  plot = OP_pie2,
  width = 4,
  height = 4,
  dpi = 300,
  units = "in",
  bg = "transparent"
)
