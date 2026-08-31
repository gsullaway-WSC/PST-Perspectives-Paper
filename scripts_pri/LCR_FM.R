library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(cowplot)  
library(showtext)
 font_add_google("DM Sans", "dm_sans")
 showtext_auto()

 
# load ====== 
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
  filter(region == "LCR", !year == 2023, year>2007,
         !population == "Cowlitz.fa"
         ) # NAs in 2023, and in two rows for the Lewis  
 
total_run_df<-data %>%  
  dplyr::select( year,population,region,total_run) %>%
  dplyr::mutate(total_run = as.numeric(total_run))%>%
  filter(!is.na(total_run))

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
  dplyr::mutate(percent_mort = as.numeric(percent_mort)/100)  %>%
  filter(!is.na(percent_mort)) # two years in the Lewis, 2016,2017

# LCR whole ======= 
LCR_fish <- catch_distributions %>%
  left_join(total_run_df) %>% 
  filter(!is.na(percent_mort )) %>% 
  dplyr::mutate(mortality_numbers = total_run *percent_mort,
                broad_region = case_when(
                  # Alaska
                  str_detect(fishery_region, "^seak") ~ "Alaska",
                  str_detect(fishery_region, "^ak_term")  ~ "Alaska",
                  
                  str_detect(fishery_region, "^US_term")  ~ "Washington Coast",
                  str_detect(fishery_region, "^term_tot")  ~ "Washington Coast",

                   # Canada
                  str_detect(fishery_region, "^can_term")  ~ "British Columbia",
                  str_detect(fishery_region, "^wcvi")     ~ "British Columbia",#"West Coast Vancouver Island",
                  str_detect(fishery_region, "^nbc")      ~ "British Columbia", #"North Coast BC",
                  str_detect(fishery_region, "^sbc")      ~ "British Columbia",
                  
                  # str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
                  
                  # Falcon (US)
                  str_detect(fishery_region, "^sfalc")    ~ "South of Falcon",
                  # str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
                  
                  fishery_region == "nfalc_s" ~ "Washington Coast",
                  fishery_region == "nfalc_t" ~ "Washington Coast",
                  fishery_region == "US_is_tot" ~ "Washington Coast",

                  # WA     
                  fishery_region == "PS_n" ~ "Puget Sound",
                  fishery_region == "PS_s" ~ "Puget Sound",
                  
                  fishery_region == "wac_n" ~ "Washington Coast",
                  # Other
                  TRUE                                    ~ "Check")) 

uniqueLCR<- data.frame(unique(LCR_fish[c("broad_region","fishery_region")]))

uniqueLCR <- uniqueLCR %>% 
  arrange(broad_region)
 
# test <- Oly_Pen_fish %>%
#   filter(fishery_region %in% c("US_term_n","US_term_s","US_term_t", "term_tot")) %>%
#   dplyr::select(year, population, fishery_region, total_run) %>%
#   spread(fishery_region, total_run) %>%
#   mutate(sum = US_term_n+ US_term_s+US_term_t)


total_FMnumbers <- LCR_fish %>%
  group_by(year) %>%
  # get annual sum of all fishery mortality from OP for the year
  dplyr::summarise(total_FM_numbers = sum(mortality_numbers))  # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize. 

LCR_plot_df<- LCR_fish %>% 
  dplyr::select(-c(total_run, percent_mort,fishery_region)) %>%
  ungroup() %>%
  group_by(year, broad_region) %>%
  dplyr::summarise(mort_broad_region = sum(mortality_numbers)) %>% 
  left_join(total_FMnumbers) %>%
  dplyr::mutate(percent_mort = mort_broad_region/total_FM_numbers,
                broad_region = factor(broad_region, levels = c(
                  "Washington Coast", 
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

## 2.5 LCR Pie Stand Alone ==========
pie_df2 <- LCR_plot_df %>%
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%
  dplyr::mutate(
    custom_region = case_when(broad_region == "Alaska" ~ "AK",
                              broad_region == "British Columbia" ~ "BC",
                              broad_region == "Puget Sound" ~ "PS",
                              broad_region == "Washington Coast" ~ "WA",
                              broad_region == "Washington Coast" ~ "WA",
                              broad_region == "South of Falcon" ~ "OR",
                               TRUE ~ NA), 
    cum_pos = cumsum(avg_mort) - avg_mort / 2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  ) 

custom_pal <- c(
  "Washington Coast" = "#2E5F6E",     
  "South of Falcon"            = "#E8E8E8",   
  "Puget Sound"                = "grey",  
  "British Columbia"           = "#8A9E7A", 
  "Alaska"                     = "#4A7A50"
)
  
 
LCR_pie2 <- ggplot(pie_df2, aes(x = "", y = avg_mort, fill = broad_region)) +
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
LCR_pie2
 

showtext_opts(dpi = 300)
  
# ggsave(
#   filename = "output/plots/LCR_FM_PieOnly.png",
#   plot = OP_pie2,
#   width = 4,
#   height = 4,
#   dpi = 300,
#   units = "in",
#   bg = "transparent"
# )
 
### 2.75 Two Treaty Periods - LCR Pie Stand Alone ==========
# have to do separate because the facet and pie charts get weird
pie_df3 <- LCR_plot_df %>%
  filter(year > 2007 & year <2019) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%
  dplyr::mutate(
    custom_region = case_when(broad_region == "Alaska" ~ "AK",
                              broad_region == "British Columbia" ~ "BC",
                              broad_region == "Puget Sound" ~ "PS",
                              broad_region == "Washington Coast" ~ "WA", 
                              broad_region == "South of Falcon" ~ "OR",
                              TRUE ~ NA), 
    cum_pos = cumsum(avg_mort) - avg_mort / 2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  ) 
 
LCR_pie3 <- ggplot(pie_df3, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) + 
  # Labels inside for large slices
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort >= 0.05, paste0(label), "")),
            size = 3.5, fontface = "bold", family = "dm_sans",color = "black") + 
  # Labels outside for small slices — nudge x past 1 to push outside pie
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort < 0.05 & avg_mort > 0.001, paste0(label), "")),
            x = 1.7,   # >1 pushes outside the pie (pie lives at x = 1)
            size = 2.3, fontface = "bold", family = "dm_sans",color = "black") + 
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
  )+
  ggtitle("LCR, Treaty Period: 2008-2018")

LCR_pie3

# have to do separate because the facet and pie charts get weird
pie_df4 <- LCR_plot_df %>%
  filter(year >2018) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%
  dplyr::mutate(
    custom_region = case_when(broad_region == "Alaska" ~ "AK",
                              broad_region == "British Columbia" ~ "BC",
                              broad_region == "Puget Sound" ~ "PS",
                              broad_region == "Washington Coast" ~ "WA", 
                              TRUE ~ NA), 
    cum_pos = cumsum(avg_mort) - avg_mort /2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  )  %>%
  filter(!is.na(custom_region)) # removing a super small % that is south of falcon 

LCR_pie4 <- ggplot(pie_df4, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) + 
  # Labels inside for large slices
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort >= 0.05, paste0(label), "")),
            size = 3.3, fontface = "bold", family = "dm_sans",color = "black") + 
  # Labels outside for small slices — nudge x past 1 to push outside pie
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort < 0.05 & avg_mort > 0.001, paste0(label), "")),
            x = 1.7,   # >1 pushes outside the pie (pie lives at x = 1)
            size = 2.3, fontface = "bold", family = "dm_sans",color = "black") + 
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
  )+
  ggtitle("LCR Treaty Period: 2018-2022")

LCR_pie4

