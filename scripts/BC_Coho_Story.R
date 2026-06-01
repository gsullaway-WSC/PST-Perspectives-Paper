library(tidyverse)
library(here)
library(readxl)
library(viridis) 
library(janitor)

# BC COHO ==========
custom_pal <- c(
  # "Oregon Coast" = "#2D6E7E",    
  # "Oregon Coast\nIn-River"    = "#7FA8B8",
  # "Washington"                = "grey",  
  "British Columbia"           = "#7FA8B8",#  "#A8B8A0",  
  "Alaska"                     = "#2D6E7E"#,"#C8D4D8"
)


toboggan_df <- read_xlsx("data/BC_Coho_Toboggan and Zolzap coho ERs for WA.xlsx", sheet =1, skip =2) %>%
  janitor::clean_names() %>% 
  dplyr::rename(total_ER = "total_43", AK_harvest = "alaska_46", CAN_harvest = "canada", Total_harvest = "total_48") %>% 
  dplyr::mutate(mean_esc = mean(as.numeric(total_esc),na.rm = TRUE),
                sd_esc = sd(as.numeric(total_esc), na.rm = TRUE)) %>% 
   dplyr::select(return_year,mean_esc, sd_esc,total_esc,total_ER,AK_harvest,CAN_harvest,Total_harvest) %>%
  gather(2:ncol(.), key = "type", value = "value") %>%
  dplyr::mutate(id = "Toboggan", value = as.numeric(value))
 
zolzap_df <- read_xlsx("data/BC_Coho_Toboggan and Zolzap coho ERs for WA.xlsx", sheet =2,skip =1 ) %>% 
  janitor::clean_names() %>% 
  dplyr::rename(total_esc = "total_esc_estimate",total_ER = "total_36", AK_harvest = "alaska_39", CAN_harvest = "canada", Total_harvest = "total_41") %>% 
  dplyr::mutate(mean_esc = mean(as.numeric(total_esc),na.rm = TRUE),
                sd_esc = sd(as.numeric(total_esc), na.rm = TRUE)) %>% 
  dplyr::select(return_year,mean_esc,sd_esc, total_esc,total_ER,AK_harvest,CAN_harvest,Total_harvest) %>%
  gather(2:ncol(.), key = "type", value = "value") %>%
  dplyr::mutate(id = "Zolzap", value = as.numeric(value))

df <- rbind(toboggan_df,zolzap_df)

## DF for plot ======= 
plot_df <- df %>%
  spread(type, value) %>% 
  dplyr::mutate(under_goal = case_when( total_esc < (mean_esc-sd_esc) ~ "Low Abundance",
                                       TRUE ~ "NA")) %>% 
  group_by(id) %>%
  dplyr::mutate(mean_ER = mean(total_ER, na.rm = TRUE),
               SD_ER = sd(total_ER, na.rm = TRUE),
               above_avg_ER = total_ER > (mean_ER-SD_ER),
         # Vulnerable = below esc goal AND above avg AABM catch
               vulnerable = case_when(under_goal == "Low Abundance" & above_avg_ER == TRUE ~ TRUE,
                                   TRUE ~ FALSE)) %>% 
  ungroup() 

new_ER_Plot<-ggplot(plot_df,
                    aes(x = return_year, y = total_esc)) +
  
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = return_year - 0.5, xmax = return_year + 0.5,
                ymin = -Inf, ymax = Inf, fill = total_ER),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
  
  # Escapement goal dashed line
  geom_path(aes(y = mean_esc), color = "black", linetype = 2) +
  
  # Red points for vulnerable years
  geom_point(data = plot_df %>% filter(vulnerable == TRUE),
             aes(x = return_year, y = total_esc),
             color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
  
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#fff5f5",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.45, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate"
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10),
                     expand = c(0,0)) +
  
  facet_wrap(~ id, scales = "free", nrow =1) +
  
  labs(
    title    = "N BC Coho Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\nDashed line = Mean Escapement\n Red Points = Above Average Exploitation Rate & Below Average Escapement",
    x = "Year", y = "Escapement"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 10),
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 9, color = "grey40"),
    panel.grid.minor = element_blank()
  )

new_ER_Plot

ggsave("output/plots/BC_Coho_esc_goal_ER_plot.png",
       plot = new_ER_Plot,
       width = 8, height = 5,
       dpi = 300, units = "in")


### Pie Charts =========
#### Toboggan ===== 
pie_df2_t <- plot_df %>%
  filter(return_year > 2008, id == "Toboggan") %>%
  select(id, AK_harvest, CAN_harvest, Total_harvest) %>% 
  group_by(id) %>%
  summarise(Alaska = mean(AK_harvest/Total_harvest, na.rm = TRUE),
            "British Columbia" = mean(CAN_harvest/Total_harvest, na.rm = TRUE)) %>%
  ungroup() %>%
  gather(c(2:3), key = "broad_region", value = "avg_mort") %>% 
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

pie <- ggplot(pie_df2_t, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  # facet_wrap(~id) + 
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


pie

ggsave(
  filename = "output/plots/Toboggan_BC_Coho_FM_PieOnly.png",
  plot = pie,
  width =6,
  height =5.5,
  dpi = 300,
  units = "in"
)

#### Zolzap ===== 
pie_df2_z <- plot_df %>%
  filter(return_year > 2008, id == "Zolzap") %>%
  select(id, AK_harvest, CAN_harvest, Total_harvest) %>% 
  group_by(id) %>%
  summarise(Alaska = mean(AK_harvest/Total_harvest, na.rm = TRUE),
            "British Columbia" = mean(CAN_harvest/Total_harvest, na.rm = TRUE)) %>%
  ungroup() %>%
  gather(c(2:3), key = "broad_region", value = "avg_mort") %>% 
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

pie <- ggplot(pie_df2_z, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  # facet_wrap(~id) + 
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


pie

ggsave(
  filename = "output/plots/Zolzap_BC_Coho_FM_PieOnly.png",
  plot = pie,
  width =6,
  height =5.5,
  dpi = 300,
  units = "in"
)

# AK COHO ==========
# data from Will
# Here are the data for the Alaska coho CWT indicators. 
# The data is only updated through 2019, since ADFG doesn't share their coho data annually online. I'm also attaching the report that the data was drawn from. 
# I think for comparison sake to Toboggan and Zolzap we would want to use Hugh Smith and Berners Creek as the two primary Alaskan CWT populations. 
# There are escapement goal ranges defined for the Alaska populations, which are outlined in the text of the report I've attached. 
# escapement goals from page 16 (table 1) -- https://www.adfg.alaska.gov/FedAidPDFs/RIR.1J.2019.12.pdf 
 
ak_coho <- read_xlsx("data/Alaska coho escapement and run size.xlsx") %>%
  filter(Population %in% c("hugh_smith", "berners")) %>%
  dplyr::select(Population, year, spawners, total_run) %>%
  dplyr::mutate(across(c(total_run, spawners), as.numeric),
                harvest = total_run - spawners, 
                ER = harvest/total_run,
                esc_goal_lower = case_when(Population == "berners" & year < 2018 ~ 4000,
                                           Population == "hugh_smith" ~ 500, # no changes in EG
                                           Population == "berners" & year > 2017 ~ 3600),
                esc_goal_upper = case_when(Population == "berners" & year < 2018 ~ 9200,
                                           Population == "hugh_smith" & year < 2013 ~ 1100, # no changes in EG
                                           Population == "hugh_smith" & year > 2012 ~ 1600, # no changes in EG
                                           Population == "berners" & year > 2017 ~ 8100))

## ER Plot  ========== 
plot_df <- ak_coho %>% 
  dplyr::mutate(Population = case_when(Population == "berners" ~ "Berners River",
                                       Population == "hugh_smith" ~ "Hugh Smith"),
                under_goal = case_when( spawners < esc_goal_upper~ "Below Esc",
                                        TRUE ~ "NA")) %>% 
  group_by(Population) %>%
  dplyr::mutate(mean_ER = mean(ER, na.rm = TRUE),
                SD_ER = sd(ER, na.rm = TRUE),
                above_avg_ER = ER > mean_ER,
                # Vulnerable = below esc goal AND above avg AABM catch
                vulnerable = case_when(under_goal == "Below Esc" & above_avg_ER == TRUE ~ TRUE,
                                       TRUE ~ FALSE))  

new_ER_Plot<-ggplot(plot_df,
                    aes(x = year, y = spawners)) +
  
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf, fill = ER),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
  
  # Escapement goal dashed line
  geom_path(aes(y = esc_goal_upper), color = "black", linetype = 2) +
  geom_path(aes(y = esc_goal_lower), color = "black") +
  
  # Red points for vulnerable years
  geom_point(data = plot_df %>% filter(vulnerable == TRUE),
             aes(x = year, y = spawners),
             color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
  
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#fff5f5",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.45, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate"
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10),
                     expand = c(0,0)) +
  
  facet_wrap(~ Population, scales = "free", nrow =1) +
  
  labs(
    title    = "SE AK Coho Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\nLines = Escapement Goal Bounds\n Red Points = Above Average Exploitation Rate & Run Below Escapement Goal",
    x = "Year", y = "Escapement"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 10),
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 9, color = "grey40"),
    panel.grid.minor = element_blank()
  )

new_ER_Plot

ggsave("output/plots/AK_Coho_esc_goal_ER_plot.png",
       plot = new_ER_Plot,
       width = 8, height = 5,
       dpi = 300, units = "in")


# Plot CWT Recovery Locations ======
# BC Coho data from CWT - Laura Elmers CWT Shared google drive, will be public on DFO and then will want to link to that in script. 
library(here)
library(tidyverse)

cwt_load <-read_csv("data/Coho_CWT_Condensed.csv") %>%
  janitor::clean_names() %>% 
  rename_with(~ sub("^x_", "", .x)) %>% 
  filter(#rl_stock_psc_region_code == "NASK",
    !rl_stock_psc_region_code == "SEAK", 
    rc_recovery_year > 2009 & rc_recovery_year < 2021) 

t <- data.frame(unique(cwt_load[c("level_2", 
                                  "psc_fishery_name", 
                                  "psc_fishery_notes")]))

test <- cwt %>% 
  filter(rl_stock_psc_region_code == "Central BC")

cwt <- cwt_load %>% 
  dplyr::select(#rl_release_psc_region_name, 
    rl_stock_psc_region_code,           
    release_site_name_2,
    rl_release_site_name, 
    rc_recovery_year,
    level_2, 
    psc_fishery_name, 
    psc_fishery_notes) %>%
  # make custom levels for PST 
  dplyr::mutate(recovery_cat_custom = case_when(level_2 %in% c("SCAK", 
                                                               "NSEAK",
                                                               "SSEAK") ~ "AK",
                                                level_2 %in% c("NBC", "CBC") ~ "North/Central BC",
                                                level_2 %in% c("SWVI") ~ "Vancouver Island",
                                                level_2 %in% c("PUSO") ~ "WA",
                                                TRUE ~ level_2),
                rl_stock_psc_region_code = case_when(rl_stock_psc_region_code == "QCI" ~ "Haida Gwaii",
                                                     release_site_name_2 %in% c("Kitwanga R", "Slamgeesh R",
                                                                                "Zymacord R","Toboggan Cr") ~ "Skeena",
                                                     release_site_name_2 %in% c("Zolzap Cr") ~ "Nass", 
                                                     release_site_name_2 %in% c("Keogh R","Quinsam R") ~ "Vancouver Island", 
                                                     rl_stock_psc_region_code == "COBC" ~ "Central BC",
                                                     TRUE ~ "CHECK")) %>%  
  group_by(rl_stock_psc_region_code) %>% 
  count(recovery_cat_custom) %>%
  dplyr::mutate(sum = sum(n),
                proportion = n/sum,
                recovery_cat_custom = factor(recovery_cat_custom, 
                                             levels = c("AK",
                                                        "North/Central BC",
                                                        "Vancouver Island",
                                                        "WA",
                                                        "Unknown")),
                rl_stock_psc_region_code = factor(rl_stock_psc_region_code,levels = rev(c(
                  "Haida Gwaii", 
                  "Nass",
                  "Skeena",    
                  "Central BC",   
                  "Vancouver Island")))) %>%
  filter(!is.na(recovery_cat_custom))


custom_cols <- c(
  "AK"="#FFB74D",
  "North/Central BC"="#64B5F6",  
  "Vancouver Island"="#1565C0",
  "WA" ="#2E7D32",
  "Unknown"      = "grey70"
)


# make horizontal bar plot =======
coho_plot<- ggplot(
  cwt ,
  aes(
    x = (rl_stock_psc_region_code),
    y = proportion,
    fill = recovery_cat_custom
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(
    # labels = percent_format(),
    limits = c(0, 1.12),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(values = custom_cols, drop = FALSE) +
  labs(
    x = "Release Region",
    y = "Percentage of CWT Catch Recoveries",
    fill = "Recovery Region",
    title = "Distribution of BC Coho CWT Recoveries (2009-2020)"
  ) +
  coord_flip() +
  theme_minimal(base_size = 13)


ggsave(
  filename = "output/plots/BC_Coho_CWTCatch.png",
  plot = coho_plot,
  width = 7,
  height = 4,
  dpi = 300,
  units = "in",
  bg = "transparent"
)







 