library(here)
library(tidyverse)

# Source: English, K. K., R. F. Alexander, I. A. Beveridge, W. Challenger, N. Percival, E. Hertz, and C.
# Atkinson. 2023. Preliminary Area 3 Salmon (2018–2022) and Nass River Summer-Run
# Steelhead (1994–2022) Escapement, Catch, Run Size, and Exploitation Rate Estimates.
# Prepared for the Pacific Salmon Foundation, Vancouver, BC, and the Nisga'a–Canada-BC
# Treaty’s Joint Fisheries Management Committee, Gitlaxt’aamiks and Prince Rupert, BC,
# by LGL Limited, Sidney, BC, and Nisga’a Fisheries and Wildlife Department,
# Gitlaxt’aamiks, BC. Nisga'a Fisheries Report #22–16: vi + 75 p

nass_df <- read_csv("data/nass_steelhead_table_f6.csv") %>%
  filter(!Year %in% c( "Min", "Max", "Averages"))

# Nass Steelhead FM - Pie =====================
nass_df_pie <- nass_df %>% 
  dplyr::select(1, 13:15) %>%
  filter(Year > 2008 ) %>%
  gather(c(2:4), key = "broad_region", value = "value") %>% 
  group_by(Year) %>%
  dplyr::mutate(pct = value/sum(value)) %>% 
  group_by(broad_region) %>% 
  summarise(avg_mort = mean(pct, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(broad_region)) %>%
  separate(broad_region, into = c("broad_region", "del"), sep = -24) %>%
  dplyr::select(-del) %>% 
  dplyr::mutate( 
    broad_region = case_when(broad_region == "Area 3" ~ "BC (Area 3)",
                             TRUE ~ broad_region),
    custom_region = case_when(broad_region == "Alaska"  ~ "AK",
                              broad_region == "BC (Area 3)"   ~ "BC",
                              broad_region == "In-River" ~ "In-River"), 
    cum_pos = cumsum(avg_mort) - avg_mort / 2,
    label = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")  # combined label
  ) 

custom_pal <- c(
  "Alaska" = "#2D6E7E",    
  "BC (Area 3)" = "#7FA8B8", 
  "In-River"= "#A8B8A0"
)

Nass_Steelhead_pie <- ggplot(nass_df_pie, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  
  # Labels inside for large slices
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort >= 0.5, paste0(label), "")),
            size = 5, fontface = "bold", color = "black") + 
  # Labels outside for small slices — nudge x past 1 to push outside pie
  geom_text(aes(y = cum_pos,
                label = ifelse(avg_mort < 0.5, paste0(label), "")),
            x = 1.63,   # >1 pushes outside the pie (pie lives at x = 1)
            size = 3.5, fontface = "bold", color = "black") + 
  coord_polar(theta = "y", clip = "off") +   
  scale_fill_manual(values = custom_pal, name = "Fishery Region") +  
  theme_void() +
  ggtitle("Nass Summer Steelhead Mortality (2009-2022)") + 
  theme(
    legend.position   = "right",
    legend.title      = element_text(size = 10, face = "bold"),
    legend.text       = element_text(size = 9),
    legend.margin     = margin(t = 10, b = -10, unit = "mm"),  # positive t pushes down, negative b pulls pie up
    # legend.key.size   = unit(0.4, "cm"),   # smaller legend keys
    legend.spacing.x  = unit(0.2, "cm"),   # tighten horizontal spacing
    plot.title = element_text(face = "bold",hjust = 0.5), 
    plot.margin       = margin(0, 5, 0, 5, "mm"),  # reduce outer margins
    plot.background   = element_blank()
  )

Nass_Steelhead_pie

ggsave(
  filename = "output/plots/Nass_Steelhead_FM_PieOnly.jpeg",
  plot = Nass_Steelhead_pie,
  width =6,
  height =5.5,
  dpi = 300,
  units = "in"
)

# Exploitation rate Plot ===========
# EGs from English et al page 8 - "Nisga’a Treaty management provisions for summer run steelhead are 
# 10,000 (80% of capacity estimate) and 4,200 (35% of capacity estimate) as an escapement goal
# and minimum lower target, respectively (NFWD 2023)." 

nass_er <- nass_df %>%
  dplyr::select(Year,`Net escapement`, `Total ER`) %>%
  dplyr::rename(Escapement = `Net escapement`,
         ER = `Total ER`) %>%
  dplyr::mutate(ER = as.numeric(gsub("%", "", ER))/100,
         Year = as.numeric(Year), 
         lower_EG = 4200, 
         upper_EG = 10000,
         under_goal = case_when(Escapement < upper_EG ~ "Under Esc. Goal",
                                 TRUE ~ NA), 
         mean_er = mean(ER),  
         above_avg_er = ER > mean_er,
         # Vulnerable = below esc goal AND above avg AABM catch
         vulnerable = case_when(under_goal == "Under Esc. Goal" & above_avg_er == TRUE ~ TRUE,
                                TRUE ~ FALSE))

nass_ER_plot<-ggplot(nass_er,
                     aes(x = Year, y = Escapement)) + 
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = Year - 0.5, xmax = Year + 0.5,
                ymin = -Inf, ymax = Inf, fill = ER),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
  
  # Escapement goal dashed line
  geom_path(aes(y = upper_EG), color = "black", linetype = 2) +
  # geom_path(aes(y = lower_EG), color = "black", linetype = 2) +
  
  # Red points for vulnerable years
  geom_point(data = nass_er %>% filter(vulnerable == TRUE),
             aes(x = Year, y = Escapement),
             color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
  
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#fff5f5",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.3, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate"
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) + 
  
  labs(
    title    = "Nass Summer Steelhead Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\nDashed Lines = Escapement Goal\n Red Points = Above Average Exploitation Rate & Below Average Escapement",
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

nass_ER_plot

ggsave("output/plots/Nass_Steelhead_esc_goal_ER_plot.jpeg",
       plot = nass_ER_plot,
       width = 8, height = 5,
       dpi = 300, units = "in")

# Skeena ER Escapement Plot ========
skeena_df <- read_csv("data/SEAK steelhead model output 2025-01-10.csv")
 
skeena_er <- skeena_df %>%
  dplyr::select(year,esc,er, lo, up) %>%
  dplyr::mutate(  
                Critical_Conservation = 24000,  
                under_goal = case_when(esc < Critical_Conservation ~ "Under Threshold",
                                       TRUE ~ NA), 
                mean_er = mean(er),  
                above_avg_er = er > mean_er,
                # Vulnerable = below esc goal AND above avg AABM catch
                vulnerable = case_when(under_goal == "Under Threshold" & above_avg_er == TRUE ~ TRUE,
                                       TRUE ~ FALSE))

skeena_ER_plot<-ggplot(skeena_er,
                     aes(x = year, y = esc)) + 
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf, fill = er),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
  
  # Conservation Threshold goal dashed line
  geom_path(aes(y = Critical_Conservation), color = "black", linetype = 2) +

  # Red points for vulnerable years
  geom_point(data = skeena_er %>% filter(vulnerable == TRUE),
             aes(x = year, y = esc),
             color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
  
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#fff5f5",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.16, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate"
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  # scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) + 
  
  labs(
    title    = "Skeena Steelhead Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\nDashed Lines = Conservation Concern Zone\n Red Points = Above Average Exploitation Rate & Below Average Escapement",
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

skeena_ER_plot

ggsave("output/plots/Skeena_Steelhead_esc_goal_ER_plot.png",
       plot = skeena_ER_plot,
       width = 8, height = 5,
       dpi = 300, units = "in")

