# Plot AABM harvest versus total stock escapements 

library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(PNWColors)
library(patchwork)  
 
esc_goals <- read_csv("data/Escapement_goals_data_all_2026-02-19.csv") %>%
  filter(#SeriesLabel %in% c("PSC-Agreed Goal","Agency Goal","PSC Goal","Agency Goal Natural Origin",
                     #     "Pre-PSC Goal: Calib. Total Adult Esc","Total Nat. Origin Esc", 
                     #       "Pre-PSC Goal: MR Esc"),
         StockName %in% c("Nooksack", "Stillaguamish", 
                          "Skagit SumFall", "Skagit Spr")) %>%          
  # filter(!is.na(Values)) %>% 
  rename(year = "Year",
         # esc_goal = "Values",
         population = "StockName")  %>%
  spread(SeriesLabel,Values) %>% 
  dplyr::mutate("Agency Goal Natural Origin" = case_when(population == "Stillaguamish" ~ 900,
                                                         # population == "Skagit Spr" & year < 2020 ~ 2000,
                                                         # population == "Skagit Spr" & year %in% c(2020,2021,2022,2023) ~ 690,
                                                         # population == "Skagit Spr" & year > 2023  ~ 1024,
                                                         # 
                                                         # population == "Skagit SumFall" & year < 2020 ~ 14500,
                                                         # population == "Skagit SumFall" & year %in% c(2020,2021,2022,2023) ~ 9202,
                                                         # population == "Skagit SumFall" & year > 2023  ~ 8201,
                                                        TRUE ~ `Agency Goal Natural Origin`)) 

# Load data =====
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>% 
  dplyr::mutate(population = case_when(population =="Skagit_sp" ~  "Skagit Spr",
                                       population =="Skagit_fa" ~  "Skagit SumFall",
                                       TRUE ~ population))
 
PS_total_run_df <- data %>%  
  dplyr::select(year, population, region, total_run, er,aabm_tot) %>% 
  filter(population %in% c("Nooksack", "Stillaguamish", "Skagit SumFall","Skagit Spr")) %>% 
  dplyr::mutate(total_run = as.numeric(total_run))  

# Join
joined_df <- PS_total_run_df %>%
  left_join(esc_goals, by = c("year", "population")) %>% 
  dplyr::mutate(esc_tot = case_when(population == "Nooksack" ~ `Total Nat. Origin Esc`,
                                    population == "Stillaguamish" ~ `Esc (tGMR)`,
                                    population == "Skagit SumFall" ~ `Esc (Redd Ct)`,
                                    population == "Skagit Spr" ~ `Esc (Redd Ct)`),
               esc_tot = as.numeric(esc_tot),
               er = as.numeric(er), 
               aabm_tot = as.numeric(aabm_tot), 
               esc_goal = as.numeric(`Agency Goal Natural Origin`),
                under_goal = case_when(esc_tot < esc_goal ~ "Under Esc. Goal",
                                       TRUE ~ "NA")) %>% 
  group_by(population) %>%
  dplyr::mutate( 
    mean_aabm = mean(aabm_tot, na.rm = TRUE),
    above_avg_aabm = aabm_tot > mean_aabm,
    # Vulnerable = below esc goal AND above avg AABM catch
    vulnerable = case_when(under_goal == "Under Esc. Goal" & above_avg_aabm == TRUE ~ TRUE,
                           TRUE ~ FALSE),
    mean_er = mean(er, na.rm = TRUE),
    above_avg_er = er > mean_er,
    # Vulnerable = below esc goal AND above avg AABM catch
    vulnerable_er = case_when(under_goal == "Under Esc. Goal" & above_avg_er == TRUE ~ TRUE,
                           TRUE ~ FALSE)) %>%
  ungroup() %>%
  filter(!year<2000, !year ==2023)

# Nooksack Stillaguamish Plot =========
## Plot with Esc Goals & ER ======
esc_goal_plotER <- ggplot(joined_df, aes(x = year, y = esc_tot)) +
  # Red shading for vulnerable years (below goal + above avg AABM)
  geom_rect(data = joined_df %>% filter(vulnerable_er),
            aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#d73027", alpha = 0.3, inherit.aes = FALSE) + 
  # Escapement bars, colored by under/over goal
  geom_col(aes(fill = under_goal), alpha = 0.85) + 
  geom_path(aes(y=esc_goal ), color = "black", linetype =2) + 
  # Red points for vulnerable years
  geom_point(data = joined_df %>% filter(vulnerable_er),
             aes(x = year, y = esc_tot),
             color = "#d73027", size = 2.5, inherit.aes = FALSE) + 
  scale_fill_manual(values = c("yes" = "#d45f5f", "no" = "#2A788E"),
                    labels = c("yes" = "Below Goal", "no" = "Above Goal"),
                    name = "Escapement") +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10)) +
  # Add vulnerable year count to facet labels
  facet_wrap(~ population, scales = "free_y", ncol = 2) +
  labs(
    title = "Puget Sound Chinook Escapement vs. Goal",
    subtitle = "Red shading = below escapement goal & above-average\n exploitation rates occurred.\nDashed line = escapement goal",
    x = "Year", y = "Escapement"
  ) + 
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 9, color = "grey40"),
    panel.grid.minor = element_blank()
  )

esc_goal_plotER

ggsave("output/plots/Puget Sound Plots/PugetSound_esc_goal_plotER.jpeg",
       plot = esc_goal_plotER,
       width = 12, height = 10,
       dpi = 300, units = "in")

 
## Background Shading, Esc Goals & ER ====== 
### Nooksack ===== 
new_ER_Plot<-ggplot(joined_df %>% 
         # filter(population %in% c("Nooksack")) %>%  
         dplyr::mutate(er = as.numeric(er)), 
         aes(x = year, y = esc_tot)) +
  
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf, fill = er),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
  
  # Escapement goal dashed line
  geom_path(aes(y = esc_goal), color = "black", linetype = 2) +
  
  # Red points for vulnerable years
  geom_point(data = joined_df %>% filter(vulnerable_er == TRUE),
             aes(x = year, y = esc_tot),
             color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
  
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#f9c9c9",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.4, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate",
    limits   = c(0, max(as.numeric(joined_df$er), na.rm = TRUE)) # force range from 0
    # breaks   = seq(0, max(as.numeric(joined_df$er), na.rm = TRUE), by = 0.1)  # optional: clean tick marks
  ) +
 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) +
  
  facet_wrap(~population, scales = "free", nrow =1) +
  
  labs(
    title    = "Nooksack Natural Origin Chinook Salmon Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\nDashed line = Escapement Goal\n Red Points = Above Average Exploitation Rate & Below Average Escapement",
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

ggsave("output/plots/Puget Sound Plots/PugetSound_esc_goal_ER_plot.jpeg",
       plot = new_ER_Plot,
       width = 8, height = 5,
       dpi = 300, units = "in")

### Stillaguamish ===== 
new_ER_Plot<-ggplot(joined_df %>% 
                      filter(population %in% c("Stillaguamish")) %>%  
                      dplyr::mutate(er = as.numeric(er)) %>%
                       dplyr::select(year, population, er,`Esc (tGMR)`)%>%   
                       dplyr::rename(esc_tot = `Esc (tGMR)`), 
                       # gather(c(4:5), key ="esc_type", value = "esc_total"), 
                    aes(x = year, y = esc_tot)) +
  
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf, fill = er),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
 
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#f9c9c9",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.4, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate",
    limits   = c(0, max(as.numeric(joined_df$er), na.rm = TRUE)) # force range from 0
    # breaks   = seq(0, max(as.numeric(joined_df$er), na.rm = TRUE), by = 0.1)  # optional: clean tick marks
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) +

  labs(
    title    = "Stillaguamish Chinook Salmon Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\n No Escapement Goals",
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

ggsave("output/plots/Puget Sound Plots/Stillaguamish_esc_goal_ER_plot.jpeg",
       plot = new_ER_Plot,
       width = 8, height = 5,
       dpi = 300, units = "in")

#"Skagit SumFall",
#"Skagit Spr" 
### Skagit Spring ===== 
skag_dat<- joined_df %>% 
  filter(population %in% c("Skagit Spr")) %>%  
  dplyr::mutate(er = as.numeric(er)) %>% 
  dplyr::rename(agency_esc_goal = "Agency Goal",
                PSC_esc_goal = "PSC-Agreed Goal") %>%
  dplyr::select(year, population, er,agency_esc_goal,PSC_esc_goal,`Esc (Redd Ct)`)%>%   
  dplyr::rename(esc_tot = `Esc (Redd Ct)`)

new_ER_Plot<-ggplot(data = skag_dat, 
                    aes(x = year, y = esc_tot)) +
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf, fill = er),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
  geom_line(aes(x = year, y = agency_esc_goal), linetype =2) + 
  geom_line(aes(x = year, y = PSC_esc_goal), color = "black") + 
  
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#f9c9c9",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.4, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate",
    limits   = c(0, max(as.numeric(joined_df$er), na.rm = TRUE)) # force range from 0
    # breaks   = seq(0, max(as.numeric(joined_df$er), na.rm = TRUE), by = 0.1)  # optional: clean tick marks
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) +
  
  labs(
    title    = "Skagit Spring Chinook Salmon Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\n Horizontal line is agency (2000-2029) and PSC (2019-2022) esc goals",
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

ggsave("output/plots/Puget Sound Plots/Skagit_SP_esc_goal_ER_plot.jpeg",
       plot = new_ER_Plot,
       width = 8, height = 5,
       dpi = 300, units = "in")

### Skagit Sum/Fall ===== 
skag_dat <- joined_df %>% 
              filter(population %in% c("Skagit SumFall")) %>%  
              dplyr::mutate(er = as.numeric(er)) %>% 
              dplyr::rename(agency_esc_goal = "Agency Goal",
                            PSC_esc_goal = "PSC-Agreed Goal") %>%
              dplyr::select(year, population, er,agency_esc_goal,PSC_esc_goal,Esc)%>%   
              dplyr::rename(esc_tot = Esc)

new_ER_Plot<-ggplot(data = skag_dat, aes(x = year, y = esc_tot)) +
   geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf, fill = er),
            alpha = 0.8, inherit.aes = FALSE) +
   # Escapement bars
  geom_col(alpha = 0.85) +
  # Background shading scaled continuously to ER rate
  geom_line(aes(x = year, y = agency_esc_goal), linetype =2) + 
  geom_line(aes(x = year, y = PSC_esc_goal), color = "black") + 
  
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#f9c9c9",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.4, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate",
    limits   = c(0, max(as.numeric(joined_df$er), na.rm = TRUE)) # force range from 0
    # breaks   = seq(0, max(as.numeric(joined_df$er), na.rm = TRUE), by = 0.1)  # optional: clean tick marks
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) +
  
  labs(
    title    = "Skagit Fall Chinook Salmon Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\n Horizontal line: agency (2000-2029) and PSC (2019-2022) esc goals",
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

ggsave("output/plots/Puget Sound Plots/Skagit_summFall_esc_goal_ER_plot.jpeg",
       plot = new_ER_Plot,
       width = 8, height = 5,
       dpi = 300, units = "in")
