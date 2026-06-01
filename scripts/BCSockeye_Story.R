library(here)
library(tidyverse)


# Load Sockeye data, emailed from Will ==========
## First Data set - Sockeye and pinks ==== 
fraser_sockeye<-read_csv("data/Sockeye_Data/Chapter4_Data_2026_05_14.csv") %>%
  
  filter(Species == "Sockeye") %>%
  dplyr::mutate(Region = recode(Region,
                         "EStu" = "Early Stuart",
                         "ESum" = "Early Summer",
                         "Summ" = "Summer",
                         "Late" = "Late",
                         "Fraser" = "Total Fraser"))
 
fraser_escapement_targets <- read_csv("data/Sockeye_Data/sockeye_escapement_targets_2026-05-29.csv") %>%
  rename(Region = "Management Group")

# join targets with sockeye data  
joined <- fraser_sockeye %>%
  left_join(fraser_escapement_targets ) %>%
  dplyr::rename(Esc_Target_Post_Season = `Spawning Escapement Target (post-season)`)
 
# Exploitation rate plots =======
fraser_sockeye_er <- joined %>%  
  dplyr::rename(Escapement = Spawners,
                ER = `Harvest Rate`) %>%
  dplyr::mutate(
                # lower_EG = 4200, 
                # upper_EG = 10000,
                under_goal = case_when(Escapement < Esc_Target_Post_Season ~ "Under Esc. Target",
                                       TRUE ~ NA),
                mean_er = mean(ER),  
                above_avg_er = ER > mean_er,
                # Vulnerable = below esc goal AND above avg AABM catch
                vulnerable = case_when(under_goal == "Under Esc. Target" & above_avg_er == TRUE ~ TRUE,
                TRUE ~ FALSE))
 
BC_sockeye_er_plot<-ggplot(fraser_sockeye_er,
                     aes(x = Year, y = Escapement)) + 
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = Year - 0.5, xmax = Year + 0.5,
                ymin = -Inf, ymax = Inf, fill = ER),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
  
  # Escapement goal dashed line
   geom_path(aes(y = Esc_Target_Post_Season), color = "black", linetype = 2) +
 
  # Red points for vulnerable years
  geom_point(data = fraser_sockeye_er %>% filter(vulnerable == TRUE),
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
  # scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) + 
  
  # facet_grid(Region~ Species,scales = "free") + 
  facet_wrap(~Region, scales = "free") +   
  
  labs(
    title    = "Sockeye Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\nDashed Lines = Escapement Target\n Red Points = Above Average Exploitation Rate & Below Average Escapement",
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

BC_sockeye_er_plot

ggsave("output/plots/BCSockeye_esc_goal_ER_plot.jpeg",
       plot = BC_sockeye_er_plot,
       width = 8, height = 5,
       dpi = 300, units = "in")
 
# Skeena Individual Populations ========
skeena <- readxl::read_xlsx("data/Sockeye_Data/Ind_Pops_Esc_Harv_Table.xlsx") %>%
  janitor::clean_names() %>% 
  dplyr::mutate(Escapement=as.numeric(t_idx_e),
                cdn_harvest = as.numeric(cdn_harvest),
                ak_harvest = as.numeric(ak_harvest), 
                total_harvest = as.numeric(total_harvest),
                ER = as.numeric(total_er)) %>% 
  dplyr::mutate(
    # lower_EG = 4200, 
    # upper_EG = 10000,
    # under_goal = case_when(Escapement < upper_EG ~ "Under Esc. Goal",
    #                        TRUE ~ NA), 
    mean_er = mean(ER, na.rm = TRUE)#,  
    # above_avg_er = ER > mean_er,
    # Vulnerable = below esc goal AND above avg AABM catch
    # vulnerable = case_when(under_goal == "Under Esc. Goal" & above_avg_er == TRUE ~ TRUE,
    # TRUE ~ FALSE))
  )

skeena_er_plot<-ggplot(skeena,
                           aes(x = year, y = Escapement)) + 
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf, fill = ER),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
  
  # Escapement goal dashed line
  # geom_path(aes(y = upper_EG), color = "black", linetype = 2) +
  
  # Red points for vulnerable years
  # geom_point(data = nass_er %>% filter(vulnerable == TRUE),
  #            aes(x = Year, y = Escapement),
  #            color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
  
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#fff5f5",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.47, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate"
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  # scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) + 
  
  # facet_grid(Region~ Species,scales = "free") + 
  facet_wrap(~cu_name, scales = "free") +   
  
  labs(
    title    = "Skeena Sockeye Escapement vs. Exploitation Rates",
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

skeena_er_plot

babine_ER_plot<-ggplot(skeena %>% filter(cu_name == "Babine-Late-Wild") %>%
                         mutate(mean_er = mean(ER)),
                       aes(x = year, y = Escapement)) + 
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf, fill = ER),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
  
  # Escapement goal dashed line
  # geom_path(aes(y = upper_EG), color = "black", linetype = 2) +
  
  # Red points for vulnerable years
  # geom_point(data = nass_er %>% filter(vulnerable == TRUE),
  #            aes(x = Year, y = Escapement),
  #            color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
  
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#fff5f5", 
    high     = "#c0392b",
    midpoint = 0.54,  
    name     = "Exploitation Rate"
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  # scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) + 
  
  # facet_grid(Region~ Species,scales = "free") + 
  facet_wrap(~cu_name, scales = "free") +   
  
  labs(
    title    = "Skeena Sockeye Escapement vs. Exploitation Rates",
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

babine_ER_plot

## AK and Can harvest for different runs ====== 
harvest_rate <- skeena %>% 
  dplyr::mutate(total_run = cdn_harvest+ak_harvest+Escapement,
         can_ER = cdn_harvest/total_run,
         AK_ER = ak_harvest/total_run) %>% 
  dplyr::select(cu_name, year, can_ER, AK_ER, Escapement) %>%
  gather(3:4, key = "id", value = "ER")

skeena_FM_stacked <- ggplot(harvest_rate,
                        aes(x = year,
                            y = ER,
                            fill = id)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  scale_y_continuous(
    expand = c(0, 0),
    name = "Fishery Mortality"#,
    # labels = scales::percent_format()
  ) +
  facet_wrap(~cu_name) + 
  scale_x_continuous(
    name = "Year",
    expand = c(0, 0)
  ) +
  # scale_fill_manual(values = custom_pal) + 
  # scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(
    fill = "Region",
    title = "Skeena Sockeye Salmon",
    subtitle = "Harvest Mortality by Region & Year"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"))

skeena_FM_stacked

unique(skeena$cu_name)

## Babine and Lakelse FM and Escapement ======
compare_df <- skeena %>%
  filter(cu_name %in% c("Babine-Late-Wild", "Lakelse"))  
  

skeena_er_plot<-ggplot(compare_df,
                           aes(x = year, y = Escapement/1000)) + 
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf, fill = ER),
            alpha = 0.8, inherit.aes = FALSE) +
  
  # Escapement bars
  geom_col(alpha = 0.85) +
  
  # Escapement goal dashed line
  # geom_path(aes(y = upper_EG), color = "black", linetype = 2) +
  
  # Red points for vulnerable years
  # geom_point(data = nass_er %>% filter(vulnerable == TRUE),
  #            aes(x = Year, y = Escapement),
  #            color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
  
  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#fff5f5",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.3, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate"
  ) +
  
  scale_y_continuous(expand = c(0,0)) +
  # scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) + 
  
  # facet_grid(Region~ Species,scales = "free") + 
  facet_wrap(~cu_name, scales = "free") +   
  
  labs(
    title    = "Skeena Sockeye Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\nDashed Lines = Escapement Goal\n Red Points = Above Average Exploitation Rate & Below Average Escapement",
    x = "Year", y = "Escapement (thousands)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 10),
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle    = element_text(hjust = 0.5, size = 9, color = "grey40"),
    panel.grid.minor = element_blank()
  )

skeena_er_plot

ggsave("output/plots/SkeenaSockeye_esc_goal_ER_plot.jpeg",
       plot = skeena_er_plot,
       width = 8, height = 5,
       dpi = 300, units = "in")



