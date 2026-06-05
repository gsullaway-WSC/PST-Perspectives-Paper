library(here)
library(tidyverse)
library(patchwork)
library(ggrepel)

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
                TRUE ~ FALSE)) %>%
  filter(Region %in% c("Late","Summer")) %>%
  dplyr::mutate(Region = factor(Region, levels = c("Summer", "Late")))
 
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
    title    = "Fraser Sockeye Escapement vs. Exploitation Rates",
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

ggsave("output/plots/Fraser_esc_goal_ER_plot.jpeg",
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
    subtitle = "Red shading = Exploitation Rates\nDashed Lines =DFO Benchmark Abundance \n Red Points = Above Average Exploitation Rate & Below Average Escapement",
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
    subtitle = "Red shading = Exploitation Rates\nDashed Lines = DFO Benchmark Abundance \n Red Points = Above Average Exploitation Rate & Below Average Escapement",
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

# Skeena FM PIE select ==== 
# POPULATIONS OF INTEREST ───────────────────────────────────────────────────
target_pops <- c("Babine-Enhanced", "Babine-Late-Wild", "Lakelse-Early-Wild")
target_years <- 2009:2020

 skeena_sub <- skeena |>
  dplyr::mutate(cu_name = case_when(cu_name == "Lakelse" ~ "Lakelse-Early-Wild",
                     TRUE ~ cu_name)) %>% 
  filter(year %in% target_years) |>
  filter(str_detect(cu_name, paste(target_pops, collapse = "|")))

harvest_sub <- skeena_sub |>
  mutate(
    total_run  = cdn_harvest + ak_harvest + Escapement,
    can_er     = cdn_harvest / total_run,
    ak_er      = ak_harvest  / total_run,
    escape_pct = Escapement  / total_run
  )

# ── PLOT 1: PIE CHARTS — mean ER by region for each population ────────────────
# Summarise mean proportional contribution across 2009-2020

pie_df <- harvest_sub |>
  group_by(cu_name) |>
  summarise(
    `CDN Harvest` = mean(can_er,     na.rm = TRUE),
    `AK Harvest`  = mean(ak_er,      na.rm = TRUE),
    Escapement    = mean(escape_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(-cu_name, names_to = "component", values_to = "proportion") |>
  dplyr::mutate(
    component = factor(component, levels = c("AK Harvest", "CDN Harvest", "Escapement")),
    label     = paste0(round(proportion * 100, 1), "%")) 

pie_colors <- c(
  "AK Harvest"  = "#ba7517",
  "CDN Harvest" = "#0f6e56",
  "Escapement"  = "#8fb8d4"
)
 
make_pie <- function(pop_name) {
  df <- pie_df |>
    filter(cu_name == pop_name) |>
    arrange(desc(component)) |>
    mutate(
      ypos    = cumsum(proportion) - proportion / 2,
      # angle helps determine left vs right side of pie for nudging
      angle   = 360 * (cumsum(proportion) - proportion / 2),
      hjust   = if_else(angle > 180, 1, 0)
    )
  
  ggplot(df, aes(x = "", y = proportion, fill = component)) +
    geom_col(width = 1, color = "white", linewidth = 0.5) +
    # white labels inside for large slices
    geom_text(data = filter(df, proportion >= 0.07),
              aes(y = ypos, label = label),
              x = 1, size = 2.8, fontface = "bold", color = "white") +
    # ggrepel for small outside labels
    geom_label_repel(data = filter(df, proportion < 0.07),
                     aes(y = ypos, label = label),
                     x = 1,
                     nudge_x      = 0.9,
                     size         = 2.8,
                     fontface     = "bold",
                     color        = "black",
                     fill         = "white",
                     label.size   = 0,        # no border on label box
                     segment.size = 0.3,
                     segment.color = "grey50",
                     direction    = "y",
                     min.segment.length = 0) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = pie_colors) +
    # scale_x_discrete(expand = expansion(add = c(0, 1.5))) +
    labs(title = pop_name, fill = NULL) +
    theme_void() +
    theme(plot.margin  = margin(0, 0, 0, 0),
           # plot.margin       = margin(0, 5, 0, 5, "mm"),
      plot.title      = element_text(face = "bold", hjust = 0.5, size = 11),
      legend.position = "none"
    )
}
 
pop_names <- unique(skeena_sub$cu_name)

pies <- map(pop_names, make_pie)

# shared legend — extract from a single pie with legend on
legend_plot <- pie_df |>
  filter(cu_name == pop_names[1]) |>
  ggplot(aes(x = "", y = proportion, fill = component)) +
  geom_col(width = 1) +
  scale_fill_manual(values = pie_colors, name = NULL) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10))

get_legend <- function(p) {
  g <- ggplotGrob(p)
  leg <- g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
  leg
}
 

pie_final <- wrap_plots(pies, nrow = 1) +
  plot_annotation(
    title    = "Skeena Sockeye — Run allocation, 2009–2020",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle = element_text(color = "grey40", size = 9, hjust = 0.5),
      plot.margin   = margin(5, 5, 5, 5)   # tight outer margin
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") 

print(pie_final)
 
ggsave("output/plots/skeena_pies.png", pie_final, 
       width = 6, height =4, dpi = 300)


## Babine and Lakelse FM and Escapement ======
compare_df <- skeena %>%
  filter(cu_name %in% c("Babine-Late-Wild", "Lakelse"))  %>%
  dplyr::mutate(
    UBM = case_when(cu_name == "Lakelse" ~ 6826,
                    cu_name == "Babine-Late-Wild" ~ 189861), 
    under_goal = case_when(Escapement < UBM ~ "Under Benchmark",
                           TRUE ~ NA),
    mean_er = mean(ER,na.rm = TRUE),  
    above_avg_er = ER > mean_er,
    # Vulnerable = below esc goal AND above avg AABM catch
    vulnerable = case_when(under_goal == "Under Benchmark" & above_avg_er == TRUE ~ TRUE,
                           TRUE ~ FALSE),
    cu_name = case_when(cu_name == "Lakelse" ~ "Lakelse-Early-Wild",
                        TRUE ~ cu_name))
  

skeena_er_plot<-ggplot(compare_df,
                           aes(x = year, y = Escapement/1000)) + 
  
  # Background shading scaled continuously to ER rate
  geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf, fill = ER),
            alpha = 0.8, inherit.aes = FALSE) +
  # Escapement bars
  geom_col(alpha = 0.85) +
  
 # Escapement goal dashed line
  geom_path(aes(y = UBM/1000), color = "black", linetype = 2) +

  #Red points for vulnerable years
  geom_point(data = compare_df %>% filter(vulnerable == TRUE),
             aes(x = year, y = Escapement/1000),
             color = "#c0392b", size = 1.5, inherit.aes = FALSE) +

  scale_fill_gradient2(
    low      = "#fff5f5",
    mid      = "#fff5f5",#"#f9c9c9", #"#f4a582",
    high     = "#c0392b",
    midpoint = 0.3, # mean(as.numeric(joined_df$er), na.rm = TRUE),
    name     = "Exploitation Rate"
  ) +
  
  # scale_y_continuous(expand = c(0,0)) +
  # scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) + 
  
  # facet_grid(Region~ Species,scales = "free") + 
  facet_wrap(~cu_name, scales = "free") +   
  
  labs(
    title    = "Skeena Sockeye Escapement vs. Exploitation Rates",
    subtitle = "Red shading = Exploitation Rates\nDashed Lines = DFO Benchmark Abundance \n Red Points = Above Average Exploitation Rate & Below Average Escapement",
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



