# Plot AABM harvest versus total stock escapements 

library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(PNWColors)
library(patchwork)  

esc_goals <- read_csv("data/Escapement_goals_data_all_2026-02-19.csv") %>%
  filter(SeriesLabel %in% c("PSC-Agreed Goal","Agency Goal","PSC Goal",
                            "Pre-PSC Goal: Calib. Total Adult Esc",
                            "Pre-PSC Goal: MR Esc"),
         StockName %in% c("Hoh Spr Sum","Hoh Fall","Queets SprSum",
                          "Queets Fall","Grays Harbor Spr","Grays Harbor Fall",
                          "Quillayute Sum" ,"Quillayute Fall")) %>%
  rename(year = "Year") %>%
  filter(!is.na(Values) ) # this is just for grays harbor fall because double goals are confusing 
 
#  
# ggplot(data = esc_goals, aes(x=year, y = Values, color = SeriesLabel)) +
#   geom_point() +
#   facet_wrap(~StockName, scales = "free")

# Load data =====
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx")
 
OP_total_run_df <- data %>%  
  dplyr::select(year, population, region, total_run, er,esc_tot,aabm_tot) %>% 
  filter(region == "OP") %>% 
  dplyr::mutate(season = case_when(str_ends(population, "_fa") ~ "fall",
                                   str_ends(population, "_sp") ~ "spring",
                                   TRUE ~ "NA"),
                total_run = as.numeric(total_run))   

# match escapement goals with escapement data ==== 
name_lookup <- c(
  "Queets_fa"       = "Queets Fall",
  "Queets_sp"       = "Queets SprSum",
  "Hoh_fa"          = "Hoh Fall",
  "Hoh_sp"          = "Hoh Spr Sum",
  "Grays_Harbor_fa" = "Grays Harbor Fall",
  "Grays_Harbor_sp" = "Grays Harbor Spr",
  "Quillayute_fa"   = "Quillayute Fall"
)

# Standardize OP_total_run_df
OP_total_run_df <- OP_total_run_df %>%
  mutate(StockName = name_lookup[population])
# 
# t<-joined_df %>%
#   filter(population == "Grays_Harbor_fa")

# Join
joined_df <- OP_total_run_df %>%
  left_join(esc_goals, by = c("year", "StockName")) %>%
  rename(esc_goal = "Values") %>%
  dplyr::mutate(esc_tot = as.numeric(esc_tot),
               er = as.numeric(er), 
               aabm_tot = as.numeric(aabm_tot), 
                under_goal = case_when(esc_tot < esc_goal ~ "Under Esc. Goal",
                                       TRUE ~ "NA")) %>% 
  group_by(StockName) %>%
  mutate(
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
  ungroup()

# All Stocks Plot =========
## Plot with Esc Goals & AABM ======
esc_goal_plot <- ggplot(joined_df, aes(x = year, y = esc_tot)) +
  # Red shading for vulnerable years (below goal + above avg AABM)
  geom_rect(data = joined_df %>% filter(vulnerable),
            aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#d73027", alpha = 0.3, inherit.aes = FALSE) +
  
  # Escapement bars, colored by under/over goal
  geom_col(aes(fill = under_goal), alpha = 0.85) +
  
  # Escapement goal dashed line
  # geom_hline(aes(yintercept = esc_goal), linetype = "dashed", 
  #            color = "black", linewidth = 0.8) +
  geom_path(aes(y=esc_goal ), color = "black", linetype =2) + 
  # Red points for vulnerable years
  geom_point(data = joined_df %>% filter(vulnerable),
             aes(x = year, y = esc_tot),
             color = "#d73027", size = 2.5, inherit.aes = FALSE) +
  
  scale_fill_manual(values = c("yes" = "#d45f5f", "no" = "#2A788E"),
                    labels = c("yes" = "Below Goal", "no" = "Above Goal"),
                    name = "Escapement") +
  
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10)) +
  
  # Add vulnerable year count to facet labels
  facet_wrap(~ StockName, scales = "free_y", ncol = 2) +
  
  labs(
    title = "Olympic Peninsula Chinook Escapement vs. Goal",
    subtitle = "Red shading = below escapement goal & above-average AABM catch (vulnerable years)\nDashed line = escapement goal",
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

esc_goal_plot

ggsave("output/plots/esc_goal_vulnerable.jpeg",
       plot = esc_goal_plot,
       width = 12, height = 10,
       dpi = 300, units = "in")

## Plot with Esc Goals & ER ======
esc_goal_plotER <- ggplot(joined_df, aes(x = year, y = esc_tot)) +
  # Red shading for vulnerable years (below goal + above avg AABM)
  geom_rect(data = joined_df %>% filter(vulnerable_er),
            aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#d73027", alpha = 0.3, inherit.aes = FALSE) +
  
  # Escapement bars, colored by under/over goal
  geom_col(aes(fill = under_goal), alpha = 0.85) +
  
  # # Escapement goal dashed line
  # geom_hline(aes(yintercept = esc_goal), linetype = "dashed", 
  #            color = "black", linewidth = 0.8) +
  # 
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
  facet_wrap(~ StockName, scales = "free_y", ncol = 2) +
  
  labs(
    title = "Olympic Peninsula Chinook Escapement vs. Goal",
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

ggsave("output/plots/esc_goal_plotER.jpeg",
       plot = esc_goal_plotER,
       width = 12, height = 10,
       dpi = 300, units = "in")


 
# Select stocks plot ====
select_joined_df <- joined_df %>%
        filter(StockName %in% 
                c("Hoh Spr Sum", "Queets SprSum", "Quillayute Fall")) %>%
         dplyr::mutate(StockName = case_when(StockName =="Hoh Spr Sum" ~ "Hoh S.",
                                     StockName =="Queets SprSum" ~ "Queets S.", 
                                     StockName =="Quillayute Fall" ~ "Quillayute F.",
                                     TRUE ~ StockName))

## Plot AABM and Esc Goals======
select_esc_goal_plot <- ggplot(select_joined_df, 
                        aes(x = year, y = esc_tot)) +
  # Red shading for vulnerable years (below goal + above avg AABM)
  geom_rect(data = select_joined_df %>% filter(vulnerable),
            aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#d73027", alpha = 0.3, inherit.aes = FALSE) +
  
  # Escapement bars, colored by under/over goal
  geom_col(aes(fill = under_goal), alpha = 0.85) +
  
  # Escapement goal dashed line
  # geom_hline(aes(yintercept = esc_goal), linetype = "dashed", 
  #            color = "black", linewidth = 0.8) +
  geom_path(aes(y=esc_goal ), color = "black", linetype =2) + 
  # Red points for vulnerable years
  geom_point(data = select_joined_df %>% filter(vulnerable),
             aes(x = year, y = esc_tot),
             color = "#d73027", size = 2.5, inherit.aes = FALSE) +
  
  scale_fill_manual(values = c("yes" = "#d45f5f", "no" = "#2A788E"),
                    labels = c("yes" = "Below Goal", "no" = "Above Goal"),
                    name = "Escapement") +
  
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10)) +
  
  # Add vulnerable year count to facet labels
  facet_wrap(~ StockName, scales = "free_y", ncol = 1) +
  
  labs(
    title = "Olympic Peninsula Chinook- AABM Catch vs Escapement",
    subtitle = "Red shading = below escapement goal & above-average AABM catch (vulnerable years)\nDashed line = escapement goal",
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

select_esc_goal_plot

ggsave("output/plots/select_esc_goal_plot.jpeg",
       plot = select_esc_goal_plot,
       width = 12, height = 10,
       dpi = 300, units = "in")

## Plot ER and Esc Goals   ======
select_esc_goal_plotER <- ggplot(select_joined_df,
                                 aes(x = year, y = esc_tot)) +
  # Red shading for vulnerable years (below goal + above avg AABM)
  geom_rect(data = select_joined_df %>% filter(vulnerable_er),
            aes(xmin = year - 0.5, xmax = year + 0.5,
                ymin = -Inf, ymax = Inf),
            fill = "#d73027", alpha = 0.3, inherit.aes = FALSE) +
  
  # Escapement bars, colored by under/over goal
  geom_col(aes(fill = under_goal), alpha = 0.85) +
  
  # # Escapement goal dashed line
  # geom_hline(aes(yintercept = esc_goal), linetype = "dashed", 
  #            color = "black", linewidth = 0.8) +
  # 
  geom_path(aes(y=esc_goal ), color = "black", linetype =2) + 
  # Red points for vulnerable years
  geom_point(data = select_joined_df %>% filter(vulnerable_er),
             aes(x = year, y = esc_tot),
             color = "#d73027", size = 2.5, inherit.aes = FALSE) +
  
  scale_fill_manual(values = c("yes" = "#d45f5f", "no" = "#2A788E"),
                    labels = c("yes" = "Below Goal", "no" = "Above Goal"),
                    name = "Escapement") +
  
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0, 0)) +
  
  # Add vulnerable year count to facet labels
  facet_wrap(~ StockName, scales = "free_y", ncol = 1) +
  
  labs(
    title = "High Ocean Harvest Occurs in Years with Low Escapement",
    subtitle = "Red shading = below escapement goal & above-average\nexploitation rates occurred.\nDashed line = escapement goal",
    x = "Year", y = "Escapement"
  ) + 
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 16),      # tick labels (both axes)
    # axis.text.x = element_text(size = 14),    # x tick labels only
    # axis.text.y = element_text(size = 14),    # y tick labels only
    axis.title = element_text(size = 18),     # axis titles (both)
    # axis.title.x = element_text(size = 16),   # x axis title only
    # axis.title.y = element_text(size = 16),
    strip.text = element_text(face = "bold", size = 18),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 24),
    plot.subtitle = element_text(hjust = 0.5, size = 20, color = "grey40"),
    panel.grid.minor = element_blank()
  )

select_esc_goal_plotER

ggsave("output/plots/select_esc_goal_plotER.jpeg",
       plot = select_esc_goal_plotER,
       width = 12, height = 10,
       dpi = 300, units = "in")

# Plot quadrant =====
simple_scatter_meanscale <- data %>%  
  dplyr::select(year, population, region, total_run, 
                tot_mixedER, aabm_tot) %>% 
  filter(region == "OP") %>% 
  dplyr::mutate(
    season = case_when(str_ends(population, "_fa") ~ "fall",
                       str_ends(population, "_sp") ~ "spring",
                       TRUE ~ "NA"),
    total_run = as.numeric(total_run),
    tot_mixedER = as.numeric(tot_mixedER),  
    aabm_tot = as.numeric(aabm_tot) * 1000  # it is in thousands
  ) %>%
  group_by(population, region) %>%
  dplyr::mutate(
    exploitation_rate = tot_mixedER*100, #(aabm_tot / total_run) * 100,
    avg_run = mean(total_run, na.rm = TRUE),
    avg_catch = mean(aabm_tot, na.rm = TRUE),
    avg_ER = mean(exploitation_rate, na.rm = TRUE),
    weak_stock = total_run < avg_run, 
    high_exploitation = exploitation_rate > mean(exploitation_rate, na.rm = TRUE),
    high_aabm = aabm_tot > mean(aabm_tot, na.rm = TRUE), 
    # high_exploitation = exploitation_rate > median(exploitation_rate, na.rm = TRUE),
    # Flag particularly vulnerable stocks
    highlighted = if_else(population %in% c("Hoh_sp", "Queets_sp"), 
                          "Weak Stocks", "Other Stocks")
  ) %>%
  ungroup() %>%
  mutate(population = case_when( population == "Hoh_sp" ~ "Hoh S.",
                                 population == "Hoh_fa" ~ "Hoh F.",
                                 population == "Queets_sp" ~ "Queets S.",
                                 population == "Queets_fa" ~ "Queets F.",
                                 population == "Grays_Harbor_sp" ~ "Grays Harbor S.",
                                 population == "Grays_Harbor_fa" ~ "Grays Harbor F.",
                                 population == "Quillayute_fa" ~ "Quillayute F."
  )) %>% 
  group_by(population) %>%
  dplyr::mutate(
    n_vulnerable = sum(weak_stock & high_aabm, na.rm = TRUE),
    population_label = paste0(population, " (n=", n_vulnerable, ")")) %>% 
  ungroup() %>%
  dplyr::mutate(  population_label = fct_reorder(population_label, n_vulnerable, .desc = TRUE)) %>%
  mutate(
    zone = case_when(
      total_run >= avg_run & exploitation_rate >= avg_ER ~ "High abundance,\nHigh fishing\n", 
      total_run >= avg_run & exploitation_rate < avg_ER ~  "High abundance,\nLow fishing\n(Low Risk)",
      total_run < avg_run & exploitation_rate >= avg_ER ~ "Low abundance,\nHigh fishing\n(High Risk)", 
      total_run < avg_run & exploitation_rate < avg_ER ~ "Low abundance,\nLow fishing\n"
    ),
    zone = factor(zone, levels = c( 
      "Low abundance,\nHigh fishing\n(High Risk)", 
      "Low abundance,\nLow fishing\n",  
      "High abundance,\nHigh fishing\n",
      "High abundance,\nLow fishing\n(Low Risk)"  
    ))
  ) %>%
  filter(!is.na(zone) )   %>% 
  group_by(population) %>%
  dplyr::mutate(total_run = as.numeric(scale(total_run)),
                exploitation_rate = as.numeric(scale(exploitation_rate))) %>%
  filter(!total_run>4)

simple_plot <- ggplot(simple_scatter_meanscale, 
                      aes(x = total_run, y = exploitation_rate)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", 
             color = "gray40", size = 0.8) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", 
             color = "gray40", size = 0.8) +
  geom_point(aes(color = zone, shape = population), size = 4, alpha = 0.8) +
  # facet_wrap(~ population, scales = "free", ncol = 3) +
  scale_color_manual(
    values = c(
      "Low abundance,\nLow fishing\n" = "darkgray",
      "Low abundance,\nHigh fishing\n(High Risk)" = "#C62828",
      "High abundance,\nLow fishing\n(Low Risk)" = "lightgray",
      "High abundance,\nHigh fishing\n" = "black"
    ),
    breaks = c(
      "Low abundance,\nHigh fishing\n(High Risk)",
      "Low abundance,\nLow fishing\n",
      "High abundance,\nHigh fishing\n",
      "High abundance,\nLow fishing\n(Low Risk)"
    ),
    name = ""
  ) +
  scale_x_continuous(limits = c(-2, 2.5)) +
  labs(
    title = " ",
    # subtitle = "Dashed lines show average values | Red points = concerning pattern",
    x = "Salmon Run Size (Mean-Scaled))",
    y = "Exploitation Rate (Mean-Scaled)",
    shape = "Population"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))

print(simple_plot)
ggsave("output/plots/simple_zones_scatter.jpeg", simple_plot, 
       width = 12, height = 8, dpi = 300)

# Combine + save ========
simple_plot <- simple_plot +
  theme(legend.position = "right") +
  guides(color = guide_legend(ncol = 1),
         shape = guide_legend(ncol = 1))

fig_3 <- ggpubr::ggarrange(simple_plot, select_esc_goal_plotER, 
                           ncol = 2, widths = c(1.5, 1))

ggsave("output/plots/Aggregate_Catch_Escapement.jpeg", fig_3, 
       width = 9, height = 6, dpi = 300)

 

# Old plots with ER, total run and AABm: ===========
## test plot run sie  ======
ggplot(data = OP_total_run_df, aes(x = year, y = total_run, group = population, color = population)) +
  geom_point() +
  geom_line() +
  facet_wrap(~population)  

## test plot aabm ====
ggplot(data = aabmcatch, aes(x = Year, y = Values, group = Fishery, color = Fishery)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Fishery) 

# Make DF ======= 
corr_df_raw <- data %>%  
  dplyr::select(year, population, region, total_run, 
                tot_mixedER, aabm_tot) %>% 
  filter(region == "OP") %>% 
  dplyr::mutate(
    season = case_when(str_ends(population, "_fa") ~ "fall",
                       str_ends(population, "_sp") ~ "spring",
                       TRUE ~ "NA"),
    total_run = as.numeric(total_run),
    tot_mixedER = as.numeric(tot_mixedER),  
    aabm_tot = as.numeric(aabm_tot) * 1000  # it is in thousands
  ) %>%
  group_by(population, region) %>%
  dplyr::mutate(
    exploitation_rate = tot_mixedER*100, #(aabm_tot / total_run) * 100,
    avg_run = mean(total_run, na.rm = TRUE),
    avg_catch = mean(aabm_tot, na.rm = TRUE),
    avg_ER = mean(exploitation_rate, na.rm = TRUE),
    weak_stock = total_run < avg_run, 
    high_exploitation = exploitation_rate > mean(exploitation_rate, na.rm = TRUE),
    high_aabm = aabm_tot > mean(aabm_tot, na.rm = TRUE), 
    # high_exploitation = exploitation_rate > median(exploitation_rate, na.rm = TRUE),
    # Flag particularly vulnerable stocks
    highlighted = if_else(population %in% c("Hoh_sp", "Queets_sp"), 
                          "Weak Stocks", "Other Stocks")
  ) %>%
  ungroup()

# Keep standardized version for correlation analysis
corr_df <- data %>%  
  dplyr::select(year, population, region, total_run, tot_mixedER, aabm_tot) %>% 
  filter(region == "OP") %>% 
  dplyr::mutate(
    season = case_when(str_ends(population, "_fa") ~ "fall",
                       str_ends(population, "_sp") ~ "spring",
                       TRUE ~ "NA"),
    total_run = as.numeric(total_run),
    tot_mixedER = as.numeric(tot_mixedER),  
    aabm_tot = as.numeric(aabm_tot) * 1000
  ) %>%
  group_by(population, region) %>% 
  dplyr::mutate(
    total_run = as.numeric(scale(total_run)),
    aabm_tot = as.numeric(scale(aabm_tot))
  ) %>%
  select(-season) 

# Correlation summary
corr_summary <- corr_df %>%
  group_by(population) %>%
  summarise(
    correlation = cor(aabm_tot, total_run, use = "complete.obs"),
    n_obs = n()
  )

print(corr_summary)

# ===== VISUALIZATION OPTIONS =====

corr_df_raw_labeled <- corr_df_raw %>%
  mutate(population = case_when( population == "Hoh_sp" ~ "Hoh S.",
                                 population == "Hoh_fa" ~ "Hoh F.",
                                 population == "Queets_sp" ~ "Queets S.",
                                 population == "Queets_fa" ~ "Queets F.",
                                 population == "Grays_Harbor_sp" ~ "Grays Harbor S.",
                                 population == "Grays_Harbor_fa" ~ "Grays Harbor F.",
                                 population == "Quillayute_fa" ~ "Quillayute F."
  )) %>% 
  group_by(population) %>%
  dplyr::mutate(
    n_vulnerable = sum(weak_stock & high_aabm, na.rm = TRUE),
    population_label = paste0(population, " (n=", n_vulnerable, ")")) %>% 
  ungroup() %>%
  dplyr::mutate(  population_label = fct_reorder(population_label, n_vulnerable, .desc = TRUE))

### 3A: Highlight years when weak stocks overlap with high AABM  ======
# plot 3 used high exploitation rates but i am not sure if those are AABM or all mixed stock fishing 

# plot3A_vulnerability <- ggplot(corr_df_raw_labeled, aes(x = year)) +
#   # Shade vulnerable periods (weak stock + high exploitation)
#   geom_rect(
#     data = corr_df_raw_labeled %>% filter(weak_stock & high_aabm),
#     aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf),
#     fill = "red", alpha = 0.15
#   ) +
#   geom_line(aes(y = total_run, color = "Total Run"), size = 1) +
#   geom_line(aes(y = aabm_tot, color = "AABM Catch"), size = 1) +
#   geom_hline(aes(yintercept = avg_run), linetype = "dashed", color = "blue") +
#   facet_wrap(~ population_label, scales = "free_y", ncol = 3) +
#   scale_color_manual(
#     values = c("Total Run" = "steelblue", "AABM Catch" = "darkred"),
#     name = "Type"
#   ) +
#   labs(
#     title = "Vulnerable Periods: Weak Stocks Face High Exploitation",
#     subtitle = "Red shading = below-average runs overlap with above-average AABM catch\nn = number of vulnerable years",
#     y = "Abundance",
#     x = "Year"
#   ) +
#   theme_bw() +
#   theme(
#     strip.text = element_text(face = "bold"),
#     legend.position = "bottom"
#   )
# 
# print(plot3A_vulnerability)
# ggsave("plots/plot3A_vulnerability.jpeg", width = 8, height =6)

## 3B with points =========
plot_aabm_points <- corr_df_raw_labeled %>%
  dplyr::mutate( ER_point_colors = case_when(weak_stock ==TRUE & high_exploitation == TRUE ~ "Vulnerable",
                                          TRUE ~ "Average"),
                 AABM_point_colors = case_when(weak_stock ==TRUE & aabm_tot == TRUE ~ "high",
                                             TRUE ~ "low"))

plot3B_vulnerability <- ggplot(plot_aabm_points, aes(x = year,y = total_run)) +
  # Shade vulnerable periods (weak stock + high exploitation)
  geom_rect(
    data = plot_aabm_points %>% filter(weak_stock & high_aabm),
    aes(xmin = year - 0.5, xmax = year + 0.5, ymin = -Inf, ymax = Inf),
    fill = "red", alpha = 0.15
  ) +
  geom_point( data = plot_aabm_points %>% filter(ER_point_colors == "Vulnerable"),
              aes(color = ER_point_colors)) +
  geom_line() + 
  # geom_line(aes(y = total_run, color = "Total Run", size = 1)) +
  # geom_line(aes(y = aabm_tot, color = "AABM Catch"), size = 1) +
  geom_hline(aes(yintercept = avg_run), linetype = "dashed", color = "blue") +
  facet_wrap(~ population_label, scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = c("Average" = "forestgreen", "Vulnerable" = "red"),
    name = " "
  ) +
  labs(
    title = "Vulnerable Periods: Weak Stocks Face High Exploitation",
    subtitle = "Red shading/Red Points = Years with below-average runs and above-average AABM catch\nn = number of vulnerable years",
    y = "Abundance",
    x = "Year"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold")
  )
 
print(plot3B_vulnerability)
ggsave("output/plots/plot3B_vulnerability.jpeg", width = 8, height =5)

##4A: Scatter plot showing exploitation rate vs stock size  ======
plot4A_scatter <- ggplot( corr_df_raw, 
                         aes(x = total_run, y = exploitation_rate)) +
  # geom_rect(aes(xmin = -Inf, xmax = 0, ymin = avg_ER, ymax = Inf),
  #           fill = "red", alpha = 0.02, inherit.aes = FALSE) +
  geom_point(aes(color = year), size = 3, alpha = 0.7) +
  # geom_vline(aes(xintercept = avg_run), linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  facet_wrap(~ population, scales = "free", ncol = 3) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Exploitation Rate vs Stock Size",
    subtitle = "Dashed line = average run size. Ideal pattern: high exploitation at high abundance, low at low abundance",
    x = "Total Run Size",
    y = "Exploitation Rate (%)",
    color = "Year"
  ) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"))

print(plot4A_scatter)

ggsave("output/plots/scatter_exploitationVSstocksize.jpg", plot4A_scatter, 
       width = 12, height = 8, dpi = 300)

## 4B: Scatter plot with quadrant, mean scale exploitation rate + stock size  ======
plot4B_scatter <- ggplot( corr_df_raw %>% 
                           group_by(population) %>% 
                           dplyr::mutate(total_run= as.numeric(scale(total_run))),
                         aes(x = total_run, y = exploitation_rate)) +
  # Add quadrant backgrounds
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = avg_ER, ymax = Inf),
            fill = "red", alpha = 0.02, inherit.aes = FALSE) +
  # geom_rect(aes(xmin = 0, xmax = Inf, ymin = avg_ER, ymax = Inf),
  #           fill = "lightgreen", alpha = 0.02, inherit.aes = FALSE) +
  geom_point(aes(color = year), size = 3, alpha = 0.7) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "black") +
  geom_hline(aes(yintercept = avg_ER), linetype = "dashed", color = "black") +
  facet_wrap(~ population, scales = "free", ncol = 3) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Exploitation Rate vs Stock Size",
    # subtitle = "Dashed lines indicate average run size (xaxis) and average exploitation rate (yaxis).",
    x = "Total Run Size",
    y = "Exploitation Rate (%)",
    color = "Year"
  ) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "bottom")

print(plot4B_scatter)
ggsave("output/plots/quadrant_exploitationVSstocksize.jpg", plot4B_scatter, 
       width = 12, height = 8, dpi = 300)


# # OPTION 6: Summary statistics table
# vulnerability_summary <- corr_df_raw %>%
#   group_by(population) %>%
#   summarise(
#     avg_run_size = mean(total_run, na.rm = TRUE),
#     avg_catch = mean(aabm_tot, na.rm = TRUE),
#     avg_exploitation_rate = mean(exploitation_rate, na.rm = TRUE),
#     max_exploitation_rate = max(exploitation_rate, na.rm = TRUE),
#     pct_years_weak = mean(weak_stock, na.rm = TRUE) * 100,
#     pct_years_weak_AND_high_exploit = mean(weak_stock & high_exploitation, na.rm = TRUE) * 100,
#     correlation = cor(aabm_tot, total_run, use = "complete.obs")
#   ) %>%
#   arrange(desc(pct_years_weak_AND_high_exploit))
# 
# print(vulnerability_summary)



## 5 - Super Simple options =====
### Traffic light visualization ======
traffic_light_plot <- corr_df_raw_labeled %>%
  mutate(
    status = case_when(
      weak_stock & high_exploitation ~ "High Risk",
      weak_stock & !high_exploitation ~ "Caution",
      !weak_stock & high_exploitation ~ "Monitor",
      TRUE ~ "Good"
    ),
    status = factor(status, levels = c("Good", "Monitor", "Caution", "High Risk"))
  ) %>%
  ggplot(aes(x = year, y = population, fill = status)) +
  geom_tile(color = "white", size = 1) +
  scale_fill_manual(
    values = c("Good" = "#2E7D32", 
               "Monitor" = "#FDD835", 
               "Caution" = "#FB8C00",
               "High Risk" = "#C62828"),
    name = "Stock Status"
  ) +
  labs(
    title = "Salmon Stock Health Over Time",
    subtitle = "Red = Low abundance + High fishing pressure (most vulnerable)\nGreen = Healthy abundance",
    x = "Year",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(face = "bold")
  )

print(traffic_light_plot)
ggsave("output/plots/traffic_light_status.jpg", traffic_light_plot, 
       width = 10, height = 6, dpi = 300)


## Imbalance score visualization ======
imbalance_data <- corr_df_raw_labeled %>%
  group_by(population) %>%
  summarise(
    # Years when fishing was high despite low abundance
    risky_years = sum(weak_stock & high_exploitation, na.rm = TRUE),
    # Years when fishing was appropriately low with low abundance
    protective_years = sum(weak_stock & !high_exploitation, na.rm = TRUE),
    total_weak_years = sum(weak_stock, na.rm = TRUE),
    # Calculate "protection rate" when stocks are weak
    protection_rate = if_else(total_weak_years > 0, 
                              protective_years / total_weak_years * 100, 
                              NA_real_)
  ) %>%
  arrange(protection_rate)

imbalance_plot <- ggplot(imbalance_data, 
                         aes(x = reorder(population, protection_rate), 
                             y = protection_rate)) +
  geom_col(aes(fill = protection_rate), width = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray30", size = 1) +
  coord_flip() +
  scale_fill_gradient2(
    low = "#C62828", 
    mid = "#FDD835", 
    high = "#2E7D32",
    midpoint = 50,
    name = "% Protected"
  ) +
  labs(
    title = "How Well Are Weak Stocks Protected?",
    subtitle = "When salmon runs are below average, how often is fishing pressure also reduced?\nAbove 50% = fishing adjusts appropriately | Below 50% = fishing stays high despite low abundance",
    x = "",
    y = "% of Low-Abundance Years with Reduced Fishing Pressure"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

print(imbalance_plot)
ggsave("output/plots/protection_rate_bars.jpg", imbalance_plot, 
       width = 10, height = 6, dpi = 300)

## Simplified scatter with zones ======
simple_scatter <- corr_df_raw_labeled %>%
  mutate(
    zone = case_when(
      total_run >= avg_run & exploitation_rate >= avg_ER ~ "High abundance,\nHigh fishing\n", 
      total_run >= avg_run & exploitation_rate < avg_ER ~  "High abundance,\nLow fishing\n(Low Risk)",
      total_run < avg_run & exploitation_rate >= avg_ER ~ "Low abundance,\nHigh fishing\n(High Risk)", 
      total_run < avg_run & exploitation_rate < avg_ER ~ "Low abundance,\nLow fishing\n"
    ),
    zone = factor(zone, levels = c(
      "Low abundance,\nLow fishing\n",
      "Low abundance,\nHigh fishing\n(High Risk)", 
      "High abundance,\nLow fishing\n(Low Risk)",
      "High abundance,\nHigh fishing\n" 
    ))
  ) %>%
  filter(!is.na(zone))

simple_plot <- ggplot(simple_scatter, 
                      aes(x = total_run, y = exploitation_rate)) +
  geom_vline(aes(xintercept = avg_run), linetype = "dashed", 
             color = "gray40", size = 0.8) +
  geom_hline(aes(yintercept = avg_ER), linetype = "dashed", 
             color = "gray40", size = 0.8) +
  geom_point(aes(color = zone, shape = zone), size = 4, alpha = 0.8) +
  facet_wrap(~ population, scales = "free", ncol = 3) +
  scale_color_manual(
    values = c(
      "Low abundance,\nLow fishing\n" = "#2E7D32",
      "Low abundance,\nHigh fishing\n(High Risk)" = "#C62828",
      "High abundance,\nLow fishing\n(Low Risk)" = "#1976D2",
      "High abundance,\nHigh fishing\n" = "#388E3C"
    ),
    name = ""
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 18),
    name = ""
  ) +
  labs(
    title = "Exploitation Rate vs Individual Run Size",
    # subtitle = "Dashed lines show average values | Red points = concerning pattern",
    x = "Salmon Run Size",
    y = "Exploitation Rate (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))

print(simple_plot)
ggsave("output/plots/simple_zones_scatterFACET.jpg", simple_plot, 
       width = 12, height = 8, dpi = 300)
# 
# simple_scatter_meanscale <- simple_scatter %>% 
#   group_by(population) %>%
#   dplyr::mutate(total_run = as.numeric(scale(total_run)),
#                 exploitation_rate = as.numeric(scale(exploitation_rate)))
#   
# simple_plot <- ggplot(simple_scatter_meanscale, 
#                       aes(x = total_run, y = exploitation_rate)) +
#   geom_vline(aes(xintercept = 0), linetype = "dashed", 
#              color = "gray40", size = 0.8) +
#   geom_hline(aes(yintercept = 0), linetype = "dashed", 
#              color = "gray40", size = 0.8) +
#   geom_point(aes(color = zone, shape = population), size = 4, alpha = 0.8) +
#   # facet_wrap(~ population, scales = "free", ncol = 3) +
#   scale_color_manual(
#     values = c(
#       "Low abundance,\nLow fishing\n" = "gray",
#       "Low abundance,\nHigh fishing\n(High Risk)" = "#C62828",
#       "High abundance,\nLow fishing\n(Low Risk)" = "gray",
#       "High abundance,\nHigh fishing\n" = "gray"
#     ),
#     name = ""
#   ) + 
#   labs(
#     title = "Exploitation Rate vs OP Chinook Salmon Run Sizes",
#     # subtitle = "Dashed lines show average values | Red points = concerning pattern",
#     x = "Salmon Run Size (Mean-Scaled))",
#     y = "Exploitation Rate (Mean-Scaled)",
#     shape = "Population"
#   ) +
#   theme_bw(base_size = 12) +
#   theme(
#     strip.text = element_text(face = "bold", size = 12),
#     legend.position = "bottom",
#     legend.text = element_text(size = 10)
#   ) +
#   guides(color = guide_legend(nrow = 2),
#          shape = guide_legend(nrow = 2))
# 
# print(simple_plot)
# ggsave("output/plots/simple_zones_scatter.jpg", simple_plot, 
#        width = 12, height = 8, dpi = 300)
