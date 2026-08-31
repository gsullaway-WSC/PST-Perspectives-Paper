# Exploitation Rates for just ocean fisheries for all stocks.
# Asks are there years when ER is high and proceeding escapement is low??

library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(PNWColors)
library(patchwork)
library(knitr)

region_pal <- c(
  # British Columbia — greens
  "NBC"  = "#4A7A50",
  "SBC"  = "#6A9B5A",
  "WCVI" = "#8A9E7A",
  
  # Alaska — purples
  "SEAK" = "#7A4A8A",
  "AK"   = "#9A6AAA",
  
  # Washington — blues
  "WA"   = "#2E5F6E",
  "OP"   = "#4A7F8E",
  "PS"   = "#6A9BA8",
  
  # Oregon — oranges/browns
  "OR"   = "#B57A4A",
  "OC"   = "#C58A5A",
  "ORC"  = "#D59A6A",
  "MCR"  = "#A56A3A",
  "LCR"  = "#956030",
  "COL"  = "#854A20"
)

region_to_jurisdiction <- c(
  # British Columbia
  "NBC"  = "British Columbia",
  "SBC"  = "British Columbia",
  "WCVI" = "British Columbia",
  
  # Alaska
  "SEAK" = "Alaska",
  "AK"   = "Alaska",
  
  # Washington
  "WA"   = "Washington",
  "OP"   = "Washington",
  "PS"   = "Washington",    # Puget Sound
  
  # Oregon
  "MCR" = "Oregon",
  "LCR"= "Oregon",
  "OC" = "Oregon",
  "ORC" = "Oregon",
  "OR"   = "Oregon",
  "COL"  = "Oregon"         # Columbia — adjust if Columbia stocks split WA/OR
)

# Geographic region order, North to South, for consistent axis ordering in
# plots (e.g. the Fig4 lollipop). Alaska is northernmost, working down through
# BC, Washington, to Oregon/Columbia. Adjust if any stock's region code
# belongs at a different point in the coastwide sequence.
region_order_n_to_s <- c(
  "AK", "SEAK",             # Alaska
  "NBC", "SBC", "WCVI",     # British Columbia
  "OP", "PS", "WA",         # Washington
  "MCR", "LCR", "COL", "OC", "ORC", "OR"  # Oregon / Columbia
)
 
## Load Esc Goals ======
esc_goals <- read_csv("data/Escapement_Goals_Use.csv")   %>%
  filter(!year < 2000) %>%
  gather(c(5:10), key = "goal_type", value = "escapement_goal") %>%
  dplyr::select(-Note) %>%
  dplyr::mutate(population = case_when(
    StockName == "Siuslaw R."   ~ "Siuslaw Fall",
    StockName == "Nehalem R."   ~ "Nehalem Fall",
    StockName == "South_umpqua" ~ "South Umpqua",
    StockName == "Unuk River"   ~ "Unuk",
    StockName == "Atnarko Wild" ~ "Atnarko",
    StockName == "Harrison"     ~ "Harrison River",
    StockName == "Stikine"      ~ "Stikine River",
    StockName == "Taku"         ~ "Taku River",
    StockName == "Cowichan"     ~ "Cowichan River Fall",
    StockName == "Lewis"        ~ "Lewis River Wild",
    StockName == "Chilkat"      ~ "Chilkat River",
    TRUE ~ StockName
  )) %>%
  filter(!year > 2023)

# All goals for plotting
esc_goals_all <- esc_goals %>%
  filter(!is.na(escapement_goal))  

# Table S1 Stock list ===== 
sufficient_data_populations <- c(
  "Chilkat River",
  "Unuk River",
  "Taku River",
  "Stikine River",
  "Atnarko River",
  "Cowichan River Fall",
  "Lower Shuswap River Summer",
  "Harrison River",
  "Skagit Spring Fingerling",
  "Skagit Summer Fingerling",
  "Queets Fall Fingerling",
  "Quillayute",
  "Hoh",
  "Columbia River Upriver Bright",
  "Hanford Wild Brights",
  "Lewis River Wild",
  "Columbia River Summers",
  "Nehalem",
  "Siletz",
  "Siuslaw"
)
 
## Load master data =====
# Filter to just use stocks in table S2
master <- read_csv("data/PSC_CTC_Chinook_master_table.csv") %>%
  filter(!calendar_year > 2023,
         !calendar_year < 2000,
          population %in% sufficient_data_populations,
         !population == "Hanford Wild Brights")


## Look at ER Limits that are stock specific 
# # CYER data taken from their CTC ER 2025
# # this sum is across CAN and US ers!
# cyer <- read_csv("data/CYER_Limits_Only.csv") %>%
#   filter(!is.na(`CYER_Limit_2019-2028`)) %>%
#   group_by(Population) %>%
#   summarise(`CYER_Limit_2019-2028` = sum(`CYER_Limit_2019-2028`)) %>%
#   rename(population = "Population")

## Line up the population names ==== 
population_recode <- tribble(
  ~stock_code,     ~population_old_style,
  "ATN",           "Atnarko",
  "UNU",           "Unuk",
  "SHU",           "Lower Shuswap",
  "Siuslaw",       "Siuslaw Fall",
  "Siletz",        "Siletz Fall",
  "Nehalem",       "Nehalem Fall",
  "South Umpqua",  "South Umpqua",
  "SUM",           "Columbia Summers",
  "URB",           "Columbia Upriver Brights",
  "CWF",           "Cowlitz Fall",
  "SKF",           "Skagit Spr",
  "SSF",           "Skagit SumFall",
  "SKY",           "Snohomish",
  "Quillayute",    "Quillayute Fall",
  "Hoh",           "Hoh Fall",
  "Grays Harbor",  "Grays Harbor Fall",
  "QUE",           "Queets Fall"
)

total_run_df <- master %>%
  left_join(population_recode, by = "stock_code") %>%
  mutate(population = coalesce(population_old_style, population)) %>%
  transmute(
    year        = calendar_year,
    population,
    region,                          
    total_run   = as.numeric(total_run),
    ocean_er    = marine_er / 100,
    terminal_er = terminal_er / 100, 
    esc_tot     = as.numeric(escapement)
  )

# ---- region-code recode onto the existing region_pal / region_order_n_to_s.
total_run_df <- total_run_df %>%
  mutate(region = case_when(
    region == "SEAK"          ~ "SEAK",
    region == "Transboundary" ~ "T",       
    region == "N BC"          ~ "NBC",
    region == "S BC"          ~ "SBC",
    region == "WCVI"          ~ "WCVI",
    region == "Fraser"        ~ "SBC",      
    region == "Puget Sound"   ~ "PS",
    region == "WA Coast"      ~ "OP",
    region == "OR Coast"      ~ "OC",
    region == "Columbia R."   ~ "COL",
    TRUE ~ region
  ))

goal_priority <- c("PSC-Agreed Goal",
                   "ESA Recovery Goal",
                   "Agency Goal",
                   "Lower Goal",
                   "Upper Goal")

esc_goals_selected <- esc_goals %>%
  filter(goal_type %in% goal_priority, !is.na(escapement_goal)) %>%
  mutate(goal_rank = match(goal_type, goal_priority)) %>%
  group_by(year, population) %>%
  slice_min(goal_rank, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-goal_rank)

## Flag when escapement is under the goal and ER is high =====
joined_df <- total_run_df %>%
  left_join(esc_goals_selected, by = c("year", "population")) %>%
  dplyr::mutate(esc_tot = as.numeric(esc_tot)) %>%
  group_by(population) %>%
  arrange(year, .by_group = TRUE) %>%
  dplyr::mutate(
    # ER benchmark for this stock, and whether year t is above that benchmark
    mean_er      = mean(ocean_er, na.rm = TRUE),
    above_avg_er = ocean_er > mean_er,
    
    # Does escapement in year t fail to meet 100% of the escapement goal?
    under_goal_same_year = case_when(
      esc_tot < escapement_goal ~ "Under Esc. Goal",
      TRUE ~ "NA"
    ),
    
    # Does escapement in year t fail to meet 85% of the escapement goal?
    PSC_85_Goal_same_year = case_when(
      esc_tot < (0.85 * escapement_goal) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Flagged year: ER(t) above average AND escapement(t) below 100% of goal
    vulnerable_er = case_when(
      above_avg_er == TRUE & under_goal_same_year == "Under Esc. Goal" ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Flagged year: ER(t) above average AND escapement(t) below 85% of goal
    PSC_85_Goal = case_when(
      above_avg_er == TRUE & PSC_85_Goal_same_year == TRUE ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  ungroup()

 
# Plots and Tables for Mean benchmarks ======

populations <- unique(joined_df$population)

## Figure S1  =========
plots <- lapply(populations, function(pop) {
  
  df_pop <- joined_df %>% filter(population == pop)
  
  ggplot(df_pop, aes(x = year, y = esc_tot)) +
    
    # Background shading scaled continuously to ER rate
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                  ymin = -Inf, ymax = Inf, fill = ocean_er),
              alpha = 0.8, inherit.aes = FALSE) +
    
    # Escapement bars
    geom_col(alpha = 0.85) +
    
    geom_point(data = df_pop %>% filter(PSC_85_Goal == TRUE),
               aes(x = year-0.15, y = esc_tot),
               color = "#2980b9", size = 1.5, shape = 17, inherit.aes = FALSE) +
    
    geom_line(data = esc_goals_all %>% filter(population == pop, LineType == "Solid"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "solid", show.legend = FALSE) +
    
    geom_line(data = esc_goals_all %>% filter(population == pop, LineType == "Dotted"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "dotted", show.legend = FALSE) +
    
    # Red points for vulnerable years (t = above-avg ER; flag based on t-1-to-t+1 avg escapement)
    geom_point(data = df_pop %>% filter(vulnerable_er == TRUE),
               aes(x = year + 0.15, y = esc_tot),
               color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
    
    scale_fill_gradient2(
      low      = "#fff5f5",
      mid      = "#fff5f5",
      high     = "#c0392b",
      midpoint =  mean(as.numeric(df_pop$ocean_er), na.rm = TRUE),
      name     = "Ocean Exploitation Rate"
    ) +
    guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5, label.theme = element_text(size = 6))) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0), limits = c(2000,2023)) +
    
    # Mean ER label, upper right corner
    annotate("text",
             x = Inf, y = Inf,
             label = paste0("Mean ER: ", round(unique(df_pop$mean_er), 2)),
             hjust = 1.1, vjust = 1.5,
             size = 3.2, fontface = "bold", color = "grey20") +
    
    labs(
      title = paste("Chinook Escapement vs. Goal:", pop),
      caption = "Horizontal line = Escapement Goal (Solid = PSC-Agreed Goal, Dotted = Agency Goal)\nRed Points = Year t had Above-Average Ocean ER and escapement below goal\nBlue Triangles = Year t had Above-Average ER and escapement below 85% of goal",
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
  
})
### Save =========
# Save each plot to its own page in a single PDF
pdf("output/plots/Figure_S1_AllChinook_Coastwide_escapement_by_population.pdf", width = 10, height = 7)
for (p in plots) {
  print(p)
}
dev.off()

## Figure 3 ========
fig_pops <- c("Unuk", "Lower Shuswap", "Queets Fall", "Siuslaw Fall")  # was "Queets SprSum" -- see note above

 
# One mean-ER label per facet, placed in the upper right of each panel
mean_er_labels_fig3 <- df_pop %>%
  distinct(population, mean_er) %>%
  mutate(label = paste0("Mean ER: ", round(mean_er, 2)))

plots_fig3 <- lapply(fig_pops, function(pop) {
  
  df_pop <- joined_df %>% filter(population == pop,
                                 !year == 2024)
  esc_goals_fig <- esc_goals_all %>% filter(population == pop,
                                            !year == 2024)
  
  mean_er <- mean(as.numeric(df_pop$ocean_er), na.rm = TRUE)
  
  ggplot(df_pop, aes(x = year, y = esc_tot)) +
    
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                  ymin = -Inf, ymax = Inf, fill = ocean_er),
              alpha = 0.8, inherit.aes = FALSE) +
    
    geom_col(alpha = 0.85) +
    
    geom_point(data = df_pop %>% filter(PSC_85_Goal == TRUE),
               aes(x = year - 0.15, y = esc_tot),
               color = "#2980b9", size = 2, shape = 17, inherit.aes = FALSE) +
    
    geom_line(data = esc_goals_fig %>% filter(LineType == "Solid"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "solid",
              show.legend = FALSE) +
    
    geom_line(data = esc_goals_fig %>% filter(LineType == "Dotted"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "dotted",
              show.legend = FALSE) +
    
    geom_point(data = df_pop %>% filter(vulnerable_er == TRUE),
               aes(x = year + 0.15, y = esc_tot),
               color = "#c0392b", size = 2, inherit.aes = FALSE) +
    
    annotate("text", x = Inf, y = Inf,
             label = paste0("Mean ER: ", scales::percent(mean_er, accuracy = 1)),
             hjust = 1.1, vjust = 1.5, size = 3.2, fontface = "bold") +
    
    scale_fill_gradient2(
      low      = "#fff5f5",
      mid      = "#fff5f5",
      high     = "#c0392b",
      midpoint = mean(as.numeric(df_pop$ocean_er), na.rm = TRUE),
      name     = "Ocean Exploitation Rate"
    ) +
    guides(fill = guide_colorbar(
      barwidth       = unit(3.5, "cm"),
      barheight      = unit(0.35, "cm"),
      title.position = "top",
      title.hjust    = 0,
      label.theme    = element_text(size = 8)
    )) +
    
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(2000, 2023, by = 5)) +
    
    labs(title = pop, x = "Year", y = "Escapement") +
    
    theme_minimal() +
    theme(
      legend.position       = "top",
      legend.justification  = "right",
      panel.grid.minor      = element_blank()
    )
})
names(plots_fig3) <- fig_pops

Figure3 <- wrap_plots(
  plots_fig3[["Unuk"]],
  plots_fig3[["Lower Shuswap"]],
  plots_fig3[["Queets Fall"]],
  plots_fig3[["Siuslaw Fall"]],
  ncol = 2
) + plot_layout(guides = "keep")

Figure3

png("output/plots/Figure_3_Paper_Chinook_Coastwide_escapement_by_population.png",
    width = 1500, height = 1500, res = 150)
print(Figure3)   
dev.off()

## Summary Tables ====
### Table 1: Stock-level summary =============
stock_below_goal <- joined_df %>%
  filter(under_goal_same_year == "Under Esc. Goal", above_avg_er == TRUE) %>%
  group_by(population, region) %>%
  summarise(
    mean_er_threshold          = round(unique(mean_er), 3),
    n_years_below_goal_high_er = n(),
    years_below_goal_high_er   = paste(sort(year), collapse = ", "),
    mean_er_when_below         = round(mean(ocean_er, na.rm = TRUE), 3),
    .groups = "drop"
  )

stock_below_psc85 <- joined_df %>%
  filter(PSC_85_Goal == TRUE, above_avg_er == TRUE) %>%
  group_by(population, region) %>%
  summarise(
    mean_er_threshold           = round(unique(mean_er), 3),
    n_years_below_psc85_high_er = n(),
    years_below_psc85_high_er   = paste(sort(year), collapse = ", "),
    mean_er_when_below_psc85    = round(mean(ocean_er, na.rm = TRUE), 3),
    .groups = "drop"
  )

stock_summary_table <- joined_df %>%
  distinct(population, region, mean_er) %>%
  mutate(mean_er = round(mean_er, 3)) %>%
  left_join(stock_below_goal,  by = c("population", "region")) %>%
  left_join(stock_below_psc85, by = c("population", "region")) %>%
  dplyr::select(-mean_er_threshold.x, -mean_er_threshold.y) %>%
  dplyr::mutate(region = factor(region, levels = c((region_order_n_to_s)))) %>%
  arrange(region, population) %>%
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0)))

print(stock_summary_table)
write_csv(stock_summary_table, "output/tables/Table_S2_stock_below_goal_high_er.csv")

### Table 4: Long format =========
period_totals_long <- joined_df %>%
  filter(year >= 2009) %>%
  mutate(
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019                ~ "2019-2023"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    below_goal_n_years      = sum(
      under_goal_same_year == "Under Esc. Goal" & above_avg_er == TRUE,
      na.rm = TRUE),
    below_goal_n_pops       = n_distinct(
      population[under_goal_same_year == "Under Esc. Goal" & above_avg_er == TRUE]),
    below_goal_pct          = round(below_goal_n_years / n() * 100, 1),
    below_goal_populations  = paste(
      sort(unique(population[under_goal_same_year == "Under Esc. Goal" & above_avg_er == TRUE])),
      collapse = ", "),
    below_psc85_n_years     = sum(
      PSC_85_Goal == TRUE & above_avg_er == TRUE,
      na.rm = TRUE),
    below_psc85_n_pops      = n_distinct(
      population[PSC_85_Goal == TRUE & above_avg_er == TRUE]),
    below_psc85_pct         = round(below_psc85_n_years / n() * 100, 1),
    below_psc85_populations = paste(
      sort(unique(population[PSC_85_Goal == TRUE & above_avg_er == TRUE])),
      collapse = ", "),
    total_population_years  = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = -period,
    names_to  = "metric",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  mutate(
    category = case_when(
      str_detect(metric, "below_goal")  ~ "Below Escapement Goal & Above Mean ER, Same Year",
      str_detect(metric, "below_psc85") ~ "Below PSC 85% Goal & Above Mean ER, Same Year",
      TRUE                              ~ "Overall"
    ),
    metric = case_when(
      str_detect(metric, "n_years")      ~ "N population-years",
      str_detect(metric, "n_pops")       ~ "N unique populations",
      str_detect(metric, "pct")          ~ "% of population-years",
      str_detect(metric, "populations")  ~ "Populations",
      metric == "total_population_years" ~ "Total population-years",
      TRUE                               ~ metric
    )
  ) %>%
  dplyr::select(period, category, metric, value) %>%
  arrange(period, category, metric)

print(period_totals_long)
write_csv(period_totals_long,
          "output/tables/period_totals_long.csv")

##  Figure 4 - Plot Comparing Regions and summary stats ===========
# Step 1: stock-level summary by period (group by population)
escapement_summary <- joined_df %>%
  filter(year >= 2009) %>%
  mutate(
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019                ~ "2019-2023"
    )
  ) %>%
  group_by(period, region) %>%
  summarise(
    total_years                = n(),
    n_below_goal                = sum(under_goal_same_year == "Under Esc. Goal", na.rm = TRUE),
    n_below_goal_above_avg_er   = sum(vulnerable_er == TRUE, na.rm = TRUE),
    pct_below_goal               = round(n_below_goal / total_years * 100, 1),
    pct_below_goal_above_avg_er  = round(n_below_goal_above_avg_er / total_years * 100, 1),
    mean_er_period               = round(mean(ocean_er, na.rm = TRUE), 3),
    .groups = "drop"
  )

# lollipop plot using % of population-years
p_pct_years <- escapement_summary %>%
  pivot_longer(
    cols      = c(pct_below_goal, pct_below_goal_above_avg_er),
    names_to  = "metric",
    values_to = "pct"
  ) %>%
  mutate(
    metric = case_when(
      metric == "pct_below_goal"              ~ "% population-years below EG" ,
      metric == "pct_below_goal_above_avg_er" ~ "% population-years below EG &\nabove mean ER"
    ),
    metric = factor(metric, levels = c(
      "% population-years below EG &\nabove mean ER",
      "% population-years below EG"
    )),
    pct    = as.numeric(pct),
    # North at top, South at bottom
    region = factor(region, levels = rev(region_order_n_to_s))
  ) %>%
  filter(!is.na(region)) %>%
  ggplot(aes(y = region, x = pct, color = metric)) +
  
  # lollipop stems
  geom_linerange(
    aes(xmin = 0, xmax = pct),
    position = position_dodge(width = 0.5),
    linewidth = 0.8
  ) +
  
  # lollipop points
  geom_point(
    position = position_dodge(width = 0.5),
    size = 3.5
  ) +
  
  facet_wrap(~period, ncol = 2) +
  
  scale_color_manual(
    values = c(
      "% population-years below EG"                            = "#6A9BA8",
      "% population-years below EG &\nabove mean ER" = "#c0392b"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  labs(
    x = "% of population-years",
    y = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title         = element_text(face = "bold", size = 18),
    plot.subtitle      = element_text(size = 18, color = "grey40"),
    panel.grid.minor   = element_blank(),
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    panel.grid.major.y = element_blank(),
    strip.text         = element_text(face = "bold", size = 18),
    legend.position    = "bottom"
  )

p_pct_years
ggsave("output/plots/Fig4_pct_popyr_below_escgoal_by_region_period_lollipop.png",
       p_pct_years, width = 11, height = 6)
