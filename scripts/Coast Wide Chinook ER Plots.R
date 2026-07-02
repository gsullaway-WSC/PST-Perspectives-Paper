# Exploitation Rates for just ocean fisheries for all stocks. 
# Plot AABM harvest versus total stock escapements 

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
esc_goals<-read_csv("data/Escapement_Goals_Use.csv")   %>%
  gather(c(5:10), key = "goal_type", value = "escapement_goal") %>%
  dplyr::select(-Note) %>%
  dplyr::mutate(population = case_when(StockName == "Siuslaw R."  ~ "Siuslaw Fall",
                                       StockName == "Siletz Fall" ~ "Siletz Fall",
                                       StockName == "Nehalem R."  ~ "Nehalem Fall",
                                       StockName == "South_umpqua" ~ "South Umpqua", 
                                       StockName == "Unuk River" ~ "Unuk", 
                                       StockName == "Atnarko Wild" ~ "Atnarko",
                                       TRUE ~ StockName))
  
# Load data =====
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx")

## Look at ER Limits that are stock specific.===========
# CYER data taken from their CTC ER 2025 
cyer <- read_csv("data/CYER_Limits_Only.csv") %>%
  filter(!is.na(`CYER_Limit_2019-2028`)) %>%
  group_by(Population) %>%
  summarise(`CYER_Limit_2019-2028` =sum(`CYER_Limit_2019-2028`)) %>% 
  rename(population = "Population")
 

# Tidy DFs =======
total_run_df <- data %>%
  filter(!population %in% c("Elk", "Cowlitz.fa", "Elochoman", "Grays", "Middle_Shuswap", "South_Thompson", 
                            "Green","Hoh_sp", "Grays_Harbor_sp",
                            "Nisqually", "Tahsish_fa","Bedwell_fa", "Megin_fa","Moyeha_fa",
                             "Puntledge_su","Colonial_fa","Artlish_fa","Kaouk_fa"#, "Kitsumkalum"
                            )) %>% 
  # convert ER to decimals
  dplyr::mutate(term_tot = as.numeric(term_tot)/100,
                aabm_tot = as.numeric(aabm_tot)/100,
                er = as.numeric(er),
                ocean_er = er-term_tot) %>%
  dplyr::select(year, population, region, total_run, ocean_er,esc_tot) %>% #, esc_tot,aabm_tot,term_tot,er) %>% 
  dplyr::mutate(total_run = as.numeric(total_run),
                population = case_when(population == "Siuslaw_fa" ~ "Siuslaw Fall",
                                       population == "Siletz_fa" ~ "Siletz Fall",
                                       population == "Nehalem_fa" ~ "Nehalem Fall",
                                       population == "South_umpqua" ~ "South Umpqua", 
                                       # population == "Atnarko"      ~ "Atnarko Wild",
                                       population == "Lower_Shuswap"~ "Lower Shuswap",
                                       population == "Skagit_sp"    ~ "Skagit Spr",
                                       population == "Skagit_fa"    ~ "Skagit SumFall",
                                       population == "Snohomish_fa" ~ "Snohomish",
                                       population == "Quillayute_fa"~ "Quillayute Fall",
                                       population == "Hoh_sp"       ~ "Hoh Spr Sum",
                                       population == "Hoh_fa"       ~ "Hoh Fall",
                                       population == "Queets_sp"    ~ "Queets SprSum",
                                       population == "Queets_fa"    ~ "Queets Fall",
                                       population == "Grays_Harbor_sp"  ~ "Grays Harbor Spr",
                                       population == "Grays_Harbor_fa"  ~ "Grays Harbor Fall",
                                       population == "Hanford_br_w"     ~ "Columbia Upriver Brights",
                                       population == "Nehalem_fa"       ~ "Nehalem R.",
                                       population == "Siletz_fa"        ~ "Siletz Fall",
                                       population == "Siuslaw_fa"       ~ "Siuslaw R.",
                                       population == "Cowlitz.fa"       ~ "Cowlitz Fall", 
                                        TRUE ~ population))
                        
  
                        # unique(total_run_df$population)
                        
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

joined_df <- total_run_df %>%
  left_join(esc_goals_selected, by = c("year", "population")) %>%
  dplyr::mutate(esc_tot = as.numeric(esc_tot),
                under_goal = case_when(esc_tot < escapement_goal ~ "Under Esc. Goal",
                                       TRUE ~ "NA")) %>% 
  group_by(population) %>%
  dplyr::mutate( 
         mean_er = mean(ocean_er, na.rm = TRUE),
         above_avg_er = ocean_er > mean_er,
         # Vulnerable = below esc goal AND above avg AABM catch
         vulnerable_er = case_when(under_goal == "Under Esc. Goal" & above_avg_er == TRUE ~ TRUE,
                                   TRUE ~ FALSE),
         # Flag years where escapement reached 85% of the goal
         PSC_85_Goal = case_when(esc_tot < (0.85 * escapement_goal) ~ TRUE,
                                 TRUE ~ FALSE)) %>%
  ungroup() %>%
  filter(!year<2000)#, !year ==2023)
 
# All goals for plotting
esc_goals_all <- esc_goals %>%
  filter(!is.na(escapement_goal)) %>%
  filter(!year <2000)

# Plots and Tables for Mean benchmarks ======

populations <- unique(joined_df$population)

## All populations =========
plots <- lapply(populations, function(pop) {
  
  df_pop <- joined_df %>% filter(population == pop)
  
  ggplot(df_pop, aes(x = year, y = esc_tot)) +

    # Background shading scaled continuously to ER rate
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                  ymin = -Inf, ymax = Inf, fill = ocean_er),
              alpha = 0.8, inherit.aes = FALSE) +
    
    # Escapement bars
    geom_col(alpha = 0.85) +
    
    # Escapement goal dashed line
    # geom_path(aes(y = escapement_goal), color = "black", linetype = 2) +
    # geom_path(aes(y = escapement_goal, linetype = LineType), color = "black",show.legend = FALSE) +
    # 
    # scale_linetype_manual(
    #   values = c("Dotted" = "dotted", "Solid" = "solid", "Dashed" = "dashed"),
    #   name = "Goal Type"
    # ) +
    # geom_path(data = esc_goals_all %>% filter(population == pop),
    #           aes(y = escapement_goal, linetype = LineType,
    #               group = goal_type),
    #           color = "black", show.legend = FALSE) +
    # 
    # scale_linetype_manual(
    #   values = c("Dotted" = "dotted", "Solid" = "solid", "Dashed" = "dashed")
    # ) +
    geom_point(data = df_pop %>% filter(PSC_85_Goal == TRUE),
               aes(x = year-0.15, y = esc_tot),
               color = "#2980b9", size = 1.5, shape = 17, inherit.aes = FALSE) +

    geom_line(data = esc_goals_all %>% filter(population == pop, LineType == "Solid"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "solid", show.legend = FALSE) +
     
    geom_line(data = esc_goals_all %>% filter(population == pop, LineType == "Dotted"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "dotted", show.legend = FALSE) +
    # 
    # Red points for vulnerable years
    geom_point(data = df_pop %>% filter(vulnerable_er == TRUE),
               aes(x = year + 0.15, y = esc_tot),
               color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
    
    scale_fill_gradient2(
      low      = "#fff5f5",
      mid      = "#fff5f5",#"#f9c9c9", #"#f4a582",
      high     = "#c0392b",
      midpoint =  mean(as.numeric(df_pop$ocean_er), na.rm = TRUE),
      name     = "Mixed Stock Exploitation Rate"
    ) +
    
    scale_y_continuous(expand = c(0,0)) +
    # scale_x_continuous(breaks = seq(1980, 2020, by = 10),expand = c(0,0)) +
    
    labs(
      title = paste("Chinook Escapement vs. Goal:", pop),
      caption = "Horizontal line = Escapement Goal (Solid = PSC -Agreed Goal, Dotted = Agency Goal\n Red Points = Above Average Exploitation Rate & Below Average Escapement, Blue Circles = Stock not meeting 85% of Escapement Goal",
  
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

# Save each plot to its own page in a single PDF
pdf("output/plots/Supplement_AllChinook_Coastwide_escapement_by_population.pdf", width = 10, height = 7)
for (p in plots) {
  print(p)
}
dev.off()

## Figure 3 with facet - Mean benchmark, pull out 4 Rivers for the PST Manuscript ========
fig_pops <- c("Unuk", "Lower Shuswap", "Queets SprSum", "Siuslaw Fall")

  df_pop <- joined_df %>% filter(population %in% fig_pops)
  esc_goals_fig <- esc_goals_all %>% filter(population %in% fig_pops)
  
  Fig3 <- ggplot(df_pop, aes(x = year, y = esc_tot)) +
    
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                  ymin = -Inf, ymax = Inf, fill = ocean_er),
              alpha = 0.8, inherit.aes = FALSE) +
    
    geom_col(alpha = 0.85) +
    
    geom_point(data = df_pop %>% filter(PSC_85_Goal == TRUE),
               aes(x = year - 0.15, y = esc_tot),
               color = "#2980b9", size = 2, shape = 17, inherit.aes = FALSE) +
    
    geom_line(data = esc_goals_fig %>% filter(LineType == "Solid"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "solid",# linewidth = 1,
              show.legend = FALSE) +
    
    geom_line(data = esc_goals_fig %>% filter(LineType == "Dotted"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "dotted", #linewidth = 1,
              show.legend = FALSE) +
    
    geom_point(data = df_pop %>% filter(vulnerable_er == TRUE),
               aes(x = year + 0.15, y = esc_tot),
               color = "#c0392b", size = 2, inherit.aes = FALSE) +
    
    scale_fill_gradient2(
      low      = "#fff5f5",
      mid      = "#fff5f5",
      high     = "#c0392b",
      midpoint = mean(as.numeric(df_pop$ocean_er), na.rm = TRUE),
      name     = "Mixed Stock ER"
    ) +
    
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(2000, 2024, by = 5)) +
    
    labs(#title = pop,
         x = "Year", y = "Escapement") +
    facet_wrap(~population,scales = "free") + 
    theme_minimal( ) +  # increased from 16
    theme(
      legend.position    = "bottom",
      # legend.title       = element_text(size = 16),
      # legend.text        = element_text(size = 16),
      # strip.text         = element_text(face = "bold", size = 16),
      # plot.title         = element_text(face = "bold", hjust = 0.5, size = 18),
      # axis.text          = element_text(size = 14),
      # axis.title         = element_text(size = 16),
      panel.grid.minor   = element_blank()
    )
 
  Fig3
  ggsave("output/plots/Figure_3_Facet_Chinook_Coastwide_escapement_Pop.png",
         width = 9, height = 7)  
    
## Figure 3 - Mean benchmark, pull out 4 Rivers for the PST Manuscript ========
plots_fig2 <- lapply(fig_pops, function(pop) {
  
  df_pop <- joined_df %>% filter(population == pop, 
                                 !year ==2023)
  esc_goals_fig <- esc_goals_all %>% filter(population == pop,
                                            !year ==2023)
  
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
              color = "black", linetype = "solid",# linewidth = 1,
              show.legend = FALSE) +
    
    geom_line(data = esc_goals_fig %>% filter(LineType == "Dotted"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "dotted", #linewidth = 1,
              show.legend = FALSE) +
    
    geom_point(data = df_pop %>% filter(vulnerable_er == TRUE),
               aes(x = year + 0.15, y = esc_tot),
               color = "#c0392b", size = 2, inherit.aes = FALSE) +
    
    scale_fill_gradient2(
      low      = "#fff5f5",
      mid      = "#fff5f5",
      high     = "#c0392b",
      midpoint = mean(as.numeric(df_pop$ocean_er), na.rm = TRUE),
      name     = "Mixed Stock\nER"
    ) +
    
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(2000, 2024, by = 5)) +
    
    labs(title = pop, x = "Year", y = "Escapement") +
    
   theme_minimal( ) +  # increased from 16
    theme(
      legend.position    = "bottom",
      # legend.title       = element_text(size = 16),
      # legend.text        = element_text(size = 10),
      # strip.text         = element_text(face = "bold", size = 16),
      # plot.title         = element_text(face = "bold", hjust = 0.5, size = 18),
      # axis.text          = element_text(size = 14),
      # axis.title         = element_text(size = 16),
      panel.grid.minor   = element_blank()
    )
})

names(plots_fig2) <- fig_pops

Figure2 <- wrap_plots(
  plots_fig2[["Unuk"]],
  plots_fig2[["Lower Shuswap"]],
  plots_fig2[["Queets SprSum"]],
  plots_fig2[["Siuslaw Fall"]],
  ncol = 2
) + plot_layout(guides = "keep")

Figure2
 
png("output/plots/Paper_Chinook_Coastwide_escapement_by_population.png", 
    width = 1500, height = 800, res = 150)
print(Figure2)   # print() needed for ggplot objects
dev.off()

## Tables ==== 
### Table 1: Stock-level summary =============
stock_below_goal <- joined_df %>%
  filter(under_goal == "Under Esc. Goal", above_avg_er == TRUE) %>%
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
  # remove duplicate mean_er threshold columns
  dplyr::select(-mean_er_threshold.x, -mean_er_threshold.y) %>%
  arrange(region, population) %>%
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0)))

print(stock_summary_table)
write_csv(stock_summary_table, "output/tables/stock_below_goal_high_er.csv")

### Table 2: Period-level totals ==============
period_totals <- joined_df %>%
  filter(year >= 2009) %>%
  mutate(
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019                ~ "2019-Present"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    n_below_goal_high_er = sum(
      under_goal == "Under Esc. Goal" & above_avg_er == TRUE,
      na.rm = TRUE),
    n_below_psc85_high_er = sum(
      PSC_85_Goal == TRUE & above_avg_er == TRUE,
      na.rm = TRUE),
    total_population_years  = n(),
    pct_below_goal_high_er  = round(
      n_below_goal_high_er / total_population_years * 100, 1),
    pct_below_psc85_high_er = round(
      n_below_psc85_high_er / total_population_years * 100, 1),
    .groups = "drop"
  )

print(period_totals)
write_csv(period_totals, "output/tables/period_totals_below_goal_high_er.csv")

### Table 3: Region x Period breakdown =========
region_period_totals <- joined_df %>%
  filter(year >= 2009) %>%
  mutate(
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019                ~ "2019-Present"
    )
  ) %>%
  group_by(region, period) %>%
  summarise(
    n_below_goal_high_er    = sum(
      under_goal == "Under Esc. Goal" & above_avg_er == TRUE,
      na.rm = TRUE),
    n_below_psc85_high_er   = sum(
      PSC_85_Goal == TRUE & above_avg_er == TRUE,
      na.rm = TRUE),
    total_population_years  = n(),
    pct_below_goal_high_er  = round(
      n_below_goal_high_er / total_population_years * 100, 1),
    pct_below_psc85_high_er = round(
      n_below_psc85_high_er / total_population_years * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(region, period)

print(region_period_totals)
write_csv(region_period_totals,
          "output/tables/region_period_totals_below_goal_high_er.csv")

### Table 3: Period-level totals with population lists =========
period_totals_with_pops <- joined_df %>%
  filter(year >= 2009) %>%
  mutate(
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019                ~ "2019-Present"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    n_below_goal_high_er = sum(
      under_goal == "Under Esc. Goal" & above_avg_er == TRUE,
      na.rm = TRUE),
    pops_below_goal_high_er = paste(
      sort(unique(population[under_goal == "Under Esc. Goal" & above_avg_er == TRUE])),
      collapse = ", "),
    n_pops_below_goal_high_er = n_distinct(
      population[under_goal == "Under Esc. Goal" & above_avg_er == TRUE]),
    n_below_psc85_high_er = sum(
      PSC_85_Goal == TRUE & above_avg_er == TRUE,
      na.rm = TRUE),
    pops_below_psc85_high_er = paste(
      sort(unique(population[PSC_85_Goal == TRUE & above_avg_er == TRUE])),
      collapse = ", "),
    n_pops_below_psc85_high_er = n_distinct(
      population[PSC_85_Goal == TRUE & above_avg_er == TRUE]),
    total_population_years  = n(),
    pct_below_goal_high_er  = round(
      n_below_goal_high_er / total_population_years * 100, 1),
    pct_below_psc85_high_er = round(
      n_below_psc85_high_er / total_population_years * 100, 1),
    .groups = "drop"
  ) %>%
  dplyr::select(
    period, total_population_years,
    n_below_goal_high_er, pct_below_goal_high_er,
    n_pops_below_goal_high_er, pops_below_goal_high_er,
    n_below_psc85_high_er, pct_below_psc85_high_er,
    n_pops_below_psc85_high_er, pops_below_psc85_high_er
  )

print(period_totals_with_pops)
write_csv(period_totals_with_pops,
          "output/tables/period_totals_with_populations.csv")

### Table 4: Long format =========
period_totals_long <- joined_df %>%
  filter(year >= 2009) %>%
  mutate(
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019                ~ "2019-Present"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    below_goal_n_years      = sum(
      under_goal == "Under Esc. Goal" & above_avg_er == TRUE,
      na.rm = TRUE),
    below_goal_n_pops       = n_distinct(
      population[under_goal == "Under Esc. Goal" & above_avg_er == TRUE]),
    below_goal_pct          = round(below_goal_n_years / n() * 100, 1),
    below_goal_populations  = paste(
      sort(unique(population[under_goal == "Under Esc. Goal" & above_avg_er == TRUE])),
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
      str_detect(metric, "below_goal")  ~ "Below Escapement Goal & Above Mean ER",
      str_detect(metric, "below_psc85") ~ "Below PSC 85% Goal & Above Mean ER",
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
      year >= 2019                ~ "2019-Present"
    )
  ) %>%
  group_by(period, region) %>%
  summarise(
    total_years                  = n(),
    n_below_psc85                = sum(PSC_85_Goal == TRUE, na.rm = TRUE),
    n_below_psc85_above_avg_er   = sum(PSC_85_Goal == TRUE & above_avg_er == TRUE, na.rm = TRUE),
    pct_below_psc85              = round(n_below_psc85 / total_years * 100, 1),
    pct_below_psc85_above_avg_er = round(n_below_psc85_above_avg_er / total_years * 100, 1),
    mean_er_period               = round(mean(ocean_er, na.rm = TRUE), 3),
    .groups = "drop"
  )

# lollipop plot using % of population-years
p_pct_years <- escapement_summary %>%
  pivot_longer(
    cols      = c(pct_below_psc85, pct_below_psc85_above_avg_er),
    names_to  = "metric",
    values_to = "pct"
  ) %>%
  mutate(
    metric = case_when(
      metric == "pct_below_psc85"              ~ "% population-years below PSC 85% goal",
      metric == "pct_below_psc85_above_avg_er" ~ "% population-years below PSC 85% goal & above mean ER"
    ),
    metric = factor(metric, levels = c(
      "% population-years below PSC 85% goal & above mean ER",
      "% population-years below PSC 85% goal"
    )),
    pct    = as.numeric(pct),
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
      "% population-years below PSC 85% goal"                         = "#6A9BA8",
      "% population-years below PSC 85% goal & above mean ER"         = "#c0392b"
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
  # theme_minimal(base_family = "dm_sans") +
  theme(
    plot.title         = element_text(face = "bold", size = 18),
    plot.subtitle      = element_text(size =18, color = "grey40"),
    panel.grid.minor   = element_blank(),
    legend.text = element_text(size =18),
    axis.text = element_text(size =18),
    axis.title = element_text(size =18),
    panel.grid.major.y = element_blank(),
    strip.text         = element_text(face = "bold", size = 18),
    legend.position    = "bottom"
  )

p_pct_years
ggsave("output/plots/pct_popyr_below_psc85_by_region_period_lollipop.png",
       p_pct_years, width = 6, height = 5)

# Plots and Tables based on 40% ER ==============

## 40% ER Benchmark - Main Figure 3 - 4 selected populations ========
fig_pops <- c("Unuk", "Lower Shuswap", "Queets SprSum", "Siuslaw Fall")

plots_fig2_updated <- lapply(fig_pops, function(pop) {
  
  df_pop       <- joined_df %>% filter(population == pop)
  esc_goals_fig <- esc_goals_all %>% filter(population == pop)
  
  ggplot(df_pop, aes(x = year, y = esc_tot)) +
    
    # Background shading
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                  ymin = -Inf, ymax = Inf, fill = ocean_er),
              alpha = 0.8, inherit.aes = FALSE) +
    
    # Escapement bars
    geom_col(alpha = 0.85) +
    
    # Goal lines
    geom_line(data = esc_goals_fig %>% filter(LineType == "Solid"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "solid", show.legend = FALSE) +
    
    geom_line(data = esc_goals_fig %>% filter(LineType == "Dotted"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "dotted", show.legend = FALSE) +
    
    # RED circle: below escapement goal AND ER > 40%
    geom_point(data = df_pop %>% filter(under_goal == "Under Esc. Goal" & ocean_er > 0.40),
               aes(x = year + 0.15, y = esc_tot),
               color = "#c0392b", fill = "#c0392b",
               size = 2, shape = 21, inherit.aes = FALSE) +
    
    # BLUE triangle: below PSC 85% goal AND ER > 40%
    geom_point(data = df_pop %>% filter(PSC_85_Goal == TRUE & ocean_er > 0.40),
               aes(x = year - 0.15, y = esc_tot),
               color = "#2980b9", fill = "#2980b9",
               size = 2, shape = 24, inherit.aes = FALSE) +
    
    scale_fill_gradient2(
      low      = "#fff5f5",
      mid      = "#fff5f5",
      high     = "#c0392b",
      midpoint = mean(as.numeric(df_pop$ocean_er), na.rm = TRUE),
      name     = "Mixed Stock ER"
    ) +
    
    scale_y_continuous(expand = c(0, 0)) +
    
    labs(title = pop, x = "Year", y = "Escapement") +
    theme_minimal(base_size = 20)
  # theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom"
    #   strip.text       = element_text(face = "bold", size = 10),
    #   plot.title       = element_text(face = "bold", hjust = 0.5, size = 12),
    #   panel.grid.minor = element_blank()
  )
})

names(plots_fig2_updated) <- fig_pops

# add shared legend using a dummy plot
legend_plot <- ggplot() +
  geom_point(aes(x = 1, y = 1, color = "Below Esc. Goal & ER > 40%"), 
             shape = 21, size = 3, fill = "#c0392b") +
  geom_point(aes(x = 1, y = 2, color = "Below PSC 85% Goal & ER > 40%"), 
             shape = 24, size = 3, fill = "#2980b9") +
  scale_color_manual(
    name   = NULL,
    values = c("Below Esc. Goal & ER > 40%"     = "#c0392b",
               "Below PSC 85% Goal & ER > 40%"  = "#2980b9")
  ) +
  theme_void() +
  theme(legend.position = "bottom")

Figure2_updated <- wrap_plots(
  plots_fig2_updated[["Unuk"]],
  plots_fig2_updated[["Lower Shuswap"]],
  plots_fig2_updated[["Queets SprSum"]],
  plots_fig2_updated[["Siuslaw Fall"]],
  ncol = 2
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

Figure2_updated

ggsave("output/plots/Paper_Chinook_Coastwide_escapement_updated.png",
       Figure2_updated, width = 12, height = 9)


# CYER is not greater than 40%, except for Cowichan, so keep baseline of 40% ...  


populations <- unique(joined_df$population)

plots_updated <- lapply(populations, function(pop) {
  
  df_pop <- joined_df %>% filter(population == pop)
  
  ggplot(df_pop, aes(x = year, y = esc_tot)) +
    
    # Background shading scaled continuously to ER rate
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                  ymin = -Inf, ymax = Inf, fill = ocean_er),
              alpha = 0.8, inherit.aes = FALSE) +
    
    # Escapement bars
    geom_col(alpha = 0.85) +
    
    # Goal lines
    geom_line(data = esc_goals_all %>% filter(population == pop, LineType == "Solid"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "solid", show.legend = FALSE) +
    
    geom_line(data = esc_goals_all %>% filter(population == pop, LineType == "Dotted"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "dotted", show.legend = FALSE) +
    
    # RED circle: below escapement goal AND ER > 40%
    geom_point(data = df_pop %>% filter(under_goal == "Under Esc. Goal" & ocean_er > 0.40),
               aes(x = year + 0.15, y = esc_tot),
               color = "#c0392b", fill = "#c0392b",
               size = 2, shape = 21, inherit.aes = FALSE) +
    
    # BLUE triangle: below PSC 85% goal AND ER > 40%
    geom_point(data = df_pop %>% filter(PSC_85_Goal == TRUE & ocean_er > 0.40),
               aes(x = year - 0.15, y = esc_tot),
               color = "#2980b9", fill = "#2980b9",
               size = 2, shape = 24, inherit.aes = FALSE) +
    
    scale_fill_gradient2(
      low      = "#fff5f5",
      mid      = "#fff5f5",
      high     = "#c0392b",
      midpoint = mean(as.numeric(df_pop$ocean_er), na.rm = TRUE),
      name     = "Mixed Stock ER"
    ) +
    
    scale_y_continuous(expand = c(0, 0)) +
    
    labs(
      title   = paste("Chinook Escapement vs. Goal:", pop),
      caption = "Background shading = ocean exploitation rate (darker red = higher ER)\nGoal lines: Solid = PSC-Agreed Goal, Dotted = Agency Goal\nRed circle = below escapement goal & ER > 40%\nBlue triangle = below PSC 85% goal & ER > 40%",
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

# Save supplement PDF
pdf("output/plots/Supplement_AllChinook_Coastwide_escapement_updated.pdf", 
    width = 10, height = 7)
for (p in plots_updated) {
  print(p)
}
dev.off()
 
## Summary statistics ===========
 
 
## Tables ==========
### Table 1: Stock-level summary =============
# years each stock fell below escapement goal AND had ER > 40%
stock_below_goal <- joined_df %>%
  filter(under_goal == "Under Esc. Goal", ocean_er > 0.40) %>%
  group_by(population, region) %>%
  summarise(
    n_years_below_goal_high_er = n(),
    years_below_goal_high_er   = paste(sort(year), collapse = ", "),
    mean_er_when_below          = round(mean(ocean_er, na.rm = TRUE), 3),
    .groups = "drop"
  )

# years each stock fell below PSC 85% goal AND had ER > 40%
stock_below_psc85 <- joined_df %>%
  filter(PSC_85_Goal == TRUE, ocean_er > 0.40) %>%
  group_by(population, region) %>%
  summarise(
    n_years_below_psc85_high_er = n(),
    years_below_psc85_high_er   = paste(sort(year), collapse = ", "),
    mean_er_when_below_psc85    = round(mean(ocean_er, na.rm = TRUE), 3),
    .groups = "drop"
  )

# join into one table
stock_summary_table <- joined_df %>%
  distinct(population, region) %>%
  left_join(stock_below_goal, by = c("population", "region")) %>%
  left_join(stock_below_psc85, by = c("population", "region")) %>%
  arrange(region, population) %>%
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0)))

print(stock_summary_table)

# save as CSV
write_csv(stock_summary_table, "output/tables/stock_below_goal_high_er.csv")

### Table 2: Period-level totals ==============
period_totals <- joined_df %>%
  filter(year >= 2009) %>%
  mutate(
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019                ~ "2019-Present"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    # total population-years below escapement goal with ER > 40%
    n_below_goal_high_er = sum(under_goal == "Under Esc. Goal" & ocean_er > 0.40, 
                               na.rm = TRUE),
    # unique populations that ever fell below goal with ER > 40%
    # n_pops_below_goal_high_er = n_distinct(
    #   population[under_goal == "Under Esc. Goal" & ocean_er > 0.40]
    # ),
    # total population-years below PSC 85% goal with ER > 40%
    n_below_psc85_high_er = sum(PSC_85_Goal == TRUE & ocean_er > 0.40, 
                                na.rm = TRUE),
    # unique populations that ever fell below PSC 85% goal with ER > 40%
    # n_pops_below_psc85_high_er = n_distinct(
    #   population[PSC_85_Goal == TRUE & ocean_er > 0.40]
    # ),
    # total number of population-years in this period
    total_population_years = n(),
    # pct of population-years below goal with high ER
    pct_below_goal_high_er = round(
      n_below_goal_high_er / total_population_years * 100, 1
    ),
    pct_below_psc85_high_er = round(
      n_below_psc85_high_er / total_population_years * 100, 1
    ),
    .groups = "drop"
  )

print(period_totals)

write_csv(period_totals, "output/tables/period_totals_below_goal_high_er.csv")

# ── Table 3: Region x Period breakdown ───────────────────────────────────────
region_period_totals <- joined_df %>%
  filter(year >= 2009) %>%
  mutate(
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019                ~ "2019-Present"
    )
  ) %>%
  group_by(region, period) %>%
  summarise(
    n_below_goal_high_er    = sum(under_goal == "Under Esc. Goal" & ocean_er > 0.40, 
                                  na.rm = TRUE),
    n_below_psc85_high_er   = sum(PSC_85_Goal == TRUE & ocean_er > 0.40, 
                                  na.rm = TRUE),
    total_population_years  = n(),
    pct_below_goal_high_er  = round(
      n_below_goal_high_er / total_population_years * 100, 1
    ),
    pct_below_psc85_high_er = round(
      n_below_psc85_high_er / total_population_years * 100, 1
    ),
    .groups = "drop"
  ) %>%
  arrange(region, period)

print(region_period_totals)

write_csv(region_period_totals, 
          "output/tables/region_period_totals_below_goal_high_er.csv")
 
### Table 3 Period-level totals with population lists =========

period_totals_with_pops <- joined_df %>%
  filter(year >= 2009) %>%
  mutate(
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019                ~ "2019-Present"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    # below escapement goal with ER > 40%
    n_below_goal_high_er = sum(under_goal == "Under Esc. Goal" & ocean_er > 0.40,
                               na.rm = TRUE),
    pops_below_goal_high_er = paste(
      sort(unique(population[under_goal == "Under Esc. Goal" & ocean_er > 0.40])),
      collapse = ", "
    ),
    n_pops_below_goal_high_er = n_distinct(
      population[under_goal == "Under Esc. Goal" & ocean_er > 0.40]
    ),
    
    # below PSC 85% goal with ER > 40%
    n_below_psc85_high_er = sum(PSC_85_Goal == TRUE & ocean_er > 0.40,
                                na.rm = TRUE),
    pops_below_psc85_high_er = paste(
      sort(unique(population[PSC_85_Goal == TRUE & ocean_er > 0.40])),
      collapse = ", "
    ),
    n_pops_below_psc85_high_er = n_distinct(
      population[PSC_85_Goal == TRUE & ocean_er > 0.40]
    ),
    
    total_population_years  = n(),
    pct_below_goal_high_er  = round(
      n_below_goal_high_er / total_population_years * 100, 1
    ),
    pct_below_psc85_high_er = round(
      n_below_psc85_high_er / total_population_years * 100, 1
    ),
    .groups = "drop"
  ) %>%
  # reorder columns for readability
  dplyr::select(
    period,
    total_population_years,
    n_below_goal_high_er,
    pct_below_goal_high_er,
    n_pops_below_goal_high_er,
    pops_below_goal_high_er,
    n_below_psc85_high_er,
    pct_below_psc85_high_er,
    n_pops_below_psc85_high_er,
    pops_below_psc85_high_er
  )

print(period_totals_with_pops)

write_csv(period_totals_with_pops,
          "output/tables/period_totals_with_populations.csv")

### Table 4: Period-level totals with population lists ===========
period_totals_long <- joined_df %>%
  filter(year >= 2009) %>%
  mutate(
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019                ~ "2019-Present"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    # below escapement goal with ER > 40%
    below_goal_n_years       = sum(under_goal == "Under Esc. Goal" & ocean_er > 0.40, na.rm = TRUE),
    below_goal_n_pops        = n_distinct(population[under_goal == "Under Esc. Goal" & ocean_er > 0.40]),
    below_goal_pct           = round(below_goal_n_years / n() * 100, 1),
    below_goal_populations   = paste(sort(unique(population[under_goal == "Under Esc. Goal" & ocean_er > 0.40])), collapse = ", "),
    
    # below PSC 85% goal with ER > 40%
    below_psc85_n_years      = sum(PSC_85_Goal == TRUE & ocean_er > 0.40, na.rm = TRUE),
    below_psc85_n_pops       = n_distinct(population[PSC_85_Goal == TRUE & ocean_er > 0.40]),
    below_psc85_pct          = round(below_psc85_n_years / n() * 100, 1),
    below_psc85_populations  = paste(sort(unique(population[PSC_85_Goal == TRUE & ocean_er > 0.40])), collapse = ", "),
    
    total_population_years = n(),
    .groups = "drop"
  ) %>%
  # pivot to long format — one row per period x metric
  pivot_longer(
    cols      = -period,
    names_to  = "metric",
    values_to = "value",
    values_transform = list(value = as.character)  # convert all to character to allow mixing
  ) %>%
  mutate(
    category = case_when(
      str_detect(metric, "below_goal")  ~ "Below Escapement Goal & ER > 40%",
      str_detect(metric, "below_psc85") ~ "Below PSC 85% Goal & ER > 40%",
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