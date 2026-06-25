# Exploitation Rates for just ocean fisheries for all stocks. 
# Plot AABM harvest versus total stock escapements 

library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(PNWColors)
library(patchwork)  

# escapement goals, likely need to be reviewed for all stocks. 

# esc_goals <- read_csv("data/Escapement_goals_data_all_2026-02-19.csv") %>%
#   filter(SeriesLabel %in% c("PSC-Agreed Goal","Agency Goal","PSC Goal",
#                             "Pre-PSC Goal: Calib. Total Adult Esc",
#                             "Pre-PSC Goal: MR Esc")) %>%          
#   rename(year = "Year")  %>%
#  spread(SeriesLabel, Values)
#  
#  write_csv(esc_goals, "data/Escapement_Goals_Use.csv") 
   
# unique(data$population)
# 
# unique(esc_goals$StockName)

#  filter(!is.na(Values)) %>%  # this is just for grays harbor fall because double goals are confusing 
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

unique(data$population)
unique(esc_goals$population)

# Total Run DF ======
total_run_df <- data %>%
  filter(!population %in% c("Elk", "Cowlitz.fa", "Elochoman", "Grays", "Middle_Shuswap", "South_Thompson", 
                            "Nisqually", "Tahsish_fa","Bedwell_fa", "Megin_fa","Moyeha_fa",
                            #"Phillips",
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
                   "Upper Goal", 
                   "Lower Goal")


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

unique(joined_df$population)

# All goals for plotting
esc_goals_all <- esc_goals %>%
  filter(!is.na(escapement_goal)) %>%
  filter(!year <2000)

## Plot with Esc Goals & ER ======

populations <- unique(joined_df$population)

#pop = populations[[1]]

# Build one plot per population
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
      caption = "Horizontal line = Escapement Goal (Solid = PSC -Agreed Goal, Dotted = Agency Goal\n Red Points = Above Average Exploitation Rate & Below Average Escapement, Blue Circles = Above average Exploitation and stock not meeting 85% of Escapement Goal",
  
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



# Pull out 4 Rivers for the PST Manuscript ========
 
  fig_pops = c("Unuk", "Queets SprSum","Siuslaw Fall", "Lower Shuswap")
  df_pop <- joined_df %>% filter(population %in% fig_pops)
  esc_goals_fig <- esc_goals_all %>% filter(population %in% fig_pops)

  Figure2 <- ggplot(df_pop, aes(x = year, y = esc_tot)) +
    
    # Background shading scaled continuously to ER rate
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                  ymin = -Inf, ymax = Inf, fill = ocean_er),
              alpha = 0.8, inherit.aes = FALSE) +
    
    # Escapement bars
    geom_col(alpha = 0.85) +
    
    geom_point(data = df_pop %>% filter(PSC_85_Goal == TRUE),
               aes(x = year-0.15, y = esc_tot),
               color = "#2980b9", size = 1.5, shape = 17, inherit.aes = FALSE) +
    
    geom_line(data = esc_goals_fig %>% filter(LineType == "Solid"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "solid", show.legend = FALSE) +
    
    geom_line(data = esc_goals_fig %>% filter(LineType == "Dotted"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "dotted", show.legend = FALSE) +
 
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
   facet_wrap(~population,scales = "free") + 
    labs(
      # title = paste("Escapement vs. Goal:", pop),
      # caption = "Red shading = Ocean Exploitation Rates\nDashed line = Escapement Goal\n Red Points = Above Average Exploitation Rate & Below Average Escapement",
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
  
  
Figure2
# Save each plot to its own page in a single PDF
ggsave("output/plots/Paper_Chinook_Coastwide_escapement_by_population.png", width = 10, height = 7)


# seperate legend: =====
library(patchwork)

fig_pops = c("Unuk", , "Lower Shuswap","Queets SprSum", "Siuslaw Fall")

plots_fig2 <- lapply(fig_pops, function(pop) {
  
  df_pop <- joined_df %>% filter(population == pop)
  esc_goals_fig <- esc_goals_all %>% filter(population == pop)
  
  ggplot(df_pop, aes(x = year, y = esc_tot)) +
    
    geom_rect(aes(xmin = year - 0.5, xmax = year + 0.5,
                  ymin = -Inf, ymax = Inf, fill = ocean_er),
              alpha = 0.8, inherit.aes = FALSE) +
    
    geom_col(alpha = 0.85) +
    
    geom_point(data = df_pop %>% filter(PSC_85_Goal == TRUE),
               aes(x = year - 0.15, y = esc_tot),
               color = "#2980b9", size = 1.5, shape = 17, inherit.aes = FALSE) +
    
    geom_line(data = esc_goals_fig %>% filter(LineType == "Solid"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "solid", show.legend = FALSE) +
    
    geom_line(data = esc_goals_fig %>% filter(LineType == "Dotted"),
              aes(y = escapement_goal, group = goal_type),
              color = "black", linetype = "dotted", show.legend = FALSE) +
    
    geom_point(data = df_pop %>% filter(vulnerable_er == TRUE),
               aes(x = year + 0.15, y = esc_tot),
               color = "#c0392b", size = 1.5, inherit.aes = FALSE) +
    
    scale_fill_gradient2(
      low      = "#fff5f5",
      mid      = "#fff5f5",
      high     = "#c0392b",
      midpoint = mean(as.numeric(df_pop$ocean_er), na.rm = TRUE),
      name     = "Mixed Stock Exploitation Rate"
    ) +
    
    scale_y_continuous(expand = c(0,0)) +
    
    labs(title = pop, x = "Year", y = "Escapement") +
    
    theme_bw(base_size = 12) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", size = 10),
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 12),
      panel.grid.minor = element_blank()
    )
})
# 
# Figure2 <- wrap_plots(plots_fig2, ncol = 2) +
#   plot_layout(guides = "keep")
# 
# Figure2

names(plots_fig2) <- fig_pops

Figure2 <- wrap_plots(plots_fig2[["Unuk"]],
  plots_fig2[["Lower Shuswap"]], plots_fig2[["Queets SprSum"]],
  plots_fig2[["Siuslaw Fall"]], 
  ncol = 2
) + plot_layout(guides = "keep")


ggsave("output/plots/Paper_Chinook_Coastwide_escapement_by_population.png",
       Figure2, width = 12, height = 9)
cyer$`CYER_Limit_2019-2028`
# Look at ER Limits that are stock specific.===========
# CYER data taken from their CTC ER 2025 
cyer <- read_csv("data/CYER_Limits_Only.csv") %>%
  filter(!is.na(`CYER_Limit_2019-2028`)) %>%
  group_by(Population) %>%
  summarise(`CYER_Limit_2019-2028` =sum(`CYER_Limit_2019-2028`)) %>% 
  rename(population = "Population")

# join cyer with 
cyer_combo <- left_join(joined_df, cyer)


# Summary statistics ===========
summ_df <- joined_df %>% 
  filter(year > 2008)  
 