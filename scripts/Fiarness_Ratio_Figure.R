library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(cowplot)
library(showtext)

font_add_google("DM Sans", "dm_sans")
showtext_auto()

custom_pal <- c(
  "Washington" = "#2E5F6E",    
  "Oregon"    = "#6A9BA8",  
  "British Columbia" = "#8A9E7A",  
  "Alaska" = "#4A7A50"
)

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

# load ======
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
  filter(!population %in% c("Elk", "Cowlitz.fa", "Middle_Shuswap"), !year ==2023)

total_run_jurisdictiondf <- data %>%
  mutate(jurisdiction = region_to_jurisdiction[region]) %>% 
  dplyr::group_by(year,jurisdiction) %>%
  dplyr::summarise(total_run_jurisdiction = sum(as.numeric(total_run), na.rm = TRUE))

total_run_df <- data %>%
  dplyr::group_by(year,population, region) %>%
  dplyr::summarise(total_run = sum(as.numeric(total_run, na.rm = TRUE)))

# PST wide annual totals (use distinct to avoid double counting run size)

pst_totals <- data %>%
  group_by(year) %>%
  dplyr::summarise(
    pst_total_run = sum(as.numeric(total_run), na.rm = TRUE)) %>%
  left_join(
    data %>%
      dplyr::mutate(total_FM_numbers= as.numeric(total_run)-as.numeric(esc_tot)) %>% 
      group_by(year) %>%
      dplyr::summarise(pst_total_harvest = sum(as.numeric(total_FM_numbers), na.rm = TRUE))) 

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
  dplyr::mutate(percent_mort = as.numeric(percent_mort) / 100) %>%
  left_join(total_run_df) %>%
  filter(!is.na(percent_mort)) %>%
  dplyr::mutate(
    mortality_numbers = total_run * percent_mort,
    broad_fishing_region = case_when(
      str_detect(fishery_region, "^seak") ~ "Alaska",
      str_detect(fishery_region, "^ak_term") ~ "Alaska",
      str_detect(fishery_region, "^US_term") & population %in% c("Chilkat", "Stikine", "Taku", "Unuk") ~ "Alaska",
      str_detect(fishery_region, "^US_term") & population %in% c("Grays", "Nisqually", "Green", "Nooksack", "Queets_fa", "Hoh_fa", "Hoh_sp",
                                                                 "Queets_sp", "Grays_Harbor_fa", "Grays_Harbor_sp", "Quillayute_fa", "Skagit_sp",
                                                                 "Snohomish_fa", "Skagit_fa", "Stillaguamish", "Elochoman",
                                                                 "Nicola", "Phillips") ~ "Washington",
      str_detect(fishery_region, "^US_term") & population %in% c("Cowlitz.fa", "Elk", "South_umpqua", "Coquille", "Coweeman", "Lewis",
                                                                 "Nehalem_fa", "Siletz_fa", "Siuslaw_fa", "Hanford_br_w") ~ "Oregon",
      str_detect(fishery_region, "^US_term") & region %in% c("WCVI", "SBC", "NBC") ~ "filter",
      str_detect(fishery_region, "^can_term") ~ "British Columbia",
      str_detect(fishery_region, "^wcvi") ~ "British Columbia",
      str_detect(fishery_region, "^nbc") ~ "British Columbia",
      str_detect(fishery_region, "^sbc") ~ "British Columbia",
      str_detect(fishery_region, "^sfalc") ~ "Oregon",
      fishery_region == "nfalc_s" ~ "Washington",
      fishery_region == "nfalc_t" ~ "Washington",
      fishery_region == "US_is_tot" ~ "Washington",
      fishery_region == "PS_n" ~ "Washington",
      fishery_region == "PS_s" ~ "Washington",
      fishery_region == "wac_n" ~ "Washington",
      TRUE ~ "Check"
    )) %>%
  filter(!broad_fishing_region == "filter") %>% 
  group_by(broad_fishing_region, year) %>%
  dplyr::summarise(FM_Sum_region = sum(mortality_numbers,na.rm=TRUE)) %>%
  rename(jurisdiction = "broad_fishing_region" ) %>%
  left_join(total_run_jurisdictiondf, by = c("year","jurisdiction")) %>%
  dplyr::rename(fish_production = "total_run_jurisdiction" ) %>%
  # dplyr::mutate(ratio = FM_Sum_region/fish_production) %>%
  left_join(pst_totals) %>%
  mutate(harvest_share = FM_Sum_region/pst_total_harvest,
         production_share = fish_production/pst_total_run,
         ratio = harvest_share/production_share, 
         ) %>%
  filter(!year <2000)

## Line Plot  ====
# jurisdiction color palette
jurisdiction_pal <- c(
  "Alaska"           = "#7A4A8A",
  "British Columbia" = "#4A7A50",
  "Washington"       = "#2E5F6E",
  "Oregon"           = "#B57A4A"
)

# calculate mean for reference
jurisdiction_means <- catch_distributions %>%
  group_by(jurisdiction) %>%
  summarise(mean_ratio = mean(ratio, na.rm = TRUE), .groups = "drop")

# time series plot faceted by jurisdiction
ratio_time_plot <- ggplot(catch_distributions, 
                          aes(x = year, y = ratio, 
                              color = jurisdiction,
                              fill = jurisdiction)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, 
             color = "black", linewidth = 0.8) +
  # mean line per jurisdiction
  # geom_hline(data = jurisdiction_means,
  #            aes(yintercept = mean_ratio, color = jurisdiction),
  #            linetype = "dotted", linewidth = 0.8) +
  facet_wrap(~jurisdiction, scales = "free_y", ncol = 2) +
  scale_color_manual(values = jurisdiction_pal, guide = "none") +
  scale_fill_manual(values = jurisdiction_pal, guide = "none") +
  scale_x_continuous(breaks = seq(2000, max(catch_distributions$year), by = 4)) +
  labs(
    title    = "Harvest Equity Ratio by Jurisdiction Over Time",
    subtitle = "Ratio = 1 indicates harvest share proportional to production share\nRatio < 1 indicates jurisdiction receives less harvest than its production warrants\nDotted line = period mean",
    x        = "Year",
    y        = "Equity Ratio (Harvest Share / Production Share)"
  ) +
  theme_minimal(base_family = "dm_sans") +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 11)
  )

ratio_time_plot

ggsave("output/plots/equity_ratio_jurisdiction_timeseries.pdf",
       ratio_time_plot, width = 10, height = 8, units = "in")

## Bar plot =======

catch_distributions <- catch_distributions %>%
  mutate(ratio_centered = ratio - 1)

catch_distributions_plot <- catch_distributions %>%
  mutate(
    ratio_centered = ratio - 1,
    fill_color = case_when(
      jurisdiction == "Alaska"           & ratio_centered >= 0 ~ "#9A6AAA",
      jurisdiction == "Alaska"           & ratio_centered <  0 ~ "#4A1A5A",
      jurisdiction == "British Columbia" & ratio_centered >= 0 ~ "#8A9E7A",
      jurisdiction == "British Columbia" & ratio_centered <  0 ~ "#2A5A30",
      jurisdiction == "Washington"       & ratio_centered >= 0 ~ "#6A9BA8",
      jurisdiction == "Washington"       & ratio_centered <  0 ~ "#1A3F4E",
      jurisdiction == "Oregon"           & ratio_centered >= 0 ~ "#C58A5A",
      jurisdiction == "Oregon"           & ratio_centered <  0 ~ "#7A3A10"
    )
  )

jurisdiction_means <- catch_distributions_plot %>%
  group_by(jurisdiction) %>%
  summarise(mean_ratio_centered = mean(ratio_centered, na.rm = TRUE), 
            .groups = "drop")

ratio_bar_plot <- ggplot(catch_distributions_plot,
                         aes(x = year, y = ratio_centered,
                             fill = fill_color)) +
  geom_col(alpha = 0.9) +
  geom_hline(yintercept = 0,  
             color = "black", linewidth = 0.8) +
  # geom_hline(data = jurisdiction_means,
  #            aes(yintercept = mean_ratio_centered),
  #            color = "grey30",
  #            linetype = "dotted", linewidth = 1,
  #            inherit.aes = FALSE) +
  facet_wrap(~jurisdiction, scales = "free_y", ncol = 2) +
  scale_fill_identity(
    guide  = "legend",
    name   = "Equity Status",
    labels = c(
      "#9A6AAA" = "Alaska: above",
      "#4A1A5A" = "Alaska: below",
      "#8A9E7A" = "BC: above",
      "#2A5A30" = "BC: below",
      "#6A9BA8" = "Washington: above",
      "#1A3F4E" = "Washington: below",
      "#C58A5A" = "Oregon: above",
      "#7A3A10" = "Oregon: below"
    ),
    breaks = c(
      "#9A6AAA", "#4A1A5A",
      "#8A9E7A", "#2A5A30",
      "#6A9BA8", "#1A3F4E",
      "#C58A5A", "#7A3A10"
    )
  ) +
  scale_x_continuous(breaks = seq(2000, max(catch_distributions$year), by = 4)) +
  labs(
    title    = "Harvest Fairness by Jurisdiction Over Time",
    subtitle = "positive = receiving more than production warrants;\nnegative = receiving less than production warrants",
    x        = "Year",
    y        = "Fairness Score (Harvest Share − Production Share)"
  ) +
  theme_minimal(base_family = "dm_sans") +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, color = "grey40"),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "bottom"
  )

ratio_bar_plot

ggsave("output/plots/equity_ratio_jurisdiction_barplot.pdf",
       ratio_bar_plot, width = 10, height = 8, units = "in")

## Two Time Periods =====
# calculate means for two time periods
equity_period_means <- catch_distributions %>%
  mutate(
    ratio_centered = ratio - 1,
    period = case_when(
      year >= 2009 & year <= 2018 ~ "2009-2018",
      year >= 2019 ~ "2019-Present",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(jurisdiction, period) %>%
  summarise(
    mean_equity = mean(ratio_centered, na.rm = TRUE),
    sd_equity   = sd(ratio_centered, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = c("2009-2018", "2019-Present")),
    fill_color = case_when(
      jurisdiction == "Alaska"           & mean_equity >= 0 ~ "#9A6AAA",
      jurisdiction == "Alaska"           & mean_equity <  0 ~ "#4A1A5A",
      jurisdiction == "British Columbia" & mean_equity >= 0 ~ "#8A9E7A",
      jurisdiction == "British Columbia" & mean_equity <  0 ~ "#2A5A30",
      jurisdiction == "Washington"       & mean_equity >= 0 ~ "#6A9BA8",
      jurisdiction == "Washington"       & mean_equity <  0 ~ "#1A3F4E",
      jurisdiction == "Oregon"           & mean_equity >= 0 ~ "#C58A5A",
      jurisdiction == "Oregon"           & mean_equity <  0 ~ "#7A3A10"
    )
  )

# plot
equity_period_plot <- ggplot(equity_period_means,
                             aes(x = period, y = mean_equity,
                                 fill = fill_color, group = jurisdiction)) +
  geom_col(alpha = 0.9, width = 0.6) +
  geom_errorbar(aes(ymin = mean_equity - sd_equity,
                    ymax = mean_equity + sd_equity),
                width = 0.2, color = "grey40", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  annotate("text", x = 0.5, y = 0.02,
           label = "Equitable (0)",
           hjust = 0, size = 3,
           family = "dm_sans", color = "grey40") +
  # facet_wrap(~jurisdiction, ncol = 2) +
  scale_fill_identity(
    guide  = "legend",
    name   = "Equity Status",
    labels = c(
      "#9A6AAA" = "Alaska: above",
      "#4A1A5A" = "Alaska: below",
      "#8A9E7A" = "BC: above",
      "#2A5A30" = "BC: below",
      "#6A9BA8" = "Washington: above",
      "#1A3F4E" = "Washington: below",
      "#C58A5A" = "Oregon: above",
      "#7A3A10" = "Oregon: below"
    ),
    breaks = c(
      "#9A6AAA", "#4A1A5A",
      "#8A9E7A", "#2A5A30",
      "#6A9BA8", "#1A3F4E",
      "#C58A5A", "#7A3A10"
    )
  ) +
  labs(
    title    = "Mean Harvest Equity Score by Jurisdiction and Time Period",
    subtitle = "0 = equitable; positive = receiving more than production warrants;\nnegative = receiving less than production warrants\nError bars = ± 1 SD",
    x        = "Time Period",
    y        = "Mean Equity Score (Harvest Share − Production Share)"
  ) +
  theme_minimal(base_family = "dm_sans") +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, color = "grey40"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "bottom",
    axis.text.x        = element_text(size = 10)
  )

equity_period_plot

ggsave("output/plots/equity_score_period_comparison.pdf",
       equity_period_plot, width = 10, height = 8, units = "in")


# Old =============
local_harvest_df <- group_df %>%  # use group_df not Pop_plot_df since we need fishery_region
  mutate(
    jurisdiction_of_origin = region_to_jurisdiction[region],
    
    # identify terminal fisheries by fishery_region prefix
    is_terminal = str_detect(fishery_region, "^can_term|^ak_term|^US_term"),
    
    # identify the jurisdiction where this fishery occurs (terminal OR marine)
    fishery_jurisdiction = case_when(
      
      # --- TERMINAL FISHERIES ---
      str_detect(fishery_region, "^ak_term") ~ "Alaska",
      str_detect(fishery_region, "^US_term") & population %in% c("Chilkat", "Stikine", "Taku", "Unuk") ~ "Alaska",
      str_detect(fishery_region, "^US_term") & population %in% c("Grays", "Nisqually", "Green", "Nooksack", 
                                                                 "Queets_fa", "Hoh_fa", "Hoh_sp",
                                                                 "Queets_sp", "Grays_Harbor_fa", "Grays_Harbor_sp", 
                                                                 "Quillayute_fa", "Skagit_sp", "Snohomish_fa", 
                                                                 "Skagit_fa", "Stillaguamish", "Elochoman",
                                                                 "Nicola", "Phillips") ~ "Washington",
      str_detect(fishery_region, "^US_term") & population %in% c("Cowlitz.fa", "Elk", "South_umpqua", "Coquille", 
                                                                 "Coweeman", "Lewis", "Nehalem_fa", "Siletz_fa", 
                                                                 "Siuslaw_fa", "Hanford_br_w") ~ "Oregon",
      str_detect(fishery_region, "^can_term") ~ "British Columbia",
      
      # --- MARINE / OCEAN FISHERIES ---
      # Alaska marine
      str_detect(fishery_region, "^seak") ~ "Alaska",
      
      # BC marine
      str_detect(fishery_region, "^nbc") ~ "British Columbia",
      str_detect(fishery_region, "^sbc") ~ "British Columbia",
      str_detect(fishery_region, "^wcvi") ~ "British Columbia",
      
      # Washington marine
      fishery_region %in% c("PS_n", "PS_s", "wac_n", 
                            "nfalc_s", "nfalc_t", 
                            "US_is_tot") ~ "Washington",
      
      # Oregon marine
      str_detect(fishery_region, "^sfalc") ~ "Oregon",
      
      TRUE ~ NA_character_
    ),
    
    # local harvest = harvested by a fishery in the jurisdiction of origin
    # (terminal OR marine)
    is_local = fishery_jurisdiction == jurisdiction_of_origin
  ) %>%
  group_by(population, region, year, total_run) %>%
  summarise(
    local_harvest  = sum(mortality_numbers[is_local], na.rm = TRUE),
    total_harvest  = sum(mortality_numbers, na.rm = TRUE),
    .groups = "drop"
  )


# Equity Ratio by region ====== 
# region level equity ratio - annual
equity_region_annual <- local_harvest_df %>%
  left_join(pst_totals, by = "year") %>%
  filter(!year < 2009) %>%
  # distinct(population, region, year, total_run, local_harvest,
  #          total_harvest, pst_total_run, pst_total_harvest) %>%
  # first sum across all stocks within a region for each year
  group_by(region, year, pst_total_run, pst_total_harvest) %>%
  summarise(
    region_total_run     = sum(total_run, na.rm = TRUE),
    region_local_harvest = sum(local_harvest, na.rm = TRUE),
    region_total_harvest = sum(total_harvest, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # production = what came back to the jurisdiction
    # escapement + local terminal harvest = fish the jurisdiction produced AND retained
    # local_production = pst_total_run, #escapement + local_harvest,
    
    # equity ratio
    production_share = region_total_run / pst_total_run,
    local_harvest_share = region_local_harvest / pst_total_harvest,
    
    equity_ratio = local_harvest_share - production_share,
    
    # map region to jurisdiction for coloring
    jurisdiction = region_to_jurisdiction[region],
    
    region = factor(region, levels = c(
      "SEAK", "AK",
      "NBC", "SBC", "WCVI",
      "PS", "OP", "WA",
      "MCR", "LCR", "OC", "ORC", "OR", "COL"
    ))
  )

# summarise to mean across years for bar plot
equity_region_mean <- equity_region_annual %>%
  group_by(region, jurisdiction) %>%
  summarise(
    mean_equity_ratio = mean(equity_ratio, na.rm = TRUE),
    sd_equity_ratio   = sd(equity_ratio, na.rm = TRUE),
    .groups = "drop"
  )

# region level bar plot
equity_region_plot <- ggplot(equity_region_mean,
                             aes(x = reorder(region, mean_equity_ratio),
                                 y = mean_equity_ratio,
                                 fill = region)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_equity_ratio - sd_equity_ratio,
                    ymax = mean_equity_ratio + sd_equity_ratio),
                width = 0.3, color = "grey40", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  annotate("text", x = 1, y = 1.05,
           label = "Equitable", hjust = 0, size = 3,
           family = "dm_sans", color = "grey40") +
  scale_fill_manual(values = region_pal, name = "Region") +
  labs(
    x        = "Region",
    y        = "Equity Ratio (Local Harvest Share / Production Share)",
    title    = "Harvest Equity by Region",
    subtitle = "Ratio = 1 indicates harvest proportional to production;\n< 1 indicates region receives less harvest than its production warrants"
  ) +
  theme_minimal(base_family = "dm_sans") +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 10),
    legend.position    = "right",
    panel.grid.major.x = element_blank()
  )

equity_region_plot

ggsave("output/plots/equity_ratio_region_barplot.pdf",
       equity_region_plot, width = 10, height = 6, units = "in")

# region level time series plots
regions <- unique(equity_region_annual$region)

equity_region_time_plots <- purrr::map(regions, function(reg) {
  
  df        <- equity_region_annual %>% filter(region == reg)
  reg_color <- region_pal[as.character(reg)]
  reg_mean  <- mean(df$equity_ratio, na.rm = TRUE)
  reg_juris <- unique(df$jurisdiction)
  
  ggplot(df, aes(x = year, y = equity_ratio)) +
    geom_line(color = reg_color, linewidth = 1) +
    geom_point(color = reg_color, size = 2) +
    geom_hline(yintercept = 1, linetype = "dashed",
               color = "black", linewidth = 0.8) +
    geom_hline(yintercept = reg_mean, linetype = "dotted",
               color = reg_color, linewidth = 0.8) +
    annotate("text", x = min(df$year), y = 1.05,
             label = "Equitable (ratio = 1)", hjust = 0, size = 3,
             family = "dm_sans", color = "grey40") +
    annotate("text", x = min(df$year), y = reg_mean + 0.05,
             label = paste0("Mean = ", round(reg_mean, 2)),
             hjust = 0, size = 3, family = "dm_sans", color = reg_color) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 2)) +
    labs(
      title    = paste0(reg, "  |  ", reg_juris),
      x        = "Year",
      y        = "Equity Ratio (Local Harvest Share / Production Share)",
      subtitle = "Ratio < 1: region receives less harvest than its production warrants\nDotted line = period mean"
    ) +
    theme_minimal(base_family = "dm_sans") +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 9, color = "grey40"),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
})

names(equity_region_time_plots) <- regions

# save to PDF
pdf("output/plots/equity_ratio_region_timeseries.pdf", width = 8, height = 5)
for (reg in levels(equity_region_annual$region)) {
  if (reg %in% names(equity_region_time_plots)) {
    print(equity_region_time_plots[[reg]])
  }
}
dev.off()

# Ratio by individual Stock  ====
# calculate equity ratio - annual (for time series plots)
equity_scaled_annual <- local_harvest_df %>%
  left_join(pst_totals, by = "year") %>%
  filter(!year < 2009) %>%
  distinct(population, region, year, total_run, local_harvest,
           total_harvest, pst_total_run, pst_total_harvest) %>%
  mutate(
    # what fraction of ALL PST production does this stock represent
    production_share = total_run / pst_total_run,
    
    # what fraction of ALL PST harvest does local jurisdiction receive from this stock
    local_harvest_share = local_harvest / pst_total_harvest,
    
    # also track external harvest for context
    external_harvest       = total_harvest - local_harvest,
    external_harvest_share = external_harvest / pst_total_harvest,
    
    # equity ratio
    equity_ratio = local_harvest_share / production_share,
    
    region = factor(region, levels = c(
      "SEAK", "AK",
      "NBC", "SBC", "WCVI",
      "PS", "OP", "WA",
      "MCR", "LCR", "OC", "ORC", "OR", "COL"
    ))
  )

# calculate mean equity ratio across years (for bar plot)
equity_scaled <- equity_scaled_annual %>%
  group_by(population, region) %>%
  summarise(
    mean_equity_ratio = mean(equity_ratio, na.rm = TRUE),
    sd_equity_ratio   = sd(equity_ratio, na.rm = TRUE),
    .groups = "drop"
  )

# mean bar plot
equity_plot <- ggplot(equity_scaled,
                      aes(x = reorder(population, mean_equity_ratio),
                          y = mean_equity_ratio,
                          fill = region)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin  = mean_equity_ratio - sd_equity_ratio,
                    ymax  = mean_equity_ratio + sd_equity_ratio),
                width = 0.3, color = "grey40", alpha = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  annotate("text", x = 1, y = 1.05,
           label = "Equitable", hjust = 0, size = 3,
           family = "dm_sans", color = "grey40") +
  scale_fill_manual(values = region_pal, name = "Stock Region") +
  labs(
    x        = "Indicator Stock",
    y        = "Equity Ratio (Local Harvest Share / Production Share)",
    title    = "Harvest Equity Relative to Production by Indicator Stock",
    subtitle = "Ratio = 1 indicates harvest proportional to production;\n< 1 indicates jurisdiction receives less harvest than its production warrants"
  ) +
  theme_minimal(base_family = "dm_sans") +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    legend.position    = "right",
    panel.grid.major.x = element_blank()
  )

equity_plot

# save bar plot
ggsave("output/plots/equity_ratio_mean_barplot.pdf", 
       equity_plot, width = 12, height = 6, units = "in")

# time series plots per stock
populations <- unique(equity_scaled_annual$population)

equity_time_plots <- purrr::map(populations, function(pop) {
  
  df         <- equity_scaled_annual %>% filter(population == pop)
  pop_region <- as.character(unique(df$region))
  pop_color  <- region_pal[pop_region]
  
  # get mean for reference line
  pop_mean <- mean(df$equity_ratio, na.rm = TRUE)
  
  ggplot(df, aes(x = year, y = equity_ratio)) +
    geom_line(color = pop_color, linewidth = 1) +
    geom_point(color = pop_color, size = 2) +
    geom_hline(yintercept = 1, linetype = "dashed",
               color = "black", linewidth = 0.8) +
    geom_hline(yintercept = pop_mean, linetype = "dotted",
               color = pop_color, linewidth = 0.8) +
    annotate("text", x = min(df$year), y = 1.05,
             label = "Equitable (ratio = 1)", hjust = 0, size = 3,
             family = "dm_sans", color = "grey40") +
    annotate("text", x = min(df$year), y = pop_mean + 0.05,
             label = paste0("Mean = ", round(pop_mean, 2)),
             hjust = 0, size = 3, family = "dm_sans", color = pop_color) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 2)) +
    labs(
      title    = paste0(pop, "  |  Region: ", pop_region),
      x        = "Year",
      y        = "Equity Ratio (Local Harvest Share / Production Share)",
      subtitle = "Ratio < 1: jurisdiction receives less harvest than its production warrants\nDotted line = stock mean"
    ) +
    theme_minimal(base_family = "dm_sans") +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 9, color = "grey40"),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
})

names(equity_time_plots) <- populations

# save time series to PDF
pdf("output/plots/equity_ratio_by_stock_timeseries.pdf", width = 8, height = 5)
for (pop in populations) {
  print(equity_time_plots[[pop]])
}
dev.off()
