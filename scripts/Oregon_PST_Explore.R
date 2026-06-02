# Kirk and Will asked to look at if the 12% reduction in AK Catch durign the last treaty resulted in any changes in 
# returning fish to OR. 

# Look at individual stocks for this  

library(here)
library(readxl)
library(viridis)
library(cowplot) 
library(ggrepel)
 
# colors ====
custom_pal <- c(
  "Oregon Coast\nIn-River" = "#2E5F6E",    
  "Oregon Coast"           = "#6A9BA8",
  "Washington"             = "#E8E8E8",   
  # "Puget Sound"          = "grey",  
  "British Columbia"       = "#8A9E7A", 
  "Alaska"                 = "#4A7A50"
)

# load ====== 
data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
  filter(region %in% c("OC","ORC"),
         !year == 2023, !is.na(total_run), !total_run =="NA") 
 

total_run_df<-data %>%  
  dplyr::select(year,population,region,total_run) %>%
  dplyr::mutate(total_run = as.numeric(total_run))

# Individual stocks =======
total_run_df<-data %>%  
  dplyr::select(year,population,region,total_run) %>%
  dplyr::mutate(total_run = as.numeric(total_run))

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
    "term_tot"
  )) %>%
  dplyr::mutate(percent_mort = as.numeric(percent_mort)/100) 

OC_fish <- catch_distributions %>%
  left_join(total_run_df) %>% 
  filter(!is.na(percent_mort )) %>% 
  dplyr::mutate(mortality_numbers = total_run *percent_mort,
                broad_region = case_when(
                  # Alaska
                  str_detect(fishery_region, "^seak") ~ "Alaska",
                  str_detect(fishery_region, "^ak_term")  ~ "Alaska",
                  
                  str_detect(fishery_region, "^US_term")  ~ "Oregon Coast\nIn-River",
             
                  # Canada
                  str_detect(fishery_region, "^can_term")  ~ "British Columbia",
                  str_detect(fishery_region, "^wcvi")     ~ "British Columbia",#"West Coast Vancouver Island",
                  str_detect(fishery_region, "^nbc")      ~ "British Columbia", #"North Coast BC",
                  str_detect(fishery_region, "^sbc")      ~ "British Columbia",
                  
                  # str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
                  
                  # Falcon (US)
                  str_detect(fishery_region, "^sfalc")    ~  "Oregon Coast",#"South of Falcon",
                  # str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
                  
                  fishery_region == "nfalc_s" ~ "Washington",#"Washington Coast\nOcean",
                  fishery_region == "nfalc_t" ~  "Washington",#"Washington Coast\nOcean",
 
                  # WA     
                  fishery_region == "PS_n" ~ "Washington", #"Puget Sound",
                  fishery_region == "PS_s" ~ "Washington", #"Puget Sound",
                  
                  fishery_region == "wac_n" ~  "Washington",#"Washington Coast\nIn-River",
                  # Other
                  TRUE                                    ~ "Check")) 

total_FMnumbers <- OC_fish %>%
  group_by(year,population) %>%
  # get annual sum of all fishery mortality from OP for the year
  dplyr::summarise(total_FM_numbers = sum(mortality_numbers))  # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize.

OC_plot_df<- OC_fish %>% 
  dplyr::select(-c(total_run,fishery_region)) %>%
  ungroup() %>%
  group_by(year, broad_region,population) %>%
  dplyr::summarise(mort_broad_region = sum(mortality_numbers)) %>% 
  left_join(total_FMnumbers) %>%
  dplyr::mutate(percent_mort = mort_broad_region/total_FM_numbers,
                broad_region = factor(broad_region, 
                                      levels = rev(c(
                                        "Oregon Coast",
                                        "Oregon Coast\nIn-River",
                                        "Washington",  
                                        "British Columbia", 
                                        "Alaska"))), 
                population = case_when(population == "Siletz_fa"  ~  "Siletz Fall",
                                       population == "Siuslaw_fa"  ~ "Siuslaw Fall",
                                       population == "Nehalem_fa"  ~ "Nehalem Fall",
                                       population == "south_umpqua"  ~ "South Umpqua",
                                       TRUE ~ population))

### Nehalem ========= 
pie_df <- OC_plot_df %>%
  filter(year > 2008,
         population %in% c("Nehalem Fall")) %>% #, "Siletz Fall", "Siuslaw Fall")) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  # group_by(population) %>%                          # <-- group by population ONLY
  arrange(desc(broad_region)) %>%             # consistent ordering within each facet
  mutate(
    cum_pos = cumsum(avg_mort) - avg_mort / 2,      # midpoint within each facet's stack
    label = paste0(round(avg_mort * 100, 1), "%")
  ) %>%
  ungroup()


Nehalem <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_label_repel(
    aes(y = cum_pos, label = ifelse(avg_mort >= 0.02, paste0(round(avg_mort * 100, 1), "%"), "")),
    size = 3,
    fontface = "bold",
    nudge_x = 0.6,          # push labels outward
    show.legend = FALSE,
    segment.size = 0.3
  ) +
  # facet_wrap(~population) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_pal) + 
  #  scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(title = "Nehalem Fall, Avg 2009-2020") +
  theme_void() +
  theme(
    # legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )
Nehalem

### Siletz ========= 
pie_df <- OC_plot_df %>%
  filter(year > 2008,
         population %in% c("Siletz Fall")) %>% #, "Siletz Fall", "Siuslaw Fall")) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  # group_by(population) %>%                          # <-- group by population ONLY
  arrange(desc(broad_region)) %>%             # consistent ordering within each facet
  mutate(
    cum_pos = cumsum(avg_mort) - avg_mort / 2,      # midpoint within each facet's stack
    label = paste0(round(avg_mort * 100, 1), "%")
  ) %>%
  ungroup()


Siletz <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_label_repel(
    aes(y = cum_pos, label = ifelse(avg_mort >= 0.02, paste0(round(avg_mort * 100, 1), "%"), "")),
    size = 3,
    fontface = "bold",
    nudge_x = 0.6,          # push labels outward
    show.legend = FALSE,
    segment.size = 0.3
  ) +
  # facet_wrap(~population) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_pal) + 
  #  scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(title = "Siletz Fall, Avg 2009-2020") +
  theme_void() +
  theme(
    # legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )
Siletz

### Siuslaw ========= 
pie_df <- OC_plot_df %>%
  filter(year > 2008,
         population %in% c("Siuslaw Fall")) %>% #, "Siletz Fall", "Siuslaw Fall")) %>% 
  group_by(broad_region) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE)) %>%
  ungroup() %>%
  # group_by(population) %>%                          # <-- group by population ONLY
  arrange(desc(broad_region)) %>%             # consistent ordering within each facet
  mutate(
    cum_pos = cumsum(avg_mort) - avg_mort / 2,      # midpoint within each facet's stack
    label = paste0(round(avg_mort * 100, 1), "%")
  ) %>%
  ungroup()


Siuslaw <- ggplot(pie_df, aes(x = "", y = avg_mort, fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  geom_label_repel(
    aes(y = cum_pos, label = ifelse(avg_mort >= 0.02, paste0(round(avg_mort * 100, 1), "%"), "")),
    size = 3,
    fontface = "bold",
    nudge_x = 0.6,          # push labels outward
    show.legend = FALSE,
    segment.size = 0.3
  ) +
  # facet_wrap(~population) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_pal) + 
  #  scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(title = "Siuslaw Fall, Avg 2009-2020") +
  theme_void() +
  theme(
    # legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
    plot.background = element_blank()
  )
Siuslaw

### Save individual Pies ========
individ_pie <- ggpubr::ggarrange(Siuslaw,Siletz, Nehalem, nrow = 1)
ggsave(
  filename = "output/plots/Individ_Pie.jpeg",
  plot = individ_pie,
  width =6,
  height =4,
  dpi = 300,
  units = "in"
)

##  3. Main bar chart ==================
OC_FM_stacked_Facet <- ggplot(OC_plot_df,
                              aes(x = year,
                                  y = percent_mort,
                                  fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  scale_y_continuous(
    expand = c(0, 0),
    name = "Proportion of Fishery Mortality",
    labels = scales::percent_format()
  ) +
  scale_x_continuous(
    name = "Year",
    expand = c(0, 0)
  ) +
  facet_wrap(~population) + 
  scale_fill_manual(values = custom_pal) + 
  # scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(
    fill = "Fishery Region",
    title = "Oregon Coast Chinook Salmon",
    subtitle = "Proportional Fishery Mortality by Region & Year"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"))#,
    # legend.position = "none")
OC_FM_stacked_Facet

## 3B. Select stocks bar chart ==================
OC_FM_stacked_Facet2 <- ggplot(OC_plot_df %>% filter(population %in% c("Nehalem Fall", "Siletz Fall", "Siuslaw Fall")),
                               aes(x = year,
                                   y = percent_mort,
                                   fill = broad_region)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  scale_y_continuous(
    expand = c(0, 0),
    name = "Proportion of Fishery Mortality",
    labels = scales::percent_format()
  ) +
  scale_x_continuous(
    name = "Year",
    expand = c(0, 0)
  ) +
  facet_wrap(~population) + 
  scale_fill_manual(values = custom_pal) + 
  # scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
  labs(
    fill = "Fishery Region",
    title = "Oregon Coast Chinook Salmon",
    subtitle = "Proportional Fishery Mortality by Region & Year"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")#,
    #legend.position = "none"
  )
OC_FM_stacked_Facet2

# 4. Save as JPEG =======
ggsave(
  filename = "output/plots/OR_Coast_FM_FACET1_stacked.jpeg",
  plot = OC_FM_stacked_Facet,
  width =6,
  height =5,
  dpi = 300,
  units = "in"
)


ggsave(
  filename = "output/plots/OR_Coast_FM_FACET2_stacked.jpeg",
  plot = OC_FM_stacked_Facet2,
  width =6,
  height =5,
  dpi = 300,
  units = "in"
)

# Time series of AK exploitation rate, total catch and OR coast abundances. 

#ak_ER <- 
  OC_plot_df %>% 
  filter(broad_region == "Alaska") %>%
  ggplot(aes(x=year, y = percent_mort)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~population)+
    labs(x="Year", y ="Exploitation Rate", title = "Alaskan Harvest of OR Chinook salmon") 
  
  OC_plot_df %>% 
    filter(broad_region == "Alaska") %>%
    ggplot(aes(x=year, y = mort_broad_region)) + 
    geom_line() + 
    geom_point() + 
    facet_wrap(~population, scales= "free")+
    labs(x="Year", y ="Harvest Numbers", title = "Alaskan Harvest of OR Chinook salmon") 

  # If AK ER changed, did total ER change too?  
  # Plot total ER and AK ER 
  ERs <- data %>% 
    dplyr::select(year, population, er, seak_t, seak_s,seak_n,ak_term_t, ak_term_n,ak_term_s) %>%
    dplyr::rename(annual_er = "er") %>% 
    dplyr::mutate(across(c(seak_t, seak_s,seak_n,ak_term_t, ak_term_n,ak_term_s), as.numeric),
                  across(c(seak_t, seak_s,seak_n,ak_term_t, ak_term_n,ak_term_s), ~ as.numeric(.x) / 100),
                  ak_er = rowSums(across(c(seak_t,seak_s,seak_n,ak_term_t, ak_term_n,ak_term_s)))) %>% 
    dplyr::select(-c(seak_t, seak_s,seak_n,ak_term_t, ak_term_n,ak_term_s))
  
  head(ERs)

  
  # ── 1. PREP DATA ──────────────────────────────────────────────────────────────
  df <- ERs |>
    mutate(
      annual_er = as.numeric(annual_er),
      period    = if_else(year < 2019, "Pre-2019", "Post-2019"),
      period    = factor(period, levels = c("Pre-2019", "Post-2019"))
    )
  
  intervention_year <- 2019
  
  pop_colors <- setNames(
    scales::hue_pal()(length(unique(df$population))),
    unique(df$population)
  )
  
  base_theme <- theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text         = element_text(face = "bold", size = 9),
      plot.title         = element_text(face = "bold", size = 12),
      plot.subtitle      = element_text(color = "grey40", size = 9),
      axis.title         = element_text(size = 9),
      legend.position    = "bottom",
      legend.title       = element_blank()
    )
  
  intervention_vline <- geom_vline(
    xintercept = intervention_year, linetype = "dashed",
    color = "grey30", linewidth = 0.6
  )
  
  # ── PLOT 1: AK exploitation rate over time, by population ────────────────────
  # Core question: did AK ER drop after 2012?
  
  p1 <- df |>
    ggplot(aes(x = year, y = ak_er, color = population, group = population)) +
    geom_line(linewidth = 0.8, alpha = 0.7) +
    # geom_point(size = 1.8, alpha = 0.8) +
    intervention_vline +
    annotate("text", x = intervention_year + 0.3, y = Inf,
             label = "2019", hjust = 0, vjust = 1.5,
             size = 2.8, color = "grey40") +
    scale_color_manual(values = pop_colors) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 4)) +
    labs(
      title    = "AK exploitation rate by OR Chinook population",
      subtitle = "Did AK ER drop post-2019 across populations?",
      x = NULL, y = "AK exploitation rate"
    ) +
    base_theme + facet_wrap(~population)
  p1
  
  # ── PLOT 2: Total (annual) ER over time, by population ───────────────────────
  # Leakage check: if AK ER ↓ but annual_er stays flat, other fisheries compensated
  
  p2 <- df |>
    ggplot(aes(x = year, y = annual_er, color = population)) +
    geom_line(linewidth = 0.8, alpha = 0.7) +
    geom_point(size = 1.8, alpha = 0.8) +
    intervention_vline +
    annotate("text", x = intervention_year + 0.3, y = Inf,
             label = "2019", hjust = 0, vjust = 1.5,
             size = 2.8, color = "grey40") +
    scale_color_manual(values = pop_colors) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 4)) +
    labs(
      title    = "Total annual exploitation rate by OR Chinook population",
      subtitle = "If total ER didn't fall when AK ER fell, leakage occurred in other fisheries",
      x = NULL, y = "Total annual exploitation rate"
    ) +
    base_theme
  p2
  
  # ── PLOT 3: AK share of total ER ─────────────────────────────────────────────
  # AK ER / total ER — normalizes for run-size effects
  # Drop in share post-2012 = real reduction in AK pressure
  
  p3 <- df |>
    mutate(ak_share = ak_er / annual_er) |>
    ggplot(aes(x = year, y = ak_share, color = population)) +
    geom_line(linewidth = 0.8, alpha = 0.7) +
    geom_point(size = 1.8, alpha = 0.8) +
    intervention_vline +
    annotate("text", x = intervention_year + 0.3, y = Inf,
             label = "2012", hjust = 0, vjust = 1.5,
             size = 2.8, color = "grey40") +
    scale_color_manual(values = pop_colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 4)) +
    labs(
      title    = "AK share of total exploitation rate, by population",
      subtitle = "AK ER / total ER — drop post-2019 = AK pressure reduced relative to other fisheries",
      x = NULL, y = "AK share of total ER"
    ) +
    base_theme
  p3
  
  # ── PLOT 4: AK ER vs total ER scatter, pre vs post ───────────────────────────
  # Key leakage plot: if post-2012 points shift left (AK ↓) but NOT down (total unchanged),
  # other fisheries filled the gap
  
  p4 <- df |>
    ggplot(aes(x = ak_er, y = annual_er, color = period)) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.12, linewidth = 0.8) +
    geom_point(aes(shape = population), size = 2.5, alpha = 0.8) +
    scale_color_manual(values = c("Pre-2019" = "#ba7517", "Post-2019" = "#a32d2d")) +
    labs(
      title    = "AK ER vs total annual ER — pre vs post 2012",
      subtitle = "Leakage = post-2019 points shift left (AK ↓) but stay high on y-axis (total ER unchanged)",
      x = "AK exploitation rate", y = "Total annual exploitation rate",
      color = NULL, shape = "Population"
    ) +
    base_theme
  p4
  
  # ── PLOT 5: Faceted by population — both ERs on same panel ───────────────────
  # Most useful for seeing stock-specific patterns
  
  p5 <- df |>
    pivot_longer(cols = c(annual_er, ak_er),
                 names_to = "er_type", values_to = "er_value") |>
    mutate(er_type = recode(er_type,
                            "annual_er" = "Total annual ER",
                            "ak_er"     = "AK ER")) |>
    ggplot(aes(x = year, y = er_value, color = er_type)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    intervention_vline +
    scale_color_manual(values = c("Total annual ER" = "#185fa5", "AK ER" = "#ba7517")) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 8)) +
    facet_wrap(~population, scales = "free_y") +
    labs(
      title    = "AK ER vs total ER by population (faceted)",
      subtitle = "Gap between lines = non-AK fishery pressure. Widening gap post-2019 = leakage",
      x = NULL, y = "Exploitation rate", color = NULL
    ) +
    base_theme
  p5
  # ── PLOT 6: Before/after boxplots ─────────────────────────────────────────────
  # Distribution of AK ER and total ER across populations and years, pre vs post
  
# 10-YEAR PERIODS WORKING BACK FROM 2019 ────────────────────────────────────
  # Assign each year to a decade bin, working backwards from 2019
  
  max_year <- max(df$year)  # whatever your most recent year is
  
  df_decades <- df |>
    mutate(
      decade = case_when(
        year >= 2019             ~ paste0("2019–", max_year),
        year >= 2009 & year < 2019 ~ "2009–2018",
        year >= 1999 & year < 2009 ~ "1999–2008",
        year >= 1989 & year < 1999 ~ "1989–1998",
        year >= 1979 & year < 1989 ~ "1979–1988",
        TRUE                       ~ paste0("Pre-1979")
      ),
      # order chronologically oldest to newest
      decade = factor(decade, levels = c(
        "Pre-1979", "1979–1988", "1989–1998",
        "1999–2008", "2009–2018", paste0("2019–", max_year)
      ))
    )
  
  # palette — one color per decade, light to dark
  decade_levels <- levels(df_decades$decade)
  decade_colors <- setNames(
    colorRampPalette(c("#d4a84b", "#8b2020"))(length(decade_levels)),
    decade_levels
  )
  
  p6 <- df_decades |>
    pivot_longer(cols = c(annual_er, ak_er),
                 names_to = "er_type", values_to = "er_value") |>
    mutate(er_type = recode(er_type,
                            "annual_er" = "Total annual ER",
                            "ak_er"     = "AK ER")) |>
    ggplot(aes(x = decade, y = er_value, fill = decade)) +
    geom_boxplot(alpha = 0.8, outlier.size = 1.2, linewidth = 0.4) +
    geom_jitter(width = 0.18, size = 0.7, alpha = 0.35) +
    stat_summary(fun = mean, geom = "point", shape = 18,
                 size = 3, color = "white", show.legend = FALSE) +
    scale_fill_manual(values = decade_colors) +
    scale_x_discrete(guide = guide_axis(angle = 30)) +
    facet_wrap(~er_type, scales = "free_y") +
    labs(
      title    = "Exploitation rate distributions by decade",
      subtitle = "10-year bins working back from 2019. Diamond = decade mean. Across all populations.",
      x = NULL, y = "Exploitation rate"
    ) +
    base_theme +
    theme(legend.position = "none")
  
  p6
 
  # ER went up while AK ER went down post 2012, so what ER is driving the increases? ======
  
  # If AK ER changed, did total ER change too?  
  # Plot total ER and AK ER 
  # All AK and non-AK columns that need /100 conversion
  ak_cols   <- c("seak_t", "seak_s", "seak_n", "ak_term_t", "ak_term_n", "ak_term_s")
  bc_cols   <- c("nbc_t", "nbc_s", "wcvi_t", "wcvi_s", "nbc_is_n", "nbc_is_s","nbc_is_t", 
                 "sbc_is_t", "sbc_is_n", "sbc_is_s", "can_term_n", "can_term_s")
  wa_cols   <- c("nfalc_t", "nfalc_s", "sfalc_t", "sfalc_s", "wac_n", "PS_n", "PS_s")
  or_cols   <- c("US_term_t", "US_term_s", "US_term_n")
  
  all_er_cols <- c(ak_cols, bc_cols, wa_cols, or_cols)
  
  ERs <- data |>
    select(year, population, er, all_of(all_er_cols)) |>
    rename(annual_er = er) |>
    mutate(
      across(all_of(all_er_cols), ~ as.numeric(.x) / 100),
      ak_er = rowSums(across(all_of(ak_cols)),  na.rm = TRUE),
      bc_er = rowSums(across(all_of(bc_cols)),  na.rm = TRUE),
      wa_er = rowSums(across(all_of(wa_cols)),  na.rm = TRUE),
      or_er = rowSums(across(all_of(or_cols)),  na.rm = TRUE)
    ) %>%
  dplyr::select(-all_of(all_er_cols))
  
  head(ERs)
  
  # What ER increased post 2019? ======
  df <- ERs |>
    mutate(
      annual_er = as.numeric(annual_er),
      period    = factor(if_else(year < 2019, "Pre-2019", "Post-2019"),
                         levels = c("Pre-2019", "Post-2019"))
    ) |>
    pivot_longer(cols = c(ak_er, bc_er, wa_er, or_er),
                 names_to = "region", values_to = "er") |>
    mutate(region = factor(toupper(str_remove(region, "_er")),
                           levels = c("AK", "BC", "WA", "OR")))
  
  region_colors <- c(AK = "#ba7517", BC = "#0f6e56", WA = "#185fa5", OR = "#a32d2d")
  
  ivline <- geom_vline(xintercept = intervention_year, linetype = "dashed",
                       color = "grey30", linewidth = 0.6)
  
  # ── PLOT 1: Time series — all regions, averaged across populations ─────────────
  # Quickest way to see which region's ER trended up post-2012
  
  p1 <- df |>
    group_by(year, region) |>
    summarise(mean_er = mean(er, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = year, y = mean_er, color = region)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    ivline +
    annotate("text", x = intervention_year + 0.3, y = Inf,
             label = "2019", hjust = 0, vjust = 1.5, size = 2.8, color = "grey40") +
    scale_color_manual(values = region_colors) +
    scale_x_continuous(breaks = seq(min(ERs$year), max(ERs$year), by = 4)) +
    labs(
      title    = "Mean exploitation rate by region over time",
      subtitle = "Averaged across all OR Chinook populations. Which region rose as AK fell?",
      x = NULL, y = "Mean exploitation rate"
    ) +
    base_theme
  p1
  
  # ── PLOT 2: Boxplots — pre vs post by region ───────────────────────────────────
  # Clean distributional comparison; leakage = non-AK boxes shift UP post-2012
  
  p2 <- df |>
    ggplot(aes(x = period, y = er, fill = period)) +
    geom_boxplot(alpha = 0.75, outlier.size = 1, linewidth = 0.4) +
    geom_jitter(width = 0.15, size = 0.6, alpha = 0.3) +
    scale_fill_manual(values = c("Pre-2019" = "#ba7517", "Post-2019" = "#a32d2d")) +
    facet_wrap(~region, scales = "free_y", nrow = 1) +
    labs(
      title    = "ER distribution by region — pre vs post 2019",
      # subtitle = "Boxes shifting UP in BC, WA, or OR post-2012 = leakage from AK reduction",
      x = NULL, y = "Exploitation rate"
    ) +
    base_theme +
    theme(legend.position = "none")
  p2
  
  # ── PLOT 3: Mean ER change — dot plot with delta ───────────────────────────────
  # Most direct answer: which regions went up, which went down, by how much?
  
  delta_df <- df |>
    group_by(region, period) |>
    summarise(mean_er = mean(er, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = period, values_from = mean_er) |>
    mutate(
      delta     = `Post-2019` - `Pre-2019`,
      direction = if_else(delta > 0, "Increased", "Decreased"),
      label     = sprintf("%+.3f", delta)
    )
  
  p3 <- delta_df |>
    ggplot(aes(x = delta, y = fct_reorder(region, delta), fill = direction)) +
    geom_col(width = 0.5, alpha = 0.85) +
    geom_text(aes(label = label,
                  hjust = if_else(delta > 0, -0.15, 1.15)),
              size = 3.5, fontface = "bold") +
    geom_vline(xintercept = 0, linewidth = 0.5, color = "grey30") +
    scale_fill_manual(values = c("Increased" = "#a32d2d", "Decreased" = "#0f6e56")) +
    scale_x_continuous(expand = expansion(mult = 0.25)) +
    labs(
      title    = "Change in mean ER post-2019 by region",
      subtitle = "Post-minus-pre mean across all populations and years",
      x = "Change in exploitation rate", y = NULL
    ) +
    base_theme +
    theme(legend.position = "none")
  p3
  
  # ── PLOT 4: Stacked area — composition of total ER over time ──────────────────
  # Shows whether total ER rose and which region's slice grew
  
  p4 <- df |>
    group_by(year, region) |>
    summarise(mean_er = mean(er, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = year, y = mean_er, fill = region)) +
    geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
    ivline +
    annotate("text", x = intervention_year + 0.3, y = Inf,
             label = "2012", hjust = 0, vjust = 1.5, size = 2.8, color = "grey40") +
    scale_fill_manual(values = region_colors) +
    scale_x_continuous(breaks = seq(min(ERs$year), max(ERs$year), by = 4)) +
    labs(
      title    = "Stacked ER by region — total exploitation composition",
      subtitle = "Growing slice for BC, WA, or OR after 2012 = those fisheries absorbed AK's reduction",
      x = NULL, y = "Exploitation rate"
    ) +
    base_theme
  p4
  # ── PLOT 5: Faceted by population — all four regions as lines ─────────────────
  # Stock-level detail: leakage patterns may differ by population
  
  p5 <- df |>
    group_by(year, population, region) |>
    summarise(er = mean(er, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = year, y = er, color = region)) +
    geom_line(linewidth = 0.7, alpha = 0.85) +
    ivline +
    scale_color_manual(values = region_colors) +
    scale_x_continuous(breaks = seq(min(ERs$year), max(ERs$year), by = 8)) +
    facet_wrap(~population, scales = "free_y") +
    labs(
      title    = "ER by region and population",
      subtitle = "Leakage may not be uniform — some stocks may be more exposed to non-AK fisheries",
      x = NULL, y = "Exploitation rate"
    ) +
    base_theme
  p5 
    
  
  # Plot P2 Box plot ER by decades =========
  max_year <- max(ERs$year)
  
  # ── PREP WITH DECADES ─────────────────────────────────────────────────────────
  df_decades <- ERs |>
    mutate(
      annual_er = as.numeric(annual_er),
      decade = case_when(
        year >= 2019               ~ paste0("2019–", max_year),
        year >= 2009 & year < 2019 ~ "2009–2018",
        year >= 1999 & year < 2009 ~ "1999–2008",
        year >= 1989 & year < 1999 ~ "1989–1998",
        year >= 1979 & year < 1989 ~ "1979–1988",
        TRUE                       ~ "Pre-1979"
      ),
      decade = factor(decade, levels = c(
        "Pre-1979", "1979–1988", "1989–1998",
        "1999–2008", "2009–2018", paste0("2019–", max_year)
      ))
    ) |>
    pivot_longer(cols = c(ak_er, bc_er, wa_er, or_er),
                 names_to = "region", values_to = "er") |>
    mutate(region = factor(toupper(str_remove(region, "_er")),
                           levels = c("AK", "BC", "WA", "OR")))
  
  # drop unused decade levels (e.g. Pre-1979 if data starts 1983)
  df_decades <- df_decades |>
    filter(!is.na(er)) |>
    mutate(decade = droplevels(decade))
  
  decade_levels <- levels(df_decades$decade)
  decade_colors <- setNames(
    colorRampPalette(c("#d4a84b", "#8b2020"))(length(decade_levels)),
    decade_levels
  )
  
  # ── P2 UPDATED: decadal boxplots faceted by region ───────────────────────────
  p2_decades <- df_decades |>
    ggplot(aes(x = decade, y = er, fill = decade)) +
    geom_boxplot(alpha = 0.8, outlier.size = 0.8, linewidth = 0.4) +
    geom_jitter(width = 0.18, size = 0.5, alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", shape = 18,
                 size = 2.5, color = "white", show.legend = FALSE) +
    scale_fill_manual(values = decade_colors) +
    scale_x_discrete(guide = guide_axis(angle = 35)) +
    facet_wrap(~region, scales = "free_y", nrow = 1) +
    labs(
      title    = "ER distribution by region — decadal comparison",
      subtitle = "10-year bins working back from 2019. Diamond = decade mean.",
      x = NULL, y = "Exploitation rate"
    ) +
    base_theme +
    theme(legend.position = "none")
  
  p2_decades
  
  
  ## P2 & 3 population specific ==========
 df <- ERs |>
    mutate(
      annual_er = as.numeric(annual_er),
      period    = factor(if_else(year < 2012, "Pre-2012", "Post-2012"),
                         levels = c("Pre-2012", "Post-2012"))
    ) |>
    pivot_longer(cols = c(ak_er, bc_er, wa_er, or_er),
                 names_to = "region", values_to = "er") |>
    mutate(region = factor(toupper(str_remove(region, "_er")),
                           levels = c("AK", "BC", "WA", "OR")))
  
  region_colors <- c(AK = "#ba7517", BC = "#0f6e56", WA = "#185fa5", OR = "#a32d2d")
   
  # ── PLOT 2 (population-specific): Boxplots faceted by population AND region ───
  # Each population gets its own row; columns are regions
  # Leakage = non-AK boxes shift UP post-2012 within a population
  
  p2_pop <- df |>
    ggplot(aes(x = period, y = er, fill = period)) +
    geom_boxplot(alpha = 0.75, outlier.size = 0.8, linewidth = 0.4) +
    geom_jitter(width = 0.15, size = 0.5, alpha = 0.3) +
    scale_fill_manual(values = c("Pre-2012" = "#ba7517", "Post-2012" = "#a32d2d")) +
    facet_grid(population ~ region, scales = "free_y") +
    labs(
      title    = "ER distribution by region and population — pre vs post 2012",
      subtitle = "Rows = populations, columns = regions. Upward shift in BC/WA/OR = leakage for that stock.",
      x = NULL, y = "Exploitation rate"
    ) +
    base_theme +
    theme(
      legend.position  = "bottom",
      axis.text.x      = element_text(size = 8, angle = 15, hjust = 1),
      strip.text.y     = element_text(angle = 0, hjust = 0)  # horizontal population labels
    )
  
  print(p2_pop)
  
  # ── PLOT 3 (population-specific): Delta bar chart faceted by population ────────
  # Shows direction and magnitude of change per region, per population
  
  delta_pop <- df |>
    group_by(population, region, period) |>
    summarise(mean_er = mean(er, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = period, values_from = mean_er) |>
    mutate(
      delta     = `Post-2012` - `Pre-2012`,
      direction = if_else(delta > 0, "Increased", "Decreased"),
      label     = sprintf("%+.3f", delta)
    )
  
  p3_pop <- delta_pop |>
    ggplot(aes(x = delta, y = fct_reorder(region, delta), fill = direction)) +
    geom_col(width = 0.6, alpha = 0.85) +
    geom_text(aes(label = label,
                  hjust = if_else(delta > 0, -0.15, 1.15)),
              size = 2.8, fontface = "bold") +
    geom_vline(xintercept = 0, linewidth = 0.5, color = "grey30") +
    scale_fill_manual(values = c("Increased" = "#a32d2d", "Decreased" = "#0f6e56")) +
    scale_x_continuous(expand = expansion(mult = 0.3)) +
    facet_wrap(~population, scales = "free_x") +
    labs(
      title    = "Change in mean ER post-2012 by region and population",
      subtitle = "Post-minus-pre mean. Red = increased, green = decreased. AK should be green; leakage = other regions red.",
      x = "Change in exploitation rate", y = NULL
    ) +
    base_theme +
    theme(legend.position = "none")
  
  print(p3_pop)

  ## P2 P3 population and decade specific =======
 
 df_decades_pop <- ERs |>
    mutate(
      annual_er = as.numeric(annual_er),
      decade = case_when(
        year >= 2019               ~ paste0("2019–", max_year),
        year >= 2009 & year < 2019 ~ "2009–2018",
        year >= 1999 & year < 2009 ~ "1999–2008",
        year >= 1989 & year < 1999 ~ "1989–1998",
        year >= 1979 & year < 1989 ~ "1979–1988",
        TRUE                       ~ "Pre-1979"
      ),
      decade = factor(decade, levels = c(
        "Pre-1979", "1979–1988", "1989–1998",
        "1999–2008", "2009–2018", paste0("2019–", max_year)
      ))
    ) |>
    pivot_longer(cols = c(ak_er, bc_er, wa_er, or_er),
                 names_to = "region", values_to = "er") |>
    mutate(region = factor(toupper(str_remove(region, "_er")),
                           levels = c("AK", "BC", "WA", "OR"))) |>
    filter(!is.na(er)) |>
    mutate(decade = droplevels(decade))
  
  decade_levels <- levels(df_decades_pop$decade)
  decade_colors <- setNames(
    colorRampPalette(c("#d4a84b", "#8b2020"))(length(decade_levels)),
    decade_levels
  )
  
  region_colors <- c(AK = "#ba7517", BC = "#0f6e56", WA = "#185fa5", OR = "#a32d2d")
  
  # ── P2_POP: grid of population × region, x = decade ─────────────────────────
  p2_pop_decades <- df_decades_pop |>
    ggplot(aes(x = decade, y = er, fill = decade)) +
    geom_boxplot(alpha = 0.8, outlier.size = 0.6, linewidth = 0.35) +
    geom_jitter(width = 0.15, size = 0.4, alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", shape = 18,
                 size = 2, color = "white", show.legend = FALSE) +
    scale_fill_manual(values = decade_colors) +
    scale_x_discrete(guide = guide_axis(angle = 40)) +
    facet_grid(region ~ population, scales = "free_y") +
    labs(
      title    = "ER distribution by region and population — decadal comparison",
      subtitle = "Rows = populations, columns = regions. Diamond = decade mean. Rising trend in BC/WA/OR = leakage.",
      x = NULL, y = "Exploitation rate"
    ) +
    base_theme +
    theme(
      legend.position = "none",
      strip.text.y    = element_text(angle = 0, hjust = 0),
      axis.text.x     = element_text(size = 7)
    )
  
  print(p2_pop_decades)
  
  # ── P3_POP: delta between consecutive decades, faceted by population ──────────
  # Compute mean ER per population × region × decade, then diff between adjacent decades
  
  delta_pop_decades <- df_decades_pop |>
    group_by(population, region, decade) |>
    summarise(mean_er = mean(er, na.rm = TRUE), .groups = "drop") |>
    arrange(population, region, decade) |>
    group_by(population, region) |>
    mutate(
      delta     = mean_er - lag(mean_er),
      direction = if_else(delta > 0, "Increased", "Decreased"),
      label     = sprintf("%+.3f", delta),
      period    = paste0(lag(as.character(decade)), " → ", as.character(decade))
    ) |>
    filter(!is.na(delta))
  
  p3_pop_decades <- delta_pop_decades |>
    ggplot(aes(x = delta, y = fct_reorder(region, delta), fill = direction)) +
    geom_col(width = 0.6, alpha = 0.85) +
    geom_text(aes(label = label,
                  hjust = if_else(delta > 0, -0.1, 1.1)),
              size = 2.3, fontface = "bold") +
    geom_vline(xintercept = 0, linewidth = 0.5, color = "grey30") +
    scale_fill_manual(values = c("Increased" = "#a32d2d", "Decreased" = "#0f6e56")) +
    scale_x_continuous(expand = expansion(mult = 0.35)) +
    facet_grid(population ~ period, scales = "free_x") +
    labs(
      title    = "Decadal change in mean ER by region and population",
      subtitle = "Change between consecutive decades. Red = increased, green = decreased.",
      x = "Change in exploitation rate", y = NULL
    ) +
    base_theme +
    theme(
      legend.position = "none",
      strip.text.x    = element_text(size = 7, angle = 20, hjust = 1),
      strip.text.y    = element_text(angle = 0, hjust = 0, size = 8)
    )
  
  print(p3_pop_decades)
   
  
  # Look at ERs through time, total and AK ============
  df <- ERs |>
    mutate(
      annual_er = as.numeric(annual_er),
      period    = factor(if_else(year < 2019, "Pre-2019", "Post-2019"),
                         levels = c("Pre-2019", "Post-2019"))
    )
  
  # ── PLOT 1: Total annual ER through time, faceted by population ───────────────
  p_total <- df |>
    ggplot(aes(x = year, y = annual_er, fill = period)) +
    geom_col(width = 0.8, alpha = 0.85) +
    geom_vline(xintercept = intervention_year - 0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.6) +
    scale_fill_manual(values = c("Pre-2019" = "#ba7517", "Post-2019" = "#a32d2d")) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 8)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    facet_wrap(~population, scales = "free_y") +
    labs(
      title    = "Total annual exploitation rate by population",
      subtitle = "Dashed line = 2019 AK catch reduction",
      x = NULL, y = "Total annual ER"
    ) +
    base_theme
  
  # ── PLOT 2: AK ER through time, faceted by population ────────────────────────
  p_ak <- df |>
    ggplot(aes(x = year, y = ak_er, fill = period)) +
    geom_col(width = 0.8, alpha = 0.85) +
    geom_vline(xintercept = intervention_year - 0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.6) +
    scale_fill_manual(values = c("Pre-2019" = "#ba7517", "Post-2019" = "#a32d2d")) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 8)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    facet_wrap(~population, scales = "free_y") +
    labs(
      title    = "AK exploitation rate by population",
      subtitle = "Dashed line = 2019 AK catch reduction",
      x = NULL, y = "AK ER"
    ) +
    base_theme
 
  p_er_individ <- df |>
    select(-annual_er) |>
    gather(c(3:6), key = "fishing_ER_ID", value = "value") |>
    ggplot(aes(x = year, y = value, fill = period)) +
    geom_col(width = 0.8, alpha = 0.85) +
    geom_vline(xintercept = intervention_year - 0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.6) +
    scale_fill_manual(values = c("Pre-2019" = "#ba7517", "Post-2019" = "#a32d2d")) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 8)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    facet_grid(population~fishing_ER_ID, scales = "free_y") +
    labs(
      title    = "Exploitation rate by population and region",
      subtitle = "Dashed line = 2019 renegotiation",
      x = NULL, y = "ER"
    ) +
    base_theme
  
  print(p_total)
  print(p_ak)
  print(p_er_individ)
  
   # look at 12% differences=========
 
  # ── PREP ──────────────────────────────────────────────────────────────────────
  df <- ERs |>
    mutate(
      annual_er = as.numeric(annual_er),
      period    = factor(if_else(year < 2019, "Pre-2019", "Post-2019"),
                         levels = c("Pre-2019", "Post-2019"))
    )
    
  # ── REFERENCE LINES — computed per population ─────────────────────────────────
  # These are passed to geom_segment so lines only span the relevant period
 
  # ── REFERENCE LINES — based only on the decade immediately before 2019 ────────
  # Pre mean = mean of 2009–2018 only (not full timeseries)
  # Target   = that decade mean * 0.88
  
  make_ref_lines <- function(data, er_col) {
    
    pre_decade <- data |>
      filter(year >= 2009, year <= 2018) |>
      group_by(population) |>
      summarise(pre_mean = mean({{ er_col }}, na.rm = TRUE), .groups = "drop")
    
    post_mean <- data |>
      filter(year >= 2019) |>
      group_by(population) |>
      summarise(post_mean = mean({{ er_col }}, na.rm = TRUE), .groups = "drop")
    
    pre_decade |>
      left_join(post_mean, by = "population") |>
      mutate(
        target_12pct = pre_mean * (1 - 0.12),
        x_pre_start  = 2009,
        x_pre_end    = 2018,
        x_post_start = 2019,
        x_post_end   = max(data$year)
      )
  }
  
  ref_total <- make_ref_lines(df, annual_er)
  ref_ak    <- make_ref_lines(df, ak_er)
  
  # ── PLOTS ─────────────────────────────────────────────────────────────────────
  p_total <- ggplot() +
    geom_col(data = df,
             aes(x = year, y = annual_er, fill = period),
             width = 0.8, alpha = 0.75) +
    geom_vline(xintercept = intervention_year - 0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.6) +
    # pre-2019 mean (2009-2018 only)
    geom_segment(data = ref_total,
                 aes(x = x_pre_start, xend = x_pre_end,
                     y = pre_mean, yend = pre_mean),
                 color = "#ba7517", linewidth = 1, linetype = "solid") +
    # post-2019 mean
    geom_segment(data = ref_total,
                 aes(x = x_post_start, xend = x_post_end,
                     y = post_mean, yend = post_mean),
                 color = "#a32d2d", linewidth = 1, linetype = "solid") +
    scale_fill_manual(values = c("Pre-2019" = "#ba7517", "Post-2019" = "#a32d2d")) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 8)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    facet_wrap(~population, scales = "free_y") +
    labs(
      title    = "Total annual exploitation rate by population",
      subtitle = "Amber line = 2009–2018 mean  |  Red line = post-2019 mean",
      x = NULL, y = "Total annual ER"
    ) +
    base_theme
  
  p_total
  
  p_ak <- ggplot() +
    geom_col(data = df,
             aes(x = year, y = ak_er, fill = period),
             width = 0.8, alpha = 0.75) +
    geom_vline(xintercept = intervention_year - 0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.6) +
    # pre-2019 mean (2009-2018 only)
    geom_segment(data = ref_ak,
                 aes(x = x_pre_start, xend = x_pre_end,
                     y = pre_mean, yend = pre_mean),
                 color = "#ba7517", linewidth = 1, linetype = "solid") +
    # post-2019 mean
    geom_segment(data = ref_ak,
                 aes(x = x_post_start, xend = x_post_end,
                     y = post_mean, yend = post_mean),
                 color = "#a32d2d", linewidth = 1, linetype = "solid") +
    # 12% reduction target off 2009-2018 mean
    geom_segment(data = ref_ak,
                 aes(x = x_post_start, xend = x_post_end,
                     y = target_12pct, yend = target_12pct),
                 color = "#185fa5", linewidth = 1, linetype = "solid") +
    scale_fill_manual(values = c("Pre-2019" = "#ba7517", "Post-2019" = "#a32d2d")) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 8)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    facet_wrap(~population, scales = "free_y") +
    labs(
      title    = "AK exploitation rate by population",
      subtitle = "Amber line = 2009–2018 mean  |  Red line = post-2019 mean  |  Blue = 12% reduction target",
      x = NULL, y = "AK ER"
    ) +
    base_theme
  
  p_ak
   
  # Look at total run size, total catch, and AK Catch. =====
  # what is the change in total catch through time, and AKs contribution to that, has AK total catch decreased? and what is a 12% total catch decrease look like?
 ak_cols <- c("seak_t", "seak_s", "seak_n", "ak_term_t", "ak_term_n", "ak_term_s")
  
  catch_df <- data |>
    mutate(
      across(all_of(ak_cols), ~ as.numeric(.x) / 100),
      across(c(total_run, esc_tot, mar.catch, term.catch), as.numeric),
      ak_er        = rowSums(across(all_of(ak_cols)), na.rm = TRUE),
      total_catch  = mar.catch + term.catch,
      ak_catch     = ak_er * total_run,
      non_ak_catch = total_catch - ak_catch,
      decade = case_when(
        year >= 2019               ~ paste0("2019–", max(year)),
        year >= 2009 & year < 2019 ~ "2009–2018",
        year >= 1999 & year < 2009 ~ "1999–2008",
        year >= 1989 & year < 1999 ~ "1989–1998",
        year >= 1979 & year < 1989 ~ "1979–1988",
        TRUE                       ~ "Pre-1979"
      ),
      decade = factor(decade, levels = c(
        "Pre-1979", "1979–1988", "1989–1998",
        "1999–2008", "2009–2018", paste0("2019–", max(year))
      )),
      period = factor(if_else(year < 2019, "Pre-2019", "Post-2019"),
                      levels = c("Pre-2019", "Post-2019"))
    ) |>
    mutate(decade = droplevels(decade))
  
  intervention_year <- 2019
  
  decade_levels <- levels(catch_df$decade)
  decade_colors <- setNames(
    colorRampPalette(c("#d4a84b", "#8b2020"))(length(decade_levels)),
    decade_levels
  )
  
   
  x_breaks <- seq(min(catch_df$year), max(catch_df$year), by = 8)
  
  # ── REFERENCE LINES per population — 2009-2018 baseline ──────────────────────
  make_catch_refs <- function(data, catch_col) {
    pre <- data |>
      filter(year >= 2009, year <= 2018) |>
      group_by(population) |>
      summarise(pre_mean = mean({{ catch_col }}, na.rm = TRUE), .groups = "drop")
    
    post <- data |>
      filter(year >= 2019) |>
      group_by(population) |>
      summarise(post_mean = mean({{ catch_col }}, na.rm = TRUE), .groups = "drop")
    
    pre |>
      left_join(post, by = "population") |>
      mutate(
        target_12pct = pre_mean * 0.88,
        x_pre_start  = 2009,
        x_pre_end    = 2018,
        x_post_start = intervention_year,
        x_post_end   = max(data$year)
      )
  }
  
  ref_total <- make_catch_refs(catch_df, total_catch)
  ref_ak    <- make_catch_refs(catch_df, ak_catch)
  ref_run   <- make_catch_refs(catch_df, total_run)
  
  # ── PLOT 1: Total run size by population ──────────────────────────────────────
  p_run <- ggplot() +
    geom_col(data = catch_df,
             aes(x = year, y = total_run / 1000, fill = period),
             width = 0.8, alpha = 0.8) +
    geom_vline(xintercept = intervention_year - 0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.6) +
    geom_segment(data = ref_run,
                 aes(x = x_pre_start, xend = x_pre_end,
                     y = pre_mean / 1000, yend = pre_mean / 1000),
                 color = "#ba7517", linewidth = 1) +
    geom_segment(data = ref_run,
                 aes(x = x_post_start, xend = x_post_end,
                     y = post_mean / 1000, yend = post_mean / 1000),
                 color = "#a32d2d", linewidth = 1) +
    scale_fill_manual(values = c("Pre-2019" = "#ba7517", "Post-2019" = "#a32d2d")) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~population, scales = "free_y") +
    labs(
      title    = "Total run size by population",
      subtitle = "Amber line = 2009–2018 mean  |  Red line = post-2019 mean",
      x = NULL, y = "Total run (thousands of fish)"
    ) +
    base_theme
  p_run
  
  # ── PLOT 2: Total catch stacked AK vs non-AK by population ───────────────────
  catch_long <- catch_df |>
    select(year, population, period, ak_catch, non_ak_catch) |>
    pivot_longer(cols = c(ak_catch, non_ak_catch),
                 names_to = "fishery", values_to = "catch") |>
    mutate(fishery = recode(fishery,
                            "ak_catch"     = "AK catch",
                            "non_ak_catch" = "Non-AK catch"))
  
  p_catch_stack <- ggplot() +
    geom_col(data = catch_long,
             aes(x = year, y = catch / 1000, fill = fishery),
             width = 0.8, alpha = 0.85) +
    geom_vline(xintercept = intervention_year - 0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.6) +
    geom_segment(data = ref_total,
                 aes(x = x_pre_start, xend = x_pre_end,
                     y = pre_mean / 1000, yend = pre_mean / 1000),
                 color = "#ba7517", linewidth = 1) +
    geom_segment(data = ref_total,
                 aes(x = x_post_start, xend = x_post_end,
                     y = post_mean / 1000, yend = post_mean / 1000),
                 color = "#a32d2d", linewidth = 1) +
    # geom_segment(data = ref_total,
    #              aes(x = x_post_start, xend = x_post_end,
    #                  y = target_12pct / 1000, yend = target_12pct / 1000),
    #              color = "#185fa5", linewidth = 1, linetype = "dashed") +
    scale_fill_manual(values = c("AK catch" = "#ba7517", "Non-AK catch" = "#8fb8d4")) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~population, scales = "free_y") +
    labs(
      title    = "Total catch — AK vs non-AK by population",
      subtitle = "Amber = 2009–2018 mean  |  Red = post-2019 mean ",
      x = NULL, y = "Catch (thousands of fish)"
    ) +
    base_theme
  p_catch_stack
  
  # ── PLOT 3: AK catch only by population ───────────────────────────────────────
  p_ak_catch <- ggplot() +
    geom_col(data = catch_df,
             aes(x = year, y = ak_catch / 1000, fill = period),
             width = 0.8, alpha = 0.8) +
    geom_vline(xintercept = intervention_year - 0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.6) +
    geom_segment(data = ref_ak,
                 aes(x = x_pre_start, xend = x_pre_end,
                     y = pre_mean / 1000, yend = pre_mean / 1000),
                 color = "#ba7517", linewidth = 1) +
    geom_segment(data = ref_ak,
                 aes(x = x_post_start, xend = x_post_end,
                     y = post_mean / 1000, yend = post_mean / 1000),
                 color = "#a32d2d", linewidth = 1) +
    geom_segment(data = ref_ak,
                 aes(x = x_post_start, xend = x_post_end,
                     y = target_12pct / 1000, yend = target_12pct / 1000),
                 color = "#185fa5", linewidth = 1, linetype = "dashed") +
    scale_fill_manual(values = c("Pre-2019" = "#ba7517", "Post-2019" = "#a32d2d")) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~population, scales = "free_y") +
    labs(
      title    = "AK catch of OR Chinook by population",
      subtitle = "Amber = 2009–2018 mean  |  Red = post-2019 mean  |  Blue dashed = 12% reduction target",
      x = NULL, y = "AK catch (thousands of fish)"
    ) +
    base_theme
  
  p_ak_catch
  
  # ── PLOT 4: AK share of total catch by population ─────────────────────────────
  catch_df <- catch_df |>
    mutate(ak_share_catch = ak_catch / total_catch)
  
  ref_share <- make_catch_refs(catch_df, ak_share_catch)
  
  p_ak_share <- ggplot() +
    geom_col(data = catch_df,
             aes(x = year, y = ak_share_catch, fill = period),
             width = 0.8, alpha = 0.8) +
    geom_vline(xintercept = intervention_year - 0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.6) +
    geom_segment(data = ref_share,
                 aes(x = x_pre_start, xend = x_pre_end,
                     y = pre_mean, yend = pre_mean),
                 color = "#ba7517", linewidth = 1) +
    geom_segment(data = ref_share,
                 aes(x = x_post_start, xend = x_post_end,
                     y = post_mean, yend = post_mean),
                 color = "#a32d2d", linewidth = 1) +
    scale_fill_manual(values = c("Pre-2019" = "#ba7517", "Post-2019" = "#a32d2d")) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~population, scales = "free_y") +
    labs(
      title    = "AK share of total catch by population",
      subtitle = "Amber = 2009–2018 mean  |  Red = post-2019 mean",
      x = NULL, y = "AK catch / total catch"
    ) +
    base_theme
  
  p_ak_share
  # ── PLOT 5: Decadal boxplots — AK and total catch by population ───────────────
  p_decade_box <- catch_df |>
    pivot_longer(cols = c(total_catch, ak_catch),
                 names_to = "type", values_to = "catch") |>
    mutate(type = recode(type,
                         "total_catch" = "Total catch",
                         "ak_catch"    = "AK catch")) |>
    ggplot(aes(x = decade, y = catch / 1000, fill = decade)) +
    geom_boxplot(alpha = 0.8, outlier.size = 0.7, linewidth = 0.35) +
    geom_jitter(width = 0.15, size = 0.5, alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", shape = 18,
                 size = 2.5, color = "white", show.legend = FALSE) +
    scale_fill_manual(values = decade_colors) +
    scale_x_discrete(guide = guide_axis(angle = 35)) +
    scale_y_continuous(labels = scales::comma) +
    facet_grid(population ~ type, scales = "free_y") +
    labs(
      title    = "Catch distributions by decade and population",
      subtitle = "Diamond = decade mean. Has catch trended down within each stock?",
      x = NULL, y = "Catch (thousands of fish)"
    ) +
    base_theme +
    theme(
      legend.position = "none",
      strip.text.y    = element_text(angle = 0, hjust = 0)
    )
  p_decade_box
 
  # look at ER and spawning abundances ===========
  data %>% 
    dplyr::select(year, population, esc_tot) %>%
    right_join(OC_plot_df) %>%
    filter(broad_region == "Alaska") %>%
    ggplot( ) + 
    geom_line(aes(x=year, y = mort_broad_region)) + 
    geom_point(aes(x=year, y = mort_broad_region)) + 
    geom_bar(aes(x=year, y = as.numeric(esc_tot)), stat = "identity") + 
    facet_wrap(~population, scales= "free")+
    labs(x="Year", y ="Harvest Numbers", title = "Alaskan Harvest of OR Chinook salmon") 
  
  
  
  
  
  
  
  
  
  
  

  
