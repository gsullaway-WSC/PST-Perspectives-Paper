library(here)
library(tidyverse)
 
library(readxl)
library(viridis)
library(cowplot) 
library(ggrepel)

# Source: CTC Escapement TCC document 2026, table 3.2 for list of indicator stocks  
# document to summarize data for WSC PSC Reform Communication document

# GSI ============
## 1. What % of PST fisheries use GSI data? ========
# (not totally sure where/how to get this info? Perhaps just from reading CTC reports?) 

# Escapement Goals ====== 
## Chinook - Pinsky data ====== 
# How many Chinook stocks are there in North America?
# Pinksy et al 2009, remove stocks not in PST region (ie russie japan) and 
# keep only distinct rivers, some naming inconsistencies,
# and rows that represent different data sources but one population 
# this is a list of names of rivers within the PST Region filtred from the complete Pinsky dataset 
pst_region_names <- read_csv("data/Pinksy et al Data/Pinsky_TakuToEel_names.csv") %>%
  dplyr::mutate(id = "pst") %>%
  dplyr::rename(Name = "unique.total_stocks.Name.") 
 
 # change the filtering so the row can only stay if there is some sort of data input, 
# indicating data collection and that a stock actually exists or has existed in that region.  
total_Chinook_stocks <- read_csv("data/Pinksy et al Data/Pinsky_etal_AbundanceDatabase2008.csv") %>%
  left_join(pst_region_names) %>%
  filter(Spp == "Chinook") %>%
  filter(!is.na(id),
         # filter out rows where there was no input data, sometimes Pinksy estiamted run size based on neighboring runs, but if there is zero data I dont want to include those, then no confirmation there is a species there
         !is.na(AbundAve) | !is.na(EscAve) | !is.na(CatchAve) | !is.na(HarvestRate), 
         !grepl("expanded", Name, ignore.case = TRUE)) %>%
  dplyr::mutate(Name = case_when(  
    grepl("adam", Name, ignore.case = TRUE) ~ "ADAMS RIVER",
    grepl("ANDERSON", Name, ignore.case = TRUE) ~ "ANDERSON RIVER",
    grepl("battle creek", Name, ignore.case = TRUE) ~ "Battle Creek",
    grepl("bedwell", Name, ignore.case = TRUE) ~ "BEDWELL", 
    grepl("BULKLEY", Name, ignore.case = TRUE) ~ "BULKLEY RIVER",  
    grepl("BABINE", Name, ignore.case = TRUE) ~ "BABINE RIVER",  
    grepl("CANYON", Name, ignore.case = TRUE) ~ "CANYON CREEK",
    grepl("Canyon", Name, ignore.case = TRUE) ~ "CANYON CREEK",
    grepl("CANTON", Name, ignore.case = TRUE) ~ "CANYON CREEK", 
    grepl("COW", Name, ignore.case = TRUE) ~ "COW CREEK", 
    grepl("CLEAR CREEK", Name, ignore.case = TRUE) ~ "CLEAR CREEK", 
    grepl("CAYUSE", Name, ignore.case = TRUE) ~ "CAYUSE RIVER",  
    grepl("CAYEGHLE", Name, ignore.case = TRUE) ~ "CAYEGHLE RIVER",  
    grepl("CEDAR", Name, ignore.case = TRUE) ~ "CEDAR RIVER",  
    grepl("CLEARWATER", Name, ignore.case = TRUE) ~ "CLEARWATER RIVER", 
    grepl("DESERTED", Name, ignore.case = TRUE) ~ "DESERTED RIVER", 
    grepl("FRENCH", Name, ignore.case = TRUE) ~ "FRENCH RIVER",  
    grepl("Grays River piece - Grays River", Name, ignore.case = TRUE) ~ "GRAYS RIVER",  
    grepl("Green", Name, ignore.case = TRUE) ~ "GREEN RIVER",  
    grepl("Mad", Name, ignore.case = TRUE) ~ "MAD RIVER",  
    grepl("MAMQUAM", Name, ignore.case = TRUE) ~ "MAMQUAM RIVER",  
    grepl("Salmon River", Name, ignore.case = TRUE) ~ "SALMON RIVER",  
    TRUE ~ Name )) %>%
  distinct(Name,Spp,Runtiming)#, AbundAve,EscAve,CatchAve, HarvestRate) 

nrow(total_Chinook_stocks)

## 2. Have EG ========
# What % of PST fisheries have escapement goals (and how recent are they)
# summaries of Y N if it has PSC goal or not
esc_summary_plain <- read_csv("data/PST_Chinook_Esc_goal_summary.csv")   

esc_summary <- esc_summary_plain %>%
  count(has_psc_escapement_goal) %>%
  dplyr::mutate(has_psc_escapement_goal = case_when(has_psc_escapement_goal == "No" ~ "No PSC Goal",
                                         has_psc_escapement_goal == "Yes" ~ "PSC-Agreed Goal")) %>%
  dplyr::mutate(percent = n/sum(n),
                cum_pos = sum(n) - cumsum(n) + n / 2, 
                label = paste0(has_psc_escapement_goal, "\n", round(percent * 100, 1), "%"))  # combined label

custom_pal <- c(
  "No PSC Goal" = "#2D6E7E",    
  "PSC-Agreed Goal"   = "#7FA8B8"
)

Chinook_pie <- ggplot(esc_summary, 
                      aes(x = "", y = n, fill = has_psc_escapement_goal)) +
  geom_col(color = "black", alpha = 0.9, width = 1) +
  
  # Labels inside for large slices
  geom_text(aes(y = cum_pos,
                label = label),  #ifelse(avg_mort >= 0.05, paste0(label), "")),
            size = 4, fontface = "bold", color = "black") +
  # Labels outside for small slices — nudge x past 1 to push outside pie
  # geom_text(aes(y = cum_pos,
  #               label = ifelse(avg_mort < 0.05 & avg_mort > 0.001, paste0(label), "")),
  #           x = 1.63,   # >1 pushes outside the pie (pie lives at x = 1)
  #           size = 3, fontface = "bold", color = "black") + 
  coord_polar(theta = "y", clip = "off") +   
  scale_fill_manual(values = custom_pal, name = " ") +  
  theme_void() +
  ggtitle("PSC Chinook salmon stocks with Escapement Goals") + 
  theme(plot.title = element_text( face = "bold",hjust = 0.5),
    legend.position   = "none",
    legend.title      = element_text(size = 10, face = "bold"),
    legend.text       = element_text(size = 9),
    legend.margin     = margin(t = 10, b = -10, unit = "mm"),  # positive t pushes down, negative b pulls pie up
    # legend.key.size   = unit(0.4, "cm"),   # smaller legend keys
    legend.spacing.x  = unit(0.2, "cm"),   # tighten horizontal spacing
    plot.margin       = margin(0, 5, 0, 5, "mm"),  # reduce outer margins
    plot.background   = element_blank()
  )

Chinook_pie

ggsave(
  filename = "output/plots/Chinook_Esc_Goal_Summary_Pie.jpeg",
  plot = Chinook_pie,
  width =6,
  height =5.5,
  dpi = 300,
  units = "in"
)

## 3. Not meeting EG ========
# Summary of how many stocks haven't met escapement goals in the last XX years
# Time point summary: 2009 - 2020
# this has esc goals and escapement counts
# of the stocks that have escapement goals, how often are the goals met? 
esc_dat_full <- read_csv("data/pst_chinook_escapement.csv") %>% 
  rename(esc_name_link = "stock") %>% 
  left_join(esc_summary_plain, by = c("esc_name_link", "species"))  

esc_dat <- esc_dat_full  %>%
  filter(has_psc_escapement_goal == "Yes",
         year >2008)  
 
esc_dat_stock <- esc_dat %>%
  group_by(stock) %>%
  arrange(year, .by_group = TRUE) %>%
  dplyr::mutate(esc_goal_average = (escapement_goal_upper+escapement_goal_lower)/2,
         esc_flag = case_when(escapement_count < esc_goal_average ~ "Goal Not Met",
                              TRUE ~ "Goal Met")) %>%
  dplyr::summarise(
    esc_goal_met              = sum(esc_flag == "Goal Met", na.rm = TRUE),
    esc_goal_not_met          = sum(esc_flag == "Goal Not Met", na.rm = TRUE)) %>% 
  gather(c(2:3) , key = "id", value = "count") %>% 
  group_by(stock) %>%
  mutate(pct = count / sum(count) * 100)
 
esc_dat_summary <- esc_dat %>%
  group_by(stock) %>%
  arrange(year, .by_group = TRUE) %>%
  dplyr::mutate(esc_goal_average = (escapement_goal_upper+escapement_goal_lower)/2,
                esc_flag = case_when(escapement_count < esc_goal_average ~ "Goal Not Met",
                                     TRUE ~ "Goal Met")) %>%
  ungroup() %>% 
  dplyr::summarise(
    esc_goal_met              = sum(esc_flag == "Goal Met", na.rm = TRUE),
    esc_goal_not_met          = sum(esc_flag == "Goal Not Met", na.rm = TRUE))  %>%
  gather(c(1:2) , key = "id", value = "count") %>% 
  mutate(pct = count / sum(count) * 100)
   
## 4. EG Updates ===========
# frequency of escapement goal updates 
esc_summary_count  <- esc_summary_plain %>%
  filter(has_psc_escapement_goal=="Yes")
 
df <- as.data.frame(unique(esc_summary_count$esc_name_link)) %>%
  arrange(`unique(esc_summary_count$esc_name_link)`)

eg_update_years <- tribble(
  ~esc_name_link,                              ~last_EG_update,
  # SE Alaska
  "Situk River",                       1998,   # revised 1997, CTC accepted 1998
  "Chilkat River",                     2004,   # CTC accepted 2004
  "Unuk River",                        2009,   # CTC accepted 2009
  # Transboundary
  "Alsek River",                       2010,   # PSC accepted 2010
  # "Alsek River (drainagewide)",        2010,   # same stock, same goal
  "Taku River",                        2010,   # updated BEG, PSC agreed 2010
  "Stikine River",                     2000,   # CTC/PSC accepted 2000
  # Canadian - with PSC goals
  "Atnarko River",                     2014,   # Vélez-Espino et al. 2014, PSC Attachment I 2019
  "Cowichan",                    2005,   # CTC accepted 2005
  "Harrison",                    2001,   # CTC accepted 2001
  "Lower Shuswap",                     2006,   # Parken et al. 2006, carried into 2019 Attachment I
  # Canadian - no PSC goal
  "NWVI 4-Stream Index" ,              NA, 
  "SWVI 3-Stream Index",                 NA,
  "East Vancouver Island North",       NA, 
  "Canadian Okanagan",                 NA, 
  # "Nass River",                        NA,
  "Skeena River",                      NA,
  "Chilko",                      NA,
  "Lower Chilcotin",                   NA,
  "Nicola",                      NA,
  "Phillips",                    NA,
  # "Fraser Spring (age 1.2)",           NA,
  # "Fraser Spring (age 1.3)",           NA,
  # "Fraser Summer (age 0.3)",           NA,
  # "Fraser Summer (age 1.3)",           NA,
  # Puget Sound - with PSC goals
  "Skagit River Spring",                     2024,   # CTC revised and accepted September 2024
  "Skagit River Summer/Fall",                2024,   # CTC revised and accepted September 2024
  # Puget Sound - no PSC goal
  "Nooksack Spring",                   NA,
  "Stillaguamish River",               NA,
  "Snohomish River",                   NA,
  # "Lake Washington",                   NA,
  # "Green River",                       NA,
  # Washington Coast - with PSC goals
  "Quillayute Fall",                   2004,   # CTC accepted 2004
  # "Hoh Spring/Summer",                 2004,   # CTC accepted 2004
  "Hoh Fall",                          2004,   # CTC accepted 2004
  # "Queets Spring/Summer",              2004,   # CTC accepted 2004
  "Queets Fall",                       2004,   # CTC accepted 2004
  "Grays Harbor Fall",                 2014,   # CTC accepted 2014
  # Washington Coast - no PSC goal
  "Hoko Fall",                         NA,
  # "Quillayute Summer",                 NA,
  # "Grays Harbor Spring",               NA,
  # Columbia River - with PSC goals
  "Columbia Upriver Brights",          2002,# PSC agreed 2002 (Columbia River Fish Management Plan)
   "Mid-Columbia Summers",             1999,   # CTC 1999 interim goal
  "Lewis River",                       1999,   # CTC accepted 1999
   # Columbia River - no PSC goal
  "Coweeman",                    NA,
  # Oregon Coast - with PSC goals
  "Nehalem River",         1999,   # Zhou and Williams 1999
  # "Nehalem River (Oregon Coast MR)",   1999,
   "Siletz River",          2000,   # Zhou and Williams 2000
  # "Siletz River (Oregon Coast MR)",    2000,
  "Siuslaw River",         2000,   # Zhou and Williams 2000
  # "Siuslaw River (Oregon Coast MR)",   2000,
  # Oregon Coast - no PSC goal
  "Umpqua River S. Fork",                    NA,
  "Coquille River",                    NA
)

esc_summary_with_years <- esc_summary_plain %>%
  left_join(eg_update_years, by = "esc_name_link") %>% 
  filter(!is.na(last_EG_update)) %>%
  dplyr::mutate(years_since_update = 2024 - last_EG_update) %>% 
  mutate(esc_name_link = str_remove(esc_name_link, " River"))
  

 frequency_eg_plot <-  ggplot(data = esc_summary_with_years, 
                              aes(x = years_since_update, 
                                  y = reorder(esc_name_link, years_since_update))) +
  geom_col(fill ="#5B9BAD") +
  geom_vline(xintercept = 10, linetype = "dashed", colour = "grey40") +
  annotate("text", x = 10.5, y = 1, label = "10 yr threshold",
           hjust = 0, size = 3, colour = "grey40") +
  labs(x = "Years Since Last Goal Update (as of 2024)",
       y = NULL,
       title = "How Outdated Are Current PSC Escapement Goals?") +
  theme_minimal()
 
 frequency_eg_plot
 
 ggsave(
   filename = "output/plots/Chinook_Esc_Goal_Update_Freq.jpeg",
   plot = frequency_eg_plot,
   width =6,
   height =5.5,
   dpi = 300,
   units = "in"
 )
 
 # All Chinook -- Nested Donut plot =======  
 ### 1. Set Numbers =========
 total_stocks     <- nrow(total_Chinook_stocks)   # total PST stocks (center ring)
 indicator_stocks <- sum(esc_summary$n)
 temp <- esc_summary %>% filter(has_psc_escapement_goal == "PSC-Agreed Goal")   # stocks that have an escapement goal
 stocks_with_goal <- as.numeric(temp[1,2])
 pst_meet_goal <- as.numeric(esc_dat_summary[1,3])/100 #0.60   # e.g. 60% of years stocks met their goal 
 
 ###   2. Build the data frame ==========
 df <- bind_rows(
   tibble(
     ring  = 0,
     label = c("Total Stocks"),
     value = c(0),
     fill  = c("#2D5F6E")
   ),
   
   # Ring 1 (innermost) — indicator stocks vs. non-indicator
   tibble(
     ring  = 1,
     label = c("Indicator", "Non-indicator"),
     value = c(indicator_stocks, total_stocks - indicator_stocks),
     fill  = c("#5B9BAD", "#D3D1C7")
   ),
   
   # Ring 2 (middle) — have escapement goal vs. no goal
   tibble(
     ring  = 2,
     label = c("Have goal", "No goal", "indicator (pad)"),
     value = c(stocks_with_goal, indicator_stocks-stocks_with_goal, total_stocks - indicator_stocks),
     fill  = c("#4A7C59", "#7DB58A", "#D3D1C7")
   ),
   
   # Ring 3 (outermost) — meet goal vs. below goal
   # Padded with gray so all rings align to 360 degrees
   tibble(
     ring  = 3,
     label = c("Meet goal", "Below goal", "No goal (pad)"),
     value = c(stocks_with_goal * pst_meet_goal,
               stocks_with_goal * (1 - pst_meet_goal),
               total_stocks - stocks_with_goal),
     fill  = c( "#8B5E3C","#C8A96E",   "#D3D1C7")  
   ))
    
 ### 3. Compute arc positions for each ring separately =============
 df <- df %>%
   group_by(ring) %>%
   mutate(
     prop = value / sum(value),
     ymax = cumsum(prop),
     ymin = lag(ymax, default = 0),
     ymid = (ymin + ymax) / 2,
     xmin = ring - 0.45,
     xmax = ring + 0.45
   ) %>%
   ungroup()
  
 labels_df <- df %>%
   filter(!grepl("pad", label),
          label %in% c("Indicator", "Have goal", "No goal"), 
          fill != "#D3D1C7") %>%
   mutate(
     pct_label = paste0(round(prop * 100,1), "%"),
     x_label   = (xmin + xmax) / 2
   )
 
 small_labels_df <-  df %>%
   filter(!grepl("pad", label),
          label %in% c("Meet goal", "Below goal"), 
          fill != "#D3D1C7") %>%
   mutate(
     pct_label = paste0(round(prop * 100,1), "%"),
     x_label   = (xmin + xmax) / 2
   )
 
## 4. Make donut plot ===========
 donut <- ggplot(df) +
   annotate("point", x = -1.5, y = 0, size = 57, color = "#2D5F6E") +
   geom_rect(
     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
     color =  "white", linewidth = 0.6
   ) +
   # Percentage labels on slices
   geom_text(
     data = labels_df,
     aes(x = x_label, y = ymid, label = pct_label),
     size = 2.2, fontface = "bold", color = "white"
   ) + 
   geom_text(data = small_labels_df,
     aes(x = 3.75, y = ymid, label = pct_label),
     size = 2.2, fontface = "bold", color = "black"
   ) +
   # geom_text(
   #   data = small_labels_df,
   #   aes(x = x_label, y = ymid, label = pct_label),
   #   size = 2.5, fontface = "bold", color = "black"
   # ) + 
   scale_fill_identity(
     guide = "legend",
     labels = c(
       "#185FA5" = "Est. Total Chinook Salmon Stocks in PST region", 
       "#5B9BAD" = "% PST indicator stocks",
       "#4A7C59" = "% Indicator stocks with escapement goal",
       "#7DB58A" = "% Indicator stocks without escapement goal",
       "#8B5E3C" = "% Meeting escapement goal",
       "#C8A96E" = "% Below escapement goal",
       "#D3D1C7" = "Non-indicator / no goal"
     ),
     breaks = c(  "#185FA5","#5B9BAD", "#4A7C59","#7DB58A", "#8B5E3C","#C8A96E", "#D3D1C7")
   ) +
   annotate("text", x = -1.5, y = 0,
            label = paste0("", format(total_stocks, big.mark = ","), "\nChinook salmon\nstocks*"),
            size = 4, 
            fontface = "bold", color = "white",
            hjust = 0.5, lineheight = 1) +
   coord_polar(theta = "y",  start = 0, clip = "off") + 
   xlim(c(-1.6, 3.8)) +  
   labs(
     title    = "PST Chinook Salmon Fisheries",
     caption = "*in the Pacific Salmon Treaty (PST) region") +  
   theme_void() +
   theme(panel.grid = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = "right",
         legend.title    = element_blank(),
         legend.text     = element_text(size = 8),
         legend.key.size = unit(0.4, "cm"),
     plot.title    = element_text(face = "bold", size = 14, hjust = 0.5,
                                  margin = margin(b = 4)),
     plot.margin   = margin(2,2,2,2)
     )
 
 donut
 
 ggsave(
   filename = "output/plots/Chinook_Donut_Summary.jpeg",
   plot = donut,
   width =  7,
   height = 5,
   dpi = 300,
   units = "in"
 )
 
 
 
 # PST Chinook -- Nested Donut plot  =======  
### 1. Set Numbers =========
  # total_stocks     <- nrow(pst_names_distinct)   # total PST stocks (center ring)
 indicator_stocks <- sum(esc_summary$n)
 temp <- esc_summary %>% filter(has_psc_escapement_goal == "PSC-Agreed Goal")   # stocks that have an escapement goal
 stocks_with_goal <- as.numeric(temp[1,2])
 pst_meet_goal <- as.numeric(esc_dat_summary[1,3])/100 #0.60   # e.g. 60% of years stocks met their goal 
 
 ###  2. Build the data frame =============== 
 df <- bind_rows(
   tibble(
     ring  = 0,
     label = c("Indicator Stocks"),
     value = c(0),
     fill  = c("#5B9BAD")
   ),
   
   # Ring 1 — have escapement goal vs. no goal
   tibble(
     ring  = 1,
     label = c("Have goal", "No goal"),
     value = c(stocks_with_goal, indicator_stocks-stocks_with_goal),
     fill  = c("#4A7C59", "#7DB58A")# ,"#D3D1C7") #"#5DCAA5" )
   ),
   
   # Ring 2  — meet goal vs. below goal
   # Padded with gray so all rings align to 360 degrees
   tibble(
     ring  = 2,
     label = c("Meet goal", "Below goal", "No goal"),
     value = c(stocks_with_goal * pst_meet_goal,
               stocks_with_goal * (1 - pst_meet_goal),
               indicator_stocks - stocks_with_goal),
     fill  = c("#8B5E3C","#C8A96E", "#D3D1C7")
   ))
  
### 3. Compute arc positions for each ring separately ==============
 df <- df %>%
   group_by(ring) %>%
   mutate(
     prop = value / sum(value),
     ymax = cumsum(prop),
     ymin = lag(ymax, default = 0),
     ymid = (ymin + ymax) / 2,
     xmin = ring - 0.45,
     xmax = ring + 0.45
   ) |>
   ungroup()
 
 labels_df <- df %>%
   filter(!grepl("pad", label),
          label %in% c("Indicator", "Have goal", "No goal","Meet goal", "Below goal"), 
          fill != "#D3D1C7") %>%
   mutate(
     pct_label = paste0(round(prop * 100,1), "%"),
     x_label   = (xmin + xmax) / 2
   ) 
 
 ### 4. Make donut plot ===========
 donut <- ggplot(df) +
   annotate("point", x = -1.5, y = 0, size = 57, color = "#5B9BAD") +
   geom_rect(
     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
     color ="white", linewidth = 0.6
   ) +
   # Percentage labels on slices
   geom_text(
     data = labels_df,
     aes(x = x_label, y = ymid, label = pct_label),
     size = 2.5, fontface = "bold", color = "white"
   ) + 
   scale_fill_identity(
     guide = "legend",
     labels = c(
        "#5B9BAD" = "Chinook Salmon Indicator Stocks", 
        "#4A7C59" = "% Indicator stocks with escapement goal",
        "#7DB58A" = "% Indicator stocks without escapement goal",
        "#8B5E3C" = "% Meeting escapement goal",
        "#C8A96E" = "% Below escapement goal"),
 breaks = c("#5B9BAD", "#4A7C59","#7DB58A", "#8B5E3C","#C8A96E")) +
   annotate("text", x = -1.5, y = 0,
            label = paste0("", format(indicator_stocks, big.mark = ","), " PST Chinook\nsalmon stocks"),
            size = 4, 
            fontface = "bold", color = "white",
            hjust = 0.5, lineheight = 1) +
   coord_polar(theta = "y",  start = 0, clip = "off") + 
   xlim(c(-1.6,3)) +  
   labs(
     title    = "PST Chinook Salmon Fisheries") +
     # caption = "*in the Pacific Salmon Treaty (PST) region") +  
   theme_void() +
   theme(panel.grid = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.position = "right",
         legend.title    = element_blank(),
         legend.text     = element_text(size = 12),        # was 8 — increase text size
         legend.key.size = unit(0.6, "cm"),                # was 0.4 — bigger color keys
         legend.spacing.y = unit(0.3, "cm"),               # adds vertical spacing between items
         
         plot.title    = element_text(face = "bold", size = 14, hjust = 0.5,
                                      margin = margin(b = 4)),
         plot.margin   = margin(2,2,2,2)
   )
 
 donut
 
 ggsave(
   filename = "output/plots/Chinook_Donut_PST_Pos_Summary.jpeg",
   plot = donut,
   width =  8,
   height = 5,
   dpi = 300,
   units = "in"
 )
 ### Layered Donut for Talk ==============
 
 ring1_df <- df %>% filter(xmin >= 0.5 & xmax <= 1.5)   # inner ring
 ring2_df <- df %>% filter(xmin >= 1.5 & xmax <= 2.5)   # middle ring
 ring3_df <- df %>% filter(xmin >= 2.5 & xmax <= 3.5)   # outer ring
 
 shared_theme <- theme_void() +
   theme(
     panel.grid       = element_blank(),
     legend.position  = "none",
     plot.background  = element_rect(fill = "transparent", color = NA),
     panel.background = element_rect(fill = "transparent", color = NA)
   )
  
 ### PLOT 1: Center circle (blue dot + label only) ============================================================
 
 plot_center <- ggplot() +
   annotate("point", x = -1.5, y = 0, size = 57, color = "#5B9BAD") +
   annotate("text", x = -1.5, y = 0,
            label = paste0(format(indicator_stocks, big.mark = ","), " PST Chinook\nsalmon stocks"),
            size = 4, fontface = "bold", color = "white",
            hjust = 0.5, lineheight = 1) +
   coord_polar(theta = "y", start = 0, clip = "off") +
   xlim(c(-1.6, 3)) +
   ylim(c(0, max(df$ymax))) +
   shared_theme
 
 ### PLOT 2: Inner ring (indicator stocks — greens) ============================================================
 
 labels_ring1 <- labels_df %>% filter(xmin >= 0.5 & xmax <= 1.5)
 
 plot_ring1 <- ggplot(ring1_df) +
   geom_rect(
     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
     color = "white", linewidth = 0.6
   ) +
   geom_text(
     data = labels_ring1,
     aes(x = x_label, y = ymid, label = pct_label),
     size = 2.5, fontface = "bold", color = "white"
   ) +
   scale_fill_identity() +
   coord_polar(theta = "y", start = 0, clip = "off") +
   xlim(c(-1.6, 3)) +
   ylim(c(0, max(df$ymax))) +
   shared_theme
 
 ## PLOT 3: Outer ring (meeting/below goal — browns/golds + gray) # ============================================================
 
 labels_ring2 <- labels_df %>% filter(xmin >= 1.5 & xmax <= 2.5)
 
 plot_ring2 <- ggplot(ring2_df) +
   geom_rect(
     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
     color = "white", linewidth = 0.6
   ) +
   geom_text(
     data = labels_ring2,
     aes(x = x_label, y = ymid, label = pct_label),
     size = 2.5, fontface = "bold", color = "white"
   ) +
   scale_fill_identity() +
   coord_polar(theta = "y", start = 0, clip = "off") +
   xlim(c(-1.6, 3)) +
   ylim(c(0, max(df$ymax))) +
   shared_theme
 
 ### SAVE all 3 as PNGs with transparent backgrounds ==========
 
 ggsave("output/plots/plot_center.png", plot_center, width = 7, height = 7, 
        dpi = 300, bg = "transparent")
 
 ggsave("output/plots/plot_ring1.png",  plot_ring1,  width = 7, height = 7, 
        dpi = 300, bg = "transparent")
 
 ggsave("output/plots/plot_ring2.png",  plot_ring2,  width = 7, height = 7, 
        dpi = 300, bg = "transparent")

 # Layered Pie Chart =======
 indicator_stocks <- as.numeric(sum(esc_summary$n))
 temp <- (esc_summary %>% filter(has_psc_escapement_goal == "PSC-Agreed Goal"))   # stocks that have an escapement goal
 stocks_with_goal <- as.numeric(temp[1,2])
 pst_meet_goal <- as.numeric(esc_dat_summary[1,3])/100 #0.60   # e.g. 60% of years stocks met their goal 
# "#5B9BAD", "#4A7C59","#7DB58A", "#8B5E3C","#C8A96E"))
 
# Build data frame
 df <- data.frame(
   label = c(paste0("Escapement Goal"), paste0("No Escapement Goal")),
   value = c(stocks_with_goal, indicator_stocks - stocks_with_goal)
 )
 
 df$label <- factor(df$label, levels = df$label)
 
## Layer 1 =======
 Summary_Pie_layer1 <- ggplot(df, aes(x = "", y = value, fill = label)) +
   geom_col(width = 1, color = "white", linewidth = 1.2) +
   coord_polar(theta = "y", start = 0) +
   scale_fill_manual(values = c("#5B9BAD", "#D3D3D3")) +
   labs(fill = NULL) +
   theme_void(base_size = 14) +
   theme(
       legend.position = "bottom",
     legend.text   = element_text(family = "dm_sans", size = 12),
     plot.margin   = margin(10, 10, 10, 10)
   ) +
   geom_text(
     aes(label = paste0(value, " stocks \n(", round(value / indicator_stocks * 100, 1), "%)")),
     position = position_stack(vjust = 0.5),
     color    = c("white", "gray30"),
     family = "dm_sans",
     fontface = "bold",
     size     = 5)
 
 ggsave(
   filename = "output/plots/Summary_Chinook_stocks_PieLayer1.jpeg",
   plot = Summary_Pie_layer1,
   width =7,
   height =6,
   dpi = 300,
   units = "in")
 
 ## Layer 2 Pie Chart ======= 
 n_meet    <- round(pst_meet_goal * stocks_with_goal)       # e.g. 15  (60% of 22)
 n_no_meet <- stocks_with_goal - n_meet                     # e.g.  7
 n_no_goal <- indicator_stocks - stocks_with_goal           # e.g. 16
 
 df2 <- data.frame(
   label = factor(
     c("Met Escapement Goal", "Has Goal – Did Not Meet", "No Escapement Goal"),
     levels = c("Met Escapement Goal", "Has Goal – Did Not Meet", "No Escapement Goal")
   ),
   value = c(n_meet, n_no_meet, n_no_goal)
 )
 
 fill_colours <- c("#2E6B7A", "#5B9BAD", "#D3D3D3")
  
 label_colours <- c("white", "white", "gray30")
 
 
 layer2_pie <- ggplot(df2, aes(x = "", y = value, fill = label)) +
   geom_col(width = 1, color = "white", linewidth = 1.2) +
   coord_polar(theta = "y", start = 0) +
   scale_fill_manual(values = fill_colours) +
   labs(fill = NULL) +
   theme_void(base_size = 14) +
   theme(
     # plot.title      = element_text(hjust = 0.5, face = "bold", size = 16),
     # plot.subtitle   = element_text(hjust = 0.5, color = "gray40", size = 13),
     legend.position = "bottom",
     legend.text     = element_text(size = 12,family = "dm_sans"),
     plot.margin     = margin(10, 10, 10, 10)
   ) +
   geom_text(
     aes(label = ifelse(label == "Met Escapement Goal",
                        paste0(value, " stocks\n(", round(value / indicator_stocks * 100, 1), "%)"),
                        "")),
     position = position_stack(vjust = 0.5),
     color    = label_colours,
     fontface = "bold",
     family = "dm_sans",
     size     = 5
   )
 
 ggsave(
   filename = "output/plots/Summary_Chinook_stocks_PieLayer2.jpeg",
   plot     = layer2_pie,
   width    = 7,
   height   = 6,
   dpi      = 300,
   units    = "in"#,
#   bg       = "transparent"   # ← overlay-ready
 )
  ## Layer 3 Pie Chart =======
 # Derived counts from Layer 2
 n_meet    <- round(pst_meet_goal * stocks_with_goal)          # e.g. 15
 n_no_meet <- stocks_with_goal - n_meet                        # e.g.  7
 n_no_goal <- indicator_stocks - stocks_with_goal              # e.g. 16
 
 # New Layer 3 value
 n_updated     <- 6                                            # goals updated in last 20 yrs
 n_met_outdated <- n_meet - n_updated                          # e.g. 9  (met goal, outdated)
 
 # --- Data frame: four slices ---
 # Order matters for coord_polar stacking — keep gray last (same visual position)
 df3 <- data.frame(
   label = factor(
     c("Updated Goal (≤20 yrs)", "Met Goal – Outdated", "Has Goal – Did Not Meet", "No Escapement Goal"),
     levels = c("Updated Goal (≤20 yrs)", "Met Goal – Outdated", "Has Goal – Did Not Meet", "No Escapement Goal")
   ),
   value = c(n_updated, n_met_outdated, n_no_meet, n_no_goal)
 )
 
 # --- Colours ---
 # "Updated goal"     → brightest/lightest teal  (innermost accent)
 # "Met, outdated"    → dark teal from Layer 2   (#2E6B7A)
 # "Has goal, unmet"  → mid blue from Layer 2    (#5B9BAD)
 # "No goal"          → same gray                (#D3D3D3)
 fill_colours <- c("#A8D5DF", "#2E6B7A", "#5B9BAD", "#D3D3D3")
 
 # --- Label colours ---
 label_colours <- c("gray20", "white", "white", "gray30")
 
 # --- Plot ---
 layer3_pie <- ggplot(df3, aes(x = "", y = value, fill = label)) +
   geom_col(width = 1, color = "white", linewidth = 1.2) +
   coord_polar(theta = "y", start = 0) +
   scale_fill_manual(values = fill_colours) +
   labs(fill = NULL) +
   theme_void(base_size = 14) +
   theme(
     # plot.title      = element_text(hjust = 0.5, face = "bold", size = 16),
     # plot.subtitle   = element_text(hjust = 0.5, color = "gray40", size = 13),
     legend.position = "bottom",
     legend.text     = element_text(size = 11,family = "dm_sans"),
     legend.key.size = unit(0.9, "lines"),
     plot.margin     = margin(10, 20, 10, 20)) +
   guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
   geom_text(
     aes(label = ifelse(label == "Updated Goal (≤20 yrs)",
                        paste0(value, " stocks\n(", round(value / indicator_stocks * 100, 1), "%)"),
                        "")),
     position = position_stack(vjust = 0.5),
     color    = label_colours,
     family = "dm_sans",
     fontface = "bold",
     size     = 5
   )
 
 ggsave(
   filename = "output/plots/Summary_Chinook_stocks_PieLayer3.jpeg",
   plot     = layer3_pie,
   width    = 7,      
   height   = 6,
   dpi      = 300,
   units    = "in"
 )
 
 # Catch Summaries =========
## 5. What % of catch by species is local/terminal vs offshore? & has that changed through time ==============
 
 data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
   filter(!year == 2023, !is.na(total_run), !total_run =="NA") 
 
 total_run_df<-data %>%  
   dplyr::group_by(year,population) %>%
   dplyr::summarise(total_run = sum(as.numeric(total_run)))
 
 catch_distributions <- data %>%
   dplyr::select(c(1:39)) %>%
   gather(4:ncol(.), key = "fishery_region", value = "percent_mort") %>%
   filter(fishery_region %in% c(
     "aabm_tot",  
     "term_tot",
     "sbc_is_tot",
     "US_is_tot"
   )) %>% 
   dplyr::mutate(percent_mort = as.numeric(percent_mort)/100) 
 
 summary_df <- catch_distributions %>%
   left_join(total_run_df) %>% 
   filter(!is.na(percent_mort)) %>% 
   dplyr::mutate(mortality_numbers = total_run *percent_mort) %>%
   group_by(year, fishery_region)  %>%
   dplyr::summarise(total_FM_numbers = sum(mortality_numbers),
                    total_run = sum(total_run),
                    p_mortality = total_FM_numbers/total_run
                    ) %>% # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize. 
   group_by(year)  %>%
    dplyr::mutate(p_FM = total_FM_numbers/sum(total_FM_numbers))
 
 
 ### bar chart =====  
 FM_stack <- ggplot(summary_df,
                         aes(x = year,
                             y = p_FM,
                             fill = fishery_region)) +
   geom_col(color = "black", alpha = 0.9, width = 1, stat="identity") +
   scale_y_continuous(
     expand = c(0, 0),
     name = "Proportion of Fishery Mortality",
     labels = scales::percent_format()
   ) +
   scale_x_continuous(
     name = "Year",
     expand = c(0, 0)
   ) +
   # scale_fill_manual(values = custom_pal) + 
   # scale_fill_viridis_d(drop = FALSE)+#, option = "plasma") +
   labs(
     fill = "Fishery Region",
     title = "PST Chinook Salmon FM Sources",
     subtitle = "Proportional Fishery Mortality by Region & Year"
   ) +
   theme_minimal() +
   theme(
     panel.grid.minor = element_blank(),
     panel.spacing = unit(1, "lines"),
     legend.position = "right")
 FM_stack 
 
 # Total Stocks - Steelhead ======

 # change the filtering so the row can only stay if there is some sort of data input, 
 # indicating data collection and that a stock actually exists or has existed in that region.  
 total_Steelhead_stocks <- read_csv("data/Pinksy et al Data/Pinsky_etal_AbundanceDatabase2008.csv") %>%
   left_join(pst_region_names) %>%
   filter(Spp == "Steelhead") %>%
   filter(!is.na(id),
          # filter out rows where there was no input data, sometimes Pinksy estiamted run size based on neighboring runs, but if there is zero data I dont want to include those, then no confirmation there is a species there
          !is.na(AbundAve) | !is.na(EscAve) | !is.na(CatchAve) | !is.na(HarvestRate), 
          !grepl("expanded", Name, ignore.case = TRUE)) %>%
   dplyr::mutate(Name = case_when(  
     grepl("adam", Name, ignore.case = TRUE) ~ "ADAMS RIVER",
     grepl("ANDERSON", Name, ignore.case = TRUE) ~ "ANDERSON RIVER",
     grepl("battle creek", Name, ignore.case = TRUE) ~ "Battle Creek",
     grepl("bedwell", Name, ignore.case = TRUE) ~ "BEDWELL", 
     grepl("BULKLEY", Name, ignore.case = TRUE) ~ "BULKLEY RIVER",  
     grepl("BABINE", Name, ignore.case = TRUE) ~ "BABINE RIVER",  
     grepl("CANYON", Name, ignore.case = TRUE) ~ "CANYON CREEK",
     grepl("Canyon", Name, ignore.case = TRUE) ~ "CANYON CREEK",
     grepl("CANTON", Name, ignore.case = TRUE) ~ "CANYON CREEK", 
     grepl("COW", Name, ignore.case = TRUE) ~ "COW CREEK", 
     grepl("CLEAR CREEK", Name, ignore.case = TRUE) ~ "CLEAR CREEK", 
     grepl("CAYUSE", Name, ignore.case = TRUE) ~ "CAYUSE RIVER",  
     grepl("CAYEGHLE", Name, ignore.case = TRUE) ~ "CAYEGHLE RIVER",  
     grepl("CEDAR", Name, ignore.case = TRUE) ~ "CEDAR RIVER",  
     grepl("CLEARWATER", Name, ignore.case = TRUE) ~ "CLEARWATER RIVER", 
     grepl("DESERTED", Name, ignore.case = TRUE) ~ "DESERTED RIVER", 
     grepl("FRENCH", Name, ignore.case = TRUE) ~ "FRENCH RIVER",  
     grepl("Grays River piece - Grays River", Name, ignore.case = TRUE) ~ "GRAYS RIVER",  
     grepl("Green", Name, ignore.case = TRUE) ~ "GREEN RIVER",  
     grepl("Mad", Name, ignore.case = TRUE) ~ "MAD RIVER",  
     grepl("MAMQUAM", Name, ignore.case = TRUE) ~ "MAMQUAM RIVER",  
     grepl("Salmon River", Name, ignore.case = TRUE) ~ "SALMON RIVER",  
     TRUE ~ Name )) %>%
   distinct(Name,Spp,Runtiming)#, AbundAve,EscAve,CatchAve, HarvestRate) 
 
 nrow(total_Steelhead_stocks)
 
 # test to make sure there arent more duplicate names 
 test <- total_Steelhead_stocks %>%
   mutate(first_word = tolower(word(Name, 1))) %>%
   mutate(first_word_dup = first_word %in% first_word[duplicated(first_word)]) %>%
   filter(first_word_dup) %>%
   arrange(first_word)
  
 
 
 # Total Stocks - Coho ======
 # change the filtering so the row can only stay if there is some sort of data input, 
 # indicating data collection and that a stock actually exists or has existed in that region.  
 total_Coho_stocks <- read_csv("data/Pinksy et al Data/Pinsky_etal_AbundanceDatabase2008.csv") %>%
   left_join(pst_region_names) %>%
   filter(Spp == "Coho") %>%
   filter(!is.na(id),
          # filter out rows where there was no input data, sometimes Pinksy estiamted run size based on neighboring runs, but if there is zero data I dont want to include those, then no confirmation there is a species there
          !is.na(AbundAve) | !is.na(EscAve) | !is.na(CatchAve) | !is.na(HarvestRate), 
          !grepl("expanded", Name, ignore.case = TRUE)) %>%
   dplyr::mutate(Name = case_when(  
     grepl("adam", Name, ignore.case = TRUE) ~ "ADAMS RIVER",
     grepl("ANDERSON", Name, ignore.case = TRUE) ~ "ANDERSON RIVER",
     grepl("battle creek", Name, ignore.case = TRUE) ~ "Battle Creek",
     grepl("bedwell", Name, ignore.case = TRUE) ~ "BEDWELL", 
     grepl("BULKLEY", Name, ignore.case = TRUE) ~ "BULKLEY RIVER",  
     grepl("BABINE", Name, ignore.case = TRUE) ~ "BABINE RIVER",  
     grepl("CANYON", Name, ignore.case = TRUE) ~ "CANYON CREEK",
     grepl("Canyon", Name, ignore.case = TRUE) ~ "CANYON CREEK",
     grepl("CANTON", Name, ignore.case = TRUE) ~ "CANYON CREEK", 
     grepl("COW", Name, ignore.case = TRUE) ~ "COW CREEK", 
     grepl("CLEAR CREEK", Name, ignore.case = TRUE) ~ "CLEAR CREEK", 
     grepl("CAYUSE", Name, ignore.case = TRUE) ~ "CAYUSE RIVER",  
     grepl("CAYEGHLE", Name, ignore.case = TRUE) ~ "CAYEGHLE RIVER",  
     grepl("CEDAR", Name, ignore.case = TRUE) ~ "CEDAR RIVER",  
     grepl("CLEARWATER", Name, ignore.case = TRUE) ~ "CLEARWATER RIVER", 
     grepl("DESERTED", Name, ignore.case = TRUE) ~ "DESERTED RIVER", 
     grepl("FRENCH", Name, ignore.case = TRUE) ~ "FRENCH RIVER",  
     grepl("Grays River piece - Grays River", Name, ignore.case = TRUE) ~ "GRAYS RIVER",  
     grepl("Green", Name, ignore.case = TRUE) ~ "GREEN RIVER",  
     grepl("Mad", Name, ignore.case = TRUE) ~ "MAD RIVER",  
     grepl("MAMQUAM", Name, ignore.case = TRUE) ~ "MAMQUAM RIVER",  
     grepl("Salmon River", Name, ignore.case = TRUE) ~ "SALMON RIVER",  
     TRUE ~ Name )) %>%
   distinct(Name,Spp,Runtiming)#, AbundAve,EscAve,CatchAve, HarvestRate) 
 
 # test to make sure there arent more duplicate names 
 # test <- total_Coho_stocks %>%
 #   mutate(first_word = tolower(word(Name, 1))) %>%
 #   mutate(first_word_dup = first_word %in% first_word[duplicated(first_word)]) %>%
 #   filter(first_word_dup) %>% 
 #   arrange(first_word)
 
 nrow(total_Coho_stocks)
