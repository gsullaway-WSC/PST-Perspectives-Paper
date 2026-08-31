### ============================================================
### Escapement bars + ocean-ER background shading + forward-shortfall flags
### Replicates the style of Figure_3_Facet_Chinook_Coastwide_escapement_Pop.png
###
### DEFINITIONS (stated explicitly since the request left them open):
### - Background shading & "High ER" test use REALIZED ER = nominal ocean catch / total
###   available cohort that calendar year (Catch_ocean / Cohort_avail) -- the plain observed
###   exploitation rate, matching how CTC reports typically show "exploitation rate" and how
###   the reference figure's "Mixed Stock ER" shading reads. Ocean_AEQ_ER (adult-equivalent,
###   natural-mortality-adjusted) is still computed and kept in the output CSV for reference/
###   comparison, but is NOT what drives the shading or the flags below.
### - "High ER" year = Realized_ER in the top tercile (upper third) of THAT STOCK's own
###   calendar-year Realized_ER distribution (computed only over the analysis window set by
###   `analysis_start_year` below). Change `er_high_quantile` to adjust.
### - "Rolling mean, next 3 years" = mean Escapement over t+1, t+2, t+3 (NOT including
###   year t itself) -- i.e. does the shortfall show up in the years AFTER the high-ER year.
### - EG (escapement goal) = the time-varying Floor value in year t (from
###   Escapement_Goals_Use.csv via the crosswalk built previously).
### - Blue triangle: High ER in year t AND mean(Escapement[t+1:t+3]) < Floor[t]
### - Red circle:    High ER in year t AND mean(Escapement[t+1:t+3]) < 0.85 * Floor[t]
###   (red and blue are mutually exclusive -- red takes precedence, matching the reference
###   figure's 85%-of-objective severity tier, which mirrors the PST's own "less than 85%
###   of the point estimate" underperformance trigger, paragraph 7(a)(iv)).
### ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(zoo)
library(ggplot2)

er_high_quantile <- 2/3   # top tercile; change to 0.75 for top quartile, 0.5 for above-median, etc.
analysis_start_year <- 2000   # series is restricted to this calendar year onward (see filter below)

## ---------------------------------------------------------------
## 0. File paths
## ---------------------------------------------------------------
cohorts_path <- "data/CLB25 Cohorts.xls"
morts_path   <- "data/CLB25 Morts"
goals_path   <- "data/Escapement_Goals_Use.csv"

## ---------------------------------------------------------------
## 1. Fishery classification, crosswalk, floor construction (as before)
## ---------------------------------------------------------------
terminal_fisheries <- c(
  "TAK TBR N", "TAK TBR S", "TBC TBR FN", "TCENTRAL FN", "TCENTRAL FS",
  "TCOL R N", "TCOL R S", "TFRAS FN", "TFRASER FS", "TGEO ST FN", "TGS FS",
  "TNORTH FS", "TPS FN", "TPS FS", "TSF FS", "TWAC FN", "TWCVI FS", "TYK YAK FN"
)

crosswalk <- c(
  "Atnarko Wild" = "CBC", "Chilkat" = "NSA", "Columbia Upriver Brights" = "URB",
  "Coquille" = "MOC", "Coweeman" = "CWF", "Cowichan" = "LGS",
  "Grays Harbor Fall" = "WCN", "Harrison" = "FHF", "Hoh Fall" = "WCN",
  "Kitsumkalum" = "NBC", "Lewis" = "LRW", "Lower Shuswap" = "FSO",
  "Mid-Columbia Sum" = "SUM", "Nehalem R." = "NOC", "Nicola" = "FS2",
  "Nooksack" = "NKS", "Queets Fall" = "WCN", "Quillayute Fall" = "WCN",
  "Siletz Fall" = "NOC", "Siuslaw R." = "NOC", "Skagit SumFall" = "SKG",
  "Snohomish" = "SNO", "South Umpqua" = "MOC", "Stikine" = "TST",
  "Stillaguamish" = "STL", "Taku" = "TST", "Unuk River" = "SSA"
)
n_components <- table(crosswalk)

goals_raw <- read_csv(goals_path, locale = locale(encoding = "latin1"), col_types = cols())
goals <- goals_raw %>%
  mutate(
    Floor_value = coalesce(`PSC-Agreed Goal`, `Lower Goal`, `Agency Goal`,
                            `Minimun Natural Escapement Goal`, `ESA Recovery Goal`),
    ModelStock = crosswalk[StockName]
  ) %>%
  filter(!is.na(ModelStock))

floor_by_year <- goals %>%
  group_by(ModelStock, year) %>%
  summarise(have = sum(!is.na(Floor_value)), Floor = sum(Floor_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(need = as.integer(n_components[ModelStock])) %>%
  filter(have >= need) %>%
  select(Stock = ModelStock, CalYear = year, Floor)

static_floor <- tribble(~Stock, ~Floor, "ALS", 3500, "YAK", 500)

## ---------------------------------------------------------------
## 2. Cohort/mortality data -> calendar-year escapement and ocean AEQ ER
## ---------------------------------------------------------------
morts <-  readxl::read_xls("data/CLB25 Morts.xls") %>%
  mutate(Stock = str_trim(Stock), Fishery = str_trim(Fishery), CalYear = Year + Age,
         AEQ_total = `AEQ Catch` + `AEQ Shakers` + `AEQ CNRLeg` + `AEQ CNRSubLeg`,
         is_terminal = Fishery %in% terminal_fisheries)

cohorts <-  readxl::read_xls("data/CLB25 Cohorts.xls") %>%
  mutate(Stock = str_trim(Stock), CalYear = Year + Age)

brood_min <- min(cohorts$Year); brood_max <- max(cohorts$Year)
age_min   <- min(cohorts$Age);  age_max   <- max(cohorts$Age)
cal_min   <- brood_min + age_max
cal_max   <- brood_max + age_min

esc_cal <- cohorts %>% group_by(CalYear, Stock) %>%
  summarise(Escapement = sum(Escapement, na.rm = TRUE), .groups = "drop")

mort_cal <- morts %>% group_by(CalYear, Stock, is_terminal) %>%
  summarise(AEQ_total = sum(AEQ_total, na.rm = TRUE),
            Catch = sum(Catch, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = is_terminal, values_from = c(AEQ_total, Catch),
              names_glue = "{.value}_{if_else(is_terminal,'terminal','ocean')}", values_fill = 0)

cohort_cal <- cohorts %>% group_by(CalYear, Stock) %>%
  summarise(Cohort_avail = sum(Cohort, na.rm = TRUE), .groups = "drop")

df <- esc_cal %>%
  inner_join(mort_cal, by = c("CalYear","Stock")) %>%
  inner_join(cohort_cal, by = c("CalYear","Stock")) %>%
  filter(CalYear >= max(cal_min, analysis_start_year), CalYear <= cal_max) %>%
  mutate(Total_AEQ_Mort = AEQ_total_ocean + AEQ_total_terminal,
         Potential_run  = Escapement + Total_AEQ_Mort,
         Ocean_AEQ_ER   = AEQ_total_ocean / Potential_run,      # kept for reference/comparison only
         Realized_ER    = Catch_ocean / Cohort_avail) %>%       # nominal catch / available cohort -- USED for shading & flags
  left_join(floor_by_year, by = c("Stock","CalYear")) %>%
  left_join(static_floor, by = "Stock", suffix = c("", "_static")) %>%
  mutate(Floor = coalesce(Floor, Floor_static)) %>%
  select(-Floor_static)

## ---------------------------------------------------------------
## 3. High-ER flag (per stock, relative to its own distribution), forward 3-year
## escapement mean (t+1:t+3, excluding t), and the blue/red compound flags
## ---------------------------------------------------------------
df <- df %>%
  arrange(Stock, CalYear) %>%
  group_by(Stock) %>%
  mutate(
    ER_high_cutoff = quantile(Realized_ER, er_high_quantile, na.rm = TRUE),
    High_ER = Realized_ER >= ER_high_cutoff,
    # forward mean over t+1..t+3: shift Escapement back by 1 then take a 3-wide forward roll
    Escapement_fwd3 = lead(rollapply(Escapement, width = 3, FUN = mean,
                                      align = "left", fill = NA, na.rm = TRUE), n = 1)
  ) %>%
  ungroup() %>%
  mutate(
    Red_flag  = High_ER & !is.na(Floor) & Escapement_fwd3 < 0.85 * Floor,
    Blue_flag = High_ER & !is.na(Floor) & Escapement_fwd3 < Floor & !Red_flag,
    FlagType  = case_when(Red_flag ~ "Red", Blue_flag ~ "Blue", TRUE ~ "None")
  )

message(sprintf("Analysis window actually used: %d - %d", max(cal_min, analysis_start_year), cal_max))

write_csv(df, "stock_escapement_ER_flags.csv")

## ---------------------------------------------------------------
## 4. Plot -- one page per stock with a floor. Bars = escapement, background = ocean ER,
## step line = floor, points = flags. Point placed just above each bar.
## ---------------------------------------------------------------
stocks_with_floor <- df %>% filter(!is.na(Floor)) %>% pull(Stock) %>% unique() %>% sort()

pdf("escapement_ER_shortfall_flags.pdf", width = 10, height = 6)

for (s in stocks_with_floor) {
  d <- df %>% filter(Stock == s)
  y_max <- max(d$Escapement, na.rm = TRUE) * 1.08

  p <- ggplot(d, aes(x = CalYear)) +
    geom_rect(aes(xmin = CalYear - 0.5, xmax = CalYear + 0.5, ymin = -Inf, ymax = Inf,
                   fill = Realized_ER)) +
    scale_fill_gradient(low = "white", high = "#B2182B", name = "Realized ER") +
    geom_col(aes(y = Escapement), fill = "grey30", width = 0.7) +
    geom_step(aes(y = Floor), color = "black", linewidth = 0.6, na.rm = TRUE) +
    geom_point(data = filter(d, FlagType == "Blue"),
               aes(y = y_max), shape = 17, color = "steelblue", size = 2.6) +
    geom_point(data = filter(d, FlagType == "Red"),
               aes(y = y_max), shape = 16, color = "firebrick", size = 2.6) +
    labs(title = paste0(s, " -- escapement, realized ocean ER, and forward shortfall flags"),
         subtitle = "Blue = high ER & next-3-yr mean escapement < EG.  Red = high ER & next-3-yr mean escapement < 85% of EG.",
         y = "Escapement", x = "Calendar year") +
    theme_minimal() +
    theme(legend.position = "right")

  print(p)
}

dev.off()
