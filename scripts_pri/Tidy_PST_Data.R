# Source: https://www.psc.org/publications/technical-reports/technical-committee-reports/chinook/ctc-data-sets/
# Mortality:   TCCHINOOK-26-01-Appendix-C-Mortality-Distribution-Tables-Detailed source: https://www.psc.org/publications/technical-reports/technical-committee-reports/chinook/ctc-data-sets/
# Escapement:  Escapement_data_all_2026-08-27.csv from:  https://psc1.shinyapps.io/ctc-shiny-app/_w_efbbad27/

# Mortality =  the AEQ equivalency estimates, a calendar-year, AEQ-accounting
# total_er = fraction of that AEQ total run killed across all fisheries that calendar year

# PSC CTC Chinook -- build one master table from:
#   (A) Appendix C "total mort" tabs         -> % distribution of AEQ mortality + esc
#   (B) Escapement_data_all_2026-08-27.csv   -> actual escapement numbers (long format:
#                                                Year, StockName, SeriesLabel, Values)
 

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(readxl)
library(here)

mort_path <- here("data/TCCHINOOK-26-01-Appendix-C-Mortality-Distribution-Tables-Detailed.xlsx")
esc_path  <- here("data/CTC_Escapement_data_all_2026-08-27.csv")

# STOCK CODE -> FULL NAME -> REGION   =========
stock_xwalk <- tribble(
  ~stock_code,      ~population,                                  ~region,
  "ATN",  "Atnarko River",                          "N BC",
  "BQR",  "Big Qualicum River Fall",                "S BC",
  "CHI",  "Chilliwack River Fall",                  "Fraser",
  "CHK",  "Chilkat River",                          "SEAK",
  "COW",  "Cowichan River Fall",                    "S BC",
  "CWF",  "Cowlitz Fall Tule",                      "Columbia R.",
  "ELK",  "Elk River",                              "OR Coast",
  "South Umpqua", "South Umpqua",                   "OR Coast",
  "Coquille",     "Coquille",                       "OR Coast",
  "ELW",  "Elwha River",                            "WA Coast",
  "GAD",  "George Adams Fall Fingerling",           "Puget Sound",
  "HAN",  "Hanford Wild Brights",                   "Columbia R.",
  "HAR",  "Harrison River",                         "Fraser",
  "HOK",  "Hoko Fall Fingerling",                   "WA Coast",
  "KLM",  "Kitsumkalum River Summer",               "N BC",
  "KLY",  "Kitsumkalum Yearling",                   "N BC",
  "LRH",  "Lower River Hatchery Tule",               "Columbia R.",
  "LRW",  "Lewis River Wild",                       "Columbia R.",
  "LYF",  "Lyons Ferry",                            "Columbia R.",
  "LYY",  "Lyons Ferry Yearling",                   "Columbia R.",
  "MSH",  "Middle Shuswap River Summer",            "Fraser",
  "NIC",  "Nicola River Spring",                    "Fraser",
  "NIS",  "Nisqually Fall Fingerling",              "Puget Sound",
  "NSF",  "Nooksack Spring Fingerling",             "Puget Sound",
  "NSA",  "Northern Southeast Alaska Spring",       "SEAK",
  "PHI",  "Phillips River Fall",                    "S BC",
  "PPS",  "Puntledge River Summer",                 "S BC",
  "QUE",  "Queets Fall Fingerling",                 "WA Coast",
  "Grays Harbor", "Grays Harbor",                   "WA Coast",
  "Hoh",          "Hoh",                            "WA Coast",
  "Quillayute",   "Quillayute",                     "WA Coast",
  "QUI",  "Quinsam River Fall",                     "S BC",
  "EVIN", "East Vancouver Island North",            "S BC",
  "RBT",  "Robertson Creek Fall",                   "WCVI",
  "NWVI", "Northwest Vancouver Island",             "WCVI",
  "SWVI", "Southwest Vancouver Island",             "WCVI",
  "SAM",  "Samish Fall Fingerling",                 "Puget Sound",
  "SHU",  "Lower Shuswap River Summer",             "Fraser",
  "SKF",  "Skagit Spring Fingerling",               "Puget Sound",
  "SKY",  "Skykomish Fall Fingerling",              "Puget Sound",
  "SMK",  "Similkameen Summer Yearling",            "Fraser",
  "SOO",  "Tsoo-Yess Fall Fingerling",              "WA Coast",
  "SPR",  "Spring Creek Tule",                      "Columbia R.",
  "SPS",  "South Puget Sound Fall Fingerling",      "Puget Sound",
  "SRH",  "Salmon River",                           "OR Coast",
  "Nehalem", "Nehalem",                             "OR Coast",
  "Siletz",  "Siletz",                              "OR Coast",
  "Siuslaw", "Siuslaw",                             "OR Coast",
  "SSA",  "Southern Southeast Alaska Spring",       "SEAK",
  "SSF",  "Skagit Summer Fingerling",               "Puget Sound",
  "STI",  "Stikine River",                          "Transboundary",
  "STL",  "Stillaguamish Fall Fingerling",          "Puget Sound",
  "SUM",  "Columbia River Summers",                 "Columbia R.",
  "TAK",  "Taku River",                             "Transboundary",
  "TST",  "Taku And Stikine Rivers",                "Transboundary",
  "UNU",  "Unuk River",                             "SEAK",
  "URB",  "Columbia River Upriver Bright",          "Columbia R.",
  "WSH",  "Willamette Spring",                      "Columbia R.",
  "NSF adj", "Nooksack Spring Fingerling (Adjusted)","Puget Sound"
)

# STOCK CODE -> river mouth latitude (decimal degrees, WGS84, N positive) =====
 
river_mouth_lat_xwalk <- tribble(
  ~stock_code,      ~river_mouth_lat,
  "ATN",  52.37,   # Atnarko/Bella Coola R. mouth, North Bentinck Arm
  "BQR",  49.36,   # Big Qualicum R. mouth, Strait of Georgia
  "CHI",  49.14,   # Chilliwack/Vedder R. confluence w/ Fraser
  "CHK",  59.23,   # Chilkat R. mouth, Chilkat Inlet near Haines
  "COW",  48.75,   # Cowichan R. mouth, Cowichan Bay
  "CWF",  46.10,   # Cowlitz R. mouth at Columbia R., Longview WA
  "ELK",  42.79,   # Elk R. mouth, Port Orford OR
  "South Umpqua", 43.70,   # Umpqua R. mouth, Reedsport OR (proxy for S. Fork)
  "Coquille",     43.12,   # Coquille R. mouth, Bandon OR
  "ELW",  48.15,   # Elwha R. mouth, Strait of Juan de Fuca
  "GAD",  47.42,   # Skokomish R. system, Hood Canal near Hoodsport WA
  "HAN",  46.55,   # Hanford Reach, mid-Columbia R. near Richland WA
  "HAR",  49.29,   # Harrison R. confluence w/ Fraser
  "HOK",  48.17,   # Hoko R. mouth, Strait of Juan de Fuca
  "KLM",  54.52,   # Kitsumkalum R. confluence w/ Skeena near Terrace BC
  "KLY",  54.52,   # same system as KLM
  "LRH",  46.19,   # Lower Columbia R. composite, near Astoria OR
  "LRW",  45.86,   # Lewis R. mouth at Columbia R., Woodland WA
  "LYF",  46.58,   # Snake R. near Lyons Ferry WA
  "LYY",  46.58,   # same system as LYF
  "MSH",  50.85,   # Shuswap R. system, interior BC (approx)
  "NIC",  50.42,   # Nicola R. confluence w/ Thompson R., Spences Bridge BC
  "NIS",  47.10,   # Nisqually R. mouth, Puget Sound near Olympia
  "NSF",  48.73,   # Nooksack R. mouth, Bellingham Bay
  "NSA",  NA,       # Northern SEAK Spring -- regional composite, no single river
  "PHI",  50.48,   # Phillips R. mouth, Phillips Arm BC
  "PPS",  49.69,   # Puntledge R. mouth, Comox/Courtenay BC
  "QUE",  47.53,   # Queets R. mouth, WA coast
  "Grays Harbor", 46.97,   # Grays Harbor entrance / Chehalis R. mouth
  "Hoh",          47.75,   # Hoh R. mouth, WA coast
  "Quillayute",   47.91,   # Quillayute R. mouth, La Push WA
  "QUI",  50.03,   # Quinsam R. confluence, Campbell River BC
  "EVIN", 50.00,   # East Vancouver Island North -- approx composite centroid
  "RBT",  49.26,   # Robertson Ck / Somass R. system, Port Alberni BC
  "NWVI", 50.05,   # NW Vancouver Island composite -- approx centroid, Kyuquot Sound area
  "SWVI", 49.15,   # SW Vancouver Island composite -- approx centroid, Clayoquot Sound area
  "SAM",  48.55,   # Samish R. mouth, Samish Bay
  "SHU",  50.83,   # Lower Shuswap R., Sicamous BC area
  "SKF",  48.35,   # Skagit R. mouth, Skagit Bay
  "SKY",  47.97,   # proxy: Snohomish R. mouth, Everett WA
  "SMK",  49.13,   # Similkameen R. confluence w/ Okanagan R., near border
  "SOO",  48.30,   # Sooes/Tsoo-Yess R. mouth near Neah Bay WA
  "SPR",  45.73,   # Spring Creek NFH, Columbia R. Gorge near Underwood WA
  "SPS",  47.20,   # South Puget Sound composite -- approx, Olympia area
  "SRH",  45.04,   # Salmon R. mouth near Lincoln City OR
  "Nehalem", 45.65,   # Nehalem R. mouth, OR coast
  "Siletz",  44.91,   # Siletz R. mouth, Lincoln City OR
  "Siuslaw", 43.97,   # Siuslaw R. mouth, Florence OR
  "SSA",  NA,       # Southern SEAK Spring -- regional composite, no single river
  "SSF",  48.35,   # Skagit R. mouth (same system as SKF)
  "STI",  56.60,   # Stikine R. mouth near Wrangell AK
  "STL",  48.25,   # Stillaguamish R. mouth, Port Susan Bay
  "SUM",  46.70,   # Columbia R. Summers -- approx mid-Columbia (upriver run, not a single tributary)
  "TAK",  58.35,   # Taku R. mouth near Juneau AK
  "TST",  NA,       # Taku + Stikine combined -- two river mouths, no single point
  "UNU",  55.72,   # Unuk R. mouth, Burroughs Bay/Behm Canal near Ketchikan
  "URB",  46.55,   # Columbia R. Upriver Bright -- approx Hanford Reach area
  "WSH",  45.65,   # Willamette R. mouth at Columbia R., Portland OR
  "NSF adj", 48.73  # same system as NSF
)

# STOCK CODES included in the CTC's synoptic stock status evaluation, per
# Table 3.2 ("Summary of information available for synoptic stock
# evaluations"), filtered to rows where Data Sufficient == "Yes".
#
# Table 3.2 has 21 such rows, but two of them -- Situk and Alsek -- have
# Exploitation Rate Indicator == "TBD", meaning there is no CWT-based AEQ
# mortality distribution table for them at all (no "total mort" sheet in
# Appendix C, no stock_code, no rows in mort_long). They cannot appear in
# `master` regardless of this filter, so they're excluded here.
#
# Upriver Brights is a single row in Table 3.2 but lists TWO exploitation
# rate indicator codes ("URB / HAN") -- confirmed by the CTC report text:
# this stock "appears twice ... because there are two exploitation rate
# indicator stocks (URB and HAN)". Both are included below.
synoptic_stock_codes <- c(
  "CHK", "UNU", "TAK", "STI",              # SEAK / Transboundary (Situk, Alsek excluded -- no mortality data)
  "ATN", "COW",                            # BC
  "SHU", "HAR",                            # Fraser
  "SKF", "SSF", "QUE", "Quillayute", "Hoh",# WA / WA Coast
  "URB", "HAN", "LRW", "SUM",              # Columbia R. (Upriver Brights = URB + HAN)
  "Nehalem", "Siletz", "Siuslaw"           # OR Coast
)

# Read every "total mort" tab from Appendix C   ========
pct_col_names <- c(
  "seak_t","seak_n","seak_s",                # AABM SEAK
  "nbc_t","nbc_s",                           # AABM NBC
  "wcvi_t","wcvi_s",                         # AABM WCVI
  "nbcis_t","nbcis_n","nbcis_s",             # ISBM NBC & CBC
  "sbcis_t","sbcis_n","sbcis_s",             # ISBM Southern BC
  "nfalc_t","nfalc_s",                       # ISBM N Falcon
  "sfalc_t","sfalc_s",                       # ISBM S Falcon
  "wac_n",                                   # ISBM WAC
  "ps_n","ps_s",                             # ISBM Puget Sound
  "seakterm_t","seakterm_n","seakterm_s",    # Terminal SEAK
  "canterm_n","canterm_s",                   # Terminal Canada
  "usterm_t","usterm_n","usterm_s",          # Terminal Southern US
  "stray",                                   # Escapement: stray
  "esc_pct"                                  # Escapement: % of total run
)

# Load and tidy mortality excel sheet =====
read_mort_tab <- function(sheet, code) {
  df <- read_excel(mort_path, sheet = sheet, skip = 6, col_names = FALSE)
  ncol_expected <- 3 + length(pct_col_names) + 1  # year,cwt,ages + 30 pct + status
  df <- df[, 1:ncol_expected]
  names(df) <- c("calendar_year", "est_cwt", "ages", pct_col_names, "data_status")
  df <- df %>% mutate(calendar_year = suppressWarnings(as.numeric(calendar_year)))
  df %>%
    filter(!is.na(calendar_year), calendar_year >= 1900, calendar_year <= 2100) %>%
    mutate(across(all_of(c("est_cwt", pct_col_names)), ~ suppressWarnings(as.numeric(as.character(.x))))) %>%
    mutate(stock_code = code, .before = 1)
}

all_sheets  <- excel_sheets(mort_path)
mort_sheets <- all_sheets[str_ends(all_sheets, "total mort")]
mort_codes  <- str_remove(mort_sheets, " total mort$")

mort_long <- map2_dfr(mort_sheets, mort_codes, read_mort_tab)

## Load and tidy escapement CSV  =====

esc_raw <- read_csv(esc_path, col_types = cols(
  Year        = col_double(),
  StockName   = col_character(),
  SeriesLabel = col_character(),
  Values      = col_double()
))

# stock_code -> which StockName in the CSV
esc_source_xwalk <- tribble(
  ~stock_code, ~csv_stock,                     ~series_primary,                     ~series_fallback,
  "CHK",  "Chilkat",                        "Esc",                                 NA,
  "UNU",  "Unuk",                           "Esc",                                 NA,
  "TAK",  "Taku",                           "Esc",                                 NA,
  "STI",  "Stikine",                        "Esc",                                 NA,
  "ATN",  "Atnarko Wild",                   "Calib. Wild Adult Esc",               "MR Wild Adult Esc",
  "COW",  "Cowichan",                       "Esc",                                 NA,
  "PHI",  "Phillips",                       "Esc",                                 NA,
  "SWVI", "South West Vancouver Island",    "SWVI Total",                          NA,
  "NWVI", "North West Vancouver Island",    "NWVI Total",                          NA,
  "HAR",  "Harrison",                       "Esc",                                 NA,
  "SHU",  "Lower Shuswap",                  "Calib. Total Adult Esc",              "Esc (MR)",
  "NIC",  "Nicola Sp 1.2",                  "Calib. Total Adult Esc",              "MR Total Adult Esc",
  "NSF",  "Nooksack",                       "Total Esc",                           "Esc (tGMR)",
  "NSF adj", "Nooksack",                    "Total Nat. Origin Esc",               "Total Esc",
  "SKF",  "Skagit Spr",                     "Esc (Redd Ct)",                       "Esc (Peak Ct)",
  "SSF",  "Skagit SumFall",                 "Esc",                                 NA,
  "STL",  "Stillaguamish",                  "Esc (Redd Ct)",                       "Esc (tGMR)",
  "SKY",  "Snohomish",                      "Esc",                                 NA,   # proxy: Skykomish is a Snohomish tributary
  "HOK",  "Hoko",                           "Esc",                                 NA,
  "Quillayute", "Quillayute Fall",          "Esc",                                 NA,   # composite proxied by Fall stock
  "Hoh",        "Hoh Fall",                 "Esc",                                 NA,   # composite proxied by Fall stock
  "QUE",  "Queets Fall",                    "Esc",                                 NA,
  "Grays Harbor", "Grays Harbor Fall",      "Esc",                                 NA,   # composite proxied by Fall stock
  "SUM",  "Mid-Columbia Sum",               "Esc",                                 NA,
  "LRW",  "Lewis",                          "Esc",                                 NA,
  "URB",  "Columbia Upriver Brights",       "Esc",                                 NA,
  "Nehalem",       "Nehalem R.",            "Esc (MR Calib.)",                     "Esc",
  "Siletz",        "Siletz Fall",           "Esc (MR Calib.)",                     "Esc",
  "Siuslaw",       "Siuslaw R.",            "Esc (MR Calib.)",                     "Esc",
  "South Umpqua",  "South Umpqua",          "Esc (Redd or Carcass Ct w MR)",       "Esc (Normative Redd Ct)",
  "Coquille",      "Coquille",              "Esc (MR Calib.)",                     NA,
  "KLM",  "Kitsumkalum",                    "Esc",                                 NA   # KLY (yearling) has no separate series in this CSV -- left unmatched
)

get_esc_for_stock <- function(code, csv_stock, series_primary, series_fallback) {
  series_wanted <- c(series_primary, series_fallback)
  series_wanted <- series_wanted[!is.na(series_wanted)]
  
  wide <- esc_raw %>%
    filter(StockName == csv_stock, SeriesLabel %in% series_wanted) %>%
    select(Year, SeriesLabel, Values) %>%
    pivot_wider(names_from = SeriesLabel, values_from = Values)
  
  if (!series_primary %in% names(wide)) wide[[series_primary]] <- NA_real_
  if (!is.na(series_fallback) && !series_fallback %in% names(wide)) wide[[series_fallback]] <- NA_real_
  
  wide %>%
    transmute(
      stock_code    = code,
      calendar_year = Year,
      escapement    = if (is.na(series_fallback)) {
        .data[[series_primary]]
      } else {
        coalesce(.data[[series_primary]], .data[[series_fallback]])
      }
    )
}

esc_long <- pmap_dfr(
  esc_source_xwalk,
  function(stock_code, csv_stock, series_primary, series_fallback) {
    get_esc_for_stock(stock_code, csv_stock, series_primary, series_fallback)
  }
)

# TST (Taku + Stikine combined) = sum of its two component wild stocks
tst_combo <- esc_long %>%
  filter(stock_code %in% c("TAK", "STI")) %>%
  group_by(calendar_year) %>%
  summarise(
    escapement = if (all(is.na(escapement))) NA_real_ else sum(escapement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(stock_code = "TST", .before = 1)

esc_long <- bind_rows(esc_long, tst_combo)

## 4. Join mortality distribution + escapement, filter to synoptic stocks

aabm_cols     <- c("seak_t","seak_n","seak_s","nbc_t","nbc_s","wcvi_t","wcvi_s")
isbm_cols     <- c("nbcis_t","nbcis_n","nbcis_s","sbcis_t","sbcis_n","sbcis_s",
                   "nfalc_t","nfalc_s","sfalc_t","sfalc_s","wac_n","ps_n","ps_s")
terminal_cols <- c("seakterm_t","seakterm_n","seakterm_s","canterm_n","canterm_s",
                   "usterm_t","usterm_n","usterm_s")

master <- mort_long %>%
  left_join(stock_xwalk, by = "stock_code") %>%
  left_join(river_mouth_lat_xwalk, by = "stock_code") %>%
  left_join(esc_long, by = c("stock_code", "calendar_year")) %>%
  filter(stock_code %in% synoptic_stock_codes) %>%
  relocate(population, region, stock_code, calendar_year, river_mouth_lat,
           escapement, est_cwt, ages, data_status)  %>%
  mutate(across(all_of(pct_col_names), ~ .x / 100,
                .names = "n_{.col}"),
         aabm_er     = rowSums(across(all_of(aabm_cols)),     na.rm = TRUE),
         isbm_er     = rowSums(across(all_of(isbm_cols)),     na.rm = TRUE),
         terminal_er = rowSums(across(all_of(terminal_cols)), na.rm = TRUE),
         stray_er    = stray,
         marine_er   = aabm_er + isbm_er,
         total_er    = aabm_er + isbm_er + terminal_er + stray_er)  %>%
  dplyr::select(-est_cwt, -ages, -data_status)


write.csv(master, "data/PSC_CTC_Chinook_master_table_long.csv", row.names = FALSE, na = "")
