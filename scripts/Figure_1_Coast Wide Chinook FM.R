library(tidyverse)
library(here)
library(readxl)
library(viridis)
library(cowplot) 
library(sf) 

library(scatterpie)  
library(rnaturalearth)
library(rnaturalearthdata) 
library(packcircles) 
library(showtext)  
library(ggnewscale)
library(ggforce) 

region_cols <- c(
  "Washington",
  "Oregon",
  "British Columbia",
  "Alaska"
)

custom_pal <- c(
  "Washington" = "#6A9BA8",   
  "Oregon"    =   "#2E5F6E",    
  "British Columbia"           = "#8A9E7A",  
  "Alaska"                     = "#4A7A50"
)

# Populations included in the Puget Sound inset
puget_sog <- c(
  "Nooksack Spring Fingerling",
  "Skagit Spring Fingerling",
  "Skagit Summer Fingerling",   
  "Stillaguamish Fall Fingerling",
  "Skykomish Fall Fingerling"   
)
##  display labels for the map
label_lookup <- tribble(
  ~population,                       ~label_short,
  
  "Chilkat River",                    "Chilkat",
  "Taku River",                       "Taku",
  "Stikine River",                    "Stikine",
  "Unuk River",                       "Unuk",
  "Atnarko River",                    "Atnarko",
  "Harrison River",                   "Harrison",
  "Lower Shuswap River Summer",       "Shuswap Su",
  "Nicola River Spring",              "Nicola Sp",
  "Cowichan River Fall",              "Cowichan F",
  "Quillayute",                       "Quillayute",
  "Hoh",                              "Hoh",
  "Queets Fall Fingerling",           "Queets F",
  "Lewis River Wild",                 "Lewis",
  "Coweeman",                         "Coweeman",
  "Hanford Wild Brights",             "Hanford Br",
  "Columbia River Upriver Bright",    "Upriver Br",
  "Columbia River Summers",           "Columbia Su",
  "Nehalem",                          "Nehalem",
  "Siletz",                           "Siletz",
  "Siuslaw",                          "Siuslaw",
  "South Umpqua",                     "S. Umpqua",
  "Coquille",                         "Coquille",
  
  # Puget Sound inset
  "Nooksack Spring Fingerling",       "Nooksack Sp",
  "Skagit Spring Fingerling",         "Skagit Sp",
  "Skagit Summer Fingerling",         "Skagit Su",
  "Stillaguamish Fall Fingerling",    "Stillaguamish F",
  "Skykomish Fall Fingerling",        "Skykomish F"
)

# Coastline polygons ==== 
coast <- ne_states(country = c("united states of america", "canada"), 
                   returnclass = "sf")
ocean <- ne_download(scale = 10, type = "ocean", category = "physical", 
                     returnclass = "sf")

# LOAD AND TIDY DATA ===
master<-read_csv("data/PSC_CTC_Chinook_master_table_long.csv") %>%
  select(-river_mouth_lat)  

pct_col_names <- c(
  "seak_t", "seak_n", "seak_s",
  "nbc_t", "nbc_s",
  "wcvi_t", "wcvi_s",
  "nbcis_t", "nbcis_n", "nbcis_s",
  "sbcis_t", "sbcis_n", "sbcis_s",
  "nfalc_t", "nfalc_s",
  "sfalc_t", "sfalc_s",
  "wac_n",
  "ps_n", "ps_s",
  "seakterm_t", "seakterm_n", "seakterm_s",
  "canterm_n", "canterm_s",
  "usterm_t", "usterm_n", "usterm_s",
  "stray", "esc_pct"
)
fishery_pct_cols  <- setdiff(pct_col_names, c("stray", "esc_pct"))
fishery_frac_cols <- paste0("n_", fishery_pct_cols)

catch_distributions <- master %>%
  select(population, region, stock_code, calendar_year, all_of(fishery_frac_cols)) %>%
  pivot_longer(all_of(fishery_frac_cols), names_to = "fishery_region", values_to = "percent_mort") %>%
  filter(!is.na(percent_mort)) %>%
  mutate(broad_region = case_when( 
    str_detect(fishery_region, "^n_seak_")     ~ "Alaska",
    str_detect(fishery_region, "^n_seakterm_") ~ "Alaska", 
    str_detect(fishery_region, "^n_usterm_") &
      population %in% c("Chilkat River", "Stikine River", "Taku River", "Unuk River") ~ "Alaska",
     
    str_detect(fishery_region, "^n_nbc")      ~ "British Columbia",   
    str_detect(fishery_region, "^n_sbcis_")   ~ "British Columbia",
    str_detect(fishery_region, "^n_wcvi_")    ~ "British Columbia",
    str_detect(fishery_region, "^n_canterm_") ~ "British Columbia",
     
    str_detect(fishery_region, "^n_nfalc_")        ~ "Washington",
    fishery_region == "n_wac_n"                    ~ "Washington",
    fishery_region %in% c("n_ps_n", "n_ps_s")      ~ "Washington",
    str_detect(fishery_region, "^n_usterm_") &
      population %in% c("Queets Fall Fingerling", "Quillayute", "Hoh",
                        "Skagit Spring Fingerling", "Skagit Summer Fingerling") ~ "Washington",
     
    str_detect(fishery_region, "^n_sfalc_") ~ "Oregon",
    str_detect(fishery_region, "^n_usterm_") &
      population %in% c("Lewis River Wild", "Columbia River Upriver Bright", "Hanford Wild Brights",
                        "Nehalem", "Siletz", "Siuslaw") ~ "Oregon",
    
    TRUE ~ "Check"  # unmatched -- inspect before treating as real (Harrison,
    # Atnarko, Cowichan, Columbia River Summers usterm, if nonzero)
  )) %>% 
  filter(!broad_region == "Check") # a few stocks have US terminal but fishery was in canada cant assign them

## Per-stock-year broad-region shares  
Poptotal_ER <- catch_distributions %>%
  group_by(population, calendar_year) %>%
  summarise(total_ER_frac = sum(percent_mort, na.rm = TRUE), .groups = "drop")

Pop_plot_df <- catch_distributions %>%
  group_by(calendar_year, population, broad_region) %>%
  summarise(mort_broad_region_frac = sum(percent_mort, na.rm = TRUE), .groups = "drop") %>%
  left_join(Poptotal_ER, by = c("population", "calendar_year")) %>%
  mutate(
    percent_mort_share = mort_broad_region_frac / total_ER_frac,
    broad_region = factor(broad_region, levels = c(
      "Washington", "Oregon", "British Columbia", "Alaska", "Check"
    ))
  )
## Averages across the two time periods ======
# Two PST Agreement periods: 2009-2018 (2009 PST Agreement) and 2019-present
# (2019 PST Agreement). 

max_year <- max(Pop_plot_df$calendar_year, na.rm = TRUE)

Pop_plot_df <- Pop_plot_df %>%
  mutate(period = case_when(
    calendar_year >= 2009 & calendar_year <= 2018 ~ "2009-2018",
    calendar_year >= 2019 & calendar_year <= max_year ~ paste0("2019-", max_year),
    TRUE ~ NA_character_  # pre-2009 years fall outside both periods
  ))

# Per-stock period averages
period_avg_by_pop <- Pop_plot_df %>%
  filter(!is.na(period)) %>%
  group_by(population, broad_region, period) %>%
  summarise(
    mean_percent_mort_share = mean(percent_mort_share, na.rm = TRUE),
    n_years = sum(!is.na(percent_mort_share)),
    .groups = "drop"
  )
 
# TRUE RIVER-MOUTH COORDINATES =========
# actual geographic locations - do not adjust 

true_coords <- tribble(
  ~population,                       ~true_lon,  ~true_lat,
  
  "Chilkat River",                    -135.57,    59.23,
  "Taku River",                       -133.93,    58.27,
  "Stikine River",                    -132.38,    56.70,
  "Unuk River",                       -130.87,    56.08,
  # "Kitsumkalum",                    -128.68,    54.52,
  "Atnarko River",                    -126.75,    52.37,
  
  "Harrison River",                   -121.93,    49.20,
  "Lower Shuswap River Summer",       -121.93,    49.20,
  # "South Thompson",                 -121.93,    49.20,
  "Nicola River Spring",              -121.93,    49.20,
  
  # "Kaouk Fall",                     -127.55,    50.10,
  # "Tahsish Fall",                   -127.48,    50.02,
  # "Artlish Fall",                    -127.42,    49.92,
  # "Megin Fall",                      -126.05,    49.22,
  # "Moyeha Fall",                     -126.03,    49.20,
  # "Bedwell Fall",                    -125.87,    49.12,
  
  # "Puntledge Summer",                -124.93,    49.67,
  "Cowichan River Fall",              -123.72,    48.78,
  # "Nooksack",                        -122.55,    48.78,
  
  # "Skagit Spring",                   -122.37,    48.32,
  # "Skagit Fall",                     -122.37,    48.32,
  # "Stillaguamish",                   -122.38,    48.18,
  # "Snohomish Fall",                  -122.35,    47.92,
  # "Green",                          -122.34,    47.53,
  # "Nisqually",                       -122.71,    47.10,
  
  "Quillayute",                       -124.63,    47.90,
  # "Hoh Spring",                      -124.44,    47.75,
  "Hoh",                               -124.44,    47.75,
  
  # "Queets Spring",                   -124.33,    47.53,
  "Queets Fall Fingerling",           -124.33,    47.53,
  
  # "Grays Harbor Spring",             -124.11,    46.90,
  # "Grays Harbor Fall",               -124.11,    46.90,
  # "Grays",                          -124.11,    46.90,
  # "Phillips",                        -123.90,    46.90,
  
  "Lewis River Wild",                 -122.77,    46.10,
  "Coweeman",                         -122.90,    46.12,
  # "Elochoman",                       -123.41,    46.18,
  
  "Hanford Wild Brights",             -119.49,    46.20,   
  "Columbia River Upriver Bright",    -119.49,    46.20,   
  "Columbia River Summers",           -119.49,    46.20,   
  # "Colonial Fall",                   -122.77,    46.10,
  
  "Nehalem",                          -123.92,    45.71,
  "Siletz",                           -124.00,    44.90,
  "Siuslaw",                          -124.13,    44.01,
  "South Umpqua",                     -124.19,    43.69,
  "Coquille",                         -124.39,    43.12
)


true_coords_PUGETSOUND <- tribble(
  ~population,                       ~true_lon,  ~true_lat,
  
  "Skagit Spring Fingerling",         -122.37,    48.32,
  "Skagit Summer Fingerling",         -122.38,    48.32,    
  "Stillaguamish Fall Fingerling",    -122.38,    48.18,
  "Skykomish Fall Fingerling",        -122.35,    47.92,    
  "Nooksack Spring Fingerling",       -122.55,    48.78
)

# PIE_START_COORDS ===========
# These are not the real river locations, spread westward into the white space.

pie_start_coords <- tribble(
  ~population,                        ~start_lon,  ~start_lat,
  # ALASKA / NORTHERN BC
  
  "Chilkat River",                    -147.5,      58,
  "Taku River",                       -141.0,      58,
  "Stikine River",                    -144.0,      56.7,
  "Unuk River",                       -138.0,      56.2,
  # "Kitsumkalum",                     -140.5,      54.5,
  "Atnarko River",                    -135.5 ,      53.3,
  
  # INTERIOR / CENTRAL BC
  
  "Harrison River",                   -143.0,      51,
  "Lower Shuswap River Summer",       -138.0,      50.2,
  # "South Thompson",                  -133.0,      49.8,
  "Nicola River Spring",              -130.0,     50.3,
  
  # VANCOUVER ISLAND
  
  # "Kaouk Fall",                      -141.0,      50.3,
  # "Tahsish Fall",                    -137.0,      50.0,
  # "Artlish Fall",                    -137.5,      49.8,
  # "Megin Fall",                      -132.0,      49.4,
  # "Moyeha Fall",                     -128.0,      49.1,
  # "Bedwell Fall",                    -124.0,      48.9,
  
  # SOUTHERN BC
  
  # "Puntledge Summer",                -139.0,      48.7,
  "Cowichan River Fall",              -132.0,      49,
  # "Nooksack",                        -134.0,      48.3,
  
  # WASHINGTON
  
  # "Skagit Spring",                   -141.0,      48.1,
  # "Skagit Fall",                     -136.0,      47.9,
  # "Stillaguamish",                   -131.0,      47.7,
  # "Snohomish Fall",                  -126.0,      47.5,
  # "Green",                          -141.0,      47.2,
  # "Nisqually",                       -135.0,      47.0,
  
  # OLYMPIC PENINSULA
  
  "Quillayute",                       -134.5,      47.9,
  # "Hoh Spring",                      -134.5,      47.7,
  "Hoh",                              -132,      48.0,
  # "Queets Spring",                   -130.0,      47.5,
  "Queets Fall Fingerling",           -140.0,      46.8,
  
  # GRAYS HARBOR / SOUTHWEST WASHINGTON
  
  # "Grays Harbor Spring",             -137.0,      46.8,
  # "Grays Harbor Fall",               -136.0,      46.9,
  # "Grays",                          -130.0,      46.4,
  # "Phillips",                        -140.0,      46.2,
  
  # LOWER COLUMBIA / SOUTHWEST WASHINGTON
  
  "Lewis River Wild",                 -130,      45,
  "Coweeman",                         -132,      46.2,
  # "Elochoman",                       -126.0,      45.3,
  
  # COLUMBIA / INTERIOR
  
  "Hanford Wild Brights",             -139.0,      45.0,    
  "Columbia River Upriver Bright",    -139.0,      44.5,    
  "Columbia River Summers",           -133.0,      45.0,    
  # "Colonial Fall",                   -133.0,      44.8,
  
  # OREGON
  
  "Nehalem",                          -128.5,      45.5,
  "Siletz",                           -140.0,      44 ,
  "Siuslaw",                          -130,      45,
  "South Umpqua",                     -128.5,      42.5,
  "Coquille",                         -133,      43.2
)

# PACK THE PIE STARTING POSITIONS ===== 
pie_radius <- 1.4 # adjust for radius white space around pie to be larger or smaller 

north_to_south_order <- c(
  # Alaska
  "Unuk River",
  "Stikine River",
  "Taku River",
  "Chilkat River",
  
  # British Columbia
  "Kitsumkalum",
  "Atnarko River",
  "Harrison River",
  "Lower Shuswap River Summer",
  "South Thompson",
  "Nicola River Spring",
  "Kaouk Fall",
  "Tahsish Fall",
  "Artlish Fall",
  "Megin Fall",
  # "Moyeha Fall",
  "Bedwell Fall",
  "Puntledge Summer",
  "Cowichan River Fall",
  
  # Washington
  "Nooksack Spring Fingerling",
  "Skagit Spring Fingerling",
  "Skagit Summer Fingerling",
  "Stillaguamish Fall Fingerling",
  "Skykomish Fall Fingerling",
  "Green",
  "Nisqually",
  "Quillayute",
  "Hoh Spring",
  "Hoh",
  "Queets Spring",
  "Queets Fall Fingerling",
  "Grays Harbor Spring",
  "Grays Harbor Fall",
  "Grays",
  "Phillips",
  
  # Columbia tributaries
  "Lewis River Wild",
  "Coweeman",
  "Elochoman",
  "Hanford Wild Brights",
  "Columbia River Upriver Bright",
  "Columbia River Summers",
  "Colonial Fall",
  
  # Oregon
  "Nehalem",
  "Siletz",
  "Siuslaw",
  "South Umpqua",
  "Coquille"
)

pack_input <- pie_start_coords %>%
  mutate(
    radius = pie_radius,
    x = start_lon * 0.6,
    y = start_lat
  )

packed <- circleRepelLayout(
  data.frame(
    x = pack_input$x,
    y = pack_input$y,
    r = pack_input$radius
  ),
   
  wrap = FALSE,
   
  sizetype = "radius", 
  maxiter = 1000, 
  xysizecols = c(1, 2, 3)
)

# EXTRACT FINAL PIE POSITIONS
repelled_coords <- pie_start_coords %>%
  mutate(
    lon = packed$layout$x / 0.6,
    lat = packed$layout$y
  ) %>%
  select(
    population,
    lon,
    lat
  )

# MANUAL NUDGES FOR INDIVIDUAL PIE CHARTS 
pie_nudges <- tribble(
  ~population,       ~dx,    ~dy,
  "Harrison",         0.0,    0.5,
  "Lower Shuswap",    0.7,    0.0,
  "Nicola",           1.0,   -0.4
)

repelled_coords <- repelled_coords %>%
  left_join(pie_nudges, by = "population") %>%
  mutate(
    dx = replace_na(dx, 0),
    dy = replace_na(dy, 0),
    lon = lon + dx,
    lat = lat + dy
  ) %>%
  select(population, lon, lat)

# COAST / MAP COLORS
coast <- coast %>%
  mutate(
    broad_region = case_when(
      name %in% c("Alaska")             ~ "Alaska",
      name %in% c("British Columbia")   ~ "British Columbia",
      name %in% c("Washington")         ~ "Washington",
      name %in% c("Oregon")             ~ "Oregon",
      TRUE                              ~ "Other"
    )
  )

custom_pal_map <- c(
  custom_pal,
  "Other" = "white"
) 

# TREATY PERIOD 1  ============================================================
# Plot and make DF 
pie_df_TREATY1 <- Pop_plot_df %>%
  filter(period ==  "2009-2018"
  ) %>% 
  group_by(period, broad_region, population) %>%
  summarise(avg_mort = mean(mort_broad_region_frac)) %>% 
  arrange(
    desc(broad_region)
  ) %>%
  
  mutate(
    
    custom_region = case_when(
      broad_region == "Alaska"           ~ "AK",
      broad_region == "British Columbia" ~ "BC",
      broad_region == "Washington"       ~ "WA",
      broad_region == "Oregon"           ~ "OR",
      TRUE                              ~ NA_character_
    )) 


## JOIN PIE DATA TO BOTH TYPES OF COORDINATES ==== 
pie_coords_TREATY1 <- pie_df_TREATY1 %>%
  dplyr::select(period, population, broad_region, avg_mort) %>%
  tidyr::pivot_wider(
    names_from  = broad_region,
    values_from = avg_mort,
    values_fill = 0
  ) %>%
  left_join(true_coords, by = "population") %>%
  dplyr::rename(lon = true_lon, lat = true_lat)

pie_coords_repelled_TREATY1 <- pie_coords_TREATY1 %>%
  select(-lat, -lon) %>%
  left_join(true_coords, by = "population") %>%
  left_join(repelled_coords, by = "population") %>%
  left_join(label_lookup, by = "population")

## MAIN MAP =======
main_map <- ggplot() + 
  geom_sf(
    data = coast,
    aes(
      fill = broad_region
    ),
    color = "lightgray",
    linewidth = 0.3
  ) +
  
  scale_fill_manual(
    values = custom_pal_map,
    guide = "none"
  ) +
  annotate(
    "rect",
    xmin = -123.25,
    xmax = -121.9,
    ymin = 46.8,
    ymax = 48.95,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) + 
  ggnewscale::new_scale_fill() +
  
  # LEADER LINES
  # Pie centre to actual river mouth
  
  geom_segment(
    data = pie_coords_repelled_TREATY1 %>% filter(!population %in% puget_sog),
    
    aes(
      x = lon,
      y = lat,
      xend = true_lon,
      yend = true_lat
    ),
    
    color = "grey40",
    linewidth = 0.4,
    linetype = "dotted"
  ) +
  geom_label(
    data = pie_coords_repelled_TREATY1 %>% filter(!population %in% puget_sog),
    
    aes(
      x = lon,# + dx,
      y = lat + 1.4,# + dy,
      label = label_short
    ),
    fontface = "bold",
    size = 6,
    fill = "white",
    color = "black",
    label.size = 0,
    label.padding = unit(0, "lines")
    # 
    # size = 15,
    # fontface = "bold",
    # hjust = 0.5,
    # vjust = 0
  ) + 
  # TRUE RIVER-MOUTH DOT
  
  geom_point(
    data = pie_coords_repelled_TREATY1 %>% filter(!population %in% puget_sog),
    
    aes(
      x = true_lon,
      y = true_lat
    ),
    
    size = 1.5,
    color = "grey30"
  ) +
  
  # PIE CHARTS
  
  geom_scatterpie(
    data = pie_coords_repelled_TREATY1%>% filter(!population %in% puget_sog),
    aes(
      x = lon,
      y = lat,
      group = population
    ),
    cols = region_cols,
    pie_scale = 2.7,
    color = "black",
    linewidth = 0.3,
    alpha = 0.9
  ) + 
  
  # PIE LEGEND
  
  scale_fill_manual(
    values = custom_pal,
    name = "Fishery Region"
  ) +
  
  # MAP EXTENT
  
  coord_sf(
    xlim = c(-155, -120),
    ylim = c(41, 61),
    expand = FALSE
  ) +
  
  ggtitle( "Treaty Period: 2009-2018") + 
 
  theme_void() +
  
  theme(  
    title = element_text(size = 40), 
    
    legend.position = "left",
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(1.2, "cm"),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    )
  )


## PUGET SOUND INSET ============================================================
puget_pie_positions <- tribble(
  ~population,                     ~lon,       ~lat,
  
  "Nooksack Spring Fingerling",    -122.73,    48.74,
  "Skagit Spring Fingerling",      -122.15,    48.50,
  "Skagit Summer Fingerling",      -122.66,    48.18,    
  "Stillaguamish Fall Fingerling", -121.92,    47.98,
  "Skykomish Fall Fingerling",     -122.3,     47.58     
)

## GET PUGET SOUND PIE DATA =========
pie_coords_PUGET <- pie_coords_TREATY1 %>%
  
  filter(
    population %in% puget_sog
  ) %>%
   
  select(
    -any_of(c("lon", "lat"))
  ) %>%
   
  left_join(
    true_coords_PUGETSOUND,
    by = "population"
  ) %>% 
  left_join(
    puget_pie_positions,
    by = "population"
  ) %>%
   
  left_join(
    label_lookup,
    by = "population"
  )


## PUGET SOUND INSET MAP  ============================================================
puget_map <- ggplot() +
 
  geom_sf(
    data = coast,
    aes(
      fill = broad_region
    ),
    color = "lightgray",
    linewidth = 0.3
  ) +
  
  scale_fill_manual(
    values = custom_pal_map,
    guide = "none"
  ) +
   
  ggnewscale::new_scale_fill() +
  # LEADER LINES
  
  geom_segment(
    data = pie_coords_PUGET,
    
    aes(
      x = lon,
      y = lat,
      xend = true_lon,
      yend = true_lat
    ),
    
    color = "black",
    linewidth = 0.8,
    linetype = "solid"
  ) +
  # ACTUAL RIVER LOCATIONS
  
  geom_point(
    data = pie_coords_PUGET,
    
    aes(
      x = true_lon,
      y = true_lat
    ),
    
    size = 2,
    color = "black"
  ) +
  
  # PUGET SOUND PIE CHARTS
  
  geom_scatterpie(
    data = pie_coords_PUGET,
    
    aes(
      x = lon,
      y = lat,
      group = population
    ),
    
    cols = region_cols,
    
    pie_scale =8,
    
    color = "black",
    linewidth = 0.7#,
    #alpha = 0.9
  ) +
  # LABELS
  
  geom_text(
    data = pie_coords_PUGET,
    
    aes(
      x = lon + -0.1,
      y = lat + 0.15,
      label = label_short
    ),
    size = 6, 
    fontface = "bold",
    label.size = 0,
    # fill = "white",
    # alpha = 0.85,
    hjust = 0.5,
    vjust = 0
  ) +
  
  scale_fill_manual(
    values = custom_pal#,
    # name = "Fishery Region"
  ) + 
  
  coord_sf(
    xlim = c(-123.2, -121.54),
    ylim = c(47, 48.97),
    expand = FALSE
  ) +
  
  theme_void() +
  
  theme(
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "none",
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    )
  )

## PUT MAIN MAP + PUGET INSET NEXT TO EACH OTHER =======
final_map <- cowplot::ggdraw() +
  
  # Main map
  cowplot::draw_plot(
    main_map,
    x = 0,
    y = 0,
    width = 1,
    height = 1
  ) +
  
  # Puget Sound inset
  cowplot::draw_plot(
    puget_map,
    x = 0.1,
    y = 0.005,
    width = 0.33,
    height = 0.38
  )   
### SAVE  =========
ggsave(
  "output/plots/population_pie_map_TREATY1.png",
  
  plot = final_map,
  
  width = 15,
  height = 12,
  
  bg = "white",
  dpi = 300
)

# TREATY PERIOD 2  ============================================================
pie_df_TREATY2 <- Pop_plot_df %>%
  filter(
    period == "2019-2024" 
  ) %>% 
  group_by(period, broad_region, population) %>%
  summarise(avg_mort = mean(mort_broad_region_frac)) %>% 
  arrange(
    desc(broad_region)
  ) %>%
  
  dplyr::mutate(
    custom_region = case_when(
      broad_region == "Alaska"           ~ "AK",
      broad_region == "British Columbia" ~ "BC",
      broad_region == "Washington"       ~ "WA",
      broad_region == "Oregon"           ~ "OR",
      TRUE                              ~ NA_character_
    )) 

pie_coords_TREATY2 <- pie_df_TREATY2 %>%
  dplyr::select(population, broad_region, avg_mort) %>%
  tidyr::pivot_wider(
    names_from  = broad_region,
    values_from = avg_mort,
    values_fill = 0
  ) %>%
  left_join(true_coords, by = "population") %>%
  dplyr::rename(lon = true_lon, lat = true_lat)

pie_coords_repelled_TREATY2 <- pie_coords_TREATY2 %>%
  select(-lat, -lon) %>%
  left_join(true_coords, by = "population") %>%
  left_join(repelled_coords, by = "population") %>%
  left_join(label_lookup, by = "population")

## MAIN MAP =======
main_map <- ggplot() +
 
  geom_sf(
    data = coast,
    aes(
      fill = broad_region
    ),
    color = "lightgray",
    linewidth = 0.3
  ) +
  
  scale_fill_manual(
    values = custom_pal_map,
    guide = "none"
  ) +
  annotate(
    "rect",
    xmin = -123.25,
    xmax = -121.9,
    ymin = 46.8,
    ymax = 48.95,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) + 
  
  ggnewscale::new_scale_fill() +
  
  # LEADER LINES 
  geom_segment(
    data = pie_coords_repelled_TREATY2 %>% filter(!population %in% puget_sog),
    
    aes(
      x = lon,
      y = lat,
      xend = true_lon,
      yend = true_lat
    ),
    
    color = "grey40",
    linewidth = 0.4,
    linetype = "dotted"
  ) +
  
  geom_label(
    data = pie_coords_repelled_TREATY2 %>% filter(!population %in% puget_sog),
    
    aes(
      x = lon,# + dx,
      y = lat + 1.4,# + dy,
      label = label_short
    ),
    fontface = "bold",
    size = 6,
    fill = "white",
    color = "black",
    label.size = 0,
    label.padding = unit(0, "lines")
    # 
    # size = 15,
    # fontface = "bold",
    # hjust = 0.5,
    # vjust = 0
  ) + 
  # TRUE RIVER-MOUTH DOT
  
  geom_point(
    data = pie_coords_repelled_TREATY2 %>% filter(!population %in% puget_sog),
    
    aes(
      x = true_lon,
      y = true_lat
    ),
    
    size = 1.5,
    color = "grey30"
  ) +
  
  # PIE CHARTS
  
  geom_scatterpie(
    data = pie_coords_repelled_TREATY2 %>% filter(!population %in% puget_sog),
    aes(
      x = lon,
      y = lat,
      group = population
    ),
    cols = region_cols,
    pie_scale = 2.7,
    color = "black",
    linewidth = 0.3,
    alpha = 0.9
  ) + 
  
  # PIE LEGEND
  
  scale_fill_manual(
    values = custom_pal,
    name = "Fishery Region"
  ) +
 
  coord_sf(
    xlim = c(-155, -120),
    ylim = c(41, 61),
    expand = FALSE 
 ) +
 
  ggtitle("Treaty Period: 2019-2024") + 
 
  theme_void() +
  
  theme(
    title = element_text(size = 40), 
    
    legend.position = "left",
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(1.2, "cm"),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    )
  )

## PUGET SOUND INSET ============================================================

pie_coords_PUGET <- pie_coords_TREATY2 %>%
  
  filter(
    population %in% puget_sog
  ) %>%
  
  # Remove old pie coordinates
  select(
    -any_of(c("lon", "lat"))
  ) %>%
   
  left_join(
    true_coords_PUGETSOUND,
    by = "population"
  ) %>%
   
  left_join(
    puget_pie_positions,
    by = "population"
  ) %>%
  
  left_join(
    label_lookup,
    by = "population"
  )

## PUGET SOUND INSET MAP  ============================================================
puget_map <- ggplot() +
  geom_sf(
    data = coast,
    aes(
      fill = broad_region
    ),
    color = "lightgray",
    linewidth = 0.3
  ) +
  
  scale_fill_manual(
    values = custom_pal_map,
    guide = "none"
  ) +
   
  ggnewscale::new_scale_fill() +
  # LEADER LINES
  
  geom_segment(
    data = pie_coords_PUGET,
    
    aes(
      x = lon,
      y = lat,
      xend = true_lon,
      yend = true_lat
    ),
    
    color = "black",
    linewidth = 0.8,
    linetype = "solid"
  ) +
  # ACTUAL RIVER LOCATIONS
  
  geom_point(
    data = pie_coords_PUGET,
    
    aes(
      x = true_lon,
      y = true_lat
    ),
    
    size = 2,
    color = "black"
  ) +
  
  # PUGET SOUND PIE CHARTS
  
  geom_scatterpie(
    data = pie_coords_PUGET,
    
    aes(
      x = lon,
      y = lat,
      group = population
    ),
    
    cols = region_cols,
    
    pie_scale =8,
    
    color = "black",
    linewidth = 0.7#,
    #alpha = 0.9
  ) +
  # LABELS
  
  geom_text(
    data = pie_coords_PUGET,
    
    aes(
      x = lon + -0.1,
      y = lat + 0.15,
      label = label_short
    ),
    size = 6, 
    fontface = "bold",
    label.size = 0,
    # fill = "white",
    # alpha = 0.85,
    hjust = 0.5,
    vjust = 0
  ) +
  
  scale_fill_manual(
    values = custom_pal#,
    # name = "Fishery Region"
  ) + 
  
  coord_sf(
    xlim = c(-123.2, -121.54),
    ylim = c(47, 48.97),
    expand = FALSE
  ) +
  
  theme_void() +
  
  theme(
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "none",
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    )
  )

## PUT MAIN MAP + PUGET INSET NEXT TO EACH OTHER =======
final_map2 <- cowplot::ggdraw() +
  
  # Main map
  cowplot::draw_plot(
    main_map,
    x = 0,
    y = 0,
    width = 1,
    height = 1
  ) +
  
  # Puget Sound inset
  cowplot::draw_plot(
    puget_map,
    x = 0.1,
    y = 0.005,
    width = 0.33,
    height = 0.38
  )   

### SAVE  =========
ggsave(
  "output/plots/population_pie_map_TREATY2.png",
  
  plot = final_map2,
  
  width = 15,
  height = 12,
  
  bg = "white",
  dpi = 300
)
