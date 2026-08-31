 
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

puget_sog <- c("Nooksack",  
               "Skagit Spring",        
               "Skagit Fall",           
               "Stillaguamish",          
               "Snohomish Fall",         
               "Nooksack") 

data <- readxl::read_excel("data/PSTChinookCWT_data_April18_2025.xlsx") %>%
  filter(!year == 2023, !population %in% c("Elk", "Cowlitz.fa","Middle_Shuswap") ) #NAs 

total_run_df<-data %>%  
  dplyr::select( year,population,region,total_run) %>%
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
    "term_tot")) %>%
  dplyr::mutate(percent_mort = as.numeric(percent_mort)/100) 

group_df <- catch_distributions %>%
  left_join(total_run_df) %>% 
  filter(!is.na(percent_mort )) %>% 
  dplyr::mutate(mortality_numbers = total_run*percent_mort,
                broad_region = case_when(
                  # Alaska
                  str_detect(fishery_region, "^seak") ~ "Alaska",
                  str_detect(fishery_region, "^ak_term")  ~ "Alaska",
                  str_detect(fishery_region, "^US_term") & population %in% c("Chilkat","Stikine", "Taku", "Unuk") ~  "Alaska", 
                   
                  # if its US terminal net and the regoins are candian remove from data ---- 
                  # change this so it is based off of the run, goes to that state. 
                  str_detect(fishery_region, "^US_term") & population %in% c("Grays" ,"Nisqually","Green","Nooksack","Queets_fa","Hoh_fa", "Hoh_sp",
                                                                             "Queets_sp","Grays_Harbor_fa","Grays_Harbor_sp", "Quillayute_fa","Skagit_sp",
                                                                             "Snohomish_fa", "Skagit_fa", "Stillaguamish","Elochoman",
                                                                             "Nicola","Phillips") ~ "Washington",
                  str_detect(fishery_region, "^US_term") & population %in% c("Cowlitz.fa", "Elk", "South_umpqua","Coquille","Coweeman", "Lewis",
                                                                             "Nehalem_fa","Siletz_fa","Siuslaw_fa","Hanford_br_w") ~ "Oregon",

                  str_detect(fishery_region, "^US_term") & region %in% c("WCVI","SBC","NBC") ~ "filter", # US terminal should not have BC fish, this is from Will doing an expand grid most likely 
                  
                   # Canada
                  str_detect(fishery_region, "^can_term")  ~ "British Columbia",
                  str_detect(fishery_region, "^wcvi")     ~ "British Columbia",#"West Coast Vancouver Island",
                  str_detect(fishery_region, "^nbc")      ~ "British Columbia", #"North Coast BC",
                  str_detect(fishery_region, "^sbc")      ~ "British Columbia",
                  
                  # str_detect(fishery_region, "^can_term") ~ "Canada Terminal",
                  
                  # Falcon (US)
                  str_detect(fishery_region, "^sfalc")    ~ "Oregon",
                  # str_detect(fishery_region, "^nfalc")    ~ "North of Falcon",
                  
                  fishery_region == "nfalc_s" ~ "Washington",
                  fishery_region == "nfalc_t" ~ "Washington",
                  fishery_region == "US_is_tot" ~ "Washington",
                  
                  # WA     
                  fishery_region == "PS_n" ~ "Washington",
                  fishery_region == "PS_s" ~ "Washington",
                  
                  fishery_region == "wac_n" ~ "Washington",
                  # Other
                  TRUE                                    ~ "Check")) %>%
  filter(!broad_region == "filter")

# group by population 
Poptotal_FMnumbers <- group_df %>%
  group_by(population,year) %>%
  # get annual sum of all fishery mortality from OP for the year
  dplyr::summarise(total_FM_numbers = sum(mortality_numbers))  # want to ask, of all the fish caught that year, what was the relative proportions. Not related to overall runsize. 

Pop_plot_df<- group_df %>% 
  dplyr::select(-c(total_run, percent_mort,fishery_region)) %>%
  ungroup() %>%
  group_by(year, population,broad_region) %>%
  dplyr::summarise(mort_broad_region = sum(mortality_numbers)) %>% 
  left_join(Poptotal_FMnumbers) %>%
  dplyr::mutate(percent_mort = mort_broad_region/total_FM_numbers,
                broad_region = factor(broad_region, levels = c(
                  "Washington",
                  "Oregon",
                  "British Columbia",
                  "Alaska"   
                )))
  
north_to_south_order <- c(
  # Alaska
  "Unuk",
  "Stikine",
  "Taku",
  "Chilkat",
  
  # British Columbia
  "Kitsumkalum",
  "Atnarko",
  "Harrison",
  "Lower Shuswap",
  "South Thompson",
  "Nicola",
  "Kaouk Fall",
  "Tahsish Fall",
  "Artlish Fall",
  "Megin Fall",
  # "Moyeha Fall",
  "Bedwell Fall",
  "Puntledge Summer",
  "Cowichan",
  
  # Washington 
  "Nooksack",
  "Skagit Spring",
  "Skagit Fall",
  "Stillaguamish",
  "Snohomish Fall",
  "Green",
  "Nisqually",
  "Quillayute Fall",
  "Hoh Spring",
  "Hoh Fall",
  "Queets Spring",
  "Queets Fall",
  "Grays Harbor Spring",
  "Grays Harbor Fall",
  "Grays",
  "Phillips",
  
  # Columbia tributaries
  "Lewis",
  "Coweeman",
  "Elochoman",
  "Hanford Upriver Bright",
  "Colonial Fall",
  
  # Oregon
  "Nehalem Fall",
  "Siletz Fall",
  "Siuslaw Fall",
  "South Umpqua",
  "Coquille"
)
 
# Map Plot with Pie Charts ===========
# Coastline polygons
coast <- ne_states(country = c("united states of america", "canada"), 
                   returnclass = "sf")
ocean <- ne_download(scale = 10, type = "ocean", category = "physical", 
                     returnclass = "sf")
 
# TRUE RIVER-MOUTH COORDINATES =========
# These are the actual geographic locations.
# DO NOT shift these.
 
true_coords <- tribble(
  ~population,              ~true_lon,  ~true_lat,
  
  "Chilkat",                 -135.57,    59.23,
  "Taku",                    -133.93,    58.27,
  "Stikine",                 -132.38,    56.70,
  "Unuk",                    -130.87,    56.08,
  # "Kitsumkalum",             -128.68,    54.52,
  "Atnarko",                 -126.75,    52.37,
  
  "Harrison",                -121.93,    49.20,
  "Lower Shuswap",           -121.93,    49.20,
  # "South Thompson",          -121.93,    49.20,
  "Nicola",                  -121.93,    49.20,
  
  # "Kaouk Fall",              -127.55,    50.10,
  # "Tahsish Fall",            -127.48,    50.02,
  # "Artlish Fall",             -127.42,    49.92,
  # "Megin Fall",               -126.05,    49.22,
  # "Moyeha Fall",              -126.03,    49.20,
  # "Bedwell Fall",             -125.87,    49.12,
  
  # "Puntledge Summer",        -124.93,    49.67,
  "Cowichan",                -123.72,    48.78,
  # "Nooksack",                -122.55,    48.78,
  
  # "Skagit Spring",           -122.37,    48.32,
  # "Skagit Fall",              -122.37,    48.32,
  # "Stillaguamish",            -122.38,    48.18,
  # "Snohomish Fall",           -122.35,    47.92,
  # "Green",                   -122.34,    47.53,
  # "Nisqually",               -122.71,    47.10,
  
  # "Quillayute Fall",         -124.63,    47.90,
  # "Hoh Spring",              -124.44,    47.75,
  # "Hoh Fall",                -124.44,    47.75,
  
  # "Queets Spring",           -124.33,    47.53,
  "Queets Fall",             -124.33,    47.53,
  
  #"Grays Harbor Spring",     -124.11,    46.90,
  # "Grays Harbor Fall",       -124.11,    46.90,
  # "Grays",                   -124.11,    46.90,
  # "Phillips",                -123.90,    46.90,
  
  "Lewis",                   -122.77,    46.10,
  "Coweeman",                -122.90,    46.12,
  # "Elochoman",               -123.41,    46.18,
  
  "Hanford Bridge Winter",   -119.49,    46.20,
  # "Colonial Fall",           -122.77,    46.10,
  
  "Nehalem Fall",            -123.92,    45.71,
  "Siletz Fall",             -124.00,    44.90,
  "Siuslaw Fall",            -124.13,    44.01,
  "South Umpqua",            -124.19,    43.69,
  "Coquille",                -124.39,    43.12
)


true_coords_PUGETSOUND <- tribble(
  ~population,              ~true_lon,  ~true_lat,
     
  "Skagit Spring",           -122.37,    48.32,
  "Skagit Fall",              -122.38,    48.32,
  "Stillaguamish",            -122.38,    48.18,
  "Snohomish Fall",           -122.35,    47.92,
  "Nooksack",                -122.55,    48.78
)


 # pie_start_coords ===========
# These are NOT the real river locations.
#
# These are deliberately spread westward into the white space.
# The true river-mouth coordinates remain in true_coords.
#
# Every population in true_coords is included here.
 
pie_start_coords <- tribble(
  ~population,              ~start_lon,  ~start_lat,
  # ALASKA / NORTHERN BC
 
  "Chilkat",                -147.5,      58,
  "Taku",                   -141.0,      58,
  "Stikine",                -144.0,      56.7,
  "Unuk",                   -138.0,      56.2,
  # "Kitsumkalum",            -140.5,      54.5,
  "Atnarko",                -136.5,      53.2,
  
 # INTERIOR / CENTRAL BC
  
  "Harrison",               -143.0,      51,
  "Lower Shuswap",          -138.0,      50.2,
  # "South Thompson",         -133.0,      49.8,
  "Nicola",                 -130.0,     50.3,
  
 # VANCOUVER ISLAND
  
  # "Kaouk Fall",             -141.0,      50.3,
  # "Tahsish Fall",           -137.0,      50.0,
  # "Artlish Fall",           -137.5,      49.8,
  # "Megin Fall",             -132.0,      49.4,
  # "Moyeha Fall",            -128.0,      49.1,
  # "Bedwell Fall",           -124.0,      48.9,
  
   # SOUTHERN BC
  
  # "Puntledge Summer",       -139.0,      48.7,
  "Cowichan",               -134.0,      49,
  # "Nooksack",               -134.0,      48.3,
  
   # WASHINGTON
  
  # "Skagit Spring",          -141.0,      48.1,
  # "Skagit Fall",            -136.0,      47.9,
  # "Stillaguamish",          -131.0,      47.7,
  # "Snohomish Fall",         -126.0,      47.5,
  # "Green",                  -141.0,      47.2,
  # "Nisqually",              -135.0,      47.0,
  # 
  
   # OLYMPIC PENINSULA
  
  # "Quillayute Fall",        -136.5,      47.9,
  # "Hoh Spring",             -134.5,      47.7,
  # "Hoh Fall",               -134.5,      48.0,
  # "Queets Spring",          -130.0,      47.5,
  "Queets Fall",            -140.0,      46.8,
  
  # GRAYS HARBOR / SOUTHWEST WASHINGTON
  
  # "Grays Harbor Spring",    -137.0,      46.8,
  # "Grays Harbor Fall",      -136.0,      46.9,
  # "Grays",                  -130.0,      46.4,
  # "Phillips",               -140.0,      46.2,
  
   # LOWER COLUMBIA / SOUTHWEST WASHINGTON
 
  "Lewis",                  -139,      46,
  "Coweeman",               -132,      46.2,
  # "Elochoman",              -126.0,      45.3,
  
  # COLUMBIA / INTERIOR
  
  "Hanford Bridge Winter",  -139.0,      45.0,
  # "Colonial Fall",          -133.0,      44.8,
   
 # OREGON
  
  "Nehalem Fall",           -128.0,      45.5,
  "Siletz Fall",            -140.0,      44 ,
  "Siuslaw Fall",           -138,      42.5,
  "South Umpqua",           -128.5,      42.5,
  "Coquille",               -133,      43.2
)
 
# PACK THE PIE STARTING POSITIONS
#
# Important:
# circleRepelLayout() normally interprets size as AREA.
# Here we explicitly say radius so that the radius value
# corresponds more closely to the visual size of the pies.
 
pie_radius <- 1.2

pack_input <- pie_start_coords %>%
  mutate(
    radius = pie_radius,
    
    # Scale longitude because degrees longitude are visually
    # compressed relative to latitude.
    x = start_lon * 0.6,
    y = start_lat
  )

packed <- circleRepelLayout(
  data.frame(
    x = pack_input$x,
    y = pack_input$y,
    r = pack_input$radius
  ),
  
  # Keep the layout from wrapping around the map
  wrap = FALSE,
  
  # Explicitly treat r as a radius
  sizetype = "radius",
  
  # More iterations = more complete repulsion
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
# dx = move left/right
# dy = move down/up
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
  
 
  
# 5. COAST / MAP COLORS
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
  filter(
    year > 2007,
    year < 2019
  ) %>%
  
  group_by(
    broad_region,
    population
  ) %>%
  
  summarise(
    avg_mort = mean(percent_mort, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
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
    ),
    
    population = case_when(
      
      grepl("_fa$", population) ~
        paste(gsub("_fa$", "", population), "Fall"),
      
      grepl("_sp$", population) ~
        paste(gsub("_sp$", "", population), "Spring"),
      
      grepl("_su$", population) ~
        paste(gsub("_su$", "", population), "Summer"),
      
      grepl("_br_w$", population) ~
        paste(gsub("_br_w$", "", population), "Upriver Bright"),
      
      population == "South_umpqua" ~
        "South Umpqua",
      
      TRUE ~
        gsub("_", " ", population)
    ),
    
    population = gsub(
      "_",
      " ",
      population
    ),
    
    population = factor(
      population,
      levels = north_to_south_order
    )
  )

 # 7. JOIN PIE DATA TO BOTH TYPES OF COORDINATES
#
# true_lon / true_lat = actual river mouth
# lon / lat             = final pie location
#  PIVOT TO WIDE FORMAT FOR SCATTERPIE
#
# scatterpie needs one column per pie slice (region),
# so we pivot pie_df_TREATY1 from long to wide.
 
pie_coords_TREATY1 <- pie_df_TREATY1 %>%
  dplyr::select(population, broad_region, avg_mort) %>%
  tidyr::pivot_wider(
    names_from  = broad_region,
    values_from = avg_mort,
    values_fill = 0
  ) %>%
  # attach starting lat/lon (these get dropped/rebuilt in step 7,
  # but pie_coords_TREATY1 needs lat/lon columns to exist for the
  # select(-lat, -lon) call downstream)
  left_join(true_coords, by = "population") %>%
  dplyr::rename(lon = true_lon, lat = true_lat)

pie_coords_repelled_TREATY1 <- pie_coords_TREATY1 %>%
  select(-lat, -lon) %>%
  left_join(true_coords, by = "population") %>%
  left_join(repelled_coords, by = "population") #%>%
 
 ## MAIN MAP =======
main_map <- ggplot() +
  
 # MAP
 
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
  # Start a NEW fill scale so the map colors and pie colors
  # don't interfere with one another.
  ggnewscale::new_scale_fill() +
   
# LEADER LINES
#
# Pie centre -> actual river mouth
 
geom_segment(
  data = pie_coords_repelled_TREATY1,
  
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
    data = pie_coords_repelled_TREATY1,
    
    aes(
      x = lon,# + dx,
      y = lat + 1.4,# + dy,
      label = population
    ),
    fontface = "bold",
    size = 12,
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
  data = pie_coords_repelled_TREATY1,
  
  aes(
    x = true_lon,
    y = true_lat
  ),
  
  size = 1.5,
  color = "grey30"
) +
  
 # PIE CHARTS
 
  geom_scatterpie(
    data = pie_coords_repelled_TREATY1,
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

  # LABELS
#
# Just a little above each pie.
#
# Change 0.65 to:
#
#   0.5 = closer
#   0.8 = farther away
#   1.0 = much farther away
 
   
  
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

  ggtitle( "Treaty Period 2009-2018") + 
  
 # THEME
 
theme_void() +
  
  theme(
    title = element_text(size = 50), 
   
     legend.position = "left",
    legend.title = element_text(size = 35),
    legend.text = element_text(size = 30),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(1.2, "cm"),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.2
    )
  )
 
## PUGET SOUND INSET ============================================================
 
# Populations included in the Puget Sound inset
puget_sog <- c(
  "Nooksack",
  "Skagit Spring",
  "Skagit Fall",
  "Stillaguamish",
  "Snohomish Fall"
)

 
### Pie start coords  ============================================================
# lon / lat = where the pie is DRAWN in the inset
# true_lon / true_lat = actual river location
#
# Change lon/lat here if you want to move individual pies.
puget_pie_positions <- tribble(
  ~population,        ~lon,       ~lat,
  
  "Nooksack",         -122.73,    48.74,
  "Skagit Spring",    -122.15,    48.50,
  "Skagit Fall",      -122.66,    48.18,
  "Stillaguamish",    -121.92,    47.98,
  "Snohomish Fall",   -122.3,    47.58
)

## GET PUGET SOUND PIE DATA =========
 
pie_coords_PUGET <- pie_coords_TREATY1 %>%
  
  filter(
    population %in% puget_sog
  ) %>%
  
  # Remove old pie coordinates
  select(
    -any_of(c("lon", "lat"))
  ) %>%
  
  # Add actual river locations
  left_join(
    true_coords_PUGETSOUND,
    by = "population"
  ) %>%
  
  # Add inset pie positions
  left_join(
    puget_pie_positions,
    by = "population"
  )


## PUGET SOUND INSET MAP  ============================================================

puget_map <- ggplot() +
 # BASE MAP
 
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
  
  # Start a separate fill scale for pies
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
    x = lon,
    y = lat + 0.15,
    label = population
  ),
  size = 10, 
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
 # PUget SOUND MAP EXTENT
 
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
    x = 0.26,
    y = 0.11,
    width = 0.33,
    height = 0.38
  )   
 ### SAVE  =========
ggsave(
  "output/plots/population_pie_map_TREATY1.png",
  
  plot = final_map,
  
  width = 11,
  height = 9,
  
  bg = "white",
  dpi = 300
)



# TREATY PERIOD 2  ============================================================
# Plot and make DF 
pie_df_TREATY2 <- Pop_plot_df %>%
  filter(
    year > 2018 
  ) %>%
  
  group_by(
    broad_region,
    population
  ) %>%
  
  summarise(
    avg_mort = mean(percent_mort, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
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
    ),
    
    population = case_when(
      
      grepl("_fa$", population) ~
        paste(gsub("_fa$", "", population), "Fall"),
      
      grepl("_sp$", population) ~
        paste(gsub("_sp$", "", population), "Spring"),
      
      grepl("_su$", population) ~
        paste(gsub("_su$", "", population), "Summer"),
      
      grepl("_br_w$", population) ~
        paste(gsub("_br_w$", "", population), "Upriver Bright"),
      
      population == "South_umpqua" ~
        "South Umpqua",
      
      TRUE ~
        gsub("_", " ", population)
    ),
    
    population = gsub(
      "_",
      " ",
      population
    ),
    
    population = factor(
      population,
      levels = north_to_south_order
    )
  )

# 7. JOIN PIE DATA TO BOTH TYPES OF COORDINATES
#
# true_lon / true_lat = actual river mouth
# lon / lat             = final pie location
#  PIVOT TO WIDE FORMAT FOR SCATTERPIE
#
# scatterpie needs one column per pie slice (region),
# so we pivot pie_df_TREATY1 from long to wide.

pie_coords_TREATY2 <- pie_df_TREATY2 %>%
  dplyr::select(population, broad_region, avg_mort) %>%
  tidyr::pivot_wider(
    names_from  = broad_region,
    values_from = avg_mort,
    values_fill = 0
  ) %>%
  # attach starting lat/lon (these get dropped/rebuilt in step 7,
  # but pie_coords_TREATY1 needs lat/lon columns to exist for the
  # select(-lat, -lon) call downstream)
  left_join(true_coords, by = "population") %>%
  dplyr::rename(lon = true_lon, lat = true_lat)

pie_coords_repelled_TREATY2 <- pie_coords_TREATY2 %>%
  select(-lat, -lon) %>%
  left_join(true_coords, by = "population") %>%
  left_join(repelled_coords, by = "population") #%>%

## MAIN MAP =======
main_map <- ggplot() +
  
  # MAP
  
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
  # Start a NEW fill scale so the map colors and pie colors
  # don't interfere with one another.
  ggnewscale::new_scale_fill() +
  
  # LEADER LINES
  #
  # Pie centre -> actual river mouth
  
  geom_segment(
    data = pie_coords_repelled_TREATY2,
    
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
    data = pie_coords_repelled_TREATY2,
    
    aes(
      x = lon,# + dx,
      y = lat + 1.4,# + dy,
      label = population
    ),
    fontface = "bold",
    size = 12,
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
    data = pie_coords_repelled_TREATY2,
    
    aes(
      x = true_lon,
      y = true_lat
    ),
    
    size = 1.5,
    color = "grey30"
  ) +
  
  # PIE CHARTS
  
  geom_scatterpie(
    data = pie_coords_repelled_TREATY2,
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
  
  # LABELS
  #
  # Just a little above each pie.
  #
  # Change 0.65 to:
  #
  #   0.5 = closer
  #   0.8 = farther away
  #   1.0 = much farther away
  
  
  
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
  
  ggtitle("Treaty Period 2019-2022") + 
  
  # THEME
  
  theme_void() +
  
  theme(
    title = element_text(size = 50), 
    
    legend.position = "left",
    legend.title = element_text(size = 35),
    legend.text = element_text(size = 30),
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
  
  # Add actual river locations
  left_join(
    true_coords_PUGETSOUND,
    by = "population"
  ) %>%
  
  # Add inset pie positions
  left_join(
    puget_pie_positions,
    by = "population"
  )


## PUGET SOUND INSET MAP  ============================================================

puget_map <- ggplot() +
  # BASE MAP
  
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
  
  # Start a separate fill scale for pies
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
      x = lon,
      y = lat + 0.15,
      label = population
    ),
    size = 10, 
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
  # PUget SOUND MAP EXTENT
  
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
    x = 0.26,
    y = 0.11,
    width = 0.33,
    height = 0.38
  )   

# SAVE  =========
ggsave(
  "output/plots/population_pie_map_TREATY2.png",
  
  plot = final_map2,
  
  width = 11,
  height = 9,
  
  bg = "white",
  dpi = 300
)


