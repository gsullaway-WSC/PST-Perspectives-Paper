 
# get Coast-Wide Chinook FM pie charts, and then put them on a map ====== 

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
  "British Columbia"           = "#8A9E7A",  
  "Alaska"                     = "#4A7A50"
)


# load ====== 
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
  "Moyeha Fall",
  "Bedwell Fall",
  "Puntledge Summer",
  "Cowichan",
  
  # Washington Coast
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
 
  # Loop Through Each population to make pie chart ======= 
pie_df2 <- Pop_plot_df %>%
  filter(year > 2008 & year < 2020) %>%
  group_by(broad_region, population) %>%
  summarise(avg_mort = mean(percent_mort, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(broad_region)) %>%
  dplyr::mutate(
    custom_region = case_when(
      broad_region == "Alaska"                  ~ "AK",
      broad_region == "British Columbia"        ~ "BC",
      broad_region == "Washington"              ~ "WA",
      broad_region == "Oregon"                  ~ "OR", 
      TRUE ~ NA_character_),
    population = case_when(
          # Handle suffix replacements first
          grepl("_fa$", population)  ~ paste(gsub("_fa$", "", population), "Fall"),
          grepl("_sp$", population)  ~ paste(gsub("_sp$", "", population), "Spring"),
          grepl("_su$", population)  ~ paste(gsub("_su$", "", population), "Summer"),
          grepl("_br_w$", population) ~ paste(gsub("_br_w$", "", population), "Upriver Bright"),
          # Then clean remaining underscores for everything else
          population == "South_umpqua" ~ "South Umpqua", 
          TRUE ~ gsub("_", " ", population)
        ),
        # Clean any remaining underscores in the suffix-replaced names too
    population = gsub("_", " ", population),
    population = factor(population, levels = north_to_south_order))
    
  
# Get the unique populations to loop over
populations <- unique(pie_df2$population)
populations

#  Loop — one plot per population
population_pies <- purrr::map(populations, function(pop) {
  
  # Filter to this population and compute cum_pos HERE (per-population)
  df <- pie_df2 %>%
    filter(population == pop) %>%
    arrange(desc(broad_region)) %>%           # consistent slice order
    mutate(
      cum_pos = cumsum(avg_mort) - avg_mort / 2,
      label   = paste0(custom_region, "\n", round(avg_mort * 100, 1), "%")
    )
  
  ggplot(df, aes(x = "", y = avg_mort, fill = broad_region)) +
    geom_col(color = "black", alpha = 0.9, width = 1) +
    
    # Labels inside for large slices
    geom_text(
      aes(y = cum_pos,
          label = ifelse(avg_mort >= 0.05, label, "")),
      size = 4, fontface = "bold", family = "dm_sans", color = "black"
    ) +
    
    # Labels outside for small slices
    geom_text(
      aes(y = cum_pos,
          label = ifelse(avg_mort < 0.05 & avg_mort > 0.001, label, "")),
       x = 1.6,
      size = 4, fontface = "bold", family = "dm_sans", color = "black"
    ) +
    
    coord_polar(theta = "y", clip = "off") +
    scale_fill_manual(values = custom_pal, name = "Fishery Region") +
    ggtitle(pop) +                             # population name as title
    theme_void() +
    theme(
      plot.title        = element_text(family = "dm_sans", size = 11,
                                       face = "bold", hjust = 0.5),
      legend.title      = element_text(family = "dm_sans", size = 10, face = "bold"),
      legend.text       = element_text(family = "dm_sans", size = 9),
      legend.margin     = margin(t = 10, b = -10, unit = "mm"),
      legend.key.size   = unit(0.4, "cm"),
      legend.spacing.x  = unit(0.2, "cm"),
      plot.margin       = margin(0, 5, 0, 5, "mm"),
      plot.background   = element_blank()
    )
})

# Name the list so you can access by population name
names(population_pies) <- populations

# Step 4: View one
# population_pies[["Taku"]]

# Step 5 (optional): Combine into a grid with patchwork
library(patchwork)
wrap_plots(population_pies, guides = "collect")

# Save all population pies to a single multi-page PDF

pdf_path <- "output/plots/Chinook_population_pies.pdf"

pdf(pdf_path, width = 8, height = 8)  # adjust dimensions as needed
 
for (pop in levels(pie_df2$population)) {
  print(population_pies[[pop]])
}

dev.off()
  
# Map Plot with Pie Charts ===========
library(sf) 
library(scatterpie)  
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(tidyverse)

# Coastline polygons
coast <- ne_states(country = c("united states of america", "canada"), 
                   returnclass = "sf")
ocean <- ne_download(scale = 10, type = "ocean", category = "physical", 
                     returnclass = "sf")

### Coordinates for each population's river mouth ==============
coords <- tribble(
  ~population,       ~lon,      ~lat,
  # Alaska — shift west into Pacific
  "Chilkat",               -140.0,    59.23,
  "Taku",                  -140.0,    58.50,
  "Stikine",               -140.0,    57.70,
  "Unuk",                  -140.0,    56.90,
  
  # BC North
  "Kitsumkalum",           -135.0,    54.52,
  "Atnarko",               -134.0,    52.37,
  
  # Vancouver Island — spread vertically, push west
  "Kaouk Fall",            -133.0,    51.50,
  "Tahsish Fall",          -133.0,    50.90,
  "Artlish Fall",          -133.0,    50.30,
  "Megin Fall",            -133.0,    49.70,
  "Moyeha Fall",           -133.0,    49.10,
  "Bedwell Fall",          -133.0,    48.50,
  "Puntledge Summer",      -131.5,    50.50,
  "Cowichan",              -131.5,    49.80,
  
  # Fraser tributaries — push east/inland with offset
  "Harrison",              -128.5,    51.50,
  "Lower Shuswap",         -128.5,    50.80,
  "South Thompson",        -128.5,    50.10,
  "Nicola",                -128.5,    49.40,
  
  # WA Coast — push west, spread vertically
  "Quillayute Fall",       -131.5,    48.00,
  "Hoh Spring",            -131.5,    47.50,
  "Hoh Fall",              -131.5,    47.00,
  "Queets Spring",         -131.5,    46.50,
  "Queets Fall",           -131.5,    46.00,
  "Grays Harbor Spring",   -131.5,    45.50,
  "Grays Harbor Fall",     -131.5,    45.00,
  "Grays",                 -131.5,    44.50,
  "Phillips",              -131.5,    44.00,
  
  # Columbia tributaries — push west
  "Lewis",                 -129.5,    46.80,
  "Coweeman",              -129.5,    46.20,
  "Elochoman",             -129.5,    45.60,
  "Hanford Bridge Winter", -126.5,    46.80,  # far inland, offset separately
  "Colonial Fall",         -129.5,    45.00,
  
  # Oregon — push west
  "Nehalem Fall",          -129.5,    45.71,
  "Siletz Fall",           -129.0,    44.50,
  "Siuslaw Fall",          -129.0,    43.80,
  "South Umpqua",          -129.0,    43.10,
  "Coquille",              -129.0,    42.40
)

pie_coords <- pie_df2 %>%
  select(population, broad_region, avg_mort) %>%
  pivot_wider(names_from = broad_region, values_from = avg_mort, values_fill = 0) %>%
  left_join(coords, by = "population")


## main map ===== 
# Populations to exclude from main map (go on inset)
puget_sog <- c("Nooksack", "Skagit Spring", "Skagit Fall", "Stillaguamish",
               "Snohomish Fall", "Green", "Nisqually", "Cowichan", 
               "Puntledge Summer")

  pie_main <- pie_coords %>% filter(!population %in% puget_sog)
pie_inset <- pie_coords %>% filter(population %in% puget_sog)

# 
# # temporary just for an exmaple plot for matt 
# puget_sog <- c("Stikine", "Unuk", 
#                "Atnarko","Kaouk Fall","Tahsish Fall", "Artlish Fall", "Megin Fall", "Moyeha Fall",       
#                "South Thompson","Nicola", "Quillayute Fall",      
#                "Hoh Spring",          
#                "Hoh Fall",             
#                "Queets Spring", 
#                "Grays Harbor Fall",     
#                "Grays",               
#                "Phillips", "Lewis",  
#                "Coweeman",        
#                "Elochoman", 
#                "Siuslaw Fall",           
#                "South Umpqua",         
#                "Coquille",    
#                "Nooksack", "Skagit Spring", "Skagit Fall", "Stillaguamish",
#                "Snohomish Fall", "Green", "Nisqually", "Cowichan", 
#                "Puntledge Summer")
# 
#  pie_main <- pie_coords %>% filter(!population %in% puget_sog)

region_cols <- names(custom_pal)  # your broad_region columns

main_map <- ggplot() +
  geom_sf(data = coast, fill = "grey85", color = "grey50", linewidth = 0.3) +
  # Main map — bump up from 1.8 to 3.5
  geom_scatterpie(
    data = pie_main,
    aes(x = lon, y = lat, group = population),
    cols = region_cols,
    pie_scale = 3.5,       # <-- increased
    color = "black",
    linewidth = 0.3,
    alpha = 0.9
  )+
  geom_text(
    data = pie_main,
    aes(x = lon, y = lat, label = population),
    nudge_y = 0.6, size = 2.5, family = "dm_sans"
  ) +
  scale_fill_manual(values = custom_pal, name = "Fishery Region") +
    coord_sf(xlim = c(-142, -115), ylim = c(42, 61))+
# OG    coord_sf(xlim = c(-136, -117), ylim = c(42, 60)) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(family = "dm_sans", size = 10, face = "bold"),
    legend.text     = element_text(family = "dm_sans", size = 9)
  )

main_map

ggsave("output/plots/FM_Map.png", width = 6, height =4, bg = "white")


## Salish Sea Map =====
inset_map <- ggplot() +
  geom_sf(data = coast, fill = "grey85", color = "grey50", linewidth = 0.3) +
  geom_scatterpie(
    data = pie_inset,
    aes(x = lon, y = lat, group = population_label),
    cols = region_cols,
    pie_scale = 1.2,
    color = "black",
    linewidth = 0.3,
    alpha = 0.9
  ) +
  geom_text(
    data = pie_inset,
    aes(x = lon, y = lat, label = population_label),
    nudge_y = 0.25, size = 2.5, family = "dm_sans"
  ) +
  scale_fill_manual(values = custom_pal, name = "Fishery Region") +
  coord_sf(xlim = c(-124, -121.5), ylim = c(47, 50)) +
  theme_void() +
  theme(legend.position = "none")

# combine ======
final_map <- ggdraw(main_map) +
  draw_plot(
    inset_map,
    x = 0.62, y = 0.15,   # position on main map (tweak as needed)
    width = 0.35, height = 0.35
  ) +
  draw_label("Puget Sound &\nStrait of Georgia", 
             x = 0.79, y = 0.49, size = 8, fontface = "bold", family = "dm_sans")

# Save
ggsave("population_pie_map.pdf", final_map, width = 12, height = 16, units = "in")


# BETTER PLOT  ====
library(packcircles) 

# 1. Define TRUE river mouth coordinates (for leader lines)
true_coords <- tribble(
  ~population,       ~true_lon,  ~true_lat,
  "Chilkat",               -135.57,    59.23,
  "Taku",                  -133.93,    58.27,
  "Stikine",               -132.38,    56.70,
  "Unuk",                  -130.87,    56.08,
  "Kitsumkalum",           -128.68,    54.52,
  "Atnarko",               -126.75,    52.37,
  "Harrison",              -121.93,    49.20,
  "Lower Shuswap",         -121.93,    49.20,
  "South Thompson",        -121.93,    49.20,
  "Nicola",                -121.93,    49.20,
  "Kaouk Fall",            -127.55,    50.10,
  "Tahsish Fall",          -127.48,    50.02,
  "Artlish Fall",          -127.42,    49.92,
  "Megin Fall",            -126.05,    49.22,
  "Moyeha Fall",           -126.03,    49.20,
  "Bedwell Fall",          -125.87,    49.12,
  "Puntledge Summer",      -124.93,    49.67,
  "Cowichan",              -123.72,    48.78,
  "Nooksack",              -122.55,    48.78,
  "Skagit Spring",         -122.37,    48.32,
  "Skagit Fall",           -122.37,    48.32,
  "Stillaguamish",         -122.38,    48.18,
  "Snohomish Fall",        -122.35,    47.92,
  "Green",                 -122.34,    47.53,
  "Nisqually",             -122.71,    47.10,
  "Quillayute Fall",       -124.63,    47.90,
  "Hoh Spring",            -124.44,    47.75,
  "Hoh Fall",              -124.44,    47.75,
  "Queets Spring",         -124.33,    47.53,
  "Queets Fall",           -124.33,    47.53,
  "Grays Harbor Spring",   -124.11,    46.90,
  "Grays Harbor Fall",     -124.11,    46.90,
  "Grays",                 -124.11,    46.90,
  "Phillips",              -123.90,    46.90,
  "Lewis",                 -122.77,    46.10,
  "Coweeman",              -122.90,    46.12,
  "Elochoman",             -123.41,    46.18,
  "Hanford Bridge Winter", -119.49,    46.20,
  "Colonial Fall",         -122.77,    46.10,
  "Nehalem Fall",          -123.92,    45.71,
  "Siletz Fall",           -124.00,    44.90,
  "Siuslaw Fall",          -124.13,    44.01,
  "South Umpqua",          -124.19,    43.69,
  "Coquille",              -124.39,    43.12
)

# 2. Run circle packing to get non-overlapping positions
#    radius controls how much space each pie gets — tune this
pie_radius <- 0.8  # degrees; increase = more spread

pack_input <- true_coords %>%
  mutate(radius = pie_radius)

# packcircles works in unitless space — scale lat/lon to be roughly equal
# (1 deg lon ~ 0.6 deg lat at these latitudes)
pack_input_scaled <- pack_input %>%
  mutate(x = true_lon * 0.6, y = true_lat)

# Run circle repulsion
packed <- circleRepelLayout(
  data.frame(x = pack_input_scaled$x,
             y = pack_input_scaled$y,
             r = pack_input_scaled$radius),
  # niter    = 1000,
  wrap     = FALSE,
  xysizecols = c(1, 2, 3)  # tell it which columns are x, y, radius
)

# Extract the repelled positions (stored in $layout)
repelled_coords <- true_coords %>%
  mutate(
    lon = packed$layout$x / 0.6,
    lat = packed$layout$y
  )

#  Add a broad_region column to coast that matches your custom_pal names
coast <- coast %>%
  mutate(broad_region = case_when(
    name %in% c("Alaska")                          ~ "Alaska",
    name %in% c("British Columbia")                ~ "British Columbia",
    name %in% c("Washington" )                     ~ "Washington",  # or whichever region fits
    name %in% c( "Oregon")                         ~ "Oregon",  # or whichever region fits
    TRUE~ "Other"))

#  Add "Other" to your palette
custom_pal_map <- c(custom_pal, "Other" = "lightgray")

# 4. Join repelled positions + true positions onto pie data
pie_coords_repelled <- pie_coords %>%
  dplyr::select(-lat,-lon) %>%
  left_join(repelled_coords, by = "population") %>%
  filter(!population %in% puget_sog) %>%
  dplyr::mutate(lon= case_when(population %in% c("Taku",  "Unuk")  ~ lon-7,
                               population %in% c("Stikine", "Chilkat", 
                                                 "Kitsumkalum","Grays Harbor Fall","Quillayute Fall"  )  ~ lon-12,
                               population %in% c("Chilkat", "Atnarko", "Hoh Spring", "Hoh Fall",
                                                 "Queets",    "Coquille") ~  lon-10, 
                TRUE ~ lon)) %>%
  filter(population %in% c("Taku",  "Unuk", "Stikine", "Chilkat", 
                           "Kitsumkalum","Grays Harbor Fall", "Hoh Fall",
                           "Chilkat", "Atnarko", "Hoh Spring", 
                           "Queets", "Quillayute Fall",  "Coquille"))

 
# 5. Build the map
main_map <- ggplot() +
  geom_sf(data = coast, aes(fill = broad_region), color = "lightgray", linewidth = 0.3) +
  scale_fill_manual(values = custom_pal_map, guide = "none") +  # guide="none" hides state fill from legend
  
  # Leader lines: repelled pie centre → true river mouth
  geom_segment(
    data = pie_coords_repelled,
    aes(x = lon, y = lat, xend = true_lon, yend = true_lat),
    color = "grey40", linewidth = 0.4, linetype = "dotted"
  ) +
  
  # Dot at true river mouth
  geom_point(
    data = pie_coords_repelled,
    aes(x = true_lon, y = true_lat),
    size = 1.5, color = "grey30"
  ) +
  
  # Pies at repelled positions
  geom_scatterpie(
    data = pie_coords_repelled,
    aes(x = lon, y = lat, group = population),
    cols = region_cols,
    pie_scale = 3,
    color = "black",
    linewidth = 0.3,
    alpha = 0.9
  ) +
  
  # Labels above repelled pies
  geom_text(
    data = pie_coords_repelled,
    aes(x = lon, y = lat+0.5, label = population),
    nudge_y = 0.7, size = 2.3, family = "dm_sans"
  ) +
  
  scale_fill_manual(values = custom_pal, name = "Fishery Region") +
   coord_sf(xlim = c(-155, -120), ylim = c(42, 61)) +
  theme_void() +
  theme(
    legend.position = "left",
    legend.title    = element_text(family = "dm_sans", size = 10, face = "bold"),
    legend.text     = element_text(family = "dm_sans", size = 9)
  )

main_map
# Save
ggsave("output/plots/population_pie_map.png", width = 7, height = 7,bg = "white")

