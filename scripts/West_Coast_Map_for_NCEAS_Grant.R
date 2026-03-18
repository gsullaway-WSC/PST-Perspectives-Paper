# West Coast Map: Southern AK to CA with Major Rivers

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)

# Load HydroRIVERS - update path to where you unzipped it
hydro <- st_read("HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp")

# Crop to your map extent first (much faster than filtering the whole dataset)
bbox <- st_bbox(c(xmin = -170, xmax = -110, ymin = 32, ymax = 68),
                crs = st_crs(4326))
 
# Filter by upstream area for intermediate detail 
target_states <- bind_rows(
  states_usa %>% filter(name %in% c("California", "Oregon", "Washington", "Alaska")),
  states_can %>% filter(name %in% c("British Columbia", "Yukon"))
)

# Dissolve into one polygon mask
target_union <- st_union(target_states)
 
hydro_arc <- st_read("HydroRIVERS_v10_ar_shp/HydroRIVERS_v10_ar_shp/HydroRIVERS_v10_ar.shp")  # Arctic region

hydro_all <- bind_rows(hydro, hydro_arc)

#  crop and intersect  
hydro_crop     <- st_crop(hydro_all, bbox)
hydro_filtered <- hydro_crop %>%
  filter(UPLAND_SKM >= 500) %>%
  st_intersection(target_union)


map <- ggplot() +
  # Land
  geom_sf(data = world,
          fill = "#e8e0d5", color = "#b0a898", linewidth = 0.3) +
  
  # US States (highlight west coast states)
  geom_sf(data = states_usa %>%
            filter(name %in% c("California", "Oregon", "Washington", "Alaska")),
          fill = "#ddd6c8", color = "#8a7f72", linewidth = 0.4) +
  
  # Canadian Provinces (highlight BC & Yukon)
  geom_sf(data = states_can %>%
            filter(name %in% c("British Columbia", "Yukon")),
          fill = "#d8d0c3", color = "#8a7f72", linewidth = 0.4) +
 
  
  # ── Labels & theme ───────────────────────────────────────────────────────
  labs(
    title    = "Pacific Salmon Jurisdictions: Southern Alaska to California"#,
    # subtitle = "Major rivers of AK, BC, WA, OR, and CA",
    # caption  = "Data: Natural Earth"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#cce0f0", color = NA), # ocean color
    panel.background = element_rect(fill = "#cce0f0", color = NA),
    panel.grid       = element_line(color = alpha("white", 0.4), linewidth = 0.3),
    plot.title       = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "#555"),
    plot.caption     = element_text(size = 8, color = "#888"),
    axis.text        = element_text(size = 8, color = "#555")
  )+ 

  geom_sf(data = hydro_filtered,
          aes(linewidth = log10(UPLAND_SKM)),
          color = "#4a90b8", alpha = 0.8) +
  scale_linewidth_continuous(range = c(0.05, 0.7), guide = "none") +
  coord_sf(
    xlim = c(-170, -113), ylim = c(32, 68),
    crs = st_crs(4326), 
    # crs =  st_crs("+proj=lcc +lat_1=40 +lat_2=65 +lat_0=52 +lon_0=-140 +x_0=0 +y_0=0 +datum=NAD83 +units=m"), 
    # crs = st_crs("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m"),
    # crs = st_crs("ESRI:102008"),
    expand = FALSE
  )  


 

# 5. SAVE ───────────────────────────────────────────────────────────────────
map
ggsave("west_coast_rivers_map.png", width = 5, height = 10) 
