 
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)

# Read rivers (NA + Arctic)
hydro <- st_read("HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp")
hydro_arc <- st_read("HydroRIVERS_v10_ar_shp/HydroRIVERS_v10_ar_shp/HydroRIVERS_v10_ar.shp")

hydro_all <- bind_rows(hydro, hydro_arc)

# Lat/long bbox for initial crop (still in 4326)
bbox <- st_bbox(c(xmin = -170, xmax = -110, ymin = 32, ymax = 68),
                crs = st_crs(4326))

# Filter by upstream area for intermediate detail
target_states <- bind_rows(
  states_usa %>% filter(name %in% c("California", "Oregon", "Washington", "Alaska")),
  states_can %>% filter(name %in% c("British Columbia", "Yukon"))
)

target_union <- st_union(target_states)

# Crop and intersect in 4326 first
hydro_crop <- st_crop(hydro_all, bbox)
hydro_filtered <- hydro_crop %>%
  filter(UPLAND_SKM >= 500) %>%
  st_intersection(target_union)

# ---- Reproject everything to North America Albers Equal Area (ESRI:102008) ----
crs_aea <- st_crs("ESRI:102008")  # North America Albers Equal Area
 

world_aea         <- st_transform(world,        crs_aea)
states_usa_aea    <- st_transform(states_usa,   crs_aea)
states_can_aea    <- st_transform(states_can,   crs_aea)
hydro_filtered_aea <- st_transform(hydro_filtered, crs_aea)
 
# Build map in equal-area projection
map <- ggplot() +
  
  geom_sf(data = world_aea,
          fill = "#e8e0d5", color = "#b0a898", linewidth = 0.3) +
  
  geom_sf(data = states_usa_aea %>%
            filter(name %in% c("California", "Oregon", "Washington", "Alaska")),
          fill = "#ddd6c8", color = "#8a7f72", linewidth = 0.4) +
  
  geom_sf(data = states_can_aea %>%
            filter(name %in% c("British Columbia", "Yukon")),
          fill = "#d8d0c3", color = "#8a7f72", linewidth = 0.4) +
  
  geom_sf(data = hydro_filtered_aea,
          aes(linewidth = log10(UPLAND_SKM)),
          color = "#4a90b8", alpha = 0.8) +
  
  scale_linewidth_continuous(range = c(0.05, 0.7), guide = "none") +
  
  labs(
    title = "Pacific Salmon Jurisdictions: Southern Alaska to California"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#cce0f0", color = NA),
    panel.background = element_rect(fill = "#cce0f0", color = NA),
    panel.grid       = element_line(color = alpha("white", 0.4), linewidth = 0.3),
    plot.title       = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "#555"),
    plot.caption     = element_text(size = 8, color = "#888"),
    axis.text        = element_text(size = 8, color = "#555")
  ) +
  
  coord_sf(
    crs = crs_aea,
    xlim = c(-170, -113),# ylim = c(32, 68),
    expand = FALSE
  )

map
