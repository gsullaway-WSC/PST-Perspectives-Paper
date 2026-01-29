# this report by DFO has groundfish harvest and bycatch stats:
# Lagasse, Cory R., Kathryn A. Fraser, Emily Braithwaite, and Nicholas Komick. “Salmon Bycatch Monitoring and Sampling Results for the Pacific Region 2023/24 Groundfish Trawl Fishery.” In Canadian Manuscript Report of Fisheries and Aquatic Sciences, with Fisheries and Oceans Canada. Fisheries and Oceans Canada, 2025. Application/pdf. https://doi.org/10.60825/D0E4-PP46.

# Below I plot salmon bycatch data from respective tables (each plot is labeled).
library(tidyverse)
library(here)
library(readxl)

# Table 1 ===========
# Summary of annual coastwide salmon catch (numbers of fish retained and released) 
# by species, and landed catches (kg) in the groundfish trawl fishery reported by 
# groundfish fishing year (February 21 of the starting year to February 20 of the 
# subsequent year). Unidentified salmon catch was reported as Pacific salmon and 
# trout and represents salmonids that could not be identified to species either 
# by fisher or independent monitoring programs. Total landed catch is the landed 
# weight of all species in the groundfish trawl fishery.
 
table1 <- read_excel("data/salmon_catch_tables.xlsx", sheet =1)  %>%
  data.frame() %>% 
  dplyr::mutate(Fishing_Season = Groundfish.Fishery,
                Total_Salmon_Numbers = as.numeric(Total.salmon....of.fish.))
 
# Read data
table1 <- read_excel("data/salmon_catch_tables.xlsx", sheet = 1) %>%
  data.frame() %>% 
  dplyr::mutate(
    Fishing_Season = Groundfish.Fishery,
    Total_Salmon_Numbers = as.numeric(Total.salmon....of.fish.),
    Pink = as.numeric(Pink....of.fish.),
    Chinook = as.numeric(Chinook....of.fish.)
  ) %>%
  dplyr::select(Fishing_Season,Total_Salmon_Numbers,Pink,Chinook) %>% 
  gather(c(2:4), key = "Type", value = "Numbers")

# Create improved plot
ggplot(data = table1, 
       aes(x = Fishing_Season, y = Numbers, group = Type, color = Type)) +
  
  # Line and points
  geom_line( linewidth = 1.2) +
  geom_point(  size = 3, shape = 19) +
  scale_color_manual(values = c("Brown", "Pink", "DarkBlue")) +
  # Y-axis formatting
  scale_y_continuous(
    limits = c(0, NA),  # Start y-axis at 0
    expand = expansion(mult = c(0, 0.1))  # Add space at top
  ) +
  
  # Labels
  labs(
    title = "Total Salmon Catch in BC Groundfish Trawl Fishery",
    subtitle = "Annual salmon bycatch (2008-2024)",
    x = "Fishing Season",
    y = "Total Salmon (Number of Fish)",
    caption = "Source: Lagasse et al 2025"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    # Title formatting
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    
    # Axis formatting
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title = element_text(face = "bold"),
    
    # Grid improvements
    panel.grid.minor = element_blank(),   
    panel.grid.major.x = element_blank())

# ggsave("Table1_plot.png", 
#        width = 12, height = 7, dpi = 300, bg = "white")
 
# Table 3 =====
# Catch by region

# Number of tows and salmon catch (numbers of fish retained and released) 
# by species, region, and gear subtype for the 2023/24 groundfish fishery 
# year (February 21, 2023 to February 20, 2024). Catch with unspecified gear 
# subtype represent a small proportion of tow events and are summarized across 
# all regions only. Regions are abbreviated as follows: NC = North Coast, 
# QC&JST = Queen Charlotte & Johnstone Strait, SoG = Strait of Georgia, 
# WCVI = West Coast Vancouver Island. Catch with Region UNK could not be 
# associated to a single geographic Region.

table3 <- read_excel("data/salmon_catch_tables.xlsx", sheet =2) 

# Table 5 =====
# CWT analysis  



# Table 6 =====
# Stock Comp



# Table 7 =====
# Bycatch Estimates

