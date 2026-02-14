# This script plots estimates of Chinook salmon bycatch from GOA rockfish and pollock trawl fisheries  
library(tidyverse)
library(here)
library(readxl)
# Potential additional sources: 
# may have to look through all of Guthries papers but many GSI reports are here
# https://scholar.google.com/scholar?hl=en&as_sdt=0%2C48&q=guthrie+charles+noaa&btnG=

# GSI GOA trawl 2016 -- 
# APPENDIX 2,PAGE 31:  https://repository.library.noaa.gov/view/noaa/17604

# TLDR: Limited info on bycatch with GSI, but NOAA provides lots of non GSI/CWT linked numbers 
# NOAA- Chinook bycatch Numbners, no GSI.
      ##https://www.fisheries.noaa.gov/alaska/commercial-fishing/fisheries-catch-and-landings-reports-alaska#goa-prohibited-species
      ## Full timeseries: https://www.fisheries.noaa.gov/sites/default/files/akro/goasalmonmort2026.html




# BC - Groundfish Trawl =======
# British Columbia’s midwater trawl  - discussed here: https://davidsuzuki.org/expert-article/bringing-clarity-and-control-to-trawl-fishery-chinook-salmon-bycatch/
# Source: Lagasse, Cory R., Kathryn A. Fraser, Emily Braithwaite, and Nicholas Komick. “Salmon Bycatch Monitoring and Sampling Results for the Pacific Region 2023/24 Groundfish Trawl Fishery.” In Canadian Manuscript Report of Fisheries and Aquatic Sciences, with Fisheries and Oceans Canada. Fisheries and Oceans Canada, 2025. Application/pdf. https://doi.org/10.60825/D0E4-PP46.
bc <- read_csv("data/BC_chinook_stock_composition_Fig4.csv") %>% 
  dplyr::mutate(year = 2024,
                Fish_Origin = case_when(Stock_Group %in% c("Alaska","US West Coast") ~ Stock_Group,
                                        TRUE ~ "Canada"),
                Fishery_ID = "BC_MidTrawl") %>%
  dplyr::rename(Bycatch_Num="Number_of_Samples") %>%
  dplyr::select(-Region, -Time_Period, -Proportion, -Total_Catch, -Stock_Group) %>%
  dplyr::select(year,Fishery_ID,Fish_Origin,Bycatch_Num)
 
# AK - BSAI Groundfish Trawl Chinook bycatch =======
# Source: https://www.fisheries.noaa.gov/s3/2025-12/2024-Salmon-Bycatch-Report-akro.pdf
bsai <- read_csv("data/BSAI_NOAA_salmon_bycatch_data.csv") %>%
  dplyr::select(-Period) %>% 
  # filter(Metric ==  "CWT mark expanded number") %>% 
  gather(3:ncol(.), key = "year", value = "Bycatch_Num") %>%
  filter(!is.na(Bycatch_Num)) %>%
  dplyr::rename(Fish_Origin = "Region") %>%
  dplyr::mutate(Fishery_ID = "BSAI_Groundfish") %>%
  dplyr::select(year,Fishery_ID,Fish_Origin,Bycatch_Num)
 
# AK GOA Trawl =======
# Source: Iii, C M Guthrie. Genetic Stock Composition Analysis of Chinook Salmon 
# (Oncorhynchus Tshawytscha) Bycatch Samples from the 2020 Gulf of Alaska Trawl 
# Fisheries. n.d.
 
GOA_pollock <- read_excel("data/GOA_pollock_fishery_data.xlsx") %>%
  gather(2:ncol(.), key = Fishery_ID, value = Bycatch_Num) %>%
  dplyr::mutate(year =2020) %>% 
  rename(Fish_Origin = "Region") %>%
  filter(Fishery_ID %in% c("GOA_Overall_Est")) %>%
  dplyr::select(year,Fishery_ID,Fish_Origin,Bycatch_Num)
  
# Rockfish ===== 
GOA_Rockfish <- read_excel("data/GOA_rockfish_fishery_data.xlsx") %>% 
              dplyr::select(Region, Total_Est) %>% 
  dplyr::mutate(year =2020,
                Fishery_ID = "Rockfish_GOA_Trawl") %>% 
  rename(Fish_Origin = "Region",
         Bycatch_Num = "Total_Est") %>% 
  dplyr::select(year,Fishery_ID,Fish_Origin,Bycatch_Num)

bycatch <- rbind(GOA_Rockfish,GOA_pollock,bsai, bc) %>%
  dplyr::mutate(Fish_Origin = case_when(Fish_Origin %in% c("Coast W AK","Mid Yukon","Up Yukon",        
                                                           "N AK Pen","NW GOA","Copper","NE GOA",          
                                                           "Coast SE AK") ~ "US-AK",
                Fish_Origin %in% c("British Columbia","Yukon Territory","Canada" ) ~ "BC",
                Fish_Origin %in% c("Oregon","Washington","US West Coast","West Coast US" ) ~ "US-WA OR",       
                TRUE ~ Fish_Origin))
 
# Filter data for US-WA OR origin
bycatch_filtered <- bycatch %>%
  filter(Fish_Origin == "US-WA OR")

# Plot =====
# Create stacked bar chart
bycatch_plot <- ggplot(bycatch_filtered, aes(x = year, y = Bycatch_Num, fill = Fishery_ID)) +
  geom_col() +
  labs(
    title = "Chinook Salmon Bycatch by Fishery",
    subtitle = "US-WA OR Origin Only",
    x = "Year",
    y = "Bycatch Numbers",
    fill = "Fishery ID"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  ) +
  scale_fill_viridis_d()

bycatch_plot 
ggsave("output/plots/bycatch_plot.jpeg", width = 8, height =6)
