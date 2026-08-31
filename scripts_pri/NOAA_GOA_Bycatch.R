# This script plots estimates of Chinook salmon bycatch from GOA rockfish and pollock trawl fisheries  
# Source: Iii, C M Guthrie. Genetic Stock Composition Analysis of Chinook Salmon 
# (Oncorhynchus Tshawytscha) Bycatch Samples from the 2020 Gulf of Alaska Trawl 
# Fisheries. n.d.

library(tidyverse)
library(here)
library(readxl)

# pollock ===== 
pollock <- read_excel("data/pollock_fishery_data.xlsx")

# rockfish ===== 
rockfish <- read_excel("data/rockfish_fishery_data.xlsx")