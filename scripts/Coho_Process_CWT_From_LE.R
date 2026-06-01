# Code from L Elmer Git Hub, customized the data based on specific PST insterests starting on line 

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(scales)
library(RColorBrewer)
library(ggrepel)
library(here)
library(readr)


 
# =====================================================
# Coho CWT Analysis
#
# Data are stored separately because the files are too large for GitHub.
#
# Download data here:
# https://drive.google.com/drive/folders/1552FV2cC8LJkCo0L0AC9Tq79C_-I8v2d?usp=drive_link
#
# After downloading, place files in:
# data/raw/
# =====================================================








############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################

####################################### RELEASE DATA #######################################

############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################




# ------------------------------------------------------------
# 1. Read in Canadian release data
# ------------------------------------------------------------

# Import the Canadian coho release dataset from CSV
CAreleasedata <- read.csv(
  here("data", "raw", "Canada Releases.csv")
)

# Look at the first few rows to confirm the file imported correctly
head(CAreleasedata)

# Check whether any MRP record IDs are duplicated
# This should return FALSE if each record ID is unique
any(duplicated(CAreleasedata$X.RC..MRP.Record.ID))


# ------------------------------------------------------------
# 2. Read in Alaska release data
# ------------------------------------------------------------

# Import the Alaska release dataset from CSV
AKreleasedata <- read.csv(
  here("data", "raw", "Alaska Releases.csv")
)

# Look at the first few rows to confirm the file imported correctly
head(AKreleasedata)

# Check whether any MRP record IDs are duplicated
# This should return FALSE if each record ID is unique
any(duplicated(AKreleasedata$X.RC..MRP.Record.ID))


# ------------------------------------------------------------
# 3. Combine Canadian and Alaska release datasets
# ------------------------------------------------------------

# Stack the two datasets together row-wise
releasedata <- bind_rows(CAreleasedata, AKreleasedata)

# Check the dimensions of the combined dataset
# Gives number of rows and columns
dim(releasedata)

# View the column names in the combined dataset
colnames(releasedata)

# Confirm that MRP record IDs are still unique after combining
# This should return FALSE
any(duplicated(releasedata$X.RC..MRP.Record.ID))

# Confirm that tag codes are unique
# This should also return FALSE
any(duplicated(releasedata$Tagcode))

# Check the range of brood years in the data
range(releasedata$Brood.Year)

# Preview the first few rows of the combined dataset
head(releasedata)


# ------------------------------------------------------------
# 4. Convert selected variables to factors
# ------------------------------------------------------------

# Convert Stage.Code to a factor
# This is useful for categorical variables in summaries and plots
releasedata$Stage.Code <- as.factor(releasedata$Stage.Code)

# View the levels present in Stage.Code
levels(releasedata$Stage.Code)

# Convert Stage.PSC.Code to a factor
releasedata$Stage.PSC.Code <- as.factor(releasedata$Stage.PSC.Code)

# View the levels present in Stage.PSC.Code
levels(releasedata$Stage.PSC.Code)

# Check the dataset again
head(releasedata)

# Convert Release.Site.Name to a factor
releasedata$Release.Site.Name <- as.factor(releasedata$Release.Site.Name)

# View all release site names
levels(releasedata$Release.Site.Name)

# Convert Stock.PSC.Region.Code to a factor
releasedata$Stock.PSC.Region.Code <- as.factor(releasedata$Stock.PSC.Region.Code)

# View all stock region codes
levels(releasedata$Stock.PSC.Region.Code)


# ------------------------------------------------------------
# 5. Create a lookup table to standardize release site names
# ------------------------------------------------------------

# This lookup table maps original site names to a standardized grouping
# The idea is to combine related or nearby release sites under one name
lookup <- tibble::tribble(
  ~`SITE NAME`, ~`SITE NAME 2`,
  "Angler Cv Lk", "Angler Cv Lk",
  "Atnarko Ch", "Atnarko",
  "Atnarko R Low", "Atnarko",
  "Babine Lk", "Babine Lk",
  "Babine R", "Babine R",
  "Babine R Up", "Babine R",
  "Bella Coola R Low", "Bella Coola R",
  "Bella Coola R Up", "Bella Coola R",
  "Buck Cr", "Buck Cr",
  "Bulkley Lk", "Bulkley Lk",
  "Bulkley R Up", "Bulkley R Up",
  "Bulkley R+Toboggan", "Toboggan Cr",
  "Canyon Cr/SKNA", "Canyon Cr",
  "Cecil Cr", "Cecil Cr",
  "Clifford Cr", "Kispiox R",
  "Cullon Cr", "Kispiox R",
  "Damshilgwit Cr", "Slamgeesh R",
  "Diana Cr", "Dianna Cr",
  "Dry Cr/SKNA", "Dry Cr",
  "Fulton R", "Fulton R",
  "Hagensborg Sl", "Bella Coola R",
  "Hartley Bay Cr", "Hartley Bay Cr",
  "Hartley Bay Lk", "Hartley Bay",
  "Hays Cr", "Hays Cr",
  "Hodder Cr", "Kispiox R",
  "Johnston Cr", "Johnston Cr",
  "Johnston Est", "Johnston Est",
  "Kispiox R", "Kispiox R",
  "Kispiox R Tribs", "Kispiox R",
  "Kitasoo Cr", "Kitasoo Cr",
  "Kitimat R", "Kitimat R",
  "Kitimat R Low", "Kitimat R",
  "Kitsumkalum R", "Kitsumkalum R",
  "Kitsumkalum R Up-use6449", "Kitsumkalum R",
  "Kitwanga R", "Kitwanga R",
  "Ksi Gingolx (Kincolith) R", "Kincolith R",
  "Lachmach R", "Lachmach R",
  "Martin R", "Martin R",
  "McBride Cr", "McBride Cr",
  "McLoughlin Bay", "McLoughlin Bay",
  "McLoughlin Bay Cr", "McLoughlin Bay Cr",
  "McQueen Cr", "Kispiox R",
  "Mission Cr/SKNA", "Mission Cr",
  "Morice R", "Morice R",
  "Murder Cr", "Kispiox R",
  "Nilkitkwa Lk", "Nilkitkwa Lk",
  "Oldfield Cr", "Oldfield Cr",
  "Owen Cr", "Owen Cr",
  "Owen Lk", "Owen Cr",
  "Red Bluff Lk", "Hartley Bay",
  "Saloompt R", "Bella Coola R",
  "Second Lk/NCST", "Second Lk",
  "Skunsnat Cr", "Kispiox R",
  "Slamgeesh R", "Slamgeesh R",
  "Snootli Cr", "Bella Coola R",
  "Sylvia Lk", "Hartley Bay",
  "Tahlo Cr", "Tahlo Cr",
  "Thornhill Cr", "Thornhill Cr",
  "Thorsen Cr/CCST", "Bella Coola R",
  "Toboggan Cr", "Toboggan Cr",
  "Trout Bay", "Trout Bay",
  "Tseax R", "Tseax R",
  "Union Pass Lk", "Hartley Bay",
  "Waterfall Cr/SKNA", "Mission Cr",
  "West Arm Cr (Drake Cr)", "West Arm Cr (Drake Cr)",
  "Whalen Lk", "Hartley Bay",
  "Zolzap Cr", "Zolzap Cr",
  "Zymacord R", "Zymacord R",
  
  "Braverman Cr", "Braverman Cr",
  "Chown Brk", "Chown Brk",
  "Coates Cr", "Coates Cr",
  "Copper Cr", "Copper Cr",
  "Deer Bay", "Deer Bay",
  "Fukwa Cr", "Fukwa Cr",
  "Gold Cr", "Gold Cr",
  "Haans Cr", "Haans Cr",
  "Honna R", "Honna R",
  "King Cr/QCI", "King Cr",
  "Marie Lk", "Marie Lk",
  "Mathers Cr", "Mathers Cr",
  "Mosquito Cr/QCI", "Mosquito Lk",
  "Mosquito Lk", "Mosquito Lk",
  "Pallant Cr", "Pallant Cr",
  "Pallant Cr Low", "Pallant Cr",
  "Pallant Cr Up", "Pallant Cr",
  "Sachs Cr", "Sachs Cr",
  "Sangan R", "Sangan R",
  "Sewell Hd Cr", "Sewell Hd Cr",
  "Stanley Lk", "Stanley Lk",
  "Tasu Cr", "Tasu Cr",
  "Tasu_Flat Cr", "Tasu Cr",
  "Tasu+Flat Cr", "Tasu Cr",
  "Torney Cr", "Torney Cr"
)

# Join the lookup table onto releasedata using Release.Site.Name
# If a site name appears in the lookup table, use the standardized name
# If it does not appear, keep the original Release.Site.Name
releasedata <- releasedata %>%
  left_join(lookup, by = c("Release.Site.Name" = "SITE NAME")) %>%
  mutate(`Site.Name.2` = coalesce(`SITE NAME 2`, Release.Site.Name)) %>%
  select(-`SITE NAME 2`)

# Convert Site.Name.2 to a factor
releasedata$`Site.Name.2` <- as.factor(releasedata$`Site.Name.2`)

# View all standardized site names
levels(releasedata$`Site.Name.2`)

# Preview the updated dataset
head(releasedata)


# ------------------------------------------------------------
# 6. Create a subset for mapping / summary analyses
# ------------------------------------------------------------

# Keep only selected stock PSC region codes:
# NASK = Northern Alaska
# COBC = Central BC
# QCI  = Queen Charlotte Islands / Haida Gwaii
#
# Also keep only the columns needed for release summaries
mapreleasedata <- releasedata %>%
  dplyr::filter(Stock.PSC.Region.Code %in% c("NASK", "COBC", "QCI")) %>%
  dplyr::select(
    Release.Site.Name,
    Site.Name.2,
    Stock.PSC.Region.Code,
    Brood.Year,
    Release.Year,
    Stage.Code,
    Num.WithCWT.Adclip,
    Num.WithCWT.NoAdclip,
    Num.WithCWT.UnknAD,
    Num.NoCWT.Adclip,
    Num.NoCWT.NoAdclip,
    Num.NoCWT.UnknAD,
    Total.Released,
    Percent.Tagged
  )


# ------------------------------------------------------------
# 7. Calculate total releases with and without CWTs
# ------------------------------------------------------------

# Create two summary columns:
# - Total.Released.With.CWT = fish released with coded-wire tags
# - Total.Released.No.CWT   = fish released without coded-wire tags
mapreleasedata <- mapreleasedata %>%
  dplyr::mutate(
    Total.Released.With.CWT = Num.WithCWT.Adclip +
      Num.WithCWT.NoAdclip +
      Num.WithCWT.UnknAD,
    
    Total.Released.No.CWT = Num.NoCWT.Adclip +
      Num.NoCWT.NoAdclip +
      Num.NoCWT.UnknAD
  )


# ------------------------------------------------------------
# 8. Clean Percent.Tagged
# ------------------------------------------------------------

# Convert Percent.Tagged to numeric
# This line removes dollar signs, although if Percent.Tagged actually contains
# percent symbols ("%") instead, you may want to replace "%" instead of "$"
mapreleasedata <- mapreleasedata %>%
  dplyr::mutate(
    Percent.Tagged = as.numeric(gsub("\\$", "", Percent.Tagged))
  )

# Preview the cleaned data
head(mapreleasedata)


# ------------------------------------------------------------
# 9. Drop unused factor levels after subsetting
# ------------------------------------------------------------

# Re-factor Release.Site.Name after filtering so only remaining levels are kept
mapreleasedata$Release.Site.Name <- as.factor(mapreleasedata$Release.Site.Name)
mapreleasedata$Release.Site.Name <- droplevels(mapreleasedata$Release.Site.Name)

mapreleasedata$Site.Name.2 <- as.factor(mapreleasedata$Site.Name.2)
mapreleasedata$Site.Name.2 <- droplevels(mapreleasedata$Site.Name.2)

# View remaining release site names
levels(mapreleasedata$Release.Site.Name)

# Re-factor Stage.Code after filtering so only remaining levels are kept
mapreleasedata$Stage.Code <- as.factor(mapreleasedata$Stage.Code)
mapreleasedata$Stage.Code <- droplevels(mapreleasedata$Stage.Code)

# View remaining stage codes
levels(mapreleasedata$Stage.Code)


head(mapreleasedata)








period_levels <- c("1972-1988", "1989-2005", "2006-2025")

# Create a lookup table: one region code per site
site_region_lookup <- mapreleasedata %>%
  select(Site.Name.2, Stock.PSC.Region.Code) %>%
  distinct()

# Add brood-year period labels
mapreleasedata_periods <- mapreleasedata %>%
  mutate(
    `Brood year period` = case_when(
      Brood.Year >= 1972 & Brood.Year <= 1988 ~ "1972-1988",
      Brood.Year >= 1989 & Brood.Year <= 2005 ~ "1989-2005",
      Brood.Year >= 2006 & Brood.Year <= 2025 ~ "2006-2025",
      TRUE ~ NA_character_
    )
  )

# Period summary + fill missing
period_summary <- mapreleasedata_periods %>%
  filter(!is.na(`Brood year period`)) %>%
  group_by(Site.Name.2, `Brood year period`) %>%
  summarise(
    Num.WithCWT.Adclip = sum(Num.WithCWT.Adclip, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    Site.Name.2,
    `Brood year period` = period_levels,
    fill = list(Num.WithCWT.Adclip = 0)
  )

# All-time summary
all_time_summary <- mapreleasedata %>%
  group_by(Site.Name.2) %>%
  summarise(
    Num.WithCWT.Adclip = sum(Num.WithCWT.Adclip, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(`Brood year period` = "All time")

# Combine + attach region code
cwtreleasesummary <- bind_rows(all_time_summary, period_summary) %>%
  left_join(site_region_lookup, by = "Site.Name.2") %>%
  mutate(
    `Brood year period` = factor(
      `Brood year period`,
      levels = c("All time", period_levels)
    )
  ) %>%
  arrange(Site.Name.2, `Brood year period`)

cwtreleasesummary <- cwtreleasesummary %>%
  rename(
    Site.Name = Site.Name.2,
    Brood.Year.Period = `Brood year period`
  )

cwtreleasesummary <- cwtreleasesummary %>%
  select(Site.Name, Stock.PSC.Region.Code, everything())

head(cwtreleasesummary)


#



























########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################







































############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################

####################################### RECOVERY DATA ######################################

############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################



# ------------------------------------------------------------
# 1. Read in Canadian CWT recovery data
# ------------------------------------------------------------

# Import the Canadian coded-wire-tag recovery dataset from CSV
CAcwtdata <- read.csv(
  here("data", "raw", "Canada Recoveries.csv")
)

# Preview the first few rows to confirm the file imported correctly
head(CAcwtdata)

# Check whether any recovery record IDs are duplicated
# Ideally this returns FALSE, meaning each recovery record is unique
any(duplicated(CAcwtdata$X.RC..MRP.Record.ID))


# ------------------------------------------------------------
# 2. Read in Alaska CWT recovery data
# ------------------------------------------------------------

# Import the Alaska recovery dataset from CSV
AKcwtdata <- read.csv(
  here("data", "raw", "Alaska Recoveries.csv")
)

# Preview the first few rows
head(AKcwtdata)

# Check whether any recovery record IDs are duplicated
any(duplicated(AKcwtdata$X.RC..MRP.Record.ID))


# ------------------------------------------------------------
# 3. Combine Canadian and Alaska recovery datasets
# ------------------------------------------------------------

# Stack the two datasets row-wise into one combined recovery table
cwtdata <- bind_rows(CAcwtdata, AKcwtdata)

# Check dimensions of the combined dataset
dim(cwtdata)

# View all column names
colnames(cwtdata)

# Confirm that recovery record IDs are still unique after combining
any(duplicated(cwtdata$X.RC..MRP.Record.ID))


# ------------------------------------------------------------
# 4. Create broad recovery-location category: Level 0
# ------------------------------------------------------------

# Translate PSC Level 0 numeric location codes into broad region names
# These represent the largest-scale recovery geography
cwtdata <- cwtdata %>%
  mutate(
    `Level 0` = case_when(
      X.RC..Recovery.PSC.Location.L0 == "1" ~ "Alaska",
      X.RC..Recovery.PSC.Location.L0 == "2" ~ "Canada",
      X.RC..Recovery.PSC.Location.L0 == "3" ~ "Washington",
      X.RC..Recovery.PSC.Location.L0 == "4" ~ "Idaho",
      X.RC..Recovery.PSC.Location.L0 == "5" ~ "Oregon",
      X.RC..Recovery.PSC.Location.L0 == "6" ~ "California",
      X.RC..Recovery.PSC.Location.L0 == "7" ~ "High Seas",
      TRUE ~ NA_character_
    )
  )

# Convert Level 0 to an ordered factor so plots/tables appear in a sensible order
cwtdata <- cwtdata %>%
  mutate(
    `Level 0` = factor(
      `Level 0`,
      levels = c("High Seas", "Alaska", "Canada", "Washington", "Idaho", "Oregon", "California")
    )
  )

# Preview results
head(cwtdata)


# ------------------------------------------------------------
# 5. Create freshwater / marine category: Level 1
# ------------------------------------------------------------

# Translate PSC Level 1 codes into broad environment type
# M = Marine, F = Freshwater
cwtdata <- cwtdata %>%
  mutate(
    `Level 1` = case_when(
      X.RC..Recovery.PSC.Location.L1 == "M" ~ "Marine",
      X.RC..Recovery.PSC.Location.L1 == "F" ~ "Freshwater",
      TRUE ~ NA_character_
    )
  )

# Preview results
head(cwtdata)


# ------------------------------------------------------------
# 6. Create a lookup table for Canadian Level 3 codes
# ------------------------------------------------------------

# This named vector maps Canadian PSC Level 3 location codes
# to your custom Level 2 regional groupings
#
# Example:
#   "01" -> "NWVI"
#   "05" -> "CBC"
#   "06" -> "NBC"
#
# Any codes not listed here will stay unmapped and become NA
l3_map_L0_2 <- c(
  "01" = "NWVI",
  "02" = "SWVI",
  "04" = "SGEO",
  "05" = "CBC",
  "06" = "NBC",
  "08" = "SGEO",
  "09" = "NBC",
  "10" = "SGEO",
  "11" = "SGEO",
  "12" = "CBC",
  "13" = "SWVI",
  "14" = "SWVI",
  "19" = "SGEO",
  "20" = "NWVI",
  "21" = "SWVI",
  "22" = "SGEO",
  "23" = "SGEO",
  "24" = "SWVI",
  "25" = "NBC",
  "26" = "CBC",
  "28" = "SGEO",
  "36" = "NBC",
  "56" = "CBC",
  "57" = "CBC",
  "61" = "SGEO",
  "62" = "SGEO",
  "SK" = "NBC"
  # 97, 99, etc. are intentionally left unmapped
)


# ------------------------------------------------------------
# 7. Create custom recovery-region grouping: Level 2
# ------------------------------------------------------------

# Build a more detailed recovery-region classification using PSC location levels
#
# Logic:
# - Alaska recoveries are split into SSEAK, NSEAK, SCAK, ARCTIC-YK
# - Canadian recoveries use the L3 lookup table above
# - Washington / Oregon use L2 codes
# - Idaho, California, and High Seas are assigned directly
cwtdata <- cwtdata %>%
  mutate(
    `Level 2` = case_when(
      
      # -----------------------------
      # Alaska (L0 == 1)
      # -----------------------------
      
      # Southeast Alaska
      X.RC..Recovery.PSC.Location.L0 == "1" &
        X.RC..Recovery.PSC.Location.L2 == "1" &
        X.RC..Recovery.PSC.Location.L3 %in% c("SE", "SW") ~ "SSEAK",
      
      # Northern Southeast Alaska
      X.RC..Recovery.PSC.Location.L0 == "1" &
        X.RC..Recovery.PSC.Location.L2 == "1" &
        X.RC..Recovery.PSC.Location.L3 %in% c("NE", "NW") ~ "NSEAK",
      
      # Southcentral Alaska
      X.RC..Recovery.PSC.Location.L0 == "1" &
        X.RC..Recovery.PSC.Location.L2 == "2" ~ "SCAK",
      
      # Arctic / Yukon
      X.RC..Recovery.PSC.Location.L0 == "1" &
        X.RC..Recovery.PSC.Location.L2 == "3" ~ "ARCTIC-YK",
      
      
      # -----------------------------
      # Canada (L0 == 2)
      # -----------------------------
      
      # Use the Canadian Level 3 mapping vector
      X.RC..Recovery.PSC.Location.L0 == "2" ~
        recode(as.character(X.RC..Recovery.PSC.Location.L3),
               !!!l3_map_L0_2,
               .default = NA_character_),
      
      
      # -----------------------------
      # Washington (L0 == 3)
      # -----------------------------
      
      # Puget Sound
      X.RC..Recovery.PSC.Location.L0 == "3" &
        X.RC..Recovery.PSC.Location.L2 == "1" ~ "PUSO",
      
      # Washington coast / general Washington
      X.RC..Recovery.PSC.Location.L0 == "3" &
        X.RC..Recovery.PSC.Location.L2 == "2" ~ "WA",
      
      # Columbia River in Washington
      X.RC..Recovery.PSC.Location.L0 == "3" &
        X.RC..Recovery.PSC.Location.L2 == "3" ~ "COL RIVER WA",
      
      
      # -----------------------------
      # Idaho (L0 == 4)
      # -----------------------------
      
      X.RC..Recovery.PSC.Location.L0 == "4" ~ "ID",
      
      
      # -----------------------------
      # Oregon (L0 == 5)
      # -----------------------------
      
      # Columbia River in Oregon
      X.RC..Recovery.PSC.Location.L0 == "5" &
        X.RC..Recovery.PSC.Location.L2 == "2" ~ "COL RIVER OR",
      
      # Other Oregon
      X.RC..Recovery.PSC.Location.L0 == "5" &
        X.RC..Recovery.PSC.Location.L2 == "3" ~ "OR",
      
      
      # -----------------------------
      # California (L0 == 6)
      # -----------------------------
      
      X.RC..Recovery.PSC.Location.L0 == "6" ~ "CA",
      
      
      # -----------------------------
      # High Seas (L0 == 7)
      # -----------------------------
      
      X.RC..Recovery.PSC.Location.L0 == "7" ~ "HIGH SEAS",
      
      # Anything not classified above becomes NA
      TRUE ~ NA_character_
    )
  )

# Preview the updated dataset
head(cwtdata)

# Check how many records fall into each Level 2 category, including missing values
table(cwtdata$`Level 2`, useNA = "always")


# ------------------------------------------------------------
# 8. Inspect records with missing Level 2 assignments
# ------------------------------------------------------------

# Extract only rows where Level 2 is missing
# Keep the relevant PSC location fields so you can diagnose why they failed to map
cwtdata_missing_L2 <- cwtdata %>%
  filter(is.na(`Level 2`)) %>%
  select(
    X.RC..Recovery.PSC.Location.L0,
    X.RC..Recovery.PSC.Location.L1,
    X.RC..Recovery.PSC.Location.L2,
    X.RC..Recovery.PSC.Location.L3,
    X.RC..Recovery.PSC.Location.L4,
    X.RC..Catch.Region.Acronym
  )

# View the missing-Level-2 subset
cwtdata_missing_L2

# Count missing Level 2 values by broad PSC Level 0 region
cwtdata %>%
  filter(is.na(`Level 2`)) %>%
  count(X.RC..Recovery.PSC.Location.L0, name = "n") %>%
  arrange(desc(n))

# For missing Canadian records, count by L3 code
cwtdata %>%
  filter(is.na(`Level 2`),
         X.RC..Recovery.PSC.Location.L0 == "2") %>%
  count(X.RC..Recovery.PSC.Location.L3, name = "n") %>%
  arrange(desc(n))

# For missing Canadian records, count by L2 and L3 combination
cwtdata %>%
  filter(is.na(`Level 2`),
         X.RC..Recovery.PSC.Location.L0 == "2") %>%
  count(X.RC..Recovery.PSC.Location.L2,
        X.RC..Recovery.PSC.Location.L3,
        name = "n") %>%
  arrange(desc(n))

# For missing Alaska records, count by L3 code
cwtdata %>%
  filter(is.na(`Level 2`),
         X.RC..Recovery.PSC.Location.L0 == "1") %>%
  count(X.RC..Recovery.PSC.Location.L3, name = "n") %>%
  arrange(desc(n))

# For missing Alaska records, count by L2 and L3 combination
cwtdata %>%
  filter(is.na(`Level 2`),
         X.RC..Recovery.PSC.Location.L0 == "1") %>%
  count(X.RC..Recovery.PSC.Location.L2,
        X.RC..Recovery.PSC.Location.L3,
        name = "n") %>%
  arrange(desc(n))


# ------------------------------------------------------------
# 9. Check missing PSC Level 2 and Level 3 source fields
# ------------------------------------------------------------

# Count rows where the source PSC Level 2 field itself is missing or blank
# Summarize by release year and stock region
l2_missing_counts <- cwtdata %>%
  filter(
    is.na(X.RC..Recovery.PSC.Location.L2) |
      X.RC..Recovery.PSC.Location.L2 == ""
  ) %>%
  count(X.RL..Release.Year,
        X.RL..Stock.PSC.Region.Code,
        name = "n_raw") %>%
  arrange(X.RL..Release.Year, desc(n_raw))

l2_missing_counts

# Count rows where the source PSC Level 3 field itself is missing or blank
l3_missing_counts <- cwtdata %>%
  filter(
    is.na(X.RC..Recovery.PSC.Location.L3) |
      X.RC..Recovery.PSC.Location.L3 == ""
  ) %>%
  count(X.RL..Release.Year,
        X.RL..Stock.PSC.Region.Code,
        name = "n_raw") %>%
  arrange(X.RL..Release.Year, desc(n_raw))

l3_missing_counts


# ------------------------------------------------------------
# 10. Override / reclassify some Level 2 assignments
# ------------------------------------------------------------

# Apply manual fixes to Level 2 for specific situations:
# - Certain NASK freshwater sport-site recoveries are reclassified to NBC
# - PFMA 8 recoveries are reclassified to CBC
# - PFMA 23 recoveries are reclassified to SWVI
#
# If none of the special rules apply, keep the existing Level 2 value
cwtdata <- cwtdata %>%
  dplyr::mutate(
    `Level 2` = dplyr::case_when(
      
      # NASK freshwater recoveries from selected Skeena sport fishing sites -> NBC
      X.RL..Stock.PSC.Region.Code == "NASK" &
        `Level 1` == "Freshwater" &
        X.RC..Sport.Site.Code %in% c(
          "TROU CK", "TOBO CK", "SKEE RI",
          "POLY BR", "MORI RI", "MO TOWN",
          "LAKE RI", "BULK RI"
        ) ~ "NBC",
      
      # COBC freshwater recoveries from selected Skeena sport fishing sites -> CBC
      X.RL..Stock.PSC.Region.Code == "COBC" &
        `Level 1` == "Freshwater" &
        X.RC..Sport.Site.Code %in% c(
          "KITI RI", "BELL RI", "BCOO RI",
          "ATNA RI"
        ) ~ "CBC",
      
      X.RL..Stock.PSC.Region.Code == "COBC" &
        `Level 1` == "Freshwater" &
        X.RC..Sport.Site.Code %in% c(
          "LANG IS"
        ) ~ "NBC",
      
      # PFMA 8 - Fitz Hugh Sound -> CBC
      X.RC..MRP.Area.Name == "PFMA 8 - Fitz Hugh Sound" ~ "CBC",
      
      # PFMA 23 - Barkley Sound Portion -> SWVI
      X.RC..MRP.Area.Name == "PFMA 23 - Barkley Sound Portion" ~ "SWVI",
      
      # Otherwise leave Level 2 unchanged
      TRUE ~ `Level 2`
    )
  )

# Preview result
head(cwtdata)




kitimat_unknown <- cwtdata[
  cwtdata$Release.Site.Name.2 == "Kitimat R" &
    cwtdata$`Level 2` == "Unknown",
]



bellacoola_unknown <- cwtdata[
  cwtdata$Release.Site.Name.2 == "Bella Coola R" &
    cwtdata$`Level 2` == "Unknown",
]






# ------------------------------------------------------------
# 11. Create a lookup table to standardize release site names
# ------------------------------------------------------------

# This lookup table groups related release-site names under a common name
# It helps simplify later summaries and plots
lookup <- tibble::tribble(
  ~`SITE NAME`, ~`SITE NAME 2`,
  "Angler Cv Lk", "Angler Cv Lk",
  "Atnarko Ch", "Atnarko",
  "Atnarko R Low", "Atnarko",
  "Babine Lk", "Babine Lk",
  "Babine R", "Babine R",
  "Babine R Up", "Babine R",
  "Bella Coola R Low", "Bella Coola R",
  "Bella Coola R Up", "Bella Coola R",
  "Buck Cr", "Buck Cr",
  "Bulkley Lk", "Bulkley Lk",
  "Bulkley R Up", "Bulkley R Up",
  "Bulkley R+Toboggan", "Toboggan Cr",
  "Canyon Cr/SKNA", "Canyon Cr",
  "Cecil Cr", "Cecil Cr",
  "Clifford Cr", "Kispiox R",
  "Cullon Cr", "Kispiox R",
  "Damshilgwit Cr", "Slamgeesh R",
  "Diana Cr", "Dianna Cr",
  "Dry Cr/SKNA", "Dry Cr",
  "Fulton R", "Fulton R",
  "Hagensborg Sl", "Bella Coola R",
  "Hartley Bay Cr", "Hartley Bay Cr",
  "Hartley Bay Lk", "Hartley Bay",
  "Hays Cr", "Hays Cr",
  "Hodder Cr", "Kispiox R",
  "Johnston Cr", "Johnston Cr",
  "Johnston Est", "Johnston Est",
  "Kispiox R", "Kispiox R",
  "Kispiox R Tribs", "Kispiox R",
  "Kitasoo Cr", "Kitasoo Cr",
  "Kitimat R", "Kitimat R",
  "Kitimat R Low", "Kitimat R",
  "Kitsumkalum R", "Kitsumkalum R",
  "Kitsumkalum R Up-use6449", "Kitsumkalum R",
  "Kitwanga R", "Kitwanga R",
  "Ksi Gingolx (Kincolith) R", "Ksi Gingolx (Kincolith) R",
  "Kincolith R", "Kincolith R",
  "Lachmach R", "Lachmach R",
  "Martin R", "Martin R",
  "McBride Cr", "McBride Cr",
  "McLoughlin Bay", "McLoughlin Bay",
  "McLoughlin Bay Cr", "McLoughlin Bay Cr",
  "McQueen Cr", "Kispiox R",
  "Mission Cr/SKNA", "Mission Cr",
  "Morice R", "Morice R",
  "Murder Cr", "Kispiox R",
  "Nilkitkwa Lk", "Nilkitkwa Lk",
  "Oldfield Cr", "Oldfield Cr",
  "Owen Cr", "Owen Cr",
  "Owen Lk", "Owen Cr",
  "Red Bluff Lk", "Hartley Bay",
  "Saloompt R", "Bella Coola R",
  "Second Lk/NCST", "Second Lk",
  "Skunsnat Cr", "Kispiox R",
  "Slamgeesh R", "Slamgeesh R",
  "Snootli Cr", "Bella Coola R",
  "Sylvia Lk", "Hartley Bay",
  "Tahlo Cr", "Tahlo Cr",
  "Thornhill Cr", "Thornhill Cr",
  "Thorsen Cr/CCST", "Bella Coola R",
  "Toboggan Cr", "Toboggan Cr",
  "Trout Bay", "Trout Bay",
  "Tseax R", "Tseax R",
  "Union Pass Lk", "Hartley Bay",
  "Waterfall Cr/SKNA", "Mission Cr",
  "Waterfalls Cr", "Kispiox R",
  "West Arm Cr (Drake Cr)", "West Arm Cr (Drake Cr)",
  "Whalen Lk", "Hartley Bay",
  "Zolzap Cr", "Zolzap Cr",
  "Zymacord R", "Zymacord R",
  
  "Braverman Cr", "Braverman Cr",
  "Chown Brk", "Chown Brk",
  "Coates Cr", "Coates Cr",
  "Copper Cr", "Copper Cr",
  "Deer Bay", "Deer Bay",
  "Fukwa Cr", "Fukwa Cr",
  "Gold Cr", "Gold Cr",
  "Haans Cr", "Haans Cr",
  "Honna R", "Honna R",
  "King Cr/QCI", "King Cr",
  "Marie Lk", "Marie Lk",
  "Mathers Cr", "Mathers Cr",
  "Mosquito Cr/QCI", "Mosquito Lk",
  "Mosquito Lk", "Mosquito Lk",
  "Pallant Cr", "Pallant Cr",
  "Pallant Cr Low", "Pallant Cr",
  "Pallant Cr Up", "Pallant Cr",
  "Sachs Cr", "Sachs Cr",
  "Sangan R", "Sangan R",
  "Sewell Hd Cr", "Sewell Hd Cr",
  "Stanley Lk", "Stanley Lk",
  "Tasu Cr", "Tasu Cr",
  "Tasu_Flat Cr", "Tasu Cr",
  "Torney Cr", "Torney Cr"
)

# Join the release-site lookup table onto cwtdata
# If a site matches the lookup table, use the grouped name
# If not, keep the original release-site name
cwtdata <- cwtdata %>%
  left_join(lookup, by = c("X.RL..Release.Site.Name" = "SITE NAME")) %>%
  mutate(`Release.Site.Name.2` = as.factor(coalesce(`SITE NAME 2`, X.RL..Release.Site.Name))) %>%
  select(-`SITE NAME 2`)


# ------------------------------------------------------------
# 12. Create a condensed analysis dataset
# ------------------------------------------------------------

# Keep only the variables needed for the downstream analysis
cwtdata_condensed <- cwtdata[, c(
  "X.RL..MRP.Record.ID",
  "X.RC..MRP.Record.ID",
  "X.RL..Country.Code",
  "X.RL..Brood.Year",
  "X.RL..Release.Year",
  "X.RL..Release.Site.Code..CDFO.",
  "X.RL..Release.Site.Name",
  "Release.Site.Name.2",
  "X.RL..Release.PSC.Region.Code",
  "X.RL..Stock.PSC.Region.Code",
  "X.RC..Recovery.Year",
  "X.RC..Recovery.Month",
  "X.RC..Stat.Week..MMW.",
  "X.RC..Catch.Region.Code",
  "X.RC..Catch.Region.Acronym",
  "X.RC..MRP.Area.Name",
  "X.RC..Sport.Site.Code",
  "X.RC..Sport.Site.Name",
  "X.RC..Age...Total",
  "X.RC..Age...Ocean",
  "X.RC..Fishery.PSC.Code",
  "X.RC..Gear.PSC.Code",
  "X.RC..Observed.Number",
  "X.RC..Estimated.Number",
  "X.RC..Expanded.Number",
  "Level 0",
  "Level 1",
  "Level 2"
)]

# Preview condensed dataset
head(cwtdata_condensed)


# ------------------------------------------------------------
# 13. Convert selected columns to factors
# ------------------------------------------------------------

# Check class of recovery month
class(cwtdata_condensed$X.RC..Recovery.Month)

# Convert recovery month to a factor
cwtdata_condensed$X.RC..Recovery.Month <- as.factor(cwtdata_condensed$X.RC..Recovery.Month)

# View all recovery month levels
levels(cwtdata_condensed$X.RC..Recovery.Month)

# Check class of Level 2
class(cwtdata_condensed$`Level 2`)

# Convert Level 2 to factor
cwtdata_condensed$`Level 2` <- as.factor(cwtdata_condensed$`Level 2`)

# View all Level 2 levels
levels(cwtdata_condensed$`Level 2`)


# ------------------------------------------------------------
# 14. Create a seasonal recovery variable
# ------------------------------------------------------------

# Map recovery month to a season
cwtdata_condensed <- cwtdata_condensed %>%
  mutate(
    X.RC..Recovery.season = case_when(
      X.RC..Recovery.Month %in% c("12", "1", "2")  ~ "Winter",
      X.RC..Recovery.Month %in% c("3", "4", "5")   ~ "Spring",
      X.RC..Recovery.Month %in% c("6", "7", "8")   ~ "Summer",
      X.RC..Recovery.Month %in% c("9", "10", "11") ~ "Fall",
      TRUE ~ NA_character_
    )
  )

# Convert season to an ordered factor so it keeps the calendar order
cwtdata_condensed <- cwtdata_condensed %>%
  mutate(
    X.RC..Recovery.season = factor(
      X.RC..Recovery.season,
      levels = c("Winter", "Spring", "Summer", "Fall"),
      ordered = TRUE
    )
  )

# Cross-tabulate month and season to verify the mapping worked
table(cwtdata_condensed$X.RC..Recovery.Month,
      cwtdata_condensed$X.RC..Recovery.season)


# ------------------------------------------------------------
# 15. Inspect stock-region codes
# ------------------------------------------------------------

# Convert stock PSC region code to a factor
cwtdata_condensed$X.RL..Stock.PSC.Region.Code <- as.factor(cwtdata_condensed$X.RL..Stock.PSC.Region.Code)

# View the stock-region levels
levels(cwtdata_condensed$X.RL..Stock.PSC.Region.Code)

# Preview the condensed data
head(cwtdata_condensed)


# ------------------------------------------------------------
# 16. Summarize estimated recoveries by stock region and Level 2
# ------------------------------------------------------------

# For each stock region and Level 2 recovery region,
# sum the estimated number of fish recovered
stock_by_region <- cwtdata_condensed %>%
  filter(!is.na(`Level 2`)) %>%
  group_by(X.RL..Stock.PSC.Region.Code, `Level 2`) %>%
  summarise(
    n_fish = sum(X.RC..Estimated.Number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(X.RL..Stock.PSC.Region.Code, desc(n_fish))

stock_by_region


# ------------------------------------------------------------
# 17. Summarize raw tag counts by stock region and Level 2
# ------------------------------------------------------------

# Count raw numbers of tag-recovery records, rather than estimated fish numbers
stock_by_region_raw <- cwtdata_condensed %>%
  filter(!is.na(`Level 2`)) %>%
  count(X.RL..Stock.PSC.Region.Code, `Level 2`, name = "n_tags") %>%
  arrange(X.RL..Stock.PSC.Region.Code, desc(n_tags))

stock_by_region_raw


# ------------------------------------------------------------
# 18. Count how many Level 2 values are still missing
# ------------------------------------------------------------

# Summarize the number of missing Level 2 assignments by stock region
cwtdata_condensed %>%
  filter(is.na(`Level 2`)) %>%
  count(X.RL..Stock.PSC.Region.Code, name = "n_missing") %>%
  arrange(desc(n_missing))


# ------------------------------------------------------------
# 19. Replace missing Level 2 values with "Unknown"
# ------------------------------------------------------------

# Convert Level 2 to character, replace missing/blank with "Unknown",
# then convert back to factor
cwtdata_condensed <- cwtdata_condensed %>%
  mutate(
    `Level 2` = as.character(`Level 2`),
    `Level 2` = if_else(
      is.na(`Level 2`) | `Level 2` == "",
      "Unknown",
      `Level 2`
    ),
    `Level 2` = factor(`Level 2`)
  )


# ------------------------------------------------------------
# 20. Inspect NASK records classified as Unknown
# ------------------------------------------------------------

# Extract all NASK stock records whose Level 2 recovery region is "Unknown"
# This is useful for quality control and further recoding if needed
nask_unknown <- cwtdata_condensed %>%
  filter(
    X.RL..Stock.PSC.Region.Code == "NASK",
    `Level 2` == "Unknown"
  )

# View those records
nask_unknown





# ------------------------------------------------------------
# 21. Join PSC fishery code definitions onto the condensed CWT dataset
# ------------------------------------------------------------

# Preview the current condensed recovery dataset
# This is a quick check to confirm the data structure before joining
head(cwtdata_condensed)

# Read in the lookup table that contains PSC fishery code definitions
# This file links each PSC fishery ID to a fishery name and notes/description
fisherycodedata <- read.csv(
  here("data", "raw", "export definitions(PSC fishery).csv")
)

# Preview the first few rows of the fishery code lookup table
head(fisherycodedata)

# Check the class (data type) of the fishery code column in cwtdata_condensed
# This is important because join keys should ideally have matching types
class(cwtdata_condensed$X.RC..Fishery.PSC.Code)

# Check the class (data type) of the PSC fishery ID column in the lookup table
class(fisherycodedata$PSC_FISHERY_ID)

fisherycodedata <- fisherycodedata %>%
  mutate(
    PSC_FISHERY_NOTES = if_else(
      PSC_FISHERY_NOTES == "Aboriginal",
      "First Nations",
      PSC_FISHERY_NOTES
    )
  )

# Join fishery name and notes onto cwtdata_condensed
#
# Keep only the lookup columns needed from fisherycodedata:
# - PSC_FISHERY_ID   = the code used as the join key
# - PSC_FISHERY_NAME = human-readable fishery name
# - PSC_FISHERY_NOTES = extra descriptive notes about the fishery
#
# Match:
#   cwtdata_condensed$X.RC..Fishery.PSC.Code
#   to
#   fisherycodedata$PSC_FISHERY_ID
#
# left_join() keeps all rows in cwtdata_condensed, even if some fishery codes
# do not find a match in the lookup table
cwtdata_condensed <- cwtdata_condensed %>%
  left_join(
    fisherycodedata %>%
      select(
        PSC_FISHERY_ID,
        PSC_FISHERY_NAME,
        PSC_FISHERY_NOTES
      ),
    by = c("X.RC..Fishery.PSC.Code" = "PSC_FISHERY_ID")
  )

# Preview the updated condensed dataset to confirm that the new columns
# (PSC_FISHERY_NAME and PSC_FISHERY_NOTES) were added successfully
head(cwtdata_condensed)

# GS Save CWT Condensed =========
write_csv(cwtdata_condensed, "data/Coho_CWT_Condensed.csv")


# Count how many rows do and do not have a matched fishery name after the join
# FALSE = fishery name is present
# TRUE  = fishery name is missing (no match found in the lookup table)
table(is.na(cwtdata_condensed$PSC_FISHERY_NAME))



























############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################












# ------------------------------------------------------------
# Summarize recoveries by stock region and recovery region,
# then calculate the proportional contribution of each
# recovery region within each stock.
# ------------------------------------------------------------


min(cwtdata_condensed$X.RC..Recovery.Year, na.rm = TRUE)


prop_data <- cwtdata_condensed %>%
  filter(!is.na(`Level 2`)) %>%
  group_by(X.RL..Stock.PSC.Region.Code, `Level 2`) %>%
  summarise(
    total_observed = sum(X.RC..Observed.Number,  na.rm = TRUE),
    total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(X.RL..Stock.PSC.Region.Code) %>%
  mutate(
    proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(X.RL..Stock.PSC.Region.Code != "WEAK") %>%
  mutate(
    # Force AYK to be 100% Arctic-YK
    proportion = dplyr::if_else(
      X.RL..Stock.PSC.Region.Code == "AYK" & `Level 2` == "ARCTIC-YK",
      1,
      dplyr::if_else(
        X.RL..Stock.PSC.Region.Code == "AYK",
        0,
        proportion
      )
    ),
    
    # Set x-axis order
    X.RL..Stock.PSC.Region.Code = factor(
      X.RL..Stock.PSC.Region.Code,
      levels = c("AYK", "CNAK", "SEAK", "QCI", "NASK", "COBC", "JNST")
    ),
    
    # Set legend order
    `Level 2` = factor(
      `Level 2`,
      levels = c(
        "HIGH SEAS",
        "ARCTIC-YK",
        "SCAK",
        "NSEAK",
        "SSEAK",
        "NBC",
        "CBC",
        "NWVI",
        "SWVI",
        "SGEO",
        "PUSO",
        "WA",
        "COL RIVER WA",
        "COL RIVER OR",
        "CA",
        "Unknown"
      )
    )
  )








# Get default discrete palette
base_cols <- hue_pal()(length(levels(prop_data$`Level 2`)))
names(base_cols) <- levels(prop_data$`Level 2`)

# Override Unknown only
base_cols["Unknown"] <- "grey70"




# ------------------------------------------------------------
# Plot proportional contribution of recovery regions
# within each stock region
# ------------------------------------------------------------


ggplot(prop_data,
       aes(x = X.RL..Stock.PSC.Region.Code,
           y = proportion,
           fill = `Level 2`)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = base_cols, drop = FALSE) +
  labs(
    x = "Stock Region Code",
    y = "Percentage of Estimated Catch",
    fill = "Recovery Region",
    title = "Percentage of each stock caught by catch region"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







custom_cols <- c(
  "HIGH SEAS"    = "#B22222",  # dep red
  "ARCTIC-YK"    = "#E64A19",  # red-orange
  "SCAK"         = "#FF8A65",  # orange
  "NSEAK"        = "#FFB74D",  # light orange
  "SSEAK"        = "#FFD54F",  # yellow
  
  "NBC"          = "#66BB6A",  # green
  "CBC"          = "#2E7D32",  # darker green
  
  "NWVI"         = "#1565C0",  # deep blue
  "SWVI"         = "#64B5F6",  # light blue
  "SGEO"         = "#BA68C8",  # light purple
  "PUSO"         = "#7B1FA2",  # darker purple
  
  "WA"           = "#F8BBD0",  # very light pink
  "COL RIVER WA" = "#F48FB1",  # soft pink
  "COL RIVER OR" = "#EC407A",  # medium pink
  "CA"           = "#C2185B",   # deep pink
  
  "Unknown"      = "grey70"    # grey
)






ggplot(prop_data,
       aes(x = X.RL..Stock.PSC.Region.Code,
           y = proportion,
           fill = `Level 2`)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = custom_cols, drop = FALSE) +
  labs(
    x = "Stock Region Code",
    y = "Percentage of Estimated Catch",
    fill = "Recovery Region",
    title = "Percentage of each stock caught by catch region"
  ) +
  coord_flip() +
  theme_minimal() +
  theme_minimal(base_size = 20) 

















# Process CWT recovery data for Northern Alaska (NASK) stocks.
# Filter dataset to include only NASK stock region and non-missing Level 2 categories.
# Summarize observed and estimated recoveries by release site and recovery region.
# Calculate proportional contribution of each recovery region within each release site
# based on total estimated recoveries.
# Convert Level 2 to a factor with predefined ordering for consistent visualization.


prop_data_NASK <- cwtdata_condensed %>%
  filter(
    X.RL..Stock.PSC.Region.Code == "NASK",
    !is.na(`Level 2`)
  ) %>%
  group_by(Release.Site.Name.2, `Level 2`) %>%
  summarise(
    total_observed = sum(X.RC..Observed.Number, na.rm = TRUE),
    total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Release.Site.Name.2) %>%
  mutate(
    proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    `Level 2` = factor(
      `Level 2`,
      levels = c(
        "HIGH SEAS",
        "ARCTIC-YK",
        "SCAK",
        "NSEAK",
        "SSEAK",
        "NBC",
        "CBC",
        "NWVI",
        "SWVI",
        "SGEO",
        "PUSO",
        "WA",
        "COL RIVER WA",
        "COL RIVER OR",
        "CA",
        "Unknown"
      )
    )
  )







# Inspect current factor levels for Level 2 recovery regions.
# Remove unused factor levels to clean up the dataset after filtering/subsetting.
# Re-check levels to confirm only active categories remain.

levels(prop_data_NASK$`Level 2`)
prop_data_NASK$`Level 2`<-droplevels(prop_data_NASK$`Level 2`)
levels(prop_data_NASK$`Level 2`)









# Define a custom color palette for Level 2 recovery regions.
# Colors are grouped by geographic region to improve interpretability:
# - Off-shore regions (HIGH SEAS): red
# - Alaska (SCAK, NSEAK, SSEAK): warm colors (orange → yellow)
# - British Columbia mainland (NBC, CBC): greens
# - Vancouver Island / south coast (NWVI, SWVI, SGEO, PUSO): blues and purples
# - Columbia River (COL RIVER OR): pink to distinguish from other regions
# - Unknown: grey to indicate undefined or missing origin


custom_cols <- c(
  "HIGH SEAS"    = "#E64A19",  # red-orange
  
  "SCAK"         = "#FF8A65",  # orange
  "NSEAK"        = "#FFB74D",  # light orange
  "SSEAK"        = "#FFD54F",  # yellow
  
  "NBC"          = "#66BB6A",  # green
  "CBC"          = "#2E7D32",  # darker green
  
  "NWVI"         = "#1565C0",  # deep blue
  "SWVI"         = "#64B5F6",  # light blue
  "SGEO"         = "#BA68C8",  # light purple
  "PUSO"         = "#7B1FA2",  # darker purple
  
  "COL RIVER OR" = "#F06292",   # pink
  
  "Unknown"      = "grey70"    # grey
)








# Create a stacked bar plot showing the proportional distribution of estimated
# recoveries by recovery region (Level 2) for each release site.
# Bars are scaled to proportions (position = "fill") so each site sums to 100%.
# Apply custom color palette for consistent geographic grouping of regions.
# Flip coordinates to display horizontal bars for improved readability of site names.
# Adjust axis labels, title, and legend for clarity, and increase base text size for presentation.

ggplot(
  prop_data_NASK,
  aes(
    x = Release.Site.Name.2,
    y = proportion,
    fill = `Level 2`
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = base_cols, drop = FALSE) +
  labs(
    x = "Release Site Name",
    y = "Percentage of Estimated Catch",
    fill = "Recovery Region",
    title = "Percentage of estimated catch by recovery region for NASK release sites"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  scale_fill_manual(values = custom_cols, drop = FALSE)+
  coord_flip()+ 
  theme_minimal(base_size = 16)








# Check the full range of brood years represented in the condensed CWT dataset.
# Filter data to include only NASK stocks with valid Level 2 recovery regions and brood years.
# Group brood years into three approximately equal time periods for temporal comparison.
# Summarize observed and estimated recoveries by brood period, release site, and recovery region.
# Calculate the proportional contribution of each recovery region within each release site
# for each brood-period grouping.
# Convert brood period and Level 2 to ordered factors to control panel and legend order in plots.



range(cwtdata_condensed$X.RL..Brood.Year)


prop_data_NASK <- cwtdata_condensed %>%
  filter(
    X.RL..Stock.PSC.Region.Code == "NASK",
    !is.na(`Level 2`),
    !is.na(X.RL..Brood.Year)
  ) %>%
  mutate(
    Brood.Period = case_when(
      X.RL..Brood.Year >= 1972 & X.RL..Brood.Year <= 1988 ~ "1972-1988",
      X.RL..Brood.Year >= 1989 & X.RL..Brood.Year <= 2005 ~ "1989-2005",
      X.RL..Brood.Year >= 2006 & X.RL..Brood.Year <= 2023 ~ "2006-2023"
    )
  ) %>%
  group_by(Brood.Period, Release.Site.Name.2, `Level 2`) %>%
  summarise(
    total_observed = sum(X.RC..Observed.Number, na.rm = TRUE),
    total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Brood.Period, Release.Site.Name.2) %>%
  mutate(
    proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Brood.Period = factor(
      Brood.Period,
      levels = c("1972-1988", "1989-2005", "2006-2023")
    ),
    `Level 2` = factor(
      `Level 2`,
      levels = c(
        "HIGH SEAS",
        "SCAK",
        "NSEAK",
        "SSEAK",
        "NBC",
        "CBC",
        "NWVI",
        "SWVI",
        "SGEO",
        "PUSO",
        "COL RIVER OR",
        "Unknown"
      )
    )
  )




# Remove unused Level 2 factor levels after filtering and summarization
# to ensure only active recovery regions are retained for plotting and analysis.

prop_data_NASK$`Level 2` <- droplevels(prop_data_NASK$`Level 2`)



custom_cols <- c(
  "HIGH SEAS"    = "#E64A19",
  "SCAK"         = "#FF8A65",
  "NSEAK"        = "#FFB74D",
  "SSEAK"        = "#FFD54F",
  "NBC"          = "#66BB6A",
  "CBC"          = "#2E7D32",
  "NWVI"         = "#1565C0",
  "SWVI"         = "#64B5F6",
  "SGEO"         = "#BA68C8",
  "PUSO"         = "#7B1FA2",
  "COL RIVER OR" = "#F06292",
  "Unknown"      = "grey70"
)














# Create stacked proportional bar plot for NASK release sites during brood years 1972–1988.
# Filter data to the specified brood period.
# Display proportional contribution of each recovery region (Level 2) within each release site.
# Apply custom color palette for consistent regional grouping.
# Flip coordinates to improve readability of release site names.
# Increase base text size for clarity in reports and presentations.


plot1 <- ggplot(
  filter(prop_data_NASK, Brood.Period == "1972-1988"),
  aes(
    x = Release.Site.Name.2,
    y = proportion,
    fill = `Level 2`
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = custom_cols, drop = FALSE) +
  labs(
    x = "Release Site Name",
    y = "Percentage of Estimated Catch",
    fill = "Recovery Region",
    title = "NASK release sites (Brood years 1972-1988)"
  ) +
  coord_flip() +
  theme_minimal(base_size = 16)






# Create stacked proportional bar plot for NASK release sites during brood years 1989–2005.
# Filter data to the specified brood period.


plot2 <- ggplot(
  filter(prop_data_NASK, Brood.Period == "1989-2005"),
  aes(
    x = Release.Site.Name.2,
    y = proportion,
    fill = `Level 2`
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = custom_cols, drop = FALSE) +
  labs(
    x = "Release Site Name",
    y = "Percentage of Estimated Catch",
    fill = "Recovery Region",
    title = "NASK release sites (Brood years 1989-2005)"
  ) +
  coord_flip() +
  theme_minimal(base_size = 16)





# Create stacked proportional bar plot for NASK release sites during brood years 2006–2023.
# Filter data to the specified brood period.


plot3 <- ggplot(
  filter(prop_data_NASK, Brood.Period == "2006-2023"),
  aes(
    x = Release.Site.Name.2,
    y = proportion,
    fill = `Level 2`
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = custom_cols, drop = FALSE) +
  labs(
    x = "Release Site Name",
    y = "Percentage of Estimated Catch",
    fill = "Recovery Region",
    title = "NASK release sites (Brood years 2006-2023)"
  ) +
  coord_flip() +
  theme_minimal(base_size = 16)




# View plots

plot1
plot2
plot3











# Create faceted stacked bar plot showing proportional distribution of estimated
# recoveries by recovery region (Level 2) for each release site.
# Bars are scaled to proportions (position = "fill") so each site sums to 100%.
# Apply custom color palette for consistent geographic grouping of regions.
# Flip coordinates to display horizontal bars for improved readability of site names.
# Facet the plot by brood period to compare patterns across time periods.
# Use smaller base text size to accommodate multiple panels in a single figure.

ggplot(
  prop_data_NASK,
  aes(
    x = Release.Site.Name.2,
    y = proportion,
    fill = `Level 2`
  )
) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = custom_cols, drop = FALSE) +
  labs(
    x = "Release Site Name",
    y = "Percentage of Estimated Catch",
    fill = "Recovery Region",
    title = "Percentage of estimated catch by recovery region for NASK release sites"
  ) +
  coord_flip() +
  facet_wrap(~ Brood.Period, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 10)







zym_unknown <- cwtdata_condensed[
  cwtdata_condensed$Release.Site.Name.2 == "Zymacord R" &
    cwtdata_condensed$`Level 2` == "Unknown",
]


tob_unknown <- cwtdata_condensed[
  cwtdata_condensed$Release.Site.Name.2 == "Toboggan Cr" &
    cwtdata_condensed$`Level 2` == "Unknown",
]


kit_unknown <- cwtdata_condensed[
  cwtdata_condensed$Release.Site.Name.2 == "Kitwanga R" &
    cwtdata_condensed$`Level 2` == "Unknown",
]




# End GS ======== 







# Summarize total number of coded-wire-tagged (CWT) fish released by site and brood period.
# Remove records with missing brood year.
# Aggregate total tagged releases (Total.Released.With.CWT) by release site and brood year.
# Assign brood years into three time periods for consistency with plotting framework.
# Further aggregate total releases by release site within each brood period.
# Create formatted label ("N = XX") for use in plot annotations.



release_labels <- mapreleasedata %>%
  filter(!is.na(Brood.Year)) %>%
  group_by(Site.Name.2, Brood.Year) %>%
  summarise(
    N_released = sum(Total.Released.With.CWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Brood.Period = case_when(
      Brood.Year >= 1972 & Brood.Year <= 1988 ~ "1972-1988",
      Brood.Year >= 1989 & Brood.Year <= 2005 ~ "1989-2005",
      Brood.Year >= 2006 & Brood.Year <= 2023 ~ "2006-2023"
    )
  ) %>%
  group_by(Brood.Period, Site.Name.2) %>%
  summarise(
    N_released = sum(N_released, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("N = ", format(N_released, big.mark = ","))
  )







# Join release summary data (total tagged releases) to the proportional recovery dataset.
# Match by release site name and brood period to align corresponding records.
# This adds N_released and formatted label (e.g., "N = XX") for use in plot annotations.

prop_data_NASK_lab <- prop_data_NASK %>%
  left_join(
    release_labels,
    by = c(
      "Release.Site.Name.2" = "Site.Name.2",
      "Brood.Period" = "Brood.Period"
    )
  )







# Create a dataset containing one row per release site and brood period for labeling.
# Remove duplicate rows so that each stacked bar receives a single "N = XX" annotation.
# Retain total releases (N_released) and formatted label for use in plotting.


label_data <- prop_data_NASK_lab %>%
  distinct(Brood.Period, Release.Site.Name.2, N_released, label)









# Create faceted stacked bar plot showing proportional distribution of estimated
# recoveries by recovery region (Level 2) for each release site.
# Bars are scaled to proportions (position = "fill") so each site sums to 100%.
# Overlay text labels indicating total number of tagged fish released (N) for each site
# and brood period, positioned just beyond the end of each bar.
# Adjust y-axis limits to accommodate label placement above 100%.
# Apply custom color palette and flip coordinates for improved readability.
# Facet by brood period to compare patterns across time periods.


ggplot(
  prop_data_NASK_lab,
  aes(
    x = Release.Site.Name.2,
    y = proportion,
    fill = `Level 2`
  )
) +
  geom_col(position = "fill") +
  geom_text(
    data = label_data,
    aes(
      x = Release.Site.Name.2,
      y = 1.02,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.6
  ) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, 1.12),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(values = custom_cols, drop = FALSE) +
  labs(
    x = "Release Site Name",
    y = "Percentage of Estimated Catch",
    fill = "Recovery Region",
    title = "Percentage of estimated catch by recovery region for NASK release sites"
  ) +
  coord_flip() +
  facet_wrap(~ Brood.Period, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 13)









# #
# 
# 
# prop_data_NASK_noUNK <- prop_data_NASK %>%
#   dplyr::filter(`Level 2` != "Unknown") %>%
#   group_by(Brood.Period, Release.Site.Name.2) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup()
# 
# 
# ggplot(
#   prop_data_NASK_noUNK,
#   aes(
#     x = Release.Site.Name.2,
#     y = proportion,
#     fill = `Level 2`
#   )
# ) +
#   geom_col(position = "fill") +
#   scale_y_continuous(labels = percent_format()) +
#   scale_fill_manual(values = custom_cols, drop = FALSE) +
#   labs(
#     x = "Release Site Name",
#     y = "Percentage of Estimated Catch",
#     fill = "Recovery Region",
#     title = "Percentage of estimated catch by recovery region for NASK release sites (Unknown removed)"
#   ) +
#   coord_flip() +
#   facet_wrap(~ Brood.Period, ncol = 1, scales = "free_y") +
#   theme_minimal(base_size = 10)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# custom_cols <- c(
#   "HIGH SEAS"    = "#E64A19",
#   "SCAK"         = "#FF8A65",
#   "NSEAK"        = "#FFB74D",
#   "SSEAK"        = "#FFD54F",
#   "NBC"          = "#66BB6A",
#   "CBC"          = "#2E7D32",
#   "NWVI"         = "#1565C0",
#   "SWVI"         = "#64B5F6",
#   "SGEO"         = "#BA68C8",
#   "PUSO"         = "#7B1FA2",
#   "COL RIVER OR" = "#F06292",
#   "Unknown"      = "grey70"
# )
# 
# prop_data_COBC <- cwtdata_condensed %>%
#   filter(
#     X.RL..Stock.PSC.Region.Code == "COBC",
#     !is.na(`Level 2`),
#     !is.na(X.RL..Brood.Year)
#   ) %>%
#   mutate(
#     Brood.Period = case_when(
#       X.RL..Brood.Year >= 1972 & X.RL..Brood.Year <= 1988 ~ "1972-1988",
#       X.RL..Brood.Year >= 1989 & X.RL..Brood.Year <= 2005 ~ "1989-2005",
#       X.RL..Brood.Year >= 2006 & X.RL..Brood.Year <= 2023 ~ "2006-2023"
#     )
#   ) %>%
#   group_by(Brood.Period, Release.Site.Name.2, `Level 2`) %>%
#   summarise(
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(Brood.Period, Release.Site.Name.2) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     `Level 2` = factor(
#       `Level 2`,
#       levels = c(
#         "HIGH SEAS",
#         "SCAK",
#         "NSEAK",
#         "SSEAK",
#         "NBC",
#         "CBC",
#         "NWVI",
#         "SWVI",
#         "SGEO",
#         "PUSO",
#         "COL RIVER OR",
#         "Unknown"
#       )
#     )
#   )
# 
# release_labels <- mapreleasedata %>%
#   filter(!is.na(Brood.Year)) %>%
#   group_by(Site.Name.2, Brood.Year) %>%
#   summarise(
#     N_released = sum(Total.Released.With.CWT, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     Brood.Period = case_when(
#       Brood.Year >= 1972 & Brood.Year <= 1988 ~ "1972-1988",
#       Brood.Year >= 1989 & Brood.Year <= 2005 ~ "1989-2005",
#       Brood.Year >= 2006 & Brood.Year <= 2023 ~ "2006-2023"
#     )
#   ) %>%
#   group_by(Brood.Period, Site.Name.2) %>%
#   summarise(
#     N_released = sum(N_released, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     label = paste0("N = ", format(N_released, big.mark = ","))
#   )
# 
# prop_data_COBC_lab <- prop_data_COBC %>%
#   left_join(
#     release_labels,
#     by = c(
#       "Release.Site.Name.2" = "Site.Name.2",
#       "Brood.Period" = "Brood.Period"
#     )
#   )
# 
# label_data <- prop_data_COBC_lab %>%
#   distinct(Brood.Period, Release.Site.Name.2, N_released, label)
# 
# ggplot(
#   prop_data_COBC_lab,
#   aes(
#     x = Release.Site.Name.2,
#     y = proportion,
#     fill = `Level 2`
#   )
# ) +
#   geom_col(position = "fill") +
#   geom_text(
#     data = label_data,
#     aes(
#       x = Release.Site.Name.2,
#       y = 1.02,
#       label = label
#     ),
#     inherit.aes = FALSE,
#     hjust = 0,
#     size = 3.6
#   ) +
#   scale_y_continuous(
#     labels = percent_format(),
#     limits = c(0, 1.12),
#     expand = expansion(mult = c(0, 0))
#   ) +
#   scale_fill_manual(values = custom_cols, drop = FALSE) +
#   coord_flip() +
#   facet_wrap(~ Brood.Period, ncol = 1, scales = "free_y") +
#   labs(
#     x = "Release Site Name",
#     y = "Percentage of Estimated Catch",
#     fill = "Recovery Region",
#     title = "Percentage of estimated catch by recovery region for COBC release sites"
#   ) +
#   theme_minimal(base_size = 13)
# 
# 
# 
# 
# 
# 
# kitimat_unknown <- cwtdata_condensed[
#   cwtdata_condensed$Release.Site.Name.2 == "Kitimat R" &
#     cwtdata_condensed$`Level 2` == "Unknown",
# ]
# 
# 
# 
# bellacoola_unknown <- cwtdata_condensed[
#   cwtdata_condensed$Release.Site.Name.2 == "Bella Coola R" &
#     cwtdata_condensed$`Level 2` == "Unknown",
# ]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# custom_cols <- c(
#   "HIGH SEAS"    = "#E64A19",
#   "SCAK"         = "#FF8A65",
#   "NSEAK"        = "#FFB74D",
#   "SSEAK"        = "#FFD54F",
#   "NBC"          = "#66BB6A",
#   "CBC"          = "#2E7D32",
#   "NWVI"         = "#1565C0",
#   "SWVI"         = "#64B5F6",
#   "SGEO"         = "#BA68C8",
#   "PUSO"         = "#7B1FA2",
#   "COL RIVER OR" = "#F06292",
#   "Unknown"      = "grey70"
# )
# 
# prop_data_QCI <- cwtdata_condensed %>%
#   filter(
#     X.RL..Stock.PSC.Region.Code == "QCI",
#     !is.na(`Level 2`),
#     !is.na(X.RL..Brood.Year)
#   ) %>%
#   mutate(
#     Brood.Period = case_when(
#       X.RL..Brood.Year >= 1972 & X.RL..Brood.Year <= 1988 ~ "1972-1988",
#       X.RL..Brood.Year >= 1989 & X.RL..Brood.Year <= 2005 ~ "1989-2005",
#       X.RL..Brood.Year >= 2006 & X.RL..Brood.Year <= 2023 ~ "2006-2023"
#     )
#   ) %>%
#   group_by(Brood.Period, Release.Site.Name.2, `Level 2`) %>%
#   summarise(
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(Brood.Period, Release.Site.Name.2) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     `Level 2` = factor(
#       `Level 2`,
#       levels = c(
#         "HIGH SEAS",
#         "SCAK",
#         "NSEAK",
#         "SSEAK",
#         "NBC",
#         "CBC",
#         "NWVI",
#         "SWVI",
#         "SGEO",
#         "PUSO",
#         "COL RIVER OR",
#         "Unknown"
#       )
#     )
#   )
# 
# release_labels <- mapreleasedata %>%
#   filter(!is.na(Brood.Year)) %>%
#   group_by(Site.Name.2, Brood.Year) %>%
#   summarise(
#     N_released = sum(Total.Released.With.CWT, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     Brood.Period = case_when(
#       Brood.Year >= 1972 & Brood.Year <= 1988 ~ "1972-1988",
#       Brood.Year >= 1989 & Brood.Year <= 2005 ~ "1989-2005",
#       Brood.Year >= 2006 & Brood.Year <= 2023 ~ "2006-2023"
#     )
#   ) %>%
#   group_by(Brood.Period, Site.Name.2) %>%
#   summarise(
#     N_released = sum(N_released, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     label = paste0("N = ", format(N_released, big.mark = ","))
#   )
# 
# prop_data_QCI_lab <- prop_data_QCI %>%
#   left_join(
#     release_labels,
#     by = c(
#       "Release.Site.Name.2" = "Site.Name.2",
#       "Brood.Period" = "Brood.Period"
#     )
#   )
# 
# label_data <- prop_data_QCI_lab %>%
#   distinct(Brood.Period, Release.Site.Name.2, N_released, label)
# 
# ggplot(
#   prop_data_QCI_lab,
#   aes(
#     x = Release.Site.Name.2,
#     y = proportion,
#     fill = `Level 2`
#   )
# ) +
#   geom_col(position = "fill") +
#   geom_text(
#     data = label_data,
#     aes(
#       x = Release.Site.Name.2,
#       y = 1.02,
#       label = label
#     ),
#     inherit.aes = FALSE,
#     hjust = 0,
#     size = 3.6
#   ) +
#   scale_y_continuous(
#     labels = percent_format(),
#     limits = c(0, 1.12),
#     expand = expansion(mult = c(0, 0))
#   ) +
#   scale_fill_manual(values = custom_cols, drop = FALSE) +
#   coord_flip() +
#   facet_wrap(~ Brood.Period, ncol = 1, scales = "free_y") +
#   labs(
#     x = "Release Site Name",
#     y = "Percentage of Estimated Catch",
#     fill = "Recovery Region",
#     title = "Percentage of estimated catch by recovery region for QCI release sites"
#   ) +
#   theme_minimal(base_size = 13)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# 
# 
# 
# 
# 
# 
# 
# 
# ##### FIGURES THAT SHOW NUMBER OF RELEASES PER RELEASE REGION OVER ALL YEARS #####
# 
# 
# 
# # Desired region order
# region_order <- c("AYK","WEAK","CNAK","SEAK","TRAN","QCI","NASK",
#                   "COBC","JNST","GST","WCVI","FRTH")
# 
# # Remove rows with empty region code
# releasedata_clean <- releasedata[releasedata$Stock.PSC.Region.Code != "", ]
# 
# # Enforce region order
# releasedata_clean$Stock.PSC.Region.Code <- factor(
#   releasedata_clean$Stock.PSC.Region.Code,
#   levels = region_order
# )
# 
# # Columns to sum
# cols_to_sum <- c("Num.WithCWT.Adclip",
#                  "Num.WithCWT.NoAdclip",
#                  "Num.WithCWT.UnknAD",
#                  "Num.NoCWT.Adclip",
#                  "Num.NoCWT.NoAdclip",
#                  "Num.NoCWT.UnknAD")
# 
# # Aggregate totals by region
# totals_df <- aggregate(releasedata_clean[, cols_to_sum],
#                        by = list(Stock.PSC.Region.Code = releasedata_clean$Stock.PSC.Region.Code),
#                        FUN = sum,
#                        na.rm = TRUE)
# 
# # Reorder rows to match region_order
# totals_df <- totals_df[match(region_order, totals_df$Stock.PSC.Region.Code), ]
# 
# # Turn off scientific notation
# options(scipen = 999)
# 
# # Increase left margin
# par(mar = c(6, 7, 4, 2) + 0.1)
# 
# totals <- totals_df$Num.WithCWT.Adclip
# names(totals) <- totals_df$Stock.PSC.Region.Code
# head(totals)
# 
# 
# # Plot
# barplot(totals,
#         las = 2,
#         cex.names = 0.9,
#         main = "Total CWT releases by stock region",
#         xlab = "Stock PSC Region Code",
#         ylab = "Total number with CWT + Adclip",
#         mgp = c(4.9, 1, 0))
# 
# 
# 
# 
# 
# 
# 
# 
# # Create a Year x Region table of summed totals
# tab <- with(releasedata,
#             tapply(Num.WithCWT.Adclip,
#                    list(Release.Year, Stock.PSC.Region.Code),
#                    sum, na.rm = TRUE))
# 
# # Replace NAs with 0 (optional but usually helpful for plotting)
# tab[is.na(tab)] <- 0
# 
# # Make sure years are in increasing order
# tab <- tab[order(as.numeric(rownames(tab))), , drop = FALSE]
# 
# # Turn off scientific notation
# options(scipen = 999)
# 
# # Give room for axis labels
# par(mar = c(8, 6, 4, 6) + 0.1)
# 
# # Heatmap-like plot (image)
# image(
#   x = 1:nrow(tab),
#   y = 1:ncol(tab),
#   z = tab,
#   xlab = "Stock PSC Region Code",
#   ylab = "Release Year",
#   axes = FALSE
# )
# 
# axis(2, at = 1:ncol(tab), labels = colnames(tab), las = 2, cex.axis = 0.8)
# axis(1, at = 1:nrow(tab), labels = rownames(tab), las = 1, cex.axis = 0.9)
# box()
# 
# title("Sum of Num.WithCWT.Adclip by Release Year and Stock Region")
# 
# 
# 
# 
# class(releasedata$Stock.PSC.Region.Code)
# releasedata$Stock.PSC.Region.Code <- as.factor(releasedata$Stock.PSC.Region.Code)
# levels(releasedata$Stock.PSC.Region.Code)
# 
# df <- aggregate(Num.WithCWT.Adclip ~ Release.Year + Stock.PSC.Region.Code,
#                 data = releasedata, sum, na.rm = TRUE)
# 
# ggplot(df, aes(x = Stock.PSC.Region.Code, y = Release.Year, fill = Num.WithCWT.Adclip)) +
#   geom_tile() +
#   scale_y_continuous(breaks = sort(unique(df$Release.Year))) +
#   labs(title = "Sum of Num.WithCWT.Adclip by Release Year and Stock Region",
#        x = "Stock PSC Region Code",
#        y = "Release Year",
#        fill = "Total") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Desired region order
# region_order <- c("AYK","WEAK","CNAK","SEAK","QCI","NASK",
#                   "COBC","JNST","GST","WCVI","TRAN","FRTH")
# 
# # Remove rows with empty region code
# releasedata_clean <- releasedata[releasedata$Stock.PSC.Region.Code != "", ]
# 
# # Convert to factor to enforce order
# releasedata_clean$Stock.PSC.Region.Code <- factor(
#   releasedata_clean$Stock.PSC.Region.Code,
#   levels = region_order
# )
# 
# # Create Year x Region table
# tab <- with(releasedata_clean,
#             tapply(Num.WithCWT.Adclip,
#                    list(Release.Year, Stock.PSC.Region.Code),
#                    sum, na.rm = TRUE))
# 
# # Replace NA with 0
# tab[is.na(tab)] <- 0
# 
# # Ensure years ordered
# tab <- tab[order(as.numeric(rownames(tab))), region_order, drop = FALSE]
# 
# # Plot
# options(scipen = 999)
# par(mar = c(8, 6, 4, 2) + 0.1)
# 
# image(0:ncol(tab),
#       0:nrow(tab),
#       t(tab)[, nrow(tab):1],
#       xlab = "Stock PSC Region Code",
#       ylab = "Release Year",
#       axes = FALSE)
# 
# axis(1, at = 0.5:(ncol(tab)-0.5), labels = region_order, las = 2)
# axis(2, at = 0.5:(nrow(tab)-0.5), labels = rev(rownames(tab)), las = 1)
# 
# title("Total Num.WithCWT.Adclip by Release Year and Stock Region")
# box()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Desired region order
# region_order <- c("AYK","WEAK","CNAK","SEAK","TRAN","QCI","NASK",
#                   "COBC","JNST","GST","WCVI","FRTH")
# 
# # Clean and summarize data
# df <- releasedata %>%
#   filter(Stock.PSC.Region.Code != "") %>%
#   group_by(Release.Year, Stock.PSC.Region.Code) %>%
#   summarise(Total = sum(Num.WithCWT.Adclip, na.rm = TRUE), .groups = "drop")
# 
# # Enforce region order
# df$Stock.PSC.Region.Code <- factor(df$Stock.PSC.Region.Code, levels = region_order)
# 
# # Plot heatmap
# ggplot(df, aes(x = Stock.PSC.Region.Code, y = Release.Year, fill = Total)) +
#   geom_tile(color = "white") +
#   scale_fill_viridis_c(option = "C") +
#   labs(title = "Total Number of coho with CWT + Adclip released by year and stock",
#        x = "Stock PSC Region Code",
#        y = "Release Year",
#        fill = "Total Releases") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ##### FIGURES THAT SHOW NUMBER OF RECOVERIES PER RELEASE SITE OVER ALL YEARS #####
# 
# 
# heatmap_data_year <- cwtdata_condensed %>%
#   mutate(
#     X.RC..Recovery.Year = as.integer(as.character(X.RC..Recovery.Year)),
#     `Level 2` = as.character(`Level 2`)
#   ) %>%
#   group_by(`Level 2`, X.RC..Recovery.Year) %>%
#   summarise(
#     Estimated_Number = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     `Level 2` = factor(
#       `Level 2`,
#       levels = rev(c(
#         "HIGH SEAS",
#         "SCAK",
#         "NSEAK",
#         "SSEAK",
#         "NBC",
#         "CBC",
#         "NWVI",
#         "SWVI",
#         "SGEO",
#         "PUSO",
#         "WA",
#         "COL RIVER WA",
#         "COL RIVER OR",
#         NA
#       ))
#     )
#   )
# 
# ggplot(heatmap_data_year,
#        aes(x = X.RC..Recovery.Year,
#            y = `Level 2`,
#            fill = Estimated_Number)) +
#   geom_tile(color = "white") +
#   scale_fill_viridis_c(name = "Estimated Number", na.value = "grey90") +
#   labs(
#     x = "Recovery Year",
#     y = "Catch Region",
#     title = "Heatmap of Estimated Number by Catch Region and Recovery Year"
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid = element_blank(),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )
# 
# 
# 
# 
# ##############################
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# ############################################################################################
# 
# 
# 
# 
# 
# 
# 
# ############################################################################################
# ############################################################################################
# 
# ############## PROPORTION OF EACH STOCK CAUGHT BY FISHERY X COUNTRY TABLES #################
# 
# ############################################################################################
# ############################################################################################
# 
# 
# 
# 
# 
# # ---- Prop data (by Fishery Notes) ----
# prop_data <- cwtdata_condensed %>%
#   filter(!is.na(PSC_FISHERY_NOTES)) %>%
#   group_by(X.RL..Stock.PSC.Region.Code, PSC_FISHERY_NOTES) %>%
#   summarise(
#     total_observed  = sum(X.RC..Observed.Number,  na.rm = TRUE),
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(X.RL..Stock.PSC.Region.Code) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   filter(X.RL..Stock.PSC.Region.Code != "WEAK") %>%
#   mutate(
#     # ---- FORCE AYK TO 100% ----
#     proportion = dplyr::if_else(
#       X.RL..Stock.PSC.Region.Code == "AYK" &
#         PSC_FISHERY_NOTES == "Escapement",  
#       1,
#       dplyr::if_else(
#         X.RL..Stock.PSC.Region.Code == "AYK",
#         0,
#         proportion
#       )
#     ),
#     
#     X.RL..Stock.PSC.Region.Code = factor(
#       X.RL..Stock.PSC.Region.Code,
#       levels = c("AYK", "CNAK", "SEAK", "QCI", "NASK", "COBC", "JNST")
#     ),
#     PSC_FISHERY_NOTES = factor(PSC_FISHERY_NOTES)
#   )
# 
# base_cols <- hue_pal()(length(levels(prop_data$PSC_FISHERY_NOTES)))
# names(base_cols) <- levels(prop_data$PSC_FISHERY_NOTES)
# 
# if ("Unknown" %in% names(base_cols)) base_cols["Unknown"] <- "grey70"
# 
# 
# 
# ggplot(
#   prop_data,
#   aes(
#     x = X.RL..Stock.PSC.Region.Code,
#     y = proportion,
#     fill = PSC_FISHERY_NOTES
#   )
# ) +
#   geom_col(position = "fill") +
#   scale_y_continuous(labels = percent_format()) +
#   scale_fill_manual(values = base_cols, drop = FALSE) +
#   labs(
#     x = "Stock Region Code",
#     y = "Percentage of Estimated Catch",
#     fill = "Fishery Type",
#     title = "Percentage of each stock caught by fishery type"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# 
# ggplot(
#   prop_data,
#   aes(
#     x = X.RL..Stock.PSC.Region.Code,
#     y = proportion,
#     fill = PSC_FISHERY_NOTES
#   )
# ) +
#   geom_col(position = "fill") +
#   scale_y_continuous(labels = percent_format()) +
#   scale_fill_manual(values = base_cols, drop = FALSE) +
#   labs(
#     x = "Stock Region Code",
#     y = "Percentage of Estimated Catch",
#     fill = "Fishery Type",
#     title = "Percentage of each stock caught by fishery type"
#   ) +
#   coord_flip() +
#   theme_minimal()+
#   theme_minimal(base_size = 20)
# 
# 
# ################
# 
# 
# 
# 
# 
# head(cwtdata_condensed)
# 
# 
# 
# class(cwtdata_condensed$X.RL..Release.Site.Name)
# cwtdata_condensed$X.RL..Release.Site.Name <- as.factor(cwtdata_condensed$X.RL..Release.Site.Name)
# levels(cwtdata_condensed$X.RL..Release.Site.Name)
# 
# cwtdata_CA <- cwtdata_condensed[cwtdata_condensed$X.RL..Country.Code == "CAN", ]
# 
# cwtdata_CA$X.RL..Release.Site.Name <- droplevels(cwtdata_CA$X.RL..Release.Site.Name)
# levels(cwtdata_CA$X.RL..Release.Site.Name)
# levels(cwtdata_CA$`Level 0`)
# cwtdata_CA$PSC_FISHERY_NOTES<-as.factor(cwtdata_CA$PSC_FISHERY_NOTES)
# levels(cwtdata_CA$PSC_FISHERY_NOTES)
# head(cwtdata_CA)
# 
# 
# 
# 
# cwtdata_zolzap <- cwtdata_condensed[cwtdata_condensed$Release.Site.Name.2 == "Zolzap Cr", ]
# 
# 
# 
# 
# 
# 
# ##### ZOLZAP
# 
# 
# 
# 
# # ---- Prop data (proportions sum to 1 within each Brood Year) ----
# prop_data_Zolzap <- cwtdata_CA %>%
#   filter(
#     X.RL..Release.Site.Name == "Zolzap Cr",
#     !is.na(PSC_FISHERY_NOTES),
#     !is.na(`Level 0`),
#     !is.na(X.RC..Recovery.Year)
#   ) %>%
#   group_by(X.RC..Recovery.Year, `Level 0`, PSC_FISHERY_NOTES) %>%
#   summarise(
#     total_observed  = sum(X.RC..Observed.Number,  na.rm = TRUE),
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(X.RC..Recovery.Year) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     PSC_FISHERY_NOTES = factor(PSC_FISHERY_NOTES),
#     X.RC..Recovery.Year = factor(X.RC..Recovery.Year)
#   )
# 
# 
# prop_data_Zolzap
# 
# 
# 
# 
# year_totals <- prop_data_Zolzap %>%
#   group_by(X.RC..Recovery.Year) %>%
#   summarise(
#     Total_Observed = sum(total_observed, na.rm = TRUE),
#     Total_Estimated = sum(total_estimated, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# 
# 
# prop_data_Zolzap_wide <- prop_data_Zolzap %>%
#   mutate(
#     col_name = paste(`Level 0`, PSC_FISHERY_NOTES, sep = "_"),
#     col_name = gsub(" ", "_", col_name),
#     percentage = proportion * 100
#   ) %>%
#   select(X.RC..Recovery.Year, col_name, percentage) %>%
#   pivot_wider(
#     names_from = col_name,
#     values_from = percentage
#   ) %>%
#   left_join(year_totals, by = "X.RC..Recovery.Year") %>%
#   relocate(Total_Observed, Total_Estimated, .after = X.RC..Recovery.Year)
# 
# prop_data_Zolzap_wide
# 
# 
# 
# 
# 
# 
# 
# ######## TOBOGGAN
# 
# 
# 
# # ---- Prop data (proportions sum to 1 within each Recovery Year) ----
# prop_data_Toboggan <- cwtdata_CA %>%
#   filter(
#     X.RL..Release.Site.Name == "Toboggan Cr",
#     !is.na(PSC_FISHERY_NOTES),
#     !is.na(`Level 0`),
#     !is.na(X.RC..Recovery.Year)
#   ) %>%
#   group_by(X.RC..Recovery.Year, `Level 0`, PSC_FISHERY_NOTES) %>%
#   summarise(
#     total_observed  = sum(X.RC..Observed.Number,  na.rm = TRUE),
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(X.RC..Recovery.Year) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     PSC_FISHERY_NOTES = factor(PSC_FISHERY_NOTES),
#     X.RC..Recovery.Year = factor(X.RC..Recovery.Year)
#   )
# 
# prop_data_Toboggan
# 
# year_totals_Toboggan <- prop_data_Toboggan %>%
#   group_by(X.RC..Recovery.Year) %>%
#   summarise(
#     Total_Observed = sum(total_observed, na.rm = TRUE),
#     Total_Estimated = sum(total_estimated, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# prop_data_Toboggan_wide <- prop_data_Toboggan %>%
#   mutate(
#     col_name = paste(`Level 0`, PSC_FISHERY_NOTES, sep = "_"),
#     col_name = gsub(" ", "_", col_name),
#     percentage = proportion * 100
#   ) %>%
#   select(X.RC..Recovery.Year, col_name, percentage) %>%
#   pivot_wider(
#     names_from = col_name,
#     values_from = percentage
#   ) %>%
#   left_join(year_totals_Toboggan, by = "X.RC..Recovery.Year") %>%
#   relocate(Total_Observed, Total_Estimated, .after = X.RC..Recovery.Year)
# 
# prop_data_Toboggan_wide
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###### ZYMACORD
# 
# 
# 
# 
# 
# 
# # ---- Prop data (proportions sum to 1 within each Recovery Year) ----
# prop_data_Zymacord <- cwtdata_CA %>%
#   filter(
#     X.RL..Release.Site.Name == "Zymacord R",
#     !is.na(PSC_FISHERY_NOTES),
#     !is.na(`Level 0`),
#     !is.na(X.RC..Recovery.Year)
#   ) %>%
#   group_by(X.RC..Recovery.Year, `Level 0`, PSC_FISHERY_NOTES) %>%
#   summarise(
#     total_observed  = sum(X.RC..Observed.Number,  na.rm = TRUE),
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(X.RC..Recovery.Year) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     PSC_FISHERY_NOTES = factor(PSC_FISHERY_NOTES),
#     X.RC..Recovery.Year = factor(X.RC..Recovery.Year)
#   )
# 
# prop_data_Zymacord
# 
# 
# year_totals_Zymacord <- prop_data_Zymacord %>%
#   group_by(X.RC..Recovery.Year) %>%
#   summarise(
#     Total_Observed = sum(total_observed, na.rm = TRUE),
#     Total_Estimated = sum(total_estimated, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# 
# prop_data_Zymacord_wide <- prop_data_Zymacord %>%
#   mutate(
#     col_name = paste(`Level 0`, PSC_FISHERY_NOTES, sep = "_"),
#     col_name = gsub(" ", "_", col_name),
#     percentage = proportion * 100
#   ) %>%
#   select(X.RC..Recovery.Year, col_name, percentage) %>%
#   pivot_wider(
#     names_from = col_name,
#     values_from = percentage
#   ) %>%
#   left_join(year_totals_Zymacord, by = "X.RC..Recovery.Year") %>%
#   relocate(Total_Observed, Total_Estimated, .after = X.RC..Recovery.Year)
# 
# prop_data_Zymacord_wide
# 
# 
# 
# 
# zymacord_2024 <- cwtdata_condensed %>%
#   filter(
#     Release.Site.Name.2 == "Zymacord R",
#     X.RC..Recovery.Year == "2024"
#   )
# 
# zymacord_2023 <- cwtdata_condensed %>%
#   filter(
#     Release.Site.Name.2 == "Zymacord R",
#     X.RC..Recovery.Year == "2023"
#   )
# 
# ######################
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###### BELLA COOLA R
# 
# 
# 
# 
# 
# 
# # ---- Prop data (proportions sum to 1 within each Recovery Year) ----
# prop_data_bellacoola <- cwtdata_CA %>%
#   filter(
#     Release.Site.Name.2 == "Bella Coola R",
#     !is.na(PSC_FISHERY_NOTES),
#     !is.na(`Level 0`),
#     !is.na(X.RC..Recovery.Year)
#   ) %>%
#   group_by(X.RC..Recovery.Year, `Level 0`, PSC_FISHERY_NOTES) %>%
#   summarise(
#     total_observed  = sum(X.RC..Observed.Number,  na.rm = TRUE),
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(X.RC..Recovery.Year) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     PSC_FISHERY_NOTES = factor(PSC_FISHERY_NOTES),
#     X.RC..Recovery.Year = factor(X.RC..Recovery.Year)
#   )
# 
# prop_data_bellacoola
# 
# 
# year_totals_bellacoola <- prop_data_bellacoola %>%
#   group_by(X.RC..Recovery.Year) %>%
#   summarise(
#     Total_Observed = sum(total_observed, na.rm = TRUE),
#     Total_Estimated = sum(total_estimated, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# 
# prop_data_bellacoola_wide <- prop_data_bellacoola %>%
#   mutate(
#     col_name = paste(`Level 0`, PSC_FISHERY_NOTES, sep = "_"),
#     col_name = gsub(" ", "_", col_name),
#     percentage = proportion * 100
#   ) %>%
#   select(X.RC..Recovery.Year, col_name, percentage) %>%
#   pivot_wider(
#     names_from = col_name,
#     values_from = percentage
#   ) %>%
#   left_join(year_totals_bellacoola, by = "X.RC..Recovery.Year") %>%
#   relocate(Total_Observed, Total_Estimated, .after = X.RC..Recovery.Year)
# 
# prop_data_bellacoola_wide
# 
# 
# 
# bella_summary <- releasedata %>%
#   filter(`Site.Name.2` == "Bella Coola R") %>%
#   group_by(Brood.Year) %>%
#   summarise(
#     total_Num.WithCWT.Adclip = sum(Num.WithCWT.Adclip, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# bella_summary
# 
# 
# 
# 
# ######################
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###### JOHNSTON EST
# 
# 
# 
# 
# 
# 
# # ---- Prop data (proportions sum to 1 within each Recovery Year) ----
# prop_data_johnstonest <- cwtdata_CA %>%
#   filter(
#     Release.Site.Name.2 == "Johnston Est",
#     !is.na(PSC_FISHERY_NOTES),
#     !is.na(`Level 0`),
#     !is.na(X.RC..Recovery.Year)
#   ) %>%
#   group_by(X.RC..Recovery.Year, `Level 0`, PSC_FISHERY_NOTES) %>%
#   summarise(
#     total_observed  = sum(X.RC..Observed.Number,  na.rm = TRUE),
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(X.RC..Recovery.Year) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     PSC_FISHERY_NOTES = factor(PSC_FISHERY_NOTES),
#     X.RC..Recovery.Year = factor(X.RC..Recovery.Year)
#   )
# 
# prop_data_johnstonest
# 
# 
# year_totals_johnstonest <- prop_data_johnstonest %>%
#   group_by(X.RC..Recovery.Year) %>%
#   summarise(
#     Total_Observed = sum(total_observed, na.rm = TRUE),
#     Total_Estimated = sum(total_estimated, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# 
# prop_data_johnstonest_wide <- prop_data_johnstonest %>%
#   mutate(
#     col_name = paste(`Level 0`, PSC_FISHERY_NOTES, sep = "_"),
#     col_name = gsub(" ", "_", col_name),
#     percentage = proportion * 100
#   ) %>%
#   select(X.RC..Recovery.Year, col_name, percentage) %>%
#   pivot_wider(
#     names_from = col_name,
#     values_from = percentage
#   ) %>%
#   left_join(year_totals_Zymacord, by = "X.RC..Recovery.Year") %>%
#   relocate(Total_Observed, Total_Estimated, .after = X.RC..Recovery.Year)
# 
# prop_data_johnstonest_wide
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###### JOHNSTON CR
# 
# 
# 
# 
# 
# 
# # ---- Prop data (proportions sum to 1 within each Recovery Year) ----
# prop_data_johnstoncr <- cwtdata_CA %>%
#   filter(
#     Release.Site.Name.2 == "Johnston Cr",
#     !is.na(PSC_FISHERY_NOTES),
#     !is.na(`Level 0`),
#     !is.na(X.RC..Recovery.Year)
#   ) %>%
#   group_by(X.RC..Recovery.Year, `Level 0`, PSC_FISHERY_NOTES) %>%
#   summarise(
#     total_observed  = sum(X.RC..Observed.Number,  na.rm = TRUE),
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(X.RC..Recovery.Year) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     PSC_FISHERY_NOTES = factor(PSC_FISHERY_NOTES),
#     X.RC..Recovery.Year = factor(X.RC..Recovery.Year)
#   )
# 
# prop_data_johnstoncr
# 
# 
# year_totals_johnstoncr <- prop_data_johnstoncr %>%
#   group_by(X.RC..Recovery.Year) %>%
#   summarise(
#     Total_Observed = sum(total_observed, na.rm = TRUE),
#     Total_Estimated = sum(total_estimated, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# 
# prop_data_johnstoncr_wide <- prop_data_johnstoncr %>%
#   mutate(
#     col_name = paste(`Level 0`, PSC_FISHERY_NOTES, sep = "_"),
#     col_name = gsub(" ", "_", col_name),
#     percentage = proportion * 100
#   ) %>%
#   select(X.RC..Recovery.Year, col_name, percentage) %>%
#   pivot_wider(
#     names_from = col_name,
#     values_from = percentage
#   ) %>%
#   left_join(year_totals_Zymacord, by = "X.RC..Recovery.Year") %>%
#   relocate(Total_Observed, Total_Estimated, .after = X.RC..Recovery.Year)
# 
# prop_data_johnstoncr_wide
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###### WEST ARM CR (DRAKE CR)
# 
# 
# 
# 
# 
# 
# # ---- Prop data (proportions sum to 1 within each Recovery Year) ----
# prop_data_westarmcr <- cwtdata_CA %>%
#   filter(
#     Release.Site.Name.2 == "West Arm Cr (Drake Cr)",
#     !is.na(PSC_FISHERY_NOTES),
#     !is.na(`Level 0`),
#     !is.na(X.RC..Recovery.Year)
#   ) %>%
#   group_by(X.RC..Recovery.Year, `Level 0`, PSC_FISHERY_NOTES) %>%
#   summarise(
#     total_observed  = sum(X.RC..Observed.Number,  na.rm = TRUE),
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(X.RC..Recovery.Year) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     PSC_FISHERY_NOTES = factor(PSC_FISHERY_NOTES),
#     X.RC..Recovery.Year = factor(X.RC..Recovery.Year)
#   )
# 
# prop_data_westarmcr
# 
# 
# year_totals_westarmcr <- prop_data_westarmcr %>%
#   group_by(X.RC..Recovery.Year) %>%
#   summarise(
#     Total_Observed = sum(total_observed, na.rm = TRUE),
#     Total_Estimated = sum(total_estimated, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# 
# prop_data_westarmcr_wide <- prop_data_westarmcr %>%
#   mutate(
#     col_name = paste(`Level 0`, PSC_FISHERY_NOTES, sep = "_"),
#     col_name = gsub(" ", "_", col_name),
#     percentage = proportion * 100
#   ) %>%
#   select(X.RC..Recovery.Year, col_name, percentage) %>%
#   pivot_wider(
#     names_from = col_name,
#     values_from = percentage
#   ) %>%
#   left_join(year_totals_Zymacord, by = "X.RC..Recovery.Year") %>%
#   relocate(Total_Observed, Total_Estimated, .after = X.RC..Recovery.Year)
# 
# prop_data_westarmcr_wide
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# pca_data <- cwtdata_CA %>%
#   filter(!is.na(`Level 2`)) %>%
#   group_by(Release.Site.Name.2, `Level 2`) %>%
#   summarise(
#     total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   group_by(Release.Site.Name.2) %>%
#   mutate(
#     proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   select(Release.Site.Name.2, `Level 2`, proportion) %>%
#   pivot_wider(
#     names_from = `Level 2`,
#     values_from = proportion,
#     values_fill = 0
#   )
# 
# 
# 
# 
# # Save row names (site names)
# row_names <- pca_data$Release.Site.Name.2
# 
# # Remove site column
# pca_matrix <- as.matrix(pca_data[,-1])
# 
# rownames(pca_matrix) <- row_names
# 
# # Remove columns with zero variance
# pca_matrix_clean <- pca_matrix[, apply(pca_matrix, 2, var) > 0]
# 
# # Run PCA
# pca_result <- prcomp(pca_matrix_clean, scale. = TRUE)
# 
# 
# 
# 
# plot(pca_result$x[,1:2],
#      pch = 19,
#      xlab = "PC1",
#      ylab = "PC2",
#      main = "PCA of Release Sites by Recovery Region")
# 
# text(pca_result$x[,1:2],
#      labels = rownames(pca_matrix),
#      pos = 3,
#      cex = 0.7)
# 
# 
# 
# 
# 
# 
# scores <- as.data.frame(pca_result$x)
# scores$Site <- rownames(scores)
# 
# ggplot(scores, aes(x = PC1, y = PC2, label = Site)) +
#   geom_point(size = 3) +
#   geom_text(nudge_y = 0.02, size = 3) +
#   theme_minimal() +
#   labs(
#     title = "PCA of Release Sites by Recovery Distribution",
#     x = "PC1",
#     y = "PC2"
#   )
# 
# 
# site_stock <- cwtdata_CA %>%
#   distinct(Release.Site.Name.2, X.RL..Stock.PSC.Region.Code)
# 
# site_stock %>%
#   count(Release.Site.Name.2) %>%
#   filter(n > 1)
# 
# 
# 
# scores <- as.data.frame(pca_result$x)
# scores$Release.Site.Name.2 <- rownames(scores)
# 
# scores <- scores %>%
#   left_join(site_stock, by = "Release.Site.Name.2")
# 
# 
# 
# 
# ggplot(
#   scores,
#   aes(
#     x = PC1,
#     y = PC2,
#     color = X.RL..Stock.PSC.Region.Code,
#     label = Release.Site.Name.2
#   )
# ) +
#   geom_point(size = 3) +
#   geom_text(vjust = -0.5, size = 3) +
#   theme_minimal() +
#   labs(
#     title = "PCA of Release Sites by Recovery Distribution",
#     x = "PC1",
#     y = "PC2",
#     color = "Stock Region"
#   )
# 
# 
# ggplot(
#   scores,
#   aes(
#     x = PC1,
#     y = PC2,
#     color = X.RL..Stock.PSC.Region.Code
#   )
# ) +
#   geom_point(size = 3) +
#   stat_ellipse(level = 0.95) +
#   theme_minimal(base_size = 20) +
#   labs(
#     title = "PCA of Release Sites by Recovery Distribution - all years",
#     x = "PC1",
#     y = "PC2",
#     color = "Stock Region"
#   )
# 
# 
# 
# 
# 
# 
# 
# ggplot(
#   scores,
#   aes(
#     x = PC1,
#     y = PC2,
#     color = X.RL..Stock.PSC.Region.Code
#   )
# ) +
#   geom_point(size = 3) +
#   stat_ellipse(
#     aes(group = X.RL..Stock.PSC.Region.Code),
#     level = 0.95
#   ) +
#   geom_text_repel(
#     aes(label = Release.Site.Name.2),
#     size = 3,
#     max.overlaps = Inf
#   ) +
#   theme_minimal() +
#   labs(
#     title = "PCA of Release Sites by Recovery Distribution - all years",
#     x = "PC1",
#     y = "PC2",
#     color = "Stock Region"
#   )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###############
# 
# 
# 
# 
# cwtdata_CA$X.RL..Brood.Year
# 
# 
# 
# 
# # ------------------------------------------------------------
# # 1. Add brood-period groups
# # ------------------------------------------------------------
# 
# cwtdata_CA_pca <- cwtdata_CA %>%
#   filter(!is.na(X.RL..Brood.Year)) %>%
#   mutate(
#     Brood.Period = case_when(
#       X.RL..Brood.Year >= 1972 & X.RL..Brood.Year <= 1988 ~ "1972-1988",
#       X.RL..Brood.Year >= 1989 & X.RL..Brood.Year <= 2005 ~ "1989-2005",
#       X.RL..Brood.Year >= 2006 & X.RL..Brood.Year <= 2023 ~ "2006-2023",
#       TRUE ~ NA_character_
#     )
#   ) %>%
#   filter(!is.na(Brood.Period))
# 
# # ------------------------------------------------------------
# # 2. Function to build PCA for one brood period
# # ------------------------------------------------------------
# 
# run_site_pca <- function(data, brood_period_label) {
#   
#   # Filter to one brood period
#   dat <- data %>%
#     filter(Brood.Period == brood_period_label, !is.na(`Level 2`))
#   
#   # Build site x recovery-region proportion table
#   pca_data <- dat %>%
#     group_by(Release.Site.Name.2, `Level 2`) %>%
#     summarise(
#       total_estimated = sum(X.RC..Estimated.Number, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     group_by(Release.Site.Name.2) %>%
#     mutate(
#       proportion = total_estimated / sum(total_estimated, na.rm = TRUE)
#     ) %>%
#     ungroup() %>%
#     select(Release.Site.Name.2, `Level 2`, proportion) %>%
#     pivot_wider(
#       names_from = `Level 2`,
#       values_from = proportion,
#       values_fill = 0
#     )
#   
#   # Save row names
#   row_names <- pca_data$Release.Site.Name.2
#   
#   # Convert to matrix
#   pca_matrix <- as.matrix(pca_data[, -1])
#   rownames(pca_matrix) <- row_names
#   
#   # Remove columns with zero variance
#   keep_cols <- apply(pca_matrix, 2, var, na.rm = TRUE) > 0
#   pca_matrix_clean <- pca_matrix[, keep_cols, drop = FALSE]
#   
#   # Run PCA
#   pca_result <- prcomp(pca_matrix_clean, scale. = TRUE)
#   
#   # Get PCA scores
#   scores <- as.data.frame(pca_result$x)
#   scores$Release.Site.Name.2 <- rownames(scores)
#   
#   # Get site -> stock lookup for this brood period
#   site_stock <- dat %>%
#     distinct(Release.Site.Name.2, X.RL..Stock.PSC.Region.Code)
#   
#   # Join stock codes onto scores
#   scores <- scores %>%
#     left_join(site_stock, by = "Release.Site.Name.2")
#   
#   # Build plot
#   p <- ggplot(
#     scores,
#     aes(
#       x = PC1,
#       y = PC2,
#       color = X.RL..Stock.PSC.Region.Code
#     )
#   ) +
#     geom_point(size = 3) +
#     stat_ellipse(
#       aes(group = X.RL..Stock.PSC.Region.Code),
#       level = 0.95
#     ) +
#     geom_text_repel(
#       aes(label = Release.Site.Name.2),
#       size = 3,
#       max.overlaps = Inf
#     ) +
#     theme_minimal() +
#     labs(
#       title = paste("PCA of Release Sites by Recovery Region Distribution:", brood_period_label),
#       x = "PC1",
#       y = "PC2",
#       color = "Stock Region"
#     )
#   
#   # Return everything
#   list(
#     brood_period = brood_period_label,
#     pca_data = pca_data,
#     pca_matrix = pca_matrix_clean,
#     pca_result = pca_result,
#     scores = scores,
#     plot = p
#   )
# }
# 
# # ------------------------------------------------------------
# # 3. Run PCA for each brood period
# # ------------------------------------------------------------
# 
# pca_1972_1988 <- run_site_pca(cwtdata_CA_pca, "1972-1988")
# pca_1989_2005 <- run_site_pca(cwtdata_CA_pca, "1989-2005")
# pca_2006_2023 <- run_site_pca(cwtdata_CA_pca, "2006-2023")
# 
# # ------------------------------------------------------------
# # 4. Show the plots
# # ------------------------------------------------------------
# 
# pca_1972_1988$plot
# pca_1989_2005$plot
# pca_2006_2023$plot
