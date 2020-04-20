param <- spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
         iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
         solve_level = 1, model = "max_score",
         crs = "+proj=longlat +datum=WGS84 +no_defs")

create_spam_folders(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi")


############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue")

# Set root
root <- here()

# R options
options(scipen=999) # Surpress scientific notation
options(digits=4)


############### LOAD DATA ###############
# Harvested area
ha <- read_csv(file.path(param$spam_path,
                         glue("processed_data/agricultural_statistics/ha_adm_{param$year}_{param$iso3c}.csv")))

# Adm list
adm_list <- read_csv(file.path(param$spam_path,
                               glue("processed_data/lists/adm_list_{param$year}_{param$iso3c}.csv")))

# Farming system shares
fs <- read_csv(file.path(param$raw_path,
                         glue("subnational_statistics/farming_system_shares_{param$year}_{param$iso3c}.csv")))

# Cropping intensity
ci <- read_csv(file.path(param$raw_path,
                         glue("subnational_statistics/cropping_intensity_{param$year}_{param$iso3c}.csv")))


############### PROCESS HARVESTED AREA ###############
# wide to long format
ha <- ha %>%
  gather(crop, ha, -adm_name, -adm_code, -adm_level)

# Set -999 and empty string values
ha <- ha %>%
  mutate(ha = if_else(ha == -999, NA_real_, ha))

# filter out crops which values are all zero or NA
crop_na_0 <- ha %>%
  group_by(crop) %>%
  filter(all(ha %in% c(0, NA))) %>%
  dplyr::select(crop) %>%
  unique

ha <- ha %>%
  filter(!crop %in% crop_na_0$crop)

# Remove lower level adm data if it would somehow not be used
ha <- ha %>%
  filter(adm_level <= param$adm_level)


########## PROCESS FARMING SYSTEM SHARES ##########
# wide to long format
fs <- fs %>%
  gather(crop, fs, -adm_name, -adm_code, -adm_level, -system)

# Set -999 and empty string values
fs <- fs %>%
  mutate(fs = if_else(fs == -999, NA_real_, fs))

# Select relevent crops using ha
fs <- fs %>%
  filter(crop %in% unique(ha$crop))


########## CROPPING INTENSITY ##########
# wide to long format
ci <- ci %>%
  gather(crop, ci, -adm_name, -adm_code, -adm_level, -system)

# Set -999 and empty string values
ci <- ci %>%
  mutate(ci = if_else(ci == -999, NA_real_, ci))

# Select relevent crops using ha
ci <- ci %>%
  filter(crop %in% unique(ha$crop))


########## CONSISTENCY CHECKS ##########


# Format checks, numerical values and code is character
# consistency within data: do they add up and are lower level totals not higher than higher level ADMs
# Check if artifical ADMs area is near zero.
# Do shares of farming systems add up.
# Is ADM system the same as the map
# Are crops the same in ci, fs and ha data files. Not possible to have ADM0 data and NA for fs and ci.

compare_adm_tot2(pa_adm, pa_fs_adm, 0)
