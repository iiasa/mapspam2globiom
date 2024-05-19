# Debug

# SETUP R --------------------------------------------------------------------------------
# Install and load pacman package that automatically installs R packages if not available
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load required packages
p_load(mapspamc, countrycode, here, glue, terra, readxl, RColorBrewer, tidyverse, sf, ggpubr,
       viridis, tictoc)

# R options
options(scipen=999) # Suppress scientific notation
options(digits=4) # limit display to four digits


# SETUP MAPSPAMC -------------------------------------------------------------------------
# Set the folders where the scripts, model and database will be stored.
# Note that R uses forward slashes even in Windows!!

# Creates a model folder structure in c:/temp/ with the name 'mapspamc_mwi'.
# the user can replace mwi with the country code of the case-study country or
# choose a new name
model_path <- "c:/temp/mapspamc_aus"

# Creates a database folder with the name mapspamc_db in c:/temp
db_path <- "c:/temp"

# Sets the location of the version of GAMS that will be used to solve the model
gams_path <- "C:/MyPrograms/GAMS/40"
#gams_path <- "C:/GAMS/41"

# Set mapspamc parameters for the min_entropy_5min_adm_level_2_solve_level_0 model
param <- mapspamc_par(
  model_path = model_path,
  db_path = db_path,
  gams_path = gams_path,
  iso3c = "AUS",
  year = 2000,
  res = "5min",
  adm_level = 2,
  solve_level = 0,
  model = "min_entropy")


# Show parameters
print(param)

# Create folder structure in the mapspamc_path
create_folders(param)

# MAPSPAM2GLOBIOM PACKAGE ----------------------------------------------------------------
library(mapspam2globiom)


# Load mapping of lc classes to globiom lc classes.
# We used the esacci2globion file stored in the package.
lc_map <- esacci2globiom

# Load mapspamc to globiom crop mapping
crop_map <- crop2globiom

# Read land cover map
lc <- rast(file.path(param$model_path, glue("processed_data/maps/land_cover/land_ cover_{param$res}_{param$year}_{param$iso3c}.tif")))

# Read simu map
simu <- st_read(file.path(param$db_path, glue("simu/simu.shp")))

# Read grid map
grid <- rast(file.path(param$model_path, glue("processed_data/maps/grid/{param$res}/grid_{param$res}_{param$year}_{param$iso3c}.tif")))

# Select country polygon
simu <- simu |> filter(iso3c == param$iso3c)
plot(simu$geometry)

# Aggregate land cover map to GLOBIOM land cover classes at simu level
# Not that the area will be expressed in 1000 ha, which is common in GLOBIOM!
create_globiom_input(lc_map, crop_map, lc, simu, grid, param)

