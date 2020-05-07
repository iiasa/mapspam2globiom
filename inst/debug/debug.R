library(mapspam2globiom)
param <- spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
         iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
         solve_level = 1, model = "max_score")

create_spam_folders(param)

options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


# param <- spam_par(spam_path = "D:/temp/mapspam2globiom_mwi",
#                   iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
#                   solve_level = 1, model = "max_score")


############### CREATE GLOBIOM INPUT GDX FILES ###############
# We use the ESACCI land cover map as land cover base map.
# The user can replace this by a country specific product if available.
# If so, a new land_cover2globiom land cover class has to be procuced and loaded
# that substitues the esacci2globiom mapping.

lc_file <- file.path(param$spam_path,
                     glue::glue("processed_data/maps/cropland/{param$res}/esa_raw_{param$year}_{param$iso3c}.tif"))
lc_map <- raster::raster(lc_file)
raster::plot(lc_map)

# Load mapping of lc classes to globiom lc classes
mapping<- readr::read_csv(file.path(param$spam_path, "mappings/esacci2globiom.csv"))
