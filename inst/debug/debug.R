library(mapspam2globiom)
param <- spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
         iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
         solve_level = 1, model = "max_score")

create_spam_folders(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi")

ac <- "MI02"
# ac <- "MWI"
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


lc_file <- file.path(param$spam_path,
                     glue::glue("processed_data/maps/cropland/{param$res}/esa_raw_{param$year}_{param$iso3c}.tif"))
lc_map <- raster::raster(lc_file)
plot(lc_map)

# Load mapping of lc classes to globiom lc classes
mapping <- readxl::read_excel(file.path(param$spam_path,
                                      "parameters/mappings_spam.xlsx"), sheet = "esacci2globiom")

