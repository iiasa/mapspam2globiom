library(mapspam2globiom)
param <- spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
         iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
         solve_level = 0, model = "min_entropy")

create_spam_folders(param)

options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


# param <- spam_par(spam_path = "D:/temp/mapspam2globiom_mwi",
#                   iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
#                   solve_level = 1, model = "max_score")

ac <- "MWI"

lc_file <- file.path(param$spam_path,
                     glue::glue("processed_data/maps/cropland/{param$res}/esa_raw_{param$year}_{param$iso3c}.tif"))
lc_map <- raster::raster(lc_file)
plot(lc_map)

# Load mapping of lc classes to globiom lc classes
mapping <- readr::read_csv(file.path(param$spam_path, "mappings/esacci2globiom.csv"))

load_data("results", param)


load_data("crop2globiom", param)
crop2globiom <- crop2globiom %>%
  dplyr::mutate(globiom_crop = ifelse(crop == "coff", "coff", globiom_crop))
readr::write_csv(crop2globiom, file.path(param$spam_path, "mappings/crop2globiom.csv"))
