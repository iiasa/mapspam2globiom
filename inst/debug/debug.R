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

var <- "biophysical_suitability"
library(magrittr)
x <- dplyr::left_join(adm_art_map, adm_map_r %>%
                        dplyr::rename(adm_code = adm2_code))

check <- x %>%
  dplyr::filter(grepl("bana", adm_code_art))
sort(unique(check$adm2_name))

i <- 0
df_x_art <- adm_art
base_xy <- base

i <- 1
df_x_art <- df_y_art
base_xy <- base
