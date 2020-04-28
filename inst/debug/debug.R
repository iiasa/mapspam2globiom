library(mapspam2globiom)
param <- spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
         iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
         solve_level = 1, model = "max_score", crs = "+init=EPSG:32633")

create_spam_folders(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi")

adm_cd <- "MI02"



