library(mapspam2globiom)
param <- spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
         iso3c = "MWI", year = 2010, res = "30sec", adm_level = 2,
         solve_level = 0, model = "max_score")

create_spam_folders(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi")

ac <- "MI02"
# ac <- "MWI"
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits



