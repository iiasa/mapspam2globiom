library(mapspam2globiom)
spamc_path <- "C:/Users/dijk158/Dropbox/crop_map_ZAMBEZI/spam5min/2010/MWI"
gams_path <- "C:/MyPrograms/gams/25.1"


# Set SPAMc parameters
param <- spam_par(spam_path = spamc_path,
                  raw_path = "C:/Users/dijk158/Dropbox/crop_map_global",
                  iso3c = "MWI",
                  year = 2010,
                  res = "5min",
                  adm_level = 2,
                  solve_level = 0,
                  model = "min_entropy",
                  gams_path = gams_path)

# Show parameters
print(param)
library(gdxrrw)
igdx("C:/MyPrograms/gams/25.1")


prepare_priors_and_scores(param)
ac <- "MWI"
