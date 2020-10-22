library(mapspam2globiom)
spamc_path <- "C:/Users/dijk158/Dropbox/mapspam2globiom_gmb/2010"
raw_path <- "C:/Users/dijk158/Dropbox/mapspam2globiom_gmb/raw_data"

# Set SPAMc parameters
param <- spam_par(spam_path = spamc_path,
                  raw_path = raw_path,
                  iso3c = "GMB",
                  year = 2010,
                  res = "30sec",
                  adm_level = 1,
                  solve_level = 0,
                  model = "max_score",
                  gams_path = "C:/gams/25.1")

# Set mapspam year_tp1
year_tp1 <- 2010

ac <- "GMB"
