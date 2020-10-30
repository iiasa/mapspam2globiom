library(mapspam2globiom)
spamc_path <- "C:/Users/dijk158/Dropbox/mapspam2globiom_test"



############### SETUP SPAMc ###############
# Set the folder where the model will be stored
# Note that R uses forward slashes even in Windows!!
spamc_path <- "C:/Users/dijk158/Dropbox/mapspam2globiom_eth"

# Set SPAMc parameters
gams_path <- "C:/MyPrograms/gams/25.1"

# Set SPAMc parameters
param <- spam_par(spam_path = spamc_path,
                  iso3c = "ETH",
                  year = 2015,
                  res = "30sec",
                  adm_level = 2,
                  solve_level = 1,
                  model = "min_entropy",
                  gams_path = gams_path)
print(param)
library(gdxrrw)
igdx("C:/MyPrograms/gams/25.1")

ac <- "ET06"



############### PREPARE SCORE ###############
#prepare_priors_and_scores(param)
split_priors(ac, param)
split_scores(ac, param)

# combine_inputs(param)
combine_inputs_adm_level(ac, param)

# run_spam(param)
run_gams_adm_level(ac, param)
