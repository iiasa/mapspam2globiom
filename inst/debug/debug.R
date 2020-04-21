param <- spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
         iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
         solve_level = 1, model = "max_score",
         crs = "+proj=longlat +datum=WGS84 +no_defs")

create_spam_folders(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi")


############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue")

# Set root
root <- here()

# R options
options(scipen=999) # Surpress scientific notation
options(digits=4)


############### LOAD DATA ###############
# Harvested area
ha <- read_csv(file.path(param$spam_path,
                         glue("processed_data/agricultural_statistics/ha_adm_{param$year}_{param$iso3c}.csv")))

# Adm list
adm_list <- read_csv(file.path(param$spam_path,
                               glue("processed_data/lists/adm_list_{param$year}_{param$iso3c}.csv")))

# Farming system shares
fs <- read_csv(file.path(param$raw_path,
                         glue("subnational_statistics/farming_system_shares_{param$year}_{param$iso3c}.csv")))

# Cropping intensity
ci <- read_csv(file.path(param$raw_path,
                         glue("subnational_statistics/cropping_intensity_{param$year}_{param$iso3c}.csv")))


############### PROCESS HARVESTED AREA ###############
# wide to long format
ha <- ha %>%
  gather(crop, ha, -adm_name, -adm_code, -adm_level)

# Set -999 and empty string values
ha <- ha %>%
  mutate(ha = if_else(ha == -999, NA_real_, ha))

# filter out crops which values are all zero or NA
crop_na_0 <- ha %>%
  group_by(crop) %>%
  filter(all(ha %in% c(0, NA))) %>%
  dplyr::select(crop) %>%
  unique

ha <- ha %>%
  filter(!crop %in% crop_na_0$crop)

# Remove lower level adm data if it would somehow not be used
ha <- ha %>%
  filter(adm_level <= param$adm_level)


########## PROCESS FARMING SYSTEM SHARES ##########
# wide to long format
fs <- fs %>%
  gather(crop, fs, -adm_name, -adm_code, -adm_level, -system)

# Set -999 and empty string values
fs <- fs %>%
  mutate(fs = if_else(fs == -999, NA_real_, fs))

# Select relevent crops using ha
fs <- fs %>%
  filter(crop %in% unique(ha$crop))


########## CROPPING INTENSITY ##########
# wide to long format
ci <- ci %>%
  gather(crop, ci, -adm_name, -adm_code, -adm_level, -system)

# Set -999 and empty string values
ci <- ci %>%
  mutate(ci = if_else(ci == -999, NA_real_, ci))

# Select relevent crops using ha
ci <- ci %>%
  filter(crop %in% unique(ha$crop))


############### SET ADM IN LINE WITH SOLVE_SEL ###############
if(param$solve_level == 0) {
  adm_code_list <- unique(adm_list$adm0_code)
} else {
  adm_code_list <- unique(adm_list$adm1_code)
}

#walk(adm_code_list, prepare_pa_stat, ha, fs, ci, param)

adm_code_sel <- adm_code_list[1]


########## CONSISTENCY CHECKS ##########


# Format checks, numerical values and code is character
# consistency within data: do they add up and are lower level totals not higher than higher level ADMs
# Check if artifical ADMs area is near zero.
# Do shares of farming systems add up.
# Is ADM system the same as the map
# Are crops the same in ci, fs and ha data files. Not possible to have ADM0 data and NA for fs and ci.

prepare_pa_stat <- function(adm_code_sel, ha, fs, ci, param){
  if(!adm_code_sel %in% ha$adm_code[ha$adm_level == param$solve_level]) {
    stop("The adm codes in adm_code_sel are not present in the list of adm codes in
             the physical area statistics.",
         call. = FALSE)
  }
  message(glue("Save pa and pa_fs statistics for {adm_code_sel}"))

  # Select ha for top level and all lower level ADMs
  ha_adm <- bind_rows(
    ha[ha$adm_code == adm_code_sel,],
    ha[ha$adm_code %in% adm_list$adm1_code[adm_list$adm0_code == adm_code_sel],],
    ha[ha$adm_code %in% adm_list$adm2_code[adm_list$adm1_code == adm_code_sel],],
    ha[ha$adm_code %in% adm_list$adm2_code[adm_list$adm0_code == adm_code_sel],]) %>%
    unique()

  # Select fs and ci for top level ADM only. We apply these to lower levels.
  fs_adm <- bind_rows(
    fs[fs$adm_code == adm_code_sel,]) %>%
    dplyr::select(-adm_code, -adm_name, -adm_level) %>%
    unique()

  ci_adm <- bind_rows(
    ci[ci$adm_code == adm_code_sel,]) %>%
    dplyr::select(-adm_code, -adm_name, -adm_level) %>%
    unique()

  # Calculate physical area using cropping intensity information.
  pa_adm <- ha_adm %>%
    left_join(ci_adm, by = "crop")  %>%
    left_join(fs_adm, by = c("crop", "system")) %>%
    mutate(pa = ha*fs/ci) %>%
    group_by(adm_name, adm_code, crop, adm_level) %>%
    summarize(pa = plus(pa, na.rm = T)) %>%
    ungroup()

  # Calculate physical area broken down by farming systems
  pa_fs_adm <- pa_adm %>%
    filter(adm_code == adm_code_sel) %>%
    left_join(fs_adm, by = "crop") %>%
    mutate(pa = pa*fs) %>%
    dplyr::select(-fs) %>%
    ungroup()

  # ADD CONSISTENCY CHECK
  compare_adm2(pa_adm, pa_fs_adm, param$solve_level)

  df1 <- pa_adm
  df2 <- pa_fs_adm
  level <- param$solve_level
  compare_adm2 <- function(df1, df2, level, out = F){
    tot1 <- sum_adm_total(df1, level) %>%
      na.omit
    tot2 <- sum_adm_total(df2, level) %>%
      na.omit
    inter <- intersect(tot1$crop, tot2$crop)
    if(!isTRUE(all.equal(tot1$value[tot1$crop %in% inter],
                         tot2$value[tot2$crop %in% inter]))){
      stop(
        glue::glue("df1 and df2 are not equal!",
                   call. = FALSE)
      )
    } else {
      message(glue::glue("df1 and df2 are equal"))
    }

    out_df <- dplyr::bind_rows(
      sum_adm_total(df1, level) %>%
        mutate(source = "df1"),
      sum_adm_total(df2, level) %>%
        mutate(source = "df2")) %>%
      tidyr::spread(source, value) %>%
      mutate(difference = round(df1 - df2, 6)) %>%
      dplyr::select(-adm_level)
    if(out) return(out_df)
  }



  # save
  temp_path <- file.path(param$spam_path,
                         glue::glue("processed_data/intermediate_output/{adm_code_sel}"))
  dir.create(temp_path, recursive = T, showWarnings = F)

  pa_adm <- pa_adm %>%
    spread(crop, pa) %>%
    arrange(adm_code, adm_name, adm_level)

  pa_fs_adm <- pa_fs_adm %>%
    spread(crop, pa) %>%
    arrange(adm_code, adm_name, adm_level)

  write_csv(pa_adm, file.path(temp_path,
                              glue::glue("pa_{param$year}_{adm_code_sel}_{param$iso3c}.csv")))
  write_csv(pa_fs_adm, file.path(temp_path,
                                 glue::glue("pa_fs_{param$year}_{adm_code_sel}_{param$iso3c}.csv")))
}
