# function to compare cl_med with pa, and replace by cl_max if needed
# https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/#a-simpler-interpolation-pattern-with

#' @importFrom rlang .data
#'
adm_level_sel <- 2
df <- cl2_df
var <- "cl2"
update_cl <- function(adm_level_sel, df, pa_adm_tot){

  # set adm
}

rename_var <- dplyr::quo(adm2_code)
#rename_var <- dplyr::quo(adm1_code)
var_sum <- "cl2"

x <- function(df, var_sum, ren){
  adm_check <- df %>%
    dplyr::rename(adm_code = {{ren}}) %>%
    dplyr::group_by(adm_code) %>%
    dplyr::summarize(cl2 = sum(.data[[var_sum]], na.rm = T),
                     cl_max = sum(cl_max, na.rm = T))
  return(adm_check)
}

x(cl2_df, "cl2", adm2_code)


starwars %>%
  group_by(gender) %>%
  summarise(mass_maximum = max(mass, na.rm = TRUE))

data(starwars)
max_by <- function(data, var, by) {
  data %>%
    group_by(!!enquo(by)) %>%
    summarise(maximum = max(!!enquo(var), na.rm = TRUE))
}

starwars %>% max_by(mass, by = gender)

x(cl2_df, var = cl2, ren = adm2_code)

# Check for which adm cl < pa
adm_check <- df %>%
  dplyr::rename(adm_code = !!rename_var) %>%
  dplyr::group_by(adm_code) %>%
  dplyr::summarize(cl2 = sum(!!dplyr::enquo(var_sum), na.rm = T),
                   cl_max = sum(cl_max, na.rm = T)) %>%
  dplyr::left_join(pa_adm_tot %>%
                     dplyr::filter(adm_level == adm_level_sel)) %>%
  dplyr::mutate(diff_cl = cl2-pa,
                diff_cl_max = cl_max-pa) %>%
  dplyr::filter(diff_cl < 0)

# replace cl_area with cl_area max apart for dt if needed
if(NROW(adm2_check) == 0) {
  message("No adjustments needed for cl")
  cl3_df <- cl2_df %>%
    mutate(cl3 = cl2)
} else {
  message("cl need to be set to cl_max for following adm2s")
  print(adm2_check)
  cl3_df <- cl2_df %>%
    mutate(cl3 = ifelse(adm2_name %in% adm2_check$adm2_name & dt == "N", cl_max, cl2))

  # check again
  adm2_pa_gr_cl <- cl3_df %>%
    group_by(adm2_name, adm2_code) %>%
    summarize(cl3 = sum(cl3, na.rm = T),
              grid_size = sum(grid_size, na.rm = T)) %>%
    left_join(pa_adm_tot %>%
                filter(adm_level == 2) %>%
                rename(adm2_name = adm_name)) %>%
    mutate(diff = cl3-pa,
           exp_factor = pa/cl3) %>%
    filter(diff < 0)

  if(NROW(adm2_pa_gr_cl) == 0) {
    message("No further adjustments needed for adm2s")
  } else {
    message("Further adjustments needed for the following adm2s after setting to cl_max")
    print(adm2_pa_gr_cl)
  }
}

