### PREPARE DATA
# Create dataframe with all adm crop combinations at lowest level = adm_lvl
# associated lower level adms

rn <- glue::glue("adm{param$adm_level}_code")
adm_list_at_lowest_level <- unique(pa$adm_code[pa$adm_level == param$adm_level])
base <- expand.grid(adm_code = adm_list_at_lowest_level, crop = unique(pa$crop), stringsAsFactors = F) %>%
  dplyr::rename({{rn}} := adm_code) %>%
  dplyr::mutate(adm_level = param$adm_level) %>%
  dplyr::left_join(adm_code_list)



create_pa_tot <- function(i, pa) {

  df <- pa %>%
    dplyr::filter(adm_level == i) %>%
    dplyr::rename("adm{{i}}_code" := .data$adm_code,
                  "pa_adm{{i}}" :=  .data$pa) %>%
    dplyr::select(-adm_name)
  return(df)
}

create_art_adm <- function(i, df_x_art) {

  df_x <- create_pa_tot(i, pa)
  df_y <- create_pa_tot(i+1, pa)
  base_xy <- base

  # Rename
  names(df_x) <- gsub("[0-9]", "X", names(df_x))
  names(df_y) <- gsub("[0-9]", "Y", names(df_y))
  names(df_x_art) <- gsub("[0-9]", "X", names(df_x_art))
  adm_level_x <- unique(df_x$adm_level)
  adm_level_y <- unique(df_y$adm_level)
  names(base_xy) <- gsub(adm_level_x, "X", names(base_xy))
  names(base_xy) <- gsub(adm_level_y, "Y", names(base_xy))

  # Drop adm_level for joining
  df_x <- dplyr::select(df_x, -adm_level)
  df_y <- dplyr::select(df_y, -adm_level)

  # Combine df_x and df_y
  df_xy <- dplyr::left_join(base_xy, df_x) %>%
    dplyr::left_join(df_y) %>%
    dplyr::select(crop, admX_code, admY_code, pa_admY) %>%
    unique

  # Calculate pa for artificial adms
  art_id <- glue::glue("ART{i+1}")
  df_y_art <- df_xy %>%
    dplyr::left_join(df_x_art) %>%
    dplyr::group_by(admX_code_art, crop) %>%
    dplyr::mutate(admY_av = sum(pa_admY, na.rm = T),
           imp_admY = ifelse(is.na(pa_admY), unique(imp_admX) - admY_av, pa_admY)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(admY_code_art = ifelse(is.na(pa_admY), paste(admX_code_art, art_id, crop, sep = "_"), admY_code)) %>%
    dplyr::select(admY_code_art, admY_code, crop, imp_admY) %>%
    unique

  # Correct names
  names(df_y_art) <- gsub("Y", i+1, names(df_y_art))

  return(df_y_art)
}


## For loop
step <- param$adm_level - param$solve_level
init <- min(pa$adm_level)
vec <- c(init:(step-1))
i <- vec[1]

# set adm_art x0
adm_art <- create_pa_tot(init, pa) %>%
  dplyr::mutate("adm{{init}}_code_art" := adm0_code) %>%
  dplyr::rename("imp_adm{{init}}" := pa_adm0) %>%
  dplyr::select(-adm_level)

# Loop 1
# Insert adm_art x0
# update adm_art to x1 for next loop
adm_art_upd1 <- create_art_adm(i, adm_art)
adm_art_upd2 <- create_art_adm(1, adm_art_upd1)



# Loop 2
# insert adm_art x1
# update adm_art to x2 for next loop

# Final adm_art
adm_art_final <- adm_art_upd2 %>%
  dplyr::select(-adm2_code) %>%
  unique



i <- 0




df_x_art <- adm0_art <- adm0_pa %>%
  dplyr::mutate(adm0_code_art = adm0_code) %>%
  dplyr::rename(imp_adm0 = pa_adm0) %>%
  dplyr::select(-adm_level)












# Prepare artificial adm combining adm0 and adm1
adm1_art <- adm0_1 %>%
  dplyr::left_join(adm0_art) %>%
  dplyr::group_by(adm0_code_art, crop) %>%
  dplyr::mutate(adm1_av = sum(pa_adm1, na.rm = T),
                imp_adm1 = ifelse(is.na(pa_adm1), unique(pa_adm0) - adm1_av, pa_adm1)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(adm1_code_art = ifelse(is.na(pa_adm1), paste(adm0_code_art, "ART1", crop, sep = "_"), adm1_code)) %>%
  dplyr::select(adm1_code_art, adm1_code, crop, imp_adm1) %>%
  unique



### ADM1_2 ARTIFICIAL UNITS
# We take the new adm1 list including artificial adms as basis

# Combine adm 1_2 data
adm1_2 <- dplyr::left_join(base, adm2_pa) %>%
  dplyr::left_join(adm1_pa) %>%
  dplyr::select(crop, adm1_code, adm2_code, pa_adm2) %>%
  unique


# Prepare artificial adm combining adm0 and adm1
adm2_art <- adm1_2 %>%
  dplyr::left_join(adm1_art) %>%
  dplyr::group_by(adm1_code_art, crop) %>%
  dplyr::mutate(adm2_av = sum(pa_adm2, na.rm = T),
                imp_adm2 = ifelse(is.na(pa_adm2), unique(imp_adm1) - adm2_av, pa_adm2)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(adm2_code_art = ifelse(is.na(pa_adm2), paste(adm1_code_art, "ART2", crop, sep = "_"), adm2_code)) %>%
  unique()




adm_art <- adm2_art %>%
  dplyr::select(crop, imp_adm2, adm2_code_art) %>%
  unique %>%
  dplyr::rename(adm_code = adm2_code_art, pa = imp_adm2)

# artificial adm mapping
adm_art_map <- adm2_art %>%
  dplyr::select(adm_code_art = adm2_code_art, adm_code = adm2_code) %>%
  unique
}
