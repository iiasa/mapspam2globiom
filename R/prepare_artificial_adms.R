# Function to prepare artificial adms
prepare_artificial_adms <- function(adm_cd, param) {

  load_intermediate_data(c("pa"), adm_cd, param, local = TRUE, mess = FALSE)
  load_data(c("adm_list"), param, local = TRUE, mess = FALSE)

  # Put statistics in long format and filter out crops where pa = 0
  # These crops create artificial adms, which created conflicts
  pa <- pa %>%
    tidyr::gather(crop, pa, -adm_code, -adm_name, -adm_level) %>%
    dplyr::filter(pa != 0)

  adm_list_at_highest_level <- unique(pa$adm_code[pa$adm_level == param$adm_level])
  adm_code_highest <- glue::glue("adm{max(pa$adm_level)}_code")
  base <- expand.grid(adm_code = adm_list_at_highest_level, crop = unique(pa$crop), stringsAsFactors = F) %>%
    dplyr::rename({{rn}} := adm_code) %>%
    dplyr::mutate(adm_level = param$adm_level) %>%
    dplyr::left_join(adm_list)

  ## Prepare loop
  step <- param$adm_level - param$solve_level
  init <- min(pa$adm_level)

  # set adm_art at lowest level
  adm_art <- filter_out_pa(init, pa) %>%
    dplyr::mutate("adm{{init}}_code_art" := adm0_code) %>%
    dplyr::rename("imp_adm{{init}}" := pa_adm0) %>%
    dplyr::select(-adm_level)

  # Only create artificial adms if step is larger than zero
  # Otherwise adm are the same as adm_art at the lowest level
  if(step > 0) {
    for(i in c(init:(step-1))) {
      adm_art <- identify_art_adms_per_level(i, adm_art, pa, base)
    }
  }


  return(adm_art)
}



# Finallize adm_art
pa_rn <- glue::glue("imp_adm{max(pa$adm_level)}")
adm_rn <- glue::glue("adm{max(pa$adm_level)}_code_art")
adm_art_final <- adm_art %>%
  dplyr::select(-{{adm_code_highest}}) %>%
  unique %>%
  dplyr::rename(pa = {{pa_rn}},
                adm_code = adm_rn)


# artificial adm mapping
adm_art_map <- adm_art %>%
  dplyr::rename(adm_code_art = {{adm_rn}},
                adm_code = {{adm_code_highest}}) %>%
  dplyr::select(adm_code_art, adm_code) %>%
  unique()


# Check if totals add up
all.equal(sum(adm_art_final$pa), sum(pa$pa[pa$adm_code == adm_cd]))

# Replace very small negative numbers which might occur because of rounding by 0
adm_art_final <- adm_art_final %>%
  dplyr::mutate(pa = ifelse(pa < 0, 0, pa))
unique(adm_art$adm_code)
