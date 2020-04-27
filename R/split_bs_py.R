# Process_bs_py
split_bs_py <- function(var, param, adm_code){

  load_intermediate_data(c("grid", "cl_harm", "pa_fs"), adm_code, param, local = TRUE, mess = FALSE)

  # Select relevant crop_system combinations to process
  pa_fs <- pa_fs %>%
    tidyr::gather(crop, pa, -adm_code, -adm_name, -adm_level, -system)

  crop_system_list <- pa_fs %>%
    dplyr::group_by(crop, system) %>%
    dplyr::filter(!all(pa %in% c(0, NA))) %>%
    dplyr::mutate(crop_system = paste(crop, system , sep = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::select(crop_system) %>%
    unique

  # Process bs_py
  lookup <- dplyr::bind_rows(
    data.frame(files_full = list.files(file.path(param$spam_path,
                  glue::glue("processed_data/maps/{var}")), full.names = TRUE, pattern = glob2rx("*.tif")),
               files = list.files(file.path(param$spam_path,
                  glue::glue("processed_data/maps/{var}")), full.names = FALSE, pattern = glob2rx("*.tif")),
               stringsAsFactors = FALSE)
  ) %>%
    tidyr::separate(files, into = c("crop", "system", "variable", "res", "year", "iso3c"), sep = "_", remove = F) %>%
    tidyr::separate(iso3c, into = c("iso3c", "ext"), sep = "\\.") %>%
    dplyr::select(-ext) %>%
    dplyr::mutate(crop_system = paste(crop, system, sep = "_")) %>%
    dplyr::filter(crop_system %in% crop_system_list$crop_system)

  # Process maps one-by-one
  df <- purrr::map_df(lookup$files_full, process_gaez, adm_code = adm_code, param = param)

  #save
  if(var == "biophysical_suitability") {
    saveRDS(df, file.path(param$spam_path,
      glue::glue("processed_data/intermediate_output/{adm_code}/bs_{param$res}_{param$year}_{adm_code}_{param$iso3c}.rds")))
  } else {
    if(var == "potential_yield") {
      saveRDS(df, file.path(param$spam_path,
        glue::glue("processed_data/intermediate_output/{adm_code}/py_{param$res}_{param$year}_{adm_code}_{param$iso3c}.rds")))
    }
  }
}

