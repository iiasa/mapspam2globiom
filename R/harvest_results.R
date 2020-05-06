#' Harvest results created after running SPAMc with GAMS
#'
#'@export
harvest_results <- function(param) {
  load_data(c("adm_list", "ci"), param, local = TRUE, mess = FALSE)

  # Set adm_level
  if(param$solve_level == 0) {
    adm_code_list <- unique(adm_list$adm0_code)
  } else {
    adm_code_list <- unique(adm_list$adm1_code)
  }

 df <- purrr::map_df(adm_code_list, prepare_results_adm_level, param) %>%
   dplyr::mutate(year = param$year,
          resolution = param$res,
          model = param$model,
          solve_level = param$solve_level)

 ac_rn <- glue::glue("adm{param$solve_level}_code")
 an_rn <- glue::glue("adm{param$solve_level}_name")

 ci <- ci %>%
   tidyr::gather(crop, ci, -adm_name, -adm_code, -adm_level, -system) %>%
   dplyr::filter(adm_level == param$solve_level) %>%
   dplyr::rename({{ac_rn}} := adm_code,
                 {{an_rn}} := adm_name) %>%
   dplyr::select(-adm_level)

 df <- df %>%
   dplyr::left_join(ci) %>%
   dplyr::mutate(ha = pa*ci) %>%
   dplyr::select(gridID, crop, system, ha, pa, everything(), pa, ha)

 temp_path <- file.path(param$spam_path,
                        glue::glue("processed_data/results/{param$res}"))
 dir.create(temp_path, showWarnings = F, recursive = T)
 saveRDS(df, file.path(temp_path, glue::glue("results_{param$res}_{param$year}_{param$iso3c}.rds")))
}



