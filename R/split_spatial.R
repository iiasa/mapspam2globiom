# Function split and save ir_df and cl_df in line with solve_level
split_spatial <- function(adm_code, df, var, adm_map_r, param){
  cat(glue::glue("Save {var} for {adm_code}"))
  adm_sel <- paste0("adm", param$solve_level, "_code")
  df <- dplyr::left_join(df, adm_map_r, by = c("gridID")) %>%
    na.omit()
  df <- df[df[[adm_sel]] == adm_code,]

  temp_path <- file.path(param$spam_path,
                         glue::glue("processed_data/intermediate_output/{adm_code}"))
  dir.create(temp_path, recursive = T, showWarnings = F)
  saveRDS(df, file.path(temp_path,
                        glue::glue("{var}_{param$res}_{param$year}_{adm_code}_{param$iso3c}.rds")))
}
