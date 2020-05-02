#'@export
create_all_tif <- function(param) {
  load_data("results", param, mess = F)

  # by crop and system
  crop_system_list <- results %>%
    dplyr::select(crop, system) %>%
    unique

  save_tif <- function(crp, sy, var, df) {
    r <- create_tif(crp, sy, var, df)
    temp_path <- file.path(param$spam_path,
                           glue::glue("processed_data/results/{param$res}/maps/{var}/"))
    dir.create(temp_path, showWarnings = F, recursive = T)
    raster::writeRaster(r,
      file.path(temp_path, glue::glue("{var}_{crp}_{sy}_{param$res}_{param$year}_{param$iso3c}.tif")),
      overwrite = T)
    }
  purrr::walk2(crop_system_list$crop, crop_system_list$system, save_tif, var = "pa", df = results)
  purrr::walk2(crop_system_list$crop, crop_system_list$system, save_tif, var = "ha", df = results)
}


