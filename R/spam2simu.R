# function to aggregate mapspam output to simu
spam2simu <- function(var, crop_map, simu_r, param) {
  cat("\n##### Aggregate mapcpamc crop distribution maps to simu #####")
  model_folder <- glue::glue("{param$model}_{param$resolution}_adm_level_{param$adm_level}_solve_level_{param$solve_level}")
  files <- list.files(file.path(param$model_path,
    glue::glue("processed_data/results/{model_folder}/maps/{var}")), full.names = T, pattern = glob2rx("*.tif"))

  # function to aggregate mapspam raster output to simu
  raster2simu <- function(file, simu_r, param) {

    map <- terra::rast(file)
    cat("\n", names(map))
    simu_map <- as.data.frame(terra::zonal(map, simu_r, fun = 'sum', na.rm = T)) |>
      setNames(c("SimUID","value"))
    crp <- strsplit(names(map), "_")[[1]][1]
    sys <- strsplit(names(map), "_")[[1]][2]
    simu_map <- simu_map |>
      dplyr::mutate(crop = crp,
             system = sys,
             resolution = param$res,
             year = param$year,
             iso3c = param$iso3c)
    return(simu_map)
  }

  df <- purrr::map_dfr(files, raster2simu, simu_r, param) |>
    dplyr::mutate(variable = var)

  # Aggregate to globiom crops
  # Divide by 1000 to get 1000 ha as in globiom
  df <- df |>
    dplyr::left_join(crop_map, by = "crop") |>
    dplyr::group_by(SimUID, year, iso3c, system, globiom_crop) |>
    dplyr::summarize(value = sum(value, na.rm = T)/1000,
                     .groups = "drop") |>
    dplyr::ungroup()

  return(df)
}




