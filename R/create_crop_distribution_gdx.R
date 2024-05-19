# Function to create crop distribution for GLOBIOM in gdx format
create_crop_distribution_gdx <- function(crop, simu, param) {
  simu_info <- simu |>
    st_drop_geometry() |>
    dplyr::select(-simu_area) |>
    unique()

  crop_upd <- crop |>
    dplyr::filter(globiom_crop != "rest") |>
    dplyr::select(SimUID, system, globiom_crop, value) |>
    dplyr::mutate(system = dplyr::recode(
      system,
      "S" = "SS",
      "L" = "LI",
      "H" = "HI",
      "I" = "IR"
    )) |>
    dplyr::left_join(simu_info, by = "SimUID") |>
    dplyr::select(
      SimUID,
      system,
      globiom_crop,
      LUId,
      ALLCOUNTRY,
      ALLCOLROW,
      AltiClass,
      SlpClass,
      SoilClass,
      AezClass,
      value
    )

  crop_upd_gdx <- para_gdx(
    crop_upd,
    c(
      "SimUID",
      "system",
      "globiom_crop",
      "LUId",
      "ALLCOUNTRY",
      "ALLCOLROW",
      "AltiClass",
      "SlpClass",
      "SoilClass",
      "AezClass"
    ),
    "crop_area",
    "Crop area (000 ha)"
  )

  model_folder <- glue::glue("{param$model}_{param$resolution}_adm_level_{param$adm_level}_solve_level_{param$solve_level}")
  temp_path <- file.path(param$model_path,
                         glue::glue("processed_data/results/{model_folder}"))
  dir.create(temp_path, showWarnings = F, recursive = T)

  gdxrrw::wgdx(file.path(
    temp_path,
    glue::glue(
      "globiom_crop_area_{param$year}_{param$iso3c}"
    )
  ),
  crop_upd_gdx)
  cat("\n############### Crop distribution gdx file saved ###############")

}
