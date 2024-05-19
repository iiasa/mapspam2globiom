# Function to create land cover for GLOBIOM in gdx format
create_land_cover_gdx <- function(lc, simu, param) {

  simu_info <- simu |>
    st_drop_geometry() |>
    dplyr::select(-simu_area) |>
    unique()

  lc <- lc |>
    dplyr::left_join(simu_info, by = "SimUID") |>
    dplyr::select(
      SimUID,
      globiom_lc_code,
      LUId,
      ALLCOUNTRY,
      ALLCOLROW,
      AltiClass,
      SlpClass,
      SoilClass,
      AezClass,
      value
    )

  lc_gdx <-
    para_gdx(
      lc,
      c(
        "SimUID",
        "globiom_lc_code",
        "LUId",
        "ALLCOUNTRY",
        "ALLCOLROW",
        "AltiClass",
        "SlpClass",
        "SoilClass",
        "AezClass"
      ),
      "land_cover",
      "Updated land cover (000 ha)"
    )

  model_folder <- glue::glue("{param$model}_{param$resolution}_adm_level_{param$adm_level}_solve_level_{param$solve_level}")
  temp_path <- file.path(param$model_path,
                         glue::glue("processed_data/results/{model_folder}"))
  dir.create(temp_path, showWarnings = F, recursive = T)

  gdxrrw::wgdx(file.path(
    temp_path,
    glue::glue(
      "globiom_land_cover_{param$year}_{param$iso3c}"
    )
  ),
  lc_gdx)

  cat("\n############### Land cover gdx file saved ###############")
}
