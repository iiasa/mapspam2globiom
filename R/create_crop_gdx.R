# Function to create crop distribution for GLOBIOM in gdx format
#'@export
create_crop_distribution_gdx <- function(crop, param) {

  crop_upd <- crop %>%
    dplyr::filter(globiom_crop != "rest") %>%
    dplyr::select(SimUID, system, globiom_crop, value) %>%
    dplyr::mutate(system = dplyr::recode(system,
                                         "S" = "SS",
                                         "L" = "LI",
                                         "H" = "HI",
                                         "I" = "IR"))

  # Test if gdxrrw and gams are installed.
  setup_gams()

  crop_upd_gdx <- para_gdx(crop_upd,
                           c("SimUID", "system", "globiom_crop"),
                           "crop_area", "Crop area (000 ha)")

  temp_path <- file.path(param$spam_path,
                         glue::glue("processed_data/results/{param$res}"))
  dir.create(temp_path, showWarnings = F, recursive = T)

  gdxrrw::wgdx(file.path(temp_path, glue::glue("globiom_crop_area_{param$res}_{param$year}_{param$iso3c}")),
               crop_upd_gdx)
  cat("\n############### Crop distribution gdx file saved ###############")

}
